--------------------------------------------------------------------------------
-- CERN (BE-CO-HT)
-- VME64x Core
-- http://www.ohwr.org/projects/vme64x-core
--------------------------------------------------------------------------------
--
-- unit name:     xvme64x_core (xvme64x_core.vhd)
--
-- author:        Tomasz Wlostowski <tomasz.wlostowski@cern.ch>
--
-- description:
--
--   This core implements an interface to transfer data between the VMEbus and
--   the WBbus. This core is a Slave in the VME side and Master in the WB side.
--
--   The main blocks:
--
--      _______________________vme64x_core_______________________
--     |      ________________   ________   ___________________  |
--     |___  |                | |        | |                   | |
--     |   | |    VME Bus     | | Funct  | |                   | |
--     |   | |                | | Match  | |  VME to WB FIFO   | |
--     | S | |       |        | |        | | (not implemented) | |
--   V | A | |  VME  |   WB   | |________| |                   | | W
--   M | M | | slave | master |  ________  |                   | | B
--   E | P | |       |        | |        | |                   | |
--     | L | |       |        | | CR/CSR | |                   | | B
--   B | I | |       |        | | Space  | |___________________| | U
--   U | N | |                | |________|  ___________________  | S
--   S | G | |                |  ________  |                   | |
--     |   | |                | |        | |  IRQ Controller   | |
--     |___| |                | |  User  | |                   | |
--     |     |                | |  CSR   | |                   | |
--     |     |________________| |________| |___________________| |
--     |_________________________________________________________|
--
--   This core complies with the VME64x specifications and allows "plug and
--   play" configuration of VME crates.
--   The base address is setted by the Geographical lines.
--   The base address can't be setted by hand with the switches on the board.
--   If the core is used in an old VME system without GA lines, the core should
--   be provided with a logic that detects if GA = "11111" and if it is the base
--   address of the module, this logic should derive the GA from the switches on
--   the board.
--   All the VMEbus's asynchronous signals must be sampled 2 or 3 times to avoid
--   metastability problem.
--   All the output signals on the WB bus are registered.
--   The Input signals from the WB bus aren't registered indeed the WB is a
--   synchronous protocol and some registers in the WB side will introduce a
--   delay that make impossible reproduce the WB PIPELINED protocol.
--   The WB Slave application must work with the same frequency as this vme64x
--   core.
--   The main component of this core is the VME_bus on the left in the block
--   diagram. Inside this component you can find the main finite state machine
--   that coordinates all the synchronisms.
--   The WB protocol is more faster than the VME protocol so to make independent
--   the two protocols a FIFO memory can be introduced.
--   The FIFO is necessary only during 2eSST access mode.
--   During the block transfer without FIFO the VME_bus accesses directly the Wb
--   bus in Single pipelined read/write mode. If this is the only Wb master this
--   solution is better than the solution with FIFO.
--   In this base version of the core the FIFO is not implemented indeed the 2e
--   access modes aren't supported yet.
--   A Configuration ROM/Control Status Register (CR/CSR) address space has been
--   introduced. The CR/CSR space can be accessed with the data transfer type
--   D08_3, D16_23, D32.
--   To access the CR/CSR space: AM = 0x2f --> this is A24 addressing type,
--   SINGLE transfer type. Base Address = Slot Number.
--   This interface is provided with an Interrupter. The IRQ Controller receives
--   from the Application (WB bus) an interrupt request and transfers this
--   interrupt request on the VMEbus. This component acts also during the
--   Interrupt acknowledge cycle, sending the status/ID to the Interrupt
--   handler.
--   Interrupt request; the IRQ Generator/your Wb application sends a pulse
--   to the IRQ Controller which asserts one of the IRQ line defined by
--   irq_level_i.
--   Inside each component, a detailed description is provided.
--   Access modes supported:
--   http://www.ohwr.org/projects/vme64x-core/repository/changes/trunk/
--          documentation/user_guides/VME_access_modes.pdf
--
-- standards:
--
--   * VMEbus             ANSI/IEEE Std 1014-1987
--   * VME64              ANSI/VITA 1-1994
--   * VME64x Extensions  ANSI/VITA 1.1-1997
--   * VME 2eSST          ANSI/VITA 1.5-2003
--
-- dependencies:
--
--------------------------------------------------------------------------------
-- GNU LESSER GENERAL PUBLIC LICENSE
--------------------------------------------------------------------------------
-- This source file is free software; you can redistribute it and/or modify it
-- under the terms of the GNU Lesser General Public License as published by the
-- Free Software Foundation; either version 2.1 of the License, or (at your
-- option) any later version. This source is distributed in the hope that it
-- will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
-- of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-- See the GNU Lesser General Public License for more details. You should have
-- received a copy of the GNU Lesser General Public License along with this
-- source; if not, download it from http://www.gnu.org/licenses/lgpl-2.1.html
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use work.wishbone_pkg.all;
use work.vme64x_pkg.all;

entity xvme64x_core is
  generic (
    -- Clock period (ns). Used for DS synchronization. The default value
    -- will genrate an assertion failure.
    g_CLOCK_PERIOD    : integer := -1;

    -- Consider AM field of ADER to decode addresses. This is what the VME64x
    -- standard says. However, for compatibility with previous implementations
    -- (or to reduce resources), it is possible for a decoder to allow all AM
    -- declared in the AMCAP.
    g_DECODE_AM       : boolean := true;

    -- Use external user CSR
    g_USER_CSR_EXT    : boolean := false;

    -- Manufacturer ID: IEEE OUID
    --                  e.g. CERN is 0x080030
    g_MANUFACTURER_ID : std_logic_vector(23 downto 0);

    -- Board ID: Per manufacturer, each board shall have an unique ID
    --           e.g. SVEC = 408 (CERN IDs: http://cern.ch/boardid)
    g_BOARD_ID        : std_logic_vector(31 downto 0);

    -- Revision ID: user defined revision code
    g_REVISION_ID     : std_logic_vector(31 downto 0);

    -- Program ID: Defined per VME64:
    --               0x00      = Not used
    --               0x01      = No program, ID ROM only
    --               0x02-0x4F = Manufacturer defined
    --               0x50-0x7F = User defined
    --               0x80-0xEF = Reserved for future use
    --               0xF0-0xFE = Reserved for Boot Firmware (P1275)
    --               0xFF      = Not to be used
    g_PROGRAM_ID      : std_logic_vector( 7 downto 0);

    -- Pointer to a user defined ASCII string.
    g_ASCII_PTR       : std_logic_vector(23 downto 0)  := x"000000";

    -- User CR/CSR, CRAM & serial number pointers
    g_BEG_USER_CR     : std_logic_vector(23 downto 0)  := x"000000";
    g_END_USER_CR     : std_logic_vector(23 downto 0)  := x"000000";
    g_BEG_CRAM        : std_logic_vector(23 downto 0)  := x"000000";
    g_END_CRAM        : std_logic_vector(23 downto 0)  := x"000000";
    g_BEG_USER_CSR    : std_logic_vector(23 downto 0)  := x"07ff33";
    g_END_USER_CSR    : std_logic_vector(23 downto 0)  := x"07ff5f";
    g_BEG_SN          : std_logic_vector(23 downto 0)  := x"000000";
    g_END_SN          : std_logic_vector(23 downto 0)  := x"000000";

    -- Number of function decoder implemented and decoder parameters.
    g_NBR_DECODERS    : natural range 1 to 8 := 2;
    g_DECODER         : t_vme64x_decoder_arr := c_vme64x_decoders_default);
  port (
    -- Main clock and reset.
    clk_i           : in  std_logic;
    rst_n_i         : in  std_logic;

    -- Reset for wishbone core.
    rst_n_o         : out std_logic;

    -- VME slave interface.
    vme_i           : in t_vme64x_in;
    vme_o           : out t_vme64x_out;

    -- Wishbone interface.
    wb_i            : in  t_wishbone_master_in;
    wb_o            : out t_wishbone_master_out;

    -- When the IRQ controller acknowledges the Interrupt cycle it sends a
    -- pulse to the IRQ Generator.
    irq_ack_o       : out std_logic;

    -- User CSR
    -- The following signals are used when g_USER_CSR_EXT = true
    -- otherwise they are connected to the internal user CSR.
    irq_level_i     : in  std_logic_vector( 7 downto 0) := (others => '0');
    irq_vector_i    : in  std_logic_vector( 7 downto 0) := (others => '0');
    user_csr_addr_o : out std_logic_vector(18 downto 2);
    user_csr_data_i : in  std_logic_vector( 7 downto 0) := (others => '0');
    user_csr_data_o : out std_logic_vector( 7 downto 0);
    user_csr_we_o   : out std_logic;

    -- User CR
    user_cr_addr_o  : out std_logic_vector(18 downto 2);
    user_cr_data_i  : in  std_logic_vector( 7 downto 0) := (others => '0'));
end xvme64x_core;

architecture rtl of xvme64x_core is

  signal s_reset                : std_logic;
  signal s_reset_n              : std_logic;

  signal s_VME_IRQ_n_o          : std_logic_vector( 7 downto 1);
  signal s_irq_ack              : std_logic;
  signal s_irq_pending          : std_logic;

  -- CR/CSR
  signal s_cr_csr_addr          : std_logic_vector(18 downto 2);
  signal s_cr_csr_data_o        : std_logic_vector( 7 downto 0);
  signal s_cr_csr_data_i        : std_logic_vector( 7 downto 0);
  signal s_cr_csr_we            : std_logic;
  signal s_ader                 : t_ader_array(0 to 7);
  signal s_module_reset         : std_logic;
  signal s_module_enable        : std_logic;
  signal s_bar                  : std_logic_vector( 4 downto 0);
  signal s_vme_berr_n           : std_logic;

  signal s_irq_vector           : std_logic_vector( 7 downto 0);
  signal s_irq_level            : std_logic_vector( 2 downto 0);
  signal s_user_csr_addr        : std_logic_vector(18 downto 2);
  signal s_user_csr_data_i      : std_logic_vector( 7 downto 0);
  signal s_user_csr_data_o      : std_logic_vector( 7 downto 0);
  signal s_user_csr_we          : std_logic;

  -- Function decoders
  signal s_addr_decoder_i       : std_logic_vector(31 downto 0);
  signal s_addr_decoder_o       : std_logic_vector(31 downto 0);
  signal s_decode_start         : std_logic;
  signal s_decode_done          : std_logic;
  signal s_decode_sel           : std_logic;
  signal s_am                   : std_logic_vector( 5 downto 0);

  -- Oversampled input signals
  signal s_VME_RST_n            : std_logic;
  signal s_VME_AS_n             : std_logic;
  signal s_VME_WRITE_n          : std_logic;
  signal s_VME_DS_n             : std_logic_vector(1 downto 0);
  signal s_VME_IACK_n           : std_logic;
  signal s_VME_IACKIN_n         : std_logic;

  -- CR/CSR parameter arrays
  constant c_ADEM : t_adem_array(0 to 7) := (
    g_decoder(0).adem, g_decoder(1).adem,
    g_decoder(2).adem, g_decoder(3).adem,
    g_decoder(4).adem, g_decoder(5).adem,
    g_decoder(6).adem, g_decoder(7).adem);
  constant c_AMCAP : t_amcap_array(0 to 7) := (
    g_decoder(0).amcap, g_decoder(1).amcap,
    g_decoder(2).amcap, g_decoder(3).amcap,
    g_decoder(4).amcap, g_decoder(5).amcap,
    g_decoder(6).amcap, g_decoder(7).amcap);
  constant c_DAWPR : t_dawpr_array(0 to 7) := (
    g_decoder(0).dawpr, g_decoder(1).dawpr,
    g_decoder(2).dawpr, g_decoder(3).dawpr,
    g_decoder(4).dawpr, g_decoder(5).dawpr,
    g_decoder(6).dawpr, g_decoder(7).dawpr);

  -- List of supported AM.
  constant c_AMCAP_ALLOWED : std_logic_vector(63 downto 0) :=
    (16#3c# to 16#3f# => '1', --  A24
     16#38# to 16#3b# => '1',
     16#2d# | 16#29#  => '1', --  A16
     16#0c# to 16#0f# => '1', --  A32
     16#08# to 16#0b# => '1',
     others => '0');
begin
  assert g_CLOCK_PERIOD > 0 report "g_CLOCK_PERIOD generic must be set"
    severity failure;

  --  Check for invalid bits in ADEM/AMCAP
  gen_gchecks: for i in 7 downto 0 generate
    assert c_ADEM(i)(c_ADEM_FAF) = '0' report "FAF bit set in ADEM"
      severity failure;
    assert c_ADEM(i)(c_ADEM_DFS) = '0' report "DFS bit set in ADEM"
      severity failure;
    assert c_ADEM(i)(c_ADEM_EFM) = '0' report "EFM bit set in ADEM"
      severity failure;
    assert (c_AMCAP(i) and c_AMCAP_ALLOWED) = c_AMCAP(i)
      report "bit set in AMCAP for not supported AM"
      severity failure;
  end generate;

  ------------------------------------------------------------------------------
  -- Metastability
  ------------------------------------------------------------------------------
  -- Input oversampling: oversampling the input data is
  -- necessary to avoid metastability problems, but of course the transfer rate
  -- will be slow down a little.
  -- NOTE: the reset value is '0', which means that all signals are active
  -- at reset. But not for a long time and so is s_VME_RST_n.
  inst_vme_rst_resync: entity work.gc_sync_register
    generic map (g_width => 1)
    port map (clk_i => clk_i,
              rst_n_a_i => rst_n_i,
              d_i(0) => vme_i.rst_n,
              q_o(0) => s_vme_rst_n);
  inst_vme_as_resync: entity work.gc_sync_register
    generic map (g_width => 1)
    port map (clk_i => clk_i,
              rst_n_a_i => rst_n_i,
              d_i(0) => vme_i.as_n,
              q_o(0) => s_vme_as_n);
  inst_vme_write_resync: entity work.gc_sync_register
    generic map (g_width => 1)
    port map (clk_i => clk_i,
              rst_n_a_i => rst_n_i,
              d_i(0) => vme_i.write_n,
              q_o(0) => s_vme_write_n);
  inst_vme_ds_resync: entity work.gc_sync_register
    generic map (g_width => 2)
    port map (clk_i => clk_i,
              rst_n_a_i => rst_n_i,
              d_i => vme_i.ds_n,
              q_o => s_vme_ds_n);
  inst_vme_iack_resync: entity work.gc_sync_register
    generic map (g_width => 1)
    port map (clk_i => clk_i,
              rst_n_a_i => rst_n_i,
              d_i(0) => vme_i.iack_n,
              q_o(0) => s_vme_iack_n);
  inst_vme_iackin_resync: entity work.gc_sync_register
    generic map (g_width => 1)
    port map (clk_i => clk_i,
              rst_n_a_i => rst_n_i,
              d_i(0) => vme_i.iackin_n,
              q_o(0) => s_VME_IACKIN_n);

  ------------------------------------------------------------------------------
  -- VME Bus
  ------------------------------------------------------------------------------
  inst_vme_bus : entity work.vme_bus
    generic map (
      g_CLOCK_PERIOD  => g_CLOCK_PERIOD
    )
    port map (
      clk_i           => clk_i,
      rst_i           => s_reset,

      -- VME
      VME_AS_n_i      => s_VME_AS_n,
      VME_LWORD_n_o   => vme_o.lword_n,
      VME_LWORD_n_i   => vme_i.lword_n,
      VME_RETRY_n_o   => vme_o.retry_n,
      VME_RETRY_OE_o  => vme_o.retry_oe,
      VME_WRITE_n_i   => s_VME_WRITE_n,
      VME_DS_n_i      => s_VME_DS_n,
      VME_DTACK_n_o   => vme_o.dtack_n,
      VME_DTACK_OE_o  => vme_o.dtack_oe,
      VME_BERR_n_o    => s_vme_berr_n,
      VME_ADDR_i      => vme_i.addr,
      VME_ADDR_o      => vme_o.addr,
      VME_ADDR_DIR_o  => vme_o.addr_dir,
      VME_ADDR_OE_N_o => vme_o.addr_oe_n,
      VME_DATA_i      => vme_i.data,
      VME_DATA_o      => vme_o.data,
      VME_DATA_DIR_o  => vme_o.data_dir,
      VME_DATA_OE_N_o => vme_o.data_oe_n,
      VME_AM_i        => vme_i.am,
      VME_IACKIN_n_i  => s_VME_IACKIN_n,
      VME_IACK_n_i    => s_VME_IACK_n,
      VME_IACKOUT_n_o => vme_o.iackout_n,

      -- WB signals
      stb_o           => wb_o.stb,
      ack_i           => wb_i.ack,
      dat_o           => wb_o.dat,
      dat_i           => wb_i.dat,
      adr_o           => wb_o.adr,
      sel_o           => wb_o.sel,
      we_o            => wb_o.we,
      cyc_o           => wb_o.cyc,
      err_i           => wb_i.err,
      stall_i         => wb_i.stall,

      -- Function decoder
      addr_decoder_i  => s_addr_decoder_o,
      addr_decoder_o  => s_addr_decoder_i,
      decode_start_o  => s_decode_start,
      decode_done_i   => s_decode_done,
      am_o            => s_am,
      decode_sel_i    => s_decode_sel,

      -- CR/CSR signals
      cr_csr_addr_o   => s_cr_csr_addr,
      cr_csr_data_i   => s_cr_csr_data_o,
      cr_csr_data_o   => s_cr_csr_data_i,
      cr_csr_we_o     => s_cr_csr_we,
      module_enable_i => s_module_enable,
      bar_i           => s_bar,

      INT_Level_i     => s_irq_level,
      INT_Vector_i    => s_irq_vector,
      irq_pending_i   => s_irq_pending,
      irq_ack_o       => s_irq_ack
    );

  s_reset    <= (not rst_n_i) or (not s_VME_RST_n);
  s_reset_n  <= not s_reset;
  rst_n_o    <= not (s_reset or s_module_reset);

  vme_o.berr_n <= s_vme_berr_n;

  inst_vme_funct_match : entity work.vme_funct_match
    generic map (
      g_ADEM      => c_ADEM,
      g_AMCAP     => c_AMCAP,
      g_DECODE_AM => g_DECODE_AM
    )
    port map (
      clk_i          => clk_i,
      rst_n_i        => s_reset_n,

      addr_i         => s_addr_decoder_i,
      addr_o         => s_addr_decoder_o,
      decode_start_i => s_decode_start,
      am_i           => s_am,
      ader_i         => s_ader,
      decode_sel_o   => s_decode_sel,
      decode_done_o  => s_decode_done
    );

  ------------------------------------------------------------------------------
  -- Output
  ------------------------------------------------------------------------------
  irq_ack_o  <= s_irq_ack;

  ------------------------------------------------------------------------------
  --  Interrupter
  ------------------------------------------------------------------------------
  inst_vme_irq_controller : entity work.vme_irq_controller
    generic map (
      g_RETRY_TIMEOUT => 1000000 / g_CLOCK_PERIOD   -- 1ms timeout
    )
    port map (
      clk_i           => clk_i,
      rst_n_i         => s_reset_n,                 -- asserted when low
      INT_Level_i     => s_irq_level,
      INT_Req_i       => wb_i.int,
      irq_pending_o   => s_irq_pending,
      irq_ack_i       => s_irq_ack,
      VME_IRQ_n_o     => vme_o.irq_n
    );

  ------------------------------------------------------------------------------
  -- CR/CSR space
  ------------------------------------------------------------------------------
  inst_vme_cr_csr_space : entity work.vme_cr_csr_space
    generic map (
      g_MANUFACTURER_ID  => g_MANUFACTURER_ID,
      g_BOARD_ID         => g_BOARD_ID,
      g_REVISION_ID      => g_REVISION_ID,
      g_PROGRAM_ID       => g_PROGRAM_ID,
      g_ASCII_PTR        => g_ASCII_PTR,
      g_BEG_USER_CR      => g_BEG_USER_CR,
      g_END_USER_CR      => g_END_USER_CR,
      g_BEG_CRAM         => g_BEG_CRAM,
      g_END_CRAM         => g_END_CRAM,
      g_BEG_USER_CSR     => g_BEG_USER_CSR,
      g_END_USER_CSR     => g_END_USER_CSR,
      g_BEG_SN           => g_BEG_SN,
      g_END_SN           => g_END_SN,
      g_ADEM             => c_ADEM,
      g_AMCAP            => c_AMCAP,
      g_DAWPR            => c_DAWPR
    )
    port map (
      clk_i               => clk_i,
      rst_n_i             => s_reset_n,

      vme_ga_i            => vme_i.ga,
      vme_berr_n_i        => s_vme_berr_n,
      bar_o               => s_bar,
      module_enable_o     => s_module_enable,
      module_reset_o      => s_module_reset,

      addr_i              => s_cr_csr_addr,
      data_i              => s_cr_csr_data_i,
      data_o              => s_cr_csr_data_o,
      we_i                => s_cr_csr_we,

      user_csr_addr_o     => s_user_csr_addr,
      user_csr_data_i     => s_user_csr_data_i,
      user_csr_data_o     => s_user_csr_data_o,
      user_csr_we_o       => s_user_csr_we,

      user_cr_addr_o      => user_cr_addr_o,
      user_cr_data_i      => user_cr_data_i,

      ader_o              => s_ader
    );

  -- User CSR space
  gen_int_user_csr : if g_USER_CSR_EXT = false generate
    inst_vme_user_csr : entity work.vme_user_csr
      port map (
        clk_i        => clk_i,
        rst_n_i      => s_reset_n,
        addr_i       => s_user_csr_addr,
        data_i       => s_user_csr_data_o,
        data_o       => s_user_csr_data_i,
        we_i         => s_user_csr_we,
        irq_vector_o => s_irq_vector,
        irq_level_o  => s_irq_level
      );
  end generate;
  gen_ext_user_csr : if g_USER_CSR_EXT = true generate
    s_user_csr_data_i <= user_csr_data_i;
    s_irq_vector      <= irq_vector_i;
    s_irq_level       <= irq_level_i(2 downto 0);
  end generate;

  user_csr_addr_o <= s_user_csr_addr;
  user_csr_data_o <= s_user_csr_data_o;
  user_csr_we_o   <= s_user_csr_we;

  assert wb_i.rty = '0' report "rty not supported";
end rtl;
