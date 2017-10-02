--------------------------------------------------------------------------------
-- CERN (BE-CO-HT)
-- VME64x Core
-- http://www.ohwr.org/projects/vme64x-core
--------------------------------------------------------------------------------
--
-- unit name:     VME64xCore_Top (VME64xCore_Top.vhd)
--
-- author:        Pablo Alvarez Sanchez <pablo.alvarez.sanchez@cern.ch>
--                Davide Pedretti       <davide.pedretti@cern.ch>
--
-- description:
--
--   This core implements an interface to transfer data between the VMEbus and
--   the WBbus. This core is a Slave in the VME side and Master in the WB side.
--
--   The main blocks:
--
--      ______________________VME64xCore_Top_____________________
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
--   be provided of a logic that detects if GA = "11111" and if it is the base
--   address of the module should be derived from the switches on the board.
--   All the VMEbus's asynchronous signals must be sampled 2 or 3 times to avoid
--   metastability problem.
--   All the output signals on the WB bus are registered.
--   The Input signals from the WB bus aren't registered indeed the WB is a
--   synchronous protocol and some registers in the WB side will introduce a
--   delay that make impossible reproduce the WB PIPELINED protocol.
--   The WB Slave application must work at the same frequency of this vme64x
--   core.
--   The main component is the VME_bus on the left of the block diagram. Inside
--   this component you can find the main finite state machine that coordinates
--   all the synchronisms.
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
--   Inside each component is possible to read a more detailed description.
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
-- last changes: see log.
--------------------------------------------------------------------------------
-- TODO: -
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.vme64x_pack.all;

entity VME64xCore_Top is
  generic (
    g_CLOCK_PERIOD    : integer   := c_CLOCK_PERIOD;  -- Clock period (ns)
    g_WB_DATA_WIDTH   : integer   := c_DATA_WIDTH;    -- WB data width: = 32
    g_WB_ADDR_WIDTH   : integer   := c_ADDR_WIDTH;    -- WB addr width: <= 32
    g_USER_CSR_EXT    : boolean   := false;           -- Use external user CSR

    -- Manufacturer ID: IEEE OUID
    --                  e.g. CERN is 0x080030
    g_MANUFACTURER_ID : std_logic_vector(23 downto 0)   := c_CERN_ID;

    -- Board ID: Per manufacturer, each board shall have an unique ID
    --           e.g. SVEC = 408 (CERN IDs: http://cern.ch/boardid)
    g_BOARD_ID        : std_logic_vector(31 downto 0)   := c_SVEC_ID;

    -- Revision ID: user defined revision code
    g_REVISION_ID     : std_logic_vector(31 downto 0)   := c_REVISION_ID;

    -- Program ID: Defined per AV1:
    --               0x00      = Not used
    --               0x01      = No program, ID ROM only
    --               0x02-0x4F = Manufacturer defined
    --               0x50-0x7F = User defined
    --               0x80-0xEF = Reserved for future use
    --               0xF0-0xFE = Reserved for Boot Firmware (P1275)
    --               0xFF      = Not to be used
    g_PROGRAM_ID      : std_logic_vector(7 downto 0)    := c_PROGRAM_ID;

    -- Pointer to a user defined ASCII string
    g_ASCII_PTR       : std_logic_vector(23 downto 0)   := x"000000";

    -- User CR/CSR, CRAM & serial number pointers
    g_BEG_USER_CR     : std_logic_vector(23 downto 0)   := x"000000";
    g_END_USER_CR     : std_logic_vector(23 downto 0)   := x"000000";

    g_BEG_CRAM        : std_logic_vector(23 downto 0)   := x"000000"; -- 0x1003
    g_END_CRAM        : std_logic_vector(23 downto 0)   := x"000000"; -- 0x13ff

    g_BEG_USER_CSR    : std_logic_vector(23 downto 0)   := x"07ff33";
    g_END_USER_CSR    : std_logic_vector(23 downto 0)   := x"07ff5f";

    g_BEG_SN          : std_logic_vector(23 downto 0)   := x"000000";
    g_END_SN          : std_logic_vector(23 downto 0)   := x"000000";

    -- Function 0
    g_F0_ADEM         : std_logic_vector( 31 downto 0)  := x"ff000000";
    g_F0_AMCAP        : std_logic_vector( 63 downto 0)  := x"00000000_0000ff00";
    g_F0_DAWPR        : std_logic_vector(  7 downto 0)  := x"84";

    -- Function 1
    g_F1_ADEM         : std_logic_vector( 31 downto 0)  := x"fff80000";
    g_F1_AMCAP        : std_logic_vector( 63 downto 0)  := x"ff000000_00000000";
    g_F1_DAWPR        : std_logic_vector(  7 downto 0)  := x"84";

    -- Function 2
    g_F2_ADEM         : std_logic_vector( 31 downto 0)  := x"00000000";
    g_F2_AMCAP        : std_logic_vector( 63 downto 0)  := x"00000000_00000000";
    g_F2_DAWPR        : std_logic_vector(  7 downto 0)  := x"84";

    -- Function 3
    g_F3_ADEM         : std_logic_vector( 31 downto 0)  := x"00000000";
    g_F3_AMCAP        : std_logic_vector( 63 downto 0)  := x"00000000_00000000";
    g_F3_DAWPR        : std_logic_vector(  7 downto 0)  := x"84";

    -- Function 4
    g_F4_ADEM         : std_logic_vector( 31 downto 0)  := x"00000000";
    g_F4_AMCAP        : std_logic_vector( 63 downto 0)  := x"00000000_00000000";
    g_F4_DAWPR        : std_logic_vector(  7 downto 0)  := x"84";

    -- Function 5
    g_F5_ADEM         : std_logic_vector( 31 downto 0)  := x"00000000";
    g_F5_AMCAP        : std_logic_vector( 63 downto 0)  := x"00000000_00000000";
    g_F5_DAWPR        : std_logic_vector(  7 downto 0)  := x"84";

    -- Function 6
    g_F6_ADEM         : std_logic_vector( 31 downto 0)  := x"00000000";
    g_F6_AMCAP        : std_logic_vector( 63 downto 0)  := x"00000000_00000000";
    g_F6_DAWPR        : std_logic_vector(  7 downto 0)  := x"84";

    -- Function 7
    g_F7_ADEM         : std_logic_vector( 31 downto 0)  := x"00000000";
    g_F7_AMCAP        : std_logic_vector( 63 downto 0)  := x"00000000_00000000";
    g_F7_DAWPR        : std_logic_vector(  7 downto 0)  := x"84"
  );
  port (
    clk_i           : in  std_logic;
    rst_n_i         : in  std_logic;
    rst_n_o         : out std_logic; -- To wishbone

    -- VME
    VME_AS_n_i      : in  std_logic;
    VME_RST_n_i     : in  std_logic;  -- asserted when '0'
    VME_WRITE_n_i   : in  std_logic;
    VME_AM_i        : in  std_logic_vector(5 downto 0);
    VME_DS_n_i      : in  std_logic_vector(1 downto 0);
    VME_GA_i        : in  std_logic_vector(5 downto 0);
    VME_BERR_o      : out std_logic;  -- [In the VME standard this line is
                                      -- asserted when low. Here is asserted
                                      -- when high indeed the logic will be
                                      -- inverted again in the VME transceivers
                                      -- on the board]*.
    VME_DTACK_n_o   : out std_logic;
    VME_RETRY_n_o   : out std_logic;
    VME_LWORD_n_i   : in  std_logic;
    VME_LWORD_n_o   : out std_logic;
    VME_ADDR_i      : in  std_logic_vector(31 downto 1);
    VME_ADDR_o      : out std_logic_vector(31 downto 1);
    VME_DATA_i      : in  std_logic_vector(31 downto 0);
    VME_DATA_o      : out std_logic_vector(31 downto 0);
    VME_IRQ_o       : out std_logic_vector( 7 downto 1);   -- the same as []*
    VME_IACKIN_n_i  : in  std_logic;
    VME_IACK_n_i    : in  std_logic;
    VME_IACKOUT_n_o : out std_logic;

    -- VME buffers
    VME_DTACK_OE_o  : out std_logic;
    VME_DATA_DIR_o  : out std_logic;
    VME_DATA_OE_N_o : out std_logic;
    VME_ADDR_DIR_o  : out std_logic;
    VME_ADDR_OE_N_o : out std_logic;
    VME_RETRY_OE_o  : out std_logic;

    -- WishBone
    DAT_i   : in  std_logic_vector(g_WB_DATA_WIDTH-1 downto 0);
    DAT_o   : out std_logic_vector(g_WB_DATA_WIDTH-1 downto 0);
    ADR_o   : out std_logic_vector(g_WB_ADDR_WIDTH-1 downto 0);
    CYC_o   : out std_logic;
    ERR_i   : in  std_logic;
    SEL_o   : out std_logic_vector(g_WB_DATA_WIDTH/8-1 downto 0);
    STB_o   : out std_logic;
    ACK_i   : in  std_logic;
    WE_o    : out std_logic;
    STALL_i : in  std_logic;

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
    user_cr_data_i  : in  std_logic_vector( 7 downto 0) := (others => '0');

    -- Functions
    function_o      : out std_logic_vector( 3 downto 0);

    -- IRQ Generator
    irq_ack_o : out std_logic;    -- when the IRQ controller acknowledges the
                                  -- Interrupt cycle it sends a pulse to the
                                  -- IRQ Generator

    irq_i     : in  std_logic     -- Interrupt request; the IRQ Generator/your
                                  -- Wb application sends a pulse to the IRQ
                                  -- Controller which asserts one of the IRQ
                                  -- lines.
  );

end VME64xCore_Top;

architecture RTL of VME64xCore_Top is

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
  signal s_function             : std_logic_vector( 2 downto 0);
  signal s_am                   : std_logic_vector( 5 downto 0);

  -- Oversampled input signals
  constant nbr_stages : natural := 2; -- Number of stages for synchronizers
  signal s_VME_RST_n            : std_logic_vector(nbr_stages - 1 downto 0);
  signal s_VME_AS_n             : std_logic_vector(nbr_stages - 1 downto 0);
  signal s_VME_WRITE_n          : std_logic_vector(nbr_stages - 1 downto 0);
  signal s_VME_DS_n             : std_logic_vector(2 * nbr_stages - 1 downto 0);
  signal s_VME_IACK_n           : std_logic_vector(nbr_stages - 1 downto 0);
  signal s_VME_IACKIN_n         : std_logic_vector(nbr_stages - 1 downto 0);

  -- CR/CSR parameter arrays
  -- ADEM array has an extra index (-1) to simplify looping while checking the
  -- EFM bit of the previous function.
  constant c_ADEM : t_adem_array(0 to 7) := (
    g_F0_ADEM, g_F1_ADEM, g_F2_ADEM, g_F3_ADEM,
    g_F4_ADEM, g_F5_ADEM, g_F6_ADEM, g_F7_ADEM
  );
  constant c_AMCAP : t_amcap_array(0 to 7) := (
    g_F0_AMCAP, g_F1_AMCAP, g_F2_AMCAP, g_F3_AMCAP,
    g_F4_AMCAP, g_F5_AMCAP, g_F6_AMCAP, g_F7_AMCAP
  );
  constant c_DAWPR : t_dawpr_array(0 to 7) := (
    g_F0_DAWPR, g_F1_DAWPR, g_F2_DAWPR, g_F3_DAWPR,
    g_F4_DAWPR, g_F5_DAWPR, g_F6_DAWPR, g_F7_DAWPR
  );

  -- List of supported AM.
  constant c_AMCAP_ALLOWED : std_logic_vector(63 downto 0) :=
    (16#3c# to 16#3f# => '1', --  A24
     16#38# to 16#3b# => '1',
     16#2d# | 16#29#  => '1', --  A16
     16#0c# to 16#0f# => '1', --  A32
     16#08# to 16#0b# => '1',
     others => '0');
begin
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
  -- Input oversampling & edge detection; oversampling the input data is
  -- necessary to avoid metastability problems. With 3 samples the probability
  -- of metastability problem will be very low but of course the transfer rate
  -- will be slow down a little.
  process (clk_i)
  begin
    if rising_edge(clk_i) then
      s_VME_RST_n    <=     VME_RST_n_i
                        & s_VME_RST_n(nbr_stages - 1 downto 1);
      s_VME_AS_n     <=     VME_AS_n_i
                        & s_VME_AS_n(nbr_stages - 1 downto 1);
      s_VME_WRITE_n  <=     VME_WRITE_n_i
                        & s_VME_WRITE_n(nbr_stages - 1 downto 1);
      s_VME_DS_n     <=     VME_DS_n_i
                        & s_VME_DS_n(2 * nbr_stages - 1 downto 2);
      s_VME_IACK_n   <=     VME_IACK_n_i
                        & s_VME_IACK_n(nbr_stages - 1 downto 1);
      s_VME_IACKIN_n <=     VME_IACKIN_n_i
                        & s_VME_IACKIN_n(nbr_stages - 1 downto 1);
    end if;
  end process;

  ------------------------------------------------------------------------------
  -- VME Bus
  ------------------------------------------------------------------------------
  Inst_VME_bus : entity work.VME_bus
    generic map (
      g_CLOCK_PERIOD  => g_CLOCK_PERIOD,
      g_WB_DATA_WIDTH => g_WB_DATA_WIDTH,
      g_WB_ADDR_WIDTH => g_WB_ADDR_WIDTH
    )
    port map (
      clk_i           => clk_i,
      rst_i           => s_reset,

      -- VME
      VME_AS_n_i      => s_VME_AS_n(0),
      VME_LWORD_n_o   => VME_LWORD_n_o,
      VME_LWORD_n_i   => VME_LWORD_n_i,
      VME_RETRY_n_o   => VME_RETRY_n_o,
      VME_RETRY_OE_o  => VME_RETRY_OE_o,
      VME_WRITE_n_i   => s_VME_WRITE_n(0),
      VME_DS_n_i      => s_VME_DS_n(1 downto 0),
      VME_DTACK_n_o   => VME_DTACK_n_o,
      VME_DTACK_OE_o  => VME_DTACK_OE_o,
      VME_BERR_n_o    => s_vme_berr_n,
      VME_ADDR_i      => VME_ADDR_i,
      VME_ADDR_o      => VME_ADDR_o,
      VME_ADDR_DIR_o  => VME_ADDR_DIR_o,
      VME_ADDR_OE_N_o => VME_ADDR_OE_N_o,
      VME_DATA_i      => VME_DATA_i,
      VME_DATA_o      => VME_DATA_o,
      VME_DATA_DIR_o  => VME_DATA_DIR_o,
      VME_DATA_OE_N_o => VME_DATA_OE_N_o,
      VME_AM_i        => VME_AM_i,
      VME_IACK_n_i    => s_VME_IACK_n(0),
      VME_IACKIN_n_i  => s_VME_IACKIN_n(0),
      VME_IACKOUT_n_o => VME_IACKOUT_n_o,

      -- WB signals
      stb_o           => STB_o,
      ack_i           => ACK_i,
      dat_o           => DAT_o,
      dat_i           => DAT_i,
      adr_o           => ADR_o,
      sel_o           => SEL_o,
      we_o            => WE_o,
      cyc_o           => CYC_o,
      err_i           => ERR_i,
      stall_i         => STALL_i,

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

  s_reset    <= (not rst_n_i) or (not s_VME_RST_n(0));
  s_reset_n  <= not s_reset;
  rst_n_o    <= not (s_reset or s_module_reset);

  VME_BERR_o <= not s_vme_berr_n; -- The VME_BERR is asserted when '1' because
                                  -- the buffers on the board invert the logic.

  Inst_VME_Funct_Match : entity work.VME_Funct_Match
    generic map (
      g_ADEM      => c_ADEM,
      g_AMCAP     => c_AMCAP
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
      decode_done_o  => s_decode_done,
      function_o     => s_function
    );

  function_o (2 downto 0) <= s_function;
  function_o (3) <= '0'; --  FIXME: purpose of that bit ?

  ------------------------------------------------------------------------------
  -- Output
  ------------------------------------------------------------------------------
  VME_IRQ_o  <= not s_VME_IRQ_n_o;  -- The buffers will invert again the logic level
  irq_ack_o  <= s_irq_ack;

  ------------------------------------------------------------------------------
  --  Interrupter
  ------------------------------------------------------------------------------
  Inst_VME_IRQ_Controller : entity work.VME_IRQ_Controller
    generic map (
      g_RETRY_TIMEOUT => 1000000/g_CLOCK_PERIOD     -- 1ms timeout
    )
    port map (
      clk_i           => clk_i,
      reset_n_i       => s_reset_n,                 -- asserted when low
      INT_Level_i     => s_irq_level,
      INT_Req_i       => irq_i,
      irq_pending_o   => s_irq_pending,
      irq_ack_i       => s_irq_ack,
      VME_IRQ_n_o     => s_VME_IRQ_n_o
    );

  ------------------------------------------------------------------------------
  -- CR/CSR space
  ------------------------------------------------------------------------------
  Inst_VME_CR_CSR_Space : entity work.VME_CR_CSR_Space
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

      vme_ga_i            => VME_GA_i,
      vme_berr_n_i        => s_vme_berr_n,
      bar_o               => s_bar,
      vme_sysfail_i       => '0',
      vme_sysfail_ena_o   => open,
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
    Inst_VME_User_CSR : entity work.VME_User_CSR
      generic map (
        g_WB_DATA_WIDTH => g_WB_DATA_WIDTH
      )
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

end RTL;
