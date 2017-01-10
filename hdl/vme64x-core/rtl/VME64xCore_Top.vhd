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
--      ________________________________________________________________
--     |                     VME64xCore_Top.vhd                         |
--     |__      ____________________                __________________  |
--     |  |    |                    |              |                  | |
--     |S |    |    VME_bus.vhd     |              |                  | |
--   V |A |    |                    |              |VME_to_WB_FIFO.vhd| |
--   M |M |    |         |          |              |(not implemented) | |
--   E |P |    |  VME    |    WB    |              |                  | |  W
--     |L |    | slave   |  master  |              |                  | |  B
--   B |I |    |         |          |   _______    |                  | |
--   U |N |    |         |          |  | CSR   |   |                  | |  B
--   S |G |    |         |          |  |______ |   |__________________| |  U
--     |  |    |                    |  |       |    _________________   |  S
--     |  |    |                    |  |CRAM   |   |                 |  |
--     |__|    |                    |  |______ |   |  IRQ_Controller |  |
--     |       |                    |  |       |   |                 |  |
--     |       |                    |  | CR    |   |                 |  |
--     |       |____________________|  |_______|   |_________________|  |
--     |________________________________________________________________|
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
    g_clock          : integer  := c_clk_period;    -- Clock period (ns)
    g_wb_data_width  : integer  := c_width;         -- WB data width: must be 32 or 64
    g_wb_addr_width  : integer  := c_addr_width;    -- WB address width: 64 or less

    -- Manufacturer ID: IEEE OUID
    --                  e.g. CERN is 0x080030
    g_manufacturer_id : std_logic_vector(23 downto 0)   := x"000000";

    -- Board ID: Per manufacturer, each board shall have an unique ID
    --           e.g. SVEC = 408 (CERN IDs: http://cern.ch/boardid)
    g_board_id        : std_logic_vector(31 downto 0)   := x"00000000";

    -- Revision ID: user defined revision code
    g_revision_id     : std_logic_vector(31 downto 0)   := x"00000000";

    -- Program ID: Defined per AV1:
    --               0x00      = Not used
    --               0x01      = No program, ID ROM only
    --               0x02-0x4F = Manufacturer defined
    --               0x50-0x7F = User defined
    --               0x80-0xEF = Reserved for future use
    --               0xF0-0xFE = Reserved for Boot Firmware (P1275)
    --               0xFF      = Not to be used
    g_program_id      : std_logic_vector(7 downto 0)    := x"00";

    -- Pointer to a user defined ASCII string
    g_ascii_ptr       : std_logic_vector(23 downto 0)   := x"000000";

    -- User CR/CSR, CRAM & serial number pointers
    g_beg_user_cr     : std_logic_vector(23 downto 0)   := x"000000";
    g_end_user_cr     : std_logic_vector(23 downto 0)   := x"000000";

    g_beg_cram        : std_logic_vector(23 downto 0)   := x"001000";
    g_end_cram        : std_logic_vector(23 downto 0)   := x"0013ff";

    g_beg_user_csr    : std_logic_vector(23 downto 0)   := x"000000";
    g_end_user_csr    : std_logic_vector(23 downto 0)   := x"000000";

    g_beg_sn          : std_logic_vector(23 downto 0)   := x"000000";
    g_end_sn          : std_logic_vector(23 downto 0)   := x"000000";

    -- Function 0
    g_f0_adem         : std_logic_vector( 31 downto 0)  := x"ff000000";
    g_f0_amcap        : std_logic_vector( 63 downto 0)  := x"00000000_0000bb00";
    g_f0_xamcap       : std_logic_vector(255 downto 0)  := x"00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000";
    g_f0_dawpr        : std_logic_vector(  7 downto 0)  := x"84";

    -- Function 1
    g_f1_adem         : std_logic_vector( 31 downto 0)  := x"00000000";
    g_f1_amcap        : std_logic_vector( 63 downto 0)  := x"00000000_00000000";
    g_f1_xamcap       : std_logic_vector(255 downto 0)  := x"00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000";
    g_f1_dawpr        : std_logic_vector(  7 downto 0)  := x"84";

    -- Function 2
    g_f2_adem         : std_logic_vector( 31 downto 0)  := x"00000000";
    g_f2_amcap        : std_logic_vector( 63 downto 0)  := x"00000000_00000000";
    g_f2_xamcap       : std_logic_vector(255 downto 0)  := x"00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000";
    g_f2_dawpr        : std_logic_vector(  7 downto 0)  := x"84";

    -- Function 3
    g_f3_adem         : std_logic_vector( 31 downto 0)  := x"00000000";
    g_f3_amcap        : std_logic_vector( 63 downto 0)  := x"00000000_00000000";
    g_f3_xamcap       : std_logic_vector(255 downto 0)  := x"00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000";
    g_f3_dawpr        : std_logic_vector(  7 downto 0)  := x"84";

    -- Function 4
    g_f4_adem         : std_logic_vector( 31 downto 0)  := x"00000000";
    g_f4_amcap        : std_logic_vector( 63 downto 0)  := x"00000000_00000000";
    g_f4_xamcap       : std_logic_vector(255 downto 0)  := x"00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000";
    g_f4_dawpr        : std_logic_vector(  7 downto 0)  := x"84";

    -- Function 5
    g_f5_adem         : std_logic_vector( 31 downto 0)  := x"00000000";
    g_f5_amcap        : std_logic_vector( 63 downto 0)  := x"00000000_00000000";
    g_f5_xamcap       : std_logic_vector(255 downto 0)  := x"00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000";
    g_f5_dawpr        : std_logic_vector(  7 downto 0)  := x"84";

    -- Function 6
    g_f6_adem         : std_logic_vector( 31 downto 0)  := x"00000000";
    g_f6_amcap        : std_logic_vector( 63 downto 0)  := x"00000000_00000000";
    g_f6_xamcap       : std_logic_vector(255 downto 0)  := x"00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000";
    g_f6_dawpr        : std_logic_vector(  7 downto 0)  := x"84";

    -- Function 7
    g_f7_adem         : std_logic_vector( 31 downto 0)  := x"00000000";
    g_f7_amcap        : std_logic_vector( 63 downto 0)  := x"00000000_00000000";
    g_f7_xamcap       : std_logic_vector(255 downto 0)  := x"00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000";
    g_f7_dawpr        : std_logic_vector(  7 downto 0)  := x"84"
  );
  port (
    clk_i           : in  std_logic;
    rst_n_i         : in  std_logic;

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
    VME_IRQ_o       : out std_logic_vector(6 downto 0);   -- the same as []*
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
    DAT_i   : in  std_logic_vector(g_wb_data_width-1 downto 0);
    DAT_o   : out std_logic_vector(g_wb_data_width-1 downto 0);
    ADR_o   : out std_logic_vector(g_wb_addr_width-1 downto 0);
    CYC_o   : out std_logic;
    ERR_i   : in  std_logic;
    RTY_i   : in  std_logic;
    SEL_o   : out std_logic_vector(g_wb_data_width/8-1 downto 0);
    STB_o   : out std_logic;
    ACK_i   : in  std_logic;
    WE_o    : out std_logic;
    STALL_i : in  std_logic;

    -- IRQ Generator
    INT_ack_o : out std_logic;    -- when the IRQ controller acknowledges the
                                  -- Interrupt cycle it sends a pulse to the
                                  -- IRQ Generator

    IRQ_i     : in  std_logic     -- Interrupt request; the IRQ Generator/your
                                  -- Wb application sends a pulse to the IRQ
                                  -- Controller which asserts one of the IRQ
                                  -- lines.
    );

end VME64xCore_Top;

architecture RTL of VME64xCore_Top is

  signal s_CRAMdataOut         : std_logic_vector(7 downto 0);
  signal s_CRAMaddr            : std_logic_vector(f_log2_size(f_size(g_beg_cram, g_end_cram))-1 downto 0);
  signal s_CRAMdataIn          : std_logic_vector(7 downto 0);
  signal s_CRAMwea             : std_logic;
  signal s_CRaddr              : std_logic_vector(11 downto 0);
  signal s_CRdata              : std_logic_vector(7 downto 0);
  signal s_RW                  : std_logic;
  signal s_reset               : std_logic;
  signal s_IRQlevelReg         : std_logic_vector(7 downto 0);
  signal s_FIFOreset           : std_logic;
  signal s_VME_DATA_IRQ        : std_logic_vector(31 downto 0);
  signal s_VME_DATA_VMEbus     : std_logic_vector(31 downto 0);
  -- signal s_VME_DATA_b          : std_logic_vector(31 downto 0);
  -- signal s_fifo                : std_logic;
  signal s_VME_DTACK_VMEbus    : std_logic;
  signal s_VME_DTACK_IRQ       : std_logic;
  signal s_VME_DTACK_OE_VMEbus : std_logic;
  signal s_VME_DTACK_OE_IRQ    : std_logic;
  signal s_VME_DATA_DIR_VMEbus : std_logic;
  signal s_VME_DATA_DIR_IRQ    : std_logic;
  signal s_INT_Level           : std_logic_vector(7 downto 0);
  signal s_INT_Vector          : std_logic_vector(7 downto 0);
  signal s_VME_IRQ_n_o         : std_logic_vector(6 downto 0);
  signal s_reset_IRQ           : std_logic;
  signal s_CSRData_o           : std_logic_vector(7 downto 0);
  signal s_CSRData_i           : std_logic_vector(7 downto 0);
  signal s_CrCsrOffsetAddr     : std_logic_vector(18 downto 0);
  signal s_Ader0               : std_logic_vector(31 downto 0);
  signal s_Ader1               : std_logic_vector(31 downto 0);
  signal s_Ader2               : std_logic_vector(31 downto 0);
  signal s_Ader3               : std_logic_vector(31 downto 0);
  signal s_Ader4               : std_logic_vector(31 downto 0);
  signal s_Ader5               : std_logic_vector(31 downto 0);
  signal s_Ader6               : std_logic_vector(31 downto 0);
  signal s_Ader7               : std_logic_vector(31 downto 0);
  signal s_en_wr_CSR           : std_logic;
  signal s_err_flag            : std_logic;
  signal s_reset_flag          : std_logic;
  signal s_Sw_Reset            : std_logic;
  signal s_ModuleEnable        : std_logic;
  signal s_Endian              : std_logic_vector(2 downto 0);
  signal s_BAR                 : std_logic_vector(4 downto 0);

  -- Oversampled input signals
  signal s_VME_RST_n              : std_logic_vector(2 downto 0);
  signal s_VME_AS_n               : std_logic_vector(2 downto 0);
  signal s_VME_WRITE_n            : std_logic_vector(2 downto 0);
  signal s_VME_DS_n               : std_logic_vector(5 downto 0);
  signal s_VME_IACK_n             : std_logic_vector(2 downto 0);
  signal s_VME_IACKIN_n           : std_logic_vector(2 downto 0);

begin

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
      s_VME_RST_n    <= s_VME_RST_n(1 downto 0)    & VME_RST_n_i;
      s_VME_AS_n     <= s_VME_AS_n(1 downto 0)     & VME_AS_n_i;
      s_VME_WRITE_n  <= s_VME_WRITE_n(1 downto 0)  & VME_WRITE_n_i;
      s_VME_DS_n     <= s_VME_DS_n(3 downto 0)     & VME_DS_n_i;
      s_VME_IACK_n   <= s_VME_IACK_n(1 downto 0)   & VME_IACK_n_i;
      s_VME_IACKIN_n <= s_VME_IACKIN_n(1 downto 0) & VME_IACKIN_n_i;
    end if;
  end process;

  ------------------------------------------------------------------------------
  -- VME Bus
  ------------------------------------------------------------------------------
  Inst_VME_bus : VME_bus
    generic map (
      g_clock         => g_clock,
      g_wb_data_width => g_wb_data_width,
      g_wb_addr_width => g_wb_addr_width,
      g_beg_user_cr   => g_beg_user_cr,
      g_end_user_cr   => g_end_user_cr,
      g_beg_cram      => g_beg_cram,
      g_end_cram      => g_end_cram,
      g_beg_user_csr  => g_beg_user_csr,
      g_end_user_csr  => g_end_user_csr,
      g_f0_adem       => g_f0_adem,
      g_f0_amcap      => g_f0_amcap,
      g_f0_xamcap     => g_f0_xamcap,
      g_f1_adem       => g_f1_adem,
      g_f1_amcap      => g_f1_amcap,
      g_f1_xamcap     => g_f1_xamcap,
      g_f2_adem       => g_f2_adem,
      g_f2_amcap      => g_f2_amcap,
      g_f2_xamcap     => g_f2_xamcap,
      g_f3_adem       => g_f3_adem,
      g_f3_amcap      => g_f3_amcap,
      g_f3_xamcap     => g_f3_xamcap,
      g_f4_adem       => g_f4_adem,
      g_f4_amcap      => g_f4_amcap,
      g_f4_xamcap     => g_f4_xamcap,
      g_f5_adem       => g_f5_adem,
      g_f5_amcap      => g_f5_amcap,
      g_f5_xamcap     => g_f5_xamcap,
      g_f6_adem       => g_f6_adem,
      g_f6_amcap      => g_f6_amcap,
      g_f6_xamcap     => g_f6_xamcap,
      g_f7_adem       => g_f7_adem,
      g_f7_amcap      => g_f7_amcap,
      g_f7_xamcap     => g_f7_xamcap
    )
    port map (
      clk_i           => clk_i,
      rst_n_i         => rst_n_i,
      reset_o         => s_reset,       -- asserted when '1'

      -- VME
      VME_RST_n_i     => s_VME_RST_n(2),
      VME_AS_n_i      => s_VME_AS_n(2),
      VME_LWORD_n_o   => VME_LWORD_n_o,
      VME_LWORD_n_i   => VME_LWORD_n_i,
      VME_RETRY_n_o   => VME_RETRY_n_o,
      VME_RETRY_OE_o  => VME_RETRY_OE_o,
      VME_WRITE_n_i   => s_VME_WRITE_n(2),
      VME_DS_n_i      => s_VME_DS_n(5 downto 4),
      VME_DS_ant_n_i  => s_VME_DS_n(3 downto 2),
      VME_DTACK_n_o   => s_VME_DTACK_VMEbus,
      VME_DTACK_OE_o  => s_VME_DTACK_OE_VMEbus,
      VME_BERR_o      => VME_BERR_o,
      VME_ADDR_i      => VME_ADDR_i,
      VME_ADDR_o      => VME_ADDR_o,
      VME_ADDR_DIR_o  => VME_ADDR_DIR_o,
      VME_ADDR_OE_N_o => VME_ADDR_OE_N_o,
      VME_DATA_i      => VME_DATA_i,
      VME_DATA_o      => s_VME_DATA_VMEbus,
      VME_DATA_DIR_o  => s_VME_DATA_DIR_VMEbus,
      VME_DATA_OE_N_o => VME_DATA_OE_N_o,
      VME_AM_i        => VME_AM_i,
      VME_IACK_n_i    => s_VME_IACK_n(2),

      -- WB
      memReq_o        => STB_o,
      memAckWB_i      => ACK_i,
      wbData_o        => DAT_o,
      wbData_i        => DAT_i,
      locAddr_o       => ADR_o,
      wbSel_o         => SEL_o,
      RW_o            => s_RW,
      cyc_o           => CYC_o,
      err_i           => ERR_i,
      rty_i           => RTY_i,
      stall_i         => STALL_i,

      -- CR/CSR signals
      CRAMaddr_o      => s_CRAMaddr,
      CRAMdata_o      => s_CRAMdataIn,
      CRAMdata_i      => s_CRAMdataOut,
      CRAMwea_o       => s_CRAMwea,
      CRaddr_o        => s_CRaddr,
      CRdata_i        => s_CRdata,
      en_wr_CSR       => s_en_wr_CSR,
      CrCsrOffsetAddr => s_CrCsrOffsetAddr,
      CSRData_o       => s_CSRData_o,
      CSRData_i       => s_CSRData_i,
      err_flag_o      => s_err_flag,
      reset_flag_i    => s_reset_flag,
      Ader0           => s_Ader0,
      Ader1           => s_Ader1,
      Ader2           => s_Ader2,
      Ader3           => s_Ader3,
      Ader4           => s_Ader4,
      Ader5           => s_Ader5,
      Ader6           => s_Ader6,
      Ader7           => s_Ader7,
      ModuleEnable    => s_ModuleEnable,
      Endian_i        => s_Endian,
      Sw_Reset        => s_Sw_Reset,
      BAR_i           => s_BAR
    );

  ------------------------------------------------------------------------------
  -- Output
  ------------------------------------------------------------------------------
  VME_IRQ_o  <= not s_VME_IRQ_n_o;  -- The buffers will invert again the logic level
  WE_o       <= not s_RW;
  INT_ack_o  <= s_VME_DTACK_IRQ;

  -- Multiplexer added on the output signal used by either VMEbus.vhd and the
  -- IRQ_controller.vhd
  VME_DATA_o     <= s_VME_DATA_VMEbus
                    when s_VME_IACK_n(2) = '1'
                    else s_VME_DATA_IRQ;

  VME_DTACK_n_o  <= s_VME_DTACK_VMEbus and s_VME_DTACK_IRQ;
                    --when s_VME_IACK_n(2) = '1'
                    --else s_VME_DTACK_IRQ;

  VME_DTACK_OE_o <= s_VME_DTACK_OE_VMEbus or s_VME_DTACK_OE_IRQ;
                    --when s_VME_IACK_n(2) = '1'
                    --else s_VME_DTACK_OE_IRQ;

  VME_DATA_DIR_o <= s_VME_DATA_DIR_VMEbus
                    when s_VME_IACK_n(2) = '1'
                    else s_VME_DATA_DIR_IRQ;

  ------------------------------------------------------------------------------
  --  Interrupter
  ------------------------------------------------------------------------------
  Inst_VME_IRQ_Controller : VME_IRQ_Controller
    generic map (
      g_retry_timeout => 1000000/g_clock    -- 1ms timeout
    )
    port map (
      clk_i           => clk_i,
      reset_n_i       => s_reset_IRQ,       -- asserted when low
      VME_IACKIN_n_i  => s_VME_IACKIN_n(2),
      VME_AS_n_i      => s_VME_AS_n(2),
      VME_DS_n_i      => s_VME_DS_n(5 downto 4),
      VME_ADDR_123_i  => VME_ADDR_i(3 downto 1),
      INT_Level_i     => s_INT_Level,
      INT_Vector_i    => s_INT_Vector,
      INT_Req_i       => irq_i,
      VME_IRQ_n_o     => s_VME_IRQ_n_o,
      VME_IACKOUT_n_o => VME_IACKOUT_n_o,
      VME_DTACK_n_o   => s_VME_DTACK_IRQ,
      VME_DTACK_OE_o  => s_VME_DTACK_OE_IRQ,
      VME_DATA_o      => s_VME_DATA_IRQ,
      VME_DATA_DIR_o  => s_VME_DATA_DIR_IRQ
    );

  s_reset_IRQ <= not(s_reset);

  ------------------------------------------------------------------------------
  -- CR/CSR space
  ------------------------------------------------------------------------------
  Inst_VME_CR_CSR_Space : VME_CR_CSR_Space
    generic map (
      g_cram_size        => f_size(g_beg_cram, g_end_cram),
      g_wb_data_width    => g_wb_data_width,
      g_cr_space         => f_vme_cr_encode(
        g_manufacturer_id, g_board_id, g_revision_id, g_program_id,
        g_ascii_ptr,
        g_beg_user_cr, g_end_user_cr,
        g_beg_cram, g_end_cram,
        g_beg_user_csr, g_end_user_csr,
        g_beg_sn, g_end_sn,
        g_f0_adem, g_f0_amcap, g_f0_xamcap, g_f0_dawpr,
        g_f1_adem, g_f1_amcap, g_f1_xamcap, g_f1_dawpr,
        g_f2_adem, g_f2_amcap, g_f2_xamcap, g_f2_dawpr,
        g_f3_adem, g_f3_amcap, g_f3_xamcap, g_f3_dawpr,
        g_f4_adem, g_f4_amcap, g_f4_xamcap, g_f4_dawpr,
        g_f5_adem, g_f5_amcap, g_f5_xamcap, g_f5_dawpr,
        g_f6_adem, g_f6_amcap, g_f6_xamcap, g_f6_dawpr,
        g_f7_adem, g_f7_amcap, g_f7_xamcap, g_f7_dawpr
      )
    )
    port map (
      clk_i              => clk_i,
      reset              => s_reset,
      CR_addr            => s_CRaddr,
      CR_data            => s_CRdata,
      CRAM_addr          => s_CRAMaddr,
      CRAM_data_o        => s_CRAMdataOut,
      CRAM_data_i        => s_CRAMdataIn,
      CRAM_Wen           => s_CRAMwea,
      en_wr_CSR          => s_en_wr_CSR,
      CrCsrOffsetAddr    => s_CrCsrOffsetAddr,
      VME_GA_oversampled => VME_GA_i,
      locDataIn          => s_CSRData_o,
      err_flag           => s_err_flag,
      reset_flag         => s_reset_flag,
      CSRdata            => s_CSRData_i,
      Ader0              => s_Ader0,
      Ader1              => s_Ader1,
      Ader2              => s_Ader2,
      Ader3              => s_Ader3,
      Ader4              => s_Ader4,
      Ader5              => s_Ader5,
      Ader6              => s_Ader6,
      Ader7              => s_Ader7,
      ModuleEnable       => s_ModuleEnable,
      Sw_Reset           => s_Sw_Reset,
      Endian_o           => s_Endian,
      BAR_o              => s_BAR,
      INT_Level          => s_INT_Level,
      numBytes           => (others => '0'),
      transfTime         => (others => '0'),
      INT_Vector         => s_INT_Vector
    );

end RTL;
