--------------------------------------------------------------------------------
-- CERN (BE-CO-HT)
-- VME64x Core
-- http://www.ohwr.org/projects/vme64x-core
--------------------------------------------------------------------------------
--
-- unit name:     vme64x_pack (vme64x_pack.vhd)
--
-- author:        Pablo Alvarez Sanchez <pablo.alvarez.sanchez@cern.ch>
--                Davide Pedretti       <davide.pedretti@cern.ch>
--
-- description:   VME64x Core Package
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

package vme64x_pack is

  ------------------------------------------------------------------------------
  -- Constants
  ------------------------------------------------------------------------------

  constant c_CLOCK_PERIOD     : integer := 10;  -- Clock period (ns)
  constant c_DATA_WIDTH       : integer := 64;  -- WB data width: must be 32 or 64
  constant c_ADDR_WIDTH       : integer := 9;   -- WB addr width: 64 or less

  -- Default boards IDs
  constant c_SVEC_ID          : std_logic_vector(31 downto 0) := x"00000198";
  constant c_CERN_ID          : std_logic_vector(23 downto 0) := x"080030";
  constant c_REVISION_ID      : std_logic_vector(31 downto 0) := x"00000001";
  constant c_PROGRAM_ID       : std_logic_vector( 7 downto 0) := x"5a";

  -- Bits in ADEM/ADER registers
  subtype  c_ADEM_M           is integer range 31 downto  8;
  constant c_ADEM_M_PAD       : std_logic_vector(7 downto 0) := (others => '0');
  constant c_ADEM_FAF         : integer := 3;
  constant c_ADEM_DFS         : integer := 2;
  constant c_ADEM_EFD         : integer := 1;
  constant c_ADEM_EFM         : integer := 0;

  subtype  c_ADER_C_XAM       is integer range 31 downto 10;
  constant c_ADER_C_XAM_PAD   : std_logic_vector(9 downto 0) := (others => '0');
  subtype  c_ADER_C_AM        is integer range 31 downto  8;
  constant c_ADER_C_AM_PAD    : std_logic_vector(7 downto 0) := (others => '0');
  subtype  c_ADER_AM          is integer range  7 downto  2;
  subtype  c_ADER_XAM         is integer range  9 downto  2;
  constant c_ADER_DFSR        : integer := 1;
  constant c_ADER_XAM_MODE    : integer := 0;

  -- AM table.
  -- References:
  -- Table 2-3 "Address Modifier Codes" pages 21/22 VME64std ANSI/VITA 1-1994
  -- Table 2.4 "Extended Address Modifier Code" page 12 2eSST ANSI/VITA 1.5-2003(R2009)
  constant c_AM_A24_S_SUP     : std_logic_vector(5 downto 0) := "111101";    -- 0x3d
  constant c_AM_A24_S         : std_logic_vector(5 downto 0) := "111001";    -- 0x39
  constant c_AM_A24_BLT       : std_logic_vector(5 downto 0) := "111011";    -- 0x3b
  constant c_AM_A24_BLT_SUP   : std_logic_vector(5 downto 0) := "111111";    -- 0x3f
  constant c_AM_A24_MBLT      : std_logic_vector(5 downto 0) := "111000";    -- 0x38
  constant c_AM_A24_MBLT_SUP  : std_logic_vector(5 downto 0) := "111100";    -- 0x3c
  constant c_AM_A24_LCK       : std_logic_vector(5 downto 0) := "110010";    -- 0x32
  constant c_AM_CR_CSR        : std_logic_vector(5 downto 0) := "101111";    -- 0x2f
  constant c_AM_A16           : std_logic_vector(5 downto 0) := "101001";    -- 0x29
  constant c_AM_A16_SUP       : std_logic_vector(5 downto 0) := "101101";    -- 0x2d
  constant c_AM_A16_LCK       : std_logic_vector(5 downto 0) := "101100";    -- 0x2c
  constant c_AM_A32           : std_logic_vector(5 downto 0) := "001001";    -- 0x09
  constant c_AM_A32_SUP       : std_logic_vector(5 downto 0) := "001101";    -- 0x0d
  constant c_AM_A32_BLT       : std_logic_vector(5 downto 0) := "001011";    -- 0x0b
  constant c_AM_A32_BLT_SUP   : std_logic_vector(5 downto 0) := "001111";    -- 0x0f
  constant c_AM_A32_MBLT      : std_logic_vector(5 downto 0) := "001000";    -- 0x08
  constant c_AM_A32_MBLT_SUP  : std_logic_vector(5 downto 0) := "001100";    -- 0x0c
  constant c_AM_A32_LCK       : std_logic_vector(5 downto 0) := "000101";    -- 0x05
  constant c_AM_A64           : std_logic_vector(5 downto 0) := "000001";    -- 0x01
  constant c_AM_A64_BLT       : std_logic_vector(5 downto 0) := "000011";    -- 0x03
  constant c_AM_A64_MBLT      : std_logic_vector(5 downto 0) := "000000";    -- 0x00
  constant c_AM_A64_LCK       : std_logic_vector(5 downto 0) := "000100";    -- 0x04
  constant c_AM_2EVME_6U      : std_logic_vector(5 downto 0) := "100000";    -- 0x20
  constant c_AM_2EVME_3U      : std_logic_vector(5 downto 0) := "100001";    -- 0x21

  constant c_AM_A32_2EVME     : std_logic_vector(7 downto 0) := "00000001";  -- 0x01
  constant c_AM_A64_2EVME     : std_logic_vector(7 downto 0) := "00000010";  -- 0x02
  constant c_AM_A32_2ESST     : std_logic_vector(7 downto 0) := "00010001";  -- 0x11
  constant c_AM_A64_2ESST     : std_logic_vector(7 downto 0) := "00010010";  -- 0x12

  ------------------------------------------------------------------------------
  -- Types
  ------------------------------------------------------------------------------

  -- CR/CSR parameter arrays
  type t_adem_array   is array (integer range <>) of std_logic_vector( 31 downto 0);
  type t_ader_array   is array (integer range <>) of std_logic_vector( 31 downto 0);
  type t_amcap_array  is array (integer range <>) of std_logic_vector( 63 downto 0);
  type t_xamcap_array is array (integer range <>) of std_logic_vector(255 downto 0);
  type t_dawpr_array  is array (integer range <>) of std_logic_vector(  7 downto 0);

  type t_typeOfDataTransfer is (
    D08_0,
    D08_1,
    D08_2,
    D08_3,
    D16_01,
    D16_23,
    D32,
    D64,
    TypeError
  );

  type t_addressingType is (
    A24,
    A24_BLT,
    A24_MBLT,
    CR_CSR,
    A16,
    A32,
    A32_BLT,
    A32_MBLT,
    A64,
    A64_BLT,
    A64_MBLT,
    TWOedge,
    AM_Error
  );

  type t_transferType is (
    SINGLE,
    BLT,
    MBLT,
    TWOe,
    error
  );

  type t_XAMtype is (
    A32_2eVME,
    A64_2eVME,
    A32_2eSST,
    A64_2eSST,
    A32_2eSSTb,
    A64_2eSSTb,
    XAM_error
  );

  type t_2eType is (
    TWOe_VME,
    TWOe_SST
  );

  type t_mainFSMstates is (
    IDLE,
    DECODE_ACCESS,
    WAIT_FOR_DS,
    LATCH_DS1,
    LATCH_DS2,
    LATCH_DS3,
    LATCH_DS4,
    CHECK_TRANSFER_TYPE,
    MEMORY_REQ,
    DATA_TO_BUS,
    DTACK_LOW,
    DECIDE_NEXT_CYCLE,
    INCREMENT_ADDR,
    SET_DATA_PHASE
    --UGLY_WAIT_TO_MAKE_DECODING_WORK
    -- uncomment for using 2e modes:
    --WAIT_FOR_DS_2e,
    --ADDR_PHASE_1,
    --ADDR_PHASE_2,
    --ADDR_PHASE_3,
    --DECODE_ACCESS_2e,
    --DTACK_PHASE_1,
    --DTACK_PHASE_2,
    --DTACK_PHASE_3,
    --TWOeVME_WRITE,
    --TWOeVME_READ,
    --TWOeVME_MREQ_RD,
    --WAIT_WR_1,
    --WAIT_WR_2,
    --WAIT_WB_ACK_WR,
    --WAIT_WB_ACK_RD,
    --TWOeVME_TOGGLE_WR,
    --TWOeVME_TOGGLE_RD,
    --TWOe_FIFO_WRITE,
    --TWOe_TOGGLE_DTACK,
    --TWOeVME_INCR_ADDR,
    --TWOe_WAIT_FOR_DS1,
    --TWOe_FIFO_WAIT_READ,
    --TWOe_FIFO_READ,
    --TWOe_CHECK_BEAT,
    --TWOe_RELEASE_DTACK,
    --TWOe_END_1,
    --TWOe_END_2
  );

  ------------------------------------------------------------------------------
  -- Components
  ------------------------------------------------------------------------------

  component VME64xCore_Top
    generic (
      g_CLOCK_PERIOD    : integer                         := c_CLOCK_PERIOD;
      g_WB_DATA_WIDTH   : integer                         := c_DATA_WIDTH;
      g_WB_ADDR_WIDTH   : integer                         := c_ADDR_WIDTH;
      g_USER_CSR_EXT    : boolean                         := false;
      g_MANUFACTURER_ID : std_logic_vector(23 downto 0)   := c_CERN_ID;
      g_BOARD_ID        : std_logic_vector(31 downto 0)   := c_SVEC_ID;
      g_REVISION_ID     : std_logic_vector(31 downto 0)   := c_REVISION_ID;
      g_PROGRAM_ID      : std_logic_vector(7 downto 0)    := c_PROGRAM_ID;
      g_ASCII_PTR       : std_logic_vector(23 downto 0)   := x"000000";
      g_BEG_USER_CR     : std_logic_vector(23 downto 0)   := x"000000";
      g_END_USER_CR     : std_logic_vector(23 downto 0)   := x"000000";
      g_BEG_CRAM        : std_logic_vector(23 downto 0)   := x"001003";
      g_END_CRAM        : std_logic_vector(23 downto 0)   := x"0013ff";
      g_BEG_USER_CSR    : std_logic_vector(23 downto 0)   := x"07ff33";
      g_END_USER_CSR    : std_logic_vector(23 downto 0)   := x"07ff5f";
      g_BEG_SN          : std_logic_vector(23 downto 0)   := x"000000";
      g_END_SN          : std_logic_vector(23 downto 0)   := x"000000";
      g_F0_ADEM         : std_logic_vector( 31 downto 0)  := x"ff000000";
      g_F0_AMCAP        : std_logic_vector( 63 downto 0)  := x"00000000_0000bb00";
      g_F0_XAMCAP       : std_logic_vector(255 downto 0)  := x"00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000";
      g_F0_DAWPR        : std_logic_vector(  7 downto 0)  := x"84";
      g_F1_ADEM         : std_logic_vector( 31 downto 0)  := x"fff80000";
      g_F1_AMCAP        : std_logic_vector( 63 downto 0)  := x"bb000000_00000000";
      g_F1_XAMCAP       : std_logic_vector(255 downto 0)  := x"00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000";
      g_F1_DAWPR        : std_logic_vector(  7 downto 0)  := x"84";
      g_F2_ADEM         : std_logic_vector( 31 downto 0)  := x"00000000";
      g_F2_AMCAP        : std_logic_vector( 63 downto 0)  := x"00000000_00000000";
      g_F2_XAMCAP       : std_logic_vector(255 downto 0)  := x"00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000";
      g_F2_DAWPR        : std_logic_vector(  7 downto 0)  := x"84";
      g_F3_ADEM         : std_logic_vector( 31 downto 0)  := x"00000000";
      g_F3_AMCAP        : std_logic_vector( 63 downto 0)  := x"00000000_00000000";
      g_F3_XAMCAP       : std_logic_vector(255 downto 0)  := x"00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000";
      g_F3_DAWPR        : std_logic_vector(  7 downto 0)  := x"84";
      g_F4_ADEM         : std_logic_vector( 31 downto 0)  := x"00000000";
      g_F4_AMCAP        : std_logic_vector( 63 downto 0)  := x"00000000_00000000";
      g_F4_XAMCAP       : std_logic_vector(255 downto 0)  := x"00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000";
      g_F4_DAWPR        : std_logic_vector(  7 downto 0)  := x"84";
      g_F5_ADEM         : std_logic_vector( 31 downto 0)  := x"00000000";
      g_F5_AMCAP        : std_logic_vector( 63 downto 0)  := x"00000000_00000000";
      g_F5_XAMCAP       : std_logic_vector(255 downto 0)  := x"00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000";
      g_F5_DAWPR        : std_logic_vector(  7 downto 0)  := x"84";
      g_F6_ADEM         : std_logic_vector( 31 downto 0)  := x"00000000";
      g_F6_AMCAP        : std_logic_vector( 63 downto 0)  := x"00000000_00000000";
      g_F6_XAMCAP       : std_logic_vector(255 downto 0)  := x"00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000";
      g_F6_DAWPR        : std_logic_vector(  7 downto 0)  := x"84";
      g_F7_ADEM         : std_logic_vector( 31 downto 0)  := x"00000000";
      g_F7_AMCAP        : std_logic_vector( 63 downto 0)  := x"00000000_00000000";
      g_F7_XAMCAP       : std_logic_vector(255 downto 0)  := x"00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000";
      g_F7_DAWPR        : std_logic_vector(  7 downto 0)  := x"84"
    );
    port (
      clk_i           : in  std_logic;
      rst_n_i         : in  std_logic;
      rst_n_o         : out std_logic;
      VME_AS_n_i      : in  std_logic;
      VME_RST_n_i     : in  std_logic;
      VME_WRITE_n_i   : in  std_logic;
      VME_AM_i        : in  std_logic_vector(5 downto 0);
      VME_DS_n_i      : in  std_logic_vector(1 downto 0);
      VME_GA_i        : in  std_logic_vector(5 downto 0);
      VME_BERR_o      : out std_logic;
      VME_DTACK_n_o   : out std_logic;
      VME_RETRY_n_o   : out std_logic;
      VME_LWORD_n_i   : in  std_logic;
      VME_LWORD_n_o   : out std_logic;
      VME_ADDR_i      : in  std_logic_vector(31 downto 1);
      VME_ADDR_o      : out std_logic_vector(31 downto 1);
      VME_DATA_i      : in  std_logic_vector(31 downto 0);
      VME_DATA_o      : out std_logic_vector(31 downto 0);
      VME_IRQ_o       : out std_logic_vector(6 downto 0);
      VME_IACKIN_n_i  : in  std_logic;
      VME_IACK_n_i    : in  std_logic;
      VME_IACKOUT_n_o : out std_logic;
      VME_DTACK_OE_o  : out std_logic;
      VME_DATA_DIR_o  : out std_logic;
      VME_DATA_OE_N_o : out std_logic;
      VME_ADDR_DIR_o  : out std_logic;
      VME_ADDR_OE_N_o : out std_logic;
      VME_RETRY_OE_o  : out std_logic;
      DAT_i           : in  std_logic_vector(g_WB_DATA_WIDTH-1 downto 0);
      DAT_o           : out std_logic_vector(g_WB_DATA_WIDTH-1 downto 0);
      ADR_o           : out std_logic_vector(g_WB_ADDR_WIDTH-1 downto 0);
      CYC_o           : out std_logic;
      ERR_i           : in  std_logic;
      RTY_i           : in  std_logic;
      SEL_o           : out std_logic_vector(g_WB_DATA_WIDTH/8-1 downto 0);
      STB_o           : out std_logic;
      ACK_i           : in  std_logic;
      WE_o            : out std_logic;
      STALL_i         : in  std_logic;
      endian_i        : in  std_logic_vector( 2 downto 0) := (others => '0');
      irq_level_i     : in  std_logic_vector( 7 downto 0) := (others => '0');
      irq_vector_i    : in  std_logic_vector( 7 downto 0) := (others => '0');
      user_csr_addr_o : out std_logic_vector(18 downto 2);
      user_csr_data_i : in  std_logic_vector( 7 downto 0) := (others => '0');
      user_csr_data_o : out std_logic_vector( 7 downto 0);
      user_csr_we_o   : out std_logic;
      user_cr_addr_o  : out std_logic_vector(18 downto 2);
      user_cr_data_i  : in  std_logic_vector( 7 downto 0) := (others => '0');
      function_o      : out std_logic_vector( 3 downto 0);
      f0_faf_ader_i   : in  std_logic_vector(31 downto 0) := (others => '0');
      f1_faf_ader_i   : in  std_logic_vector(31 downto 0) := (others => '0');
      f2_faf_ader_i   : in  std_logic_vector(31 downto 0) := (others => '0');
      f3_faf_ader_i   : in  std_logic_vector(31 downto 0) := (others => '0');
      f4_faf_ader_i   : in  std_logic_vector(31 downto 0) := (others => '0');
      f5_faf_ader_i   : in  std_logic_vector(31 downto 0) := (others => '0');
      f6_faf_ader_i   : in  std_logic_vector(31 downto 0) := (others => '0');
      f7_faf_ader_i   : in  std_logic_vector(31 downto 0) := (others => '0');
      f0_dfs_adem_i   : in  std_logic_vector(31 downto 0) := (others => '0');
      f1_dfs_adem_i   : in  std_logic_vector(31 downto 0) := (others => '0');
      f2_dfs_adem_i   : in  std_logic_vector(31 downto 0) := (others => '0');
      f3_dfs_adem_i   : in  std_logic_vector(31 downto 0) := (others => '0');
      f4_dfs_adem_i   : in  std_logic_vector(31 downto 0) := (others => '0');
      f5_dfs_adem_i   : in  std_logic_vector(31 downto 0) := (others => '0');
      f6_dfs_adem_i   : in  std_logic_vector(31 downto 0) := (others => '0');
      f7_dfs_adem_i   : in  std_logic_vector(31 downto 0) := (others => '0');
      irq_ack_o       : out std_logic;
      irq_i           : in  std_logic
    );
  end component;

  component VME_bus is
    generic (
      g_CLOCK_PERIOD  : integer;
      g_WB_DATA_WIDTH : integer;
      g_WB_ADDR_WIDTH : integer
    );
    port (
      clk_i           : in  std_logic;
      rst_i           : in  std_logic;
      VME_AS_n_i      : in  std_logic;
      VME_LWORD_n_o   : out std_logic;
      VME_LWORD_n_i   : in  std_logic;
      VME_RETRY_n_o   : out std_logic;
      VME_RETRY_OE_o  : out std_logic;
      VME_WRITE_n_i   : in  std_logic;
      VME_DS_n_i      : in  std_logic_vector(1 downto 0);
      VME_DS_ant_n_i  : in  std_logic_vector(1 downto 0);
      VME_DTACK_n_o   : out std_logic;
      VME_DTACK_OE_o  : out std_logic;
      VME_BERR_n_o    : out std_logic;
      VME_ADDR_i      : in  std_logic_vector(31 downto 1);
      VME_ADDR_o      : out std_logic_vector(31 downto 1);
      VME_ADDR_DIR_o  : out std_logic;
      VME_ADDR_OE_N_o : out std_logic;
      VME_DATA_i      : in  std_logic_vector(31 downto 0);
      VME_DATA_o      : out std_logic_vector(31 downto 0);
      VME_DATA_DIR_o  : out std_logic;
      VME_DATA_OE_N_o : out std_logic;
      VME_AM_i        : in  std_logic_vector(5 downto 0);
      VME_IACK_n_i    : in  std_logic;
      stb_o           : out std_logic;
      ack_i           : in  std_logic;
      dat_o           : out std_logic_vector(g_WB_DATA_WIDTH-1 downto 0);
      dat_i           : in  std_logic_vector(g_WB_DATA_WIDTH-1 downto 0);
      adr_o           : out std_logic_vector(g_WB_ADDR_WIDTH-1 downto 0);
      sel_o           : out std_logic_vector(g_WB_DATA_WIDTH/8-1 downto 0);
      we_o            : out std_logic;
      cyc_o           : out std_logic;
      err_i           : in  std_logic;
      rty_i           : in  std_logic;
      stall_i         : in  std_logic;
      addr_decoder_i  : in  std_logic_vector(63 downto 0);
      addr_decoder_o  : out std_logic_vector(63 downto 0);
      decode_o        : out std_logic;
      am_o            : out std_logic_vector( 5 downto 0);
      xam_o           : out std_logic_vector( 7 downto 0);
      sel_i           : in  std_logic;
      cr_csr_addr_o   : out std_logic_vector(18 downto 2);
      cr_csr_data_i   : in  std_logic_vector( 7 downto 0);
      cr_csr_data_o   : out std_logic_vector( 7 downto 0);
      cr_csr_we_o     : out std_logic;
      endian_i        : in  std_logic_vector(2 downto 0);
      module_enable_i : in  std_logic;
      bar_i           : in  std_logic_vector(4 downto 0)
    );
  end component VME_bus;

  component VME_Funct_Match is
    generic (
      g_ADEM      : t_adem_array(-1 to 7);
      g_AMCAP     : t_amcap_array(0 to 7);
      g_XAMCAP    : t_xamcap_array(0 to 7)
    );
    port (
      clk_i       : in  std_logic;
      rst_n_i     : in  std_logic;
      addr_i      : in  std_logic_vector(63 downto 0);
      addr_o      : out std_logic_vector(63 downto 0);
      decode_i    : in  std_logic;
      am_i        : in  std_logic_vector( 5 downto 0);
      xam_i       : in  std_logic_vector( 7 downto 0);
      ader_i      : in  t_ader_array(0 to 7);
      dfs_adem_i  : in  t_adem_array(0 to 7);
      sel_o       : out std_logic;
      function_o  : out std_logic_vector( 2 downto 0)
    );
  end component VME_Funct_Match;

  component VME_CR_CSR_Space is
    generic (
      g_MANUFACTURER_ID : std_logic_vector(23 downto 0);
      g_BOARD_ID        : std_logic_vector(31 downto 0);
      g_REVISION_ID     : std_logic_vector(31 downto 0);
      g_PROGRAM_ID      : std_logic_vector(7 downto 0);
      g_ASCII_PTR       : std_logic_vector(23 downto 0);
      g_BEG_USER_CR     : std_logic_vector(23 downto 0);
      g_END_USER_CR     : std_logic_vector(23 downto 0);
      g_BEG_CRAM        : std_logic_vector(23 downto 0);
      g_END_CRAM        : std_logic_vector(23 downto 0);
      g_BEG_USER_CSR    : std_logic_vector(23 downto 0);
      g_END_USER_CSR    : std_logic_vector(23 downto 0);
      g_BEG_SN          : std_logic_vector(23 downto 0);
      g_END_SN          : std_logic_vector(23 downto 0);
      g_ADEM            : t_adem_array(-1 to 7);
      g_AMCAP           : t_amcap_array(0 to 7);
      g_XAMCAP          : t_xamcap_array(0 to 7);
      g_DAWPR           : t_dawpr_array(0 to 7)
    );
    port (
      clk_i               : in  std_logic;
      rst_n_i             : in  std_logic;
      vme_ga_i            : in  std_logic_vector(5 downto 0);
      vme_berr_n_i        : in  std_logic;
      bar_o               : out std_logic_vector(4 downto 0);
      vme_sysfail_i       : in  std_logic;
      vme_sysfail_ena_o   : out std_logic;
      module_enable_o     : out std_logic;
      module_reset_o      : out std_logic;
      addr_i              : in  std_logic_vector(18 downto 2);
      data_i              : in  std_logic_vector( 7 downto 0);
      data_o              : out std_logic_vector( 7 downto 0);
      we_i                : in  std_logic;
      user_csr_addr_o     : out std_logic_vector(18 downto 2);
      user_csr_data_i     : in  std_logic_vector( 7 downto 0);
      user_csr_data_o     : out std_logic_vector( 7 downto 0);
      user_csr_we_o       : out std_logic;
      user_cr_addr_o      : out std_logic_vector(18 downto 2);
      user_cr_data_i      : in  std_logic_vector( 7 downto 0);
      ader_o              : out t_ader_array(0 to 7);
      faf_ader_i          : in  t_ader_array(0 to 7);
      dfs_adem_i          : in  t_adem_array(0 to 7)
    );
  end component VME_CR_CSR_Space;

  component VME_User_CSR is
    generic (
      g_WB_DATA_WIDTH     : integer
    );
    port (
      clk_i               : in  std_logic;
      rst_n_i             : in  std_logic;
      addr_i              : in  std_logic_vector(18 downto 2);
      data_i              : in  std_logic_vector( 7 downto 0);
      data_o              : out std_logic_vector( 7 downto 0);
      we_i                : in  std_logic;
      irq_vector_o        : out std_logic_vector( 7 downto 0);
      irq_level_o         : out std_logic_vector( 7 downto 0);
      endian_o            : out std_logic_vector( 2 downto 0);
      bytes_i             : in  std_logic_vector(15 downto 0);
      time_i              : in  std_logic_vector(39 downto 0)
    );
  end component VME_User_CSR;

  component VME_Wb_master is
    generic (
      g_WB_DATA_WIDTH : integer;
      g_WB_ADDR_WIDTH : integer
    );
    port (
      memReq_i        : in  std_logic;
      clk_i           : in  std_logic;
      cardSel_i       : in  std_logic;
      reset_i         : in  std_logic;
      BERRcondition_i : in  std_logic;
      sel_i           : in  std_logic_vector(7 downto 0);
      locDataInSwap_i : in  std_logic_vector(63 downto 0);
      rel_locAddr_i   : in  std_logic_vector(63 downto 0);
      RW_i            : in  std_logic;
      stall_i         : in  std_logic;
      rty_i           : in  std_logic;
      err_i           : in  std_logic;
      wbData_i        : in  std_logic_vector(g_WB_DATA_WIDTH-1 downto 0);
      memAckWB_i      : in  std_logic;
      locDataOut_o    : out std_logic_vector(63 downto 0);
      memAckWb_o      : out std_logic;
      err_o           : out std_logic;
      rty_o           : out std_logic;
      cyc_o           : out std_logic;
      memReq_o        : out std_logic;
      WBdata_o        : out std_logic_vector(g_WB_DATA_WIDTH-1 downto 0);
      locAddr_o       : out std_logic_vector(g_WB_ADDR_WIDTH-1 downto 0);
      WbSel_o         : out std_logic_vector(g_WB_DATA_WIDTH/8-1 downto 0);
      RW_o            : out std_logic
    );
  end component VME_Wb_master;

  component VME_swapper is
    port (
      d_i : in  std_logic_vector(63 downto 0);
      sel : in  std_logic_vector(2 downto 0);
      d_o : out std_logic_vector(63 downto 0)
    );
  end component VME_swapper;

  component VME_IRQ_Controller
    generic (
      g_RETRY_TIMEOUT : integer range 1024 to 16777215
    );
    port (
      clk_i           : in  std_logic;
      reset_n_i       : in  std_logic;
      VME_IACKIN_n_i  : in  std_logic;
      VME_AS_n_i      : in  std_logic;
      VME_DS_n_i      : in  std_logic_vector (1 downto 0);
      VME_ADDR_123_i  : in  std_logic_vector (2 downto 0);
      INT_Level_i     : in  std_logic_vector (7 downto 0);
      INT_Vector_i    : in  std_logic_vector (7 downto 0);
      INT_Req_i       : in  std_logic;
      VME_IRQ_n_o     : out std_logic_vector (6 downto 0);
      VME_IACKOUT_n_o : out std_logic;
      VME_DTACK_n_o   : out std_logic;
      VME_DTACK_OE_o  : out std_logic;
      VME_DATA_o      : out std_logic_vector (31 downto 0);
      VME_DATA_DIR_o  : out std_logic
    );
  end component;

end vme64x_pack;

package body vme64x_pack is

end vme64x_pack;
