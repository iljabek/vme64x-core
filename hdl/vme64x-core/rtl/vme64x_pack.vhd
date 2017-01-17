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
  constant c_ADEM_FAF         : integer := 3;
  constant c_ADEM_DFS         : integer := 2;
  constant c_ADEM_EFD         : integer := 1;
  constant c_ADEM_EFM         : integer := 0;

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
  constant c_AM_TWOEDGE       : std_logic_vector(5 downto 0) := "100000";    -- 0x20

  constant c_AM_A32_2EVME     : std_logic_vector(7 downto 0) := "00000001";  -- 0x01
  constant c_AM_A64_2EVME     : std_logic_vector(7 downto 0) := "00000010";  -- 0x02
  constant c_AM_A32_2ESST     : std_logic_vector(7 downto 0) := "00010001";  -- 0x11
  constant c_AM_A64_2ESST     : std_logic_vector(7 downto 0) := "00010010";  -- 0x12

  -- Defined CR area
  constant c_BEG_CR           : unsigned(19 downto 0) := x"00000";
  constant c_END_CR           : unsigned(19 downto 0) := x"00FFF";

  -- Defined CSR area
  -- NOTE: by the standard, the defined CSR area is from 7FC00..7FFFF and the
  --       area from 7FC00..7FF5F is reserved. However the previous version of
  --       this core placed some user defined registers from 7FF30..7FF5F.
  --       Therefore, to allow compatibility with that version, the reserved
  --       area is not decoded as part of the defined CSR. However, it is not
  --       advisable to place any registers in this area in new designs.
  constant c_BEG_CSR          : unsigned(19 downto 0) := x"7FF60";
  constant c_END_CSR          : unsigned(19 downto 0) := x"7FFFF";

  -- Defined CSR addresses
  constant c_ADDR_BAR         : unsigned(19 downto 0) := x"7FFFF";
  constant c_ADDR_BIT_SET_REG : unsigned(19 downto 0) := x"7FFFB";
  constant c_ADDR_BIT_CLR_REG : unsigned(19 downto 0) := x"7FFF7";
  constant c_ADDR_CRAM_OWNER  : unsigned(19 downto 0) := x"7FFF3";
  constant c_ADDR_USR_SET_REG : unsigned(19 downto 0) := x"7FFEF";
  constant c_ADDR_USR_CLR_REG : unsigned(19 downto 0) := x"7FFEB";
  constant c_ADDR_F7_ADER_0   : unsigned(19 downto 0) := x"7FFDF";
  constant c_ADDR_F7_ADER_1   : unsigned(19 downto 0) := x"7FFDB";
  constant c_ADDR_F7_ADER_2   : unsigned(19 downto 0) := x"7FFD7";
  constant c_ADDR_F7_ADER_3   : unsigned(19 downto 0) := x"7FFD3";
  constant c_ADDR_F6_ADER_0   : unsigned(19 downto 0) := x"7FFCF";
  constant c_ADDR_F6_ADER_1   : unsigned(19 downto 0) := x"7FFCB";
  constant c_ADDR_F6_ADER_2   : unsigned(19 downto 0) := x"7FFC7";
  constant c_ADDR_F6_ADER_3   : unsigned(19 downto 0) := x"7FFC3";
  constant c_ADDR_F5_ADER_0   : unsigned(19 downto 0) := x"7FFBF";
  constant c_ADDR_F5_ADER_1   : unsigned(19 downto 0) := x"7FFBB";
  constant c_ADDR_F5_ADER_2   : unsigned(19 downto 0) := x"7FFB7";
  constant c_ADDR_F5_ADER_3   : unsigned(19 downto 0) := x"7FFB3";
  constant c_ADDR_F4_ADER_0   : unsigned(19 downto 0) := x"7FFAF";
  constant c_ADDR_F4_ADER_1   : unsigned(19 downto 0) := x"7FFAB";
  constant c_ADDR_F4_ADER_2   : unsigned(19 downto 0) := x"7FFA7";
  constant c_ADDR_F4_ADER_3   : unsigned(19 downto 0) := x"7FFA3";
  constant c_ADDR_F3_ADER_0   : unsigned(19 downto 0) := x"7FF9F";
  constant c_ADDR_F3_ADER_1   : unsigned(19 downto 0) := x"7FF9B";
  constant c_ADDR_F3_ADER_2   : unsigned(19 downto 0) := x"7FF97";
  constant c_ADDR_F3_ADER_3   : unsigned(19 downto 0) := x"7FF93";
  constant c_ADDR_F2_ADER_0   : unsigned(19 downto 0) := x"7FF8F";
  constant c_ADDR_F2_ADER_1   : unsigned(19 downto 0) := x"7FF8B";
  constant c_ADDR_F2_ADER_2   : unsigned(19 downto 0) := x"7FF87";
  constant c_ADDR_F2_ADER_3   : unsigned(19 downto 0) := x"7FF83";
  constant c_ADDR_F1_ADER_0   : unsigned(19 downto 0) := x"7FF7F";
  constant c_ADDR_F1_ADER_1   : unsigned(19 downto 0) := x"7FF7B";
  constant c_ADDR_F1_ADER_2   : unsigned(19 downto 0) := x"7FF77";
  constant c_ADDR_F1_ADER_3   : unsigned(19 downto 0) := x"7FF73";
  constant c_ADDR_F0_ADER_0   : unsigned(19 downto 0) := x"7FF6F";
  constant c_ADDR_F0_ADER_1   : unsigned(19 downto 0) := x"7FF6B";
  constant c_ADDR_F0_ADER_2   : unsigned(19 downto 0) := x"7FF67";
  constant c_ADDR_F0_ADER_3   : unsigned(19 downto 0) := x"7FF63";

  -- User CSR addresses
  constant c_ADDR_IRQ_VECTOR  : unsigned(19 downto 0) := x"0002F";
  constant c_ADDR_IRQ_LEVEL   : unsigned(19 downto 0) := x"0002B";
  constant c_ADDR_ENDIAN      : unsigned(19 downto 0) := x"00023";
  constant c_ADDR_TIME0_NS    : unsigned(19 downto 0) := x"0001F";
  constant c_ADDR_TIME1_NS    : unsigned(19 downto 0) := x"0001B";
  constant c_ADDR_TIME2_NS    : unsigned(19 downto 0) := x"00017";
  constant c_ADDR_TIME3_NS    : unsigned(19 downto 0) := x"00013";
  constant c_ADDR_TIME4_NS    : unsigned(19 downto 0) := x"0000F";
  constant c_ADDR_BYTES0      : unsigned(19 downto 0) := x"0000B";
  constant c_ADDR_BYTES1      : unsigned(19 downto 0) := x"00007";
  constant c_ADDR_WB32BITS    : unsigned(19 downto 0) := x"00003";

  ------------------------------------------------------------------------------
  -- Types
  ------------------------------------------------------------------------------

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

  type t_FUNC_32b_array is
      array (0 to 7) of unsigned(31 downto 0);          -- ADER register array

  type t_FUNC_64b_array is
      array (0 to 7) of unsigned(63 downto 0);          -- AMCAP register array

  type t_FUNC_256b_array is
      array (0 to 7) of unsigned(255 downto 0);         -- XAMCAP register array

  type t_cr_array is
      array (natural range <>) of std_logic_vector(7 downto 0);

  ------------------------------------------------------------------------------
  -- Functions
  ------------------------------------------------------------------------------

  function f_vme_cr_encode (
    manufacturer_id : std_logic_vector( 23 downto 0);
    board_id        : std_logic_vector( 31 downto 0);
    revision_id     : std_logic_vector( 31 downto 0);
    program_id      : std_logic_vector(  7 downto 0);
    ascii_ptr       : std_logic_vector( 23 downto 0);
    beg_user_cr     : std_logic_vector( 23 downto 0);
    end_user_cr     : std_logic_vector( 23 downto 0);
    beg_cram        : std_logic_vector( 23 downto 0);
    end_cram        : std_logic_vector( 23 downto 0);
    beg_user_csr    : std_logic_vector( 23 downto 0);
    end_user_csr    : std_logic_vector( 23 downto 0);
    beg_sn          : std_logic_vector( 23 downto 0);
    end_sn          : std_logic_vector( 23 downto 0);
    f0_adem         : std_logic_vector( 31 downto 0);
    f0_amcap        : std_logic_vector( 63 downto 0);
    f0_xamcap       : std_logic_vector(255 downto 0);
    f0_dawpr        : std_logic_vector(  7 downto 0);
    f1_adem         : std_logic_vector( 31 downto 0);
    f1_amcap        : std_logic_vector( 63 downto 0);
    f1_xamcap       : std_logic_vector(255 downto 0);
    f1_dawpr        : std_logic_vector(  7 downto 0);
    f2_adem         : std_logic_vector( 31 downto 0);
    f2_amcap        : std_logic_vector( 63 downto 0);
    f2_xamcap       : std_logic_vector(255 downto 0);
    f2_dawpr        : std_logic_vector(  7 downto 0);
    f3_adem         : std_logic_vector( 31 downto 0);
    f3_amcap        : std_logic_vector( 63 downto 0);
    f3_xamcap       : std_logic_vector(255 downto 0);
    f3_dawpr        : std_logic_vector(  7 downto 0);
    f4_adem         : std_logic_vector( 31 downto 0);
    f4_amcap        : std_logic_vector( 63 downto 0);
    f4_xamcap       : std_logic_vector(255 downto 0);
    f4_dawpr        : std_logic_vector(  7 downto 0);
    f5_adem         : std_logic_vector( 31 downto 0);
    f5_amcap        : std_logic_vector( 63 downto 0);
    f5_xamcap       : std_logic_vector(255 downto 0);
    f5_dawpr        : std_logic_vector(  7 downto 0);
    f6_adem         : std_logic_vector( 31 downto 0);
    f6_amcap        : std_logic_vector( 63 downto 0);
    f6_xamcap       : std_logic_vector(255 downto 0);
    f6_dawpr        : std_logic_vector(  7 downto 0);
    f7_adem         : std_logic_vector( 31 downto 0);
    f7_amcap        : std_logic_vector( 63 downto 0);
    f7_xamcap       : std_logic_vector(255 downto 0);
    f7_dawpr        : std_logic_vector(  7 downto 0)
  ) return t_cr_array;

  function f_size (
    A : std_logic_vector;
    B : std_logic_vector
  ) return integer;

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
      g_WB_ADDR_WIDTH : integer;
      g_BEG_USER_CR   : std_logic_vector( 23 downto 0);
      g_END_USER_CR   : std_logic_vector( 23 downto 0);
      g_BEG_CRAM      : std_logic_vector( 23 downto 0);
      g_END_CRAM      : std_logic_vector( 23 downto 0);
      g_BEG_USER_CSR  : std_logic_vector( 23 downto 0);
      g_END_USER_CSR  : std_logic_vector( 23 downto 0);
      g_F0_ADEM       : std_logic_vector( 31 downto 0);
      g_F0_AMCAP      : std_logic_vector( 63 downto 0);
      g_F0_XAMCAP     : std_logic_vector(255 downto 0);
      g_F1_ADEM       : std_logic_vector( 31 downto 0);
      g_F1_AMCAP      : std_logic_vector( 63 downto 0);
      g_F1_XAMCAP     : std_logic_vector(255 downto 0);
      g_F2_ADEM       : std_logic_vector( 31 downto 0);
      g_F2_AMCAP      : std_logic_vector( 63 downto 0);
      g_F2_XAMCAP     : std_logic_vector(255 downto 0);
      g_F3_ADEM       : std_logic_vector( 31 downto 0);
      g_F3_AMCAP      : std_logic_vector( 63 downto 0);
      g_F3_XAMCAP     : std_logic_vector(255 downto 0);
      g_F4_ADEM       : std_logic_vector( 31 downto 0);
      g_F4_AMCAP      : std_logic_vector( 63 downto 0);
      g_F4_XAMCAP     : std_logic_vector(255 downto 0);
      g_F5_ADEM       : std_logic_vector( 31 downto 0);
      g_F5_AMCAP      : std_logic_vector( 63 downto 0);
      g_F5_XAMCAP     : std_logic_vector(255 downto 0);
      g_F6_ADEM       : std_logic_vector( 31 downto 0);
      g_F6_AMCAP      : std_logic_vector( 63 downto 0);
      g_F6_XAMCAP     : std_logic_vector(255 downto 0);
      g_F7_ADEM       : std_logic_vector( 31 downto 0);
      g_F7_AMCAP      : std_logic_vector( 63 downto 0);
      g_F7_XAMCAP     : std_logic_vector(255 downto 0)
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
      cr_csr_addr_o   : out std_logic_vector(18 downto 2);
      cr_csr_data_i   : in  std_logic_vector( 7 downto 0);
      cr_csr_data_o   : out std_logic_vector( 7 downto 0);
      cr_csr_we_o     : out std_logic;
      f0_ader_i       : in  std_logic_vector(31 downto 0);
      f1_ader_i       : in  std_logic_vector(31 downto 0);
      f2_ader_i       : in  std_logic_vector(31 downto 0);
      f3_ader_i       : in  std_logic_vector(31 downto 0);
      f4_ader_i       : in  std_logic_vector(31 downto 0);
      f5_ader_i       : in  std_logic_vector(31 downto 0);
      f6_ader_i       : in  std_logic_vector(31 downto 0);
      f7_ader_i       : in  std_logic_vector(31 downto 0);
      endian_i        : in  std_logic_vector(2 downto 0);
      module_enable_i : in  std_logic;
      bar_i           : in  std_logic_vector(4 downto 0)
    );
  end component VME_bus;

  component VME_Access_Decode is
    port (
      clk_i          : in  std_logic;
      reset          : in  std_logic;
      mainFSMreset   : in  std_logic;
      decode         : in  std_logic;
      ModuleEnable   : in  std_logic;
      Addr           : in  std_logic_vector(63 downto 0);
      Ader0          : in  std_logic_vector(31 downto 0);
      Ader1          : in  std_logic_vector(31 downto 0);
      Ader2          : in  std_logic_vector(31 downto 0);
      Ader3          : in  std_logic_vector(31 downto 0);
      Ader4          : in  std_logic_vector(31 downto 0);
      Ader5          : in  std_logic_vector(31 downto 0);
      Ader6          : in  std_logic_vector(31 downto 0);
      Ader7          : in  std_logic_vector(31 downto 0);
      Adem0          : in  std_logic_vector(31 downto 0);
      Adem1          : in  std_logic_vector(31 downto 0);
      Adem2          : in  std_logic_vector(31 downto 0);
      Adem3          : in  std_logic_vector(31 downto 0);
      Adem4          : in  std_logic_vector(31 downto 0);
      Adem5          : in  std_logic_vector(31 downto 0);
      Adem6          : in  std_logic_vector(31 downto 0);
      Adem7          : in  std_logic_vector(31 downto 0);
      AmCap0         : in  std_logic_vector(63 downto 0);
      AmCap1         : in  std_logic_vector(63 downto 0);
      AmCap2         : in  std_logic_vector(63 downto 0);
      AmCap3         : in  std_logic_vector(63 downto 0);
      AmCap4         : in  std_logic_vector(63 downto 0);
      AmCap5         : in  std_logic_vector(63 downto 0);
      AmCap6         : in  std_logic_vector(63 downto 0);
      AmCap7         : in  std_logic_vector(63 downto 0);
      XAmCap0        : in  std_logic_vector(255 downto 0);
      XAmCap1        : in  std_logic_vector(255 downto 0);
      XAmCap2        : in  std_logic_vector(255 downto 0);
      XAmCap3        : in  std_logic_vector(255 downto 0);
      XAmCap4        : in  std_logic_vector(255 downto 0);
      XAmCap5        : in  std_logic_vector(255 downto 0);
      XAmCap6        : in  std_logic_vector(255 downto 0);
      XAmCap7        : in  std_logic_vector(255 downto 0);
      Am             : in  std_logic_vector(5 downto 0);
      XAm            : in  std_logic_vector(7 downto 0);
      BAR_i          : in  std_logic_vector(4 downto 0);
      AddrWidth      : in  std_logic_vector(1 downto 0);
      Funct_Sel      : out std_logic_vector(7 downto 0);
      Base_Addr      : out std_logic_vector(63 downto 0);
      Confaccess     : out std_logic;
      CardSel        : out std_logic
    );
  end component VME_Access_Decode;

  component VME_Funct_Match is
    port (
      clk_i        : in  std_logic;
      reset        : in  std_logic;
      decode       : in  std_logic;
      mainFSMreset : in  std_logic;
      Addr         : in  std_logic_vector(63 downto 0);
      AddrWidth    : in  std_logic_vector(1 downto 0);
      Ader0        : in  std_logic_vector(31 downto 0);
      Ader1        : in  std_logic_vector(31 downto 0);
      Ader2        : in  std_logic_vector(31 downto 0);
      Ader3        : in  std_logic_vector(31 downto 0);
      Ader4        : in  std_logic_vector(31 downto 0);
      Ader5        : in  std_logic_vector(31 downto 0);
      Ader6        : in  std_logic_vector(31 downto 0);
      Ader7        : in  std_logic_vector(31 downto 0);
      Adem0        : in  std_logic_vector(31 downto 0);
      Adem1        : in  std_logic_vector(31 downto 0);
      Adem2        : in  std_logic_vector(31 downto 0);
      Adem3        : in  std_logic_vector(31 downto 0);
      Adem4        : in  std_logic_vector(31 downto 0);
      Adem5        : in  std_logic_vector(31 downto 0);
      Adem6        : in  std_logic_vector(31 downto 0);
      Adem7        : in  std_logic_vector(31 downto 0);
      FunctMatch   : out std_logic_vector(7 downto 0);
      DFS_o        : out std_logic_vector(7 downto 0);
      Nx_Base_Addr : out std_logic_vector(63 downto 0)
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
      g_F0_ADEM         : std_logic_vector( 31 downto 0);
      g_F0_AMCAP        : std_logic_vector( 63 downto 0);
      g_F0_XAMCAP       : std_logic_vector(255 downto 0);
      g_F0_DAWPR        : std_logic_vector(  7 downto 0);
      g_F1_ADEM         : std_logic_vector( 31 downto 0);
      g_F1_AMCAP        : std_logic_vector( 63 downto 0);
      g_F1_XAMCAP       : std_logic_vector(255 downto 0);
      g_F1_DAWPR        : std_logic_vector(  7 downto 0);
      g_F2_ADEM         : std_logic_vector( 31 downto 0);
      g_F2_AMCAP        : std_logic_vector( 63 downto 0);
      g_F2_XAMCAP       : std_logic_vector(255 downto 0);
      g_F2_DAWPR        : std_logic_vector(  7 downto 0);
      g_F3_ADEM         : std_logic_vector( 31 downto 0);
      g_F3_AMCAP        : std_logic_vector( 63 downto 0);
      g_F3_XAMCAP       : std_logic_vector(255 downto 0);
      g_F3_DAWPR        : std_logic_vector(  7 downto 0);
      g_F4_ADEM         : std_logic_vector( 31 downto 0);
      g_F4_AMCAP        : std_logic_vector( 63 downto 0);
      g_F4_XAMCAP       : std_logic_vector(255 downto 0);
      g_F4_DAWPR        : std_logic_vector(  7 downto 0);
      g_F5_ADEM         : std_logic_vector( 31 downto 0);
      g_F5_AMCAP        : std_logic_vector( 63 downto 0);
      g_F5_XAMCAP       : std_logic_vector(255 downto 0);
      g_F5_DAWPR        : std_logic_vector(  7 downto 0);
      g_F6_ADEM         : std_logic_vector( 31 downto 0);
      g_F6_AMCAP        : std_logic_vector( 63 downto 0);
      g_F6_XAMCAP       : std_logic_vector(255 downto 0);
      g_F6_DAWPR        : std_logic_vector(  7 downto 0);
      g_F7_ADEM         : std_logic_vector( 31 downto 0);
      g_F7_AMCAP        : std_logic_vector( 63 downto 0);
      g_F7_XAMCAP       : std_logic_vector(255 downto 0);
      g_F7_DAWPR        : std_logic_vector(  7 downto 0)
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
      f0_ader_o           : out std_logic_vector(31 downto 0);
      f1_ader_o           : out std_logic_vector(31 downto 0);
      f2_ader_o           : out std_logic_vector(31 downto 0);
      f3_ader_o           : out std_logic_vector(31 downto 0);
      f4_ader_o           : out std_logic_vector(31 downto 0);
      f5_ader_o           : out std_logic_vector(31 downto 0);
      f6_ader_o           : out std_logic_vector(31 downto 0);
      f7_ader_o           : out std_logic_vector(31 downto 0);
      f0_faf_ader_i       : in  std_logic_vector(31 downto 0);
      f1_faf_ader_i       : in  std_logic_vector(31 downto 0);
      f2_faf_ader_i       : in  std_logic_vector(31 downto 0);
      f3_faf_ader_i       : in  std_logic_vector(31 downto 0);
      f4_faf_ader_i       : in  std_logic_vector(31 downto 0);
      f5_faf_ader_i       : in  std_logic_vector(31 downto 0);
      f6_faf_ader_i       : in  std_logic_vector(31 downto 0);
      f7_faf_ader_i       : in  std_logic_vector(31 downto 0);
      f0_dfs_adem_i       : in  std_logic_vector(31 downto 0);
      f1_dfs_adem_i       : in  std_logic_vector(31 downto 0);
      f2_dfs_adem_i       : in  std_logic_vector(31 downto 0);
      f3_dfs_adem_i       : in  std_logic_vector(31 downto 0);
      f4_dfs_adem_i       : in  std_logic_vector(31 downto 0);
      f5_dfs_adem_i       : in  std_logic_vector(31 downto 0);
      f6_dfs_adem_i       : in  std_logic_vector(31 downto 0);
      f7_dfs_adem_i       : in  std_logic_vector(31 downto 0)
    );
  end component VME_CR_CSR_Space;

  component VME_Am_Match is
    port (
      clk_i        : in  std_logic;
      reset        : in  std_logic;
      mainFSMreset : in  std_logic;
      Ader0        : in  std_logic_vector(31 downto 0);
      Ader1        : in  std_logic_vector(31 downto 0);
      Ader2        : in  std_logic_vector(31 downto 0);
      Ader3        : in  std_logic_vector(31 downto 0);
      Ader4        : in  std_logic_vector(31 downto 0);
      Ader5        : in  std_logic_vector(31 downto 0);
      Ader6        : in  std_logic_vector(31 downto 0);
      Ader7        : in  std_logic_vector(31 downto 0);
      AmCap0       : in  std_logic_vector(63 downto 0);
      AmCap1       : in  std_logic_vector(63 downto 0);
      AmCap2       : in  std_logic_vector(63 downto 0);
      AmCap3       : in  std_logic_vector(63 downto 0);
      AmCap4       : in  std_logic_vector(63 downto 0);
      AmCap5       : in  std_logic_vector(63 downto 0);
      AmCap6       : in  std_logic_vector(63 downto 0);
      AmCap7       : in  std_logic_vector(63 downto 0);
      XAmCap0      : in  std_logic_vector(255 downto 0);
      XAmCap1      : in  std_logic_vector(255 downto 0);
      XAmCap2      : in  std_logic_vector(255 downto 0);
      XAmCap3      : in  std_logic_vector(255 downto 0);
      XAmCap4      : in  std_logic_vector(255 downto 0);
      XAmCap5      : in  std_logic_vector(255 downto 0);
      XAmCap6      : in  std_logic_vector(255 downto 0);
      XAmCap7      : in  std_logic_vector(255 downto 0);
      Am           : in  std_logic_vector(5 downto 0);
      XAm          : in  std_logic_vector(7 downto 0);
      DFS_i        : in  std_logic_vector(7 downto 0);
      decode       : in  std_logic;
      AmMatch      : out std_logic_vector(7 downto 0)
    );
  end component VME_Am_Match;

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

  component VME_CRAM is
    generic (
      g_BEG_CRAM : std_logic_vector(23 downto 0);
      g_END_CRAM : std_logic_vector(23 downto 0)
    );
    port (
      clk_i   : in  std_logic;
      we_i    : in  std_logic;
      addr_i  : in  std_logic_vector(18 downto 2);
      data_i  : in  std_logic_vector( 7 downto 0);
      data_o  : out std_logic_vector( 7 downto 0)
    );
  end component VME_CRAM;

end vme64x_pack;

package body vme64x_pack is

  function f_vme_cr_encode (
    manufacturer_id : std_logic_vector( 23 downto 0);
    board_id        : std_logic_vector( 31 downto 0);
    revision_id     : std_logic_vector( 31 downto 0);
    program_id      : std_logic_vector(  7 downto 0);
    ascii_ptr       : std_logic_vector( 23 downto 0);
    beg_user_cr     : std_logic_vector( 23 downto 0);
    end_user_cr     : std_logic_vector( 23 downto 0);
    beg_cram        : std_logic_vector( 23 downto 0);
    end_cram        : std_logic_vector( 23 downto 0);
    beg_user_csr    : std_logic_vector( 23 downto 0);
    end_user_csr    : std_logic_vector( 23 downto 0);
    beg_sn          : std_logic_vector( 23 downto 0);
    end_sn          : std_logic_vector( 23 downto 0);
    f0_adem         : std_logic_vector( 31 downto 0);
    f0_amcap        : std_logic_vector( 63 downto 0);
    f0_xamcap       : std_logic_vector(255 downto 0);
    f0_dawpr        : std_logic_vector(  7 downto 0);
    f1_adem         : std_logic_vector( 31 downto 0);
    f1_amcap        : std_logic_vector( 63 downto 0);
    f1_xamcap       : std_logic_vector(255 downto 0);
    f1_dawpr        : std_logic_vector(  7 downto 0);
    f2_adem         : std_logic_vector( 31 downto 0);
    f2_amcap        : std_logic_vector( 63 downto 0);
    f2_xamcap       : std_logic_vector(255 downto 0);
    f2_dawpr        : std_logic_vector(  7 downto 0);
    f3_adem         : std_logic_vector( 31 downto 0);
    f3_amcap        : std_logic_vector( 63 downto 0);
    f3_xamcap       : std_logic_vector(255 downto 0);
    f3_dawpr        : std_logic_vector(  7 downto 0);
    f4_adem         : std_logic_vector( 31 downto 0);
    f4_amcap        : std_logic_vector( 63 downto 0);
    f4_xamcap       : std_logic_vector(255 downto 0);
    f4_dawpr        : std_logic_vector(  7 downto 0);
    f5_adem         : std_logic_vector( 31 downto 0);
    f5_amcap        : std_logic_vector( 63 downto 0);
    f5_xamcap       : std_logic_vector(255 downto 0);
    f5_dawpr        : std_logic_vector(  7 downto 0);
    f6_adem         : std_logic_vector( 31 downto 0);
    f6_amcap        : std_logic_vector( 63 downto 0);
    f6_xamcap       : std_logic_vector(255 downto 0);
    f6_dawpr        : std_logic_vector(  7 downto 0);
    f7_adem         : std_logic_vector( 31 downto 0);
    f7_amcap        : std_logic_vector( 63 downto 0);
    f7_xamcap       : std_logic_vector(255 downto 0);
    f7_dawpr        : std_logic_vector(  7 downto 0)
  ) return t_cr_array
  is
    variable cr  : t_cr_array(1023 downto 0) := (others => x"00");
    variable crc : unsigned(7 downto 0)      := x"00";
  begin

    cr(16#001#) := x"00";  -- Length of CR (excluding CRC)
    cr(16#002#) := x"03";
    cr(16#003#) := x"ff";

    cr(16#004#) := x"81";  -- CR data access width
    cr(16#005#) := x"81";  -- CSR data access width
    cr(16#006#) := x"02";  -- CR/CSR Space Specification ID
    cr(16#007#) := x"43";  -- ASCII "C"
    cr(16#008#) := x"52";  -- ASCII "R"

    cr(16#009#) := manufacturer_id(23 downto 16);
    cr(16#00A#) := manufacturer_id(15 downto  8);
    cr(16#00B#) := manufacturer_id( 7 downto  0);

    cr(16#00C#) := board_id(31 downto 24);
    cr(16#00D#) := board_id(23 downto 16);
    cr(16#00E#) := board_id(15 downto  8);
    cr(16#00F#) := board_id( 7 downto  0);

    cr(16#010#) := revision_id(31 downto 24);
    cr(16#011#) := revision_id(23 downto 16);
    cr(16#012#) := revision_id(15 downto  8);
    cr(16#013#) := revision_id( 7 downto  0);

    cr(16#014#) := ascii_ptr(23 downto 16);
    cr(16#015#) := ascii_ptr(15 downto  8);
    cr(16#016#) := ascii_ptr( 7 downto  0);

    cr(16#01F#) := program_id;

    cr(16#020#) := beg_user_cr(23 downto 16);
    cr(16#021#) := beg_user_cr(15 downto  8);
    cr(16#022#) := beg_user_cr( 7 downto  0);

    cr(16#023#) := end_user_cr(23 downto 16);
    cr(16#024#) := end_user_cr(15 downto  8);
    cr(16#025#) := end_user_cr( 7 downto  0);

    cr(16#026#) := beg_cram(23 downto 16);
    cr(16#027#) := beg_cram(15 downto  8);
    cr(16#028#) := beg_cram( 7 downto  0);

    cr(16#029#) := end_cram(23 downto 16);
    cr(16#02A#) := end_cram(15 downto  8);
    cr(16#02B#) := end_cram( 7 downto  0);

    cr(16#02C#) := beg_user_csr(23 downto 16);
    cr(16#02D#) := beg_user_csr(15 downto  8);
    cr(16#02E#) := beg_user_csr( 7 downto  0);

    cr(16#02F#) := end_user_csr(23 downto 16);
    cr(16#030#) := end_user_csr(15 downto  8);
    cr(16#031#) := end_user_csr( 7 downto  0);

    cr(16#032#) := beg_sn(23 downto 16);
    cr(16#033#) := beg_sn(15 downto  8);
    cr(16#034#) := beg_sn( 7 downto  0);

    cr(16#035#) := end_sn(23 downto 16);
    cr(16#036#) := end_sn(15 downto  8);
    cr(16#037#) := end_sn( 7 downto  0);

    cr(16#038#) := x"04";  -- Slave characteristics parameter
    cr(16#039#) := x"00";  -- User-defined slave characteristics parameter

    cr(16#03D#) := x"0E";  -- Interrupter capabilities

    cr(16#03F#) := x"81";  -- CRAM data access width

    cr(16#040#) := f0_dawpr;
    cr(16#041#) := f1_dawpr;
    cr(16#042#) := f2_dawpr;
    cr(16#043#) := f3_dawpr;
    cr(16#044#) := f4_dawpr;
    cr(16#045#) := f5_dawpr;
    cr(16#046#) := f6_dawpr;
    cr(16#047#) := f7_dawpr;

    cr(16#048#) := f0_amcap(63 downto 56);
    cr(16#049#) := f0_amcap(55 downto 48);
    cr(16#04A#) := f0_amcap(47 downto 40);
    cr(16#04B#) := f0_amcap(39 downto 32);
    cr(16#04C#) := f0_amcap(31 downto 24);
    cr(16#04D#) := f0_amcap(23 downto 16);
    cr(16#04E#) := f0_amcap(15 downto  8);
    cr(16#04F#) := f0_amcap( 7 downto  0);

    cr(16#050#) := f1_amcap(63 downto 56);
    cr(16#051#) := f1_amcap(55 downto 48);
    cr(16#052#) := f1_amcap(47 downto 40);
    cr(16#053#) := f1_amcap(39 downto 32);
    cr(16#054#) := f1_amcap(31 downto 24);
    cr(16#055#) := f1_amcap(23 downto 16);
    cr(16#056#) := f1_amcap(15 downto  8);
    cr(16#057#) := f1_amcap( 7 downto  0);

    cr(16#058#) := f2_amcap(63 downto 56);
    cr(16#059#) := f2_amcap(55 downto 48);
    cr(16#05A#) := f2_amcap(47 downto 40);
    cr(16#05B#) := f2_amcap(39 downto 32);
    cr(16#05C#) := f2_amcap(31 downto 24);
    cr(16#05D#) := f2_amcap(23 downto 16);
    cr(16#05E#) := f2_amcap(15 downto  8);
    cr(16#05F#) := f2_amcap( 7 downto  0);

    cr(16#060#) := f3_amcap(63 downto 56);
    cr(16#061#) := f3_amcap(55 downto 48);
    cr(16#062#) := f3_amcap(47 downto 40);
    cr(16#063#) := f3_amcap(39 downto 32);
    cr(16#064#) := f3_amcap(31 downto 24);
    cr(16#065#) := f3_amcap(23 downto 16);
    cr(16#066#) := f3_amcap(15 downto  8);
    cr(16#067#) := f3_amcap( 7 downto  0);

    cr(16#068#) := f4_amcap(63 downto 56);
    cr(16#069#) := f4_amcap(55 downto 48);
    cr(16#06A#) := f4_amcap(47 downto 40);
    cr(16#06B#) := f4_amcap(39 downto 32);
    cr(16#06C#) := f4_amcap(31 downto 24);
    cr(16#06D#) := f4_amcap(23 downto 16);
    cr(16#06E#) := f4_amcap(15 downto  8);
    cr(16#06F#) := f4_amcap( 7 downto  0);

    cr(16#070#) := f5_amcap(63 downto 56);
    cr(16#071#) := f5_amcap(55 downto 48);
    cr(16#072#) := f5_amcap(47 downto 40);
    cr(16#073#) := f5_amcap(39 downto 32);
    cr(16#074#) := f5_amcap(31 downto 24);
    cr(16#075#) := f5_amcap(23 downto 16);
    cr(16#076#) := f5_amcap(15 downto  8);
    cr(16#077#) := f5_amcap( 7 downto  0);

    cr(16#078#) := f6_amcap(63 downto 56);
    cr(16#079#) := f6_amcap(55 downto 48);
    cr(16#07A#) := f6_amcap(47 downto 40);
    cr(16#07B#) := f6_amcap(39 downto 32);
    cr(16#07C#) := f6_amcap(31 downto 24);
    cr(16#07D#) := f6_amcap(23 downto 16);
    cr(16#07E#) := f6_amcap(15 downto  8);
    cr(16#07F#) := f6_amcap( 7 downto  0);

    cr(16#080#) := f7_amcap(63 downto 56);
    cr(16#081#) := f7_amcap(55 downto 48);
    cr(16#082#) := f7_amcap(47 downto 40);
    cr(16#083#) := f7_amcap(39 downto 32);
    cr(16#084#) := f7_amcap(31 downto 24);
    cr(16#085#) := f7_amcap(23 downto 16);
    cr(16#086#) := f7_amcap(15 downto  8);
    cr(16#087#) := f7_amcap( 7 downto  0);

    cr(16#088#) := f0_xamcap(255 downto 248);
    cr(16#089#) := f0_xamcap(247 downto 240);
    cr(16#08A#) := f0_xamcap(239 downto 232);
    cr(16#08B#) := f0_xamcap(231 downto 224);
    cr(16#08C#) := f0_xamcap(223 downto 216);
    cr(16#08D#) := f0_xamcap(215 downto 208);
    cr(16#08E#) := f0_xamcap(207 downto 200);
    cr(16#08F#) := f0_xamcap(199 downto 192);
    cr(16#090#) := f0_xamcap(191 downto 184);
    cr(16#091#) := f0_xamcap(183 downto 176);
    cr(16#092#) := f0_xamcap(175 downto 168);
    cr(16#093#) := f0_xamcap(167 downto 160);
    cr(16#094#) := f0_xamcap(159 downto 152);
    cr(16#095#) := f0_xamcap(151 downto 144);
    cr(16#096#) := f0_xamcap(143 downto 136);
    cr(16#097#) := f0_xamcap(135 downto 128);
    cr(16#098#) := f0_xamcap(127 downto 120);
    cr(16#099#) := f0_xamcap(119 downto 112);
    cr(16#09A#) := f0_xamcap(111 downto 104);
    cr(16#09B#) := f0_xamcap(103 downto  96);
    cr(16#09C#) := f0_xamcap( 95 downto  88);
    cr(16#09D#) := f0_xamcap( 87 downto  80);
    cr(16#09E#) := f0_xamcap( 79 downto  72);
    cr(16#09F#) := f0_xamcap( 71 downto  64);
    cr(16#0A0#) := f0_xamcap( 63 downto  56);
    cr(16#0A1#) := f0_xamcap( 55 downto  48);
    cr(16#0A2#) := f0_xamcap( 47 downto  40);
    cr(16#0A3#) := f0_xamcap( 39 downto  32);
    cr(16#0A4#) := f0_xamcap( 31 downto  24);
    cr(16#0A5#) := f0_xamcap( 23 downto  16);
    cr(16#0A6#) := f0_xamcap( 15 downto   8);
    cr(16#0A7#) := f0_xamcap(  7 downto   0);

    cr(16#0A8#) := f1_xamcap(255 downto 248);
    cr(16#0A9#) := f1_xamcap(247 downto 240);
    cr(16#0AA#) := f1_xamcap(239 downto 232);
    cr(16#0AB#) := f1_xamcap(231 downto 224);
    cr(16#0AC#) := f1_xamcap(223 downto 216);
    cr(16#0AD#) := f1_xamcap(215 downto 208);
    cr(16#0AE#) := f1_xamcap(207 downto 200);
    cr(16#0AF#) := f1_xamcap(199 downto 192);
    cr(16#0B0#) := f1_xamcap(191 downto 184);
    cr(16#0B1#) := f1_xamcap(183 downto 176);
    cr(16#0B2#) := f1_xamcap(175 downto 168);
    cr(16#0B3#) := f1_xamcap(167 downto 160);
    cr(16#0B4#) := f1_xamcap(159 downto 152);
    cr(16#0B5#) := f1_xamcap(151 downto 144);
    cr(16#0B6#) := f1_xamcap(143 downto 136);
    cr(16#0B7#) := f1_xamcap(135 downto 128);
    cr(16#0B8#) := f1_xamcap(127 downto 120);
    cr(16#0B9#) := f1_xamcap(119 downto 112);
    cr(16#0BA#) := f1_xamcap(111 downto 104);
    cr(16#0BB#) := f1_xamcap(103 downto  96);
    cr(16#0BC#) := f1_xamcap( 95 downto  88);
    cr(16#0BD#) := f1_xamcap( 87 downto  80);
    cr(16#0BE#) := f1_xamcap( 79 downto  72);
    cr(16#0BF#) := f1_xamcap( 71 downto  64);
    cr(16#0C0#) := f1_xamcap( 63 downto  56);
    cr(16#0C1#) := f1_xamcap( 55 downto  48);
    cr(16#0C2#) := f1_xamcap( 47 downto  40);
    cr(16#0C3#) := f1_xamcap( 39 downto  32);
    cr(16#0C4#) := f1_xamcap( 31 downto  24);
    cr(16#0C5#) := f1_xamcap( 23 downto  16);
    cr(16#0C6#) := f1_xamcap( 15 downto   8);
    cr(16#0C7#) := f1_xamcap(  7 downto   0);

    cr(16#0C8#) := f2_xamcap(255 downto 248);
    cr(16#0C9#) := f2_xamcap(247 downto 240);
    cr(16#0CA#) := f2_xamcap(239 downto 232);
    cr(16#0CB#) := f2_xamcap(231 downto 224);
    cr(16#0CC#) := f2_xamcap(223 downto 216);
    cr(16#0CD#) := f2_xamcap(215 downto 208);
    cr(16#0CE#) := f2_xamcap(207 downto 200);
    cr(16#0CF#) := f2_xamcap(199 downto 192);
    cr(16#0D0#) := f2_xamcap(191 downto 184);
    cr(16#0D1#) := f2_xamcap(183 downto 176);
    cr(16#0D2#) := f2_xamcap(175 downto 168);
    cr(16#0D3#) := f2_xamcap(167 downto 160);
    cr(16#0D4#) := f2_xamcap(159 downto 152);
    cr(16#0D5#) := f2_xamcap(151 downto 144);
    cr(16#0D6#) := f2_xamcap(143 downto 136);
    cr(16#0D7#) := f2_xamcap(135 downto 128);
    cr(16#0D8#) := f2_xamcap(127 downto 120);
    cr(16#0D9#) := f2_xamcap(119 downto 112);
    cr(16#0DA#) := f2_xamcap(111 downto 104);
    cr(16#0DB#) := f2_xamcap(103 downto  96);
    cr(16#0DC#) := f2_xamcap( 95 downto  88);
    cr(16#0DD#) := f2_xamcap( 87 downto  80);
    cr(16#0DE#) := f2_xamcap( 79 downto  72);
    cr(16#0DF#) := f2_xamcap( 71 downto  64);
    cr(16#0E0#) := f2_xamcap( 63 downto  56);
    cr(16#0E1#) := f2_xamcap( 55 downto  48);
    cr(16#0E2#) := f2_xamcap( 47 downto  40);
    cr(16#0E3#) := f2_xamcap( 39 downto  32);
    cr(16#0E4#) := f2_xamcap( 31 downto  24);
    cr(16#0E5#) := f2_xamcap( 23 downto  16);
    cr(16#0E6#) := f2_xamcap( 15 downto   8);
    cr(16#0E7#) := f2_xamcap(  7 downto   0);

    cr(16#0E8#) := f3_xamcap(255 downto 248);
    cr(16#0E9#) := f3_xamcap(247 downto 240);
    cr(16#0EA#) := f3_xamcap(239 downto 232);
    cr(16#0EB#) := f3_xamcap(231 downto 224);
    cr(16#0EC#) := f3_xamcap(223 downto 216);
    cr(16#0ED#) := f3_xamcap(215 downto 208);
    cr(16#0EE#) := f3_xamcap(207 downto 200);
    cr(16#0EF#) := f3_xamcap(199 downto 192);
    cr(16#0F0#) := f3_xamcap(191 downto 184);
    cr(16#0F1#) := f3_xamcap(183 downto 176);
    cr(16#0F2#) := f3_xamcap(175 downto 168);
    cr(16#0F3#) := f3_xamcap(167 downto 160);
    cr(16#0F4#) := f3_xamcap(159 downto 152);
    cr(16#0F5#) := f3_xamcap(151 downto 144);
    cr(16#0F6#) := f3_xamcap(143 downto 136);
    cr(16#0F7#) := f3_xamcap(135 downto 128);
    cr(16#0F8#) := f3_xamcap(127 downto 120);
    cr(16#0F9#) := f3_xamcap(119 downto 112);
    cr(16#0FA#) := f3_xamcap(111 downto 104);
    cr(16#0FB#) := f3_xamcap(103 downto  96);
    cr(16#0FC#) := f3_xamcap( 95 downto  88);
    cr(16#0FD#) := f3_xamcap( 87 downto  80);
    cr(16#0FE#) := f3_xamcap( 79 downto  72);
    cr(16#0FF#) := f3_xamcap( 71 downto  64);
    cr(16#100#) := f3_xamcap( 63 downto  56);
    cr(16#101#) := f3_xamcap( 55 downto  48);
    cr(16#102#) := f3_xamcap( 47 downto  40);
    cr(16#103#) := f3_xamcap( 39 downto  32);
    cr(16#104#) := f3_xamcap( 31 downto  24);
    cr(16#105#) := f3_xamcap( 23 downto  16);
    cr(16#106#) := f3_xamcap( 15 downto   8);
    cr(16#107#) := f3_xamcap(  7 downto   0);

    cr(16#108#) := f4_xamcap(255 downto 248);
    cr(16#109#) := f4_xamcap(247 downto 240);
    cr(16#10A#) := f4_xamcap(239 downto 232);
    cr(16#10B#) := f4_xamcap(231 downto 224);
    cr(16#10C#) := f4_xamcap(223 downto 216);
    cr(16#10D#) := f4_xamcap(215 downto 208);
    cr(16#10E#) := f4_xamcap(207 downto 200);
    cr(16#10F#) := f4_xamcap(199 downto 192);
    cr(16#110#) := f4_xamcap(191 downto 184);
    cr(16#111#) := f4_xamcap(183 downto 176);
    cr(16#112#) := f4_xamcap(175 downto 168);
    cr(16#113#) := f4_xamcap(167 downto 160);
    cr(16#114#) := f4_xamcap(159 downto 152);
    cr(16#115#) := f4_xamcap(151 downto 144);
    cr(16#116#) := f4_xamcap(143 downto 136);
    cr(16#117#) := f4_xamcap(135 downto 128);
    cr(16#118#) := f4_xamcap(127 downto 120);
    cr(16#119#) := f4_xamcap(119 downto 112);
    cr(16#11A#) := f4_xamcap(111 downto 104);
    cr(16#11B#) := f4_xamcap(103 downto  96);
    cr(16#11C#) := f4_xamcap( 95 downto  88);
    cr(16#11D#) := f4_xamcap( 87 downto  80);
    cr(16#11E#) := f4_xamcap( 79 downto  72);
    cr(16#11F#) := f4_xamcap( 71 downto  64);
    cr(16#120#) := f4_xamcap( 63 downto  56);
    cr(16#121#) := f4_xamcap( 55 downto  48);
    cr(16#122#) := f4_xamcap( 47 downto  40);
    cr(16#123#) := f4_xamcap( 39 downto  32);
    cr(16#124#) := f4_xamcap( 31 downto  24);
    cr(16#125#) := f4_xamcap( 23 downto  16);
    cr(16#126#) := f4_xamcap( 15 downto   8);
    cr(16#127#) := f4_xamcap(  7 downto   0);

    cr(16#128#) := f5_xamcap(255 downto 248);
    cr(16#129#) := f5_xamcap(247 downto 240);
    cr(16#12A#) := f5_xamcap(239 downto 232);
    cr(16#12B#) := f5_xamcap(231 downto 224);
    cr(16#12C#) := f5_xamcap(223 downto 216);
    cr(16#12D#) := f5_xamcap(215 downto 208);
    cr(16#12E#) := f5_xamcap(207 downto 200);
    cr(16#12F#) := f5_xamcap(199 downto 192);
    cr(16#130#) := f5_xamcap(191 downto 184);
    cr(16#131#) := f5_xamcap(183 downto 176);
    cr(16#132#) := f5_xamcap(175 downto 168);
    cr(16#133#) := f5_xamcap(167 downto 160);
    cr(16#134#) := f5_xamcap(159 downto 152);
    cr(16#135#) := f5_xamcap(151 downto 144);
    cr(16#136#) := f5_xamcap(143 downto 136);
    cr(16#137#) := f5_xamcap(135 downto 128);
    cr(16#138#) := f5_xamcap(127 downto 120);
    cr(16#139#) := f5_xamcap(119 downto 112);
    cr(16#13A#) := f5_xamcap(111 downto 104);
    cr(16#13B#) := f5_xamcap(103 downto  96);
    cr(16#13C#) := f5_xamcap( 95 downto  88);
    cr(16#13D#) := f5_xamcap( 87 downto  80);
    cr(16#13E#) := f5_xamcap( 79 downto  72);
    cr(16#13F#) := f5_xamcap( 71 downto  64);
    cr(16#140#) := f5_xamcap( 63 downto  56);
    cr(16#141#) := f5_xamcap( 55 downto  48);
    cr(16#142#) := f5_xamcap( 47 downto  40);
    cr(16#143#) := f5_xamcap( 39 downto  32);
    cr(16#144#) := f5_xamcap( 31 downto  24);
    cr(16#145#) := f5_xamcap( 23 downto  16);
    cr(16#146#) := f5_xamcap( 15 downto   8);
    cr(16#147#) := f5_xamcap(  7 downto   0);

    cr(16#148#) := f6_xamcap(255 downto 248);
    cr(16#149#) := f6_xamcap(247 downto 240);
    cr(16#14A#) := f6_xamcap(239 downto 232);
    cr(16#14B#) := f6_xamcap(231 downto 224);
    cr(16#14C#) := f6_xamcap(223 downto 216);
    cr(16#14D#) := f6_xamcap(215 downto 208);
    cr(16#14E#) := f6_xamcap(207 downto 200);
    cr(16#14F#) := f6_xamcap(199 downto 192);
    cr(16#150#) := f6_xamcap(191 downto 184);
    cr(16#151#) := f6_xamcap(183 downto 176);
    cr(16#152#) := f6_xamcap(175 downto 168);
    cr(16#153#) := f6_xamcap(167 downto 160);
    cr(16#154#) := f6_xamcap(159 downto 152);
    cr(16#155#) := f6_xamcap(151 downto 144);
    cr(16#156#) := f6_xamcap(143 downto 136);
    cr(16#157#) := f6_xamcap(135 downto 128);
    cr(16#158#) := f6_xamcap(127 downto 120);
    cr(16#159#) := f6_xamcap(119 downto 112);
    cr(16#15A#) := f6_xamcap(111 downto 104);
    cr(16#15B#) := f6_xamcap(103 downto  96);
    cr(16#15C#) := f6_xamcap( 95 downto  88);
    cr(16#15D#) := f6_xamcap( 87 downto  80);
    cr(16#15E#) := f6_xamcap( 79 downto  72);
    cr(16#15F#) := f6_xamcap( 71 downto  64);
    cr(16#160#) := f6_xamcap( 63 downto  56);
    cr(16#161#) := f6_xamcap( 55 downto  48);
    cr(16#162#) := f6_xamcap( 47 downto  40);
    cr(16#163#) := f6_xamcap( 39 downto  32);
    cr(16#164#) := f6_xamcap( 31 downto  24);
    cr(16#165#) := f6_xamcap( 23 downto  16);
    cr(16#166#) := f6_xamcap( 15 downto   8);
    cr(16#167#) := f6_xamcap(  7 downto   0);

    cr(16#168#) := f7_xamcap(255 downto 248);
    cr(16#169#) := f7_xamcap(247 downto 240);
    cr(16#16A#) := f7_xamcap(239 downto 232);
    cr(16#16B#) := f7_xamcap(231 downto 224);
    cr(16#16C#) := f7_xamcap(223 downto 216);
    cr(16#16D#) := f7_xamcap(215 downto 208);
    cr(16#16E#) := f7_xamcap(207 downto 200);
    cr(16#16F#) := f7_xamcap(199 downto 192);
    cr(16#170#) := f7_xamcap(191 downto 184);
    cr(16#171#) := f7_xamcap(183 downto 176);
    cr(16#172#) := f7_xamcap(175 downto 168);
    cr(16#173#) := f7_xamcap(167 downto 160);
    cr(16#174#) := f7_xamcap(159 downto 152);
    cr(16#175#) := f7_xamcap(151 downto 144);
    cr(16#176#) := f7_xamcap(143 downto 136);
    cr(16#177#) := f7_xamcap(135 downto 128);
    cr(16#178#) := f7_xamcap(127 downto 120);
    cr(16#179#) := f7_xamcap(119 downto 112);
    cr(16#17A#) := f7_xamcap(111 downto 104);
    cr(16#17B#) := f7_xamcap(103 downto  96);
    cr(16#17C#) := f7_xamcap( 95 downto  88);
    cr(16#17D#) := f7_xamcap( 87 downto  80);
    cr(16#17E#) := f7_xamcap( 79 downto  72);
    cr(16#17F#) := f7_xamcap( 71 downto  64);
    cr(16#180#) := f7_xamcap( 63 downto  56);
    cr(16#181#) := f7_xamcap( 55 downto  48);
    cr(16#182#) := f7_xamcap( 47 downto  40);
    cr(16#183#) := f7_xamcap( 39 downto  32);
    cr(16#184#) := f7_xamcap( 31 downto  24);
    cr(16#185#) := f7_xamcap( 23 downto  16);
    cr(16#186#) := f7_xamcap( 15 downto   8);
    cr(16#187#) := f7_xamcap(  7 downto   0);

    cr(16#188#) := f0_adem(31 downto 24);
    cr(16#189#) := f0_adem(23 downto 16);
    cr(16#18A#) := f0_adem(15 downto  8);
    cr(16#18B#) := f0_adem( 7 downto  0);

    cr(16#18C#) := f1_adem(31 downto 24);
    cr(16#18D#) := f1_adem(23 downto 16);
    cr(16#18E#) := f1_adem(15 downto  8);
    cr(16#18F#) := f1_adem( 7 downto  0);

    cr(16#190#) := f2_adem(31 downto 24);
    cr(16#191#) := f2_adem(23 downto 16);
    cr(16#192#) := f2_adem(15 downto  8);
    cr(16#193#) := f2_adem( 7 downto  0);

    cr(16#194#) := f3_adem(31 downto 24);
    cr(16#195#) := f3_adem(23 downto 16);
    cr(16#196#) := f3_adem(15 downto  8);
    cr(16#197#) := f3_adem( 7 downto  0);

    cr(16#198#) := f4_adem(31 downto 24);
    cr(16#199#) := f4_adem(23 downto 16);
    cr(16#19A#) := f4_adem(15 downto  8);
    cr(16#19B#) := f4_adem( 7 downto  0);

    cr(16#19C#) := f5_adem(31 downto 24);
    cr(16#19D#) := f5_adem(23 downto 16);
    cr(16#19E#) := f5_adem(15 downto  8);
    cr(16#19F#) := f5_adem( 7 downto  0);

    cr(16#1A0#) := f6_adem(31 downto 24);
    cr(16#1A1#) := f6_adem(23 downto 16);
    cr(16#1A2#) := f6_adem(15 downto  8);
    cr(16#1A3#) := f6_adem( 7 downto  0);

    cr(16#1A4#) := f7_adem(31 downto 24);
    cr(16#1A5#) := f7_adem(23 downto 16);
    cr(16#1A6#) := f7_adem(15 downto  8);
    cr(16#1A7#) := f7_adem( 7 downto  0);

    -- Calculate CRC
    for i in 1 to cr'length-1 loop
      crc := crc + unsigned(cr(i));
    end loop;

    cr(16#000#) := std_logic_vector(crc);

    return cr;
  end;

  function f_size (
    A : std_logic_vector;
    B : std_logic_vector
  ) return integer is
  begin
    return ((to_integer(unsigned(B)) - to_integer(unsigned(A))) / 4) + 1;
  end;

end vme64x_pack;
