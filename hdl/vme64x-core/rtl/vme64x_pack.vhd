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
  constant c_DATA_WIDTH       : integer := 32;  -- WB data width: must be 32
  constant c_ADDR_WIDTH       : integer := 32;  -- WB addr width: 32.

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
  type t_dawpr_array  is array (integer range <>) of std_logic_vector(  7 downto 0);

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
      g_BEG_CRAM        : std_logic_vector(23 downto 0)   := x"000000";
      g_END_CRAM        : std_logic_vector(23 downto 0)   := x"000000";
      g_BEG_USER_CSR    : std_logic_vector(23 downto 0)   := x"07ff33";
      g_END_USER_CSR    : std_logic_vector(23 downto 0)   := x"07ff5f";
      g_BEG_SN          : std_logic_vector(23 downto 0)   := x"000000";
      g_END_SN          : std_logic_vector(23 downto 0)   := x"000000";
      g_F0_ADEM         : std_logic_vector( 31 downto 0)  := x"ff000000";
      g_F0_AMCAP        : std_logic_vector( 63 downto 0)  := x"00000000_0000ff00";
      g_F0_DAWPR        : std_logic_vector(  7 downto 0)  := x"84";
      g_F1_ADEM         : std_logic_vector( 31 downto 0)  := x"fff80000";
      g_F1_AMCAP        : std_logic_vector( 63 downto 0)  := x"ff000000_00000000";
      g_F1_DAWPR        : std_logic_vector(  7 downto 0)  := x"84";
      g_F2_ADEM         : std_logic_vector( 31 downto 0)  := x"00000000";
      g_F2_AMCAP        : std_logic_vector( 63 downto 0)  := x"00000000_00000000";
      g_F2_DAWPR        : std_logic_vector(  7 downto 0)  := x"84";
      g_F3_ADEM         : std_logic_vector( 31 downto 0)  := x"00000000";
      g_F3_AMCAP        : std_logic_vector( 63 downto 0)  := x"00000000_00000000";
      g_F3_DAWPR        : std_logic_vector(  7 downto 0)  := x"84";
      g_F4_ADEM         : std_logic_vector( 31 downto 0)  := x"00000000";
      g_F4_AMCAP        : std_logic_vector( 63 downto 0)  := x"00000000_00000000";
      g_F4_DAWPR        : std_logic_vector(  7 downto 0)  := x"84";
      g_F5_ADEM         : std_logic_vector( 31 downto 0)  := x"00000000";
      g_F5_AMCAP        : std_logic_vector( 63 downto 0)  := x"00000000_00000000";
      g_F5_DAWPR        : std_logic_vector(  7 downto 0)  := x"84";
      g_F6_ADEM         : std_logic_vector( 31 downto 0)  := x"00000000";
      g_F6_AMCAP        : std_logic_vector( 63 downto 0)  := x"00000000_00000000";
      g_F6_DAWPR        : std_logic_vector(  7 downto 0)  := x"84";
      g_F7_ADEM         : std_logic_vector( 31 downto 0)  := x"00000000";
      g_F7_AMCAP        : std_logic_vector( 63 downto 0)  := x"00000000_00000000";
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
      VME_IRQ_o       : out std_logic_vector( 7 downto 1);
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
      SEL_o           : out std_logic_vector(g_WB_DATA_WIDTH/8-1 downto 0);
      STB_o           : out std_logic;
      ACK_i           : in  std_logic;
      WE_o            : out std_logic;
      STALL_i         : in  std_logic;
      irq_level_i     : in  std_logic_vector( 7 downto 0) := (others => '0');
      irq_vector_i    : in  std_logic_vector( 7 downto 0) := (others => '0');
      user_csr_addr_o : out std_logic_vector(18 downto 2);
      user_csr_data_i : in  std_logic_vector( 7 downto 0) := (others => '0');
      user_csr_data_o : out std_logic_vector( 7 downto 0);
      user_csr_we_o   : out std_logic;
      user_cr_addr_o  : out std_logic_vector(18 downto 2);
      user_cr_data_i  : in  std_logic_vector( 7 downto 0) := (others => '0');
      irq_ack_o       : out std_logic;
      irq_i           : in  std_logic
    );
  end component;
end vme64x_pack;

package body vme64x_pack is

end vme64x_pack;
