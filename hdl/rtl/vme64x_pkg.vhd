--------------------------------------------------------------------------------
-- CERN (BE-CO-HT)
-- VME64x Core
-- http://www.ohwr.org/projects/vme64x-core
--------------------------------------------------------------------------------
--
-- unit name:     vme64x_pkg
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

use work.wishbone_pkg.all;

package vme64x_pkg is

  ------------------------------------------------------------------------------
  -- Constants
  ------------------------------------------------------------------------------

  -- Manufactuer IDs.
  constant c_CERN_ID          : std_logic_vector(23 downto 0) := x"080030";

  -- Boards IDs / Revision IDs.
  -- For SVEC:
  constant c_SVEC_ID          : std_logic_vector(31 downto 0) := x"00000198";
  constant c_SVEC_REVISION_ID : std_logic_vector(31 downto 0) := x"00000001";

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
  -- Table 2.4 "Extended Address Modifier Code" page 12 2eSST
  --  ANSI/VITA 1.5-2003(R2009)
  subtype am_vec_type is std_logic_vector(5 downto 0);
  constant c_AM_A24_S_SUP     : am_vec_type := "111101";  -- 0x3d
  constant c_AM_A24_S         : am_vec_type := "111001";  -- 0x39
  constant c_AM_A24_BLT       : am_vec_type := "111011";  -- 0x3b
  constant c_AM_A24_BLT_SUP   : am_vec_type := "111111";  -- 0x3f
  constant c_AM_A24_MBLT      : am_vec_type := "111000";  -- 0x38
  constant c_AM_A24_MBLT_SUP  : am_vec_type := "111100";  -- 0x3c
  constant c_AM_A24_LCK       : am_vec_type := "110010";  -- 0x32
  constant c_AM_CR_CSR        : am_vec_type := "101111";  -- 0x2f
  constant c_AM_A16           : am_vec_type := "101001";  -- 0x29
  constant c_AM_A16_SUP       : am_vec_type := "101101";  -- 0x2d
  constant c_AM_A16_LCK       : am_vec_type := "101100";  -- 0x2c
  constant c_AM_A32           : am_vec_type := "001001";  -- 0x09
  constant c_AM_A32_SUP       : am_vec_type := "001101";  -- 0x0d
  constant c_AM_A32_BLT       : am_vec_type := "001011";  -- 0x0b
  constant c_AM_A32_BLT_SUP   : am_vec_type := "001111";  -- 0x0f
  constant c_AM_A32_MBLT      : am_vec_type := "001000";  -- 0x08
  constant c_AM_A32_MBLT_SUP  : am_vec_type := "001100";  -- 0x0c
  constant c_AM_A32_LCK       : am_vec_type := "000101";  -- 0x05
  constant c_AM_A64           : am_vec_type := "000001";  -- 0x01
  constant c_AM_A64_BLT       : am_vec_type := "000011";  -- 0x03
  constant c_AM_A64_MBLT      : am_vec_type := "000000";  -- 0x00
  constant c_AM_A64_LCK       : am_vec_type := "000100";  -- 0x04
  constant c_AM_2EVME_6U      : am_vec_type := "100000";  -- 0x20
  constant c_AM_2EVME_3U      : am_vec_type := "100001";  -- 0x21

  subtype xam_vec_type is std_logic_vector(7 downto 0);
  constant c_AM_A32_2EVME     : xam_vec_type := "00000001";  -- 0x01
  constant c_AM_A64_2EVME     : xam_vec_type := "00000010";  -- 0x02
  constant c_AM_A32_2ESST     : xam_vec_type := "00010001";  -- 0x11
  constant c_AM_A64_2ESST     : xam_vec_type := "00010010";  -- 0x12

  ------------------------------------------------------------------------------
  -- Types
  ------------------------------------------------------------------------------

  -- CR/CSR parameter arrays
  type t_adem_array   is
    array (integer range <>) of std_logic_vector( 31 downto 0);
  type t_ader_array   is
    array (integer range <>) of std_logic_vector( 31 downto 0);
  type t_amcap_array  is
    array (integer range <>) of std_logic_vector( 63 downto 0);
  type t_dawpr_array  is
    array (integer range <>) of std_logic_vector(  7 downto 0);

  type t_vme64x_in is record
    as_n     : std_logic;
    rst_n    : std_logic;
    write_n  : std_logic;
    am       : std_logic_vector(5 downto 0);
    ds_n     : std_logic_vector(1 downto 0);
    ga       : std_logic_vector(5 downto 0);
    lword_n  : std_logic;
    data     : std_logic_vector(31 downto 0);
    addr     : std_logic_vector(31 downto 1);
    iack_n   : std_logic;
    iackin_n : std_logic;
  end record;

  type t_vme64x_out is record
    iackout_n : std_logic;
    dtack_n   : std_logic;
    dtack_oe  : std_logic;
    lword_n   : std_logic;
    data      : std_logic_vector(31 downto 0);
    data_dir  : std_logic;
    data_oe_n : std_logic;
    addr      : std_logic_vector(31 downto 1);
    addr_dir  : std_logic;
    addr_oe_n : std_logic;
    retry_n   : std_logic;
    retry_oe  : std_logic;
    berr_n    : std_logic;
    irq_n     : std_logic_vector(6 downto 0);
  end record;

  type t_vme64x_decoder is record
    adem       : std_logic_vector(31 downto 0);
    amcap      : std_logic_vector(63 downto 0);
    dawpr      : std_logic_vector( 7 downto 0);
  end record;

  constant c_vme64x_decoder_disabled : t_vme64x_decoder := (
    adem  => x"00000000",
    amcap => x"00000000_00000000",
    dawpr => x"84");

  type t_vme64x_decoder_arr is array(0 to 7) of t_vme64x_decoder;

  constant c_vme64x_decoders_default : t_vme64x_decoder_arr := (
    0 => (adem  => x"ff000000",
          amcap => x"00000000_0000bb00",
          dawpr => x"84"),
    1 => (adem  => x"fff80000",
          amcap => x"bb000000_00000000",
          dawpr => x"84"),
    others => c_vme64x_decoder_disabled);

  ------------------------------------------------------------------------------
  -- Components declaration
  ------------------------------------------------------------------------------
  component xvme64x_core
    generic (
      -- Clock period (ns).  Used for DS synchronization.
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
  end component xvme64x_core;
end vme64x_pkg;
