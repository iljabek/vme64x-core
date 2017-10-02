--------------------------------------------------------------------------------
-- CERN (BE-CO-HT)
-- VME64x Core
-- http://www.ohwr.org/projects/vme64x-core
--------------------------------------------------------------------------------
--
-- unit name:     xvme64x_core_pkg (xvme64x_core_pkg.vhd)
--
-- author:        Tomasz Wlostowski <tomasz.wlostowski@cern.ch>
--
-- description:   Package for wrapped VME64x Core
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
-- last changes: see svn log.
--------------------------------------------------------------------------------
-- TODO: -
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.wishbone_pkg.all;
use work.vme64x_pack.all;

package xvme64x_core_pkg is

  ------------------------------------------------------------------------------
  -- Types declaration
  ------------------------------------------------------------------------------
  type t_vme64x_in is record
    as_n     : std_logic;
    rst_n    : std_logic;
    write_n  : std_logic;
    am       : std_logic_vector(5 downto 0);
    ds_n     : std_logic_vector(1 downto 0);
    ga       : std_logic_vector(5 downto 0);
    bbsy_n   : std_logic;
    iack_n   : std_logic;
    iackin_n : std_logic;
  end record;

  type t_vme64x_out is record
    iackout_n : std_logic;
    dtack_oe  : std_logic;
    dtack_n   : std_logic;
    data_dir  : std_logic;
    data_oe_n : std_logic;
    addr_dir  : std_logic;
    addr_oe_n : std_logic;
    retry_n   : std_logic;
    retry_oe  : std_logic;
    berr      : std_logic;
    irq_n     : std_logic_vector(6 downto 0);
  end record;

  type t_vme64x_bidir is record
    lword_n : std_logic;
    addr    : std_logic_vector(31 downto 1);
    data    : std_logic_vector(31 downto 0);
  end record;

  ------------------------------------------------------------------------------
  -- Components declaration
  ------------------------------------------------------------------------------
  component xvme64x_core
    generic (
      g_CLOCK_PERIOD    : integer                         := c_CLOCK_PERIOD;
      g_WB_DATA_WIDTH   : integer                         := c_wishbone_data_width;
      g_WB_ADDR_WIDTH   : integer                         := c_wishbone_address_width;
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
      g_F0_AMCAP        : std_logic_vector( 63 downto 0)  := x"00000000_0000bb00";
      g_F0_DAWPR        : std_logic_vector(  7 downto 0)  := x"84";
      g_F1_ADEM         : std_logic_vector( 31 downto 0)  := x"fff80000";
      g_F1_AMCAP        : std_logic_vector( 63 downto 0)  := x"bb000000_00000000";
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
      VME_RETRY_OE_o  : out std_logic;
      VME_LWORD_n_b_i : in  std_logic;
      VME_LWORD_n_b_o : out std_logic;
      VME_ADDR_b_i    : in  std_logic_vector(31 downto 1);
      VME_ADDR_b_o    : out std_logic_vector(31 downto 1);
      VME_DATA_b_i    : in  std_logic_vector(31 downto 0);
      VME_DATA_b_o    : out std_logic_vector(31 downto 0);
      VME_IRQ_n_o     : out std_logic_vector( 7 downto 1);
      VME_IACKIN_n_i  : in  std_logic;
      VME_IACK_n_i    : in  std_logic;
      VME_IACKOUT_n_o : out std_logic;
      VME_DTACK_OE_o  : out std_logic;
      VME_DATA_DIR_o  : out std_logic;
      VME_DATA_OE_N_o : out std_logic;
      VME_ADDR_DIR_o  : out std_logic;
      VME_ADDR_OE_N_o : out std_logic;
      master_o        : out t_wishbone_master_out;
      master_i        : in  t_wishbone_master_in;
      irq_i           : in  std_logic;
      irq_ack_o       : out std_logic;
      irq_level_i     : in  std_logic_vector( 7 downto 0) := (others => '0');
      irq_vector_i    : in  std_logic_vector( 7 downto 0) := (others => '0');
      endian_i        : in  std_logic_vector( 2 downto 0) := (others => '0');
      user_csr_addr_o : out std_logic_vector(18 downto 2);
      user_csr_data_i : in  std_logic_vector( 7 downto 0) := (others => '0');
      user_csr_data_o : out std_logic_vector( 7 downto 0);
      user_csr_we_o   : out std_logic;
      user_cr_addr_o  : out std_logic_vector(18 downto 2);
      user_cr_data_i  : in  std_logic_vector( 7 downto 0) := (others => '0')
    );
  end component xvme64x_core;

end xvme64x_core_pkg;

package body xvme64x_core_pkg is

end xvme64x_core_pkg;
