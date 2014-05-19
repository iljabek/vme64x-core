--------------------------------------------------------------------------------
-- CERN (BE-CO-HT)
-- VME64x core package
-- http://www.ohwr.org/projects/vme64x-core
--------------------------------------------------------------------------------
--
-- unit name: xvme64x_core_pkg.vhd (xvme64x_core_pkg.vhd)
--
-- author: Tomasz Wlostowski
--
-- date: 05-07-2013
--
-- version: 1.0
--
-- description: Package for VME64x core
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

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;
use WORK.wishbone_pkg.all;

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
      g_adem_a24 : std_logic_vector(31 downto 0) := x"ff800000";
      g_adem_a32 : std_logic_vector(31 downto 0) := x"ff000000"
      );
    port (
      clk_i   : in std_logic;
      rst_n_i : in std_logic;

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
      VME_IRQ_n_o     : out std_logic_vector(6 downto 0);
      VME_IACKIN_n_i  : in  std_logic;
      VME_IACK_n_i    : in  std_logic;
      VME_IACKOUT_n_o : out std_logic;
      VME_DTACK_OE_o  : out std_logic;
      VME_DATA_DIR_o  : out std_logic;
      VME_DATA_OE_N_o : out std_logic;
      VME_ADDR_DIR_o  : out std_logic;
      VME_ADDR_OE_N_o : out std_logic;

      master_o : out t_wishbone_master_out;
      master_i : in  t_wishbone_master_in;

      irq_i     : in  std_logic;
      irq_ack_o : out std_logic
      );
  end component xvme64x_core;

  component xvme64x_core_structs is
    generic (
      g_wb_data_width   : integer                       := 32;
      g_wb_addr_width   : integer                       := 64;
      g_cram_size       : integer                       := 1024;
      g_window_size_a24 : std_logic_vector(31 downto 0) := x"00080000";
      g_window_size_a32 : std_logic_vector(31 downto 0) := x"00080000");
    port (
      clk_i     : in    std_logic;
      rst_n_i   : in    std_logic;
      vme_i     : in    t_vme64x_in;
      vme_o     : out   t_vme64x_out;
      vme_b     : inout t_vme64x_bidir;
      master_o  : out   t_wishbone_master_out;
      master_i  : in    t_wishbone_master_in;
      irq_i     : in    std_logic;
      irq_ack_o : out   std_logic);
  end component xvme64x_core_structs;
  
end xvme64x_core_pkg;

package body xvme64x_core_pkg is

end xvme64x_core_pkg;
