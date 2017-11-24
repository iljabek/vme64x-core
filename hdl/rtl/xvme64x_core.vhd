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
-- description:   Wrapped VME64x Core
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
use work.wishbone_pkg.all;
use work.vme64x_pkg.all;

entity xvme64x_core is
    generic (
      g_CLOCK_PERIOD    : integer := -1;
      g_DECODE_AM       : boolean := true;
      g_USER_CSR_EXT    : boolean := false;

      g_MANUFACTURER_ID : std_logic_vector(23 downto 0);
      g_BOARD_ID        : std_logic_vector(31 downto 0);
      g_REVISION_ID     : std_logic_vector(31 downto 0);
      g_PROGRAM_ID      : std_logic_vector( 7 downto 0)  := c_PROGRAM_ID;
      g_ASCII_PTR       : std_logic_vector(23 downto 0)  := x"000000";
      g_BEG_USER_CR     : std_logic_vector(23 downto 0)  := x"000000";
      g_END_USER_CR     : std_logic_vector(23 downto 0)  := x"000000";
      g_BEG_CRAM        : std_logic_vector(23 downto 0)  := x"000000";
      g_END_CRAM        : std_logic_vector(23 downto 0)  := x"000000";
      g_BEG_USER_CSR    : std_logic_vector(23 downto 0)  := x"07ff33";
      g_END_USER_CSR    : std_logic_vector(23 downto 0)  := x"07ff5f";
      g_BEG_SN          : std_logic_vector(23 downto 0)  := x"000000";
      g_END_SN          : std_logic_vector(23 downto 0)  := x"000000";

      g_nbr_decoders    : natural range 1 to 8 := 2;
      g_address_decoder : t_vme64x_decoder_arr := c_vme64x_decoders_default);
    port (
      clk_i           : in  std_logic;
      rst_n_i         : in  std_logic;
      rst_n_o         : out std_logic;

      vme_i           : in t_vme64x_in;
      vme_o           : in t_vme64x_out;

      wb_o            : out t_wishbone_master_out;
      wb_i            : in  t_wishbone_master_in;

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
end xvme64x_core;

architecture wrapper of xvme64x_core is

  signal dat_out,
         dat_in             : std_logic_vector(31 downto 0);
  signal adr_out            : std_logic_vector(31 downto 0);

begin  -- wrapper

  assert wb_i.rty = '0' report "rty not supported";
  assert endian_i = "000" report "endian_i not supported";
end wrapper;
