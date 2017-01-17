--------------------------------------------------------------------------------
-- CERN (BE-CO-HT)
-- VME64x Core
-- http://www.ohwr.org/projects/vme64x-core
--------------------------------------------------------------------------------
--
-- unit name:     VME_CRAM (VME_CRAM.vhd)
--
-- author:        Pablo Alvarez Sanchez <pablo.alvarez.sanchez@cern.ch>
--                Davide Pedretti       <davide.pedretti@cern.ch>
--
-- description:   CRAM memory
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

entity VME_CRAM is
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
end VME_CRAM;

architecture rtl of VME_CRAM is

  type t_cram is array (f_size(g_BEG_CRAM, g_END_CRAM)-1 downto 0)
                 of std_logic_vector(7 downto 0);

  signal s_cram   : t_cram;
  signal s_addr   : unsigned(18 downto 2);
  signal s_addr_1 : unsigned(18 downto 2);

begin

  s_addr <= unsigned(addr_i(18 downto 2));

  process (clk_i) begin
    if rising_edge(clk_i) then
      if we_i = '1' then
        s_cram(to_integer(s_addr)) <= data_i;
      end if;
      s_addr_1 <= s_addr;
    end if;
  end process;

  data_o <= s_cram(to_integer(s_addr_1));

end rtl;
