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
-- description:   RAM memory
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
    dl : integer;
    al : integer
  );
  port (
    clk : in  std_logic;
    we  : in  std_logic;
    aw  : in  std_logic_vector(al-1 downto 0);
    di  : in  std_logic_vector(dl-1 downto 0);
    dw  : out std_logic_vector(dl-1 downto 0)
  );
end VME_CRAM;

architecture syn of VME_CRAM is

  type ram_type is array (2**al-1 downto 0) of std_logic_vector (dl-1 downto 0);
  signal CRAM : ram_type;

begin

  process (clk) begin
    if rising_edge(clk) then
      if (we = '1') then
        CRAM(to_integer(unsigned(aw))) <= di;
      end if;
      dw <= CRAM(to_integer(unsigned(aw)));
    end if;
  end process;

end syn;
