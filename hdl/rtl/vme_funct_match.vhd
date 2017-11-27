--------------------------------------------------------------------------------
-- CERN (BE-CO-HT)
-- VME64x Core
-- http://www.ohwr.org/projects/vme64x-core
--------------------------------------------------------------------------------
--
-- unit name:     vme_funct_match
--
-- author:        Pablo Alvarez Sanchez <pablo.alvarez.sanchez@cern.ch>
--                Davide Pedretti       <davide.pedretti@cern.ch>
--
-- description:
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
use work.vme64x_pkg.all;

entity vme_funct_match is
  generic (
    g_ADEM      : t_adem_array(0 to 7);
    g_AMCAP     : t_amcap_array(0 to 7);
    g_DECODE_AM : boolean
  );
  port (
    clk_i          : in  std_logic;
    rst_n_i        : in  std_logic;

    -- Input address (to be decoded).
    addr_i         : in  std_logic_vector(31 downto 0);
    -- Sub-address of the function (the part not masked by adem).
    addr_o         : out std_logic_vector(31 downto 0);
    decode_start_i : in  std_logic;
    am_i           : in  std_logic_vector( 5 downto 0);

    ader_i         : in  t_ader_array(0 to 7);

    -- Set when a function is selected.
    decode_sel_o   : out std_logic;
    -- Set when sel_o is valid (decoding is done).
    decode_done_o  : out std_logic
  );
end vme_funct_match;

architecture rtl of vme_funct_match is
  -- Function index and ADEM from priority encoder
  signal s_function_sel : natural range 0 to 7;
  signal s_function_sel_valid : std_logic;
  signal s_decode_start_1 : std_logic;

  -- Selected function
  signal s_function      : std_logic_vector( 7 downto 0);
  signal s_ader_am_valid : std_logic_vector( 7 downto 0);
begin
  ------------------------------------------------------------------------------
  -- Address and AM comparators
  ------------------------------------------------------------------------------
  gen_match_loop : for i in 0 to 7 generate
    -- True in case of match
    s_function(i) <=
      '1' when (((addr_i(t_ADEM_M) and g_ADEM(i)(t_ADEM_M))
                 = ader_i(i)(t_ADEM_M))
                and ((am_i = ader_i(i)(t_ADER_AM))
                     or not g_DECODE_AM))
      else '0';
    -- True if the AM part of ADER is enabled by AMCAP
    s_ader_am_valid(i) <=
      g_AMCAP(i)(to_integer(unsigned(ader_i(i)(t_ADER_AM))));
  end generate;

  ------------------------------------------------------------------------------
  -- Function priority encoder
  ------------------------------------------------------------------------------
  process (clk_i)
  begin
    if rising_edge(clk_i) then
      if rst_n_i = '0' or decode_start_i = '0' then
        s_decode_start_1 <= '0';
        s_function_sel <= 0;
        s_function_sel_valid <= '0';
      else
        s_decode_start_1 <= '1';
        for i in 0 to 7 loop
          if s_function(i) = '1' then
            s_function_sel <= i;
            s_function_sel_valid <= s_ader_am_valid(i);
            exit;
          end if;
        end loop;
      end if;
    end if;
  end process;

  ------------------------------------------------------------------------------
  -- Address output latch
  ------------------------------------------------------------------------------
  process (clk_i) is
    variable mask : std_logic_vector(31 downto 0);
  begin
    if rising_edge(clk_i) then
      if rst_n_i = '0' or s_decode_start_1 = '0' then
        addr_o <= (others => '0');
        decode_done_o <= '0';
        decode_sel_o <= '0';
      else
        -- s_decode_start_1 is set.
        decode_done_o <= '1';

        if s_function_sel_valid = '1' then
          mask := (others => '0');
          mask(t_ADEM_M) := g_ADEM(s_function_sel)(t_ADEM_M);
          addr_o <= addr_i and not mask;
          decode_sel_o <= '1';
        else
          decode_sel_o <= '0';
        end if;
      end if;
    end if;
  end process;
end rtl;
