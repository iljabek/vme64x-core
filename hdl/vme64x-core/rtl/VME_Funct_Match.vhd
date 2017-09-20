--------------------------------------------------------------------------------
-- CERN (BE-CO-HT)
-- VME64x Core
-- http://www.ohwr.org/projects/vme64x-core
--------------------------------------------------------------------------------
--
-- unit name:     VME_Funct_Match (VME_Funct_Match.vhd)
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
use work.vme64x_pack.all;

entity VME_Funct_Match is
  generic (
    g_ADEM      : t_adem_array(0 to 7);
    g_AMCAP     : t_amcap_array(0 to 7)
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

    -- Set when a function is selected (ie function_o is valid).
    decode_sel_o   : out std_logic;
    -- Set when sel_o is valid (decoding is done).
    decode_done_o  : out std_logic;
    -- Selected function.
    function_o     : out std_logic_vector( 2 downto 0)
  );
end VME_Funct_Match;

architecture rtl of VME_Funct_Match is
  -- Function index and ADEM from priority encoder
  signal s_function_sel : natural range 0 to 7;
  signal s_function_sel_valid : std_logic;
  signal s_decode_start_1 : std_logic;
  
  -- Selected function
  signal s_function      : std_logic_vector( 7 downto 0);
  signal s_ader_am_valid : std_logic_vector( 7 downto 0);

  -- List of supported AM.
  constant c_AMCAP_ALLOWED : std_logic_vector(63 downto 0) :=
    (16#3d# to 16#3f# => '1', --  A24
     16#39# to 16#3b# => '1',
     16#2d# | 16#29#  => '1', --  A16
     16#0d# to 16#0f# => '1', --  A32
     16#09# to 16#0b# => '1',
     others => '0');
  
  ------------------------------------------------------------------------------
  -- Generate EFD lookup table
  ------------------------------------------------------------------------------
  type t_efd_lut is array (0 to 7) of std_logic_vector(2 downto 0);

  function f_gen_efd_lut return t_efd_lut is
    variable lut : t_efd_lut;
  begin
    lut(0) := "000";
    for i in 1 to 7 loop
      if g_ADEM(i-1)(c_ADEM_EFD) = '1' then
        lut(i) := lut(i - 1);
      else
        lut(i) := std_logic_vector(to_unsigned(i, 3));
      end if;
    end loop;
    return lut;
  end function;

  -- Map from function defined by address to real function. Handle extra
  -- decoders.
  constant c_EFD_LUT : t_efd_lut := f_gen_efd_lut;

begin
  --  Check for invalid bits in ADEM/AMCAP
  gen_gchecks: for i in 7 downto 0 generate
    assert g_ADEM(i)(c_ADEM_FAF) = '0' report "FAF bit set in ADEM"
      severity error;
    assert g_ADEM(i)(c_ADEM_DFS) = '0' report "DFS bit set in ADEM"
      severity error;
    assert g_ADEM(i)(c_ADEM_EFM) = '0' report "EFM bit set in ADEM"
      severity error;
    assert (g_AMCAP(i) and c_AMCAP_ALLOWED) = g_AMCAP(i)
      report "bit set in AMCAP for not supported AM"
      severity error;
  end generate;
  
  ------------------------------------------------------------------------------
  -- Address and AM comparators
  ------------------------------------------------------------------------------
  gen_match_loop : for i in 0 to 7 generate
    -- True in case of match
    s_function(i) <=
      '1' when (((addr_i(c_ADEM_M) and g_ADEM(i)(c_ADEM_M))
                 = ader_i(i)(c_ADEM_M))
                and (am_i = ader_i(i)(c_ADER_AM)))
      else '0';
    -- True if the AM part of ADER is enabled by AMCAP
    s_ader_am_valid(i) <=
      g_AMCAP(i)(to_integer(unsigned(ader_i(i)(c_ADER_AM))));
  end generate;

  ------------------------------------------------------------------------------
  -- Function priority encoder
  ------------------------------------------------------------------------------
  process (clk_i) begin
    if rising_edge(clk_i) then
      s_function_sel <= 0;
      s_function_sel_valid <= '0';
      s_decode_start_1 <= '0';
      
      if rst_n_i = '0' then
        null;
      elsif decode_start_i = '1' then
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
        function_o <= (others => '0');
        decode_done_o <= '0';
        decode_sel_o <= '0';
      else
        -- s_decode_start_1 is set.
        decode_done_o <= '1';
        
        if s_function_sel_valid = '1' then
          function_o <= c_EFD_LUT(s_function_sel);
          mask := (others => '0');
          mask(c_ADEM_M) := g_adem(s_function_sel)(c_ADEM_M);
          addr_o <= addr_i and not mask;
          decode_sel_o <= '1';
        else
          decode_sel_o <= '0';
        end if;
      end if;
    end if;
  end process;
end rtl;
