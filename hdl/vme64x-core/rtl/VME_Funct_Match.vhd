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
    clk_i       : in  std_logic;
    rst_n_i     : in  std_logic;

    addr_i      : in  std_logic_vector(31 downto 0);
    -- Sub-address of the function (the part not masked by adem).
    addr_o      : out std_logic_vector(31 downto 0);
    decode_i    : in  std_logic;
    am_i        : in  std_logic_vector( 5 downto 0);

    ader_i      : in  t_ader_array(0 to 7);

    -- Set when a function is selected (ie function_o is valid).
    sel_o       : out std_logic;
    -- Selected function.
    function_o  : out std_logic_vector( 2 downto 0)
  );
end VME_Funct_Match;

architecture rtl of VME_Funct_Match is

  -- AM matches ADER AM bits for each function
  signal s_am_match     : std_logic_vector( 7 downto 0);

  -- AM in AMCAP for each function
  signal s_am_valid     : std_logic_vector( 7 downto 0);

  -- Function index and ADEM from priority encoder
  signal s_function_sel : std_logic_vector( 7 downto 0);
  signal s_adem_sel     : std_logic_vector(31 downto 0);

  -- Selected function
  signal s_function     : std_logic_vector( 7 downto 0);
  signal s_function_ena : std_logic_vector( 7 downto 0);

  constant c_AMCAP_ALLOWED : std_logic_vector(63 downto 0) :=
    (16#38# to 16#3f# => '1', --  A24
     16#2f# => '1',           --  CR/CSR
     16#2d# | 16#29# => '1',  --  A16
     16#08# to 16#0f# => '1', --  A32
     others => '0');
  
  ------------------------------------------------------------------------------
  -- Generate AM lookup table
  ------------------------------------------------------------------------------
  -- There are 64 positions in the LUT corresponding to each AM. Each position
  -- is a vector whose bit N indicate whether function N accepts this AM.
  -- For example if s_am_lut(9) = "00001010", this means that functions 1 & 3
  -- accept AM=9.
  type t_am_lut is array (0 to 63) of std_logic_vector(7 downto 0);

  function f_gen_am_lut return t_am_lut is
    variable lut : t_am_lut := (others => "00000000");
  begin
    for i in 0 to 63 loop
      for j in 0 to 7 loop
        lut(i)(j) := g_AMCAP(j)(i);
      end loop;
    end loop;
    return lut;
  end function;

  constant c_am_lut : t_am_lut := f_gen_am_lut;

  ------------------------------------------------------------------------------
  -- Generate function enabled vector
  ------------------------------------------------------------------------------
  function f_function_ena return std_logic_vector is
    variable ena : std_logic_vector(7 downto 0) := (others => '0');
  begin
    for i in 0 to 7 loop
      if g_AMCAP(i) /= (63 downto 0 => '0') then
        ena(i) := '1';
      end if;
    end loop;
    return ena;
  end function;

  -- c_ENABLED is true when a function's AMCAP /= 0 and the previous
  -- function does not have the EFM bit set.
  constant c_ENABLED : std_logic_vector(7 downto 0) := f_function_ena;

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
  -- Function match
  ------------------------------------------------------------------------------
  gen_match_loop : for i in 0 to 7 generate
    s_function(i) <=
      '1' when (((addr_i(c_ADEM_M) and g_ADEM(i)(c_ADEM_M))
                 = ader_i(i)(c_ADEM_M))
                and (am_i = ader_i(i)(c_ADER_AM)))
      else '0';
  end generate;

  ------------------------------------------------------------------------------
  -- Function priority encoder
  ------------------------------------------------------------------------------
  process (clk_i) begin
    if rising_edge(clk_i) then
      if rst_n_i = '0' then
        s_function_sel <= (others => '0');
        s_adem_sel     <= (others => '0');
      else
        s_function_sel <= (others => '0');
        s_adem_sel     <= (others => '0');
        
        for i in 0 to 7 loop
          if s_function(i) = '1' then
            s_function_sel(i) <= '1';
            s_adem_sel (c_ADEM_M) <= g_adem(i)(c_ADEM_M);
            exit;
          end if;
        end loop;
      end if;
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- AM lookup table
  -----------------------------------------------------------------------------
  process (clk_i) begin
    if rising_edge(clk_i) then
      s_am_valid <= c_am_lut(to_integer(unsigned(am_i)));
    end if;
  end process;

  -- Check of AM against AMCAP
  s_function_ena <= s_function_sel and s_am_valid;

  ------------------------------------------------------------------------------
  -- Address output latch
  ------------------------------------------------------------------------------
  process (clk_i) begin
    if rising_edge(clk_i) then
      if rst_n_i = '0' then
        addr_o <= (others => '0');
      else
        if decode_i = '1' then
          addr_o <= addr_i and not s_adem_sel;
        end if;
      end if;
    end if;
  end process;

  ------------------------------------------------------------------------------
  -- EFD decoder and output latch
  ------------------------------------------------------------------------------
  process (clk_i) begin
    if rising_edge(clk_i) then
      if rst_n_i = '0' then
        function_o <= (others => '0');
      else
        if decode_i = '1' then
          sel_o <= '1';
          case s_function_ena is
            when "00000001" => function_o <= c_EFD_LUT(0);
            when "00000010" => function_o <= c_EFD_LUT(1);
            when "00000100" => function_o <= c_EFD_LUT(2);
            when "00001000" => function_o <= c_EFD_LUT(3);
            when "00010000" => function_o <= c_EFD_LUT(4);
            when "00100000" => function_o <= c_EFD_LUT(5);
            when "01000000" => function_o <= c_EFD_LUT(6);
            when "10000000" => function_o <= c_EFD_LUT(7);
            when others     => function_o <= (others => '0');
                               sel_o <= '0';
          end case;
        end if;
      end if;
    end if;
  end process;
end rtl;
