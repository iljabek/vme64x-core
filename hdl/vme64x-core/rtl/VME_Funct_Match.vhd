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
    g_ADEM      : t_adem_array(-1 to 7);
    g_AMCAP     : t_amcap_array(0 to 7);
    g_XAMCAP    : t_xamcap_array(0 to 7)
  );
  port (
    clk_i       : in  std_logic;
    rst_n_i     : in  std_logic;

    addr_i      : in  std_logic_vector(63 downto 0);
    -- Sub-address of the function (the part not masked by adem).
    addr_o      : out std_logic_vector(63 downto 0);
    decode_i    : in  std_logic;
    am_i        : in  std_logic_vector( 5 downto 0);
    xam_i       : in  std_logic_vector( 7 downto 0);

    ader_i      : in  t_ader_array(0 to 7);
    dfs_adem_i  : in  t_adem_array(0 to 7);

    -- Set when a function is selected (ie function_o is valid).
    sel_o       : out std_logic;
    -- Selected function.
    function_o  : out std_logic_vector( 2 downto 0)
  );
end VME_Funct_Match;

architecture rtl of VME_Funct_Match is

  type t_addr_array is array (0 to 7) of std_logic_vector(63 downto 0);

  signal s_ader         : t_addr_array;
  signal s_adem         : t_addr_array;

  -- AM matches ADER AM bits for each function
  signal s_am_match     : std_logic_vector( 7 downto 0);

  -- AM/XAM in AMCAP/XAMCAP for each function
  signal s_am_valid     : std_logic_vector( 8 downto 0);
  signal s_xam_valid    : std_logic_vector( 7 downto 0);

  -- Function index and ADEM from priority encoder
  signal s_function_sel : std_logic_vector( 7 downto 0);
  signal s_adem_sel     : std_logic_vector(63 downto 0);

  -- Selected function
  signal s_function     : std_logic_vector( 7 downto 0);
  signal s_function_ena : std_logic_vector( 7 downto 0);

  ------------------------------------------------------------------------------
  -- Generate AM lookup table
  ------------------------------------------------------------------------------
  -- There are 64 positions in the LUT corresponding to each AM. Each position
  -- is a vector whose bit N indicate whether function N accepts this AM.
  -- For example if s_am_lut(9) = "00001010", this means that functions 1 & 3
  -- accept AM=9. The lookup table has an extra bit (8) which is set only for
  -- the 2eVME AMs (0x20 & 0x21) to indicate that these are valid in XAM mode.
  type t_am_lut is array (0 to 63) of std_logic_vector(8 downto 0);

  function f_gen_am_lut return t_am_lut is
    variable lut : t_am_lut := (others => "000000000");
  begin
    for i in 0 to 63 loop
      for j in 0 to 7 loop
        lut(i)(j) := g_AMCAP(j)(i);
      end loop;
    end loop;
    lut(to_integer(unsigned(c_AM_2EVME_6U)))(8) := '1';
    lut(to_integer(unsigned(c_AM_2EVME_3U)))(8) := '1';
    return lut;
  end function;

  signal s_am_lut : t_am_lut := f_gen_am_lut;

  ------------------------------------------------------------------------------
  -- Generate XAM lookup table
  ------------------------------------------------------------------------------
  -- Same purpose as the AM lookup table, with 256 positions for each XAM.
  type t_xam_lut is array (0 to 255) of std_logic_vector(7 downto 0);

  function f_gen_xam_lut return t_xam_lut is
    variable lut : t_xam_lut;
  begin
    for i in 0 to 255 loop
      for j in 0 to 7 loop
        lut(i)(j) := g_XAMCAP(j)(i);
      end loop;
    end loop;
    return lut;
  end function;

  signal s_xam_lut : t_xam_lut := f_gen_xam_lut;

  ------------------------------------------------------------------------------
  -- Generate XAM enabled flag
  ------------------------------------------------------------------------------
  -- c_XAM_ENA is true when any XAMCAP /= 0 to conditionally enable the
  -- generation of the XAM lookup table.
  function f_xam_ena return boolean is
  begin
    for i in 0 to 7 loop
      if g_XAMCAP(i) /= (255 downto 0 => '0') then
        return true;
      end if;
    end loop;
    return false;
  end function;

  constant c_XAM_ENA : boolean := f_xam_ena;

  ------------------------------------------------------------------------------
  -- Generate function enabled vector
  ------------------------------------------------------------------------------
  function f_function_ena return std_logic_vector is
    variable ena : std_logic_vector(7 downto 0) := (others => '0');
  begin
    for i in 0 to 7 loop
      if g_AMCAP(i) /= (63 downto 0 => '0')
        and (i = 0 or g_ADEM(i-1)(c_ADEM_EFM) = '0')
      then
        ena(i) := '1';
      end if;
    end loop;
    return ena;
  end function;

  -- c_ENABLED is true when a function's AMCAP /= 0 and the previous
  -- function does not have the EFM bit set.
  constant c_ENABLED : std_logic_vector(7 downto 0) := f_function_ena;

  ------------------------------------------------------------------------------
  -- Generate function EFM/EFD enabled vector
  ------------------------------------------------------------------------------
  function f_efm_efd (v : integer) return std_logic_vector is
    variable e : std_logic_vector(7 downto 0) := (others => '0');
  begin
    -- EFM and EFD are not meaningful for function 7 (as there is no next
    -- function).
    for i in 0 to 6 loop
      e(i) := g_ADEM(i)(v);
    end loop;
    return e;
  end function;

  constant c_EFM      : std_logic_vector(7 downto 0)  := f_efm_efd(c_ADEM_EFM);
  constant c_EFD      : std_logic_vector(7 downto 0)  := f_efm_efd(c_ADEM_EFD);
  constant c_EFD_ENA  : boolean                       := c_EFD /= x"00";

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

  ------------------------------------------------------------------------------
  -- AM lookup table
  ------------------------------------------------------------------------------
  process (clk_i) begin
    if rising_edge(clk_i) then
      s_am_valid <= s_am_lut(to_integer(unsigned(am_i)));
    end if;
  end process;

  ------------------------------------------------------------------------------
  -- XAM lookup table
  ------------------------------------------------------------------------------
  gen_xam_ena : if c_XAM_ENA = true generate
    process (clk_i) begin
      if rising_edge(clk_i) then
        s_xam_valid <= s_xam_lut(to_integer(unsigned(xam_i)));
      end if;
    end process;
  end generate;
  gen_xam_dis : if c_XAM_ENA = false generate
    s_xam_valid <= x"00";
  end generate;

  ------------------------------------------------------------------------------
  -- Function match
  ------------------------------------------------------------------------------
  gen_match_loop : for i in 0 to 7 generate
    gen_ena_function: if c_ENABLED(i) = '1' generate

      -- Create 64-bit ADEM/ADER based on EFM and DFS setting
      gen_efm_ena: if c_EFM(i) = '1' generate
        --  Extra mask
        gen_dfs_ena: if g_ADEM(i)(c_ADEM_DFS) = '1' generate
            s_adem(i) <= dfs_adem_i(i+1) & dfs_adem_i(i)(c_ADEM_M) & c_ADEM_M_PAD;
        end generate;

        gen_dfs_dis: if g_ADEM(i)(c_ADEM_DFS) = '0' generate
            s_adem(i) <= g_ADEM(i+1) & g_ADEM(i)(c_ADEM_M) & c_ADEM_M_PAD;
        end generate;

        s_ader(i)(63 downto 32) <= ader_i(i+1);
      end generate;

      gen_efm_dis: if c_EFM(i) = '0' generate
        gen_dfs_ena: if g_ADEM(i)(c_ADEM_DFS) = '1' generate
            s_adem(i) <= x"ffff_ffff" & dfs_adem_i(i)(c_ADEM_M) & c_ADEM_M_PAD;
        end generate;

        gen_dfs_dis: if g_ADEM(i)(c_ADEM_DFS) = '0' generate
            s_adem(i) <= x"ffff_ffff" & g_ADEM(i)(c_ADEM_M) & c_ADEM_M_PAD;
        end generate;

        s_ader(i)(63 downto 32) <= x"0000_0000";
      end generate;

      process (ader_i(i), am_i, xam_i) begin
        if ader_i(i)(c_ADER_XAM_MODE) = '1' then
          s_ader(i)(31 downto 0) <= ader_i(i)(c_ADER_C_XAM) & c_ADER_C_XAM_PAD;

          if ader_i(i)(c_ADER_XAM) = xam_i then
            s_am_match(i) <= '1';
          else
            s_am_match(i) <= '0';
          end if;
        else
          s_ader(i)(31 downto 0) <= ader_i(i)(c_ADER_C_AM) & c_ADER_C_AM_PAD;

          if ader_i(i)(c_ADER_AM) = am_i then
            s_am_match(i) <= '1';
          else
            s_am_match(i) <= '0';
          end if;
        end if;
      end process;

      s_function(i) <= '1' when (addr_i and s_adem(i)) = s_ader(i) and
                                s_am_match(i) = '1'
                           else '0';
    end generate;

    gen_dis_function: if c_ENABLED(i) = '0' generate
      s_adem(i)     <= (others => '0');
      s_ader(i)     <= (others => '0');
      s_am_match(i) <= '0';
      s_function(i) <= '0';
    end generate;
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
        if s_function(0) = '1' then
          s_function_sel <= "00000001";
          s_adem_sel     <= s_adem(0);
        elsif s_function(1) = '1' then
          s_function_sel <= "00000010";
          s_adem_sel     <= s_adem(1);
        elsif s_function(2) = '1' then
          s_function_sel <= "00000100";
          s_adem_sel     <= s_adem(2);
        elsif s_function(3) = '1' then
          s_function_sel <= "00001000";
          s_adem_sel     <= s_adem(3);
        elsif s_function(4) = '1' then
          s_function_sel <= "00010000";
          s_adem_sel     <= s_adem(4);
        elsif s_function(5) = '1' then
          s_function_sel <= "00100000";
          s_adem_sel     <= s_adem(5);
        elsif s_function(6) = '1' then
          s_function_sel <= "01000000";
          s_adem_sel     <= s_adem(6);
        elsif s_function(7) = '1' then
          s_function_sel <= "10000000";
          s_adem_sel     <= s_adem(7);
        end if;
      end if;
    end if;
  end process;

  ------------------------------------------------------------------------------
  -- Check of AM/XAM against AMCAP/XAMCAP
  ------------------------------------------------------------------------------
  process (s_ader, s_am_valid, s_xam_valid, s_function_sel) begin
    for i in 0 to 7 loop
      if s_ader(i)(c_ADER_XAM_MODE) = '1' then
        s_function_ena(i) <= s_function_sel(i) and s_am_valid(i) and
                             s_xam_valid(i) and s_am_valid(8);
      else
        s_function_ena(i) <= s_function_sel(i) and s_am_valid(i);
      end if;
    end loop;
  end process;

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
