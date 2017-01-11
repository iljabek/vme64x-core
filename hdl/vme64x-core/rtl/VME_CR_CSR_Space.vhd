--------------------------------------------------------------------------------
-- CERN (BE-CO-HT)
-- VME64x Core
-- http://www.ohwr.org/projects/vme64x-core
--------------------------------------------------------------------------------
--
-- unit name:     VME_CR_CSR_Space (VME_CR_CSR_Space.vhd)
--
-- author:        Pablo Alvarez Sanchez <pablo.alvarez.sanchez@cern.ch>
--                Davide Pedretti       <davide.pedretti@cern.ch>
--
-- description:
--
--   Implementation of CR/CSR space.
--
--                            width = 1 byte
--                 /---------------------------------/
--                 _________________________________
--                |                                 | 0x7ffff
--                |     Defined and Reserved CSR    |
--                |                                 |
--                |   Table 10-13 "Defined Control  |
--                |   Status register Assignments"  |
--                |       ANSI/VITA 1.1-1997        |
--                |        VME64 Extensions         |
--                |_________________________________| 0x7fc00
--                |_________________________________|
--                |                                 | 0xXXXXX
--                |            User CSR             |
--                |_________________________________| 0xXXXXX
--                |_________________________________|
--                |                                 | 0xXXXXX
--                |              CRAM               |
--                |_________________________________| 0xXXXXX
--                |_________________________________|
--                |                                 | 0xXXXXX
--                |             User CR             |
--                |_________________________________| 0xXXXXX
--                |_________________________________|
--                |                                 | 0x00fff
--                |     Defined and reserved CR     |
--                |                                 |
--                |     Table 10-12 "Defined        |
--                |  Configuration ROM Assignments" |
--                |       ANSI/VITA 1.1-1997        |
--                |        VME64 Extensions         |
--                |_________________________________| 0x00000
--
--   Please note that only every fourth location in the CR/CSR space is used,
--   so it is possible read and write the CR/CSR by selecting the data transfer
--   mode D08 (byte 3), D16 (bytes 2 & 3) or D32. If other data transfer modes
--   are used the operation will not be successful.
--
--   If the size of the register is bigger than 1 byte, (e.g. ADER is 4 bytes)
--   these bytes are stored in BIG ENDIAN order.
--
--   How to use the CRAM:
--
--     1) The Master first reads the CRAM_OWNER register (location 0x7fff3).
--        If it is zero the CRAM is available.
--     2) The Master writes his ID to the CRAM_OWNER register.
--     3) If the Master can readback his ID from the CRAM_OWNER register it
--        means that he is the owner of the CRAM and has exclusive access.
--     4) If other Masters write their ID to the CRAM_OWNER register when it
--        contains a non-zero value, the write operation will not be successful.
--        This allows the first Master that writes a non-zero value to acquire
--        ownership.
--     5) When a Master has ownership of the CRAM, bit 2 of the Bit Set Register
--        (location 0x7fffb) will be set.
--     6) The Master can release the ownership by writing '1' to bit 2 of the
--        Bit Clr Register (location 0x7fff7).
--
--   Bit Set Register control bits (location 0x7fffb):
--
--     7: RESET -----------> When high the module is held in reset.
--     6: SYSFAIL ENABLE --> When high the VME_SYSFAIL output driver is enabled.
--     5: FAILED ----------> When high the module has failed.
--     4: ENABLE ----------> When high the WB accesses are enabled.
--     3: BERR ------------> When high the module has asserted BERR.
--     2: CRAM OWNER ------> When high the CRAM is owned.
--
--   The Master can clear these bits by writing '1' in the corresponding bits
--   to the Bit Clr Register (location 0x7fff7).
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

entity VME_CR_CSR_Space is
  generic (
    g_beg_user_cr   : std_logic_vector(23 downto 0);
    g_end_user_cr   : std_logic_vector(23 downto 0);
    g_beg_cram      : std_logic_vector(23 downto 0);
    g_end_cram      : std_logic_vector(23 downto 0);
    g_beg_user_csr  : std_logic_vector(23 downto 0);
    g_end_user_csr  : std_logic_vector(23 downto 0);
    g_cr_space      : t_cr_array
  );
  port (
    clk_i               : in  std_logic;
    reset_i             : in  std_logic;

    vme_ga_i            : in  std_logic_vector(5 downto 0);
    vme_berr_n_i        : in  std_logic;
    bar_o               : out std_logic_vector(4 downto 0);
    vme_sysfail_i       : in  std_logic;
    vme_sysfail_ena_o   : out std_logic;
    module_enable_o     : out std_logic;
    module_reset_o      : out std_logic;

    addr_i              : in  std_logic_vector(18 downto 2);
    data_i              : in  std_logic_vector( 7 downto 0);
    data_o              : out std_logic_vector( 7 downto 0);
    we_i                : in  std_logic;

    user_csr_addr_o     : out std_logic_vector(18 downto 2);
    user_csr_data_i     : in  std_logic_vector( 7 downto 0);
    user_csr_data_o     : out std_logic_vector( 7 downto 0);
    user_csr_we_o       : out std_logic;

    user_cr_addr_o      : out std_logic_vector(18 downto 2);
    user_cr_data_i      : in  std_logic_vector( 7 downto 0);

    ader0_o             : out std_logic_vector(31 downto 0);
    ader1_o             : out std_logic_vector(31 downto 0);
    ader2_o             : out std_logic_vector(31 downto 0);
    ader3_o             : out std_logic_vector(31 downto 0);
    ader4_o             : out std_logic_vector(31 downto 0);
    ader5_o             : out std_logic_vector(31 downto 0);
    ader6_o             : out std_logic_vector(31 downto 0);
    ader7_o             : out std_logic_vector(31 downto 0)
  );
end VME_CR_CSR_Space;

architecture rtl of VME_CR_CSR_Space is

  signal s_addr                 : unsigned(18 downto 2);

  signal s_ga_parity            : std_logic;

  signal s_cr_access            : std_logic;
  signal s_csr_access           : std_logic;
  signal s_cram_access          : std_logic;
  signal s_user_cr_access       : std_logic;
  signal s_user_csr_access      : std_logic;

  signal s_cr_data              : std_logic_vector(7 downto 0);
  signal s_csr_data             : std_logic_vector(7 downto 0);
  signal s_cram_data            : std_logic_vector(7 downto 0);

  signal s_cram_addr            : std_logic_vector(18 downto 2);
  signal s_cram_we              : std_logic;

  signal s_reg_bar              : std_logic_vector(7 downto 0);
  signal s_reg_bit_reg          : std_logic_vector(7 downto 0);
  signal s_reg_cram_owner       : std_logic_vector(7 downto 0);
  signal s_reg_usr_bit_reg      : std_logic_vector(7 downto 0);

  signal s_reg_ader0            : std_logic_vector(31 downto 0);
  signal s_reg_ader1            : std_logic_vector(31 downto 0);
  signal s_reg_ader2            : std_logic_vector(31 downto 0);
  signal s_reg_ader3            : std_logic_vector(31 downto 0);
  signal s_reg_ader4            : std_logic_vector(31 downto 0);
  signal s_reg_ader5            : std_logic_vector(31 downto 0);
  signal s_reg_ader6            : std_logic_vector(31 downto 0);
  signal s_reg_ader7            : std_logic_vector(31 downto 0);

  signal s_cr_rom               : t_cr_array(g_cr_space'range)  := g_cr_space;

begin

  s_addr <= unsigned(addr_i);

  ------------------------------------------------------------------------------
  -- Defined CR
  ------------------------------------------------------------------------------
  s_cr_access  <= '1' when s_addr >= c_beg_cr(18 downto 2) and
                           s_addr <= c_end_cr(18 downto 2)
                      else '0';

  process (clk_i)
  begin
    if rising_edge(clk_i) then
      s_cr_data <= s_cr_rom(to_integer(s_addr));
    end if;
  end process;

  ------------------------------------------------------------------------------
  -- Defined CSR
  ------------------------------------------------------------------------------
  s_csr_access <= '1' when s_addr >= c_beg_csr(18 downto 2) and
                           s_addr <= c_end_csr(18 downto 2)
                      else '0';

  -- If the crate is not driving the GA lines or the parity is even the BAR
  -- register is set to 0x00 and the board will not answer CR/CSR accesses.
  s_ga_parity  <= vme_ga_i(5) xor vme_ga_i(4) xor vme_ga_i(3) xor
                  vme_ga_i(2) xor vme_ga_i(1) xor vme_ga_i(0);

  -- Write
  process (clk_i)
  begin
    if rising_edge(clk_i) then
      if reset_i = '1' then
        if s_ga_parity = '1' then
          s_reg_bar       <= (not vme_ga_i(4 downto 0)) & "000";
        else
          s_reg_bar       <= x"00";
        end if;
        s_reg_bit_reg     <= x"00";
        s_reg_cram_owner  <= x"00";
        s_reg_usr_bit_reg <= x"00";
        s_reg_ader0       <= x"00000000";
        s_reg_ader1       <= x"00000000";
        s_reg_ader2       <= x"00000000";
        s_reg_ader3       <= x"00000000";
        s_reg_ader4       <= x"00000000";
        s_reg_ader5       <= x"00000000";
        s_reg_ader6       <= x"00000000";
        s_reg_ader7       <= x"00000000";
      else
        if we_i = '1' and s_csr_access = '1' then
          case s_addr is
            when c_addr_bar(18 downto 2) =>
              s_reg_bar <= data_i;

            when c_addr_bit_set_reg(18 downto 2) =>
              for i in 0 to 7 loop
                s_reg_bit_reg(i) <= s_reg_bit_reg(i) or data_i(i);
              end loop;

            when c_addr_bit_clr_reg(18 downto 2) =>
              for i in 0 to 7 loop
                s_reg_bit_reg(i) <= s_reg_bit_reg(i) and (not data_i(i));
              end loop;
              if data_i(2) = '1' then
                s_reg_cram_owner <= x"00";
              end if;

            when c_addr_cram_owner(18 downto 2) =>
              if s_reg_cram_owner = x"00" then
                s_reg_cram_owner <= data_i;
                s_reg_bit_reg(2) <= '1';
              end if;

            when c_addr_usr_set_reg(18 downto 2) =>
              for i in 0 to 7 loop
                s_reg_usr_bit_reg(i) <= s_reg_usr_bit_reg(i) or data_i(i);
              end loop;

            when c_addr_usr_clr_reg(18 downto 2) =>
              for i in 0 to 7 loop
                s_reg_usr_bit_reg(i) <= s_reg_usr_bit_reg(i) and (not data_i(i));
              end loop;

            when c_addr_func7_ader_0(18 downto 2) => s_reg_ader7( 7 downto  0) <= data_i;
            when c_addr_func7_ader_1(18 downto 2) => s_reg_ader7(15 downto  8) <= data_i;
            when c_addr_func7_ader_2(18 downto 2) => s_reg_ader7(23 downto 16) <= data_i;
            when c_addr_func7_ader_3(18 downto 2) => s_reg_ader7(31 downto 24) <= data_i;
            when c_addr_func6_ader_0(18 downto 2) => s_reg_ader6( 7 downto  0) <= data_i;
            when c_addr_func6_ader_1(18 downto 2) => s_reg_ader6(15 downto  8) <= data_i;
            when c_addr_func6_ader_2(18 downto 2) => s_reg_ader6(23 downto 16) <= data_i;
            when c_addr_func6_ader_3(18 downto 2) => s_reg_ader6(31 downto 24) <= data_i;
            when c_addr_func5_ader_0(18 downto 2) => s_reg_ader5( 7 downto  0) <= data_i;
            when c_addr_func5_ader_1(18 downto 2) => s_reg_ader5(15 downto  8) <= data_i;
            when c_addr_func5_ader_2(18 downto 2) => s_reg_ader5(23 downto 16) <= data_i;
            when c_addr_func5_ader_3(18 downto 2) => s_reg_ader5(31 downto 24) <= data_i;
            when c_addr_func4_ader_0(18 downto 2) => s_reg_ader4( 7 downto  0) <= data_i;
            when c_addr_func4_ader_1(18 downto 2) => s_reg_ader4(15 downto  8) <= data_i;
            when c_addr_func4_ader_2(18 downto 2) => s_reg_ader4(23 downto 16) <= data_i;
            when c_addr_func4_ader_3(18 downto 2) => s_reg_ader4(31 downto 24) <= data_i;
            when c_addr_func3_ader_0(18 downto 2) => s_reg_ader3( 7 downto  0) <= data_i;
            when c_addr_func3_ader_1(18 downto 2) => s_reg_ader3(15 downto  8) <= data_i;
            when c_addr_func3_ader_2(18 downto 2) => s_reg_ader3(23 downto 16) <= data_i;
            when c_addr_func3_ader_3(18 downto 2) => s_reg_ader3(31 downto 24) <= data_i;
            when c_addr_func2_ader_0(18 downto 2) => s_reg_ader2( 7 downto  0) <= data_i;
            when c_addr_func2_ader_1(18 downto 2) => s_reg_ader2(15 downto  8) <= data_i;
            when c_addr_func2_ader_2(18 downto 2) => s_reg_ader2(23 downto 16) <= data_i;
            when c_addr_func2_ader_3(18 downto 2) => s_reg_ader2(31 downto 24) <= data_i;
            when c_addr_func1_ader_0(18 downto 2) => s_reg_ader1( 7 downto  0) <= data_i;
            when c_addr_func1_ader_1(18 downto 2) => s_reg_ader1(15 downto  8) <= data_i;
            when c_addr_func1_ader_2(18 downto 2) => s_reg_ader1(23 downto 16) <= data_i;
            when c_addr_func1_ader_3(18 downto 2) => s_reg_ader1(31 downto 24) <= data_i;
            when c_addr_func0_ader_0(18 downto 2) => s_reg_ader0( 7 downto  0) <= data_i;
            when c_addr_func0_ader_1(18 downto 2) => s_reg_ader0(15 downto  8) <= data_i;
            when c_addr_func0_ader_2(18 downto 2) => s_reg_ader0(23 downto 16) <= data_i;
            when c_addr_func0_ader_3(18 downto 2) => s_reg_ader0(31 downto 24) <= data_i;

            when others => null;
          end case;
        end if;

        if vme_berr_n_i = '0' then
          s_reg_bit_reg(3) <= '1';
        end if;

        if vme_sysfail_i = '1' then
          s_reg_bit_reg(5) <= '1';
        end if;
      end if;
    end if;
  end process;

  bar_o             <= s_reg_bar(7 downto 3);
  ader0_o           <= s_reg_ader0;
  ader1_o           <= s_reg_ader1;
  ader2_o           <= s_reg_ader2;
  ader3_o           <= s_reg_ader3;
  ader4_o           <= s_reg_ader4;
  ader5_o           <= s_reg_ader5;
  ader6_o           <= s_reg_ader6;
  ader7_o           <= s_reg_ader7;
  module_enable_o   <= s_reg_bit_reg(4);
  vme_sysfail_ena_o <= s_reg_bit_reg(6);
  module_reset_o    <= s_reg_bit_reg(7);

  -- Read
  process (clk_i)
  begin
    if rising_edge(clk_i) then
      if reset_i = '1' then
        s_csr_data <= x"ff";
      else
        case s_addr is
          when c_addr_bar(18 downto 2)          => s_csr_data <= s_reg_bar;
          when c_addr_bit_set_reg(18 downto 2)  => s_csr_data <= s_reg_bit_reg;
          when c_addr_bit_clr_reg(18 downto 2)  => s_csr_data <= s_reg_bit_reg;
          when c_addr_cram_owner(18 downto 2)   => s_csr_data <= s_reg_cram_owner;
          when c_addr_usr_set_reg(18 downto 2)  => s_csr_data <= s_reg_usr_bit_reg;
          when c_addr_usr_clr_reg(18 downto 2)  => s_csr_data <= s_reg_usr_bit_reg;
          when c_addr_func7_ader_0(18 downto 2) => s_csr_data <= s_reg_ader7( 7 downto  0);
          when c_addr_func7_ader_1(18 downto 2) => s_csr_data <= s_reg_ader7(15 downto  8);
          when c_addr_func7_ader_2(18 downto 2) => s_csr_data <= s_reg_ader7(23 downto 16);
          when c_addr_func7_ader_3(18 downto 2) => s_csr_data <= s_reg_ader7(31 downto 24);
          when c_addr_func6_ader_0(18 downto 2) => s_csr_data <= s_reg_ader6( 7 downto  0);
          when c_addr_func6_ader_1(18 downto 2) => s_csr_data <= s_reg_ader6(15 downto  8);
          when c_addr_func6_ader_2(18 downto 2) => s_csr_data <= s_reg_ader6(23 downto 16);
          when c_addr_func6_ader_3(18 downto 2) => s_csr_data <= s_reg_ader6(31 downto 24);
          when c_addr_func5_ader_0(18 downto 2) => s_csr_data <= s_reg_ader5( 7 downto  0);
          when c_addr_func5_ader_1(18 downto 2) => s_csr_data <= s_reg_ader5(15 downto  8);
          when c_addr_func5_ader_2(18 downto 2) => s_csr_data <= s_reg_ader5(23 downto 16);
          when c_addr_func5_ader_3(18 downto 2) => s_csr_data <= s_reg_ader5(31 downto 24);
          when c_addr_func4_ader_0(18 downto 2) => s_csr_data <= s_reg_ader4( 7 downto  0);
          when c_addr_func4_ader_1(18 downto 2) => s_csr_data <= s_reg_ader4(15 downto  8);
          when c_addr_func4_ader_2(18 downto 2) => s_csr_data <= s_reg_ader4(23 downto 16);
          when c_addr_func4_ader_3(18 downto 2) => s_csr_data <= s_reg_ader4(31 downto 24);
          when c_addr_func3_ader_0(18 downto 2) => s_csr_data <= s_reg_ader3( 7 downto  0);
          when c_addr_func3_ader_1(18 downto 2) => s_csr_data <= s_reg_ader3(15 downto  8);
          when c_addr_func3_ader_2(18 downto 2) => s_csr_data <= s_reg_ader3(23 downto 16);
          when c_addr_func3_ader_3(18 downto 2) => s_csr_data <= s_reg_ader3(31 downto 24);
          when c_addr_func2_ader_0(18 downto 2) => s_csr_data <= s_reg_ader2( 7 downto  0);
          when c_addr_func2_ader_1(18 downto 2) => s_csr_data <= s_reg_ader2(15 downto  8);
          when c_addr_func2_ader_2(18 downto 2) => s_csr_data <= s_reg_ader2(23 downto 16);
          when c_addr_func2_ader_3(18 downto 2) => s_csr_data <= s_reg_ader2(31 downto 24);
          when c_addr_func1_ader_0(18 downto 2) => s_csr_data <= s_reg_ader1( 7 downto  0);
          when c_addr_func1_ader_1(18 downto 2) => s_csr_data <= s_reg_ader1(15 downto  8);
          when c_addr_func1_ader_2(18 downto 2) => s_csr_data <= s_reg_ader1(23 downto 16);
          when c_addr_func1_ader_3(18 downto 2) => s_csr_data <= s_reg_ader1(31 downto 24);
          when c_addr_func0_ader_0(18 downto 2) => s_csr_data <= s_reg_ader0( 7 downto  0);
          when c_addr_func0_ader_1(18 downto 2) => s_csr_data <= s_reg_ader0(15 downto  8);
          when c_addr_func0_ader_2(18 downto 2) => s_csr_data <= s_reg_ader0(23 downto 16);
          when c_addr_func0_ader_3(18 downto 2) => s_csr_data <= s_reg_ader0(31 downto 24);
          when others                           => s_csr_data <= x"ff";
        end case;
      end if;
    end if;
  end process;

  ------------------------------------------------------------------------------
  -- CRAM
  ------------------------------------------------------------------------------
  gen_cram: if f_size(g_beg_cram, g_end_cram) > 1 generate
    s_cram_access <= '1' when s_addr(18 downto 2) >= unsigned(g_beg_cram(18 downto 2)) and
                              s_addr(18 downto 2) <= unsigned(g_end_cram(18 downto 2))
                         else '0';

    s_cram_addr   <= std_logic_vector(s_addr - unsigned(g_beg_cram(18 downto 2)));
    s_cram_we     <= we_i and s_cram_access;

    cmp_cram: VME_CRAM
      generic map (
        g_beg_cram => g_beg_cram,
        g_end_cram => g_end_cram
      )
      port map (
        clk_i  => clk_i,
        we_i   => s_cram_we,
        addr_i => s_cram_addr,
        data_i => data_i,
        data_o => s_cram_data
      );
  end generate;
  gen_no_cram: if f_size(g_beg_cram, g_end_cram) <= 1 generate
    s_cram_access <= '0';
    s_cram_addr   <= (others => '0');
    s_cram_data   <= x"00";
  end generate;

  ------------------------------------------------------------------------------
  -- User CR/CSR
  ------------------------------------------------------------------------------
  gen_user_cr: if f_size(g_beg_user_cr, g_end_user_cr) > 1 generate
    s_user_cr_access <= '1' when s_addr >= unsigned(g_beg_user_cr(18 downto 2)) and
                                 s_addr <= unsigned(g_end_user_cr(18 downto 2))
                            else '0';

    user_cr_addr_o   <= std_logic_vector(s_addr - unsigned(g_beg_user_cr(18 downto 2)));
  end generate;
  gen_no_user_cr: if f_size(g_beg_user_cr, g_end_user_cr) <= 1 generate
    s_user_cr_access <= '0';
    user_cr_addr_o   <= (others => '0');
  end generate;

  gen_user_csr: if f_size(g_beg_user_csr, g_end_user_csr) > 1 generate
    s_user_csr_access <= '1' when s_addr >= unsigned(g_beg_user_csr(18 downto 2)) and
                                  s_addr <= unsigned(g_end_user_csr(18 downto 2))
                             else '0';

    user_csr_addr_o   <= std_logic_vector(s_addr - unsigned(g_beg_user_csr(18 downto 2)));
  end generate;
  gen_no_user_csr: if f_size(g_beg_user_csr, g_end_user_csr) <= 1 generate
    s_user_csr_access <= '0';
    user_csr_addr_o   <= (others => '0');
  end generate;

  user_csr_data_o <= data_i;
  user_csr_we_o   <= we_i and s_user_csr_access;

  ------------------------------------------------------------------------------
  -- Read Multiplexer
  ------------------------------------------------------------------------------
  process (
    s_cr_access,       s_cr_data,
    s_csr_access,      s_csr_data,
    s_cram_access,     s_cram_data,
    s_user_cr_access,  user_cr_data_i,
    s_user_csr_access, user_csr_data_i
  ) begin
    if s_cr_access = '1' then
      data_o <= s_cr_data;
    elsif s_csr_access = '1' then
      data_o <= s_csr_data;
    elsif s_cram_access = '1' then
      data_o <= s_cram_data;
    elsif s_user_cr_access = '1' then
      data_o <= user_cr_data_i;
    elsif s_user_csr_access = '1' then
      data_o <= user_csr_data_i;
    else
      data_o <= x"ff";
    end if;
  end process;

end rtl;
