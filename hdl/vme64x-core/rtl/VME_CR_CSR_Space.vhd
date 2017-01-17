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
    g_MANUFACTURER_ID : std_logic_vector(23 downto 0);
    g_BOARD_ID        : std_logic_vector(31 downto 0);
    g_REVISION_ID     : std_logic_vector(31 downto 0);
    g_PROGRAM_ID      : std_logic_vector(7 downto 0);
    g_ASCII_PTR       : std_logic_vector(23 downto 0);
    g_BEG_USER_CR     : std_logic_vector(23 downto 0);
    g_END_USER_CR     : std_logic_vector(23 downto 0);
    g_BEG_CRAM        : std_logic_vector(23 downto 0);
    g_END_CRAM        : std_logic_vector(23 downto 0);
    g_BEG_USER_CSR    : std_logic_vector(23 downto 0);
    g_END_USER_CSR    : std_logic_vector(23 downto 0);
    g_BEG_SN          : std_logic_vector(23 downto 0);
    g_END_SN          : std_logic_vector(23 downto 0);
    g_F0_ADEM         : std_logic_vector( 31 downto 0);
    g_F0_AMCAP        : std_logic_vector( 63 downto 0);
    g_F0_XAMCAP       : std_logic_vector(255 downto 0);
    g_F0_DAWPR        : std_logic_vector(  7 downto 0);
    g_F1_ADEM         : std_logic_vector( 31 downto 0);
    g_F1_AMCAP        : std_logic_vector( 63 downto 0);
    g_F1_XAMCAP       : std_logic_vector(255 downto 0);
    g_F1_DAWPR        : std_logic_vector(  7 downto 0);
    g_F2_ADEM         : std_logic_vector( 31 downto 0);
    g_F2_AMCAP        : std_logic_vector( 63 downto 0);
    g_F2_XAMCAP       : std_logic_vector(255 downto 0);
    g_F2_DAWPR        : std_logic_vector(  7 downto 0);
    g_F3_ADEM         : std_logic_vector( 31 downto 0);
    g_F3_AMCAP        : std_logic_vector( 63 downto 0);
    g_F3_XAMCAP       : std_logic_vector(255 downto 0);
    g_F3_DAWPR        : std_logic_vector(  7 downto 0);
    g_F4_ADEM         : std_logic_vector( 31 downto 0);
    g_F4_AMCAP        : std_logic_vector( 63 downto 0);
    g_F4_XAMCAP       : std_logic_vector(255 downto 0);
    g_F4_DAWPR        : std_logic_vector(  7 downto 0);
    g_F5_ADEM         : std_logic_vector( 31 downto 0);
    g_F5_AMCAP        : std_logic_vector( 63 downto 0);
    g_F5_XAMCAP       : std_logic_vector(255 downto 0);
    g_F5_DAWPR        : std_logic_vector(  7 downto 0);
    g_F6_ADEM         : std_logic_vector( 31 downto 0);
    g_F6_AMCAP        : std_logic_vector( 63 downto 0);
    g_F6_XAMCAP       : std_logic_vector(255 downto 0);
    g_F6_DAWPR        : std_logic_vector(  7 downto 0);
    g_F7_ADEM         : std_logic_vector( 31 downto 0);
    g_F7_AMCAP        : std_logic_vector( 63 downto 0);
    g_F7_XAMCAP       : std_logic_vector(255 downto 0);
    g_F7_DAWPR        : std_logic_vector(  7 downto 0)
  );
  port (
    clk_i               : in  std_logic;
    rst_n_i             : in  std_logic;

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

    f0_ader_o           : out std_logic_vector(31 downto 0);
    f1_ader_o           : out std_logic_vector(31 downto 0);
    f2_ader_o           : out std_logic_vector(31 downto 0);
    f3_ader_o           : out std_logic_vector(31 downto 0);
    f4_ader_o           : out std_logic_vector(31 downto 0);
    f5_ader_o           : out std_logic_vector(31 downto 0);
    f6_ader_o           : out std_logic_vector(31 downto 0);
    f7_ader_o           : out std_logic_vector(31 downto 0);

    f0_faf_ader_i       : in  std_logic_vector(31 downto 0);
    f1_faf_ader_i       : in  std_logic_vector(31 downto 0);
    f2_faf_ader_i       : in  std_logic_vector(31 downto 0);
    f3_faf_ader_i       : in  std_logic_vector(31 downto 0);
    f4_faf_ader_i       : in  std_logic_vector(31 downto 0);
    f5_faf_ader_i       : in  std_logic_vector(31 downto 0);
    f6_faf_ader_i       : in  std_logic_vector(31 downto 0);
    f7_faf_ader_i       : in  std_logic_vector(31 downto 0);

    f0_dfs_adem_i       : in  std_logic_vector(31 downto 0);
    f1_dfs_adem_i       : in  std_logic_vector(31 downto 0);
    f2_dfs_adem_i       : in  std_logic_vector(31 downto 0);
    f3_dfs_adem_i       : in  std_logic_vector(31 downto 0);
    f4_dfs_adem_i       : in  std_logic_vector(31 downto 0);
    f5_dfs_adem_i       : in  std_logic_vector(31 downto 0);
    f6_dfs_adem_i       : in  std_logic_vector(31 downto 0);
    f7_dfs_adem_i       : in  std_logic_vector(31 downto 0)
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

  type t_reg_array is array (integer range <>) of std_logic_vector(31 downto 0);

  signal s_reg_ader             : t_reg_array(0 to 7);
  signal s_ader                 : t_reg_array(0 to 7);
  signal s_faf_ader             : t_reg_array(0 to 7);
  signal s_dfs_adem             : t_reg_array(0 to 7);

  constant c_ADEM : t_reg_array(-1 to 7) := (
    x"00000000",
    g_F0_ADEM, g_F1_ADEM, g_F2_ADEM, g_F3_ADEM,
    g_F4_ADEM, g_F5_ADEM, g_F6_ADEM, g_F7_ADEM
  );

  signal s_cr_rom : t_cr_array(1023 downto 0) := f_vme_cr_encode(
    g_MANUFACTURER_ID, g_BOARD_ID, g_REVISION_ID, g_PROGRAM_ID,
    g_ASCII_PTR,
    g_BEG_USER_CR, g_END_USER_CR,
    g_BEG_CRAM, g_END_CRAM,
    g_BEG_USER_CSR, g_END_USER_CSR,
    g_BEG_SN, g_END_SN,
    g_F0_ADEM, g_F0_AMCAP, g_F0_XAMCAP, g_F0_DAWPR,
    g_F1_ADEM, g_F1_AMCAP, g_F1_XAMCAP, g_F1_DAWPR,
    g_F2_ADEM, g_F2_AMCAP, g_F2_XAMCAP, g_F2_DAWPR,
    g_F3_ADEM, g_F3_AMCAP, g_F3_XAMCAP, g_F3_DAWPR,
    g_F4_ADEM, g_F4_AMCAP, g_F4_XAMCAP, g_F4_DAWPR,
    g_F5_ADEM, g_F5_AMCAP, g_F5_XAMCAP, g_F5_DAWPR,
    g_F6_ADEM, g_F6_AMCAP, g_F6_XAMCAP, g_F6_DAWPR,
    g_F7_ADEM, g_F7_AMCAP, g_F7_XAMCAP, g_F7_DAWPR
  );

begin

  s_addr <= unsigned(addr_i);

  ------------------------------------------------------------------------------
  -- Defined CR
  ------------------------------------------------------------------------------
  s_cr_access  <= '1' when s_addr >= c_BEG_CR(18 downto 2) and
                           s_addr <= c_END_CR(18 downto 2)
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
  s_csr_access <= '1' when s_addr >= c_BEG_CSR(18 downto 2) and
                           s_addr <= c_END_CSR(18 downto 2)
                      else '0';

  -- If the crate is not driving the GA lines or the parity is even the BAR
  -- register is set to 0x00 and the board will not answer CR/CSR accesses.
  s_ga_parity  <= vme_ga_i(5) xor vme_ga_i(4) xor vme_ga_i(3) xor
                  vme_ga_i(2) xor vme_ga_i(1) xor vme_ga_i(0);

  -- Write
  process (clk_i)
  begin
    if rising_edge(clk_i) then
      if rst_n_i = '0' then
        if s_ga_parity = '1' then
          s_reg_bar       <= (not vme_ga_i(4 downto 0)) & "000";
        else
          s_reg_bar       <= x"00";
        end if;
        s_reg_bit_reg     <= x"00";
        s_reg_cram_owner  <= x"00";
        s_reg_usr_bit_reg <= x"00";
        s_reg_ader        <= (others => x"00000000");
      else
        if we_i = '1' and s_csr_access = '1' then
          case s_addr is
            when c_ADDR_BAR(18 downto 2) =>
              s_reg_bar <= data_i;

            when c_ADDR_BIT_SET_REG(18 downto 2) =>
              for i in 0 to 7 loop
                s_reg_bit_reg(i) <= s_reg_bit_reg(i) or data_i(i);
              end loop;

            when c_ADDR_BIT_CLR_REG(18 downto 2) =>
              for i in 0 to 7 loop
                s_reg_bit_reg(i) <= s_reg_bit_reg(i) and (not data_i(i));
              end loop;
              if data_i(2) = '1' then
                s_reg_cram_owner <= x"00";
              end if;

            when c_ADDR_CRAM_OWNER(18 downto 2) =>
              if s_reg_cram_owner = x"00" then
                s_reg_cram_owner <= data_i;
                s_reg_bit_reg(2) <= '1';
              end if;

            when c_ADDR_USR_SET_REG(18 downto 2) =>
              for i in 0 to 7 loop
                s_reg_usr_bit_reg(i) <= s_reg_usr_bit_reg(i) or data_i(i);
              end loop;

            when c_ADDR_USR_CLR_REG(18 downto 2) =>
              for i in 0 to 7 loop
                s_reg_usr_bit_reg(i) <= s_reg_usr_bit_reg(i) and (not data_i(i));
              end loop;

            when c_ADDR_F7_ADER_0(18 downto 2) => s_reg_ader(7)( 7 downto  0) <= data_i;
            when c_ADDR_F7_ADER_1(18 downto 2) => s_reg_ader(7)(15 downto  8) <= data_i;
            when c_ADDR_F7_ADER_2(18 downto 2) => s_reg_ader(7)(23 downto 16) <= data_i;
            when c_ADDR_F7_ADER_3(18 downto 2) => s_reg_ader(7)(31 downto 24) <= data_i;
            when c_ADDR_F6_ADER_0(18 downto 2) => s_reg_ader(6)( 7 downto  0) <= data_i;
            when c_ADDR_F6_ADER_1(18 downto 2) => s_reg_ader(6)(15 downto  8) <= data_i;
            when c_ADDR_F6_ADER_2(18 downto 2) => s_reg_ader(6)(23 downto 16) <= data_i;
            when c_ADDR_F6_ADER_3(18 downto 2) => s_reg_ader(6)(31 downto 24) <= data_i;
            when c_ADDR_F5_ADER_0(18 downto 2) => s_reg_ader(5)( 7 downto  0) <= data_i;
            when c_ADDR_F5_ADER_1(18 downto 2) => s_reg_ader(5)(15 downto  8) <= data_i;
            when c_ADDR_F5_ADER_2(18 downto 2) => s_reg_ader(5)(23 downto 16) <= data_i;
            when c_ADDR_F5_ADER_3(18 downto 2) => s_reg_ader(5)(31 downto 24) <= data_i;
            when c_ADDR_F4_ADER_0(18 downto 2) => s_reg_ader(4)( 7 downto  0) <= data_i;
            when c_ADDR_F4_ADER_1(18 downto 2) => s_reg_ader(4)(15 downto  8) <= data_i;
            when c_ADDR_F4_ADER_2(18 downto 2) => s_reg_ader(4)(23 downto 16) <= data_i;
            when c_ADDR_F4_ADER_3(18 downto 2) => s_reg_ader(4)(31 downto 24) <= data_i;
            when c_ADDR_F3_ADER_0(18 downto 2) => s_reg_ader(3)( 7 downto  0) <= data_i;
            when c_ADDR_F3_ADER_1(18 downto 2) => s_reg_ader(3)(15 downto  8) <= data_i;
            when c_ADDR_F3_ADER_2(18 downto 2) => s_reg_ader(3)(23 downto 16) <= data_i;
            when c_ADDR_F3_ADER_3(18 downto 2) => s_reg_ader(3)(31 downto 24) <= data_i;
            when c_ADDR_F2_ADER_0(18 downto 2) => s_reg_ader(2)( 7 downto  0) <= data_i;
            when c_ADDR_F2_ADER_1(18 downto 2) => s_reg_ader(2)(15 downto  8) <= data_i;
            when c_ADDR_F2_ADER_2(18 downto 2) => s_reg_ader(2)(23 downto 16) <= data_i;
            when c_ADDR_F2_ADER_3(18 downto 2) => s_reg_ader(2)(31 downto 24) <= data_i;
            when c_ADDR_F1_ADER_0(18 downto 2) => s_reg_ader(1)( 7 downto  0) <= data_i;
            when c_ADDR_F1_ADER_1(18 downto 2) => s_reg_ader(1)(15 downto  8) <= data_i;
            when c_ADDR_F1_ADER_2(18 downto 2) => s_reg_ader(1)(23 downto 16) <= data_i;
            when c_ADDR_F1_ADER_3(18 downto 2) => s_reg_ader(1)(31 downto 24) <= data_i;
            when c_ADDR_F0_ADER_0(18 downto 2) => s_reg_ader(0)( 7 downto  0) <= data_i;
            when c_ADDR_F0_ADER_1(18 downto 2) => s_reg_ader(0)(15 downto  8) <= data_i;
            when c_ADDR_F0_ADER_2(18 downto 2) => s_reg_ader(0)(23 downto 16) <= data_i;
            when c_ADDR_F0_ADER_3(18 downto 2) => s_reg_ader(0)(31 downto 24) <= data_i;

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
  f0_ader_o         <= s_ader(0) when s_ader(0)(c_ADER_DFSR) = '0' else (others => '0');
  f1_ader_o         <= s_ader(1) when s_ader(1)(c_ADER_DFSR) = '0' else (others => '0');
  f2_ader_o         <= s_ader(2) when s_ader(2)(c_ADER_DFSR) = '0' else (others => '0');
  f3_ader_o         <= s_ader(3) when s_ader(3)(c_ADER_DFSR) = '0' else (others => '0');
  f4_ader_o         <= s_ader(4) when s_ader(4)(c_ADER_DFSR) = '0' else (others => '0');
  f5_ader_o         <= s_ader(5) when s_ader(5)(c_ADER_DFSR) = '0' else (others => '0');
  f6_ader_o         <= s_ader(6) when s_ader(6)(c_ADER_DFSR) = '0' else (others => '0');
  f7_ader_o         <= s_ader(7) when s_ader(7)(c_ADER_DFSR) = '0' else (others => '0');
  module_enable_o   <= s_reg_bit_reg(4);
  vme_sysfail_ena_o <= s_reg_bit_reg(6);
  module_reset_o    <= s_reg_bit_reg(7);

  -- Handle DFS and FAF
  s_faf_ader <= (f0_faf_ader_i, f1_faf_ader_i, f2_faf_ader_i, f3_faf_ader_i,
                 f4_faf_ader_i, f5_faf_ader_i, f6_faf_ader_i, f7_faf_ader_i);

  s_dfs_adem <= (f0_dfs_adem_i, f1_dfs_adem_i, f2_dfs_adem_i, f3_dfs_adem_i,
                 f4_dfs_adem_i, f5_dfs_adem_i, f6_dfs_adem_i, f7_dfs_adem_i);

  process (s_reg_ader, s_faf_ader, s_dfs_adem)
  begin
    for i in 0 to 7 loop
      if (c_ADEM(i-1)(c_ADEM_EFM) = '1' and c_ADEM(i-1)(c_ADEM_FAF) = '1') or
         (c_ADEM(i-1)(c_ADEM_EFM) = '0' and c_ADEM(i)(c_ADEM_FAF) = '1')
      then
        s_ader(i) <= s_faf_ader(i);
      elsif (c_ADEM(i-1)(c_ADEM_EFM) = '1' and c_ADEM(i-1)(c_ADEM_DFS) = '1' and s_reg_ader(i-1)(c_ADER_DFSR) = '1') then
        s_ader(i) <= s_dfs_adem(i);
      elsif (c_ADEM(i-1)(c_ADEM_EFM) = '0' and c_ADEM(i)(c_ADEM_DFS) = '1' and s_reg_ader(i)(c_ADER_DFSR) = '1') then
        s_ader(i) <= s_dfs_adem(i)(31 downto 8) & s_reg_ader(i)(7 downto 0);
      else
        s_ader(i) <= s_reg_ader(i);
      end if;
    end loop;
  end process;

  -- Read
  process (clk_i)
  begin
    if rising_edge(clk_i) then
      if rst_n_i = '0' then
        s_csr_data <= x"ff";
      else
        case s_addr is
          when c_ADDR_BAR(18 downto 2)         => s_csr_data <= s_reg_bar;
          when c_ADDR_BIT_SET_REG(18 downto 2) => s_csr_data <= s_reg_bit_reg;
          when c_ADDR_BIT_CLR_REG(18 downto 2) => s_csr_data <= s_reg_bit_reg;
          when c_ADDR_CRAM_OWNER(18 downto 2)  => s_csr_data <= s_reg_cram_owner;
          when c_ADDR_USR_SET_REG(18 downto 2) => s_csr_data <= s_reg_usr_bit_reg;
          when c_ADDR_USR_CLR_REG(18 downto 2) => s_csr_data <= s_reg_usr_bit_reg;
          when c_ADDR_F7_ADER_0(18 downto 2)   => s_csr_data <= s_ader(7)( 7 downto  0);
          when c_ADDR_F7_ADER_1(18 downto 2)   => s_csr_data <= s_ader(7)(15 downto  8);
          when c_ADDR_F7_ADER_2(18 downto 2)   => s_csr_data <= s_ader(7)(23 downto 16);
          when c_ADDR_F7_ADER_3(18 downto 2)   => s_csr_data <= s_ader(7)(31 downto 24);
          when c_ADDR_F6_ADER_0(18 downto 2)   => s_csr_data <= s_ader(6)( 7 downto  0);
          when c_ADDR_F6_ADER_1(18 downto 2)   => s_csr_data <= s_ader(6)(15 downto  8);
          when c_ADDR_F6_ADER_2(18 downto 2)   => s_csr_data <= s_ader(6)(23 downto 16);
          when c_ADDR_F6_ADER_3(18 downto 2)   => s_csr_data <= s_ader(6)(31 downto 24);
          when c_ADDR_F5_ADER_0(18 downto 2)   => s_csr_data <= s_ader(5)( 7 downto  0);
          when c_ADDR_F5_ADER_1(18 downto 2)   => s_csr_data <= s_ader(5)(15 downto  8);
          when c_ADDR_F5_ADER_2(18 downto 2)   => s_csr_data <= s_ader(5)(23 downto 16);
          when c_ADDR_F5_ADER_3(18 downto 2)   => s_csr_data <= s_ader(5)(31 downto 24);
          when c_ADDR_F4_ADER_0(18 downto 2)   => s_csr_data <= s_ader(4)( 7 downto  0);
          when c_ADDR_F4_ADER_1(18 downto 2)   => s_csr_data <= s_ader(4)(15 downto  8);
          when c_ADDR_F4_ADER_2(18 downto 2)   => s_csr_data <= s_ader(4)(23 downto 16);
          when c_ADDR_F4_ADER_3(18 downto 2)   => s_csr_data <= s_ader(4)(31 downto 24);
          when c_ADDR_F3_ADER_0(18 downto 2)   => s_csr_data <= s_ader(3)( 7 downto  0);
          when c_ADDR_F3_ADER_1(18 downto 2)   => s_csr_data <= s_ader(3)(15 downto  8);
          when c_ADDR_F3_ADER_2(18 downto 2)   => s_csr_data <= s_ader(3)(23 downto 16);
          when c_ADDR_F3_ADER_3(18 downto 2)   => s_csr_data <= s_ader(3)(31 downto 24);
          when c_ADDR_F2_ADER_0(18 downto 2)   => s_csr_data <= s_ader(2)( 7 downto  0);
          when c_ADDR_F2_ADER_1(18 downto 2)   => s_csr_data <= s_ader(2)(15 downto  8);
          when c_ADDR_F2_ADER_2(18 downto 2)   => s_csr_data <= s_ader(2)(23 downto 16);
          when c_ADDR_F2_ADER_3(18 downto 2)   => s_csr_data <= s_ader(2)(31 downto 24);
          when c_ADDR_F1_ADER_0(18 downto 2)   => s_csr_data <= s_ader(1)( 7 downto  0);
          when c_ADDR_F1_ADER_1(18 downto 2)   => s_csr_data <= s_ader(1)(15 downto  8);
          when c_ADDR_F1_ADER_2(18 downto 2)   => s_csr_data <= s_ader(1)(23 downto 16);
          when c_ADDR_F1_ADER_3(18 downto 2)   => s_csr_data <= s_ader(1)(31 downto 24);
          when c_ADDR_F0_ADER_0(18 downto 2)   => s_csr_data <= s_ader(0)( 7 downto  0);
          when c_ADDR_F0_ADER_1(18 downto 2)   => s_csr_data <= s_ader(0)(15 downto  8);
          when c_ADDR_F0_ADER_2(18 downto 2)   => s_csr_data <= s_ader(0)(23 downto 16);
          when c_ADDR_F0_ADER_3(18 downto 2)   => s_csr_data <= s_ader(0)(31 downto 24);
          when others                          => s_csr_data <= x"ff";
        end case;
      end if;
    end if;
  end process;

  ------------------------------------------------------------------------------
  -- CRAM
  ------------------------------------------------------------------------------
  gen_cram: if f_size(g_BEG_CRAM, g_END_CRAM) > 1 generate
    s_cram_access <= '1' when s_addr(18 downto 2) >= unsigned(g_BEG_CRAM(18 downto 2)) and
                              s_addr(18 downto 2) <= unsigned(g_END_CRAM(18 downto 2))
                         else '0';

    s_cram_addr   <= std_logic_vector(s_addr - unsigned(g_BEG_CRAM(18 downto 2)));
    s_cram_we     <= we_i and s_cram_access;

    cmp_cram: VME_CRAM
      generic map (
        g_BEG_CRAM => g_BEG_CRAM,
        g_END_CRAM => g_END_CRAM
      )
      port map (
        clk_i  => clk_i,
        we_i   => s_cram_we,
        addr_i => s_cram_addr,
        data_i => data_i,
        data_o => s_cram_data
      );
  end generate;
  gen_no_cram: if f_size(g_BEG_CRAM, g_END_CRAM) <= 1 generate
    s_cram_access <= '0';
    s_cram_addr   <= (others => '0');
    s_cram_data   <= x"00";
  end generate;

  ------------------------------------------------------------------------------
  -- User CR/CSR
  ------------------------------------------------------------------------------
  gen_user_cr: if f_size(g_BEG_USER_CR, g_END_USER_CR) > 1 generate
    s_user_cr_access <= '1' when s_addr >= unsigned(g_BEG_USER_CR(18 downto 2)) and
                                 s_addr <= unsigned(g_END_USER_CR(18 downto 2))
                            else '0';

    user_cr_addr_o   <= std_logic_vector(s_addr - unsigned(g_BEG_USER_CR(18 downto 2)));
  end generate;
  gen_no_user_cr: if f_size(g_BEG_USER_CR, g_END_USER_CR) <= 1 generate
    s_user_cr_access <= '0';
    user_cr_addr_o   <= (others => '0');
  end generate;

  gen_user_csr: if f_size(g_BEG_USER_CSR, g_END_USER_CSR) > 1 generate
    s_user_csr_access <= '1' when s_addr >= unsigned(g_BEG_USER_CSR(18 downto 2)) and
                                  s_addr <= unsigned(g_END_USER_CSR(18 downto 2))
                             else '0';

    user_csr_addr_o   <= std_logic_vector(s_addr - unsigned(g_BEG_USER_CSR(18 downto 2)));
  end generate;
  gen_no_user_csr: if f_size(g_BEG_USER_CSR, g_END_USER_CSR) <= 1 generate
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
