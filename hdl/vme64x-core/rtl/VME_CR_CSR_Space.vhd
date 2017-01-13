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
    g_manufacturer_id : std_logic_vector(23 downto 0);
    g_board_id        : std_logic_vector(31 downto 0);
    g_revision_id     : std_logic_vector(31 downto 0);
    g_program_id      : std_logic_vector(7 downto 0);
    g_ascii_ptr       : std_logic_vector(23 downto 0);
    g_beg_user_cr     : std_logic_vector(23 downto 0);
    g_end_user_cr     : std_logic_vector(23 downto 0);
    g_beg_cram        : std_logic_vector(23 downto 0);
    g_end_cram        : std_logic_vector(23 downto 0);
    g_beg_user_csr    : std_logic_vector(23 downto 0);
    g_end_user_csr    : std_logic_vector(23 downto 0);
    g_beg_sn          : std_logic_vector(23 downto 0);
    g_end_sn          : std_logic_vector(23 downto 0);
    g_f0_adem         : std_logic_vector( 31 downto 0);
    g_f0_amcap        : std_logic_vector( 63 downto 0);
    g_f0_xamcap       : std_logic_vector(255 downto 0);
    g_f0_dawpr        : std_logic_vector(  7 downto 0);
    g_f1_adem         : std_logic_vector( 31 downto 0);
    g_f1_amcap        : std_logic_vector( 63 downto 0);
    g_f1_xamcap       : std_logic_vector(255 downto 0);
    g_f1_dawpr        : std_logic_vector(  7 downto 0);
    g_f2_adem         : std_logic_vector( 31 downto 0);
    g_f2_amcap        : std_logic_vector( 63 downto 0);
    g_f2_xamcap       : std_logic_vector(255 downto 0);
    g_f2_dawpr        : std_logic_vector(  7 downto 0);
    g_f3_adem         : std_logic_vector( 31 downto 0);
    g_f3_amcap        : std_logic_vector( 63 downto 0);
    g_f3_xamcap       : std_logic_vector(255 downto 0);
    g_f3_dawpr        : std_logic_vector(  7 downto 0);
    g_f4_adem         : std_logic_vector( 31 downto 0);
    g_f4_amcap        : std_logic_vector( 63 downto 0);
    g_f4_xamcap       : std_logic_vector(255 downto 0);
    g_f4_dawpr        : std_logic_vector(  7 downto 0);
    g_f5_adem         : std_logic_vector( 31 downto 0);
    g_f5_amcap        : std_logic_vector( 63 downto 0);
    g_f5_xamcap       : std_logic_vector(255 downto 0);
    g_f5_dawpr        : std_logic_vector(  7 downto 0);
    g_f6_adem         : std_logic_vector( 31 downto 0);
    g_f6_amcap        : std_logic_vector( 63 downto 0);
    g_f6_xamcap       : std_logic_vector(255 downto 0);
    g_f6_dawpr        : std_logic_vector(  7 downto 0);
    g_f7_adem         : std_logic_vector( 31 downto 0);
    g_f7_amcap        : std_logic_vector( 63 downto 0);
    g_f7_xamcap       : std_logic_vector(255 downto 0);
    g_f7_dawpr        : std_logic_vector(  7 downto 0)
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

  type t_reg_array is array (0 to 7) of std_logic_vector(31 downto 0);

  signal s_reg_ader             : t_reg_array;
  signal s_ader                 : t_reg_array;
  signal s_faf_ader             : t_reg_array;
  signal s_dfs_adem             : t_reg_array;

  constant c_adem : t_reg_array := (
    g_f0_adem, g_f1_adem, g_f2_adem, g_f3_adem,
    g_f4_adem, g_f5_adem, g_f6_adem, g_f7_adem
  );

  signal s_cr_rom : t_cr_array(1023 downto 0) := f_vme_cr_encode(
    g_manufacturer_id, g_board_id, g_revision_id, g_program_id,
    g_ascii_ptr,
    g_beg_user_cr, g_end_user_cr,
    g_beg_cram, g_end_cram,
    g_beg_user_csr, g_end_user_csr,
    g_beg_sn, g_end_sn,
    g_f0_adem, g_f0_amcap, g_f0_xamcap, g_f0_dawpr,
    g_f1_adem, g_f1_amcap, g_f1_xamcap, g_f1_dawpr,
    g_f2_adem, g_f2_amcap, g_f2_xamcap, g_f2_dawpr,
    g_f3_adem, g_f3_amcap, g_f3_xamcap, g_f3_dawpr,
    g_f4_adem, g_f4_amcap, g_f4_xamcap, g_f4_dawpr,
    g_f5_adem, g_f5_amcap, g_f5_xamcap, g_f5_dawpr,
    g_f6_adem, g_f6_amcap, g_f6_xamcap, g_f6_dawpr,
    g_f7_adem, g_f7_amcap, g_f7_xamcap, g_f7_dawpr
  );

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

            when c_addr_f7_ader_0(18 downto 2) => s_reg_ader(7)( 7 downto  0) <= data_i;
            when c_addr_f7_ader_1(18 downto 2) => s_reg_ader(7)(15 downto  8) <= data_i;
            when c_addr_f7_ader_2(18 downto 2) => s_reg_ader(7)(23 downto 16) <= data_i;
            when c_addr_f7_ader_3(18 downto 2) => s_reg_ader(7)(31 downto 24) <= data_i;
            when c_addr_f6_ader_0(18 downto 2) => s_reg_ader(6)( 7 downto  0) <= data_i;
            when c_addr_f6_ader_1(18 downto 2) => s_reg_ader(6)(15 downto  8) <= data_i;
            when c_addr_f6_ader_2(18 downto 2) => s_reg_ader(6)(23 downto 16) <= data_i;
            when c_addr_f6_ader_3(18 downto 2) => s_reg_ader(6)(31 downto 24) <= data_i;
            when c_addr_f5_ader_0(18 downto 2) => s_reg_ader(5)( 7 downto  0) <= data_i;
            when c_addr_f5_ader_1(18 downto 2) => s_reg_ader(5)(15 downto  8) <= data_i;
            when c_addr_f5_ader_2(18 downto 2) => s_reg_ader(5)(23 downto 16) <= data_i;
            when c_addr_f5_ader_3(18 downto 2) => s_reg_ader(5)(31 downto 24) <= data_i;
            when c_addr_f4_ader_0(18 downto 2) => s_reg_ader(4)( 7 downto  0) <= data_i;
            when c_addr_f4_ader_1(18 downto 2) => s_reg_ader(4)(15 downto  8) <= data_i;
            when c_addr_f4_ader_2(18 downto 2) => s_reg_ader(4)(23 downto 16) <= data_i;
            when c_addr_f4_ader_3(18 downto 2) => s_reg_ader(4)(31 downto 24) <= data_i;
            when c_addr_f3_ader_0(18 downto 2) => s_reg_ader(3)( 7 downto  0) <= data_i;
            when c_addr_f3_ader_1(18 downto 2) => s_reg_ader(3)(15 downto  8) <= data_i;
            when c_addr_f3_ader_2(18 downto 2) => s_reg_ader(3)(23 downto 16) <= data_i;
            when c_addr_f3_ader_3(18 downto 2) => s_reg_ader(3)(31 downto 24) <= data_i;
            when c_addr_f2_ader_0(18 downto 2) => s_reg_ader(2)( 7 downto  0) <= data_i;
            when c_addr_f2_ader_1(18 downto 2) => s_reg_ader(2)(15 downto  8) <= data_i;
            when c_addr_f2_ader_2(18 downto 2) => s_reg_ader(2)(23 downto 16) <= data_i;
            when c_addr_f2_ader_3(18 downto 2) => s_reg_ader(2)(31 downto 24) <= data_i;
            when c_addr_f1_ader_0(18 downto 2) => s_reg_ader(1)( 7 downto  0) <= data_i;
            when c_addr_f1_ader_1(18 downto 2) => s_reg_ader(1)(15 downto  8) <= data_i;
            when c_addr_f1_ader_2(18 downto 2) => s_reg_ader(1)(23 downto 16) <= data_i;
            when c_addr_f1_ader_3(18 downto 2) => s_reg_ader(1)(31 downto 24) <= data_i;
            when c_addr_f0_ader_0(18 downto 2) => s_reg_ader(0)( 7 downto  0) <= data_i;
            when c_addr_f0_ader_1(18 downto 2) => s_reg_ader(0)(15 downto  8) <= data_i;
            when c_addr_f0_ader_2(18 downto 2) => s_reg_ader(0)(23 downto 16) <= data_i;
            when c_addr_f0_ader_3(18 downto 2) => s_reg_ader(0)(31 downto 24) <= data_i;

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
  f0_ader_o         <= s_reg_ader(0);
  f1_ader_o         <= s_reg_ader(1);
  f2_ader_o         <= s_reg_ader(2);
  f3_ader_o         <= s_reg_ader(3);
  f4_ader_o         <= s_reg_ader(4);
  f5_ader_o         <= s_reg_ader(5);
  f6_ader_o         <= s_reg_ader(6);
  f7_ader_o         <= s_reg_ader(7);
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
    -- Function 0
    if c_adem(0)(ADEM_FAF) = '1' then
      s_ader(0) <= s_faf_ader(0);
    elsif c_adem(0)(ADEM_DFS) = '1' and s_reg_ader(0)(ADER_DFSR) = '1' then
      s_ader(0) <= s_dfs_adem(0)(31 downto 8) & s_reg_ader(0)(7 downto 0);
    else
      s_ader(0) <= s_reg_ader(0);
    end if;

    -- Function 1..7
    for i in 1 to 7 loop
      if (c_adem(i-1)(ADEM_EFM) = '1' and c_adem(i-1)(ADEM_FAF) = '1') or
         (c_adem(i-1)(ADEM_EFM) = '0' and c_adem(i)(ADEM_FAF) = '1')
      then
        s_ader(i) <= s_faf_ader(i);
      elsif (c_adem(i-1)(ADEM_EFM) = '1' and c_adem(i-1)(ADEM_DFS) = '1' and s_reg_ader(i-1)(ADER_DFSR) = '1') then
        s_ader(i) <= s_dfs_adem(i);
      elsif (c_adem(i-1)(ADEM_EFM) = '0' and c_adem(i)(ADEM_DFS) = '1' and s_reg_ader(i)(ADER_DFSR) = '1') then
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
          when c_addr_bar(18 downto 2)         => s_csr_data <= s_reg_bar;
          when c_addr_bit_set_reg(18 downto 2) => s_csr_data <= s_reg_bit_reg;
          when c_addr_bit_clr_reg(18 downto 2) => s_csr_data <= s_reg_bit_reg;
          when c_addr_cram_owner(18 downto 2)  => s_csr_data <= s_reg_cram_owner;
          when c_addr_usr_set_reg(18 downto 2) => s_csr_data <= s_reg_usr_bit_reg;
          when c_addr_usr_clr_reg(18 downto 2) => s_csr_data <= s_reg_usr_bit_reg;
          when c_addr_f7_ader_0(18 downto 2)   => s_csr_data <= s_ader(7)( 7 downto  0);
          when c_addr_f7_ader_1(18 downto 2)   => s_csr_data <= s_ader(7)(15 downto  8);
          when c_addr_f7_ader_2(18 downto 2)   => s_csr_data <= s_ader(7)(23 downto 16);
          when c_addr_f7_ader_3(18 downto 2)   => s_csr_data <= s_ader(7)(31 downto 24);
          when c_addr_f6_ader_0(18 downto 2)   => s_csr_data <= s_ader(6)( 7 downto  0);
          when c_addr_f6_ader_1(18 downto 2)   => s_csr_data <= s_ader(6)(15 downto  8);
          when c_addr_f6_ader_2(18 downto 2)   => s_csr_data <= s_ader(6)(23 downto 16);
          when c_addr_f6_ader_3(18 downto 2)   => s_csr_data <= s_ader(6)(31 downto 24);
          when c_addr_f5_ader_0(18 downto 2)   => s_csr_data <= s_ader(5)( 7 downto  0);
          when c_addr_f5_ader_1(18 downto 2)   => s_csr_data <= s_ader(5)(15 downto  8);
          when c_addr_f5_ader_2(18 downto 2)   => s_csr_data <= s_ader(5)(23 downto 16);
          when c_addr_f5_ader_3(18 downto 2)   => s_csr_data <= s_ader(5)(31 downto 24);
          when c_addr_f4_ader_0(18 downto 2)   => s_csr_data <= s_ader(4)( 7 downto  0);
          when c_addr_f4_ader_1(18 downto 2)   => s_csr_data <= s_ader(4)(15 downto  8);
          when c_addr_f4_ader_2(18 downto 2)   => s_csr_data <= s_ader(4)(23 downto 16);
          when c_addr_f4_ader_3(18 downto 2)   => s_csr_data <= s_ader(4)(31 downto 24);
          when c_addr_f3_ader_0(18 downto 2)   => s_csr_data <= s_ader(3)( 7 downto  0);
          when c_addr_f3_ader_1(18 downto 2)   => s_csr_data <= s_ader(3)(15 downto  8);
          when c_addr_f3_ader_2(18 downto 2)   => s_csr_data <= s_ader(3)(23 downto 16);
          when c_addr_f3_ader_3(18 downto 2)   => s_csr_data <= s_ader(3)(31 downto 24);
          when c_addr_f2_ader_0(18 downto 2)   => s_csr_data <= s_ader(2)( 7 downto  0);
          when c_addr_f2_ader_1(18 downto 2)   => s_csr_data <= s_ader(2)(15 downto  8);
          when c_addr_f2_ader_2(18 downto 2)   => s_csr_data <= s_ader(2)(23 downto 16);
          when c_addr_f2_ader_3(18 downto 2)   => s_csr_data <= s_ader(2)(31 downto 24);
          when c_addr_f1_ader_0(18 downto 2)   => s_csr_data <= s_ader(1)( 7 downto  0);
          when c_addr_f1_ader_1(18 downto 2)   => s_csr_data <= s_ader(1)(15 downto  8);
          when c_addr_f1_ader_2(18 downto 2)   => s_csr_data <= s_ader(1)(23 downto 16);
          when c_addr_f1_ader_3(18 downto 2)   => s_csr_data <= s_ader(1)(31 downto 24);
          when c_addr_f0_ader_0(18 downto 2)   => s_csr_data <= s_ader(0)( 7 downto  0);
          when c_addr_f0_ader_1(18 downto 2)   => s_csr_data <= s_ader(0)(15 downto  8);
          when c_addr_f0_ader_2(18 downto 2)   => s_csr_data <= s_ader(0)(23 downto 16);
          when c_addr_f0_ader_3(18 downto 2)   => s_csr_data <= s_ader(0)(31 downto 24);
          when others                          => s_csr_data <= x"ff";
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
