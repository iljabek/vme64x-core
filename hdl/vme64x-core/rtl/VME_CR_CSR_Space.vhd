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
    g_ADEM            : t_adem_array(-1 to 7);
    g_AMCAP           : t_amcap_array(0 to 7);
    g_XAMCAP          : t_xamcap_array(0 to 7);
    g_DAWPR           : t_dawpr_array(0 to 7)
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

    ader_o              : out t_ader_array(0 to 7);
    faf_ader_i          : in  t_ader_array(0 to 7);
    dfs_adem_i          : in  t_adem_array(0 to 7)
  );
end VME_CR_CSR_Space;

architecture rtl of VME_CR_CSR_Space is

  signal s_addr             : unsigned(18 downto 2);
  signal s_ga_parity        : std_logic;
  signal s_reg_bar          : std_logic_vector(7 downto 0);
  signal s_reg_bit_reg      : std_logic_vector(7 downto 0);
  signal s_reg_cram_owner   : std_logic_vector(7 downto 0);
  signal s_reg_usr_bit_reg  : std_logic_vector(7 downto 0);
  signal s_reg_ader         : t_ader_array(0 to 7);
  signal s_ader             : t_ader_array(0 to 7);

  -- CR/CSR
  signal s_cr_access        : std_logic;
  signal s_csr_access       : std_logic;
  signal s_cram_access      : std_logic;
  signal s_user_cr_access   : std_logic;
  signal s_user_csr_access  : std_logic;

  signal s_cr_data          : std_logic_vector(7 downto 0);
  signal s_csr_data         : std_logic_vector(7 downto 0);

  -- Function to calculate the size of a CR/CSR area
  function f_size (s, e : std_logic_vector) return integer is
  begin
    return ((to_integer(unsigned(e)) - to_integer(unsigned(s))) / 4) + 1;
  end;

  -- User CR/CSR and CRAM enabled when size of area greater than 1
  constant c_CRAM_SIZE      : integer := f_size(g_BEG_CRAM, g_END_CRAM);
  constant c_CRAM_ENA       : boolean := c_CRAM_SIZE > 1;

  constant c_USER_CR_SIZE   : integer := f_size(g_BEG_USER_CR, g_END_USER_CR);
  constant c_USER_CR_ENA    : boolean := c_USER_CR_SIZE > 1;

  constant c_USER_CSR_SIZE  : integer := f_size(g_BEG_USER_CSR, g_END_USER_CSR);
  constant c_USER_CSR_ENA   : boolean := c_USER_CSR_SIZE > 1;

  -- CRAM
  type t_cram is array (c_CRAM_SIZE-1 downto 0) of std_logic_vector(7 downto 0);

  signal s_cram             : t_cram;
  signal s_cram_data        : std_logic_vector(7 downto 0);
  signal s_cram_waddr       : unsigned(18 downto 2);
  signal s_cram_raddr       : unsigned(18 downto 2);
  signal s_cram_we          : std_logic;

  -- Addresses
  constant c_BEG_CR         : unsigned(18 downto 2) := to_unsigned(16#00000#/4, 17);
  constant c_END_CR         : unsigned(18 downto 2) := to_unsigned(16#00fff#/4, 17);
  constant c_BEG_CSR        : unsigned(18 downto 2) := to_unsigned(16#7ff60#/4, 17);
  constant c_END_CSR        : unsigned(18 downto 2) := to_unsigned(16#7ffff#/4, 17);
  constant c_BEG_USER_CR    : unsigned(18 downto 2) := unsigned(g_BEG_USER_CR(18 downto 2));
  constant c_END_USER_CR    : unsigned(18 downto 2) := unsigned(g_END_USER_CR(18 downto 2));
  constant c_BEG_USER_CSR   : unsigned(18 downto 2) := unsigned(g_BEG_USER_CSR(18 downto 2));
  constant c_END_USER_CSR   : unsigned(18 downto 2) := unsigned(g_END_USER_CSR(18 downto 2));
  constant c_BEG_CRAM       : unsigned(18 downto 2) := unsigned(g_BEG_CRAM(18 downto 2));
  constant c_END_CRAM       : unsigned(18 downto 2) := unsigned(g_END_CRAM(18 downto 2));

  constant c_BAR_REG        : integer := 16#7ffff#/4;
  constant c_BIT_SET_REG    : integer := 16#7fffb#/4;
  constant c_BIT_CLR_REG    : integer := 16#7fff7#/4;
  constant c_CRAM_OWNER_REG : integer := 16#7fff3#/4;
  constant c_USR_SET_REG    : integer := 16#7ffef#/4;
  constant c_USR_CLR_REG    : integer := 16#7ffeb#/4;
  constant c_ADER_REG_END   : integer := 16#7ffdf#/4;
  constant c_ADER_REG_BEG   : integer := 16#7ff63#/4;

  -- Indexes in bit set/clr register
  constant c_RESET_BIT      : integer := 7;
  constant c_SYSFAIL_EN_BIT : integer := 6;
  constant c_FAILED_BIT     : integer := 5;
  constant c_ENABLE_BIT     : integer := 4;
  constant c_BERR_BIT       : integer := 3;
  constant c_CRAM_OWNER_BIT : integer := 2;

  -- Value for unused memory locations
  constant c_UNUSED         : std_logic_vector(7 downto 0) := x"ff";

  ------------------------------------------------------------------------------
  -- Generate configuration ROM
  ------------------------------------------------------------------------------
  type t_cr_array is array (natural range <>) of std_logic_vector(7 downto 0);

  -- Function to generate a CR sub-array from a std_logic_vector
  function f_cr_vec (v : std_logic_vector) return t_cr_array is
    variable a : t_cr_array(0 to v'length/8-1);
  begin
    for i in 0 to a'length-1 loop
      a(i) := v(v'length-(i*8)-1 downto v'length-(i*8)-8);
    end loop;
    return a;
  end function;

  -- Function to encode the configuration ROM
  function f_cr_encode return t_cr_array is
    variable cr  : t_cr_array(0 to 1023) := (others => x"00");
    variable crc : unsigned(7 downto 0)  := x"00";
  begin
    cr(16#001# to 16#003#)  := (x"00", x"03", x"ff");             -- Length of CR (excluding checksum)
    cr(16#004#)             := x"81";                             -- CR data access width
    cr(16#005#)             := x"81";                             -- CSR data access width
    cr(16#006#)             := x"02";                             -- CR/CSR space specification ID
    cr(16#007#)             := x"43";                             -- ASCII "C"
    cr(16#008#)             := x"52";                             -- ASCII "R"
    cr(16#009# to 16#00b#)  := f_cr_vec(g_MANUFACTURER_ID);       -- Manufacturer ID
    cr(16#00c# to 16#00f#)  := f_cr_vec(g_BOARD_ID);              -- Board ID
    cr(16#010# to 16#013#)  := f_cr_vec(g_REVISION_ID);           -- Revision ID
    cr(16#014# to 16#016#)  := f_cr_vec(g_ASCII_PTR);             -- Pointer to an ASCII string
    cr(16#01f#)             := g_PROGRAM_ID;                      -- Program ID
    cr(16#020# to 16#022#)  := f_cr_vec(g_BEG_USER_CR);           -- Beg user CR
    cr(16#023# to 16#025#)  := f_cr_vec(g_END_USER_CR);           -- End user CR
    cr(16#026# to 16#028#)  := f_cr_vec(g_BEG_CRAM);              -- Beg CRAM
    cr(16#029# to 16#02b#)  := f_cr_vec(g_END_CRAM);              -- End CRAM
    cr(16#02c# to 16#02e#)  := f_cr_vec(g_BEG_USER_CSR);          -- Beg user CSR
    cr(16#02f# to 16#031#)  := f_cr_vec(g_END_USER_CSR);          -- End user CSR
    cr(16#032# to 16#034#)  := f_cr_vec(g_BEG_SN);                -- Beg serial number
    cr(16#035# to 16#037#)  := f_cr_vec(g_END_SN);                -- End serial number
    cr(16#038#)             := x"04";                             -- Slave characteristics parameter
    cr(16#039#)             := x"00";                             -- User-defined slave char. param.
    cr(16#03d#)             := x"0e";                             -- Interrupter capabilities
    cr(16#03f#)             := x"81";                             -- CRAM data access width
    for i in 0 to 7 loop
      cr(16#040#+i)                    := g_DAWPR(i);             -- Function X DAWPR
      cr(16#048#+i*8  to 16#04f#+i*8)  := f_cr_vec(g_AMCAP(i));   -- Function X AMCAP
      cr(16#088#+i*32 to 16#0a7#+i*32) := f_cr_vec(g_XAMCAP(i));  -- Function X XAMCAP
      cr(16#188#+i*4  to 16#18b#+i*4)  := f_cr_vec(g_ADEM(i));    -- Function X ADEM
    end loop;
    for i in 1 to cr'length-1 loop
      crc := crc + unsigned(cr(i));
    end loop;
    cr(16#000#)            := std_logic_vector(crc);              -- Checksum
    return cr;
  end;

  signal s_cr_rom : t_cr_array(0 to 1023) := f_cr_encode;

  ------------------------------------------------------------------------------

begin

  s_addr <= unsigned(addr_i);

  ------------------------------------------------------------------------------
  -- Defined CR
  ------------------------------------------------------------------------------
  s_cr_access  <= '1' when s_addr >= c_BEG_CR and
                           s_addr <= c_END_CR
                      else '0';

  process (clk_i)
  begin
    if rising_edge(clk_i) then
      s_cr_data <= s_cr_rom(to_integer(s_addr) mod 1024);
    end if;
  end process;

  ------------------------------------------------------------------------------
  -- Defined CSR
  ------------------------------------------------------------------------------
  s_csr_access <= '1' when s_addr >= c_BEG_CSR and
                           s_addr <= c_END_CSR
                      else '0';

  -- If the crate is not driving the GA lines or the parity is even the BAR
  -- register is set to 0x00 and the board will not answer CR/CSR accesses.
  s_ga_parity  <= vme_ga_i(5) xor vme_ga_i(4) xor vme_ga_i(3) xor
                  vme_ga_i(2) xor vme_ga_i(1) xor vme_ga_i(0);

  -- Write
  process (clk_i)
    variable v_addr  : unsigned(6 downto 2);
    variable v_index : integer;
    variable v_byte  : integer;
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
          case to_integer(s_addr) is
            when c_BAR_REG =>
              s_reg_bar <= data_i;

            when c_BIT_SET_REG =>
              s_reg_bit_reg <= s_reg_bit_reg or data_i;

            when c_BIT_CLR_REG =>
              s_reg_bit_reg <= s_reg_bit_reg and not data_i;
              if data_i(c_CRAM_OWNER_BIT) = '1' then
                s_reg_cram_owner <= x"00";
              end if;

            when c_CRAM_OWNER_REG =>
              if s_reg_cram_owner = x"00" then
                s_reg_cram_owner <= data_i;
                s_reg_bit_reg(c_CRAM_OWNER_BIT) <= '1';
              end if;

            when c_USR_SET_REG =>
              s_reg_usr_bit_reg <= s_reg_usr_bit_reg or data_i;

            when c_USR_CLR_REG =>
              s_reg_usr_bit_reg <= s_reg_usr_bit_reg and not data_i;

            when c_ADER_REG_BEG to c_ADER_REG_END =>
              v_addr  := s_addr(6 downto 2) - to_unsigned(c_ADER_REG_BEG, 5);
              v_index := to_integer(v_addr(6 downto 4));
              v_byte  := 3-to_integer(v_addr(3 downto 2));
              s_reg_ader(v_index)(8*v_byte+7 downto 8*v_byte) <= data_i;

            when others =>
              null;
          end case;
        end if;

        if vme_berr_n_i = '0' then
          s_reg_bit_reg(c_BERR_BIT) <= '1';
        end if;

        if vme_sysfail_i = '1' then
          s_reg_bit_reg(c_FAILED_BIT) <= '1';
        end if;
      end if;
    end if;
  end process;

  bar_o             <= s_reg_bar(7 downto 3);
  module_enable_o   <= s_reg_bit_reg(c_ENABLE_BIT);
  vme_sysfail_ena_o <= s_reg_bit_reg(c_SYSFAIL_EN_BIT);
  module_reset_o    <= s_reg_bit_reg(c_RESET_BIT);

  -- Handle DFS and FAF
  process (s_reg_ader, faf_ader_i, dfs_adem_i)
    variable v_ader_b0 : std_logic_vector(7 downto 0);
  begin
    for i in 0 to 7 loop
      -- When FAF function or upper bits of previous FAF function, readback
      -- and output ADER given at the FAF inputs.
      if (g_ADEM(i-1)(c_ADEM_EFM) = '1' and g_ADEM(i-1)(c_ADEM_FAF) = '1') or
         (g_ADEM(i-1)(c_ADEM_EFM) = '0' and g_ADEM( i )(c_ADEM_FAF) = '1')
      then
        s_ader(i) <= faf_ader_i(i);
        ader_o(i) <= faf_ader_i(i);

      -- When upper bits of previous DFS function and DFSR enabled, readback
      -- the ADEM value and output zero.
      elsif g_ADEM(i-1)(c_ADEM_EFM) = '1' and g_ADEM(i-1)(c_ADEM_DFS) = '1' and
            s_reg_ader(i-1)(c_ADER_DFSR) = '1'
      then
        s_ader(i) <= dfs_adem_i(i);
        ader_o(i) <= (others => '0');

      -- When a DFS function and DFSR enabled, readback the ADEM and output
      -- zero.
      elsif g_ADEM(i-1)(c_ADEM_EFM) = '0' and g_ADEM(i)(c_ADEM_DFS) = '1' and
            s_reg_ader(i)(c_ADER_DFSR) = '1'
      then
        v_ader_b0   := (c_ADER_DFSR => s_reg_ader(i)(c_ADER_DFSR),
                        others => '0');
        s_ader(i) <= dfs_adem_i(i)(c_ADEM_M) & v_ader_b0;
        ader_o(i) <= (others => '0');

      -- In all other cases, readback and output the ADER register value.
      else
        s_ader(i) <= s_reg_ader(i);
        ader_o(i) <= s_reg_ader(i);
      end if;
    end loop;
  end process;

  -- Read
  process (clk_i)
    variable v_addr  : unsigned(6 downto 2);
    variable v_index : integer;
    variable v_byte  : integer;
  begin
    if rising_edge(clk_i) then
      if rst_n_i = '0' then
        s_csr_data <= c_UNUSED;
      else
        case to_integer(s_addr) is
          when c_BAR_REG =>
            s_csr_data <= s_reg_bar;

          when c_BIT_SET_REG =>
            s_csr_data <= s_reg_bit_reg;

          when c_BIT_CLR_REG =>
            s_csr_data <= s_reg_bit_reg;

          when c_CRAM_OWNER_REG =>
            s_csr_data <= s_reg_cram_owner;

          when c_USR_SET_REG =>
            s_csr_data <= s_reg_usr_bit_reg;

          when c_USR_CLR_REG =>
            s_csr_data <= s_reg_usr_bit_reg;

          when c_ADER_REG_BEG to c_ADER_REG_END =>
            v_addr  := s_addr(6 downto 2) - to_unsigned(c_ADER_REG_BEG, 5);
            v_index := to_integer(v_addr(6 downto 4));
            v_byte  := 3-to_integer(v_addr(3 downto 2));
            s_csr_data <= s_ader(v_index)(8*v_byte+7 downto 8*v_byte);

          when others =>
            s_csr_data <= c_UNUSED;
        end case;
      end if;
    end if;
  end process;

  ------------------------------------------------------------------------------
  -- CRAM
  ------------------------------------------------------------------------------
  gen_cram_ena: if c_CRAM_ENA = true generate
    s_cram_access <= '1' when s_addr >= c_BEG_CRAM and
                              s_addr <= c_END_CRAM
                         else '0';

    s_cram_we     <= we_i and s_cram_access;
    s_cram_waddr  <= s_addr - c_BEG_CRAM;
    s_cram_data   <= s_cram(to_integer(s_cram_raddr) mod c_CRAM_SIZE);

    process (clk_i) begin
      if rising_edge(clk_i) then
        if s_cram_we = '1' then
          s_cram(to_integer(s_cram_waddr)) <= data_i;
        end if;
        s_cram_raddr <= s_cram_waddr;
      end if;
    end process;
  end generate;
  gen_cram_dis: if c_CRAM_ENA = false  generate
    s_cram_access <= '0';
    s_cram_raddr  <= (others => '0');
    s_cram_waddr  <= (others => '0');
    s_cram_data   <= x"00";
  end generate;

  ------------------------------------------------------------------------------
  -- User CR
  ------------------------------------------------------------------------------
  gen_user_cr_ena: if c_USER_CR_ENA = true generate
    s_user_cr_access <= '1' when s_addr >= c_BEG_USER_CR and
                                 s_addr <= c_END_USER_CR
                            else '0';

    user_cr_addr_o   <= std_logic_vector(s_addr - c_BEG_USER_CR);
  end generate;
  gen_user_cr_dis: if c_USER_CR_ENA = false generate
    s_user_cr_access <= '0';
    user_cr_addr_o   <= (others => '0');
  end generate;

  ------------------------------------------------------------------------------
  -- User CSR
  ------------------------------------------------------------------------------
  gen_user_csr_ena: if c_USER_CSR_ENA = true generate
    s_user_csr_access <= '1' when s_addr >= c_BEG_USER_CSR and
                                  s_addr <= c_END_USER_CSR
                             else '0';

    user_csr_addr_o   <= std_logic_vector(s_addr - c_BEG_USER_CSR);
  end generate;
  gen_user_csr_dis: if c_USER_CSR_ENA = false generate
    s_user_csr_access <= '0';
    user_csr_addr_o   <= (others => '0');
  end generate;

  user_csr_data_o <= data_i;
  user_csr_we_o   <= we_i and s_user_csr_access;

  ------------------------------------------------------------------------------
  -- Read multiplexer
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
      data_o <= c_UNUSED;
    end if;
  end process;

end rtl;
