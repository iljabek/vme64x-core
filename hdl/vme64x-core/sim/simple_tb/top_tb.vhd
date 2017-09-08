entity top_tb is
end;

library ieee;
use std.textio.all;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_textio.all;
use work.vme64x_pack.all;

architecture behaviour of top_tb is
  --  Clock
  constant g_CLOCK_PERIOD : natural := 10;  -- in ns

  --  WB widths
  constant g_WB_DATA_WIDTH   : integer   := 32;
  constant g_WB_ADDR_WIDTH   : integer   := 32;

  --  VME core
  signal clk_i           : std_logic;
  signal rst_n_i         : std_logic;
  signal rst_n_o         : std_logic;
  signal VME_AS_n_i      : std_logic;
  signal VME_RST_n_i     : std_logic;
  signal VME_WRITE_n_i   : std_logic;
  signal VME_AM_i        : std_logic_vector(5 downto 0);
  signal VME_DS_n_i      : std_logic_vector(1 downto 0);
  signal VME_GA_i        : std_logic_vector(5 downto 0);
  signal VME_BERR_o      : std_logic;
  signal VME_DTACK_n_o   : std_logic;
  signal VME_RETRY_n_o   : std_logic;
  signal VME_LWORD_n_i   : std_logic;
  signal VME_LWORD_n_o   : std_logic;
  signal VME_ADDR_i      : std_logic_vector(31 downto 1);
  signal VME_ADDR_o      : std_logic_vector(31 downto 1);
  signal VME_DATA_i      : std_logic_vector(31 downto 0);
  signal VME_DATA_o      : std_logic_vector(31 downto 0);
  signal VME_IRQ_o       : std_logic_vector(6 downto 0);
  signal VME_IACKIN_n_i  : std_logic;
  signal VME_IACK_n_i    : std_logic;
  signal VME_IACKOUT_n_o : std_logic;
  signal VME_DTACK_OE_o  : std_logic;
  signal VME_DATA_DIR_o  : std_logic;
  signal VME_DATA_OE_N_o : std_logic;
  signal VME_ADDR_DIR_o  : std_logic;
  signal VME_ADDR_OE_N_o : std_logic;
  signal VME_RETRY_OE_o  : std_logic;
  signal DAT_i           : std_logic_vector(g_WB_DATA_WIDTH-1 downto 0);
  signal DAT_o           : std_logic_vector(g_WB_DATA_WIDTH-1 downto 0);
  signal ADR_o           : std_logic_vector(g_WB_ADDR_WIDTH-1 downto 0);
  signal CYC_o           : std_logic;
  signal ERR_i           : std_logic;
  signal RTY_i           : std_logic;
  signal SEL_o           : std_logic_vector(g_WB_DATA_WIDTH/8-1 downto 0);
  signal STB_o           : std_logic;
  signal ACK_i           : std_logic;
  signal WE_o            : std_logic;
  signal STALL_i         : std_logic;
  signal endian_i        : std_logic_vector(2 downto 0)  := (others => '0');
  signal irq_level_i     : std_logic_vector(7 downto 0)  := (others => '0');
  signal irq_vector_i    : std_logic_vector(7 downto 0)  := (others => '0');
  signal user_csr_addr_o : std_logic_vector(18 downto 2);
  signal user_csr_data_i : std_logic_vector(7 downto 0)  := (others => '0');
  signal user_csr_data_o : std_logic_vector(7 downto 0);
  signal user_csr_we_o   : std_logic;
  signal user_cr_addr_o  : std_logic_vector(18 downto 2);
  signal user_cr_data_i  : std_logic_vector( 7 downto 0)  := (others => '0');
  signal function_o      : std_logic_vector( 2 downto 0);
  signal f0_faf_ader_i   : std_logic_vector(31 downto 0) := (others => '0');
  signal f1_faf_ader_i   : std_logic_vector(31 downto 0) := (others => '0');
  signal f2_faf_ader_i   : std_logic_vector(31 downto 0) := (others => '0');
  signal f3_faf_ader_i   : std_logic_vector(31 downto 0) := (others => '0');
  signal f4_faf_ader_i   : std_logic_vector(31 downto 0) := (others => '0');
  signal f5_faf_ader_i   : std_logic_vector(31 downto 0) := (others => '0');
  signal f6_faf_ader_i   : std_logic_vector(31 downto 0) := (others => '0');
  signal f7_faf_ader_i   : std_logic_vector(31 downto 0) := (others => '0');
  signal f0_dfs_adem_i   : std_logic_vector(31 downto 0) := (others => '0');
  signal f1_dfs_adem_i   : std_logic_vector(31 downto 0) := (others => '0');
  signal f2_dfs_adem_i   : std_logic_vector(31 downto 0) := (others => '0');
  signal f3_dfs_adem_i   : std_logic_vector(31 downto 0) := (others => '0');
  signal f4_dfs_adem_i   : std_logic_vector(31 downto 0) := (others => '0');
  signal f5_dfs_adem_i   : std_logic_vector(31 downto 0) := (others => '0');
  signal f6_dfs_adem_i   : std_logic_vector(31 downto 0) := (others => '0');
  signal f7_dfs_adem_i   : std_logic_vector(31 downto 0) := (others => '0');
  signal irq_ack_o       : std_logic;
  signal irq_i           : std_logic;

  constant slave_ga : std_logic_vector (4 downto 0) := b"0_1101";

begin
  set_ga: block
  begin
    VME_GA_i (4 downto 0) <= not slave_ga;
    VME_GA_i (5) <= (slave_ga (4) xor slave_ga (3) xor slave_ga (2)
                     xor slave_ga (1) xor slave_ga (0));
  end block;

  dut: entity work.VME64xCore_Top
    generic map (g_CLOCK_PERIOD => g_CLOCK_PERIOD,
                 g_WB_DATA_WIDTH => g_WB_DATA_WIDTH,
                 g_WB_ADDR_WIDTH => g_WB_ADDR_WIDTH)
    port map (
        clk_i           => clk_i,
        rst_n_i         => rst_n_i,
        rst_n_o         => rst_n_o,
        VME_AS_n_i      => VME_AS_n_i,
        VME_RST_n_i     => VME_RST_n_i,
        VME_WRITE_n_i   => VME_WRITE_n_i,
        VME_AM_i        => VME_AM_i,
        VME_DS_n_i      => VME_DS_n_i,
        VME_GA_i        => VME_GA_i,
        VME_BERR_o      => VME_BERR_o,
        VME_DTACK_n_o   => VME_DTACK_n_o,
        VME_RETRY_n_o   => VME_RETRY_n_o,
        VME_LWORD_n_i   => VME_LWORD_n_i,
        VME_LWORD_n_o   => VME_LWORD_n_o,
        VME_ADDR_i      => VME_ADDR_i,
        VME_ADDR_o      => VME_ADDR_o,
        VME_DATA_i      => VME_DATA_i,
        VME_DATA_o      => VME_DATA_o,
        VME_IRQ_o       => VME_IRQ_o,
        VME_IACKIN_n_i  => VME_IACKIN_n_i,
        VME_IACK_n_i    => VME_IACK_n_i,
        VME_IACKOUT_n_o => VME_IACKOUT_n_o,
        VME_DTACK_OE_o  => VME_DTACK_OE_o,
        VME_DATA_DIR_o  => VME_DATA_DIR_o,
        VME_DATA_OE_N_o => VME_DATA_OE_N_o,
        VME_ADDR_DIR_o  => VME_ADDR_DIR_o,
        VME_ADDR_OE_N_o => VME_ADDR_OE_N_o,
        VME_RETRY_OE_o  => VME_RETRY_OE_o,
        DAT_i           => DAT_i,
        DAT_o           => DAT_o,
        ADR_o           => ADR_o,
        CYC_o           => CYC_o,
        ERR_i           => ERR_i,
        RTY_i           => RTY_i,
        SEL_o           => SEL_o,
        STB_o           => STB_o,
        ACK_i           => ACK_i,
        WE_o            => WE_o,
        STALL_i         => STALL_i,
        endian_i        => endian_i,
        irq_level_i     => irq_level_i,
        irq_vector_i    => irq_vector_i,
        user_csr_addr_o => user_csr_addr_o,
        user_csr_data_i => user_csr_data_i,
        user_csr_data_o => user_csr_data_o,
        user_csr_we_o   => user_csr_we_o,
        user_cr_addr_o  => user_cr_addr_o,
        user_cr_data_i  => user_cr_data_i,
        function_o      => function_o,
        f0_faf_ader_i   => f0_faf_ader_i,
        f1_faf_ader_i   => f1_faf_ader_i,
        f2_faf_ader_i   => f2_faf_ader_i,
        f3_faf_ader_i   => f3_faf_ader_i,
        f4_faf_ader_i   => f4_faf_ader_i,
        f5_faf_ader_i   => f5_faf_ader_i,
        f6_faf_ader_i   => f6_faf_ader_i,
        f7_faf_ader_i   => f7_faf_ader_i,
        f0_dfs_adem_i   => f0_dfs_adem_i,
        f1_dfs_adem_i   => f1_dfs_adem_i,
        f2_dfs_adem_i   => f2_dfs_adem_i,
        f3_dfs_adem_i   => f3_dfs_adem_i,
        f4_dfs_adem_i   => f4_dfs_adem_i,
        f5_dfs_adem_i   => f5_dfs_adem_i,
        f6_dfs_adem_i   => f6_dfs_adem_i,
        f7_dfs_adem_i   => f7_dfs_adem_i,
        irq_ack_o       => irq_ack_o,
        irq_i           => irq_i);

  clk_gen: process
  begin
    clk_i <= '0';
    wait for (g_CLOCK_PERIOD / 2) * 1 ns;
    clk_i <= '1';
    wait for (g_CLOCK_PERIOD / 2) * 1 ns;
  end process;

  tb: process
    subtype cfg_addr_t is std_logic_vector (19 downto 0);
    subtype byte_t is std_logic_vector (7 downto 0);

    constant c_log : boolean := False;

    -- Convert a CR/CSR address to the VME address.  Insert GA, strip A0.
    -- The ADDR is on 20 bits (so the x"" notation can be used), but as
    -- ADDR(19) is stipped, it must be '0'.
    function to_vme_cfg_addr (addr : cfg_addr_t)
      return std_logic_vector is
    begin
      assert addr (19) = '0' report "a19 is discarded" severity error;
      return x"00" & slave_ga & addr (18 downto 1);
    end to_vme_cfg_addr;

    procedure read8_conf (addr : cfg_addr_t;
                          variable data : out byte_t)
    is
      variable l : line;
    begin
      if c_log then
        write (l, string'("read8_conf at 0x"));
        hwrite (l, addr);
        writeline (output, l);
      end if;

      VME_ADDR_i <= to_vme_cfg_addr (addr);
      VME_AM_i <= c_AM_CR_CSR;
      VME_LWORD_n_i <= '1';
      VME_IACK_n_i <= '1';
      wait for 35 ns;
      VME_AS_n_i <= '0';
      VME_WRITE_n_i <= '1';
      if not (VME_DTACK_OE_o = '0' and VME_BERR_o = '0') then
        wait until VME_DTACK_OE_o = '0' and VME_BERR_o = '0';
      end if;

      VME_DS_n_i <= "10";
      wait until VME_DTACK_OE_o = '1' and VME_DTACK_n_o = '0';
      assert VME_DATA_DIR_o = '1';
      assert VME_DATA_OE_N_o = '0';
      data := VME_DATA_o (7 downto 0);

      --  Release
      VME_AS_n_i <= '1';
      VME_DS_n_i <= "11";

      if c_log then
        write (l, string'(" => 0x"));
        hwrite(l, VME_DATA_o (7 downto 0));
        writeline (output, l);
      end if;
    end read8_conf;

    procedure read8_conf_mb (addr : cfg_addr_t;
                             variable data : out std_logic_vector)
    is
      variable ad : cfg_addr_t := addr;
      constant bsize : natural := data'length / 8;
      subtype d_type is std_logic_vector (bsize * 8 - 1 downto 0);
      alias d : d_type is data;
    begin
      assert data'length mod 8 = 0 report "data is not a multiple of bytes"
        severity error;
      for i in bsize - 1 downto 0 loop
        read8_conf (ad, d (i * 8 + 7 downto i * 8));
        ad := cfg_addr_t (unsigned(ad) + 4);
      end loop;
    end read8_conf_mb;

    function hex1 (v : std_logic_vector (3 downto 0)) return character is
    begin
      case v is
        when x"0" => return '0';
        when x"1" => return '1';
        when x"2" => return '2';
        when x"3" => return '3';
        when x"4" => return '4';
        when x"5" => return '5';
        when x"6" => return '6';
        when x"7" => return '7';
        when x"8" => return '8';
        when x"9" => return '9';
        when x"a" => return 'a';
        when x"b" => return 'b';
        when x"c" => return 'c';
        when x"d" => return 'd';
        when x"e" => return 'e';
        when x"f" => return 'f';
        when "ZZZZ" => return 'z';
        when others => return '?';
      end case;
    end hex1;

    function hex2 (v : byte_t) return string is
    begin
      return hex1 (v(7 downto 4)) & hex1 (v(3 downto 0));
    end hex2;

    function hex6 (v : std_logic_vector (23 downto 0)) return string is
      variable res : string (6 downto 1);
    begin
      for i in res'range loop
        res (i) := hex1 (v (i * 4 - 1 downto i * 4 - 4));
      end loop;
      return res;
    end hex6;

    function hex8 (v : std_logic_vector (31 downto 0)) return string is
      variable res : string (8 downto 1);
    begin
      for i in res'range loop
        res (i) := hex1 (v (i * 4 - 1 downto i * 4 - 4));
      end loop;
      return res;
    end hex8;

    function hex (v : std_logic_vector) return string
    is
      constant ndigits : natural := v'length / 4;
      subtype av_t is std_logic_vector (ndigits * 4 - 1 downto 0);
      alias av : av_t is v;
      variable res : string (ndigits downto 1);
    begin
      for i in res'range loop
        res (i) := hex1 (av (i * 4 - 1 downto i * 4 - 4));
      end loop;
      return res;
    end hex;

    procedure Dump_CR
    is
      variable d : byte_t;
      variable b3 : std_logic_vector (23 downto 0);
      variable w : std_logic_vector (31 downto 0);
      variable b8 : std_logic_vector (63 downto 0);
      variable b32 : std_logic_vector (255 downto 0);
    begin
      read8_conf (x"0_0003", d);
      write (output, "CR checksum:     0x" & hex2 (d) & LF);

      read8_conf (x"0_0013", d);
      assert d = x"81" report "invalid data width at 0x13" severity error;
      write (output, "CR data width:   0x" & hex2 (d) & LF);

      read8_conf (x"0_0017", d);
      assert d = x"81" report "invalid data width at 0x17" severity error;
      write (output, "CSR data width:  0x" & hex2 (d) & LF);

      read8_conf (x"0_001b", d);
      assert d = x"02" report "invalid spec ID at 0x1b" severity error;
      write (output, "CR/CSR version:  0x" & hex2 (d) & LF);

      read8_conf (x"0_001f", d);
      assert d = x"43" report "invalid valid CR byte at 0x1f" severity error;
      write (output, "CR valid 'C':    0x" & hex2 (d) & LF);

      read8_conf (x"0_0023", d);
      assert d = x"52" report "invalid valid CR byte at 0x23" severity error;
      write (output, "CR valid 'R':    0x" & hex2 (d) & LF);

      read8_conf_mb (x"0_0027", b3);
      write (output, "CR Manu ID:      0x" & hex6 (b3) & LF);

      read8_conf_mb (x"0_0033", w);
      write (output, "CR Board ID:     0x" & hex8 (w) & LF);

      read8_conf_mb (x"0_0043", w);
      write (output, "CR Rev ID:       0x" & hex8 (w) & LF);

      read8_conf_mb (x"0_0053", b3);
      write (output, "CR ASCII ptr:    0x" & hex6 (b3) & LF);

      read8_conf (x"0_007F", d);
      write (output, "CR Program ID:   0x" & hex2 (d) & LF);

      --  VVME64x
      read8_conf_mb (x"0_0083", b3);
      write (output, "CR BEG_USER_CR:  0x" & hex6 (b3) & LF);

      read8_conf_mb (x"0_008F", b3);
      write (output, "CR END_USER_CR:  0x" & hex6 (b3) & LF);

      read8_conf_mb (x"0_009B", b3);
      write (output, "CR BEG_CRAM:     0x" & hex6 (b3) & LF);

      read8_conf_mb (x"0_00A7", b3);
      write (output, "CR END_CRAM:     0x" & hex6 (b3) & LF);

      read8_conf_mb (x"0_00B3", b3);
      write (output, "CR BEG_USER_CSR: 0x" & hex6 (b3) & LF);

      read8_conf_mb (x"0_00B3", b3);
      write (output, "CR END_USER_CSR: 0x" & hex6 (b3) & LF);

      read8_conf_mb (x"0_00CB", b3);
      write (output, "CR BEG_SN:       0x" & hex6 (b3) & LF);

      read8_conf_mb (x"0_00D7", b3);
      write (output, "CR END_SN:       0x" & hex6 (b3) & LF);

      read8_conf (x"0_00E3", d);
      write (output, "CR Slave charac: 0x" & hex2 (d) & LF);

      read8_conf (x"0_00E7", d);
      write (output, "CR user-def:     0x" & hex2 (d) & LF);

      read8_conf (x"0_00EB", d);
      write (output, "CR master chara: 0x" & hex2 (d) & LF);

      read8_conf (x"0_00EF", d);
      write (output, "CR user-def:     0x" & hex2 (d) & LF);

      read8_conf (x"0_00F3", d);
      write (output, "CR INT hand cap: 0x" & hex2 (d) & LF);

      read8_conf (x"0_00F7", d);
      write (output, "CR INT cap:      0x" & hex2 (d) & LF);

      read8_conf (x"0_00FF", d);
      write (output, "CR CRAM acc wd:  0x" & hex2 (d) & LF);

      read8_conf (x"0_0103", d);
      write (output, "CR FN0 DAW:      0x" & hex2 (d) & LF);
      read8_conf (x"0_0107", d);
      write (output, "CR FN1 DAW:      0x" & hex2 (d) & LF);
      read8_conf (x"0_010b", d);
      write (output, "CR FN2 DAW:      0x" & hex2 (d) & LF);
      read8_conf (x"0_010f", d);
      write (output, "CR FN3 DAW:      0x" & hex2 (d) & LF);
      read8_conf (x"0_0113", d);
      write (output, "CR FN4 DAW:      0x" & hex2 (d) & LF);
      read8_conf (x"0_0117", d);
      write (output, "CR FN5 DAW:      0x" & hex2 (d) & LF);
      read8_conf (x"0_011b", d);
      write (output, "CR FN6 DAW:      0x" & hex2 (d) & LF);
      read8_conf (x"0_011f", d);
      write (output, "CR FN7 DAW:      0x" & hex2 (d) & LF);

      read8_conf_mb (x"0_0123", b8);
      write (output, "CR FN0 AMCAP:    0x" & hex (b8) & LF);
      read8_conf_mb (x"0_0143", b8);
      write (output, "CR FN1 AMCAP:    0x" & hex (b8) & LF);
      read8_conf_mb (x"0_0163", b8);
      write (output, "CR FN2 AMCAP:    0x" & hex (b8) & LF);
      read8_conf_mb (x"0_0183", b8);
      write (output, "CR FN3 AMCAP:    0x" & hex (b8) & LF);
      read8_conf_mb (x"0_01a3", b8);
      write (output, "CR FN4 AMCAP:    0x" & hex (b8) & LF);
      read8_conf_mb (x"0_01c3", b8);
      write (output, "CR FN5 AMCAP:    0x" & hex (b8) & LF);
      read8_conf_mb (x"0_01e3", b8);
      write (output, "CR FN6 AMCAP:    0x" & hex (b8) & LF);
      read8_conf_mb (x"0_0203", b8);
      write (output, "CR FN7 AMCAP:    0x" & hex (b8) & LF);


      read8_conf_mb (x"0_0223", b32);
      write (output, "CR FN0 XAMCAP:   0x" & hex (b32) & LF);
      read8_conf_mb (x"0_02a3", b32);
      write (output, "CR FN1 XAMCAP:   0x" & hex (b32) & LF);
      --  ...

      read8_conf_mb (x"0_0623", w);
      write (output, "CR FN0 ADEM:     0x" & hex (w) & LF);
      read8_conf_mb (x"0_0633", w);
      write (output, "CR FN1 ADEM:     0x" & hex (w) & LF);
      -- ...

      read8_conf_mb (x"7_FF63", w);
      write (output, "CR FN0 ADER:     0x" & hex (w) & LF);
      read8_conf_mb (x"7_FF73", w);
      write (output, "CR FN1 ADER:     0x" & hex (w) & LF);

      read8_conf (x"0_06af", d);
      write (output, "CR master DAWPR: 0x" & hex2 (d) & LF);

      read8_conf_mb (x"0_06b3", b8);
      write (output, "CR master AMCAP: 0x" & hex (b8) & LF);

      read8_conf_mb (x"0_06d3", b32);
      write (output, "CR mastr XAMCAP: 0x" & hex (b32) & LF);

    end Dump_CR;

    variable l : line;
    variable d : byte_t;
  begin
    --  VME reset
    rst_n_i <= '0';
    VME_RST_n_i <= '0';
    VME_AS_n_i <= '1';
    VME_ADDR_i <= (others => '1');
    VME_AM_i <= (others => '1');
    VME_DS_n_i <= "11";
    for i in 1 to 2 loop
      wait until rising_edge (clk_i);
    end loop;
    VME_RST_n_i <= '1';
    rst_n_i <= '1';
    for i in 1 to 8 loop
      wait until rising_edge (clk_i);
    end loop;

    --  Read CSR
    read8_conf (x"7_FFFF", d);
    assert d = slave_ga & "000" report "bad CR/CSR BAR value" severity error;

    --  READ CR
    Dump_CR;

    assert false report "end of simulation" severity failure;
    wait;
  end process;
end behaviour;
