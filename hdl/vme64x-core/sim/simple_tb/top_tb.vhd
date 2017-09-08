entity top_tb is
end;

library ieee;
use std.textio.all;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_textio.all;
use work.vme64x_pack.all;

architecture behaviour of top_tb is
  subtype cfg_addr_t is std_logic_vector (19 downto 0);
  subtype byte_t is std_logic_vector (7 downto 0);
  subtype vme_am_t is std_logic_vector (5 downto 0);

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

  --  Decode DAWPR byte
  function Disp_DAWPR (v : byte_t) return string is
  begin
    case v is
      when x"81" => return "D08(O)";
      when x"82" => return "D08(EO)";
      when x"83" => return "D16 + D08";
      when x"84" => return "D32 + D16 + D08";
      when x"85" => return "MD32 + D16 + D08";
      when others => return "??";
    end case;
  end Disp_DAWPR;

  function Image_AM (am : natural range 0 to 63) return String is
  begin
    case am is
      when 16#3f# => return "A24S-BLT";
      when 16#3e# => return "A24S-PRG";
      when 16#3d# => return "A24S-DAT";
      when 16#3c# => return "A24S-MBL";
      when 16#3b# => return "A24U-BLT";
      when 16#3a# => return "A24U-PRG";
      when 16#39# => return "A24U-DAT";
      when 16#38# => return "A24U-MBL";

      when 16#37# => return "A40x-BLT";
      when 16#36# => return "Resvd-46";
      when 16#35# => return "A40x-LCK";
      when 16#34# => return "A40x-TFR";
      when 16#33# => return "Resvd-33";
      when 16#32# => return "A24x-LCK";
      when 16#31# => return "Resvd-31";
      when 16#30# => return "Resvd-30";

      when 16#2f# => return "CR-CSR  ";
      when 16#2e# => return "Resvd-2e";
      when 16#2d# => return "A16S    ";
      when 16#2c# => return "A16x-LCK";
      when 16#2b# => return "Resvd-2b";
      when 16#2a# => return "Resvd-2a";
      when 16#29# => return "A16U    ";
      when 16#28# => return "Resvd-28";

      when 16#27# downto 16#20# => return "Resvd-2x";
      when 16#1f# downto 16#10# => return "UD-1x";

      when 16#0f# => return "A32S-BLT";
      when 16#0e# => return "A32S-PRG";
      when 16#0d# => return "A32S-DAT";
      when 16#0c# => return "A32S-MBL";
      when 16#0b# => return "A32U-BLT";
      when 16#0a# => return "A32U-PRG";
      when 16#09# => return "A32U-DAT";
      when 16#08# => return "A32U-MBL";

      when 16#07# => return "Resvd-07";
      when 16#06# => return "Resvd-06";
      when 16#05# => return "A32x-LCK";
      when 16#04# => return "A64x-LCK";
      when 16#03# => return "A64x-BLT";
      when 16#02# => return "Resvd-02";
      when 16#01# => return "A64x-TFR";
      when 16#00# => return "A64x-MBL";
    end case;
  end Image_AM;

  procedure Disp_AMCAP (cap : std_logic_vector (63 downto 0)) is
  begin
    if cap = (cap'range => '0') then
      write (output, " -");
    else
      for i in cap'range loop
        if cap (i) = '1' then
          write (output, " ");
          write (output, Image_AM (i));
        end if;
      end loop;
    end if;
  end Disp_AMCAP;

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

  signal bus_timer : std_logic;
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

  bus_timer_proc : process (clk_i)
    type state_t is (IDLE, WAIT_DS, COUNTING, WAIT_END, ERR);
    variable state : state_t;
    variable count : natural;
  begin
    if rising_edge (clk_i) then
      if VME_RST_n_i = '0' then
        state := IDLE;
        bus_timer <= '0';
      else
        bus_timer <= '0';

        case state is
          when IDLE =>
            if VME_AS_n_i = '0' then
              state := WAIT_DS;
            end if;
          when WAIT_DS =>
            count := 20;
            if VME_DS_n_i /= "11" then
              state := COUNTING;
            end if;
          when COUNTING =>
            if VME_DS_n_i = "11" then
              state := WAIT_END;
            else
              if count = 0 then
                state := ERR;
              else
                count := count - 1;
              end if;
            end if;
          when WAIT_END =>
            if VME_AS_n_i = '1' then
              state := IDLE;
            end if;

          when ERR =>
            bus_timer <= '1';
            if VME_AS_n_i = '1' then
              state := IDLE;
            end if;
        end case;
      end if;
    end if;
  end process;

  wb_p : process (clk_i)
    type slv_array is array (0 to 2**14 - 1) of std_logic_vector (31 downto 0);
    variable mem : slv_array := (others => x"1234_5678");
  begin
    if rising_edge (clk_i) then
      if rst_n_o = '0' then
        ERR_i <= '0';
        RTY_i <= '0';
        STALL_i <= '0'; --  ??
        ACK_i <= '0';
      else
        ACK_i <= '0';
        if STB_o = '1' then
          if WE_o = '0' then
            DAT_i <= mem (to_integer (unsigned (ADR_o (13 + 2 downto 2))));
            ACK_i <= '1';
          end if;
        end if;
      end if;
    end if;
  end process;
  
  tb: process
    constant c_log : boolean := False;

    -- Convert a CR/CSR address to the VME address: insert GA.
    -- The ADDR is on 20 bits (so the x"" notation can be used), but as
    -- ADDR(19) is stipped, it must be '0'.
    function to_vme_cfg_addr (addr : cfg_addr_t)
      return std_logic_vector is
    begin
      assert addr (19) = '0' report "a19 is discarded" severity error;
      return x"00" & slave_ga & addr (18 downto 0);
    end to_vme_cfg_addr;

    procedure read8 (addr : std_logic_vector (31 downto 0);
                     am : vme_am_t;
                     variable data : out byte_t)
    is
      variable l : line;
      variable res : byte_t;
    begin
      if c_log then
        write (output, "read8 at 0x" & hex (addr) & " ["
               & Image_AM (to_integer (unsigned (am))) & " ]" & LF);
      end if;

      VME_ADDR_i <= addr (31 downto 1);
      VME_AM_i <= am;
      VME_LWORD_n_i <= '1';
      VME_IACK_n_i <= '1';
      wait for 35 ns;
      VME_AS_n_i <= '0';
      VME_WRITE_n_i <= '1';
      if not (VME_DTACK_OE_o = '0' and VME_BERR_o = '0') then
        wait until VME_DTACK_OE_o = '0' and VME_BERR_o = '0';
      end if;

      if addr (0) = '0' then
        VME_DS_n_i <= "01";
      else
        VME_DS_n_i <= "10";
      end if;
      wait until (VME_DTACK_OE_o = '1' and VME_DTACK_n_o = '0')
        or bus_timer = '1';
      if bus_timer = '0' then
        assert VME_DATA_DIR_o = '1' report "bad data_dir_o";
        assert VME_DATA_OE_N_o = '0' report "bad data_oe_n_o";
        if addr (0) = '0' then
          res := VME_DATA_o (15 downto 8);
        else
          res := VME_DATA_o (7 downto 0);
        end if;
      else
        res := "XXXXXXXX";
      end if;
      data := res;

      --  Release
      VME_AS_n_i <= '1';
      VME_DS_n_i <= "11";

      if c_log then
        write (output," => 0x" & hex(res) & LF);
      end if;
    end read8;

    procedure read8_conf (addr : cfg_addr_t;
                          variable data : out byte_t)
    is
      variable l : line;
    begin
      read8 (to_vme_cfg_addr (addr), c_AM_CR_CSR, data);
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

    procedure write8_conf (addr : cfg_addr_t;
                           data : byte_t)
    is
      variable l : line;
    begin
      if c_log then
        write (l, string'("write8_conf at 0x"));
        hwrite (l, addr);
        write (l, string'(" <= "));
        hwrite (l, data);
        writeline (output, l);
      end if;

      if not (VME_DTACK_OE_o = '0' and VME_BERR_o = '0') then
        wait until VME_DTACK_OE_o = '0' and VME_BERR_o = '0';
      end if;
      VME_ADDR_i <= to_vme_cfg_addr (addr)(0 to 30);
      VME_AM_i <= c_AM_CR_CSR;
      VME_LWORD_n_i <= '1';
      VME_IACK_n_i <= '1';
      wait for 35 ns;
      VME_AS_n_i <= '0';
      VME_WRITE_n_i <= '0';
      VME_DS_n_i <= "10";

      wait until VME_DTACK_OE_o = '1' and VME_DTACK_n_o = '0';
      VME_DS_n_i <= "11";

      wait until VME_DTACK_OE_o = '0' or VME_DTACK_n_o = '1';
      assert VME_DATA_DIR_o = '0' report "bad data_dir_o (wr)";
      VME_DATA_i (7 downto 0) <= data;
      VME_DS_n_i <= "10";

      wait until VME_DTACK_OE_o = '1' and VME_DTACK_n_o = '0';
      VME_DS_n_i <= "11";

      wait until VME_DTACK_OE_o = '0' or VME_DTACK_n_o = '1';
      VME_AS_n_i <= '1';
    end write8_conf;

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

      --  VME64x
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

      for i in 0 to 7 loop
        read8_conf (std_logic_vector (unsigned'(x"0_0103") + i * 4), d);
        write (output, "CR FN" & natural'image (i) & " DAW:     0x"
               & hex2 (d) & "  [" & Disp_DAWPR (d) & ']' & LF);
      end loop;

      for i in 0 to 7 loop
        read8_conf_mb (std_logic_vector (unsigned'(x"0_0123") + i * 32), b8);
        write (output, "CR FN" & natural'image (i) & " AMCAP:    0x"
               & hex (b8) & " [");
        Disp_AMCAP (b8);
        write (output, " ]" & LF);
      end loop;


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

    read8_conf (x"7_FFFB", d);
    write (output, "CSR bit reg:     0x" & hex (d) & LF);
    read8_conf (x"7_FFEF", d);
    write (output, "CSR usr bit reg: 0x" & hex (d) & LF);

    --  READ CR
    Dump_CR;

    --  Check ADER is 0
    read8_conf (x"7_ff63", d);
    assert d = x"00" report "bad initial ADER0 value" severity error;

    -- Set ADER
    write8_conf (x"7_ff63", x"56");
    write8_conf (x"7_ff6f", c_AM_A32 & "00");
    read8_conf  (x"7_ff63", d);
    assert d = x"56" report "bad ADER0 value" severity error;

    -- Try to read (but card is not yet enabled)
    read8 (x"56_00_00_00", c_AM_A32, d);
    report "read data: " & hex (d);
    assert d = "XXXXXXXX" report "unexpected reply" severity error;

    -- Enable card
    write8_conf (x"7_fffb", b"0001_0000");
    read8_conf  (x"7_fffb", d);
    assert d = b"0001_0000" report "module must be enabled" severity error;

    --  WB read
    read8 (x"56_00_00_00", c_AM_A32, d);
    report "read data: " & hex (d);

    read8 (x"56_00_00_01", c_AM_A32, d);
    report "read data: " & hex (d);

    assert false report "end of simulation" severity failure;
    wait;
  end process;
end behaviour;
