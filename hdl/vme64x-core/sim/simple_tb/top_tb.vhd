entity top_tb is
end;

library ieee;
use ieee.std_logic_1164.all;
use work.vme64x_pack.all;
use std.textio.all;
use ieee.std_logic_textio.all;

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

    constant c_log : boolean := True;

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

      if c_log then
        write (l, string'(" => 0x"));
        hwrite(l, VME_DATA_o (7 downto 0));
        writeline (output, l);
      end if;
    end read8_conf;

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

    --  Read CR
    read8_conf (x"7_FFFF", d);
    assert d = slave_ga & "000" report "bad CR/CSR BAR value" severity error;

    assert false report "end of simulation" severity failure;
    wait;
  end process;
end behaviour;
