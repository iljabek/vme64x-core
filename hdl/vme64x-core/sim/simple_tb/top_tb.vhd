entity top_tb is
end;

library ieee;
use ieee.std_logic_1164.all;
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
begin
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
  begin
    rst_n_i <= '0';
    VME_AS_n_i <= '1';
    VME_ADDR_i <= (others => '1');
    VME_AM_i <= (others => '1');
    for i in 1 to 8 loop
      wait until rising_edge (clk_i);
    end loop;

    assert false report "end of simulation" severity failure;
    wait;
  end process;
end behaviour;
