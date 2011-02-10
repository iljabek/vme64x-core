library ieee;
use ieee.std_logic_1164.all;

-- Add your library and packages declaration here ...
 
entity vme64xcore_top_reg_tb is
end vme64xcore_top_reg_tb;

architecture TB_ARCHITECTURE of vme64xcore_top_reg_tb is
	-- Component declaration of the tested unit
	component vme64xcore_top_reg
		port(
			clk_i : in STD_LOGIC;
			VME_AS_n_i : in STD_LOGIC;
			VME_RST_n_i : in STD_LOGIC;
			VME_WRITE_n_i : in STD_LOGIC;
			VME_AM_i : in STD_LOGIC_VECTOR(5 downto 0);
			VME_DS_n_i : in STD_LOGIC_VECTOR(1 downto 0);
			VME_GA_i : in STD_LOGIC_VECTOR(5 downto 0);
			VME_BERR_o : out STD_LOGIC;
			VME_DTACK_n_o : out STD_LOGIC;
			VME_RETRY_n_o : out STD_LOGIC;
			VME_LWORD_n_b : inout STD_LOGIC;
			VME_ADDR_b : inout STD_LOGIC_VECTOR(31 downto 1);
			VME_DATA_b : inout STD_LOGIC_VECTOR(31 downto 0);
--			VME_BBSY_n_i : in STD_LOGIC; 
			VME_IRQ_n_o : out STD_LOGIC_VECTOR(6 downto 0);
--			VME_IACKIN_n_i : in STD_LOGIC;
--			VME_IACKOUT_n_o : out STD_LOGIC; 
			
			VME_DTACK_OE_o:       out std_logic;
        	VME_DATA_DIR_o:       out std_logic;
        	VME_DATA_OE_o:        out std_logic;
        	VME_ADDR_DIR_o:       out std_logic;
        	VME_ADDR_OE_o:        out std_logic);
end component;	
	
	-- Component declaration of the "sim_vme64master(sim_vme64master)" unit defined in
	-- file: "./../../testbenches/sim_vme64master.vhd"
	component sim_vme64master
	port(
		clk_i : in STD_LOGIC;
		VME_AS_n_i : out STD_LOGIC;
		VME_LWORD_n_b : inout STD_LOGIC;
		VME_RETRY_n_i : in STD_LOGIC;
		VME_WRITE_n_o : out STD_LOGIC;
		VME_GA_i 		: 		in STD_LOGIC_VECTOR(4 downto 0);

		VME_DS_n_o : out STD_LOGIC_VECTOR(1 downto 0);
		VME_DTACK_n_i : in STD_LOGIC;
		VME_BERR_n_i : in STD_LOGIC;
		VME_ADDR_b : inout STD_LOGIC_VECTOR(31 downto 1);
		VME_DATA_b : inout STD_LOGIC_VECTOR(31 downto 0);
		VME_AM_o : out STD_LOGIC_VECTOR(5 downto 0);
		VME_IRQ_n_i : in STD_LOGIC_VECTOR(6 downto 0); 
		
		VME_DTACK_OE_i:       in std_logic;
        VME_DATA_DIR_i:       in std_logic;
        VME_DATA_OE_i:        in std_logic;
        VME_ADDR_DIR_i:       in std_logic;
        VME_ADDR_OE_i:        in std_logic;
		
		VME_IACKOUT_n_o : out STD_LOGIC);
	end component;
--	for all: sim_vme64master use entity work.sim_vme64master(sim_vme64master);

	
	-- Component declaration of the "sim_wbslave(sim_wbslave)" unit defined in
	-- file: "./../../testbenches/sim_wbslave.vhd"
	component sim_wbslave
	port(
		clk_i : in STD_LOGIC;
		RST_i : in STD_LOGIC;
		DAT_i : in STD_LOGIC_VECTOR(63 downto 0);
		DAT_o : out STD_LOGIC_VECTOR(63 downto 0);
		ADR_i : in STD_LOGIC_VECTOR(63 downto 0);
		--TGA_i : in STD_LOGIC_VECTOR(3 downto 0);
		--TGC_i : in STD_LOGIC_VECTOR(3 downto 0);
		CYC_i : in STD_LOGIC;
		ERR_o : out STD_LOGIC;
		LOCK_i : in STD_LOGIC;
		RTY_o : out STD_LOGIC;
		SEL_i : in STD_LOGIC_VECTOR(7 downto 0);
		STB_i : in STD_LOGIC;
		ACK_o : out STD_LOGIC;
		WE_i : in STD_LOGIC; 
		STALL_o : out STD_LOGIC;
		IRQ_o : out STD_LOGIC);
	end component;
	--for all: sim_wbslave use entity work.sim_wbslave(sim_wbslave);

	-- Stimulus signals - signals mapped to the input and inout ports of tested entity
	signal clk_i : STD_LOGIC;
	signal VME_AS_n_i : STD_LOGIC;
	signal VME_RST_n_i : STD_LOGIC;
	signal VME_WRITE_n_i : STD_LOGIC;
	signal VME_AM_i : STD_LOGIC_VECTOR(5 downto 0);
	signal VME_DS_n_i : STD_LOGIC_VECTOR(1 downto 0);
	signal VME_GA_i : STD_LOGIC_VECTOR(5 downto 0);
	signal VME_BBSY_n_i : STD_LOGIC;
	signal VME_IACKIN_n_i : STD_LOGIC;
	signal RST_i : STD_LOGIC;
	signal DAT_i : STD_LOGIC_VECTOR(63 downto 0);
	signal ERR_i : STD_LOGIC;
	signal RTY_i : STD_LOGIC;
	signal ACK_i : STD_LOGIC;
	signal IRQ_i : STD_LOGIC;
	signal STALL_i : STD_LOGIC;
	signal VME_LWORD_n_b : STD_LOGIC;
	signal VME_ADDR_b, s_VME_ADDR_b : STD_LOGIC_VECTOR(31 downto 1);
	signal VME_DATA_b : STD_LOGIC_VECTOR(31 downto 0);
	-- Observed signals - signals mapped to the output ports of tested entity
	signal VME_BERR_n_o, VME_BERR_o : STD_LOGIC;
	signal VME_DTACK_n_o : STD_LOGIC;
	signal VME_RETRY_n_o : STD_LOGIC;
	signal VME_IRQ_n_o : STD_LOGIC_VECTOR(6 downto 0);
	signal VME_IACKOUT_n_o : STD_LOGIC;
	signal DAT_o : STD_LOGIC_VECTOR(63 downto 0);
	signal ADR_o : STD_LOGIC_VECTOR(63 downto 0);
	signal TGA_o : STD_LOGIC_VECTOR(3 downto 0);
	signal TGC_o : STD_LOGIC_VECTOR(3 downto 0);
	signal CYC_o : STD_LOGIC;
	signal LOCK_o : STD_LOGIC;
	signal SEL_o : STD_LOGIC_VECTOR(7 downto 0);
	signal STB_o : STD_LOGIC;
	signal WE_o : STD_LOGIC;
	
	signal VME_DTACK_OE_o:std_logic;
    signal VME_DATA_DIR_o:std_logic;
   	signal VME_DATA_OE_o:std_logic;
    signal VME_ADDR_DIR_o:       std_logic;
    signal VME_ADDR_OE_o:std_logic;
	 signal s_VME_DTACK_n : std_logic;
	-- Add your code here ...
	
begin
	
	-- Unit Under Test port map
	UUT : vme64xcore_top_reg
	port map (
		clk_i => clk_i,
		VME_AS_n_i => VME_AS_n_i,
		VME_RST_n_i => VME_RST_n_i,
		VME_WRITE_n_i => VME_WRITE_n_i,
		VME_AM_i => VME_AM_i,
		VME_DS_n_i => VME_DS_n_i,
		VME_GA_i => VME_GA_i,
		VME_BERR_o => VME_BERR_o,
		VME_DTACK_n_o => VME_DTACK_n_o,
		VME_RETRY_n_o => VME_RETRY_n_o,
		VME_LWORD_n_b => VME_LWORD_n_b,
		VME_ADDR_b => VME_ADDR_b,
		VME_DATA_b => VME_DATA_b,
--		VME_BBSY_n_i => VME_BBSY_n_i,
					
		VME_DTACK_OE_o => VME_DTACK_OE_o,
        VME_DATA_DIR_o => VME_DATA_DIR_o,
        VME_DATA_OE_o => VME_DATA_OE_o,
        VME_ADDR_DIR_o => VME_ADDR_DIR_o,
        VME_ADDR_OE_o => VME_ADDR_OE_o,
			
		VME_IRQ_n_o => VME_IRQ_n_o
--		VME_IACKIN_n_i => VME_IACKIN_n_i,
--		VME_IACKOUT_n_o => VME_IACKOUT_n_o
--		RST_i => RST_i,
--		DAT_i => DAT_i,
--		DAT_o => DAT_o,
--		ADR_o => ADR_o,
--		--TGA_o => TGA_o,
--		--TGC_o => TGC_o,
--		CYC_o => CYC_o,
--		ERR_i => ERR_i,
--		LOCK_o => LOCK_o,
--		RTY_i => RTY_i,
--		SEL_o => SEL_o,
--		STB_o => STB_o,
--		ACK_i => ACK_i,
--		WE_o => WE_o,
--		STALL_i => STALL_i,
--		IRQ_i => IRQ_i
		);
		
	VME_BERR_n_o <= VME_BERR_o;
	s_VME_DTACK_n <= VME_DTACK_n_o when VME_DTACK_OE_o = '1' else '1';
	 VME_ADDR_b <= s_VME_ADDR_b when VME_ADDR_DIR_o = '0' else (others => 'Z');
--		VME_DTACK_OE_i => VME_DTACK_OE_o,
--        VME_DATA_DIR_i => VME_DATA_DIR_o,
--        VME_DATA_OE_i => VME_DATA_OE_o,
--        VME_ADDR_DIR_i => VME_ADDR_DIR_o,
--        VME_ADDR_OE_i => VME_ADDR_OE_o,
	
		  
		  
	stimulGen : sim_vme64master
	port map(
		clk_i => clk_i,
		VME_AS_n_i => VME_AS_n_i,
		VME_LWORD_n_b => VME_LWORD_n_b,
		VME_RETRY_n_i => VME_RETRY_n_o,
		VME_WRITE_n_o => VME_WRITE_n_i,
	   VME_GA_i  => VME_GA_i(4 downto 0),
		VME_DS_n_o => VME_DS_n_i,
		VME_DTACK_n_i => s_VME_DTACK_n,
		VME_BERR_n_i => VME_BERR_n_o,
		VME_ADDR_b => s_VME_ADDR_b,
		VME_DATA_b => VME_DATA_b,
		VME_AM_o => VME_AM_i, 
		
		VME_DTACK_OE_i => VME_DTACK_OE_o,
        VME_DATA_DIR_i => VME_DATA_DIR_o,
        VME_DATA_OE_i => VME_DATA_OE_o,
        VME_ADDR_DIR_i => VME_ADDR_DIR_o,
        VME_ADDR_OE_i => VME_ADDR_OE_o,
		
		VME_IRQ_n_i => VME_IRQ_n_o,
		VME_IACKOUT_n_o => VME_IACKIN_n_i
		);


--	stimulGen_wb : sim_wbslave
--	port map(
--		clk_i => clk_i,
--		RST_i => RST_i,
--		DAT_i => DAT_o,
--		DAT_o => DAT_i,
--		ADR_i => ADR_o,
--		--TGA_i => TGA_o,
--		--TGC_i => TGC_o,
--		CYC_i => CYC_o,
--		ERR_o => ERR_i,
--		LOCK_i => LOCK_o,
--		RTY_o => RTY_i,
--		SEL_i => SEL_o,
--		STB_i => STB_o,
--		ACK_o => ACK_i,
--		WE_i => WE_o,
--		STALL_O => STALL_i,
--		IRQ_o => IRQ_i
--	);
		
	clkGen: process 
	begin
		--clk_i<='0', '1' after 0.5 ns;
		--wait for 0.5ns;
		clk_i <= '0';
		wait for 5 ns;
		clk_i <= '1';
		wait for 5 ns;
	end	process;
	
	VME_GA_i <= "111011";
	
	VME_RST_n_i <= '0', '1' after 200 ns;	 
	RST_i <= '1', '0' after 200 ns;
	--RTY_i <= '0';
	--ERR_I <= '0';
	
end TB_ARCHITECTURE;


