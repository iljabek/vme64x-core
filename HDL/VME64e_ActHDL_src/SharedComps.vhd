library IEEE;
use IEEE.STD_LOGIC_1164.all;

-- tripple sample sig_i signals to avoid metastable states
entity SigInputSample is
	port (
		sig_i, clk_i: in std_logic;
		sig_o: out std_logic );
end SigInputSample;

architecture RTL of SigInputSample is
	signal s_1: std_logic;
	signal s_2: std_logic;
begin
	process(clk_i)
	begin
		if rising_edge(clk_i) then
			s_1   <= sig_i;
			s_2   <= s_1;
			sig_o <= s_2;
		end if;
	end process;
end RTL;

-- *************************************************** 

library IEEE;
use IEEE.STD_LOGIC_1164.all;

-- detect rising edge
entity RisEdgeDetection is
	port (
		sig_i, clk_i: in std_logic;
		RisEdge_o: out std_logic );
end RisEdgeDetection;

architecture RTL of RisEdgeDetection is
	signal s_1: std_logic;
begin
	process(clk_i)
	begin
		if rising_edge(clk_i) then
			
			s_1 <= sig_i;
			
			if s_1 = '0' and sig_i = '1' then
				RisEdge_o <= '1';
			else
				RisEdge_o <= '0';
			end if;
			
		end if;
	end process;
end RTL;   

-- *************************************************** 

library IEEE;
use IEEE.STD_LOGIC_1164.all;

-- detect rising edge
entity StretchedRisEdgeDetection is
	port (
		sig_i, clk_i: in std_logic;
		RisEdge_o: out std_logic );
end StretchedRisEdgeDetection;

architecture RTL of StretchedRisEdgeDetection is
	signal s_1: std_logic;
	signal s_2: std_logic;
begin
	process(clk_i)
	begin
		if rising_edge(clk_i) then
			
			s_1 <= sig_i;
			s_2 <= s_1;
			
			if (s_1 = '0' and sig_i = '1') or (s_2 = '0' and s_1 = '1') then
				RisEdge_o <= '1';
			else
				RisEdge_o <= '0';
			end if;
			
		end if;
	end process;
end RTL;

-- *************************************************** 

library IEEE;
use IEEE.STD_LOGIC_1164.all;

-- double sample input signal (sig_i) to avoid metastable states
-- and give pulse (sigEdge_o) at falling edge
entity SigInputSampleAndFallingEdgeDetection is
	port (
		sig_i, clk_i: in std_logic;
		sigEdge_o: out std_logic);
end SigInputSampleAndFallingEdgeDetection;

architecture RTL of SigInputSampleAndFallingEdgeDetection is
	signal s_1, s_2, s_3: std_logic;
begin
	process(clk_i)
	begin
		if rising_edge(clk_i) then
			
			s_1 <= sig_i;
			s_2 <= s_1;
			s_3 <= s_2;
			
			if s_3 = '1' and s_2 = '0' then
				sigEdge_o <= '1';
			else
				sigEdge_o <= '0';
			end if;
			
		end if;
	end process;
end RTL;

-- ***************************************************

library IEEE;
use IEEE.STD_LOGIC_1164.all;

-- double sample input signal (sig_i) to avoid metastable states
-- and give pulse (sigEdge_o) at rising edge
entity SigInputSampleAndRisingEdgeDetection is
	port (
		sig_i, 
		clk_i: in std_logic;
		sigEdge_o: out std_logic 
		);
end SigInputSampleAndRisingEdgeDetection;

architecture RTL of SigInputSampleAndRisingEdgeDetection is
	signal s_1, s_2, s_3: std_logic;
begin
	process(clk_i)
	begin
		if rising_edge(clk_i) then
			
			s_1 <= sig_i;
			s_2 <= s_1;
			s_3 <= s_2;
			
			if s_3 = '0' and s_2 = '1' then
				sigEdge_o <= '1';
			else
				sigEdge_o <= '0';
			end if;
			
		end if;
	end process;
end RTL;

-- ***************************************************

library IEEE;
use IEEE.STD_LOGIC_1164.all;

-- double sample input signal (sig_i) to avoid metastable states
-- and give pulse (sigEdge_o) at rising and falling edge
entity SigInputSampleAndEdgeDetection is
	port (
		sig_i, 
		clk_i: in std_logic;
		sigEdge_o: out std_logic
		);
end SigInputSampleAndEdgeDetection;

architecture RTL of SigInputSampleAndEdgeDetection is
	signal s_1, s_2, s_3: std_logic;
begin
	process(clk_i)
	begin
		if rising_edge(clk_i) then
			
			s_1 <= sig_i;
			s_2 <= s_1;
			s_3 <= s_2;
			
			if (s_3 = '0' and s_2 = '1') or (s_3 = '1' and s_2 = '0') then
				sigEdge_o <= '1';
			else
				sigEdge_o <= '0';
			end if;
			
		end if;
	end process;
end RTL;

-- ***************************************************

library IEEE;
use IEEE.STD_LOGIC_1164.all;

-- triple sample input register reg_i to avoid metastable states
-- and catching of transition values
entity RegInputSample is 
	generic(
		width: natural:=8
		);
	port (
		reg_i: in std_logic_vector(width-1 downto 0);
		reg_o: out std_logic_vector(width-1 downto 0);
		clk_i: in std_logic 
		);
end RegInputSample;

architecture RTL of RegInputSample is
	signal reg_1, reg_2: std_logic_vector(width-1 downto 0);
begin
	process(clk_i)
	begin
		if rising_edge(clk_i) then
			reg_1 <= reg_i;
			reg_2 <= reg_1;	
			reg_o <= reg_2;	 

		end if;
	end process;
end RTL;


