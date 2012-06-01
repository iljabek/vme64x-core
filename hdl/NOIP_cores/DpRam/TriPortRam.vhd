--------------------------------------------------------------------------------
-- Company: 
-- Engineer:
--
-- Create Date:    14:33:36 08/28/06
-- Design Name:    
-- Module Name:    QuadPortRam - Behavioral
-- Project Name:   
-- Target Device:  
-- Tool versions:  
-- Description:
--
-- Dependencies:
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
--------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

---- Uncomment the following library declaration if instantiating
---- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity TriPortRam is
generic(Al : integer := 8;
dl : integer := 32;
nw : integer := 256);

port(
Clk : in std_logic;
RstN : in std_logic;

Mux : in std_logic;

VmeAdd : in std_logic_vector(Al-1 downto 0);
VmeDataIn : in std_logic_vector(31 downto 0);
VmeWrEn : in std_logic;
VmeDataOut : out std_logic_vector(31 downto 0);
VmeRdEn :in std_logic;
VmeRdDone : out std_logic;

AAdd : in std_logic_vector(Al-1 downto 0);
ADataIn : in std_logic_vector(dl-1 downto 0);
AWrEn : in std_logic;

BAdd : in std_logic_vector(Al-1 downto 0);
BDataOut : out std_logic_vector(dl-1 downto 0));
end TriPortRam;

architecture Behavioral of TriPortRam is
component dpblockram 
 generic (dl : integer := 42; 		-- Length of the data word 
 			 al : integer := 10;			-- Size of the addr map (10 = 1024 words)
			 nw : integer := 1024);    -- Number of words
			 									-- 'nw' has to be coherent with 'al'
 port (clk  : in std_logic; 			-- Global Clock
 	we   : in std_logic; 				-- Write Enable
 	aw    : in std_logic_vector(al - 1 downto 0); -- Write Address 
 	ar : in std_logic_vector(al - 1 downto 0); 	 -- Read Address
 	di   : in std_logic_vector(dl - 1 downto 0);  -- Data input
 	dw  : out std_logic_vector(dl - 1 downto 0);  -- Data write, normaly open
 	do  : out std_logic_vector(dl - 1 downto 0)); 	 -- Data output
 end component dpblockram; 
--signal nxBCDataIn :  std_logic_vector(31 downto 0);
signal nxVMEADataIn :  std_logic_vector(dl-1 downto 0);

signal nxBCAdd :  std_logic_vector(Al-1 downto 0);
signal nxVMEAAdd :  std_logic_vector(Al-1 downto 0);
signal VMEAre,nxVMEAre : std_logic;
signal VMEADataOut :  std_logic_vector(dl-1 downto 0);
signal BCDataOut :  std_logic_vector(dl-1 downto 0);
signal doBC, idoBC :  std_logic_vector(dl-1 downto 0);
signal nxVMEAwe : std_logic;
signal iVmeDataOut : std_logic_vector(dl -1 downto 0);
signal pendingVmeRdEn, pendingVmeWrEn  : std_logic := '0';
 
signal vmeAddD : std_logic_vector(Al-1 downto 0);
signal vmeDataInD : std_logic_vector(31 downto 0);
signal pendingVmeD : std_logic;
begin
process(clk)
begin
if rising_edge(clk) then
   if VmeWrEn = '1' or VmeRdEn = '1' then
		vmeAddD <= VmeAdd;
		vmeDataInD <= VmeDataIn;
	end if;
	if RstN = '0' then
		pendingVmeWrEn <= '0';
	elsif VmeWrEn = '1' then
		pendingVmeWrEn <= '1';
		vmeAddD <= VmeAdd;
		vmeDataInD <= VmeDataIn;
	elsif pendingVmeWrEn = '1' and AWrEn = '0' then
		pendingVmeWrEn <= '0';
	end if;	
	
	if RstN = '0' then
		pendingVmeRdEn <= '0';
	elsif VmeRdEn = '1' then
		pendingVmeRdEn <= '1';
	elsif pendingVmeRdEn = '1' and AWrEn = '0' then
		pendingVmeRdEn <= '0';
	end if;	
end if;
end process;

nxVMEADataIn <= vmeDataInD(dl - 1 downto 0) when AWrEn = '0' else ADataIn;
nxVMEAAdd <= vmeAddD when AWrEn = '0' else AAdd;

nxBCAdd <= BAdd;--when Mux = '1' else CAdd;
nxVMEAwe <= (pendingVmeWrEn and (not AWrEn)) or  AWrEn;

-- nxVMEAre <= (pendingVmeRdEn and (not AWrEn));--or  AWrEn;

Udpblockram : dpblockram
 generic map(dl => dl, 		-- Length of the data word 
 			 al => al,			-- Size of the addr map (10 = 1024 words)
			 nw => nw)    -- Number of words
			 									-- 'nw' has to be coherent with 'al'
 port map(clk  => clk, 			-- Global Clock
 	we  => nxVMEAwe,				-- Write Enable
 	aw  => nxVMEAAdd, -- Write Address 
 	ar  => nxBCAdd, 	 -- Read Address
 	di  => nxVMEADataIn,   -- Data input
 	dw =>  iVmeDataOut,-- Data write, normaly open
 	do  => idoBC); 	 -- Data output

BDataOut <=	doBC;
--CDataOut <= doBC;


process(Clk)
begin
if rising_edge(Clk) then
	if RstN = '0' then
		pendingVmeD <= '0';
	else
	   pendingVmeD  <= (pendingVmeRdEn or pendingVmeWrEn) and (not AWrEn);
		VmeRdDone <= pendingVmeD;
	end if;
end if;
end process;

-------------------------------------
process(Clk)
begin
if rising_edge(Clk) then
	VmeDataOut(VmeDataOut'left downto iVmeDataOut'left +1) <= (others => '0');
	VmeDataOut(iVmeDataOut'range) <= iVmeDataOut;
end if;
end process;

doBC <= idoBC;

end Behavioral;
