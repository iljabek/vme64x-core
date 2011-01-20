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

entity QuadPortRam is
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
BDataOut : out std_logic_vector(dl-1 downto 0);

CAdd : in std_logic_vector(Al-1 downto 0);
CDataOut : out std_logic_vector(dl-1 downto 0));
end QuadPortRam;

architecture Behavioral of QuadPortRam is
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
begin

nxVMEADataIn <= VMEDataIn(dl - 1 downto 0) when Mux = '1' else ADataIn;
nxVMEAAdd <= VMEAdd when Mux = '1' else AAdd;
nxBCAdd <= BAdd when Mux = '1' else CAdd;
nxVMEAwe <= VmeWrEn when Mux = '1' else AWrEn;
nxVMEAre <= VmeRdEn when Mux = '1' else '0';

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
CDataOut <= doBC;


process(Clk)
begin
if rising_edge(Clk) then
	if RstN = '0' then
		VmeRdDone <= '0';
		VMEAre <= '0';
	else
		VMEAre <= nxVMEAre;
		VmeRdDone <= VMEAre;
	end if;
end if;
end process;
-------------------------------------
process(Clk)
begin
if rising_edge(Clk) then
	VmeDataOut <= (others => '0');
	VmeDataOut(iVmeDataOut'range) <= iVmeDataOut;
end if;
end process;
-------------------------------------
process(nxVMEAAdd, nxBCAdd, idoBC, nxVMEADataIn, iVmeDataOut)
begin
VmeDataOut <= (others => '0');
VmeDataOut(iVmeDataOut'range) <= iVmeDataOut;
if nxVMEAAdd = nxBCAdd then
doBC <= nxVMEADataIn;
else
doBC <= idoBC;
end if;
end process;

end Behavioral;
