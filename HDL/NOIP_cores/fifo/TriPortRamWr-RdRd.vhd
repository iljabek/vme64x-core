-------------------------------------------------------------------------------
--! @file TriPortRamWr-RdRd.vhd
-------------------------------------------------------------------------------
--! Standard library
library IEEE;
--! Standard packages
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
--! Specific packages
use work.common_components.all;
----------------------------------------------------------------------------
----------------------------------------------------------------------------
-- --
-- CERN, BE  --
-- --
-------------------------------------------------------------------------------
--
-- unit name: TriPortRamWr_RdRd
--
--! @brief The TriPortRamWr_RdRd implements a time multiplexed Triport ram. Port A has permanent access, Port B has access when mux = '1' and C when mux = '0'
--! 
--! @author <Pablo Alvarez(pablo.alvarez.sanchez@cern.ch)>
--
--! @date 24\01\2009
--
--! @version 1
--
--! @details
--!
--! <b>Dependencies:</b>\n
--! 
--!
--! <b>References:</b>\n
--! <reference one> \n
--! <reference two>
--!
--! <b>Modified by:</b>\n
--! Author: <name>
-------------------------------------------------------------------------------
--! \n\n<b>Last changes:</b>\n
--! 24\01\2009 paas header included\n
--! <extended description>
-------------------------------------------------------------------------------
--! @todo Adapt vhdl sintax to ohr standard\n
--! <another thing to do> \n
--
-------------------------------------------------------------------------------


entity TriPortRamWr_RdRD is
generic(Al : integer := 8;
nw : integer := 256;
dl : integer := 32);

port(
Clk : in std_logic;
RstN : in std_logic;

MuxB_CN : in std_logic; 


AAdd : in std_logic_vector(Al-1 downto 0);
ADataIn : in std_logic_vector(dl - 1  downto 0);
AWrEn : in std_logic;

BAdd : in std_logic_vector(Al-1 downto 0);
BDataOut : out std_logic_vector(dl - 1  downto 0);

CAdd : in std_logic_vector(Al-1 downto 0);
CDataOut : out std_logic_vector(dl - 1 downto 0));
end TriPortRamWr_RdRD;

architecture Behavioral of TriPortRamWr_RdRD is

--signal nxBCDataIn :  std_logic_vector(31 downto 0);
signal nxDADataIn :  std_logic_vector(dl - 1 downto 0);

signal nxBCAdd :  std_logic_vector(Al-1 downto 0);
signal nxDAAdd :  std_logic_vector(Al-1 downto 0);

signal DADataOut :  std_logic_vector(dl - 1  downto 0);
--signal BCDataOut :  std_logic_vector(dl - 1  downto 0);
signal doBC, idoBC :  std_logic_vector(dl - 1 downto 0);
signal nxDAwe : std_logic;

begin

nxDADataIn <=  ADataIn;
nxDAAdd <= AAdd;
nxBCAdd <= BAdd when MuxB_CN = '1' else CAdd;
nxDAwe <=  AWrEn;

Udpblockram : dpblockram 
 generic map(dl => dl, 		-- Length of the data word 
 			 al => al,			-- Size of the addr map (10 = 1024 words)
			 nw => nw)    -- Number of words
			 									-- 'nw' has to be coherent with 'al'
 port map(clk  => clk, 			-- Global Clock
 	we  => nxDAwe,				-- Write Enable
 	aw  => nxDAAdd, -- Write Address 
 	ar  => nxBCAdd, 	 -- Read Address
 	di  => nxDADataIn,   -- Data input
 	dw =>  open,-- Data write, normaly open
 	do  => idoBC); 	 -- Data output

BDataOut <=	idoBC;
CDataOut <= idoBC;


end Behavioral;
