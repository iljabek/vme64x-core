library IEEE;
use IEEE.STD_LOGIC_1164.ALL;


----------------------------------------------------------------------------
-- --
-- CERN, BE  --
-- --
-------------------------------------------------------------------------------
--
-- unit name: common_components
--
--! @brief  The common_components package defines some common components. 
--! 
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
--!  \n
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

 package common_components is

 component  dpblockram
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


component QuadPortRam
generic(Al : integer := 8;
nw : integer := 256;
dl : integer := 32
);

port(
Clk : in std_logic;
RstN : in std_logic;

Mux : in std_logic;

DAdd : in std_logic_vector(Al-1 downto 0);
DDataIn : in std_logic_vector(dl -1 downto 0);
DWrEn : in std_logic;
DDataOut : out std_logic_vector(dl -1 downto 0);

AAdd : in std_logic_vector(Al-1 downto 0);
ADataIn : in std_logic_vector(dl -1 downto 0);
AWrEn : in std_logic;

BAdd : in std_logic_vector(Al-1 downto 0);
BDataOut : out std_logic_vector(dl -1 downto 0);

CAdd : in std_logic_vector(Al-1 downto 0);
CDataOut : out std_logic_vector(dl -1 downto 0));
end component QuadPortRam;

component TriPortRamWr_RdRD 
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
end component TriPortRamWr_RdRD;


component Fifo 
   generic(g_ADDR_LENGTH : integer := 8;
		     g_DATA_LENGTH : integer := 32);
      port(
         Rst : in std_logic; 
         Clk : in std_logic;
			Mux : in std_logic;
					-- NotUsed mux ='1' else FifoWrEn
					--BusRead mux ='1' else FifoRead
         DataRdEn : in std_logic; 
         Addr : in std_logic_vector(g_ADDR_LENGTH - 1 downto 0);
			
--			DataOutRec : out DataOutRecordType;
			data_o : out std_logic_vector(g_DATA_LENGTH - 1 downto 0);
			RdDone : out std_logic;			
			
--        DataRdDone : in std_logic;
-- 		 DataRdEn and DataRdDone should be synch with Mux in a top level entity

         Index : out std_logic_vector(g_DATA_LENGTH - 1 downto 0);
					
--         FifoIn : in FifoInRecordType;
			data_i : in std_logic_vector(g_DATA_LENGTH - 1 downto 0);
			WrEn : in std_logic;

			
         GetNewData : in std_logic; --Resquests new data from the FIFO. It should			                           -- be synch with Mux in a top level entity
--			FifoControl : out FifoOutRecordType
			Empty :  out std_logic;
         NewFifoDataReady : out std_logic;
			FifoDataOut : out std_logic_vector(g_DATA_LENGTH - 1 downto 0);
			FifoOverFlow : out std_logic);
end component Fifo;



function log2_f(n : in integer) return integer ;

end package common_components;
 
package body common_components is 


   function log2_f(n : in integer) return integer is
      variable i : integer := 0;
   begin
      while (2**i <= n) loop
         i := i + 1;
      end loop;
      return i-1;
   end log2_f;


end package body common_components;