-------------------------------------------------------------------------------
--! @file FifoTable.vhd
-------------------------------------------------------------------------------
--! Standard library
library IEEE;
--! Standard packages
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.ALL;
--! Specific packages
use work.common_components.all;
----------------------------------------------------------------------------
-- --
-- CERN, BE  --
-- --
-------------------------------------------------------------------------------
--
-- unit name: FifoTable
--
--! @brief FifoTable provides with a FIFO whose contents can also be accessed randomly through a Control port 
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

entity Fifo is
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
			FifoOverFlow : out std_logic

			

);
end Fifo;

architecture Rtl of Fifo is
-- signal fifoDataInD : std_logic_vector(g_DATA_LENGTH - 1 downto 0);
 signal writeIndex : signed(g_ADDR_LENGTH downto 0);
 signal readIndex : signed(g_ADDR_LENGTH downto 0);
 signal fifoAdd, iIndex : std_logic_vector(g_ADDR_LENGTH -1 downto 0);
 signal fifoData : std_logic_vector(g_DATA_LENGTH - 1 downto 0);
-- signal wrMem : std_logic;
 signal fifoEmpty, nxFifoEmpty : std_logic;
-- signal fifoOverFlow : std_logic;
 signal rstN : std_logic;
 signal dataRdEnD : std_logic;
 signal getNewDataFlag, newDataReady : std_logic;
 signal muxD : std_logic;
begin
process(Clk)
begin 
if rising_edge(Clk) then
   if Rst = '1' then 
		writeIndex <= (others => '0');
		readIndex <= (others => '0');
		fifoOverFlow <= '0';
		RdDone <= '0';
		dataRdEnD <= '0';
		fifoEmpty <= '1';
		getNewDataFlag <= '0';
	   newDataReady <= '0';
		muxD <= '0';
	else
			muxD <= Mux;

	if WrEn = '1' then
--		fifoDataInD <= Data;
		writeIndex <= writeIndex + 1;
	end if;
--	wrMem <= WrEn;
--	wrMemD <= wrMem;

	
	if GetNewData = '1' and fifoEmpty = '0' then
	   readIndex <= readIndex + 1;
	end if;

		fifoEmpty <= nxFifoEmpty;
	if GetNewData = '1' then
		getNewDataFlag <= '1';
	elsif newDataReady = '1' then 
		getNewDataFlag <= '0';
	end if;
	
	if (getNewDataFlag = '1' or GetNewData = '1') and fifoEmpty = '0' and Mux = '0' then
	   newDataReady <= '1';
	else
	   newDataReady <= '0';
	end if;
	if MuxD = '0' then
		FifoDataOut <= fifoData;
	end if;
	if (writeIndex - readIndex) < 0 then
	 fifoOverFlow <= '1';
	end if;
	
	dataRdEnD <= DataRdEn;
	if mux = '1' and DataRdEn = '1' then
	 RdDone <= '1';
	elsif mux = '1' and dataRdEnD = '1' then
	 RdDone <= '1';
	else
	 RdDone <= '0';
	end if;
	end if;
end if;
end process;
--FifoOverFlow <= fifoOverFlow;
Empty <= fifoEmpty;
NewFifoDataReady <= newDataReady;


iIndex <= std_logic_vector(writeIndex(iIndex'range));


process(iIndex)
begin
Index <= (others => '0');
Index(iIndex'range) <= iIndex;
end process;

fifoAdd <= std_logic_vector(readIndex(fifoAdd'range));

process(readIndex, writeIndex)
begin
   nxFifoEmpty <= '0';
   if readIndex = writeIndex then
      nxFifoEmpty <= '1';
   end if;
end process;

rstN <= not Rst;



UTriPortRamWr_RdRD : TriPortRamWr_RdRD 
generic map(Al => g_ADDR_LENGTH,
nw => 2**g_ADDR_LENGTH,
dl => g_DATA_LENGTH)

port map(
	Clk => Clk,
	RstN  => RstN,
	MuxB_CN => Mux, 
	
	AAdd => iIndex,
	ADataIn => data_i,
	AWrEn => WrEn,

	BAdd => Addr,
	BDataOut => data_o,

	CAdd => fifoAdd,
	CDataOut => fifoData);

end rtl;