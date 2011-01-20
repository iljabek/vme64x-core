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
--! @brief  The common_components package defines record interfaces and components used on the dmtd design. 
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
constant DATA_LENGTH : integer := 32;
constant TABLE_FIFO_ADDR_LENGTH : integer := 6;
constant FIFOS_ADDR_LENGTH : integer := 10;
constant NUMBER_CHANNELS : integer := 8;
constant CHANNEL_NUMBER_LENGTH : integer := 5;
 type DataArrayType is array (Natural range <>) of std_logic_vector(DATA_LENGTH - 1 downto 0);

type  FifoOutRecordType is
record
			Empty :  std_logic;
         NewDataReady :  std_logic;
			FifoDataOut :  std_logic_vector(DATA_LENGTH - 1 downto 0);
			FifoOverFlow :  std_logic;
end record;

type  FifoInRecordType is
record
			Data :  std_logic_vector(DATA_LENGTH - 1 downto 0);
			WrEn : std_logic;
end record;

type  DataOutRecordType is
record
			Data :  std_logic_vector(DATA_LENGTH - 1 downto 0);
			RdDone : std_logic;
--			Index : std_logic;
end record;

type  DataToSerializerRecordType is
record
			Data :  std_logic_vector(DATA_LENGTH - 1 downto 0);
			Channel : std_logic_vector(CHANNEL_NUMBER_LENGTH -1 downto 0);
			Rdy : std_logic;
--			Index : std_logic;
end record;

type  ChannelTableRecordType is
record
			DataArray :  DataArrayType(2**(TABLE_FIFO_ADDR_LENGTH+2) - 1 downto 0);
--			Channel : std_logic_vector(CHANNEL_NUMBER_LENGTH -1 downto 0);
			Last : integer;
--			Index : std_logic;
end record;


--	FifoWrEn : in std_logic;
 type FifoOutRecordArrayType is array (Natural range <>) of FifoOutRecordType;
 type FifoInRecordArrayType is array (Natural range <>) of FifoInRecordType;
 type DataOutRecordArrayType is array (Natural range <>) of DataOutRecordType;
 type ChannelTableRecordArrayType is array (Natural range <>) of ChannelTableRecordType;

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

component FifoTable 
   generic(ADDR_LENGTH : integer := 8);
      port(
         Rst : in std_logic; 
         Clk : in std_logic;
			Mux : in std_logic;
					-- NotUsed mux ='1' else FifoWrEn
					--BusRead mux ='1' else FifoRead
         DataRdEn : in std_logic; 
         Addr : in std_logic_vector(ADDR_LENGTH - 1 downto 0);
			DataOutRec : out DataOutRecordType;
--        DataRdDone : in std_logic;
-- 		 DataRdEn and DataRdDone should be synch with Mux in a top level entity

         Index : out std_logic_vector(DATA_LENGTH - 1 downto 0);
					
         FifoIn : in FifoInRecordType;

         GetNewData : in std_logic; --Resquests new data from the FIFO. It should
			                           -- be synch with Mux in a top level entity

			FifoControl : out FifoOutRecordType
			
--			FifoEmpty : out std_logic;
--	
--         NewDataReady : out std_logic;
--			FifoDataOut : out std_logic_vector(DATA_LENGTH - 1 downto 0);
--			FifoOverFlow : out std_logic
);
end component FifoTable;


component FifosReadScheduler  
generic (NFIFO : integer := 8;
         DATAL : integer := DATA_LENGTH );

port (
	Rst : in std_logic;
	Clk : in std_logic;
	
	GetNewFifoData : out std_logic_vector(NFIFO - 1 downto 0);
	FifoControl : in FifoOutRecordArrayType(NFIFO - 1 downto 0);
	
	DataToSerializerRecord : out DataToSerializerRecordType;
	SerializerRequest : in std_logic );
end component FifosReadScheduler;


component Fifos  
generic (NFIFO : integer := 8;
ADDR_LENGTH : integer := 8
);

port (
	Rst : in std_logic;
	Clk : in std_logic;
	FifoInArray : in FifoInRecordArrayType(NFIFO - 1 downto 0);
   Addr : in std_logic_vector(ADDR_LENGTH - 1 downto 0);
   DataRdEn : in std_logic_vector(NFIFO - 1 downto 0);
   DataOutRecArray : out DataOutRecordArrayType(NFIFO - 1 downto 0);
	IndexArray : out DataArrayType(NFIFO - 1 downto 0);
	DataToSerializerRecord : out DataToSerializerRecordType;
	SerializerRequest : in std_logic );

end component Fifos;

component Display is
    Port ( RstNA : in std_logic;
           Clk : in std_logic; -- was 16 Mhz in the xgater, but must work with 1 Mhz with correct rs232_tx module
			  En1m : in std_logic;
--			  PpsPulse : in std_logic;
--			  rs232_start : in std_logic;   -- signal to start the message (no effect when message_stop = '1')
			  DataToSerializerRecord : in DataToSerializerRecordType;
           SerializerRequest    : out std_logic;

--			  message_env : out std_logic; -- '1' when the message when is being sent on rs232out 
           Rs232out : out std_logic      -- the rs232 output
			);
end component Display;	

component Dmtd
generic (
REFPERIOD : integer := 40;
NCLOCKS : integer := 8;
ADDR_LENGTH : integer := 8
);
port(
rst : in std_logic;
clocks : in std_logic_vector(NCLOCKS - 1 downto 0);
refclk : in std_logic;
Addr : in std_logic_vector(ADDR_LENGTH - 1 downto 0);
DataRdEn : in std_logic_vector(NCLOCKS - 1 downto 0);
DataOutRecArray : out DataOutRecordArrayType(NCLOCKS - 1 downto 0);
IndexArray : out DataArrayType(NCLOCKS - 1 downto 0);
rs232out : out std_logic
);
end component Dmtd;

component DmtdTop is
generic(NCLOCKS : integer := 2;
GDIFF : boolean := false);
port(

rst_n : in std_logic;
clocks_p : in std_logic_vector(NCLOCKS - 1 downto 0);
clocks_n : in std_logic_vector(NCLOCKS - 1 downto 0);

refclk_p : in std_logic;
refclk_n : in std_logic;

clocksout : out std_logic_vector(NCLOCKS - 1 downto 0);
refout : out std_logic;
rs232out : out std_logic);

end component DmtdTop;

component Scaler 
   generic( N : integer := 8);
	port(
	   Rst : in  std_logic;
		Init : in std_logic;
		Clk : in std_logic;
		ClkN : out std_logic);
	
end component Scaler;


component PhaseTag is
   generic(Modulo : integer := 0; 
		     MESLENGTH : integer := 8);
	port(
	   Rst : in std_logic;
		Clk : in std_logic;
		PulseToTagA: in std_logic;
		TagDone : out std_logic;
		Tag : out std_logic_vector(MesLength - 1 downto 0));

end component PhaseTag;

component cic
generic(  INVALUELENGTH : integer := 16;
			SAMPLESTOP : integer := 2); --2^SAMPLESTOP
port (
Clk : in std_logic;
RstN : in std_logic;

Data : in std_logic_vector(31 downto 0);
ShiftCICWr : in std_logic; 
ValidValue : in std_logic;

ValueIn : in std_logic_vector(INVALUELENGTH -1 downto 0);
ValueOut : out std_logic_vector(INVALUELENGTH + 31 downto 0);
ValueDone : out std_logic;
ShiftCICD : out  std_logic_vector(31 downto 0)
);
end component cic;


component DacPll 

generic (
			TagLength : integer := 8;
			InternalCalcLength : integer := 32;
--	      PhErrorLength : integer := 32 + 8;
			ValueLength : integer := 16;
			DefaultPhError : integer := 1;
			IntegralDefault : integer := 0;
			KP : integer := 16#00008000#;
			KI : integer := 1000;
			DataLength : integer := 32); 

port (Rst : in std_logic;
		FixedClk	  : in std_logic; --Syncro Clock
		VcxoClk : in std_logic;

		DataWr : in std_logic_vector(DataLength - 1 downto 0);
		PhRefWrEn : in std_logic;
		KpWrEn : in std_logic;
		KiWrEn : in std_logic;
		PhSamplesWrEn : in std_logic;
		IntegratorWrEn : in std_logic;
		DacWrEn : in std_logic;
	   PhRef : out std_logic_vector(DataLength - 1 downto 0);
		ShiftCICWr : in std_logic;
		
      Beat : out std_logic;
		PllIteration : out std_logic;
		DacCsN : out std_logic;
		DacData : out std_logic;
		DacClk : out std_logic;
		DacClrN : out std_logic;
	   
		KiD : out std_logic_vector(31 downto 0);
		KpD : out std_logic_vector(31 downto 0);
		
		LockTopWrEn : in std_logic;
		LockTop : out std_logic_vector(31 downto 0);
		PllIterationLength : out std_logic_vector(31 downto 0);
		DeltaErrorD : out std_logic_vector(31 downto 0);   
		IntegratorD : out std_logic_vector(31 downto 0);
		ProportionalD : out std_logic_vector(31 downto 0);
		ValueD 		 : out std_logic_vector(31 downto 0);		
		SampleTopD : out std_logic_vector(31 downto 0)
	);
end component DacPll;



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