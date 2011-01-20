--	Package File Template
--
--	Purpose: This package defines supplemental types, subtypes, 
--		 constants, and functions 


library IEEE;
use IEEE.STD_LOGIC_1164.all;
--use work.vme.all;

package IntContPackage is
  

	constant	DPRAM_COUNT_HIS : integer := 1;
	constant  ADDTOP : integer := 16#FFFFF#;  --
	-- 	constant ADDLENGTH  : integer := 23 - 2 + 1	; --Ctrp and Ctri
	constant ADDLENGTH  : integer := 14	; 
	constant DATALENGTH : integer := 32 ;

	subtype RamAType is integer range 0 to ADDTOP;
	type RwType is  (rw, --Read Write
				   ro, --Read Only
			   wo, --Write Only
			   cr  --Clear on Read
			   );

  type AddRwRecord is
  record
		AddL : RamAType;
		AddH : RamAType;
		Delay : integer;
		Rw : RwType;
		PosToSel : integer;
--		Name : string(1 to 12);
  end record;
  
constant NUMMEMPOSITION : integer := 24;

subtype MEMPOSITION  is   integer range 0 to NUMMEMPOSITION - 1;

constant InterruptSourceP: MEMPOSITION :=0;
constant InterruptEnableP: MEMPOSITION :=1;
constant HptdcJtagP: MEMPOSITION :=2;
constant InputDelayP: MEMPOSITION :=3;
constant CableIdP: MEMPOSITION :=4;
constant VhdlVersionP: MEMPOSITION :=5;
constant OutputByteP: MEMPOSITION :=6;
constant StatusP: MEMPOSITION :=7;
constant CommandP: MEMPOSITION :=8;
--    CtrDrvrPll                       /* Pll parameters */
constant ErrorBP: MEMPOSITION :=9;
constant IntegratorBP: MEMPOSITION :=10;
constant DacBP: MEMPOSITION :=11;
constant LastItLenBP: MEMPOSITION :=12;
constant PhaseBP: MEMPOSITION :=13;
constant NumAverageBP: MEMPOSITION :=14;
constant KPBP: MEMPOSITION :=15;
constant KIBP: MEMPOSITION :=16;
--    ReadTime CtrDrvrCTime                /* Latched date time and CTrain */
constant Fifo0P: MEMPOSITION :=17;
--    CtrDrvrTime  TimeP
constant IndexArray0P: MEMPOSITION :=18;
constant Fifo1P: MEMPOSITION :=19;
--    SetTime CtrDrvrTime                  /* Used to set UTC if no cable */
constant IndexArray1P: MEMPOSITION :=20;

constant Refill1P :  MEMPOSITION := 21;
constant SetUpIrqP :  MEMPOSITION := 22;

constant Refill2P :  MEMPOSITION := 23;


type ADDMAPPINGType is array (0 to NUMMEMPOSITION - 1) of AddRwRecord;
constant ADDTABLE	 : ADDMAPPINGType  :=
(
InterruptSourceP =>      (AddL =>  0, AddH  => 0, Delay => 0, rw =>  cr, PosToSel => 0),
InterruptEnableP =>      (AddL =>  1, AddH  => 1, Delay => 0, rw =>  rw, PosToSel => 1),
HptdcJtagP =>      (AddL =>  2, AddH  => 2, Delay => 0, rw =>  rw, PosToSel => 2),
InputDelayP =>      (AddL =>  3, AddH  => 3, Delay => 0, rw =>  rw, PosToSel => 3),
CableIdP =>      (AddL =>  4, AddH  => 4, Delay => 0, rw =>  rw, PosToSel => 4),
VhdlVersionP =>      (AddL =>  5, AddH  => 5, Delay => 0, rw =>  rw, PosToSel => 5),
OutputByteP =>      (AddL =>  6, AddH  => 6, Delay => 0, rw =>  rw, PosToSel => 6),
StatusP =>      (AddL =>  7, AddH  => 7, Delay => 0, rw =>  rw, PosToSel => 7),
CommandP =>      (AddL =>  8, AddH  => 8, Delay => 0, rw =>  rw, PosToSel => 8),

ErrorBP =>      (AddL =>  9, AddH  => 9, Delay => 0, rw =>  rw, PosToSel => 9),
IntegratorBP =>      (AddL =>  10, AddH  => 10, Delay => 0, rw =>  rw, PosToSel => 10),
DacBP =>      (AddL =>  11, AddH  => 11, Delay => 0, rw =>  rw, PosToSel => 11),
LastItLenBP =>      (AddL =>  12, AddH  => 12, Delay => 0, rw =>  rw, PosToSel => 12),
PhaseBP =>      (AddL =>  13, AddH  => 13, Delay => 0, rw =>  rw, PosToSel => 13),
NumAverageBP =>      (AddL =>  14, AddH  => 14, Delay => 0, rw =>  rw, PosToSel => 14),
KPBP =>      (AddL =>  15, AddH  => 15, Delay => 0, rw =>  rw, PosToSel => 15),
KIBP =>      (AddL =>  16, AddH  => 16, Delay => 0, rw =>  rw, PosToSel => 16),


Fifo0P =>      (AddL =>  17, AddH  => 17+255, Delay => 1, rw =>  rw, PosToSel => Fifo0P),
IndexArray0P =>      (AddL =>17+256, AddH  => 17+256, Delay => 0, rw =>  rw, PosToSel => IndexArray0P),
Fifo1P =>      (AddL =>  17+257, AddH  => 17+257+255, Delay => 1, rw =>  rw, PosToSel => Fifo1P),
IndexArray1P =>      (AddL =>  17+257+256, AddH  => 17+257+256, Delay => 0, rw =>  rw, PosToSel => IndexArray1P),
Refill1P =>      (AddL =>  12679, AddH  => 16383, Delay => 0, rw =>  ro, PosToSel => Refill1P),

SetUpIrqP =>      (AddL =>  12678, AddH  => 12678, Delay => 0, rw =>  rw, PosToSel => SetUpIrqP),
Refill2P =>      (AddL =>  12679, AddH  => 16383, Delay => 0, rw =>  ro, PosToSel => Refill2P)

);	 

	constant  INT_COUNTER_0P : integer := 0;
	constant  INT_COUNTER_1P : integer := 1;
	constant  INT_COUNTER_2P : integer := 2;
	constant  INT_COUNTER_3P : integer := 3;
	constant  INT_COUNTER_4P : integer := 4;
	constant  INT_COUNTER_5P : integer := 5;
	constant  INT_COUNTER_6P : integer := 6;
	constant  INT_COUNTER_7P : integer := 7;
	constant  INT_COUNTER_8P : integer := 8;

	constant  INT_PLL_ITERATIONBP : integer := 9;
	constant  INT_GMTP : integer := 10;
	constant  INT_PPSP : integer := 11;
	constant  INT_MSP : integer := 12;
	constant  INT_MATCHP : integer := 13;
	constant  INT_PLLNOTLOCKEDBP : integer := 14;
	constant  INT_PLL_ITERATIONAP : integer := 15;
--	constant  INT_PLLNOTLOCKEDAP : integer := 14;
	constant  INT_CALBRATION : integer := 16;
	constant  INT_EVENTMISSED : integer := 17;

	constant RESET_ACTIVE : std_logic := '0';


  subtype IntDataType is std_logic_vector(DATALENGTH - 1 downto 0);
  subtype IntAddrOutType is std_logic_vector(ADDLENGTH - 1 downto 0);  


  type MuxDataArrType is array (0 to NUMMEMPOSITION -1) of IntDataType;

  subtype MuxSelType is std_logic_vector(NUMMEMPOSITION -1 downto 0);  
  
  
  subtype SelectedPosType is std_logic_vector(NUMMEMPOSITION - 1 downto 0);
  
  type SelRamDataType is array ( 0 to NUMMEMPOSITION - 1) of IntDataType;
 
  type ContToMemType is
  record
  		Data : IntDataType;	  -- std_logic_vector(31 downto 0);
		Add  : IntAddrOutType;  -- std_logic_vector(addtop downto 0);
		AddOffSet : IntAddrOutType;
		SelectedPos : SelectedPosType;  -- register to be accessed
		WrEn : SelectedPosType;			 -- register to be written std_logic_vector(NUMMEMPOSITION - 1 downto 0);
		RdEn : SelectedPosType;
		Wr : std_logic;
		Rd : std_logic;
  end record;

  type MemToContCellType is
  record
		Data : IntDataType;
		RdDone : std_logic;
  end record;

  type MemToContType is array (integer range <>) of MemToContCellType;
 -- type MemToContType is array (integer range 0 to NUMMEMPOSITION - 1) of MemToContCellType;

component BusIntControl 
    Port ( 
			Clk : in std_logic;
			RstN : in std_logic;
			--	SychRst : in std_logic;

			-- Interface
			IntRead : in std_logic;   -- Interface Read Signal
			IntWrite: in std_logic;    -- Interface Write Signal
			DataFromInt : in IntDataType; -- Data From interface
			IntAdd : in IntAddrOutType;   -- Address From interface 

			OpDone : out std_logic;       -- Operation Done, Read or Write Finished
			DataToInt : out IntDataType;  -- Data going from Control to the Interface

			-- Registers
--			ContToRegs : out ContToRegsType; -- Data going from Control to the Registers
														-- This consists of Data + Write Enable Siganal
--			RegsToCont : in RegsToContType;  -- Data Array From the Registers to the Control
			
			-- Memory
			ContToMem : out ContToMemType;   -- Data going from Control to the Registers
														-- This consists of Data + Enable + Read + Write

			MemToCont : in  MemToContType(0 to NUMMEMPOSITION - 1)	   -- Data Array  From the Registers to the Control
		                                  -- Data + Done
);
end component BusIntControl;
		
end IntContPackage;
package body IntContPackage is

 
end IntContPackage;
