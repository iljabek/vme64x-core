
-------------------------------------------------------------------------------
--
-- Title       : sim_vme64master
-- Design      : vme64tb
-- Author      : tslejko
-- Company     : 
--
-------------------------------------------------------------------------------
--
-- File        : sim_vme64master.vhd
-- Generated   : Wed Mar 17 10:07:09 2010
-- From        : interface description file
-- By          : Itf2Vhdl ver. 1.20
--
-------------------------------------------------------------------------------
--
-- Description : 
--
-------------------------------------------------------------------------------

--{{ Section below this comment is automatically maintained
--   and may be overwritten
--{entity {sim_vme64master} architecture {sim_vme64master}}

library IEEE;
use IEEE.STD_LOGIC_1164.all;		



entity sim_vme64master is
	port(
		clk_i : in STD_LOGIC; 
		
		VME_AS_n_i 		: 		out STD_LOGIC;
		VME_LWORD_n_b 	: 		inout STD_LOGIC;
		VME_RETRY_n_i 	: 		in STD_LOGIC;
		VME_WRITE_n_o	: 		out STD_LOGIC;
		VME_DS_n_o 		: 		out STD_LOGIC_VECTOR(1 downto 0);
		--VME_GA_i 		: 		in STD_LOGIC_VECTOR(4 downto 0);
		VME_DTACK_n_i 	:		in STD_LOGIC;
		VME_BERR_n_i 	:		in STD_LOGIC;
		VME_ADDR_b 		: 		inout STD_LOGIC_VECTOR(31 downto 1);
		VME_DATA_b 		: 		inout STD_LOGIC_VECTOR(31 downto 0);
		VME_AM_o 		: 		out std_logic_vector(5 downto 0)
		);
end sim_vme64master;

--}} End of automatically maintained section



architecture sim_vme64master of sim_vme64master is 
	
	signal AS 		: std_logic := '1';
	signal LWORD 	: std_logic := '1';
	signal RETRY 	: std_logic := '1';
	signal WRITE	: std_logic := '1';
	signal DS		: std_logic_vector(1 downto 0) := "11";
	signal DTACK	: std_logic := '1';
	signal BERR		: std_logic := '1';
	signal ADDR		: std_logic_vector(31 downto 1);
	signal DATA		: std_logic_vector(31 downto 0);
	signal AM		: std_logic_vector(5 downto 0);	
	
	--control signals
	signal s_dataToSend 	: std_logic_vector(31 downto 0);
	signal s_dataToReceive 	: std_logic_vector(31 downto 0);
	signal s_address 		: std_logic_vector(63 downto 0);
	signal s_AM : std_logic_vector(5 downto 0);				 
	
	
	type t_dataToTransfer is array (0 to 255) of std_logic_vector(31 downto 0);
	signal s_dataToTransfer : t_dataToTransfer;
	
	signal s_receivedData : std_logic_vector(63 downto 0);
	--types of access
	type t_dataTransferType is  (HIGH, D08E, D08O, D16, D32, D08_BLOCK1, D08_BLOCK2, D16_BLOCK, D32_BLOCK, D08_RMW, D16_RMW, D32_RMW, UA0_2, UA1_3, UA1_2, D64);
	signal s_dataTransferTypeSelect : t_dataTransferType;
	signal s_dataTransferType : std_logic_vector(3 downto 0);
	--signal s_write : std_logic;
	
	  -- converts a std_logic_vector into a hex string.
   function hstr(slv: std_logic_vector) return string is
       variable hexlen: integer;
       variable longslv : std_logic_vector(67 downto 0) := (others => '0');
       variable hex : string(1 to 16);
       variable fourbit : std_logic_vector(3 downto 0);
     begin
       hexlen := (slv'left+1)/4;
       if (slv'left+1) mod 4 /= 0 then
         hexlen := hexlen + 1;
       end if;
       longslv(slv'left downto 0) := slv;
       for i in (hexlen -1) downto 0 loop
         fourbit := longslv(((i*4)+3) downto (i*4));
         case fourbit is
           when "0000" => hex(hexlen -I) := '0';
           when "0001" => hex(hexlen -I) := '1';
           when "0010" => hex(hexlen -I) := '2';
           when "0011" => hex(hexlen -I) := '3';
           when "0100" => hex(hexlen -I) := '4';
           when "0101" => hex(hexlen -I) := '5';
           when "0110" => hex(hexlen -I) := '6';
           when "0111" => hex(hexlen -I) := '7';
           when "1000" => hex(hexlen -I) := '8';
           when "1001" => hex(hexlen -I) := '9';
           when "1010" => hex(hexlen -I) := 'A';
           when "1011" => hex(hexlen -I) := 'B';
           when "1100" => hex(hexlen -I) := 'C';
           when "1101" => hex(hexlen -I) := 'D';
           when "1110" => hex(hexlen -I) := 'E';
           when "1111" => hex(hexlen -I) := 'F';
           when "ZZZZ" => hex(hexlen -I) := 'z';
           when "UUUU" => hex(hexlen -I) := 'u';
           when "XXXX" => hex(hexlen -I) := 'x';
           when others => hex(hexlen -I) := '?';
         end case;
       end loop;
       return hex(1 to hexlen);
     end hstr;
	
    function chr(sl: std_logic) return character is
    variable c: character;
    begin
      case sl is
         when 'U' => c:= 'U';
         when 'X' => c:= 'X';
         when '0' => c:= '0';
         when '1' => c:= '1';
         when 'Z' => c:= 'Z';
         when 'W' => c:= 'W';
         when 'L' => c:= 'L';
         when 'H' => c:= 'H';
         when '-' => c:= '-';
      end case;
    return c;
   end chr;
	
	
   function str(slv: std_logic_vector) return string is
     variable result : string (1 to slv'length);
     variable r : integer;
   begin
     r := 1;
     for i in slv'range loop
        result(r) := chr(slv(i));
        r := r + 1;
     end loop;
     return result;
   end str;

	
begin
	
	
	
	--data transfer type select
	with s_dataTransferTypeSelect select --DS1 DS0 A1 LWORD
	s_dataTransferType <= 	
	"1111" when HIGH,
	"0101" when D08E,
	"1011" when D08O,
	"0001" when D16,
	"0000" when D32,
	"0111" when D08_BLOCK1,
	"1011" when D08_BLOCK2,
	"0001" when D16_BLOCK, --also possible "0011" page 36 note 2
	"0000" when D32_BLOCK,
	--add RMW
	"0100" when UA0_2,
	"1000" when UA1_3,
	"0010" when UA1_2,
	"0000" when D64,
	
	"1110" when others;
	
	--assert s_dataTransferType = "1110" report "invalid data transfer type selection" severity failure;
	-----------------------------------
	---------RULES---------------------
	-----------------------------------
	assert not ((DS(1)='1' and DS(0) = '0' and ADDR(1)='1' and LWORD = '0') or 
	(DS(1)='0' and DS(0) = '1' and ADDR(1)='1' and LWORD = '0')) 
	report "violation of rule 2.1 by master" severity error;
	
	
	--	assert
	--	AM=x"36" or
	--	AM=x"33" or
	--	AM=x"31" or
	--	AM=x"30" or
	--	AM=x"2E" or
	--	AM=x"2B" or
	--	AM=x"2A" or
	--	AM=x"28" or
	--	AM=x"27" or
	--	AM=x"26" or
	--	AM=x"25" or
	--	AM=x"24" or
	--	AM=x"23" or
	--	AM=x"22" or
	--	AM=x"21" or
	--	AM=x"20"
	--	
	--	report "AM reserved selection" severity error;
	
	
	-----------------------------------
	---------ERRORS---------------------
	-----------------------------------
	assert not BERR = '0' report "bus error line driven low" severity error;
	assert not RETRY = '0' report "RETRY line driven low" severity error;
	
	
	-----------------------------------
	---------TESTS---------------------
	-----------------------------------
	
	stimuli: process 
		variable s_write : std_logic;
		
		--use procedure testReservedAms to check wether slave responds to illigal AM codes	
		procedure testReservedAm is
		begin 
			--present address
			ADDR(31 downto 1) <= (others => '0');
			
			ADDR(1) <= s_dataTransferType(1); --set A1
			LWORD <= s_dataTransferType(0); --set LWORD
			--present address modifier
			AM <= s_AM;
			AS <= '0';
			--give slave time to decode
			wait for 175 ns; 
			assert DTACK = '1' report "slave should not respond to invalid AM" severity error;
			DS <= "00";
			wait for 175 ns;
			assert DTACK = '1' report "slave should not respond to invalid AM" severity error;
			AS <= '1';
			DS <= "11";
			wait for 75ns;
		end testReservedAm;
		
		procedure testReservedAms is
			--TODO check with valid
		begin
			s_AM <= "110110"; --x36
			testReservedAm;
			s_AM <= "110011"; --x33
			testReservedAm;
			s_AM <= "110001"; --x31
			testReservedAm;
			s_AM <= "110000"; --x30
			testReservedAm;
			s_AM <= "101110"; --2e
			testReservedAm;
			s_AM <= "101011"; --2B
			testReservedAm;
			s_AM <= "101010"; --2A
			testReservedAm;
			s_AM <= "101000"; --28
			testReservedAm;
			s_AM <= "100111"; --27
			testReservedAm;
			s_AM <= "000111"; --7
			testReservedAm;
			s_AM <= "000110"; --6
			testReservedAm;
			s_AM <= "100010"; --3
		end testReservedAms;
		
		
		--non multiplexed addressPhase (for use with A16, A24, A32)
		--make sure you set up s_AM (address modifier lines) before calling this procedure
		--make sure you set up desired address (s_address) before calling this procedure
		--		make sure to align address properly to reflect AM modes
		procedure addressPhase is
		begin
			assert not (s_AM(5 downto 3) = "110"  or  s_AM(5 downto 3) = "000") report "only non multiplexed address modes" severity error;
			--present address
			ADDR(31 downto 1) <= s_address(31 downto 1);
			--present address modifier
			AM <= s_AM;						
			--ADDR(1) <= s_address(0);--s_dataTransferType(1); --set A1
			LWORD <= s_dataTransferType(0); --set LWORD	 
			wait for 10ns;
			AS <= '0';
			WRITE <= s_write;
			--give slave time to decode
			wait for 75 ns; 
			--wait for DTACK			
			if DTACK = '0' then
				wait until DTACK = '1';
			end if;
		end addressPhase;	
		
		--multiplexed addressPhase (for use with A40, A64)
		--make sure you set up s_AM (address modifier lines) before calling this procedure
		--make sure you set up desired address (s_address) before calling this procedure
		--		make sure to align address properly to reflect AM modes
		procedure addressPhaseMultiplexed is
		begin 
			assert (s_AM(5 downto 3) = "110"  or  s_AM(5 downto 3) = "000") report "only multiplexed address modes" severity error;
			--presnet the address
			ADDR(31 downto 1) <= s_address(31 downto 1);
			DATA <= s_address(63 downto 32);
			--present address modifiers
			AM <= s_AM;						
			--address strobe
			AS <= '0';
			--specifiy the most siginificant address and data direction
			WRITE <= s_write;
			DS <= s_dataTransferType(3 downto 2);
			--slave process data
			if DTACK = '1' then
				wait until DTACK = '0';
			end if;
			--terminate address brodcast phase
			DS <= "11";
			if DTACK = '0' then
				wait until DTACK = '1';
			end if;
		end addressPhaseMultiplexed;
		
		--terminate cycle procedure, use this on end of every cycle
		procedure terminateCycle is
		begin
			ADDR <= (others => 'Z');
			AM <= (others => '1');
			DATA <= (others => 'Z');
			DS <= "11";
			LWORD <= 'Z'; 
			--if DTACK = '0' then
			--report "waiting";
			--wait until not DTACK = '0';
			--end if;
			--end of termination
			WRITE <= '1';
			AS <= '1';		
		end terminateCycle;
		
		--read single 32 bit value from bus. this procedure uses non multiplexed addressing
		--avaible address modes A16,A24,A32
		--
		--make sure to set up s_dataToReceive to expected results
		--make sure to set up s_AM (address modifier lines) and s_address (check addressPhase)		
		procedure readD32single is
		begin 
			--d32 read opeartion
			s_dataTransferTypeSelect <= D32;
			wait for 100ns; --addressing	
			s_write := '1';
			addressPhase;
			
			DS <= s_dataTransferType(3 downto 2);			
			DATA <= (others => 'Z'); --input on data
			
			assert DTACK = '0' report "DTACK should not be low here" severity error;
			wait until DTACK = '0';	--wait for data			 			
			--recive data
			assert s_dataToReceive = DATA report "did not receive expected data" severity error;
			wait for 75ns;
			terminateCycle;		 
		end readD32single;
		
		--write single 32 bit value from bus. this procedure uses non multiplexed addressing
		--avaible address modes A16,A24,A32
		--
		--make sure to set up s_dataToReceive to expected results
		--make sure to set up s_AM (address modifier lines) and s_address (check addressPhase)		
		
		procedure writeD32single is
		begin 
			--d32 write opeartion
			s_dataTransferTypeSelect <= D32;
			wait for 100ns;
			s_write := '0';
			addressPhase;			
			
			DATA <= s_dataToSend;			
			DS <= s_dataTransferType(3 downto 2);
			wait until DTACK = '0'; 
			--terminate cycle
			ADDR <= (others => 'Z');
			AM <= (others => '1');
			DATA <= (others => 'Z');
			DS <= "11";
			LWORD <= 'Z';
			wait until DTACK = '1';
			--end of termination
			WRITE <= '1';
			AS <= '1';		
			
		end writeD32single;
		
		procedure writeD32singleMA is
		begin 
			--d32 write opeartion
			s_dataTransferTypeSelect <= D32;
			wait for 100ns;
			--address the slave
			
			addressPhase;	
			wait for 75ns; 			
			WRITE <= '0'; --write operation 			
			DS <= s_dataTransferType(3 downto 2);			
			wait until DTACK = '0'; --ack address
			--terminate address brodcast phase
			DS <= "11";
			wait until DTACK = '1';
			--start data phase
			DATA <= s_dataToSend;
			DS <= s_dataTransferType(3 downto 2);
			wait until DTACK = '0'; 
			--terminate cycle
			ADDR <= (others => 'Z');
			AM <= (others => '1');
			DATA <= (others => 'Z');
			DS <= "11";	   
			LWORD <= 'Z';
			wait until DTACK = '1';
			--end of termination
			WRITE <= '1';
			AS <= '1';		
			
		end writeD32singleMA;
		
		
		
		procedure writeGenericBlock(numberOf: integer) is
		begin 
			wait for 100 ns;
			addressPhase;			
			--write operation
			s_write := '0';
			--address the slave
			addressPhase;
			wait for 75 ns;			
			
			for I in 0 to numberOf loop
				if DTACK = '0' then
					wait until DTACK /= '0';
				end if;
				DATA <= s_dataToTransfer(I);
				DS <= s_dataTransferType(3 downto 2);
				wait until DTACK = '0'; 
				wait for 10ns;
				DS <= "11"; --rise strobe			
				
			end loop;
			
			terminateCycle;
		end writeGenericBlock; 
		
		
		procedure readGenericBlock(numberOf: integer) is
		begin 
			wait for 100 ns;
			addressPhase;			
			--write operation
			s_write := '1';
			--address the slave
			addressPhase;
			wait for 75 ns;			
			
			for I in 0 to numberOf loop
				if DTACK = '0' then			   
					wait until  DTACK /= '0';
				end if;
				DS <= s_dataTransferType(3 downto 2);
				wait until DTACK = '0'; 
				s_dataToTransfer(I) <= VME_data_b; -- save data
				wait for 10ns;
				DS <= "11"; --rise strobe			
				
			end loop;
			
			terminateCycle;
		end readGenericBlock;
		
		--write single 8-32 bit value from bus. this procedure uses non multiplexed addressing
		--avaible address modes A16,A24,A32
		--
		--make sure to set up --s_dataTransferTypeSelect <= D32;
		--make sure to set up data to send s_dataTransferType (make sure it is aligned according to transfer type)
		procedure writeGenericSingle is 
		begin											
			wait for 20ns; --addressing	
			s_write := '0'; --it's a write op
			addressPhase;			
			DS <= s_dataTransferType(3 downto 2);			
			DATA <= s_dataToSend;						
			
			--assert DTACK = '1' report "DTACK should not be low here" severity error;
			wait until DTACK = '0';	--wait for ack
			wait for 20ns; --read time sim
			report "terminating";
			terminateCycle;	
		end writeGenericSingle;
		
		--read single 8-32 bit value from bus. this procedure uses non multiplexed addressing
		--avaible address modes A16,A24,A32
		--
		--make sure to set up --s_dataTransferTypeSelect <= D32;
		--make sure to set up s_dataTransferType (make sure it is aligned according to transfer type)
		--make sure to set up expected data (s_dataToReceive)
		
		procedure readGenericSingle(check:boolean ) is
		begin
			wait for 20ns; --addressing	
			s_write := '1';	 -- read op
			addressPhase;
			
			
			DS <= s_dataTransferType(3 downto 2);			
			DATA <= (others => 'Z'); --input on data		
			
			wait until DTACK = '0';	--wait for data			 			
			--read data
			if check = true then 
				assert s_dataToReceive = VME_DATA_b report "did not receive expected data" severity failure;
			end if;
			s_receivedData(31 downto 0) <= VME_DATA_b;
			wait for 20ns;--simulate reading delay
			
			terminateCycle;	
			
		end readGenericSingle;
		
		
		--try to configure the bus
		procedure configBus is
		begin
			s_dataTransferTypeSelect <= D08O;
			s_AM <= "101111"; --set AM to CR/CSR access
			
--			s_address(32 downto 1) <= x"00000081";
--			readGenericSingle(false);
--			s_address(32 downto 1) <= x"00000123";
--			readGenericSingle(false);
--			
			s_address(31 downto 0) <= x"00000623";--adem 0
			report "reading adem";
			--readGenericBlock(4);
			
			readGenericSingle(false);							
			s_address(31 downto 0) <= x"00000627";--adem 1			
			readGenericSingle(false);
			s_address(31 downto 0) <= x"0000062B";--adem 2
			readGenericSingle(false);
			s_address(31 downto 0) <= x"0000062F";--adem 3
			readGenericSingle(false);	
--			
			s_address(31 downto 0) <= x"0007ff63";--ader 0
			s_dataToSend <= x"00000007";
			writeGenericSingle;	  			
															 			
			s_address(31 downto 0) <= x"0007ff67";--ader 0
			s_dataToSend <= x"000000f0";
			writeGenericSingle;	  			
			
			s_address(31 downto 0) <= x"0007ff6f";--ader 3
			s_dataToSend <= x"000000f4";
			--s_dataToSend <= x"00000034";
			writeGenericSingle;
			--s_address(31 downto 0) <= x"0007ff63";--ader 0
			
			--s_dataToReceive <= s_dataToSend;
			--s_dataToReceive(31 downto 8) <= (others => 'Z');
			--readGenericSingle(true);
			
			s_address(31 downto 0) <= x"0007fffb";
			s_dataToSend <= x"00000010"; --enable module
			writeGenericSingle;
			s_dataToReceive <= s_dataToSend;
			s_dataToReceive(31 downto 8) <= (others => 'Z');
			--readGenericSingle(true);
			
			report "end configuration";
		end procedure;
		
		procedure testCram is
		use IEEE.STD_LOGIC_1164.all;
		use IEEE.STD_LOGIC_UNSIGNED.all;
		variable BEG_CRAM:std_logic_vector(23 downto 0);
		variable END_CRAM:std_logic_vector(23 downto 0);
		begin
			s_AM <= "101111"; --set AM to CR/CSR access
			s_dataTransferTypeSelect <= D08O;
			
			--read BEG_CRAM
			s_address(31 downto 0) <= x"0000009b";		
			readGenericSingle(false);
			BEG_CRAM(23 downto 16) := s_receivedData(7 downto 0);
			s_address(31 downto 0) <= x"0000009f";		
			readGenericSingle(false);
			BEG_CRAM(15 downto 8) := s_receivedData(7 downto 0);
			s_address(31 downto 0) <= x"000000a3";		
			readGenericSingle(false);					
			BEG_CRAM(7 downto 0) := s_receivedData(7 downto 0);
			report "BEG_CRAM value is " & hstr(BEG_CRAM); 			
			--read END_CRAM
			s_address(31 downto 0) <= x"000000a7";		
			readGenericSingle(false);
			END_CRAM(23 downto 16) := s_receivedData(7 downto 0);
			s_address(31 downto 0) <= x"000000ab";		
			readGenericSingle(false);
			END_CRAM(15 downto 8) := s_receivedData(7 downto 0);
			s_address(31 downto 0) <= x"000000af";		
			readGenericSingle(false);					
			END_CRAM(7 downto 0) := s_receivedData(7 downto 0);
			
			report "END_CRAM value is " & hstr(END_CRAM); 			
			
			--read CRAM data width
			s_address(32 downto 1) <= x"0000007f";		
			readGenericSingle(false);
			report "CRAM data width" & hstr(s_receivedData(7 downto 0));
			
			wait for 200 ns;
			--TEST no1: writing to CRAM
			s_AM <= "101111"; --set AM to CR/CSR access	
			
									  				 
			--BEG_CRAM := BEG_CRAM -1; --must fail!!! uncomment to test
			s_address(23 downto 0) <= BEG_CRAM;
			
			s_dataToReceive <= (others => 'Z'); --check if ram is empty
			s_dataToReceive(7 downto 0) <= x"00";
			readGenericSingle(true);
			
			s_dataToSend <= x"00000012";		
			writeGenericSingle;			-- write to ram
			s_dataToReceive(7 downto 0) <= x"12";
			readGenericSingle(true);
			
			
			
			--END_CRAM := END_CRAM+4; -- must fail 		
			s_address(23 downto 0) <= END_CRAM;						
			s_dataToReceive(7 downto 0) <= x"00";
			readGenericSingle(false);
			readGenericSingle(true);			
			
			s_dataToSend <= x"00000010";		
			writeGenericSingle;		
			
			s_dataToReceive(7 downto 0) <= x"10";
			readGenericSingle(true); --check if correct data is received						
			
		end procedure;
		
	begin
		--execute
		wait for 70 ns;
		
		configBus;
		wait for 200 ns;
		s_AM <= "111101"; --A24 data access blt
		--s_AM <= "001101"; --A32 data access
		s_dataTransferTypeSelect <= D32_BLOCK;
		
		
		s_address(31 downto 0) <= x"00f00004";--x"0003fffd";			
		s_dataToSend <= x"00001110";
		writeGenericSingle;
		readGenericSingle(false);		
		wait;
		--s_AM <= "111111"; --A24 data access blt
		s_address(32 downto 1) <= x"07000020";--x"0003fffd";			
		--readGenericBlock(5);
		writeGenericBlock(5);
		
		wait;
		
		s_AM <= "101111"; --set AM to CR/CSR access
		s_dataTransferTypeSelect <= D08O;
		
		s_address(31 downto 0) <= x"0007fffb";
		readGenericSingle(false);
		report "received data" & hstr(s_receivedData);
		s_dataToSend(7 downto 0) <= "00010000"; --enable module
		writeGenericSingle;	
	
		readGenericSingle(false);
		report "received data" & hstr(s_receivedData);
		wait;
		
		--testCram;
		--configBus;		
		
		report "accessing data";
		--s_AM <= "111101"; --A24 data access
		s_AM <= "001101"; --A32 data access
		s_dataTransferTypeSelect <= D32;
		s_address(32 downto 1) <= x"7F800000";--x"0003fffd";			
		s_dataToSend <= x"00000010";
		readGenericSingle(false);
		
		wait;
		
		writeGenericBlock(5);
		readGenericSingle(false);
		writeGenericSingle;
		wait;
		
		testReservedAms;
		readD32single;
		wait;
		
		
		wait;
		
		
	end process;  
	
	
	
	
	--	 
	
	----process used for simulating slave block write
	--		dtackproc: process
	--		begin 
	--			DTACK <= '1';	
	--			wait for 100 ns;
	--			wait until AS = '0';
	--			
	--			loop
	--			wait until DS = "00";
	--			wait for 75 ns;		
	--			--write data
	--			DTACK <= '0';
	--			wait until DS = "11";
	--			wait for 25ns;
	--			DTACK <= '1';  
	--			end loop;
	--			
	--			wait until DS = "11";
	--			DTACK <= '1';
	--			--DATA <= (others => 'Z');
	--		end process;
	
	
	
	--	dtackproc: process
	--	begin 	
	--		loop
	--			DATA <= (others => 'Z');
	--			DTACK <= '1';	
	--			wait for 10 ns;
	--			wait until AS = '0';
	--			--end of address
	--			
	--			loop 
	--				--wait for data
	--				if not DS="00" then wait until DS = "00"; end if;
	--				if(WRITE = '0') then
	--					report "new data";
	--				else
	--					DATA <= x"01234567";
	--				end if;
	--				wait for 75 ns; --simulate reading/writing
	--				--send ack
	--				DTACK <= '0';
	--				if not DS="11"	then wait until DS = "11";  end if;
	--				wait for 25ns;
	--				DTACK <= '1';
	--				wait for 10ns;
	--				
	--				if AS='1' then 	 
	--					report "exiting";
	--					exit;
	--				end if;
	--				
	--			end loop;
	--		end loop;
	--		
	--	end process;
	
	
	
	
	
	
	
	
	
	-----------------------------------
	---------CONNECTIONS---------------
	-----------------------------------
	VME_AS_n_i	<= AS;
	VME_LWORD_n_b 	<= LWORD;
	--LWORD <= VME_LWORD_n_b;
	RETRY <= VME_RETRY_n_i;
	VME_WRITE_n_o	<= WRITE;
	VME_DS_n_o 		<= DS;
	--VME_GA_i 		: 		in STD_LOGIC_VECTOR(4 downto 0);
	DTACK <= VME_DTACK_n_i;
	BERR <= VME_BERR_n_i;
	
	VME_ADDR_b 		<= ADDR;
	--ADDR <= VME_ADDR_b;
	VME_DATA_b 		<= DATA;
	--DATA <= VME_DATA_b;
	VME_AM_o 		<= AM;
	
	
	
end sim_vme64master;

