
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
use IEEE.std_logic_arith.all;
use IEEE.STD_logic_unsigned.all;



entity sim_vme64master is
	port(
		clk_i : in STD_LOGIC; 
		
		VME_AS_n_i 		: 		out STD_LOGIC;
		VME_LWORD_n_b 	: 		inout STD_LOGIC;
		VME_RETRY_n_i 	: 		in STD_LOGIC;
		VME_WRITE_n_o	: 		out STD_LOGIC;
		VME_DS_n_o 		: 		out STD_LOGIC_VECTOR(1 downto 0);
		VME_GA_i 		: 		in STD_LOGIC_VECTOR(4 downto 0);
		VME_DTACK_n_i 	:		in STD_LOGIC;
		VME_BERR_n_i 	:		in STD_LOGIC;
		VME_ADDR_b 		: 		inout STD_LOGIC_VECTOR(31 downto 1);
		VME_DATA_b 		: 		inout STD_LOGIC_VECTOR(31 downto 0);
		VME_AM_o 		: 		out std_logic_vector(5 downto 0); 
		
		VME_DTACK_OE_i	:       in std_logic;
		VME_DATA_DIR_i	:       in std_logic;
		VME_DATA_OE_i	:       in std_logic;
		VME_ADDR_DIR_i	:       in std_logic;
		VME_ADDR_OE_i	:       in std_logic;
		
		VME_IRQ_n_i 	: 		in STD_LOGIC_VECTOR(6 downto 0);
		VME_IACKOUT_n_o : 		out STD_LOGIC
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
	
	--irq
	signal IRQ		: std_logic_vector(6 downto 0);
	signal IACK		: std_logic;  
	
	signal s_IRQ_A : std_logic := '0';
	signal s_IRQ_ADDR: std_logic_vector(31 downto 1);
	signal s_IRQ_DS : std_logic_vector (1 downto 0);  
	signal s_IRQ_AS : std_logic ;
	
	--control signals
	signal s_dataToSend 	: std_logic_vector(31 downto 0);
	signal s_dataToReceive 	: std_logic_vector(31 downto 0);
	signal s_address 		: std_logic_vector(63 downto 0);
	signal s_AM : std_logic_vector(5 downto 0);				 
	
	type t_dataToTransfer is array (0 to 255) of std_logic_vector(31 downto 0);
	signal s_dataToTransfer : t_dataToTransfer;
	
	signal s_receivedData : std_logic_vector(63 downto 0);
	--types of access
	type t_dataTransferType is  (HIGH, D08E, D08O, D16, D32, D08_BLOCK1, D08_BLOCK2, D16_BLOCK,
   D32_BLOCK, D08_RMW, D16_RMW, D32_RMW, UA0_2, UA1_3, UA1_2, D64);
   
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
			
			--present address
			ADDR <= (others => '0');
--			ADDR(18 downto 1) <= s_address(18 downto 1); 
--			ADDR(23 downto 19) <= not VME_GA_i(4 downto 0);

			ADDR <= s_address(ADDR'range); 
			
			DATA <= s_address(63 downto 32);
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
			wait for 10ns; --addressing	
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
			wait for 10 ns;
			
			--write operation
			s_write := '1';
			--address the slave
			addressPhase;
			wait for 75 ns;			
			
            DATA <= (others=>'Z');
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
		
		
		procedure readGenericBlockMBLT(numberOf: integer) is
		begin 
			wait for 100 ns;
			s_write := '1'; --read operation
			addressPhase;	--address the slave
			
			
			wait for 25 ns;			
			DS <= s_dataTransferType(3 downto 2);
			wait until DTACK = '0';
			DS <= "11";
			
			DATA <= (others => 'Z');
			ADDR <= (others => 'Z'); 
			LWORD <= 'Z';
			
			wait for 25 ns;
			
			
			for I in 0 to numberOf loop
				if DTACK = '0' then			   
					wait until  DTACK /= '0';
				end if;
				DS <= s_dataTransferType(3 downto 2);
				wait until DTACK = '0'; 
				s_dataToTransfer(I) <= VME_data_b; -- save data
				s_receivedData(63 downto 33) <= VME_ADDR_b(31 downto 1);
				s_receivedData(32) <= VME_LWORD_n_b;
				s_receivedData(31 downto 0) <= VME_DATA_b;
				report "RECEIVED MBLT DATA: " & hstr(s_receivedData);
				
				wait for 10ns;
				DS <= "11"; --rise strobe			
			end loop;
			
			terminateCycle;
		end readGenericBlockMBLT;
		
		--write single 8-32 bit value from bus. 
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



		--write single 8-32 bit value from bus. 
		--avaible address modes A16,A24,A32
		--
		--make sure to set up --s_dataTransferTypeSelect <= D32;
		--make sure to set up data to send s_dataTransferType (make sure it is aligned according to transfer type)
		procedure writeGenericConfig is 
		begin											
			wait for 20ns; --addressing	
			s_write := '0'; --it's a write op
			s_address(23 downto 19) <= not VME_GA_i(4 downto 0);
			addressPhase;			
			DS <= s_dataTransferType(3 downto 2);			
			DATA <= s_dataToSend;
			
			--assert DTACK = '1' report "DTACK should not be low here" severity error;
			wait until DTACK = '0';	--wait for ack
			wait for 20ns; --read time sim
			report "terminating";
			terminateCycle;	
		end writeGenericConfig;
		
		
		


		
		--read single 8-32 bit value from bus. 
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
			--addressPhaseMultiplexed;
			
			
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
		
		
		--read single 8-32 bit value from bus. 
		--avaible address modes A16,A24,A32
		--
		--make sure to set up --s_dataTransferTypeSelect <= D32;
		--make sure to set up s_dataTransferType (make sure it is aligned according to transfer type)
		--make sure to set up expected data (s_dataToReceive)
		
		procedure readGenericConfig(check:boolean ) is
		begin
			wait for 20ns; --addressing	
			s_write := '1';	 -- read op
		   s_address(23 downto 19) <= not VME_GA_i(4 downto 0);
			s_address(31 downto 24) <= (others => '0');
	

			addressPhase;
			--addressPhaseMultiplexed;
			
			
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
			
		end readGenericConfig;	
		
		
		
		--same as read generic single, but with address pipelining
		procedure readGenericSingleAP(check:boolean ) is
		begin
			wait for 20ns; --addressing	
			s_write := '1';	 -- read op
			addressPhase;
			--addressPhaseMultiplexed;
			
			DS <= s_dataTransferType(3 downto 2);			
			DATA <= (others => 'Z'); --input on data		
			
			wait until DTACK = '0';	--wait for data	
			s_address <= s_address +4;
			wait for 1ns;
			ADDR(31 downto 1) <= s_address(31 downto 1);
			--read data
			if check = true then 
				assert s_dataToReceive = VME_DATA_b report "did not receive expected data" severity failure;
			end if;
			s_receivedData(31 downto 0) <= VME_DATA_b;
			wait for 20ns;--simulate reading delay
			AS <= '1','0' after 25ns;	
			
			wait until DTACK = '0';	--wait for data	
			s_receivedData(31 downto 0) <= VME_DATA_b;
			wait for 20ns;--simulate reading delay
			
			terminateCycle;	
			
		end readGenericSingleAP;
		
		
		--try to configure the bus
		procedure configBus is
		begin
			s_dataTransferTypeSelect <= D08O;
			s_AM <= "101111"; --set AM to CR/CSR access
			
									
--			s_address(31 downto 0) <= x"00000623";--adem 0
--			report "reading adem";				
--			readGenericSingle(false);							
--			s_address(31 downto 0) <= x"00000627";--adem 1			
--			readGenericSingle(false);
--			s_address(31 downto 0) <= x"0000062B";--adem 2
--			readGenericSingle(false);
--			s_address(31 downto 0) <= x"0000062F";--adem 3
--			readGenericSingle(false);	
			s_address <= (others => '0');

			
			-------CONFIG ADER 0 ------
			s_address(31 downto 0) <= x"0007ff63";--ader 0-3

			s_dataToSend <= x"00000077";
			writeGenericConfig;	  			
			
			s_address(31 downto 0) <= x"0007ff67";--ader 0-2
			s_dataToSend <= x"000000f0";
			writeGenericConfig;	  			

			s_address(31 downto 0) <= x"0007ff6B";--ader 0-1
			s_dataToSend <= x"00000000";
			writeGenericConfig;	  			
			
			s_address(31 downto 0) <= x"0007ff6f";--ader 0-0
			--s_dataToSend <= x"000000e0";--a24 mblt
			--s_dataToSend <= x"00000004";--a64
			--s_dataToSend <= x"0000000C";--a64 blt
			--s_dataToSend <= x"00000000";--a64 mblt
			--s_dataToSend <= x"000000fc";--a24 blt
			--s_dataToSend <= x"00000034";--a32
			--s_dataToSend <= x"00000030";--a32 mblt 
			--s_dataToSend <= x"00000020";--a32 mblt 
			--s_dataToSend <= x"00000005";--xam a32 xam:0x01 
			--s_dataToSend <= x"00000009";--xam a64	xam:0x02
			s_dataToSend <= x"00000045";--xam a32/d64 sst xam:0x11
			--s_dataToSend <= x"00000049";--xam a64/d64 sst	xam:0x12   			
			writeGenericConfig;

			
			-------CONFIG ADER 1 ------
			s_address(31 downto 0) <= x"0007ff73";--ader 1-3
			s_dataToSend <= x"00000077";
			writeGenericConfig;	  			
			
			s_address(31 downto 0) <= x"0007ff77";--ader 1-2
			s_dataToSend <= x"000000f0";
			writeGenericConfig;	  			

			s_address(31 downto 0) <= x"0007ff7B";--ader 1-1
			s_dataToSend <= x"00000000";
			writeGenericConfig;	  			
			
			s_address(31 downto 0) <= x"0007ff7f";--ader 1-0
			--s_dataToSend <= x"000000e0";--a24 mblt
			--s_dataToSend <= x"00000004";--a64
			--s_dataToSend <= x"0000000C";--a64 blt
			--s_dataToSend <= x"00000000";--a64 mblt
			--s_dataToSend <= x"000000fc";--a24 blt
			--s_dataToSend <= x"00000034";--a32
	
			s_dataToSend <= x"00000030";--a32 mblt 
			--s_dataToSend <= x"00000020";--a32 mblt 
			--s_dataToSend <= x"00000005";--xam a32 xam:0x01 
			--s_dataToSend <= x"00000009";--xam a64	xam:0x02
			--s_dataToSend <= x"00000045";--xam a32/d64 sst xam:0x11
			--s_dataToSend <= x"00000049";--xam a64/d64 sst	xam:0x12   			
			writeGenericConfig;
			-------CONFIG ADER 2 ------
			s_address(31 downto 0) <= x"0007ff83";--ader 2-3
			s_dataToSend <= x"00000087";
			writeGenericConfig;	  			
			
			s_address(31 downto 0) <= x"0007ff87";--ader 2-2
			s_dataToSend <= x"000000f0";
			writeGenericConfig;	  			

			s_address(31 downto 0) <= x"0007ff8B";--ader 2-1
			s_dataToSend <= x"00000000";
			writeGenericConfig;	  			
			
			s_address(31 downto 0) <= x"0007ff8f";--ader 2-0
			s_dataToSend <= x"000000e0";--a24 mblt
			--s_dataToSend <= x"00000004";--a64
			--s_dataToSend <= x"0000000C";--a64 blt
			--s_dataToSend <= x"00000000";--a64 mblt
			--s_dataToSend <= x"000000fc";--a24 blt
			--s_dataToSend <= x"00000034";--a32
			--s_dataToSend <= x"00000039";--a32
			
			--s_dataToSend <= x"00000030";--a32 mblt 
			--s_dataToSend <= x"00000020";--a32 mblt 
			--s_dataToSend <= x"00000005";--xam a32 xam:0x01 
			--s_dataToSend <= x"00000009";--xam a64	xam:0x02
			--s_dataToSend <= x"00000045";--xam a32/d64 sst xam:0x11
			--s_dataToSend <= x"00000049";--xam a64/d64 sst	xam:0x12   			
			writeGenericConfig;		
			
			
			
			----CONFIG IRQ REGISTERS----
			s_address(31 downto 0) <= x"0007FBFB"; --set IRQ level
			s_dataToSend <= x"00000002"; --set it to line 2
			writeGenericConfig;	
			--readGenericSingle(false);
			
			s_address(31 downto 0) <= x"0007FBFF"; --set IRQ id
			s_dataToSend <= x"000000AB"; --set it to 0xAB
			writeGenericConfig;
			
			-----ENABLE MODULE----------------
			s_address(31 downto 0) <= x"0007fffb";
			s_dataToSend <= x"00000010"; --enable module
			writeGenericConfig;
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
		
		procedure read2e(numberOf:integer) is
		begin 
			--addressing phase 1
			WRITE <= '1';
			AM <= "100000"; --0x20 xam
			AS <='0';
			wait for 25ns;
			
			ADDR(7 downto 1) <= (others => '0');
			LWORD <= '1'; --a32/d64						
			--ADDR(1) <= '1'; --a64/d64
			--LWORD <= '0'; --a64/d64
			
			ADDR(31 downto 8) <= s_address(31 downto 8);
			DATA <= s_address(63 downto 32);
			
			DS(0) <= '0';
			wait until DTACK = '0';
			wait for 10ns;			
			--end of phase 1
			--phase 2
			
			ADDR(7 downto 1) <= s_address(7 downto 1);
			LWORD <= s_address(1);
			
			ADDR(15 downto 8) <= conv_std_logic_vector(numberOf,8); --beat count
			ADDR(31 downto 15) <= (others => '0');
			
			DATA <= (others => '0');
			
			DS(0) <= '1';
			wait until DTACK = '1';
			
			
			--end of phase 2
			--phase 3			
			
			DS(0) <= '0';
			wait until DTACK <= '0';			
			
			--end of phase 3
			-- end of addressing !!!
			
			--relaes data and address lines!
			
			LWORD <= 'Z';
			ADDR <= (others => 'Z');
			DATA <= (others => 'Z');
			wait for 10ns;
			
			
			for I in 0 to numberOf - 1 loop
				DS(1) <= '0';
				wait until DTACK <= '1';
				s_receivedData(63 downto 33) <= VME_ADDR_b(31 downto 1);
				s_receivedData(32) <= VME_LWORD_n_b;
				s_receivedData(31 downto 0) <= VME_DATA_b;
				report "RECEIVED MBLT DATA: " & hstr(s_receivedData);
				wait for 10ns; --sim reading delay
				DS(1) <= '1';
				wait until DTACK <='0'; 
				s_receivedData(63 downto 33) <= VME_ADDR_b(31 downto 1);
				s_receivedData(32) <= VME_LWORD_n_b;
				s_receivedData(31 downto 0) <= VME_DATA_b;
				report "RECEIVED MBLT DATA: " & hstr(s_receivedData);
				wait for 10ns;
			end loop;
			
			DS <= "11";
			terminateCycle;
			
		end procedure read2e;
		
		procedure read2eSST(numberOf:integer) is 
			variable dataToReceive : std_logic_vector(63 downto 0); 
			variable xam : std_logic_vector (7 downto 0);
		begin 
			--addressing phase 1
			--ADDR <= (others => '0');
			WRITE <= '1'; --read!
			AM <= "100000"; --0x20 xam
			AS <='0';
			wait for 25ns;
			
			xam := x"11"; --a32/d64
			--xam := x"12"; --a64/d64
			--xam <= 0x21 --a32/d64 broadcast
			--xam <= 0x22 --a32/d64 broadcast
			
			ADDR(7 downto 1) <= xam(7 downto 1);
			LWORD <= xam(0);
			
			ADDR(31 downto 8) <= s_address(31 downto 8);
			DATA <= s_address(63 downto 32);
			
			DS(0) <= '0';
			wait until DTACK = '0';
			wait for 10ns;			
			--end of phase 1
			--phase 2
			
			ADDR(7 downto 1) <= s_address(7 downto 1);
			LWORD <= s_address(1);
			
			ADDR(15 downto 8) <= x"0A"; --beat count
			ADDR(31 downto 15) <= (others => '0');
			
			DATA <= (others => '0');
			DATA(3 downto 0) <= x"2";
			
			DS(0) <= '1';
			wait until DTACK = '1';
			
			
			--end of phase 2
			--phase 3			
			
			DS(0) <= '0';
			wait until DTACK <= '0';			
			
			--end of phase 3
			-- end of addressing !!!
			
			--relaes data and address lines!
			
			LWORD <= 'Z';
			ADDR <= (others => 'Z');
			DATA <= (others => 'Z');
			wait for 10ns;
			
			
			
			wait for 50 ns;
			for I in 0 to (numberOf - 1) loop				
				
				DS(1) <= '0';
				wait until DTACK /= '0';
				dataToReceive(63 downto 33) := VME_ADDR_B;
				dataToReceive(32) := VME_LWORD_n_b;
				dataToReceive(31 downto 0) := VME_DATA_b;
				report "data:"  & hstr(dataToReceive);
				
				wait until DTACK /= '1';
				dataToReceive(63 downto 33) := VME_ADDR_B;
				dataToReceive(32) := VME_LWORD_n_b;
				dataToReceive(31 downto 0) := VME_DATA_b;
				report "data:"  & hstr(dataToReceive);
				
			end loop;
			DS <= "11";
			terminateCycle;		
		end procedure read2eSST;
		
		procedure write2e(numberOf:integer) is 
			variable dataToSend : std_logic_vector(63 downto 0);
		begin 
			--addressing phase 1
			WRITE <= '0'; --write!
			AM <= "100000"; --0x20 xam
			AS <='0';
			wait for 25ns;
			
			ADDR(7 downto 1) <= (others => '0');
			LWORD <= '1'; --a32/d64						
			--ADDR(1) <= '1'; --a64/d64
			--LWORD <= '0'; --a64/d64
			
			ADDR(31 downto 8) <= s_address(31 downto 8);
			DATA <= s_address(63 downto 32);
			
			DS(0) <= '0';
			wait until DTACK = '0';
			wait for 10ns;			
			--end of phase 1
			--phase 2
			
			ADDR(7 downto 1) <= s_address(7 downto 1);
			LWORD <= s_address(1);
			
			ADDR(15 downto 8) <=  conv_std_logic_vector(numberOf,8); --beat count
			ADDR(31 downto 15) <= (others => '0');
			
			DATA <= (others => '0');
			
			DS(0) <= '1';
			wait until DTACK = '1';
			
			
			--end of phase 2
			--phase 3			
			
			DS(0) <= '0';
			wait until DTACK <= '0';			
			
			--end of phase 3
			-- end of addressing !!!
			
			--relaes data and address lines!
			
			LWORD <= 'Z';
			ADDR <= (others => 'Z');
			DATA <= (others => 'Z');
			wait for 10ns;
			
			
			dataToSend := x"0123456789ABC001";
			
			for I in 0 to numberOf -1 loop				
				
				wait for 10ns;
				ADDR <= dataToSend(63 downto 33);
				LWORD <= dataToSend(32);
				DATA <= dataToSend(31 downto 0);				
				
				DS(1) <= '0';
				wait until DTACK <= '1'; --odd end
				dataToSend := dataToSend +1;
				
				wait for 10ns;
				ADDR <= dataToSend(63 downto 33);
				LWORD <= dataToSend(32);
				DATA <= dataToSend(31 downto 0);					
				
				DS(1) <= '1';
				wait until DTACK <='0';  --even end
				dataToSend := dataToSend +1;
				
			end loop;
			
			DS <= "11";
			terminateCycle;
			
		end procedure write2e; 
		
		procedure write2eSST(numberOf:integer) is 
			variable dataToSend : std_logic_vector(63 downto 0); 
			variable xam : std_logic_vector (7 downto 0);
		begin 
			--addressing phase 1
			ADDR <= (others => '0');
			WRITE <= '0'; --write!
			AM <= "100000"; --0x20 xam
			AS <='0';
			wait for 25ns;
			
			xam := x"11"; --a32/d64
			--xam := x"12"; --a64/d64
			--xam <= 0x21 --a32/d64 broadcast
			--xam <= 0x22 --a32/d64 broadcast
			
			ADDR(7 downto 1) <= xam(7 downto 1);
			LWORD <= xam(0);
			
			ADDR(31 downto 8) <= s_address(31 downto 8);
			DATA <= s_address(63 downto 32);
			
			DS(0) <= '0';
			wait until DTACK = '0';
			wait for 10ns;			
			--end of phase 1
			--phase 2
			
			ADDR(7 downto 1) <= s_address(7 downto 1);
			LWORD <= s_address(1);
			
			ADDR(15 downto 8) <= x"0A"; --beat count
			ADDR(31 downto 15) <= (others => '0');
			
			DATA <= (others => '0');
			DATA(3 downto 0) <= x"2";
			
			DS(0) <= '1';
			wait until DTACK = '1';
			
			
			--end of phase 2
			--phase 3			
			
			DS(0) <= '0';
			wait until DTACK <= '0';			
			
			--end of phase 3
			-- end of addressing !!!
			
			--relaes data and address lines!
			
			LWORD <= 'Z';
			ADDR <= (others => 'Z');
			DATA <= (others => 'Z');
			wait for 10ns;
			
			
			dataToSend := x"0123456789ABC001";
			wait for 50 ns;
			for I in 0 to 4 loop				
				
				--wait for 12ns;
				ADDR <= dataToSend(63 downto 33);
				LWORD <= dataToSend(32);
				DATA <= dataToSend(31 downto 0);				
				
				wait for 12ns;
				DS(1) <= '0';
				wait for 12ns;
				
				dataToSend := dataToSend +1;
				
				ADDR <= dataToSend(63 downto 33);
				LWORD <= dataToSend(32);
				DATA <= dataToSend(31 downto 0);					
				wait for 12ns;
				DS(1) <= '1';
				wait for 12ns;
				dataToSend := dataToSend +1;
				
			end loop;
			
			DS <= "11";
			terminateCycle;
			
		end procedure write2eSST;
	
		
		----------------------------------------------------
		--------------TEST PROCEDURES START-----------------
		----------------------------------------------------

		
		begin
		--execute
		wait for 100 us;
		
		configBus;
		wait for 200 ns;
		--s_AM <= "111101"; --A24 data access blt
		--s_AM <= "111111"; --A24 data access 
--		s_AM <= "111001";
		--s_AM <= "000001"; --A64 data access 
		--s_AM <= "000011"; --A64 data access blt
		--s_AM <= "000000"; --A64 data access mblt
		--s_AM <= "001101"; --A32 data access
		s_AM <= "001100"; --A32 data access	mblt
		--s_AM <= "001000"; --A32 data access	mblt
		s_dataTransferTypeSelect <= D32;
        
		
		s_address(63 downto 0) <= (others => '0');
		wait for 10 ns;
		s_address(31 downto 0) <= x"87f00004";--x"0003fffd"; 
		
		--s_address <= x"00ff00ff77f00004";--x"0003fffd"; 
		s_dataToSend <= x"00001110";
		wait for 500 ns;

--
--		---2eVME test suite ----
--        write2e(5);  
--		wait for 200 ns;
--		read2e(5);   
--        wait;
--		--------------------------
    		
--        write2eSST(5);
--		wait for 200ns;
--		read2eSST(5);
--		wait;  

 --     readGenericBlock(5);
      writeGenericBlock(5);
      wait for 200ns;
      readGenericBlockMBLT(5);
      wait;

--		writeGenericBlock(5);	
--		
--		
--		--read2eSST(10);
--		--write2eSST(10);
--		wait for 30 ns;
--		write2eSST(10);		
--		wait for 30 ns;
--		write2eSST(10);
----		read2e(100);
--		wait;
--		
--		
--		wait;
--		
--		readGenericSingleAP(false);
--		s_address(31 downto 0) <= x"77f00004";--x"0003fffd"; 
--		s_dataToSend <= x"00001110";
--      wait for 100 ns;
--		writeGenericSingle;
--
--
--
--		s_address(31 downto 0) <= x"77f00009";--x"0003fffd"; 
--		s_dataToSend <= x"00001310";
--      wait for 100 ns;
--		writeGenericSingle;
--
--		s_address(31 downto 0) <= x"77f00004";--x"0003fffd"; 
--		s_dataToSend <= x"00001110";
--
--		readGenericSingle(false);		
--	
--
--		s_address(31 downto 0) <= x"77f00009";--x"0003fffd"; 
--		s_dataToSend <= x"00001310";
--
--		readGenericSingle(false);		
	
--		readGenericBlock(5);
--		
--		readGenericSingle(false);		
--		wait;
--		--s_AM <= "111111"; --A24 data access blt
--		s_address(32 downto 1) <= x"00f0000f";--x"0003fffd";			
--		--readGenericBlock(5);
--		writeGenericBlock(5);
		
		
	end process;
	
	
	
	
--	IRQ_ACK: process                -- NOTE: commented out by zkroflic
--	begin          
--		IACK <= '1';
--		wait until IRQ(1) = '0';
--		--wait for 3050ns;
--		report "INTERRUPT TRIGGERED";  
--		s_IRQ_A <= '1';
--		s_IRQ_DS <= "11";			  
--		s_IRQ_AS <= '1';
--		s_IRQ_ADDR <= (others => '0');
--		wait for 1ns;
--		s_IRQ_ADDR(3 downto 1) <= "010";
--		IACK <= '0'; 
--		s_IRQ_AS <= '0';
--		--s_IRQ_DS <= "10";
--		wait for 10 ns;
--		s_IRQ_DS <= "00";
--		wait until DTACK = '0';
--		s_IRQ_A <= '0';	
--	end process;
	
	
	
	
	
	-----------------------------------
	---------CONNECTIONS---------------
	-----------------------------------
	VME_AS_n_i	<=s_IRQ_AS when s_IRQ_A = '1' else  AS;
	VME_LWORD_n_b 	<= LWORD;
	--LWORD <= VME_LWORD_n_b;
	RETRY <= VME_RETRY_n_i;
	VME_WRITE_n_o	<= WRITE;
	VME_DS_n_o 		<= s_IRQ_DS when S_IRQ_A = '1' else DS;
	--VME_GA_i 		: 		in STD_LOGIC_VECTOR(4 downto 0);
	DTACK <= VME_DTACK_n_i;
	BERR <= VME_BERR_n_i;
	
	VME_ADDR_b 		<= s_IRQ_ADDR when S_IRQ_A= '1' else ADDR;
	--ADDR <= VME_ADDR_b;
	VME_DATA_b 		<= DATA;
	--DATA <= VME_DATA_b;
	VME_AM_o 		<= AM;
	
	IRQ <= VME_IRQ_n_i;
	VME_IACKOUT_n_o <= IACK;
	
	
	
	
latchIcDtackCheck: process
	begin
		wait until DTACK = '0';
		assert VME_DTACK_OE_i = '1' REPORT "DTACK is '0' but LATCH ic is NOT enabled" severity failure; 
	end process;

--assert VME_DTACK_OE_i = '1' and DTACK /= 'Z' report "DTACK is '0' but LATCH ic is NOT enabled" severity failure; 
	
--	latchIcDtackCheck1: process
--	begin
--		wait until VME_DTACK_OE_i = '1';
--		wait for 10ns;
--		assert DTACK = '0' REPORT "DTACK is  not '0' but LATCH ic is enabled" severity failure; 
--	end process;
--	
	
	
	
	
	--    VME_DATA_DIR_i	:       in std_logic;
	--    VME_DATA_OE_i	:       in std_logic;
	--    VME_ADDR_DIR_i	:       in std_logic;
	--    VME_ADDR_OE_i	:       in std_logic;
	
end sim_vme64master;