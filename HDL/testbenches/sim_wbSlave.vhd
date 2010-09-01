-------------------------------------------------------------------------------
--
-- Title       : sim_wbSlave
-- Design      : VME64xCore
-- Author      : tslejko
-- Company     : CSL
--
-------------------------------------------------------------------------------
--
-- File        : C:\Users\tom\CSL\vme64\FAIR-VME64ext\trunk\HDL\testbenches\sim_wbSlave.vhd
-- Generated   : Fri Apr  2 10:47:37 2010
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
--{entity {sim_wbSlave} architecture {sim_wbSlave}}

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_UNSIGNED.all;

entity sim_wbSlave is
	port(
		clk_i : in STD_LOGIC;
		RST_i: 	in std_logic;
		DAT_i: 	in std_logic_vector(63 downto 0);
		DAT_o: 	out std_logic_vector(63 downto 0);
		ADR_i: 	in std_logic_vector(63 downto 0);
		--TGA_i: 	in std_logic_vector(3 downto 0);
		--TGC_i: 	in std_logic_vector(3 downto 0);
		CYC_i: 	in std_logic;
		ERR_o: 	out std_logic;
		LOCK_i: in std_logic;
		RTY_o: 	out std_logic;
		SEL_i: 	in  std_logic_vector(7 downto 0);
		STB_i: 	in std_logic;
		ACK_o: 	out std_logic := '0';
		WE_i: 	in std_logic;
		STALL_o : out std_logic;
		IRQ_o: 	out std_logic
		);
end sim_wbSlave;



architecture sim_wbSlave of sim_wbSlave is
type t_ram is array (0 to 1024) of std_logic_vector(63 downto 0);
signal s_ram : t_ram; 

signal s_selectedData : std_logic_vector(63 downto 0);

signal s_stb_count : integer := 0;	 
signal s_sendCount : std_logic_vector(3 downto 0) := "0000";

begin
	

	
ERR_o <= '0';
RTY_o <= '0';


--ACK_o <= '1' when STB_i = '1';

fakeInterruot: process
begin
	IRQ_o <= '0';
	wait for 3500ns;
	IRQ_o <= '1','0' after 50 ns;
	wait;
end process;

fakeReadWrite: process

begin
	--ACK_o <= '0';
	--if(CYC_i = '0') then wait until CYC_i = '1'; end if;
	
	if STB_i /= '1' then wait until STB_i = '1'; end if;
	wait until clk_i = '1';
	
	if(WE_i = '1') then
		s_ram(CONV_INTEGER(ADR_i(9 downto 0))) <= DAT_i;
		report "saving data";
	else 
		--DAT_o <= s_ram(CONV_INTEGER(ADR_i(9 downto 0)));
		
	end if;	 	
	
	--wait for 10ns;
	--ACK_o <= '1', '0' after 1ns;	
end process;  

DAT_o(63 downto 4) <= x"012345670123456";  
DAT_o(3 downto 0) <= s_sendCount;

fakeAckReply: process
variable ack_counter : integer := 0;
begin 
	if(ack_counter /= s_stb_count) then
		ACK_o <= '1', '0' after 1ns;	
		ack_counter := ack_counter +1 ;	
		s_sendCount <= s_sendCount +1;
	end if;
	wait for 30ns;
end process;

fakeSTALL: process
begin
	STALL_o <= '0';
	if s_stb_count = 5 then
		--STALL_o <= '1','0' after 50ns; ---uncomment to test if stall works..
		wait for 50ns;
	end if;
	wait for 1ns;
		
end process;
		
						
stbCounter: process(clk_i)
begin
	if rising_edge(clk_i) then
		if(STB_i = '1') then
		s_stb_count <= s_stb_count +1;
		end if;
	end if;
end process;
		

--	
end sim_wbSlave;
