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
		TGA_i: 	in std_logic_vector(3 downto 0);
		TGC_i: 	in std_logic_vector(3 downto 0);
		CYC_i: 	in std_logic;
		ERR_o: 	out std_logic;
		LOCK_i: in std_logic;
		RTY_o: 	out std_logic;
		SEL_i: 	in  std_logic_vector(7 downto 0);
		STB_i: 	in std_logic;
		ACK_o: 	out std_logic;
		WE_i: 	in std_logic;
		IRQ_o: 	out std_logic_vector(6 downto 0)
		);
end sim_wbSlave;



architecture sim_wbSlave of sim_wbSlave is
type t_ram is array (0 to 1024) of std_logic_vector(63 downto 0);
signal s_ram : t_ram; 

signal s_selectedData : std_logic_vector(63 downto 0);

begin
	

	
ERR_o <= '0';
RTY_o <= '0';

--ACK_o <= '1' when STB_i = '1';

fakeReadWrite: process
begin
	if(CYC_i = '0') then wait until CYC_i = '1'; end if;
	
	wait until STB_i = '1';
	
	if(WE_i = '1') then
		s_ram(CONV_INTEGER(ADR_i(9 downto 0))) <= DAT_i;
		report "saving data";
	else 
		DAT_o <= s_ram(CONV_INTEGER(ADR_i(9 downto 0)));
	end if;	 	
	
	wait for 10ns;
	ACK_o <= '1', '0' after 10ns;
		
	
end process;
--	
end sim_wbSlave;
