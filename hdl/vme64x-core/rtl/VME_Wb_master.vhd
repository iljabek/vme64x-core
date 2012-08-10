--___________________________________________________________________________________
--                              VME TO WB INTERFACE
--
--                                CERN,BE/CO-HT 
--___________________________________________________________________________________
-- File:                           VME_Wb_master.vhd
--___________________________________________________________________________________
-- Description:
-- This component implements the WB master side in the vme64x core.
-- Work mode:
--            PIPELINED 
--            SINGLE READ/WRITE
--
-- The WB bus can be 64 bit wide or 32 bit wide and the data organization is BIG ENDIAN 
-- --> the most significant byte is carried in the lower position of the bus.
-- Eg:
--   _______________________________________________________________________
--  | Byte(0)| Byte(1)| Byte(2)| Byte(3)| Byte(4)| Byte(5)| Byte(6)| Byte(7)|
--  |________|________|________|________|________|________|________|________|
--   D[63:56] D[55:48] D[47:40] D[39:32] D[31:24] D[23:16] D[15:8]  D[7:0]
--
-- eg of timing diagram with synchronous WB Slave:
--             
--       Clk   _____       _____       _____       _____       _____       _____       _____
--       _____|     |_____|     |_____|     |_____|     |_____|     |_____|     |_____|     
--      cyc_o  ____________________________________________________________
--       _____|                                                            |________________
--      stb_o  ________________________________________________
--       _____|                                                |____________________________
--       __________________________________________
--      stall_i                                    |________________________________________
--      ack_i                                                   ___________
--       ______________________________________________________|           |________________       
--
-- The ack_i can be asserted with some Tclk of delay, not immediately.
-- This component implements the correct shift of the data in input/output from/to WB bus
--
--______________________________________________________________________________
-- Authors:                                      
--               Pablo Alvarez Sanchez (Pablo.Alvarez.Sanchez@cern.ch)                             
--               Davide Pedretti       (Davide.Pedretti@cern.ch)  
-- Date         08/2012                                                                           
-- Version      v0.02  
--______________________________________________________________________________
--                               GNU LESSER GENERAL PUBLIC LICENSE                                
--                              ------------------------------------   
-- Copyright (c) 2009 - 2011 CERN                           
-- This source file is free software; you can redistribute it and/or modify it under the terms of 
-- the GNU Lesser General Public License as published by the Free Software Foundation; either     
-- version 2.1 of the License, or (at your option) any later version.                             
-- This source is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;       
-- without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.     
-- See the GNU Lesser General Public License for more details.                                    
-- You should have received a copy of the GNU Lesser General Public License along with this       
-- source; if not, download it from http://www.gnu.org/licenses/lgpl-2.1.html                     
---------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.all;
use work.vme64x_pack.all;

--===========================================================================
-- Entity declaration
--===========================================================================
entity VME_Wb_master is
   generic(g_width      : integer := c_width;
	        g_addr_width : integer := c_addr_width
	);
   Port ( s_memReq        : in   std_logic;
          clk_i           : in   std_logic;
          cardSel         : in   std_logic;
          reset           : in   std_logic;
          mainFSMreset    : in   std_logic;
          BERRcondition   : in   std_logic;
          sel             : in   std_logic_vector(7 downto 0);
          beatCount       : in   std_logic_vector(8 downto 0);
          locDataInSwap   : in   std_logic_vector(63 downto 0);
          locDataOut      : out  std_logic_vector(63 downto 0);
          rel_locAddr     : in   std_logic_vector(63 downto 0);
          memAckWb        : out  std_logic;
          err             : out  std_logic;
          rty             : out  std_logic;
          RW              : in   std_logic;
          psize_o         : out  std_logic_vector(8 downto 0);
          stall_i         : in   std_logic;
          rty_i           : in   std_logic;
          err_i           : in   std_logic;
			 W32             : in   std_logic;
          cyc_o           : out  std_logic;
          memReq_o        : out  std_logic;
          WBdata_o        : out  std_logic_vector(g_width - 1 downto 0);
          wbData_i        : in   std_logic_vector(g_width - 1 downto 0);
          locAddr_o       : out  std_logic_vector(g_addr_width - 1 downto 0);
          memAckWB_i      : in   std_logic;
          WbSel_o         : out  std_logic_vector(f_div8(g_width) - 1 downto 0);
          RW_o            : out  std_logic);
end VME_Wb_master;

--===========================================================================
-- Architecture declaration
--===========================================================================
architecture Behavioral of VME_Wb_master is
signal s_shift_dx     :   std_logic;
signal s_cyc          :   std_logic;
signal s_AckWithError :   std_logic;
signal s_wbData_i     :   std_logic_vector(63 downto 0);
signal s_select       :   std_logic_vector(8 downto 0);

--===========================================================================
-- Architecture begin
--===========================================================================
begin

s_select <= cardSel &  sel;

s_wbData_i <= std_logic_vector(resize(unsigned(wbData_i),s_wbData_i'length));
-- stb handler
  process(clk_i)
   begin
      if rising_edge(clk_i) then
         if reset = '1' or mainFSMreset = '1' or (stall_i = '0' and s_cyc = '1') then
            memReq_o <= '0';
         elsif s_memReq = '1' and cardSel = '1' and BERRcondition = '0' then	 
            memReq_o <= '1';
         end if;
      end if;
   end process;
	
-- cyc_o handler
   process(clk_i)
   begin
      if rising_edge(clk_i) then
         if reset = '1' or mainFSMreset = '1' or memAckWB_i = '1' then
            s_cyc <= '0';
         elsif s_memReq = '1' and cardSel = '1' and BERRcondition = '0' then	 
            s_cyc <= '1';
         end if;
      end if;
   end process;
	cyc_o  <= s_cyc;
	
  process(clk_i)
  begin
      if rising_edge(clk_i) then
         RW_o           <= RW;
         s_AckWithError <=(s_memReq and cardSel and BERRcondition);        
      end if;
  end process;
	
-- shift data and address for WB data bus 64 bits 
  gen64: if (g_width = 64) generate

         process(clk_i)
         begin
	         if rising_edge(clk_i) then
              locAddr_o <= std_logic_vector(resize(unsigned(rel_locAddr) srl 3,c_addr_width));
	         end if;
	      end process;

         process(clk_i)
         begin
            if rising_edge(clk_i) then
               if sel = "10000000" then
                  WBdata_o <= std_logic_vector(unsigned(locDataInSwap) sll 56);               
               elsif sel = "01000000" then
                  WBdata_o <= std_logic_vector(unsigned(locDataInSwap) sll 48);                                     
               elsif sel = "00100000" then
                  WBdata_o <=  std_logic_vector(unsigned(locDataInSwap) sll 40);                     
               elsif sel = "00010000" then
                  WBdata_o <=   std_logic_vector(unsigned(locDataInSwap) sll 32);                      
               elsif sel = "00001000" then
                  WBdata_o <=  std_logic_vector(unsigned(locDataInSwap) sll 24);                                
               elsif sel = "00000100" then
                  WBdata_o <=   std_logic_vector(unsigned(locDataInSwap) sll 16);                     
               elsif sel = "00000010" then
                  WBdata_o <= std_logic_vector(unsigned(locDataInSwap) sll 8);                       
               elsif sel = "11000000" then
                  WBdata_o <=   std_logic_vector(unsigned(locDataInSwap) sll 48); 
               elsif sel = "00110000" then
                  WBdata_o <=  std_logic_vector(unsigned(locDataInSwap) sll 32);                    
               elsif sel = "00001100" then
                  WBdata_o <=  std_logic_vector(unsigned(locDataInSwap) sll 16);                               
               elsif sel = "11110000" then
                  WBdata_o <=  std_logic_vector(unsigned(locDataInSwap) sll 32);                                  
               else 
                  WBdata_o <=  locDataInSwap;                                  
               end if;
			      WbSel_o  <= std_logic_vector(sel);
		      end if;
         end process;		
         
			process (s_select,s_wbData_i)
         begin
           case s_select is
               when "100000010" => locDataOut <= std_logic_vector(		
					     resize(unsigned(s_wbData_i(15 downto 0)) srl 8, locDataOut'length));
               when "100000100" => locDataOut <= std_logic_vector(
					     resize(unsigned(s_wbData_i(23 downto 0)) srl 16,locDataOut'length));
               when "100001000" => locDataOut <= std_logic_vector(
					     resize(unsigned(s_wbData_i(31 downto 0)) srl 24,locDataOut'length));
               when "100010000" => locDataOut <= std_logic_vector(
					     resize(unsigned(s_wbData_i(39 downto 0)) srl 32,locDataOut'length));
               when "100100000" => locDataOut <= std_logic_vector(
					     resize(unsigned(s_wbData_i(47 downto 0)) srl 40,locDataOut'length));
					when "101000000" => locDataOut <= std_logic_vector(
					     resize(unsigned(s_wbData_i(55 downto 0)) srl 48,locDataOut'length));
					when "110000000" => locDataOut <= std_logic_vector(
					     resize(unsigned(s_wbData_i) srl 56,locDataOut'length));
					when "100001100" => locDataOut <= std_logic_vector(
					     resize(unsigned(s_wbData_i(31 downto 0)) srl 16,locDataOut'length));
					when "100110000" => locDataOut <= std_logic_vector(
					     resize(unsigned(s_wbData_i(47 downto 0)) srl 32,locDataOut'length));
					when "111000000" => locDataOut <= std_logic_vector(
					     resize(unsigned(s_wbData_i) srl 48,locDataOut'length));
					when "100000001" => locDataOut <= std_logic_vector(
					     resize(unsigned(s_wbData_i(7 downto 0)), locDataOut'length));
					when "100000011" => locDataOut <= std_logic_vector(
					     resize(unsigned(s_wbData_i(15 downto 0)), locDataOut'length));
					when "100001111" => locDataOut <= std_logic_vector(
					     resize(unsigned(s_wbData_i(31 downto 0)), locDataOut'length));
					when "111110000" => locDataOut <= std_logic_vector(
					     resize(unsigned(s_wbData_i) srl 32, locDataOut'length));
					when "111111111" => locDataOut <= std_logic_vector(
					     resize(unsigned(s_wbData_i),locDataOut'length));
					when others => locDataOut <= (others => '0');
           end case;
         end process;

  end generate gen64; 
		
	-- shift data and address for WB data bus 32 bits 	
		
  gen32: if (g_width = 32) generate

			process(clk_i)
         begin
	         if rising_edge(clk_i) then
               locAddr_o <= std_logic_vector(resize(unsigned(rel_locAddr) srl 2,c_addr_width));
	         end if;
	      end process;
			
			process(sel)
         begin
             if sel = "10000000" or  sel = "01000000" or sel = "00100000" or sel = "00010000" 
			       or sel = "11000000" or sel = "00110000" or sel = "11110000" then
                s_shift_dx <= '1';
             else	 
                s_shift_dx <= '0';
             end if;
         end process;	
			
		   process(clk_i)
         begin
           if rising_edge(clk_i) then
       	     if sel = "10000000" then
                 WBdata_o <= std_logic_vector(resize(unsigned(locDataInSwap) sll 24,c_width));               
              elsif sel = "01000000" then
                 WBdata_o <= std_logic_vector(resize(unsigned(locDataInSwap) sll 16,c_width));                                     
              elsif sel = "00100000" then
                 WBdata_o <=  std_logic_vector(resize(unsigned(locDataInSwap) sll 8,c_width));                                          
              elsif sel = "00001000" then
                 WBdata_o <=  std_logic_vector(resize(unsigned(locDataInSwap) sll 24,c_width));                                
              elsif sel = "00000100" then
                 WBdata_o <=   std_logic_vector(resize(unsigned(locDataInSwap) sll 16,c_width));                     
              elsif sel = "00000010" then
                 WBdata_o <= std_logic_vector(resize(unsigned(locDataInSwap) sll 8,c_width));                       
              elsif sel = "11000000" then
                 WBdata_o <=   std_logic_vector(resize(unsigned(locDataInSwap) sll 16,c_width));                     
              elsif sel = "00001100" then
                 WBdata_o <=  std_logic_vector(resize(unsigned(locDataInSwap) sll 16,c_width));                                                                 
              else 
                 WBdata_o <=  std_logic_vector(resize(unsigned(locDataInSwap),c_width));                                  
              end if;
			  
			     if s_shift_dx = '1' then
			        WbSel_o  <= sel(7 downto 4);  -- b"0000" &
			     else
			        WbSel_o  <= sel(3 downto 0);
              end if;			  		  
           end if;	
         end process;
		
		   process (s_select,s_wbData_i)
         begin
           case s_select is
               when "100000010" => locDataOut <= std_logic_vector(
					     resize(unsigned(s_wbData_i(15 downto 0)) srl 8, locDataOut'length));
               when "100000100" => locDataOut <= std_logic_vector(
					     resize(unsigned(s_wbData_i(23 downto 0)) srl 16,locDataOut'length));
               when "100001000" => locDataOut <= std_logic_vector(
					     resize(unsigned(s_wbData_i(31 downto 0)) srl 24,locDataOut'length));
               when "100010000" => locDataOut <= std_logic_vector(
					     resize(unsigned(s_wbData_i(7 downto 0)),locDataOut'length));
               when "100100000" => locDataOut <= std_logic_vector(
					     resize(unsigned(s_wbData_i(15 downto 0)) srl 8,locDataOut'length));
					when "101000000" => locDataOut <= std_logic_vector(
					     resize(unsigned(s_wbData_i(23 downto 0)) srl 16,locDataOut'length));
					when "110000000" => locDataOut <= std_logic_vector(
					     resize(unsigned(s_wbData_i(31 downto 0)) srl 24,locDataOut'length));
					when "100001100" => locDataOut <= std_logic_vector(
					     resize(unsigned(s_wbData_i(31 downto 0)) srl 16,locDataOut'length));
					when "100110000" => locDataOut <= std_logic_vector(
					     resize(unsigned(s_wbData_i(15 downto 0)),locDataOut'length));
					when "111000000" => locDataOut <= std_logic_vector(
					     resize(unsigned(s_wbData_i(31 downto 0)) srl 16,locDataOut'length));
					when "100000001" => locDataOut <= std_logic_vector(
					     resize(unsigned(s_wbData_i(7 downto 0)), locDataOut'length));
					when "100000011" => locDataOut <= std_logic_vector(
					     resize(unsigned(s_wbData_i(15 downto 0)), locDataOut'length));
					when "100001111" => locDataOut <= std_logic_vector(
					     resize(unsigned(s_wbData_i(31 downto 0)), locDataOut'length));
					when "111110000" => locDataOut <= std_logic_vector(
					     resize(unsigned(s_wbData_i(31 downto 0)), locDataOut'length));
					when others => locDataOut <= (others => '0');
           end case;
         end process;
			
  end generate gen32;			               
			  
   err <= err_i;
   rty <= rty_i; 
   memAckWb <= memAckWB_i or s_AckWithError;
   psize_o <= beatCount;

end Behavioral;
--===========================================================================
-- Architecture end
--===========================================================================
