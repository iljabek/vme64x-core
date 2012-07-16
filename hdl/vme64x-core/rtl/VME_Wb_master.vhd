--___________________________________________________________________________________
--                              VME TO WB INTERFACE
--
--                                CERN,BE/CO-HT 
--___________________________________________________________________________________
-- File:                           Wb_master.vhd
--___________________________________________________________________________________
-- Description:
-- This component implements the WB master side in the vme64x core.
-- Work mode:
--            PIPELINED 
--            SINGLE READ/WRITE
--
-- The WB bus is 64 bit wide and the data organization is BIG ENDIAN --> the most 
-- significant byte is carried in the lower position of the bus.
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
-- This component implements the correct shift of the data in input/output from WB bus
--
--______________________________________________________________________________
-- Authors:                                      
--               Pablo Alvarez Sanchez (Pablo.Alvarez.Sanchez@cern.ch)                             
--               Davide Pedretti       (Davide.Pedretti@cern.ch)  
-- Date         06/2012                                                                           
-- Version      v0.01  
--______________________________________________________________________________
--                               GNU LESSER GENERAL PUBLIC LICENSE                                
--                              ------------------------------------                              
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

entity VME_Wb_master is
   Port ( s_memReq        : in   std_logic;
          clk_i           : in   std_logic;
          s_cardSel       : in   std_logic;
          s_reset         : in   std_logic;
          s_mainFSMreset  : in   std_logic;
          s_BERRcondition : in   std_logic;
          s_sel           : in   std_logic_vector (7 downto 0);
          s_beatCount     : in   std_logic_vector (8 downto 0);
          s_locDataInSwap : in   std_logic_vector (63 downto 0);
          s_locDataOut    : out  std_logic_vector (63 downto 0);
          s_rel_locAddr   : in   std_logic_vector (63 downto 0);
          s_AckWithError  : out  std_logic;
          memAckWb        : out  std_logic;
          err             : out  std_logic;
          rty             : out  std_logic;
          s_RW            : in   std_logic;
          psize_o         : out  std_logic_vector (8 downto 0);
          stall_i         : in   std_logic;
          rty_i           : in   std_logic;
          err_i           : in   std_logic;
          cyc_o           : out  std_logic;
          memReq_o        : out  std_logic;
          WBdata_o        : out  std_logic_vector (63 downto 0);
          wbData_i        : in   std_logic_vector (63 downto 0);
          locAddr_o       : out  std_logic_vector (63 downto 0);
          memAckWB_i      : in   std_logic;
          WbSel_o         : out  std_logic_vector (7 downto 0);
          RW_o            : out  std_logic);
end VME_Wb_master;

architecture Behavioral of VME_Wb_master is
-- stb_o handler
begin
   process(clk_i)
   begin
      if rising_edge(clk_i) then
         if s_reset = '1' or s_mainFSMreset = '1' or stall_i = '0' then
            memReq_o <= '0';
         elsif s_memReq = '1' and s_cardSel = '1' and s_BERRcondition = '0' then	 
            memReq_o <= '1';
         end if;
      end if;
   end process;
-- cyc_o handler
   process(clk_i)
   begin
      if rising_edge(clk_i) then
         if s_reset = '1' or s_mainFSMreset = '1' or memAckWB_i = '1' then
            cyc_o <= '0';
         elsif s_memReq = '1' and s_cardSel = '1' and s_BERRcondition = '0' then	 
            cyc_o <= '1';
         end if;
      end if;
   end process;
-- shift output data to WB
   process(clk_i)
   begin
      if rising_edge(clk_i) then
         if s_sel = "10000000" then
            WBdata_o <= std_logic_vector(unsigned(s_locDataInSwap) sll 56);               
         elsif s_sel = "01000000" then
            WBdata_o <= std_logic_vector(unsigned(s_locDataInSwap) sll 48);                                     
         elsif s_sel = "00100000" then
            WBdata_o <=  std_logic_vector(unsigned(s_locDataInSwap) sll 40);                     
         elsif s_sel = "00010000" then
            WBdata_o <=   std_logic_vector(unsigned(s_locDataInSwap) sll 32);                      
         elsif s_sel = "00001000" then
            WBdata_o <=  std_logic_vector(unsigned(s_locDataInSwap) sll 24);                                
         elsif s_sel = "00000100" then
            WBdata_o <=   std_logic_vector(unsigned(s_locDataInSwap) sll 16);                     
         elsif s_sel = "00000010" then
            WBdata_o <= std_logic_vector(unsigned(s_locDataInSwap) sll 8);                       
         elsif s_sel = "11000000" then
            WBdata_o <=   std_logic_vector(unsigned(s_locDataInSwap) sll 48); 
         elsif s_sel = "00110000" then
            WBdata_o <=  std_logic_vector(unsigned(s_locDataInSwap) sll 32);                    
         elsif s_sel = "00001100" then
            WBdata_o <=  std_logic_vector(unsigned(s_locDataInSwap) sll 16);                               
         elsif s_sel = "11110000" then
            WBdata_o <=  std_logic_vector(unsigned(s_locDataInSwap) sll 32);                                  
         else 
            WBdata_o <=  s_locDataInSwap;                                  
         end if;
         RW_o     <= s_RW;
         s_AckWithError <=(s_memReq and s_cardSel and s_BERRcondition);    
         WbSel_o  <= std_logic_vector(s_sel);
      end if;
   end process;
-- shift input data from WB
   s_locDataOut <=  std_logic_vector(resize(unsigned(wbData_i(15 downto 0)) srl 8,s_locDataOut'length))  when 
                      s_cardSel = '1' and s_sel = "00000010" else      
                    std_logic_vector(resize(unsigned(wbData_i(23 downto 0)) srl 16,s_locDataOut'length)) when 
                      s_cardSel = '1' and s_sel = "00000100" else                          
                    std_logic_vector(resize(unsigned(wbData_i(31 downto 0)) srl 24,s_locDataOut'length)) when 
                      s_cardSel = '1' and s_sel = "00001000" else 						
                    std_logic_vector(resize(unsigned(wbData_i(39 downto 0)) srl 32,s_locDataOut'length)) when 
                      s_cardSel = '1' and s_sel = "00010000" else       
                    std_logic_vector(resize(unsigned(wbData_i(47 downto 0)) srl 40,s_locDataOut'length)) when 
                      s_cardSel = '1' and s_sel = "00100000" else                          
                    std_logic_vector(resize(unsigned(wbData_i(55 downto 0)) srl 48,s_locDataOut'length)) when 
                      s_cardSel = '1' and s_sel = "01000000" else
                    std_logic_vector(resize(unsigned(wbData_i) srl 56,s_locDataOut'length))              when 
                      s_cardSel = '1' and s_sel = "10000000" else
                    std_logic_vector(resize(unsigned(wbData_i(31 downto 0)) srl 16,s_locDataOut'length)) when 
                      s_cardSel = '1' and s_sel = "00001100" else                          
                    std_logic_vector(resize(unsigned(wbData_i(47 downto 0)) srl 32,s_locDataOut'length)) when 
                      s_cardSel = '1' and s_sel = "00110000" else
                    std_logic_vector(resize(unsigned(wbData_i) srl 48,s_locDataOut'length))              when 
                      s_cardSel = '1' and s_sel = "11000000" else
                    std_logic_vector(resize(unsigned(wbData_i(7 downto 0)), s_locDataOut'length))        when 
                      s_cardSel = '1' and s_sel = "00000001" else
                    std_logic_vector(resize(unsigned(wbData_i(15 downto 0)), s_locDataOut'length))       when 
                      s_cardSel = '1' and s_sel = "00000011" else
                    std_logic_vector(resize(unsigned(wbData_i(31 downto 0)), s_locDataOut'length))       when 
                      s_cardSel = '1' and s_sel = "00001111" else
                    std_logic_vector(unsigned(wbData_i) srl 32)                                          when 
                      s_cardSel = '1' and s_sel = "11110000" else
                    std_logic_vector(unsigned(wbData_i))                                                 when 
                      s_cardSel = '1' and s_sel = "11111111" else
                    (others => '0'); 


   locAddr_o <= b"000" & s_rel_locAddr(63 downto 3);		
   err <= err_i;
   rty <= rty_i; 
   memAckWb <= memAckWB_i;
   psize_o <= s_beatCount;

end Behavioral;

