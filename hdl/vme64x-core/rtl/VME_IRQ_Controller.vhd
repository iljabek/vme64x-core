--_________________________________________________________________________________________
--                             VME TO WB INTERFACE
--
--                                CERN,BE/CO-HT 
--_________________________________________________________________________________________
-- File:                      VME_IRQ_Controller.vhd
--_________________________________________________________________________________________
-- Description:
-- This block acts as Interrupter. Phases of an interrupt cycle:
-- 1) The Interrupt Controller receives an interrupt request by the WB bus; 
--    this request is a pulse on the INT_Req input 
-- 2) The Interrupt Controller asserts ('0') one of the 7 VME_IRQ lines; --> request of a service.
--    The Interrupt priority is specificated by the Master writing the INT_Level register 
--    in the CR/CSR space
-- 3) The Interrupter Controller wait for the falling edge on the VME_IACKIN line.
-- 4) When detects VME_IACKIN_n_i = '0' and the Interrupt Handler initiates the Interrupt 
--    cycle by asserting AS,the Interrupt Controller check if it is the responding interrupter.
--    Indeed before responding to an interrupt acknowledge cycle the interrupter shall have 
--    an interrupt request pending, shall check if the level of that request match the level 
--    indicated on the address lines A1, A2 and A3,the data transfer width during the interrupt 
--    acknowledge cycle should be equal or greater than the size the it can respond with, and 
--    it shall receive a falling edge on its IACKIN*.
-- 5) If it is the responding interrupter should send the source/ID on the VME_DATA lines 
--    (in our case the source/ID is the INT_Vector that the Master can write in the corresponding 
--    register in the CR/CSR space) and it terminates the interrupt cycle with an acknowledge before 
--    releasing the IRQ lines. If it isn't the responding interrupter, it should pass a falling edge on 
--    down the daisy-chain so other interrupters can respond.
--     
-- All the output signals are registered   
-- To implement the 5 phases before mentioned the follow FSM has been implemented:

--	     __________
--	 |--| IACKOUT2 |<-|
--  |  |__________|  |
--  |                |
--  |    _________   |  _________     _________     _________     __________         
--  |-->|  IDLE   |--->|  IRQ    |-->| WAIT_AS |-->| WAIT_DS |-->| LATCH_DS |-->--|        
--      |_________|    |_________|   |_________|   |_________|   |__________|     | 
--         |             |                                                        |
--         |             |                       _________      _________         |
--         |             |---------<------------| IACKOUT1| <--| ACK_INT |<-------|
--         |                                    |_________|    |_________|     
--         |                     __________     __________         |
--         |--<-----------------|  DTACK   |<--| DATA_OUT |---<----|
--                              |__________|   |__________|   
--
-- The interrupter wait the IACKIN falling edge in the IRQ state, so if the interrupter
-- don't have interrupt pending for sure it will not respond because it is in IDLE.
-- Time constraint:
--                      
--  Time constraint n° 35:
--       Clk   _____       _____       _____       _____       _____       _____      
--       _____|     |_____|     |_____|     |_____|     |_____|     |_____|     |_____     
--  VME_AS1_n_i ______________________________________________________________________
--       ______|
--  VME_AS_n_i              __________________________________________________________
--       __________________|
--  AS_RisingEdge           ___________
--       __________________|           |______________________________________________
--  s_IACKOUT   ______________________________________________________________________
--       ______|          
--  VME_IACKOUT_o           __________________________________________________________
--       __________________|
--
--       ______________________________  _____________________________________________
--          IACKOUT 1/2                \/     IDLE/IRQ
--       ------------------------------/\--------------------------------------------- 
--
--  To avoid the time constraint indicated with the number 35 fig. 55 pag. 183 in the
--  "VMEbus Specification" ANSI/IEEE STD1014-1987, is necessary generate the VME_AS1_n_i 
--  signal who is the AS signal sampled only two times and not 3 times as the VME_AS_n_i 
--  signal, and assign this signal to the s_IACKOUT signal when the fsm is in the 
--  IACKOUTx state.
--
--____________________________________________________________________________________
-- Authors:       
--               Pablo Alvarez Sanchez (Pablo.Alvarez.Sanchez@cern.ch)                                                          
--               Davide Pedretti       (Davide.Pedretti@cern.ch)  
-- Date          08/2012                                                                           
-- Version       v0.02  
--_____________________________________________________________________________________
--                               GNU LESSER GENERAL PUBLIC LICENSE                                
--                              ------------------------------------    
-- Copyright (c) 2009 - 2011 CERN                           
-- This source file is free software; you can redistribute it and/or modify it 
-- under the terms of the GNU Lesser General Public License as published by the 
-- Free Software Foundation; either version 2.1 of the License, or (at your option) 
-- any later version. This source is distributed in the hope that it will be useful, 
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
-- FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for 
-- more details. You should have received a copy of the GNU Lesser General Public 
-- License along with this source; if not, download it from 
-- http://www.gnu.org/licenses/lgpl-2.1.html                     
---------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.all;
use work.vme64x_pack.all;
--===========================================================================
-- Entity declaration
--===========================================================================
entity VME_IRQ_Controller is
   Port ( clk_i            : in   std_logic;
          reset            : in   std_logic;  
          VME_IACKIN_n_i   : in   std_logic;
          VME_AS_n_i       : in   std_logic;
			 VME_AS1_n_i      : in   std_logic;
          VME_DS_n_i       : in   std_logic_vector (1 downto 0);
          VME_LWORD_n_i    : in   std_logic;
          VME_ADDR_123     : in   std_logic_vector (2 downto 0);
          INT_Level        : in   std_logic_vector (7 downto 0);
          INT_Vector       : in   std_logic_vector (7 downto 0);
          INT_Req          : in   std_logic;
          VME_IRQ_n_o      : out  std_logic_vector (6 downto 0);
          VME_IACKOUT_n_o  : out  std_logic;
          VME_DTACK_n_o    : out  std_logic;
          VME_DTACK_OE_o   : out  std_logic;
          VME_DATA_o       : out  std_logic_vector (31 downto 0);
          VME_DATA_DIR_o   : out  std_logic);
end VME_IRQ_Controller;
--===========================================================================
-- Architecture declaration
--===========================================================================
architecture Behavioral of VME_IRQ_Controller is
--input signals
   signal INT_Req_sample            : std_logic;
--output signals
   signal s_DTACK                   : std_logic;
   signal s_DTACK_OE                : std_logic;
	signal s_DTACK_OE_o              : std_logic;
   signal s_DataDir                 : std_logic;
   signal s_IACKOUT                 : std_logic;
	signal s_IACKOUT_o               : std_logic;
	signal s_enable                  : std_logic;
   signal s_IRQ                     : std_logic_vector(6 downto 0);
   signal s_Data                    : std_logic_vector(31 downto 0);
--
   signal AS_FallingEdge            : std_logic;
	signal AS_RisingEdge             : std_logic;     
   type t_MainFSM is (IDLE, IRQ, WAIT_AS, WAIT_DS, LATCH_DS, ACK_INT, DATA_OUT, DTACK,IACKOUT1,IACKOUT2);
   signal currs, nexts              : t_MainFSM;
   signal s_ack_int                 : std_logic;
   signal s_resetIRQ                : std_logic;
   signal s_enableIRQ               : std_logic;
   signal VME_ADDR_123_latched      : std_logic_vector(2 downto 0);
   signal VME_DS_latched            : std_logic_vector(1 downto 0);
   signal DSlatch                   : std_logic;
   signal ADDRmatch                 : std_logic;
--===========================================================================
-- Architecture begin
--===========================================================================
begin

-- Input sampling and edge detection
   ASrisingEdge : RisEdgeDetection
      port map (
               sig_i      => VME_AS1_n_i,
               clk_i      => clk_i,
               RisEdge_o  => AS_RisingEdge
             );
				 
   ASfallingEdge : FallingEdgeDetection
      port map (
               sig_i      => VME_AS_n_i,
               clk_i      => clk_i,
               FallEdge_o => AS_FallingEdge
             );
				 
   INT_ReqinputSample : FlipFlopD
      port map(
               sig_i     => INT_Req,
               sig_o     => INT_Req_sample,
               clk_i     => clk_i,
               reset     => '0',
               enable    => s_enable
            );		

--Output registers:

   DTACKOutputSample : FlipFlopD
     port map(
              sig_i  => s_DTACK,
              sig_o  => VME_DTACK_n_o,
              clk_i  => clk_i,
              reset  => '0',
              enable => '1'
           );		
   DataDirOutputSample : FlipFlopD
     port map(
              sig_i  => s_DataDir,
              sig_o  => VME_DATA_DIR_o,
              clk_i  => clk_i,
              reset  => '0',
              enable => '1'
           );		
   IACKOUTOutputSample : FlipFlopD
     port map(
              sig_i  => s_IACKOUT,
              sig_o  => s_IACKOUT_o,
              clk_i  => clk_i,
              reset  => '0',
              enable => '1'
           );		
   DTACKOEOutputSample : FlipFlopD
     port map(
              sig_i  => s_DTACK_OE,
              sig_o  => s_DTACK_OE_o,
              clk_i  => clk_i,
              reset  => '0',
              enable => '1'
           );		

   process(clk_i)
   begin
      if rising_edge(clk_i) then
         if s_resetIRQ = '1' then
            VME_IRQ_n_o <= (others => '1');
         elsif s_enableIRQ = '1' then	 
            VME_IRQ_n_o <= s_IRQ; 
         end if;
      end if;	 
   end process;		

   process(clk_i)
   begin
      if rising_edge(clk_i) then
         VME_DATA_o <= s_Data; 
      end if;
   end process;		

-- Update current state
   process(clk_i)
   begin
      if rising_edge(clk_i) then
         if reset = '0' then
            currs <= IDLE;
         else
            currs <= nexts;
         end if;	
     end if;
  end process;		
-- Update next state
  process(currs,INT_Req_sample,VME_AS_n_i,VME_DS_n_i,s_ack_int,VME_IACKIN_n_i,AS_RisingEdge)
  begin
    case currs is 
      when IDLE =>
         if INT_Req_sample = '1' and VME_IACKIN_n_i = '1' then
            nexts <= IRQ;
         elsif VME_IACKIN_n_i = '0' then
            nexts <= IACKOUT2;
			else
			   nexts <= IDLE;
         end if;

      when IRQ => 
         if VME_IACKIN_n_i = '0' then  -- Each Interrupter who is driving an interrupt request line
                                       -- low waits for a falling edge on IACKIN input -->
                                       -- the IRQ_Controller have to detect a falling edge on the IACKIN.
            nexts <= WAIT_AS;
         else 
            nexts <= IRQ;
         end if;

      when WAIT_AS =>
         if VME_AS_n_i = '0' then  -- NOT USE FALLING EDGE HERE!
            nexts <= WAIT_DS;
         else 
            nexts <= WAIT_AS;
         end if;

      when WAIT_DS =>
         if VME_DS_n_i /= "11" then
            nexts <= LATCH_DS;
         else 
            nexts <= WAIT_DS;
         end if;
      when LATCH_DS =>	  
         nexts <= ACK_INT;

      when ACK_INT =>	  
         if s_ack_int = '1' then
            nexts <= DATA_OUT;  -- The Interrupter send the INT_Vector
         else 
            nexts <= IACKOUT1;   -- the Interrupter must pass a falling edge on the IACKOUT output
         end if;

      when IACKOUT1 =>	 
		   if AS_RisingEdge = '1' then  
            nexts <= IRQ;
         else 
            nexts <= IACKOUT1;
         end if;	
          
			
      when  DATA_OUT=>	  
         nexts <= DTACK;	 
      
		when IACKOUT2 =>	
         if AS_RisingEdge = '1' then  
            nexts <= IDLE;
         else 
            nexts <= IACKOUT2;
         end if;			         
      	
      when  DTACK=>	
         if AS_RisingEdge = '1' then  
            nexts <= IDLE;
         else 
            nexts <= DTACK;
         end if;		 	  

    end case;

  end process;
-- Update Outputs
-- Mealy FSM
  process(currs,VME_AS1_n_i)
  begin
    case currs is 
      when IDLE =>
          s_IACKOUT   <= '1';
          s_DataDir   <= '0'; 
          s_DTACK     <= '1';
          s_enableIRQ <= '0';
          s_resetIRQ  <= '1';
          DSlatch     <= '0';
          s_DTACK_OE  <= '0';
      
       when IRQ => 
          s_IACKOUT   <= '1';
          s_DataDir   <= '0'; 
          s_DTACK     <= '1';
          s_enableIRQ <= '1';
          s_resetIRQ  <= '0';
          DSlatch     <= '0';
          s_DTACK_OE  <= '0';

      when WAIT_AS =>
          s_IACKOUT   <= '1';
          s_DataDir   <= '0'; 
          s_DTACK     <= '1';
          s_enableIRQ <= '0';
          s_resetIRQ  <= '0';
          DSlatch     <= '0';
          s_DTACK_OE  <= '0';

      when WAIT_DS =>
          s_IACKOUT   <= '1';
          s_DataDir   <= '0'; 
          s_DTACK     <= '1';
          s_enableIRQ <= '0';
          s_resetIRQ  <= '0';
          DSlatch     <= '0';
          s_DTACK_OE  <= '0';

      when LATCH_DS =>	  
          s_IACKOUT   <= '1';
          s_DataDir   <= '0'; 
          s_DTACK     <= '1';
          s_enableIRQ <= '0';
          s_resetIRQ  <= '0';
          DSlatch     <= '1';
          s_DTACK_OE  <= '0';

      when ACK_INT =>	  
          s_IACKOUT   <= '1';
          s_DataDir   <= '0'; 
          s_DTACK     <= '1';
          s_enableIRQ <= '0';
          s_resetIRQ  <= '0';
          DSlatch     <= '0';
          s_DTACK_OE  <= '0';
 
      when  IACKOUT1 =>
          s_IACKOUT   <= VME_AS1_n_i;
          s_DataDir   <= '0'; 
          s_DTACK     <= '1';
          s_enableIRQ <= '0';
          s_resetIRQ  <= '0';
          DSlatch     <= '0';
          s_DTACK_OE  <= '0';
			 
		when  IACKOUT2 =>
          s_IACKOUT   <= VME_AS1_n_i;
          s_DataDir   <= '0'; 
          s_DTACK     <= '1';
          s_enableIRQ <= '0';
          s_resetIRQ  <= '0';
          DSlatch     <= '0';
          s_DTACK_OE  <= '0';	 

      when  DATA_OUT=>	  
          s_IACKOUT   <= '1';
          s_DataDir   <= '1'; 
          s_DTACK     <= '1';
          s_enableIRQ <= '0';
          s_resetIRQ  <= '0';
          DSlatch     <= '0'; 
          s_DTACK_OE  <= '1';

      when  DTACK=>	
          s_IACKOUT   <= '1';
          s_DataDir   <= '1'; 
          s_DTACK     <= '0';
          s_enableIRQ <= '0';
          s_resetIRQ  <= '1';
          DSlatch     <= '0'; 	
          s_DTACK_OE  <= '1';				

    end case;
  end process;

-- This process provides the IRQ vector
  process(INT_Level)
  begin
    case (INT_Level) is
      when "00000001" => s_IRQ <= "1111110";
      when "00000010" => s_IRQ <= "1111101";
      when "00000011" => s_IRQ <= "1111011";
      when "00000100" => s_IRQ <= "1110111";
      when "00000101" => s_IRQ <= "1101111";
      when "00000110" => s_IRQ <= "1011111";
      when "00000111" => s_IRQ <= "0111111";
      when others     => s_IRQ <= "1111111";
    end case;
  end process;

-- This process sampling the address lines on AS falling edge
  process(clk_i)
  begin
    if rising_edge(clk_i) then
      if reset = '0' then 
         VME_ADDR_123_latched <= (others => '0');
      elsif AS_FallingEdge = '1' then  
         VME_ADDR_123_latched <= VME_ADDR_123;
      end if;	
    end if;
  end process;	

-- Data strobo latch 
  process(clk_i)
  begin
    if rising_edge(clk_i) then
      if reset = '0' then 
         VME_DS_latched <= (others => '0');
      elsif DSlatch = '1' then  
         VME_DS_latched <= VME_DS_n_i;
      end if;	
    end if;
  end process;	

--This process check the A01 A02 A03:
  process(clk_i)
  begin
    if rising_edge(clk_i) then
      if reset = '0' then 
         ADDRmatch <= '0';
      elsif unsigned(INT_Level) = unsigned(VME_ADDR_123_latched) then  
         ADDRmatch <= '1';
		else 	
		   ADDRmatch <= '0';
      end if;	
    end if;
  end process;	
  s_ack_int <= (not(VME_DS_latched(0))) and ADDRmatch; --D08 Byte3 access or D32 access  
  s_Data <= x"000000" & INT_Vector;  
  s_enable <= (not INT_Req_sample) or (not s_DTACK);   -- VME_IACKIN_n_i and s_IACKOUT_o;
  -- the INT_Vector is in the D0:7 lines (byte3 in big endian order)  
  VME_DTACK_OE_o  <= s_DTACK_OE_o;
  VME_IACKOUT_n_o <= s_IACKOUT_o;
end Behavioral;
--===========================================================================
-- Architecture end
--===========================================================================

