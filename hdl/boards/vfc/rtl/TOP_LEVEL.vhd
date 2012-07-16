--______________________________________________________________________
--                             VME TO WB INTERFACE
--
--                                CERN,BE/CO-HT 
--______________________________________________________________________
-- Description: 
-- The aim of this top level is to debug the vme64x interface so in the
-- WB side a RAM memory WB capable has been inserted as show in the following 
-- block diagram.
--
-- TOP_LEVEL's block diagram
--              ____________________________________________________________ 
--    ___      |   ______________________            ___________________    |
--   | B |     |  |                      |          |                   |   |
--   | A |     |  |      VME TO WB       |          |                   |   |
--   | C |     |  |      INTERFACE       |          |                   |   |
--   | K |     |  | (VME64xCore_Top.vhd) |          |      SPRAM        |   |
--   | P |_____|__|           |          |__________|        WB         |   |
--   | L |_____|__|           |          |__________|      SLAVE        |   |
--   | A |     |  |   VME     |   WB     |Point to  |  (xwb_ram.vhd)    |   |
--   | N |     |  |  SLAVE    | MASTER   |  Point   |    64-bit port    |   |
--   | E |     |  |           |          |Interconn |  Byte Granularity |   |
--   |   |     |  |           |          |          |                   |   |
--   |   |     |  |           |          |          |                   |   |
--   |___|     |  |______________________|          |___________________|   | 
--             |____________________________________________________________|       
--
--
-- The wb slave supports the PIPELINED mode.  
-- A little about the clk:
-- The VME  is an asynchronous and handshake protocol so the vme64x interface 
-- has to work at any clock frequency but since all the asynchronous signals
-- are sampled and the core work around the main FSM that of course is a synchronous
-- machine and the VME standards provide a set of timing rules, not all the 
-- clock frequency ensure proper operation of the core.
--
-- 1)   Fig. 25 pag. 107----"VMEbus Specification" ANSI/IEEE STD1014-1987 
--                        min 30ns
--                       <------->
--                       _________
--             AS*______/         \______
-- As show in the figure, to be sure that the slave detects the rising edge
-- and the following falling edge on the AS* signal the clk_i's period must be 
-- maximum 30 ns.
-- 2)  Fig. 20 pag. 99----"VMEbus Specification" ANSI/IEEE STD1014-1987
--         max 20ns
--         <--->
--  ______
--        \__________DSA*
--  ___________
--             \_____DSB*
-- The Master may not assert the data strobe lines at the same time; the 
-- maximum delay between the two falling edge is 20 ns --> in the MFS 
-- machine in the VME_bus.vhd file the LATCH_DS state has been inserted and the 
-- minimum clk_i's period must be of 10 ns.
-- 
-- VME to WB interface:
-- See the VME64xCore_Top.vhd component
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

-- uncomment to use the PLL 
-- Library UNISIM;
-- use UNISIM.vcomponents.all;
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.all;
use work.wishbone_pkg.all;


entity TOP_LEVEL is
port(
    clk_i            : in    std_logic;    	 
    Reset            : in    std_logic;  -- hand reset; button PB1
   	-- VME                            
    VME_AS_n_i       : in    std_logic;
    VME_RST_n_i      : in    std_logic;
    VME_WRITE_n_i    : in    std_logic;
    VME_AM_i         : in    std_logic_vector(5 downto 0);
    VME_DS_n_i       : in    std_logic_vector(1 downto 0);
    VME_GA_i         : in    std_logic_vector(5 downto 0);
    VME_BERR_o       : out   std_logic;
    VME_DTACK_n_o    : out   std_logic;
    VME_RETRY_n_o    : out   std_logic;  
    VME_LWORD_n_b    : inout std_logic;
    VME_ADDR_b       : inout std_logic_vector(31 downto 1);
    VME_DATA_b       : inout std_logic_vector(31 downto 0);
    VME_BBSY_n_i     : in    std_logic;
    VME_IRQ_n_o      : out   std_logic_vector(6 downto 0);
    VME_IACKIN_n_i   : in    std_logic;
    VME_IACKOUT_n_o  : out   std_logic;
    VME_IACK_n_i     : in    std_logic;  
    -- VME buffers
	 VME_RETRY_OE_o   : out   std_logic;
    VME_DTACK_OE_o   : out   std_logic;
    VME_DATA_DIR_o   : out   std_logic;
    VME_DATA_OE_N_o  : out   std_logic;
    VME_ADDR_DIR_o   : out   std_logic;
    VME_ADDR_OE_N_o  : out   std_logic;
	 -- not used
	 RST_i            : in    std_logic;
	 -- for debug:
	 leds             : out   std_logic_vector(7 downto 0)
	 );

end TOP_LEVEL;

architecture Behavioral of TOP_LEVEL is

COMPONENT VME64xCore_Top
	PORT(
	   -- VME signals:
		clk_i           : in    std_logic;
		VME_AS_n_i      : in    std_logic;
		VME_RST_n_i     : in    std_logic;
		VME_WRITE_n_i   : in    std_logic;
		VME_AM_i        : in    std_logic_vector(5 downto 0);
		VME_DS_n_i      : in    std_logic_vector(1 downto 0);
		VME_GA_i        : in    std_logic_vector(5 downto 0);
		VME_BBSY_n_i    : in    std_logic;
		VME_IACKIN_n_i  : in    std_logic;
		VME_IACK_n_i    : in    std_logic;	
		VME_LWORD_n_b_i : in    std_logic;
		VME_LWORD_n_b_o : out   std_logic;
		VME_ADDR_b_i    : in    std_logic_vector(31 downto 1);
		VME_ADDR_b_o    : out   std_logic_vector(31 downto 1);
		VME_DATA_b_i    : in    std_logic_vector(31 downto 0); 
		VME_DATA_b_o    : out   std_logic_vector(31 downto 0); 
		VME_BERR_o      : out   std_logic;
		VME_DTACK_n_o   : out   std_logic;
		VME_RETRY_n_o   : out   std_logic;
		VME_RETRY_OE_o  : out   std_logic;
		VME_IRQ_n_o     : out   std_logic_vector(6 downto 0);
		VME_IACKOUT_n_o : out   std_logic;
		VME_DTACK_OE_o  : out   std_logic;
		VME_DATA_DIR_o  : out   std_logic;
		VME_DATA_OE_N_o : out   std_logic;
		VME_ADDR_DIR_o  : out   std_logic;
		VME_ADDR_OE_N_o : out   std_logic;
		-- WB signals
		RST_i           : in    std_logic;
		DAT_i           : in    std_logic_vector(63 downto 0);
		ERR_i           : in    std_logic;
		RTY_i           : in    std_logic;
		ACK_i           : in    std_logic;
		STALL_i         : in    std_logic;
		IRQ_i           : in    std_logic;  
      INT_ack         : out   std_logic;	
		DAT_o           : out   std_logic_vector(63 downto 0);
		ADR_o           : out   std_logic_vector(63 downto 0);
		CYC_o           : out   std_logic;
		SEL_o           : out   std_logic_vector(7 downto 0);
		STB_o           : out   std_logic;
		WE_o            : out   std_logic;
		reset_o         : out   std_logic;
		-- for debug:
	   leds            : out   std_logic_vector(7 downto 0)
		);
END COMPONENT;

COMPONENT xwb_ram
        generic(
                g_size                  : natural := 256;
                g_init_file             : string  := "";
                g_must_have_init_file   : boolean := true;
                g_slave1_interface_mode : t_wishbone_interface_mode;
                g_slave1_granularity    : t_wishbone_address_granularity
       );
	PORT(
		          clk_sys_i : in std_logic;
		          rst_n_i   : in std_logic;
	           	 INT_ack   : in std_logic;
		          slave1_i  : in t_wishbone_slave_in;          
		          slave1_o  : out t_wishbone_slave_out
		);
END COMPONENT;

signal WbDat_i                   : std_logic_vector(63 downto 0);
signal WbDat_o                   : std_logic_vector(63 downto 0);
signal WbAdr_o                   : std_logic_vector(63 downto 0);
signal WbCyc_o                   : std_logic;
signal WbErr_i                   : std_logic;
signal WbRty_i                   : std_logic;
signal WbSel_o                   : std_logic_vector(7 downto 0);
signal WbStb_o                   : std_logic;
signal WbAck_i                   : std_logic;	
signal WbWe_o                    : std_logic;		
signal WbStall_i                 : std_logic;		
signal WbIrq_i                   : std_logic;		
signal Rst                       : std_logic;								
signal clk_in                    : std_logic;
signal s_locked                  : std_logic;
signal s_fb                      : std_logic;
signal s_INT_ack                 : std_logic;
signal s_rst                     : std_logic;
--mux
signal s_VME_DATA_b_o            : std_logic_vector(31 downto 0);
signal s_VME_DATA_DIR            : std_logic;
signal s_VME_ADDR_DIR            : std_logic;
signal s_VME_ADDR_b_o            : std_logic_vector(31 downto 1);
signal s_VME_LWORD_n_b_o         : std_logic;
begin

Inst_VME64xCore_Top: VME64xCore_Top PORT MAP(
		clk_i           => clk_in,
		VME_AS_n_i      => VME_AS_n_i,
		VME_RST_n_i     => Rst,
		VME_WRITE_n_i   => VME_WRITE_n_i,
		VME_AM_i        => VME_AM_i,
		VME_DS_n_i      => VME_DS_n_i,
		VME_GA_i        => VME_GA_i,
		VME_BERR_o      => VME_BERR_o,
		VME_DTACK_n_o   => VME_DTACK_n_o,
		VME_RETRY_n_o   => VME_RETRY_n_o,
		VME_RETRY_OE_o  => VME_RETRY_OE_o,
		VME_LWORD_n_b_i => VME_LWORD_n_b,
		VME_LWORD_n_b_o => s_VME_LWORD_n_b_o,
		VME_ADDR_b_i    => VME_ADDR_b,
		VME_ADDR_b_o    => s_VME_ADDR_b_o,
		VME_DATA_b_i    => VME_DATA_b,
		VME_DATA_b_o    => s_VME_DATA_b_o,
		VME_BBSY_n_i    => VME_BBSY_n_i,
		VME_IRQ_n_o     => VME_IRQ_n_o,
		VME_IACKIN_n_i  => VME_IACKIN_n_i,
		VME_IACK_n_i    => VME_IACK_n_i,
		VME_IACKOUT_n_o => VME_IACKOUT_n_o,
		VME_DTACK_OE_o  => VME_DTACK_OE_o,
		VME_DATA_DIR_o  => s_VME_DATA_DIR,
		VME_DATA_OE_N_o => VME_DATA_OE_N_o,
		VME_ADDR_DIR_o  => s_VME_ADDR_DIR,
		VME_ADDR_OE_N_o => VME_ADDR_OE_N_o,
		RST_i           => RST_i,
		DAT_i           => WbDat_i,  
		DAT_o           => WbDat_o,  
		ADR_o           => WbAdr_o,  
		CYC_o           => WbCyc_o,  
		ERR_i           => WbErr_i,  
		RTY_i           => WbRty_i,  
		SEL_o           => WbSel_o, 
		STB_o           => WbStb_o, 
		ACK_i           => WbAck_i, 
		WE_o            => WbWe_o,  
		STALL_i         => WbStall_i, 
		IRQ_i           => WbIrq_i,  
		INT_ack         => s_INT_ack,
		reset_o         => s_rst,
		-- Add by Davide for debug:
	   leds            => leds
	);

Inst_xwb_ram: xwb_ram 
      generic map(g_size                   => 256,
                  g_init_file              => "",
                  g_must_have_init_file    => false,
                  g_slave1_interface_mode  => PIPELINED,
                  g_slave1_granularity     => BYTE
						)
    		port map(
		            clk_sys_i                => clk_in,
		            rst_n_i                  => s_rst,
						INT_ack                  => s_INT_ack,
						slave1_i.cyc             => WbCyc_o,
                  slave1_i.stb             => WbStb_o,
                  slave1_i.adr             => WbAdr_o,
                  slave1_i.sel             => WbSel_o,
                  slave1_i.we              => WbWe_o,
                  slave1_i.dat             => WbDat_o,
		            slave1_o.ack             => WbAck_i,
                  slave1_o.err             => WbErr_i,
                  slave1_o.rty             => WbRty_i,
                  slave1_o.stall           => WbStall_i,
                  slave1_o.int             => WbIrq_i, 
                  slave1_o.dat             => WbDat_i
	);
	
	
  Rst <= VME_RST_n_i and Reset;
---------------------------------------------------------------------------------  
    -- buffers...The buffers on the board work in the same way                                                             
    VME_DATA_b       <= s_VME_DATA_b_o    when s_VME_DATA_DIR = '1' else (others => 'Z');
    VME_ADDR_b       <= s_VME_ADDR_b_o    when s_VME_ADDR_DIR = '1' else (others => 'Z');
    VME_LWORD_n_b    <= s_VME_LWORD_n_b_o when s_VME_ADDR_DIR = '1' else 'Z';
---------------------------------------------------------------------------------	 
	 -- Outputs:
	 VME_ADDR_DIR_o   <= s_VME_ADDR_DIR;
	 VME_DATA_DIR_o   <= s_VME_DATA_DIR;
-- uncomment to use the PLL:
-- PLL_BASE_inst : PLL_BASE
--   generic map (
--      BANDWIDTH => "OPTIMIZED",   -- "HIGH", "LOW" or "OPTIMIZED" 
--      CLKFBOUT_MULT => 30,        -- Multiply value for all CLKOUT clock outputs (1-64)
--      CLKFBOUT_PHASE => 0.000,    -- Phase offset in degrees of the clock feedback output
--                                  -- (0.0-360.0).
--      CLKIN_PERIOD => 50.000,     -- Input clock period in ns to ps resolution (i.e. 33.333 is 30
--                                  -- MHz).
--      -- CLKOUT0_DIVIDE - CLKOUT5_DIVIDE: Divide amount for CLKOUT# clock output (1-128)
--      CLKOUT0_DIVIDE => 12,
--      CLKOUT1_DIVIDE => 1,
--      CLKOUT2_DIVIDE => 1,
--      CLKOUT3_DIVIDE => 1,
--      CLKOUT4_DIVIDE => 1,
--      CLKOUT5_DIVIDE => 1,
--      -- CLKOUT0_DUTY_CYCLE - CLKOUT5_DUTY_CYCLE: 
--      -- Duty cycle for CLKOUT# clock output (0.01-0.99).
--      CLKOUT0_DUTY_CYCLE => 0.500,
--      CLKOUT1_DUTY_CYCLE => 0.500,
--      CLKOUT2_DUTY_CYCLE => 0.500,
--      CLKOUT3_DUTY_CYCLE => 0.500,
--      CLKOUT4_DUTY_CYCLE => 0.500,
--      CLKOUT5_DUTY_CYCLE => 0.500,
--      -- CLKOUT0_PHASE - CLKOUT5_PHASE: 
--      -- Output phase relationship for CLKOUT# clock output (-360.0-360.0).
--      CLKOUT0_PHASE => 0.000,
--      CLKOUT1_PHASE => 0.000,
--      CLKOUT2_PHASE => 0.000,
--      CLKOUT3_PHASE => 0.000,
--      CLKOUT4_PHASE => 0.000,
--      CLKOUT5_PHASE => 0.000,
--      CLK_FEEDBACK => "CLKFBOUT",           
--      COMPENSATION => "SYSTEM_SYNCHRONOUS", 
--      DIVCLK_DIVIDE => 1,                   -- Division value for all output clocks (1-52)
--      REF_JITTER => 0.1,                    -- Reference Clock Jitter in UI (0.000-0.999).
--      RESET_ON_LOSS_OF_LOCK => FALSE        -- Must be set to FALSE
--   )
--   port map (
--      CLKFBOUT => s_fb,     -- 1-bit output: PLL_BASE feedback output
--      -- CLKOUT0 - CLKOUT5: 1-bit (each) output: Clock outputs
--      CLKOUT0 => clk_in,    --clk 50 MHz
--      CLKOUT1 => open,
--      CLKOUT2 => open,
--      CLKOUT3 => open,
--      CLKOUT4 => open,
--      CLKOUT5 => open,
--      LOCKED => s_locked,   -- 1-bit output: PLL_BASE lock status output
--      CLKFBIN => s_fb,      -- 1-bit input: Feedback clock input
--      CLKIN => clk_i,       -- 1-bit input: Clock input
--      RST => '0'            -- 1-bit input: Reset input
--   );	

-- comment the next line if the PLL is used:
	clk_in <= clk_i;
end Behavioral;

