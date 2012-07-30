--______________________________________________________________________
--                             VME TO WB INTERFACE
--
--                                CERN,BE/CO-HT 
--______________________________________________________________________
-- File:                          xwb_ram.vhd
--______________________________________________________________________
-- Description: This block acts as WB Slave to test the vme64x interface
-- Block diagram:
--                          ____________________________________________
--                         |                                            |
--                         |                                            |
--                         |  __________            ______________      |
--                         | |   WB     |          | INT_COUNT    |     |
--                         | | LOGIC    |          |______________|     |
--                    W    | |          |           ______________      |
--                    B    | |          |          |  FREQ        |     |
--                         | |__________|          |______________|     |
--                    B    |                        ______________      |
--                    U    |                       |              |     |
--                    S    |                       |              |     |
--                         |                       |     RAM      |     |
--                         |   ______________      | 64-bit port  |     |
--                         |  |              |     |     Byte     |     |
--                         |  |     IRQ      |     | Granularity  |     |
--                         |  |  Generator   |     |              |     |
--                         |  |              |     |              |     |
--                         |  |              |     |              |     |
--                         |  |              |     |______________|     |
--                         |  |              |                          |
--                         |  |______________|                          |
--                         |____________________________________________|
--
-- The RAM is a single port ram, 64 bit wide with byte granularity.
-- The INT_COUNT and FREQ registers are mapped in the location 0x00 of the
-- RAM memory, but these two 32 bit registers are outside the RAM because 
-- they are used to generate the interrupt requests and some logic has been 
-- added around these registers.
-- INT_COUNT --> address: 0x000
-- FREQ      --> address: 0x004
-- The address above mentioned are the offsett VME address of the two registers
-- WB LOGIC: some process add to generate the acknowledge and stall signals.
-- IRQ Generator: this component sends an Interrupt request (pulse) to the 
-- IRQ Controller --> Necessary to test the boards.
--______________________________________________________________________________
-- Authors:                                     
--               Pablo Alvarez Sanchez (Pablo.Alvarez.Sanchez@cern.ch)                             
--               Davide Pedretti       (Davide.Pedretti@cern.ch)  
-- Date         06/2012                                                                           
-- Version      v0.01  
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
--
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


library work;
use work.genram_pkg.all;
use work.wishbone_pkg.all;

entity xwb_ram is
  generic(
    g_size                  : natural := 256;  
    g_init_file             : string  := "";
    g_must_have_init_file   : boolean := false;
    g_slave1_interface_mode : t_wishbone_interface_mode;
    g_slave1_granularity    : t_wishbone_address_granularity
    );
  port(
    clk_sys_i               : in  std_logic;
    rst_n_i                 : in  std_logic;
	 INT_ack                 : in  std_logic;
    slave1_i                : in  t_wishbone_slave_in;
    slave1_o                : out t_wishbone_slave_out
    );
end xwb_ram;

architecture struct of xwb_ram is

  function f_zeros(size : integer)
    return std_logic_vector is
  begin
    return std_logic_vector(to_unsigned(0, size));
  end f_zeros;


  signal s_wea      : std_logic;
  signal s_bwea     : std_logic_vector(c_wishbone_data_width/8-1 downto 0);    
  signal slave1_in  : t_wishbone_slave_in;
  signal slave1_out : t_wishbone_slave_out;
  signal s_cyc      : std_logic;
  signal s_stb      : std_logic;

COMPONENT IRQ_generator
	PORT(
		clk_i          : in  std_logic;
		reset          : in  std_logic;
		Freq           : in  std_logic_vector(31 downto 0);
		Int_Count_i    : in  std_logic_vector(31 downto 0);
		Read_Int_Count : in  std_logic;
		INT_ack        : in  std_logic;          
		IRQ_o          : out std_logic;
		Int_Count_o    : out std_logic_vector(31 downto 0)
		);
END COMPONENT;

signal s_INT_COUNT     : std_logic_vector(31 downto 0);
signal s_FREQ          : std_logic_vector(31 downto 0); 
signal s_q_o           : std_logic_vector(63 downto 0);
signal s_q_o1          : std_logic_vector(63 downto 0);
signal s_en_Freq       : std_logic;
signal s_sel_IntCount  : std_logic;
signal s_Int_Count_o   : std_logic_vector(31 downto 0);
signal s_Int_Count_o1  : std_logic_vector(31 downto 0);
signal s_Read_IntCount : std_logic;
signal s_rst           : std_logic;
signal s_stall         : std_logic;

begin
-- reset
s_rst <= not(rst_n_i); 
-- IRQ Generator, INT_COUNT and FREQ logic:
s_q_o1          <= s_INT_COUNT & s_FREQ;
s_en_Freq       <= '1' when (unsigned(slave1_i.adr(f_log2_size(g_size)-1 downto 0)) = 0 
                 and s_bwea = "00001111") else '0';
s_Int_Count_o1  <= slave1_i.dat(63 downto 32) when (s_bwea = "11110000" and 
                 (unsigned(slave1_i.adr(f_log2_size(g_size)-1 downto 0))) = 0) 
					  else s_Int_Count_o;
s_Read_IntCount <= '1' when (slave1_i.we = '0' and slave1_i.sel = "11110000" and 
                 (unsigned(slave1_i.adr(f_log2_size(g_size)-1 downto 0))) = 0 and 
					  slave1_out.ack = '1') else '0';

-- Reg INT_COUNT
INT_COUNT : entity work.Reg32bit
    port map(
      reset  => s_rst,
		enable => '1',
		di     => s_Int_Count_o1,
      do     => s_INT_COUNT,
      clk_i  => clk_sys_i
      );		
		
-- Reg FREQ
FREQ : entity work.Reg32bit
    port map(
      reset  => s_rst,
		enable => s_en_Freq,
		di     => slave1_i.dat(31 downto 0),
      do     => s_FREQ,
      clk_i  => clk_sys_i
      );	

-- IRQ Generator		
Inst_IRQ_generator: IRQ_generator PORT MAP(
		clk_i          => clk_sys_i,
		reset          => s_rst,
		Freq           => s_FREQ,
		Int_Count_i    => s_INT_COUNT,
		Read_Int_Count => s_Read_IntCount,
		INT_ack        => INT_ack,
		IRQ_o          => slave1_o.int,
		Int_Count_o    => s_Int_Count_o
	);

-- RAM memory
  U_DPRAM : entity work.spram
    generic map(
      -- standard parameters
      g_data_width               => 64,
      g_size                     => 256,
      g_with_byte_enable         =>  true,
      g_init_file                => "",
      g_addr_conflict_resolution => "read_first"
      )
    port map(
      clk_i   => clk_sys_i,
      bwe_i   => s_bwea,
      a_i     => slave1_i.adr(f_log2_size(g_size)-1 downto 0),                                 
      d_i     => slave1_i.dat,                                  
      q_o     => s_q_o	                                 
	 );

-- WB Logic:	 
  s_bwea <= slave1_i.sel when s_wea = '1' else f_zeros(c_wishbone_data_width/8);    
  s_wea <= slave1_i.we and slave1_i.cyc and slave1_i.stb and (not s_stall);               
  
  process(clk_sys_i)
  begin
    if(rising_edge(clk_sys_i)) then
      if(s_rst = '0') then
        slave1_out.ack <= '0';      
      else
        if(slave1_out.ack = '1' and g_slave1_interface_mode = CLASSIC) then
          slave1_out.ack <= '0';
        else
          slave1_out.ack <= slave1_i.cyc and slave1_i.stb and (not s_stall) ;
        end if;
      end if;
    end if;
  end process;
  
  process(clk_sys_i)
  begin
   if(rising_edge(clk_sys_i)) then
      if(s_rst = '0') or slave1_out.ack = '1' then
           s_stall <= '1';
      elsif slave1_i.cyc = '1' then
		     s_stall <= '0';  
		end if; 
   end if;       		
  end process;
  
  slave1_o.dat <= s_q_o1 when  unsigned(slave1_i.adr(f_log2_size(g_size)-1 downto 0)) = 0 
                         else s_q_o;
  slave1_o.stall <= s_stall;
  slave1_o.err <= '0';
  slave1_o.rty <= '0'; 
  slave1_o.ack <= slave1_out.ack;
  
end struct;

