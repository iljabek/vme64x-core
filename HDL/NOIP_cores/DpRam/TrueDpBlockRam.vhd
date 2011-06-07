--==============================================================--
--Design Units : CTX1 Control and Statistics
--Size:		
--Speed:	 	 
--File Name: 	 MebRam.vhd
--
--Purpose:	The dpblockram implements a synthetisable model of a 
--				dual port RAM.
--				There are an input data and addr ports to allow the
--				writing at the reception of a GMT frame.
--				The output data and addr ports allow a simultanous
--				reading of the circular buffer by the user, at the
--				same time than it is beeing written.
--				
--				The frame and the millisecond stamp are stored in the
--				same ram word. It is the task of the MEB block to 
--				separate the frame data from the millisecond stamp data. 
--
--Limitations:	
--
--Errors:
--
--Libraries: 
--
--Dependancies:  It instantiates a synthetisable model of a DPRAM
--					  See MebRam.vhd
--
--Author: Pablo Antonio Alvarez Sanchez
--		  	 European Organisation for Nuclear Research
--		 	 SL SPS/LHC -- Control -- Timing Division
--		 	 CERN, Geneva, Switzerland,  CH-1211
--		 	 Building 864 Room 1 - A24
--
--Simulator:               ModelSim XE 5.5e_p1
--==============================================================--
--Revision List
--Version Author Date		 Changes
--
--1.0		 PAAS	  30.09.2002 Added comments, tested with the
--										rest of the design
--==============================================================--

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

 entity TrueDpblockram is
 generic (dl : integer := 42; 		-- Length of the data word 
 			 al : integer := 10);			-- Size of the addr map (10 = 1024 words)

			  									-- 'nw' has to be coherent with 'al'

 port (clk_a_i  : in std_logic; 			-- Global Clock
 	we_a_i   : in std_logic; 				-- Write Enable
 	a_a_i    : in std_logic_vector(al - 1 downto 0); -- Write Address 
 	di_a_i   : in std_logic_vector(dl - 1 downto 0);  -- Data input
 	do_a_o  : out std_logic_vector(dl - 1 downto 0);  -- Data write, normaly open

clk_b_i  : in std_logic; 			-- Global Clock
 	we_b_i   : in std_logic; 				-- Write Enable
 	a_b_i    : in std_logic_vector(al - 1 downto 0); -- Write Address 
 	di_b_i   : in std_logic_vector(dl - 1 downto 0);  -- Data input
 	do_b_o  : out std_logic_vector(dl - 1 downto 0));  -- Data write, normaly open
           

 end TrueDpblockram; 
 												 -- DATA OUTPUT NOT REGISTERED!
--library synplify;
--use synplify.attributes.all;
 architecture syn of TrueDpblockram is 
 


type t_ram is array (2**al-1 downto 0) of std_logic_vector (dl-1 downto 0);
shared variable ram: t_ram;

begin 


process (clk_a_i)
begin
   if (clk_a_i'event and clk_a_i = '1') then
--      if (<enableA> = '1') then
         if (we_a_i = '1') then
            ram(conv_integer(a_a_i)) := di_a_i;
         end if;
         do_a_o <= ram(conv_integer(a_a_i));
--      end if;
   end if;
end process;

process (clk_b_i)
begin
   if (clk_b_i'event and clk_b_i = '1') then
 --     if (<enableB> = '1') then
         if (we_b_i = '1') then
            ram(conv_integer(a_b_i)) := di_b_i;
         end if;
         do_b_o <= ram(conv_integer(a_b_i));
      end if;
 --  end if;
end process;

 end syn;

 
