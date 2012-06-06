----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    17:57:26 03/12/2012 
-- Design Name: 
-- Module Name:    gen_buff - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: 
--
-- Dependencies: 
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: 
--
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity gen_buff is
  generic (n : natural);
  port (input: in std_logic_vector(n-1 downto 0);
        en : in std_logic;
        output : out std_logic_vector(n-1 downto 0));
end;		  

architecture Behavioral of gen_buff is
  component Buff
	port(
		input : in std_logic;
		en : in std_logic;          
		output : out std_logic
		);
	end component;
begin
  gen : for i in 0 to n-1 generate
  begin
     buff1 : Buff port map (input => input(i), en => en, output => output(i));
  end generate;
end Behavioral;

