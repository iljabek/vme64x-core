----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    17:42:21 03/12/2012 
-- Design Name: 
-- Module Name:    Buffer - Behavioral 
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

entity Buff is
    Port ( input : in  STD_LOGIC;
           output : out  STD_LOGIC;
           en : in  STD_LOGIC);
end Buff;

architecture Behavioral of Buff is

begin
process(en,input)
begin
  if en = '1' then 
     output <= input;
  else 	
     output <= 'Z';
  end if;
end process;  

end Behavioral;

