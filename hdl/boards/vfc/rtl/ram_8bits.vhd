----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    08:56:12 05/31/2012 
-- Design Name: 
-- Module Name:    ram_8bits - Behavioral 
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
use ieee.std_logic_arith.all;
library work;
use work.genram_pkg.all;
-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity ram_8bits is
  generic (
    size       : natural := 256
	 );
    Port ( addr : in  STD_LOGIC_VECTOR (f_log2_size(size)-1 downto 0);
           di : in  STD_LOGIC_VECTOR (7 downto 0);
           do : out  STD_LOGIC_VECTOR (7 downto 0);
           we : in  STD_LOGIC;
           clk_i : in  STD_LOGIC);
end ram_8bits;

architecture Behavioral of ram_8bits is
type t_ram_type is array(size-1 downto 0) of std_logic_vector(7 downto 0);
signal sram  : t_ram_type;
begin
process (clk_i)
    begin
        if (clk_i'event and clk_i = '1') then
            if (we = '1') then
                sram(conv_integer(unsigned(addr))) <= di;
            end if;
				do <= sram(conv_integer(unsigned(addr)));
        end if;
    end process;
end Behavioral;

