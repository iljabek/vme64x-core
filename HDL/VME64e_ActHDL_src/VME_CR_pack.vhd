library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.numeric_std.all;

use work.VME_pack.all;
package VME_CR_pack is
     constant c_cr_array : 	t_cr_array(2**12 downto 0) :=
(
16#03#  => (others => '0'),
-- Length of ROM
16#07#  => x"01",
16#0b#  => x"00",
16#0f#  => x"00",
--Configuration ROM data acces width
16#13#  => x"00",
--Configuration ROM data acces width
16#1b#  => x"01",
--Ascii "C"
16#1F#  => x"01", 
--Ascii "R"
16#23#  => x"43",
--Manufacturer's ID
16#27#  => x"52",
16#2b#  => x"01",
16#2f#  => x"02",
--board id
16#33#  => x"03",
16#37#  => x"03",
16#3b#  => x"04",
16#3f#  => x"04",
--Rev id
16#43#  => x"03",
16#47#  => x"03",
16#4b#  => x"04",
16#4f#  => x"04",
--Point to ascii null terminatied
16#53#  => x"03",
16#57#  => x"03",
16#5b#  => x"04",
--Program Id code
16#7f#  => x"12",

--Function data access width
16#103#  => x"84", -- Fun 0 D32
--16#107#  => x"12", -- Fun 1 
--16#10b#  => x"12", -- Fun 2 
--16#10f#  => x"12", -- Fun 3

--Function AM code Mask
16#123#  => x"00", -- Fun 0 
16#127#  => x"00", -- Fun 0 
16#12b#  => x"00", -- Fun 0 
16#12f#  => x"80", -- Fun 0 AM=39

16#133#  => x"00", -- Fun 0 
16#137#  => x"00", -- Fun 0 
16#13b#  => x"00", -- Fun 0 
16#13f#  => x"00", -- Fun 0

--16#143#  => x"12", -- Fun 1 
--16#147#  => x"12", -- Fun 1 
--16#14b#  => x"12", -- Fun 1 
--16#14f#  => x"12", -- Fun 1 
--16#153#  => x"12", -- Fun 1 
--16#157#  => x"12", -- Fun 1 
--16#15b#  => x"12", -- Fun 1 
--16#15f#  => x"12", -- Fun 1
--
--
--16#163#  => x"12", -- Fun 2 
--16#167#  => x"12", -- Fun 2 
--16#16b#  => x"12", -- Fun 2 
--16#16f#  => x"12", -- Fun 2 
--16#1733#  => x"12", -- Fun 2 
--16#177#  => x"12", -- Fun 2 
--16#17b#  => x"12", -- Fun 2 
--16#17f#  => x"12", -- Fun 2
--
--
--16#183#  => x"12", -- Fun 3 
--16#187#  => x"12", -- Fun 3 
--16#18b#  => x"12", -- Fun 3 
--16#18f#  => x"12", -- Fun 3 
--16#193#  => x"12", -- Fun 3 
--16#197#  => x"12", -- Fun 3 
--16#19b#  => x"12", -- Fun 3 
--16#19f#  => x"12", -- Fun 3




-- Address Decoder Mask ADEM
16#633#  => x"00", -- Fun 0 
16#637#  => x"ff", -- Fun 0 
16#63b#  => x"00", -- Fun 0 
16#63f#  => x"80", -- Fun 0 Fixed decoder

others => (others => '0'));

end VME_CR_pack;                                                                




















