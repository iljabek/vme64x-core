library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.numeric_std.all;

use work.VME_pack.all;
package VME_CR_pack is
     constant c_cr_array : 	t_cr_array(2**12 downto 0) :=
(
16#00#  => (others => '0'),
-- Length of ROM
16#01#  => x"01",
16#02#  => x"00",
16#03#  => x"00",
--Configuration ROM data acces width
16#04#  => x"00",
--Configuration ROM data acces width
16#05#  => x"01",
--Ascii "C"
16#06#  => x"01", 
--Ascii "R"
16#07#  => x"43",
--Manufacturer's ID
16#08#  => x"52",
16#09#  => x"01",
16#0A#  => x"02",
--board id
16#0B#  => x"03",
16#0C#  => x"03",
16#0D#  => x"04",
16#0E#  => x"04",
--Rev id
16#0F#  => x"03",
16#10#  => x"03",
16#11#  => x"04",
16#12#  => x"04",
--Point to ascii null terminatied
16#13#  => x"03",
16#14#  => x"03",
16#15#  => x"04",
--Program Id code
16#1E#  => x"12",

--Function data access width
16#40#  => x"85", -- Fun 0 D32
16#41#  => x"85", -- Fun 1 D32
16#42#  => x"85", -- Fun 2 
16#43#  => x"85", -- Fun 3

16#44#  => x"85", -- Fun 4
16#45#  => x"85", -- Fun 5
16#46#  => x"85", -- Fun 6
16#47#  => x"85", -- Fun 7


--Function AM code Mask
16#48#  => x"00", -- Fun 0  
16#49#  => x"00", -- Fun 0 
16#4A#  => x"00", -- Fun 0 
16#4B#  => x"01", -- Fun 0  X"01" AM=20

16#4C#  => x"00", -- Fun 0 
16#4D#  => x"00", -- Fun 0 
16#4E#  => x"00", -- Fun 0  
16#4F#  => x"00", -- Fun 0

16#50#  => x"03", -- Fun 1  x"02" AM=39, AM=38
16#51#  => x"00", -- Fun 1 
16#52#  => x"00", -- Fun 1 
16#53#  => x"00", -- Fun 1 
16#54#  => x"00", -- Fun 1 
16#55#  => x"00", -- Fun 1 
16#56#  => x"00", -- Fun 1  
16#57#  => x"00", -- Fun 1
--
--
16#58#  => x"03", -- Fun 2  x"02" AM=39, AM=38
16#59#  => x"00", -- Fun 2  
16#5a#  => x"00", -- Fun 2 
16#5b#  => x"00", -- Fun 2 
16#5c#  => x"00", -- Fun 2 
16#5d#  => x"00", -- Fun 2 
16#5e#  => x"00", -- Fun 2  X"10" AM=0c
16#5f#  => x"00", -- Fun 2
--
--
16#60#  => x"00", -- Fun 3 
16#61#  => x"00", -- Fun 3 
16#62#  => x"00", -- Fun 3 
16#63#  => x"00", -- Fun 3 
16#64#  => x"00", -- Fun 3 
16#65#  => x"00", -- Fun 3 
16#66#  => x"03", -- Fun 3 
16#67#  => x"00", -- Fun 3

--
16#68#  => x"00", -- Fun 4
16#69#  => x"00", -- Fun 4 
16#6a#  => x"00", -- Fun 4 
16#6b#  => x"00", -- Fun 4 
16#6c#  => x"00", -- Fun 4 
16#6d#  => x"00", -- Fun 4 
16#6e#  => x"03", -- Fun 4 
16#6f#  => x"00", -- Fun 4
--
16#70#  => x"00", -- Fun 5 
16#71#  => x"00", -- Fun 5 
16#72#  => x"00", -- Fun 5 
16#73#  => x"01", -- Fun 5
16#74#  => x"00", -- Fun 5 
16#75#  => x"00", -- Fun 5 
16#76#  => x"00", -- Fun 5 
16#77#  => x"00", -- Fun 5


--XAMCAP
16#88#  => x"00", -- Fun 0  XAMCAP MSB
16#a5#  => x"06", -- Fun 0  XAMCAP=0x11
16#A7# => x"00",

16#108#  => x"00", -- Fun 4  XAMCAP MSB
16#109#  => x"06", -- Fun 4  XAMCAP=0x11
16#10a# => x"00", -- Fun 4  

--...

--16#C6#  => x"00", -- Fun 0  XAMCAP LSB
--16#C7#  => x"01", -- Fun 0  XAMCAP LSB
--......

-- Address Decoder Mask ADEM
16#188#  => x"f0", -- Fun 0 
16#189#  => x"00", -- Fun 0 
16#18A#  => x"00", -- Fun 0 
16#18B#  => x"02", -- Fun 0 

16#18c#  => x"ff", -- Fun 1 
16#18d#  => x"ff", -- Fun 1 
16#18e#  => x"00", -- Fun 1 
16#18f#  => x"00", -- Fun 1 

16#190#  => x"00", -- Fun 2 
16#191#  => x"e0", -- Fun 2 
16#192#  => x"00", -- Fun 2 
16#193#  => x"00", -- Fun 2

16#194#  => x"ff", -- Fun 3 
16#195#  => x"00", -- Fun 3 
16#196#  => x"00", -- Fun 3 
16#197#  => x"00", -- Fun 3

others => (others => '0'));

end VME_CR_pack;                                                                




















