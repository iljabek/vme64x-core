library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.numeric_std.all;

use work.VME_pack.all;
package VME_CR_pack is

  constant c_amcap : std_logic_vector(63 downto 0) :=
    "1111111100000000001100100000000000000000000100001111111100001011";
  constant c_amcap0 : std_logic_vector(63 downto 0) :=
    "1011101100000000101000100000000100000000000000001011101100001011";   -- added by Davide	 
   -- the function 0 support the modalities: A24, A24S, A24_BLT, A24_MBLT, A16, A32, A32_BLT, A32_MBLT, A64, A64_BLT, A64_MBLT, Two Edge 
  constant c_amcap1 : std_logic_vector(63 downto 0) :=
    "0000000000000000000000000000000000000000000000000000000000001011"; -- added by Davide for modalities A64, A64_BLT, A64_MBLT
  constant c_amcap2 : std_logic_vector(63 downto 0) :=
    "0000000000000000000000000000000100000000000000000000000000000000"; -- added by Davide for modalities TWO_edge 
	 
  constant c_xamcap0 : std_logic_vector(255 downto 0) :=
    (others => '0');   -- added by Davide	 	 
	 
  constant c_xamcap1 : std_logic_vector(255 downto 0) :=
    (others => '0');   -- added by Davide
	 
  constant c_xamcap2 : std_logic_vector(255 downto 0) :=
    x"0000000000000000000000000000000000000000000000000000000000060006";   -- added by Davide
	 
  constant c_amb : t_cr_array(0 to 7) :=(
    c_amcap(7 downto 0), c_amcap(15 downto 8),
    c_amcap(23 downto 16), c_amcap(31 downto 24),
            c_amcap(39 downto 32), c_amcap(47 downto 40),
            c_amcap(55 downto 48), c_amcap(63 downto 56));    

  constant c_amb0 : t_cr_array(0 to 7) :=(                -- Added by Davide
    c_amcap0(7 downto 0), c_amcap0(15 downto 8),
    c_amcap0(23 downto 16), c_amcap0(31 downto 24),
            c_amcap0(39 downto 32), c_amcap0(47 downto 40),
            c_amcap0(55 downto 48), c_amcap0(63 downto 56));	
  
  constant c_amb1 : t_cr_array(0 to 7) :=(                -- Added by Davide
    c_amcap1(7 downto 0), c_amcap1(15 downto 8),
    c_amcap1(23 downto 16), c_amcap1(31 downto 24),
            c_amcap1(39 downto 32), c_amcap1(47 downto 40),
            c_amcap1(55 downto 48), c_amcap1(63 downto 56));			

  	constant c_amb2 : t_cr_array(0 to 7) :=(               -- Added by Davide
    c_amcap2(7 downto 0), c_amcap2(15 downto 8),
    c_amcap2(23 downto 16), c_amcap2(31 downto 24),
            c_amcap2(39 downto 32), c_amcap2(47 downto 40),
            c_amcap2(55 downto 48), c_amcap2(63 downto 56));			
    
   constant c_xam0 : t_cr_array(0 to 31) :=(               -- Added by Davide
            c_xamcap0(7 downto 0), c_xamcap0(15 downto 8),c_xamcap0(23 downto 16), c_xamcap0(31 downto 24),
            c_xamcap0(39 downto 32), c_xamcap0(47 downto 40), c_xamcap0(55 downto 48), c_xamcap0(63 downto 56), 
				c_xamcap0(71 downto 64), c_xamcap0(79 downto 72), c_xamcap0(87 downto 80), c_xamcap0(95 downto 88),
				c_xamcap0(103 downto 96), c_xamcap0(111 downto 104), c_xamcap0(119 downto 112),c_xamcap0(127 downto 120),
				c_xamcap0(135 downto 128), c_xamcap0(143 downto 136), c_xamcap0(151 downto 144), c_xamcap0(159 downto 152),
				c_xamcap0(167 downto 160), c_xamcap0(175 downto 168), c_xamcap0(183 downto 176), c_xamcap0(191 downto 184),
				c_xamcap0(199 downto 192), c_xamcap0(207 downto 200), c_xamcap0(215 downto 208), c_xamcap0(223 downto 216),
				c_xamcap0(231 downto 224), c_xamcap0(239 downto 232), c_xamcap0(247 downto 240), c_xamcap0(255 downto 248));
				
				
	constant c_xam1 : t_cr_array(0 to 31) :=(               -- Added by Davide
            c_xamcap1(7 downto 0), c_xamcap1(15 downto 8),c_xamcap1(23 downto 16), c_xamcap1(31 downto 24),
            c_xamcap1(39 downto 32), c_xamcap1(47 downto 40), c_xamcap1(55 downto 48), c_xamcap1(63 downto 56), 
				c_xamcap1(71 downto 64), c_xamcap1(79 downto 72), c_xamcap1(87 downto 80), c_xamcap1(95 downto 88),
				c_xamcap1(103 downto 96), c_xamcap1(111 downto 104), c_xamcap1(119 downto 112),c_xamcap1(127 downto 120),
				c_xamcap1(135 downto 128), c_xamcap1(143 downto 136), c_xamcap1(151 downto 144), c_xamcap1(159 downto 152),
				c_xamcap1(167 downto 160), c_xamcap1(175 downto 168), c_xamcap1(183 downto 176), c_xamcap1(191 downto 184),
				c_xamcap1(199 downto 192), c_xamcap1(207 downto 200), c_xamcap1(215 downto 208), c_xamcap1(223 downto 216),
				c_xamcap1(231 downto 224), c_xamcap1(239 downto 232), c_xamcap1(247 downto 240), c_xamcap1(255 downto 248));	


   constant c_xam2 : t_cr_array(0 to 31) :=(               -- Added by Davide
            c_xamcap2(7 downto 0), c_xamcap2(15 downto 8),c_xamcap2(23 downto 16), c_xamcap2(31 downto 24),
            c_xamcap2(39 downto 32), c_xamcap2(47 downto 40), c_xamcap2(55 downto 48), c_xamcap1(63 downto 56), 
				c_xamcap2(71 downto 64), c_xamcap2(79 downto 72), c_xamcap2(87 downto 80), c_xamcap1(95 downto 88),
				c_xamcap2(103 downto 96), c_xamcap2(111 downto 104), c_xamcap2(119 downto 112),c_xamcap2(127 downto 120),
				c_xamcap2(135 downto 128), c_xamcap2(143 downto 136), c_xamcap2(151 downto 144), c_xamcap2(159 downto 152),
				c_xamcap2(167 downto 160), c_xamcap2(175 downto 168), c_xamcap2(183 downto 176), c_xamcap2(191 downto 184),
				c_xamcap2(199 downto 192), c_xamcap2(207 downto 200), c_xamcap2(215 downto 208), c_xamcap2(223 downto 216),
				c_xamcap2(231 downto 224), c_xamcap2(239 downto 232), c_xamcap2(247 downto 240), c_xamcap2(255 downto 248));
				
    
    constant c_cr_array : 	t_cr_array(2**12 downto 0) :=
    (
      16#00#  => (others => '0'),
-- Length of ROM
      16#01#  => x"01",
      16#02#  => x"00",
      16#03#  => x"00",
--Configuration ROM data acces width
      16#04#  => x"00",
--CSR data acces width
      16#05#  => x"81",  -- it was 01...changed by Davide
--CR/CSR Space Specification ID
      16#06#  => x"01", 		
--Ascii "C"
      16#07#  => x"43", 
--Ascii "R"
      16#08#  => x"52",
--Manufacturer's ID
      16#09#  => x"01",
      16#0A#  => x"02",
		16#0B#  => x"03",
--board id
      16#0C#  => x"03",
      16#0D#  => x"04",
      16#0E#  => x"04",
		16#0F#  => x"03",
--Rev id
      16#10#  => x"03",
      16#11#  => x"04",
      16#12#  => x"04",
		16#13#  => x"03",
--Point to ascii null terminatied
      16#14#  => x"00",
      16#15#  => x"00",
		16#16#  => x"00",  
--Program Id code
      16#1F#  => x"12",
--Offset to BEG_USER_CR    --Added by Davide
		16#20#  => x"00",
		16#21#  => x"00",
		16#22#  => x"00",
--Offset to END_USER_CR    --Added by Davide
      16#23#  => x"00",
		16#24#  => x"00",
		16#25#  => x"00",
--Offset to BEG_CRAM       --Added by Davide
      16#26#  => x"00",
		16#27#  => x"10",    --10
		16#28#  => x"00",    --00
--Offset to END_CRAM       --Added by Davide
      16#29#  => x"07",   
		16#2A#  => x"fb",   
		16#2B#  => x"ef",   
--Offset to BEG_USER_CSR   --Added by Davide
      16#2C#  => x"07",
		16#2D#  => x"fb",
		16#2E#  => x"f0",    --NB: 0x7fbf0 and NOT 0x7fbf3 because is possible access with D32 mode
--Offset to END_USER_CSR   --Added by Davide
      16#2F#  => x"07",
		16#30#  => x"fb",
		16#31#  => x"ff",
--CRAM_ACCESS_WIDTH
      16#39#  => x"81",
--Function data access width
      16#40#  => x"85", -- Fun 0 accepts MD32, D16, D08(EO) cycles
      16#41#  => x"85", -- Fun 1 
      16#42#  => x"85", -- Fun 2 
      16#43#  => x"85", -- Fun 3

      16#44#  => x"85", -- Fun 4
      16#45#  => x"85", -- Fun 5
      16#46#  => x"85", -- Fun 6
      16#47#  => x"85", -- Fun 7


--Function AM code Mask
      16#48#  => c_amb0(7), -- Fun 0    --modified by Davide
      16#49#  => c_amb0(6), -- Fun 0 
      16#4A#  => c_amb0(5), -- Fun 0 
      16#4B#  => c_amb0(4), -- Fun 0  

      16#4C#  => c_amb0(3), -- Fun 0 
      16#4D#  => c_amb0(2), -- Fun 0 
      16#4E#  => c_amb0(1), -- Fun 0  
      16#4F#  => c_amb0(0), -- Fun 0

      16#50#  => c_amb1(7), -- Fun 1    --modified by Davide   
      16#51#  => c_amb1(6), -- Fun 1 
      16#52#  => c_amb1(5), -- Fun 1 
      16#53#  => c_amb1(4), -- Fun 1 
      16#54#  => c_amb1(3), -- Fun 1 
      16#55#  => c_amb1(2), -- Fun 1 
      16#56#  => c_amb1(1), -- Fun 1  
      16#57#  => c_amb1(0), -- Fun 1


      16#58#  => x"00", -- Fun 1_b     --modified by Davide
      16#59#  => x"00", -- Fun 1_b  
      16#5A#  => x"00", -- Fun 1_b
      16#5B#  => x"00", -- Fun 1_b 
      16#5C#  => x"00", -- Fun 1_b 
      16#5D#  => x"00", -- Fun 1_b 
      16#5E#  => x"00", -- Fun 1_b 
      16#5F#  => x"00", -- Fun 1_b


      16#60#  => c_amb2(7), -- Fun 2 
      16#61#  => c_amb2(6), -- Fun 2 
      16#62#  => c_amb2(5), -- Fun 2 
      16#63#  => c_amb2(4), -- Fun 2 
      16#64#  => c_amb2(3), -- Fun 2 
      16#65#  => c_amb2(2), -- Fun 2 
      16#66#  => c_amb2(1), -- Fun 2 
      16#67#  => c_amb2(0), -- Fun 2


      16#68#  => x"00", -- Fun 2_b
      16#69#  => x"00", -- Fun 2_b 
      16#6A#  => x"00", -- Fun 2_b 
      16#6B#  => x"00", -- Fun 2_b 
      16#6C#  => x"00", -- Fun 2_b 
      16#6D#  => x"00", -- Fun 2_b 
      16#6E#  => x"00", -- Fun 2_b 
      16#6F#  => x"00", -- Fun 2_b

      16#70#  => x"00", -- Fun 3 
      16#71#  => x"00", -- Fun 3 
      16#72#  => x"00", -- Fun 3 
      16#73#  => x"01", -- Fun 3
      16#74#  => x"00", -- Fun 3 
      16#75#  => x"00", -- Fun 3 
      16#76#  => x"00", -- Fun 3 
      16#77#  => x"00", -- Fun 3

      16#78#  => x"00", -- Fun 4 
      16#79#  => x"00", -- Fun 4 
      16#7A#  => x"00", -- Fun 4 
      16#7B#  => x"00", -- Fun 4
      16#7C#  => x"00", -- Fun 4 
      16#7D#  => x"00", -- Fun 4 
      16#7E#  => x"0b", -- Fun 4 
      16#7F#  => x"00", -- Fun 4


--Xamcap
      16#88#  => c_xam0(31), -- Fun 0  XAMCAP MSB
		16#89#  => c_xam0(30),
		16#8A#  => c_xam0(29),
		16#8B#  => c_xam0(28),
		16#8C#  => c_xam0(27),
		16#8D#  => c_xam0(26),
		16#8E#  => c_xam0(25),
		16#8F#  => c_xam0(24),
		16#90#  => c_xam0(23),
		16#91#  => c_xam0(22),
		16#92#  => c_xam0(21),
		16#93#  => c_xam0(20),
		16#94#  => c_xam0(19),
		16#95#  => c_xam0(18),
		16#96#  => c_xam0(17),
		16#97#  => c_xam0(16),
		16#98#  => c_xam0(15),
		16#99#  => c_xam0(14),
		16#9A#  => c_xam0(13),
		16#9B#  => c_xam0(12),
		16#9C#  => c_xam0(11),
		16#9D#  => c_xam0(10),
		16#9E#  => c_xam0(9),
		16#9F#  => c_xam0(8),
		16#A0#  => c_xam0(7),
		16#A1#  => c_xam0(6),
		16#A2#  => c_xam0(5),
		16#A3#  => c_xam0(4),
		16#A4#  => c_xam0(3),
		16#A5#  => c_xam0(2),
		16#A6#  => c_xam0(1),
		16#A7#  => c_xam0(0),
		
 
      16#A8#  => c_xam1(31),         -- Fun 1  XAMCAP MSB
		16#A9#  => c_xam1(30),
		16#AA#  => c_xam1(29),
		16#AB#  => c_xam1(28),
		16#AC#  => c_xam1(27),
		16#AD#  => c_xam1(26),
		16#AE#  => c_xam1(25),
		16#AF#  => c_xam1(24),
		16#B0#  => c_xam1(23),
		16#B1#  => c_xam1(22),
		16#B2#  => c_xam1(21),
		16#B3#  => c_xam1(20),
		16#B4#  => c_xam1(19),
		16#B5#  => c_xam1(18),
		16#B6#  => c_xam1(17),
		16#B7#  => c_xam1(16),
		16#B8#  => c_xam1(15),
		16#B9#  => c_xam1(14),
		16#BA#  => c_xam1(13),
		16#BB#  => c_xam1(12),
		16#BC#  => c_xam1(11),
		16#BD#  => c_xam1(10),
		16#BE#  => c_xam1(9),
		16#BF#  => c_xam1(8),
		16#C0#  => c_xam1(7),
		16#C1#  => c_xam1(6),
		16#C2#  => c_xam1(5),
		16#C3#  => c_xam1(4),
		16#C4#  => c_xam1(3),
		16#C5#  => c_xam1(2),
		16#C6#  => c_xam1(1),
		16#C7#  => c_xam1(0),
		
		16#C8#  => x"00",         -- Fun 1_b  XAMCAP MSB
		16#C9#  => x"00",
		16#CA#  => x"00",
		16#CB#  => x"00",
		16#CC#  => x"00",
		16#CD#  => x"00",
		16#CE#  => x"00",
		16#CF#  => x"00",
		16#D0#  => x"00",
		16#D1#  => x"00",
		16#D2#  => x"00",
		16#D3#  => x"00",
		16#D4#  => x"00",
		16#D5#  => x"00",
		16#D6#  => x"00",
		16#D7#  => x"00",
		16#D8#  => x"00",
		16#D9#  => x"00",
		16#DA#  => x"00",
		16#DB#  => x"00",
		16#DC#  => x"00",
		16#DD#  => x"00",
		16#DE#  => x"00",
		16#DF#  => x"00",
		16#E0#  => x"00",
		16#E1#  => x"00",
		16#E2#  => x"00",
		16#E3#  => x"00",
		16#E4#  => x"00",
		16#E5#  => x"00",
		16#E6#  => x"00",
		16#E7#  => x"00",
		
      16#E8#  => c_xam2(31),         -- Fun 2  XAMCAP MSB
		16#E9#  => c_xam2(30),
		16#EA#  => c_xam2(29),
		16#EB#  => c_xam2(28),
		16#EC#  => c_xam2(27),
		16#ED#  => c_xam2(26),
		16#EE#  => c_xam2(25),
		16#EF#  => c_xam2(24),
		16#F0#  => c_xam2(23),
		16#F1#  => c_xam2(22),
		16#F2#  => c_xam2(21),
		16#F3#  => c_xam2(20),
		16#F4#  => c_xam2(19),
		16#F5#  => c_xam2(18),
		16#F6#  => c_xam2(17),
		16#F7#  => c_xam2(16),
		16#F8#  => c_xam2(15),
		16#F9#  => c_xam2(14),
		16#FA#  => c_xam2(13),
		16#FB#  => c_xam2(12),
		16#FC#  => c_xam2(11),
		16#FD#  => c_xam2(10),
		16#FE#  => c_xam2(9),
		16#FF#  => c_xam2(8),
		16#100#  => c_xam2(7),
		16#101#  => c_xam2(6),
		16#102#  => c_xam2(5),
		16#103#  => c_xam2(4),
		16#104#  => c_xam2(3),
		16#105#  => c_xam2(2),
		16#106#  => c_xam2(1),
		16#107#  => c_xam2(0),
		
      16#108#  => x"00", -- Fun 2_b  XAMCAP MSB
      16#109#  => x"00", -- Fun 2_b  XAMCAP=0x11
      16#10a#  => x"00",  -- Fun 2_b  

--...

--16#C6#  => x"00", -- Fun 0  XAMCAP LSB
--16#C7#  => x"01", -- Fun 0  XAMCAP LSB
--......

-- Address Decoder Mask ADEM
      16#188#  => x"ff", -- Fun 0 
      16#189#  => x"ff", -- Fun 0 
      16#18A#  => x"f8", -- Fun 0 
      16#18B#  => x"00", -- Fun 0 

      16#18c#  => x"00", -- Fun 1 
      16#18d#  => x"00", -- Fun 1 
      16#18e#  => x"00", -- Fun 1 
      16#18f#  => x"01", -- Fun 1 

      16#190#  => x"ff", -- Fun 1_b 
      16#191#  => x"00", -- Fun 1_b
      16#192#  => x"00", -- Fun 1_b 
      16#193#  => x"00", -- Fun 1_b

      16#194#  => x"00", -- Fun 2 
      16#195#  => x"00", -- Fun 2 
      16#196#  => x"00", -- Fun 2 
      16#197#  => x"01", -- Fun 2

      16#198#  => x"ff", -- Fun 2_b 
      16#199#  => x"ff", -- Fun 2_b 
      16#19a#  => x"ff", -- Fun 2_b 
      16#19b#  => x"f0", -- Fun 2_b


      16#19c#  => x"ff", -- Fun 5 
      16#19d#  => x"00", -- Fun 5 
      16#19e#  => x"00", -- Fun 5 
      16#19f#  => x"00", -- Fun 5

      16#1a0#  => x"ff", -- Fun 6 
      16#1a1#  => x"00", -- Fun 6 
      16#1a2#  => x"00", -- Fun 6 
      16#1a3#  => x"00", -- Fun 6


      others => (others => '0'));

    end VME_CR_pack;                                                                




















