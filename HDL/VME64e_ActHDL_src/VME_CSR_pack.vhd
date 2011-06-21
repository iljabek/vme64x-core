library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.numeric_std.all;

--type  is array(BAR downto IRQ_level) of unsigned(7 downto 0);

use work.VME_pack.all;
package VME_CSR_pack is

     constant c_csr_array : 	t_CSRarray :=
(
BAR  => x"00", --CR/CSR BAR
BIT_SET_CLR_REG  => x"00", --Bit set register 
USR_BIT_SET_CLR_REG  => x"00", --Bit clear register
CRAM_OWNER  => x"00", --CRAM_OWNER

FUNC0_ADER_0 =>x"45",
FUNC0_ADER_1 =>x"00",
FUNC0_ADER_2 =>x"00",
FUNC0_ADER_3 =>x"80",

FUNC1_ADER_0 =>x"00",
FUNC1_ADER_1 =>x"00",

--FUNC1_ADER_2 =>x"34",
--FUNC1_ADER_3 =>x"12",     

FUNC2_ADER_0 =>x"e4",
FUNC2_ADER_1 =>x"00",
FUNC2_ADER_2 =>x"80",
FUNC2_ADER_3 =>x"00",

FUNC3_ADER_0 =>x"24",
FUNC3_ADER_1 =>x"00",
FUNC3_ADER_2 =>x"00",
FUNC3_ADER_3 =>x"80",

FUNC4_ADER_0 =>x"44",
FUNC4_ADER_1 =>x"00",
FUNC4_ADER_2 =>x"00",
FUNC4_ADER_3 =>x"00",

FUNC5_ADER_0 =>x"00",
FUNC5_ADER_1 =>x"00",
FUNC5_ADER_2 =>x"34",
FUNC5_ADER_3 =>x"11",

others => (others => '0'));


--    constant BAR : integer := 255;
--    constant BIT_SET_CLR_REG : integer := 254;
--    constant USR_BIT_SET_CLR_REG : integer := 253;
--    constant CRAM_OWNER : integer := 252;
--
--    constant FUNC7_ADER_0 : integer := 251;
--    constant FUNC7_ADER_1 : integer := FUNC7_ADER_0 - 1;
--    constant FUNC7_ADER_2 : integer := FUNC7_ADER_0 - 2;
--    constant FUNC7_ADER_3 : integer := FUNC7_ADER_0 - 3;
--    constant FUNC6_ADER_0 : integer := FUNC7_ADER_0 - 4;
--    constant FUNC6_ADER_1 : integer := FUNC7_ADER_0 - 5;
--    constant FUNC6_ADER_2 : integer := FUNC7_ADER_0 - 6;
--    constant FUNC6_ADER_3 : integer := FUNC7_ADER_0 - 7;
--    constant FUNC5_ADER_0 : integer := FUNC7_ADER_0 - 8;
--    constant FUNC5_ADER_1 : integer := FUNC7_ADER_0 - 9;
--    constant FUNC5_ADER_2 : integer := FUNC7_ADER_0 - 10;
--    constant FUNC5_ADER_3 : integer := FUNC7_ADER_0 - 11;
--    constant FUNC4_ADER_0 : integer := FUNC7_ADER_0 - 12;
--    constant FUNC4_ADER_1 : integer := FUNC7_ADER_0 - 13;
--    constant FUNC4_ADER_2 : integer := FUNC7_ADER_0 - 14;
--    constant FUNC4_ADER_3 : integer := FUNC7_ADER_0 - 15;
--    constant FUNC3_ADER_0 : integer := FUNC7_ADER_0 - 16;
--    constant FUNC3_ADER_1 : integer := FUNC7_ADER_0 - 17;
--    constant FUNC3_ADER_2 : integer := FUNC7_ADER_0 - 18;
--    constant FUNC3_ADER_3 : integer := FUNC7_ADER_0 - 19;
--    constant FUNC2_ADER_0 : integer := FUNC7_ADER_0 - 20;
--    constant FUNC2_ADER_1 : integer := FUNC7_ADER_0 - 21;
--    constant FUNC2_ADER_2 : integer := FUNC7_ADER_0 - 22;
--    constant FUNC2_ADER_3 : integer := FUNC7_ADER_0 - 23;
--    constant FUNC1_ADER_0 : integer := FUNC7_ADER_0 - 24;
--    constant FUNC1_ADER_1 : integer := FUNC7_ADER_0 - 25;
--    constant FUNC1_ADER_2 : integer := FUNC7_ADER_0 - 26;
--    constant FUNC1_ADER_3 : integer := FUNC7_ADER_0 - 27;
--    constant FUNC0_ADER_0 : integer := FUNC7_ADER_0 - 28;
--    constant FUNC0_ADER_1 : integer := FUNC7_ADER_0 - 29;
--    constant FUNC0_ADER_2 : integer := FUNC7_ADER_0 - 30;
--    constant FUNC0_ADER_3 : integer := FUNC7_ADER_0 - 31;
end VME_CSR_pack;                                                                







 












