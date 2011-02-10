library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.numeric_std.all;

package VME_pack is
  
    type t_reg38x8bit is array(37 downto 0) of unsigned(7 downto 0);
    type t_reg52x8bit is array(51 downto 0) of unsigned(7 downto 0);
    type t_reg52x12bit is array(51 downto 0) of unsigned(11 downto 0);
    type t_cr_array is array (Natural range <>) of std_logic_vector(7 downto 0);
	 
	 type t_rom_cell is 
	 record
	   add : integer;
		len : integer;
	 end record;
	 
    type t_cr_struc is array (Natural range <>) of t_rom_cell;
    
	 constant c_cr_step : integer := 4;
    constant BAR : integer := 0;
    constant BIT_SET_CLR_REG : integer := 1;
    constant CRAM_OWNER : integer := 2;
    constant USR_BIT_SET_CLR_REG : integer := 3;
    constant FUNC7_ADER_0 : integer := 4;
    constant FUNC7_ADER_1 : integer := 5;
    constant FUNC7_ADER_2 : integer := 6;
    constant FUNC7_ADER_3 : integer := 7;
    constant FUNC6_ADER_0 : integer := 8;
    constant FUNC6_ADER_1 : integer := 9;
    constant FUNC6_ADER_2 : integer := 10;
    constant FUNC6_ADER_3 : integer := 11;
    constant FUNC5_ADER_0 : integer := 12;
    constant FUNC5_ADER_1 : integer := 13;
    constant FUNC5_ADER_2 : integer := 14;
    constant FUNC5_ADER_3 : integer := 15;
    constant FUNC4_ADER_0 : integer := 16;
    constant FUNC4_ADER_1 : integer := 17;
    constant FUNC4_ADER_2 : integer := 18;
    constant FUNC4_ADER_3 : integer := 19;
    constant FUNC3_ADER_0 : integer := 20;
    constant FUNC3_ADER_1 : integer := 21;
    constant FUNC3_ADER_2 : integer := 22;
    constant FUNC3_ADER_3 : integer := 23;
    constant FUNC2_ADER_0 : integer := 24;
    constant FUNC2_ADER_1 : integer := 25;
    constant FUNC2_ADER_2 : integer := 26;
    constant FUNC2_ADER_3 : integer := 27;
    constant FUNC1_ADER_0 : integer := 28;
    constant FUNC1_ADER_1 : integer := 29;
    constant FUNC1_ADER_2 : integer := 30;
    constant FUNC1_ADER_3 : integer := 31;
    constant FUNC0_ADER_0 : integer := 32;
    constant FUNC0_ADER_1 : integer := 33;
    constant FUNC0_ADER_2 : integer := 34;
    constant FUNC0_ADER_3 : integer := 35;
    constant IRQ_ID : integer := 36;
    constant IRQ_level : integer := 37;
 


--0x7FFFF CR/CSR (BAR) 1 byte VME64
--        Base Address Register 
--0x7FFFB Bit Set Register 1 byte VME64
--        see Table 10-6 
--0x7FFF7 Bit Clear Register 1 byte VME64
--        see Table 10-7 
--0x7FFF3 CRAM_OWNER Register 1 byte VME64x
--0x7FFEF User-Defined Bit Set 1 byte VME64x
--        Register 
--0x7FFEB User-Defined Bit Clear 1 byte VME64x
--        Register 
--0x7FFE3 ... 0x7FFE7 RESERVED 2 bytes VME64x
--0x7FFD3 ... 0x7FFDF Function 7 ADER 4 bytes VME64x
--                    see Table 10-8 
--0x7FFC3 ... 0x7FFCF Function 6 ADER 4 bytes VME64x
--0x7FFB3 ... 0x7FFBF Function 5 ADER 4 bytes VME64x
--0x7FFA3 ... 0x7FFAF Function 4 ADER 4 bytes VME64x
--0x7FF93 ... 0x7FF9F Function 3 ADER 4 bytes VME64x
--0x7FF83 ... 0x7FF8F Function 2 ADER 4 bytes VME64x
--0x7FF73 ... 0x7FF7F Function 1 ADER 4 bytes VME64x
--0x7FF63 ... 0x7FF6F Function 0 ADER 4 bytes VME64x
--0x7FC00 ... 0x7FF5F RESERVED 216 bytes VME64x
--
-------------------
    constant BAR_addr : integer := 16#7FFFF#;        
    constant BIT_SET_REG_addr : integer := 16#7FFFB#;   
    constant BIT_CLR_REG_addr : integer := 16#7FFF7#;   
    constant CRAM_OWNER_addr : integer := 16#7FFF3#;    
    constant USR_BIT_SET_REG_addr : integer := 16#7FFEF#;   
    constant USR_BIT_CLR_REG_addr : integer := 16#7FFEB#; 
--Reserved 16#7FFE7#;   
--Reserved 16#7FFE3#;   
    constant FUNC7_ADER_0_addr : integer := 16#7FFDF#;   
    constant FUNC7_ADER_1_addr : integer := 16#7FFDB#;   
    constant FUNC7_ADER_2_addr : integer := 16#7FFD7#;   
    constant FUNC7_ADER_3_addr : integer := 16#7FFD3#;
	 
    constant FUNC6_ADER_0_addr : integer := 16#7FFCF#;   
    constant FUNC6_ADER_1_addr : integer := 16#7FFCB#;      
    constant FUNC6_ADER_2_addr : integer := 16#7FFC7#;      
    constant FUNC6_ADER_3_addr : integer := 16#7FFC3#; 
	 
    constant FUNC5_ADER_0_addr : integer := 16#7FFBF#;      
    constant FUNC5_ADER_1_addr : integer := 16#7FFBB#;      
    constant FUNC5_ADER_2_addr : integer := 16#7FFB7#;      
    constant FUNC5_ADER_3_addr : integer := 16#7FFB3#;
	 
    constant FUNC4_ADER_0_addr : integer := 16#7FFAF#;      
    constant FUNC4_ADER_1_addr : integer := 16#7FFAB#;      
    constant FUNC4_ADER_2_addr : integer := 16#7FFA7#;      
    constant FUNC4_ADER_3_addr : integer := 16#7FFA3#;
	 
    constant FUNC3_ADER_0_addr : integer := 16#7FF9F#;      
    constant FUNC3_ADER_1_addr : integer := 16#7FF9B#;      
    constant FUNC3_ADER_2_addr : integer := 16#7FF97#;      
    constant FUNC3_ADER_3_addr : integer := 16#7FF93#;
	 
    constant FUNC2_ADER_0_addr : integer := 16#7FF8F#;      
    constant FUNC2_ADER_1_addr : integer := 16#7FF8B#;      
    constant FUNC2_ADER_2_addr : integer := 16#7FF87#;     
    constant FUNC2_ADER_3_addr : integer := 16#7FF83#;
	 
    constant FUNC1_ADER_0_addr : integer := 16#7FF7F#;      
    constant FUNC1_ADER_1_addr : integer := 16#7FF7B#;      
    constant FUNC1_ADER_2_addr : integer := 16#7FF77#;      
    constant FUNC1_ADER_3_addr : integer := 16#7FF73#;
	 
    constant FUNC0_ADER_0_addr : integer := 16#7FF6F#;      
    constant FUNC0_ADER_1_addr : integer := 16#7FF6B#;      
    constant FUNC0_ADER_2_addr : integer := 16#7FF67#;     
    constant FUNC0_ADER_3_addr : integer := 16#7FF63#; 
	 
    constant IRQ_ID_addr : integer := 16#7fbff#;   
    constant IRQ_level_addr : integer := 16#7fbef#;   
----------------------------------
 
 
 
 
 
 
--    constant BAR_addr : integer := to_integer(unsigned(bit_vector("1111111111111111111")));        
--    constant BIT_SET_REG_addr : integer := to_integer(unsigned(bit_vector("1111111111111111011")));
--    constant BIT_CLR_REG_addr : integer := to_integer(unsigned(bit_vector("1111111111111110111")));
--    constant CRAM_OWNER_addr : integer := to_integer(unsigned(bit_vector("1111111111111110011")));   
--    constant USR_BIT_SET_REG_addr : integer := to_integer(unsigned(bit_vector("1111111111111101111")));
--    constant USR_BIT_CLR_REG_addr : integer := to_integer(unsigned(bit_vector("1111111111111101011")));
--    constant FUNC7_ADER_0_addr : integer := to_integer(unsigned(bit_vector("1111111111111011111")));   
--    constant FUNC7_ADER_1_addr : integer := to_integer(unsigned(bit_vector("1111111111111011011")));   
--    constant FUNC7_ADER_2_addr : integer := to_integer(unsigned(bit_vector("1111111111111010111")));   
--    constant FUNC7_ADER_3_addr : integer := to_integer(unsigned(bit_vector("1111111111111010011")));   
--    constant FUNC6_ADER_0_addr : integer := to_integer(unsigned(bit_vector("1111111111111001111")));   
--    constant FUNC6_ADER_1_addr : integer := to_integer(unsigned(bit_vector("1111111111111001011")));   
--    constant FUNC6_ADER_2_addr : integer := to_integer(unsigned(bit_vector("1111111111111000111")));   
--    constant FUNC6_ADER_3_addr : integer := to_integer(unsigned(bit_vector("1111111111111000011")));   
--    constant FUNC5_ADER_0_addr : integer := to_integer(unsigned(bit_vector("1111111111110111111")));   
--    constant FUNC5_ADER_1_addr : integer := to_integer(unsigned(bit_vector("1111111111110111011")));   
--    constant FUNC5_ADER_2_addr : integer := to_integer(unsigned(bit_vector("1111111111110110111")));   
--    constant FUNC5_ADER_3_addr : integer := to_integer(unsigned(bit_vector("1111111111110110011")));   
--    constant FUNC4_ADER_0_addr : integer := to_integer(unsigned(bit_vector("1111111111110101111")));   
--    constant FUNC4_ADER_1_addr : integer := to_integer(unsigned(bit_vector("1111111111110101011")));   
--    constant FUNC4_ADER_2_addr : integer := to_integer(unsigned(bit_vector("1111111111110100111")));   
--    constant FUNC4_ADER_3_addr : integer := to_integer(unsigned(bit_vector("1111111111110100011")));   
--    constant FUNC3_ADER_0_addr : integer := to_integer(unsigned(bit_vector("1111111111110011111")));   
--    constant FUNC3_ADER_1_addr : integer := to_integer(unsigned(bit_vector("1111111111110011011")));   
--    constant FUNC3_ADER_2_addr : integer := to_integer(unsigned(bit_vector("1111111111110010111")));   
--    constant FUNC3_ADER_3_addr : integer := to_integer(unsigned(bit_vector("1111111111110010011")));   
--    constant FUNC2_ADER_0_addr : integer := to_integer(unsigned(bit_vector("1111111111110001111")));   
--    constant FUNC2_ADER_1_addr : integer := to_integer(unsigned(bit_vector("1111111111110001011")));   
--    constant FUNC2_ADER_2_addr : integer := to_integer(unsigned(bit_vector("1111111111110000111")));   
--    constant FUNC2_ADER_3_addr : integer := to_integer(unsigned(bit_vector("1111111111110000011")));   
--    constant FUNC1_ADER_0_addr : integer := to_integer(unsigned(bit_vector("1111111111101111111")));   
--    constant FUNC1_ADER_1_addr : integer := to_integer(unsigned(bit_vector("1111111111101111011")));   
--    constant FUNC1_ADER_2_addr : integer := to_integer(unsigned(bit_vector("1111111111101110111")));   
--    constant FUNC1_ADER_3_addr : integer := to_integer(unsigned(bit_vector("1111111111101110011")));   
--    constant FUNC0_ADER_0_addr : integer := to_integer(unsigned(bit_vector("1111111111101101111")));   
--    constant FUNC0_ADER_1_addr : integer := to_integer(unsigned(bit_vector("1111111111101101011")));   
--    constant FUNC0_ADER_2_addr : integer := to_integer(unsigned(bit_vector("1111111111101100111")));   
--    constant FUNC0_ADER_3_addr : integer := to_integer(unsigned(bit_vector("1111111111101100011")));
--    constant IRQ_ID_addr : integer := to_integer(unsigned(bit_vector("1111111101111111111")));
--    constant IRQ_level_addr : integer := to_integer(unsigned(bit_vector("1111111101111111011")));
---------------------------------------------------------------------------    

---------------------------------------------------------------------------
    constant BEG_USER_CR_0: integer :=        1;
    constant BEG_USER_CR_1: integer :=        2;
    constant BEG_USER_CR_2: integer :=        3;
    constant END_USER_CR_0: integer :=        4;
    constant END_USER_CR_1: integer :=        5;
    constant END_USER_CR_2: integer :=        6;
    constant BEG_CRAM_0: integer :=           7;
    constant BEG_CRAM_1: integer :=           8;
    constant BEG_CRAM_2: integer :=           9;
    constant END_CRAM_0: integer :=           10;
    constant END_CRAM_1: integer :=           11;
    constant END_CRAM_2: integer :=           12;
    constant BEG_USER_CSR_0: integer :=       13;
    constant BEG_USER_CSR_1: integer :=       14;
    constant BEG_USER_CSR_2: integer :=       15;
    constant END_USER_CSR_0: integer :=       16;
    constant END_USER_CSR_1: integer :=       17;
    constant END_USER_CSR_2: integer :=       18;
    constant FUNC0_ADEM_0 : integer :=        19;
    constant FUNC0_ADEM_1 : integer :=        20;
    constant FUNC0_ADEM_2 : integer :=        21;
    constant FUNC0_ADEM_3 : integer :=        22;
    constant FUNC1_ADEM_0 : integer :=        23;
    constant FUNC1_ADEM_1 : integer :=        24;
    constant FUNC1_ADEM_2 : integer :=        25;
    constant FUNC1_ADEM_3 : integer :=        26;
    constant FUNC2_ADEM_0 : integer :=        27;
    constant FUNC2_ADEM_1 : integer :=        28;
    constant FUNC2_ADEM_2 : integer :=        29;
    constant FUNC2_ADEM_3 : integer :=        30;
    constant FUNC3_ADEM_0 : integer :=        31;
    constant FUNC3_ADEM_1 : integer :=        32;
    constant FUNC3_ADEM_2 : integer :=        33;
    constant FUNC3_ADEM_3 : integer :=        34;
    constant FUNC4_ADEM_0 : integer :=        35;
    constant FUNC4_ADEM_1 : integer :=        36;
    constant FUNC4_ADEM_2 : integer :=        37;
    constant FUNC4_ADEM_3 : integer :=        38;
    constant FUNC5_ADEM_0 : integer :=        39;
    constant FUNC5_ADEM_1 : integer :=        40;
    constant FUNC5_ADEM_2 : integer :=        41;
    constant FUNC5_ADEM_3 : integer :=        42;
    constant FUNC6_ADEM_0 : integer :=        43;
    constant FUNC6_ADEM_1 : integer :=        44;
    constant FUNC6_ADEM_2 : integer :=        45;
    constant FUNC6_ADEM_3 : integer :=        46;
    constant FUNC7_ADEM_0 : integer :=        47;
    constant FUNC7_ADEM_1 : integer :=        48;
    constant FUNC7_ADEM_2 : integer :=        49;
    constant FUNC7_ADEM_3 : integer :=        50;    

    
                                                                            

constant c_CRinitAddr: t_reg52x12bit := (
BEG_USER_CR_2 =>   x"083",
BEG_USER_CR_1 =>   x"087",    
BEG_USER_CR_0 =>   x"08B",    
END_USER_CR_2 =>   x"08F",    
END_USER_CR_1 =>   x"093",    
END_USER_CR_0 =>   x"097",    
BEG_CRAM_2 =>      x"09B",            
BEG_CRAM_1 =>      x"09F",
BEG_CRAM_0 =>      x"0A3",
END_CRAM_2 =>      x"0A7",
END_CRAM_1 =>      x"0AB",
END_CRAM_0 =>      x"0AF",
BEG_USER_CSR_2 =>  x"0B3",    
BEG_USER_CSR_1 =>  x"0B7",    
BEG_USER_CSR_0 =>  x"0BB",    
END_USER_CSR_2 =>  x"0BF",    
END_USER_CSR_1 =>  x"0C3",    
END_USER_CSR_0 =>  x"0C7",    
FUNC0_ADEM_3 =>    x"623",     
FUNC0_ADEM_2 =>    x"627",     
FUNC0_ADEM_1 =>    x"62B",     
FUNC0_ADEM_0 =>    x"62F",     
FUNC1_ADEM_3 =>    x"633",     
FUNC1_ADEM_2 =>    x"637",     
FUNC1_ADEM_1 =>    x"63B",     
FUNC1_ADEM_0 =>    x"63F",     
FUNC2_ADEM_3 =>    x"643",     
FUNC2_ADEM_2 =>    x"647",     
FUNC2_ADEM_1 =>    x"64B",     
FUNC2_ADEM_0 =>    x"64F",     
FUNC3_ADEM_3 =>    x"653",     
FUNC3_ADEM_2 =>    x"657",     
FUNC3_ADEM_1 =>    x"65B",     
FUNC3_ADEM_0 =>    x"65F",     
FUNC4_ADEM_3 =>    x"663",     
FUNC4_ADEM_2 =>    x"667",     
FUNC4_ADEM_1 =>    x"66B",     
FUNC4_ADEM_0 =>    x"66F",     
FUNC5_ADEM_3 =>    x"673",     
FUNC5_ADEM_2 =>    x"677",     
FUNC5_ADEM_1 =>    x"67B",     
FUNC5_ADEM_0 =>    x"67F",     
FUNC6_ADEM_3 =>    x"683",     
FUNC6_ADEM_2 =>    x"687",     
FUNC6_ADEM_1 =>    x"68B",     
FUNC6_ADEM_0 =>    x"68F",     
FUNC7_ADEM_3 =>    x"693",     
FUNC7_ADEM_2 =>    x"697",     
FUNC7_ADEM_1 =>    x"69B",     
FUNC7_ADEM_0 =>    x"69F",
others => (others => '0'));


constant c_checksum_po : integer :=0;
constant c_length_of_rom_po : integer :=1;
constant c_csr_data_acc_width_po : integer :=2;
constant c_cr_space_specification_id_po : integer :=3;
constant c_ascii_c_po  : integer :=4;
constant c_ascii_r_po : integer :=5;
constant c_manu_id_po  : integer :=6;
constant c_board_id_po : integer :=7;
constant c_rev_id_po : integer :=8;
constant c_cus_ascii_po : integer :=9;
constant c_last_CR_pointer_po : integer := 9;
--
--constant c_cr_struc: t_cr_struc(0 to c_last_CR_pointer_po) := (
--
--c_checksum_po => (16#03#, 1),
--c_length_of_rom_po =>	  (16#07#, 3),
--c_csr_data_acc_width_po => (16#1b#, 1),
--c_cr_space_specification_id_po => (16#1F#, 1),
--c_ascii_c_po => (, 1),
--c_ascii_r_po => (, 1),
--c_manu_id_po => (, 1),
--c_board_id_po => (, 4),
--c_rev_id_po => (, 3),
--c_cus_ascii_po => (, 3),
--others => (0,0));




end VME_pack;                                                                




















