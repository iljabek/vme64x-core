library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.numeric_std.all;

package VME_pack is

  
  type t_reg424x8bit is array(423 downto 0) of unsigned(7 downto 0);  -- it was t_reg52x8bit, modified by Davide
  type t_reg52x12bit is array(51 downto 0) of unsigned(11 downto 0);
  type t_cr_array is array (natural range <>) of std_logic_vector(7 downto 0);

  type t_rom_cell is
  record
    add : integer;
    len : integer;
  end record;

  type t_cr_add_table is array (natural range <>) of t_rom_cell;

  constant c_A24_S_sup    : std_logic_vector(5 downto 0) := "111101";
  constant c_A24_S        : std_logic_vector(5 downto 0) := "111001";
  constant c_A24_BLT      : std_logic_vector(5 downto 0) := "111011";
  constant c_A24_BLT_sup  : std_logic_vector(5 downto 0) := "111111";
  constant c_A24_MBLT     : std_logic_vector(5 downto 0) := "111000";
  constant c_A24_MBLT_sup : std_logic_vector(5 downto 0) := "111100";
  constant c_A24_LCK      : std_logic_vector(5 downto 0) := "110010";
  constant c_CR_CSR       : std_logic_vector(5 downto 0) := "101111";
  constant c_A16          : std_logic_vector(5 downto 0) := "101001";
  constant c_A16_sup      : std_logic_vector(5 downto 0) := "101101";
  constant c_A16_LCK      : std_logic_vector(5 downto 0) := "101100";
  constant c_A32          : std_logic_vector(5 downto 0) := "001001";
  constant c_A32_sup      : std_logic_vector(5 downto 0) := "001101";
  constant c_A32_BLT      : std_logic_vector(5 downto 0) := "001011";
  constant c_A32_BLT_sup  : std_logic_vector(5 downto 0) := "001111";
  constant c_A32_MBLT     : std_logic_vector(5 downto 0) := "001000";
  constant c_A32_MBLT_sup : std_logic_vector(5 downto 0) := "001100";
  constant c_A32_LCK      : std_logic_vector(5 downto 0) := "000101";
  constant c_A64          : std_logic_vector(5 downto 0) := "000001";
  constant c_A64_BLT      : std_logic_vector(5 downto 0) := "000011";
  constant c_A64_MBLT     : std_logic_vector(5 downto 0) := "000000";
  constant c_A64_LCK      : std_logic_vector(5 downto 0) := "000100";
  constant c_TWOedge      : std_logic_vector(5 downto 0) := "100000";
  constant c_A32_2eVME      : std_logic_vector(7 downto 0) := "00000001";
  constant c_A64_2eVME      : std_logic_vector(7 downto 0) := "00000010";
  constant c_A32_2eSST      : std_logic_vector(7 downto 0) := "00010001";
  constant c_A64_2eSST      : std_logic_vector(7 downto 0) := "00010010";

  constant c_cr_step           : integer := 4;
  constant BAR                 : integer := 255;
  constant BIT_SET_CLR_REG     : integer := 254;
  constant USR_BIT_SET_CLR_REG : integer := 253;
  constant CRAM_OWNER          : integer := 252;

  constant FUNC7_ADER_0 : integer := 251;
  constant FUNC7_ADER_1 : integer := FUNC7_ADER_0 - 1;
  constant FUNC7_ADER_2 : integer := FUNC7_ADER_0 - 2;
  constant FUNC7_ADER_3 : integer := FUNC7_ADER_0 - 3;
  constant FUNC6_ADER_0 : integer := FUNC7_ADER_0 - 4;
  constant FUNC6_ADER_1 : integer := FUNC7_ADER_0 - 5;
  constant FUNC6_ADER_2 : integer := FUNC7_ADER_0 - 6;
  constant FUNC6_ADER_3 : integer := FUNC7_ADER_0 - 7;
  constant FUNC5_ADER_0 : integer := FUNC7_ADER_0 - 8;
  constant FUNC5_ADER_1 : integer := FUNC7_ADER_0 - 9;
  constant FUNC5_ADER_2 : integer := FUNC7_ADER_0 - 10;
  constant FUNC5_ADER_3 : integer := FUNC7_ADER_0 - 11;
  constant FUNC4_ADER_0 : integer := FUNC7_ADER_0 - 12;
  constant FUNC4_ADER_1 : integer := FUNC7_ADER_0 - 13;
  constant FUNC4_ADER_2 : integer := FUNC7_ADER_0 - 14;
  constant FUNC4_ADER_3 : integer := FUNC7_ADER_0 - 15;
  constant FUNC3_ADER_0 : integer := FUNC7_ADER_0 - 16;
  constant FUNC3_ADER_1 : integer := FUNC7_ADER_0 - 17;
  constant FUNC3_ADER_2 : integer := FUNC7_ADER_0 - 18;
  constant FUNC3_ADER_3 : integer := FUNC7_ADER_0 - 19;
  constant FUNC2_ADER_0 : integer := FUNC7_ADER_0 - 20;
  constant FUNC2_ADER_1 : integer := FUNC7_ADER_0 - 21;
  constant FUNC2_ADER_2 : integer := FUNC7_ADER_0 - 22;
  constant FUNC2_ADER_3 : integer := FUNC7_ADER_0 - 23;
  constant FUNC1_ADER_0 : integer := FUNC7_ADER_0 - 24;
  constant FUNC1_ADER_1 : integer := FUNC7_ADER_0 - 25;
  constant FUNC1_ADER_2 : integer := FUNC7_ADER_0 - 26;
  constant FUNC1_ADER_3 : integer := FUNC7_ADER_0 - 27;
  constant FUNC0_ADER_0 : integer := FUNC7_ADER_0 - 28;
  constant FUNC0_ADER_1 : integer := FUNC7_ADER_0 - 29;
  constant FUNC0_ADER_2 : integer := FUNC7_ADER_0 - 30;
  constant FUNC0_ADER_3 : integer := FUNC7_ADER_0 - 31;

  constant IRQ_Vector    : integer := FUNC0_ADER_3 -1;
  constant IRQ_level : integer := FUNC0_ADER_3 -2;
  constant MBLT_Endian : integer := FUNC0_ADER_3 -3; --Added by Davide

  type t_CSRarray is array(BAR downto MBLT_Endian) of unsigned(7 downto 0);

  ----------------------------------
  --Bit accronyms
  constant DFS : integer := 2;


  constant XAM_MODE : integer := 0; -- used to access at the bit 0 of the ADER register


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
  constant BAR_addr             : integer := 16#7FFFF#;
  constant BIT_SET_REG_addr     : integer := 16#7FFFB#;
  constant BIT_CLR_REG_addr     : integer := 16#7FFF7#;
  constant CRAM_OWNER_addr      : integer := 16#7FFF3#;
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

  constant IRQ_ID_addr    : integer := 16#7fbff#;
  constant IRQ_level_addr : integer := 16#7fbef#;

----------------------------------
---------------------------------------------------------------------------
  constant BEG_USER_CR  : integer := 1;  
  constant END_USER_CR  : integer := 2;
  constant BEG_CRAM     : integer := 3;
  constant END_CRAM     : integer := 4;
  constant BEG_USER_CSR : integer := 5;
  constant END_USER_CSR : integer := 6;
  constant FUNC_AMCAP   : integer := 7;
  constant FUNC_XAMCAP  : integer := 8;
  constant FUNC_ADEM    : integer := 9;  -- 340


  
  

  constant c_CRinitAddr : t_cr_add_table(BEG_USER_CR to FUNC_ADEM) := (
    BEG_USER_CR => (add => 16#020#, len => 3),
    END_USER_CR => (add => 16#023#, len => 3),

    BEG_CRAM => (add => 16#26#, len => 3),
    END_CRAM => (add => 16#29#, len => 3),

    BEG_USER_CSR => (add => 16#02C#, len => 3),
    END_USER_CSR => (add => 16#02F#, len => 3),
	 
    FUNC_AMCAP  => (add => 16#048#, len => 64),
    FUNC_XAMCAP => (add => 16#088#, len => 256),
    FUNC_ADEM   => (add => 16#188#, len => 32));


  constant c_checksum_po                  : integer := 0;
  constant c_length_of_rom_po             : integer := 1;
  constant c_csr_data_acc_width_po        : integer := 2;
  constant c_cr_space_specification_id_po : integer := 3;
  constant c_ascii_c_po                   : integer := 4;
  constant c_ascii_r_po                   : integer := 5;
  constant c_manu_id_po                   : integer := 6;
  constant c_board_id_po                  : integer := 7;
  constant c_rev_id_po                    : integer := 8;
  constant c_cus_ascii_po                 : integer := 9;
  constant c_last_CR_pointer_po           : integer := 9;

  constant c_BAR_addr             : unsigned(19 downto 0) := x"7FFFF";
  constant c_BIT_SET_REG_addr     : unsigned(19 downto 0) := x"7FFFB";
  constant c_BIT_CLR_REG_addr     : unsigned(19 downto 0) := x"7FFF7";
  constant c_CRAM_OWNER_addr      : unsigned(19 downto 0) := x"7FFF3";
  constant c_USR_BIT_SET_REG_addr : unsigned(19 downto 0) := x"7FFEF";
  constant c_USR_BIT_CLR_REG_addr : unsigned(19 downto 0) := x"7FFEB";


  constant c_FUNC7_ADER_0_addr : unsigned(19 downto 0) := x"7FFDF";
  constant c_FUNC7_ADER_1_addr : unsigned(19 downto 0) := x"7FFDB";
  constant c_FUNC7_ADER_2_addr : unsigned(19 downto 0) := x"7FFD7";
  constant c_FUNC7_ADER_3_addr : unsigned(19 downto 0) := x"7FFD3";

  constant c_FUNC6_ADER_0_addr : unsigned(19 downto 0) := x"7FFCF";
  constant c_FUNC6_ADER_1_addr : unsigned(19 downto 0) := x"7FFCB";
  constant c_FUNC6_ADER_2_addr : unsigned(19 downto 0) := x"7FFC7";
  constant c_FUNC6_ADER_3_addr : unsigned(19 downto 0) := x"7FFC3";

  constant c_FUNC5_ADER_0_addr : unsigned(19 downto 0) := x"7FFBF";
  constant c_FUNC5_ADER_1_addr : unsigned(19 downto 0) := x"7FFBB";
  constant c_FUNC5_ADER_2_addr : unsigned(19 downto 0) := x"7FFB7";
  constant c_FUNC5_ADER_3_addr : unsigned(19 downto 0) := x"7FFB3";

  constant c_FUNC4_ADER_0_addr : unsigned(19 downto 0) := x"7FFAF";
  constant c_FUNC4_ADER_1_addr : unsigned(19 downto 0) := x"7FFAB";
  constant c_FUNC4_ADER_2_addr : unsigned(19 downto 0) := x"7FFA7";
  constant c_FUNC4_ADER_3_addr : unsigned(19 downto 0) := x"7FFA3";

  constant c_FUNC3_ADER_0_addr : unsigned(19 downto 0) := x"7FF9F";
  constant c_FUNC3_ADER_1_addr : unsigned(19 downto 0) := x"7FF9B";
  constant c_FUNC3_ADER_2_addr : unsigned(19 downto 0) := x"7FF97";
  constant c_FUNC3_ADER_3_addr : unsigned(19 downto 0) := x"7FF93";

  constant c_FUNC2_ADER_0_addr : unsigned(19 downto 0) := x"7FF8F";
  constant c_FUNC2_ADER_1_addr : unsigned(19 downto 0) := x"7FF8B";
  constant c_FUNC2_ADER_2_addr : unsigned(19 downto 0) := x"7FF87";
  constant c_FUNC2_ADER_3_addr : unsigned(19 downto 0) := x"7FF83";

  constant c_FUNC1_ADER_0_addr : unsigned(19 downto 0) := x"7FF7F";
  constant c_FUNC1_ADER_1_addr : unsigned(19 downto 0) := x"7FF7B";
  constant c_FUNC1_ADER_2_addr : unsigned(19 downto 0) := x"7FF77";
  constant c_FUNC1_ADER_3_addr : unsigned(19 downto 0) := x"7FF73";

  constant c_FUNC0_ADER_0_addr : unsigned(19 downto 0) := x"7FF6F";
  constant c_FUNC0_ADER_1_addr : unsigned(19 downto 0) := x"7FF6B";
  constant c_FUNC0_ADER_2_addr : unsigned(19 downto 0) := x"7FF67";
  constant c_FUNC0_ADER_3_addr : unsigned(19 downto 0) := x"7FF63";
  constant c_IRQ_Vector_addr   : unsigned(19 downto 0) := x"7FF5F";
  constant c_IRQ_level_addr    : unsigned(19 downto 0) := x"7FF5B";
  constant c_MBLT_Endian_addr  : unsigned(19 downto 0) := x"7FF53";




end VME_pack;




















