library IEEE;
use IEEE.STD_LOGIC_1164.all;

package VME_pack is
        
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
    
    constant BAR_addr : std_logic_vector(18 downto 0) :=                  "1111111111111111111";        
    constant BIT_SET_REG_addr : std_logic_vector(18 downto 0) :=          "1111111111111111011";
    constant BIT_CLR_REG_addr : std_logic_vector(18 downto 0) :=          "1111111111111110111";
    constant CRAM_OWNER_addr : std_logic_vector(18 downto 0) :=           "1111111111111110011";   
    constant USR_BIT_SET_REG_addr : std_logic_vector(18 downto 0) :=      "1111111111111101111";
    constant USR_BIT_CLR_REG_addr : std_logic_vector(18 downto 0) :=      "1111111111111101011";
    constant FUNC7_ADER_0_addr : std_logic_vector(18 downto 0) :=         "1111111111111011111";   
    constant FUNC7_ADER_1_addr : std_logic_vector(18 downto 0) :=         "1111111111111011011";   
    constant FUNC7_ADER_2_addr : std_logic_vector(18 downto 0) :=         "1111111111111010111";   
    constant FUNC7_ADER_3_addr : std_logic_vector(18 downto 0) :=         "1111111111111010011";   
    constant FUNC6_ADER_0_addr : std_logic_vector(18 downto 0) :=         "1111111111111001111";   
    constant FUNC6_ADER_1_addr : std_logic_vector(18 downto 0) :=         "1111111111111001011";   
    constant FUNC6_ADER_2_addr : std_logic_vector(18 downto 0) :=         "1111111111111000111";   
    constant FUNC6_ADER_3_addr : std_logic_vector(18 downto 0) :=         "1111111111111000011";   
    constant FUNC5_ADER_0_addr : std_logic_vector(18 downto 0) :=         "1111111111110111111";   
    constant FUNC5_ADER_1_addr : std_logic_vector(18 downto 0) :=         "1111111111110111011";   
    constant FUNC5_ADER_2_addr : std_logic_vector(18 downto 0) :=         "1111111111110110111";   
    constant FUNC5_ADER_3_addr : std_logic_vector(18 downto 0) :=         "1111111111110110011";   
    constant FUNC4_ADER_0_addr : std_logic_vector(18 downto 0) :=         "1111111111110101111";   
    constant FUNC4_ADER_1_addr : std_logic_vector(18 downto 0) :=         "1111111111110101011";   
    constant FUNC4_ADER_2_addr : std_logic_vector(18 downto 0) :=         "1111111111110100111";   
    constant FUNC4_ADER_3_addr : std_logic_vector(18 downto 0) :=         "1111111111110100011";   
    constant FUNC3_ADER_0_addr : std_logic_vector(18 downto 0) :=         "1111111111110011111";   
    constant FUNC3_ADER_1_addr : std_logic_vector(18 downto 0) :=         "1111111111110011011";   
    constant FUNC3_ADER_2_addr : std_logic_vector(18 downto 0) :=         "1111111111110010111";   
    constant FUNC3_ADER_3_addr : std_logic_vector(18 downto 0) :=         "1111111111110010011";   
    constant FUNC2_ADER_0_addr : std_logic_vector(18 downto 0) :=         "1111111111110001111";   
    constant FUNC2_ADER_1_addr : std_logic_vector(18 downto 0) :=         "1111111111110001011";   
    constant FUNC2_ADER_2_addr : std_logic_vector(18 downto 0) :=         "1111111111110000111";   
    constant FUNC2_ADER_3_addr : std_logic_vector(18 downto 0) :=         "1111111111110000011";   
    constant FUNC1_ADER_0_addr : std_logic_vector(18 downto 0) :=         "1111111111101111111";   
    constant FUNC1_ADER_1_addr : std_logic_vector(18 downto 0) :=         "1111111111101111011";   
    constant FUNC1_ADER_2_addr : std_logic_vector(18 downto 0) :=         "1111111111101110111";   
    constant FUNC1_ADER_3_addr : std_logic_vector(18 downto 0) :=         "1111111111101110011";   
    constant FUNC0_ADER_0_addr : std_logic_vector(18 downto 0) :=         "1111111111101101111";   
    constant FUNC0_ADER_1_addr : std_logic_vector(18 downto 0) :=         "1111111111101101011";   
    constant FUNC0_ADER_2_addr : std_logic_vector(18 downto 0) :=         "1111111111101100111";   
    constant FUNC0_ADER_3_addr : std_logic_vector(18 downto 0) :=         "1111111111101100011";
    constant IRQ_ID_addr : std_logic_vector(18 downto 0) :=               "1111111101111111111";
    constant IRQ_level_addr : std_logic_vector(18 downto 0) :=            "1111111101111111011";
    
    
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
    
    type t_reg38x8bit is array(37 downto 0) of std_logic_vector(7 downto 0);
    type t_reg52x8bit is array(51 downto 0) of std_logic_vector(7 downto 0);
    type t_reg52x12bit is array(51 downto 0) of std_logic_vector(11 downto 0);
                                                                            
end VME_pack;                                                                




















