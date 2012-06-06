----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    09:53:50 05/23/2012 
-- Design Name: 
-- Module Name:    spram - Behavioral 
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
--use work.genram_pkg.all;
-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity spram is
generic (
    -- standard parameters
    g_data_width : natural := 64;
    g_size       : natural := 256;

    -- if true, the user can write individual bytes by using bwe_i
    g_with_byte_enable : boolean := true;

    -- RAM read-on-write conflict resolution. Can be "read_first" (read-then-write)
    -- or "write_first" (write-then-read)
    g_addr_conflict_resolution : string := "read_first";
    g_init_file                : string := ""
    );
port (
    --rst_n_i : in std_logic;             -- synchronous reset, active LO
    clk_i   : in std_logic;             -- clock input

    -- byte write enable
    bwe_i : in std_logic_vector(((g_data_width)/8)-1 downto 0);

    -- global write enable (masked by bwe_i if g_with_byte_enable = true)
    --we_i : in std_logic;

    -- address input
    a_i : in std_logic_vector(f_log2_size(g_size)-1 downto 0);

    -- data input
    d_i : in std_logic_vector(g_data_width-1 downto 0);

    -- data output
    q_o : out std_logic_vector(g_data_width-1 downto 0)
    );
end spram;

architecture Behavioral of spram is

constant c_num_bytes : integer := (g_data_width)/8;
--type t_ram_type is array(g_size-1 downto 0) of std_logic_vector(g_data_width-1 downto 0);
--signal sram  : t_ram_type;

begin
  spram: for i in 0 to c_num_bytes-1 generate
        ram8bits : entity work.ram_8bits
             generic map(g_size)
             port map(addr => a_i, 
                      di => d_i(8*i+7 downto 8*i),
							 do => q_o(8*i+7 downto 8*i),
                      we => bwe_i(i),
                      clk_i => clk_i
                      );							 
  end generate;


	--process(clk_i)
   -- begin
	 --for i in c_num_bytes-1 downto 0 loop  
     -- if rising_edge(clk_i) then
		      
          --       if (bwe_i(i) = '1') then
           --        sram(conv_integer(unsigned(a_i)))(8*i+7 downto 8*i) <= d_i(8*i+7 downto 8*i);
           --      else
				----	    q_o(8*i+7 downto 8*i) <= sram(conv_integer(unsigned(a_i)))(8*i+7 downto 8*i);
				--	  end if;    
                 	 
		--end if;
	-- end loop;
   -- end process;
	 --q_o(8*i+7 downto 8*i) <= sram(conv_integer(unsigned(a_i)))(8*i+7 downto 8*i);
   --end generate;
	 --q_o <= sram(conv_integer(unsigned(a_i)));
end Behavioral;

