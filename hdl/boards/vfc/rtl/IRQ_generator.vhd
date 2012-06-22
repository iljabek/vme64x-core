----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    11:18:01 06/13/2012 
-- Design Name: 
-- Module Name:    IRQ_generator - Behavioral 
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
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity IRQ_generator is
    Port ( clk_i : in  STD_LOGIC;
           reset : in  STD_LOGIC;
           Freq : in  STD_LOGIC_VECTOR (31 downto 0);
           Int_Count_i : in  STD_LOGIC_VECTOR (31 downto 0);
           Read_Int_Count : in  STD_LOGIC;
			  INT_ack : in  STD_LOGIC;
           IRQ_o : out  STD_LOGIC;
           Int_Count_o : out  STD_LOGIC_VECTOR (31 downto 0));
end IRQ_generator;

architecture Behavioral of IRQ_generator is
signal s_en_int : std_logic;
type t_FSM is (IDLE, CHECK, INCR, IRQ, WAIT_INT_ACK, WAIT_RD);
signal currs, nexts : t_FSM;
signal s_IRQ_o : std_logic;
signal s_count : unsigned(31 downto 0);
signal s_Rd_Int_Count_delayed : std_logic;
signal s_pulse : std_logic;
signal s_count_int : unsigned(31 downto 0);
signal s_count_req : unsigned(31 downto 0);
signal s_incr : std_logic;
signal s_gen_irq : std_logic;
signal s_count0 : std_logic;
signal s_Freq : std_logic_vector(31 downto 0);
begin
	
RDinputSample : entity work.DoubleSigInputSample
    port map(
      sig_i => Read_Int_Count,
      sig_o => s_Rd_Int_Count_delayed,
      clk_i => clk_i
      );			
IRQOutputSample : entity work.FlipFlopD
    port map(
      sig_i => s_IRQ_o,
      sig_o => IRQ_o,
      clk_i => clk_i,
		reset => '0',
		enable => '1'
      );		
process(clk_i)
  begin
    if rising_edge(clk_i) then
	    if reset = '0' then s_Freq <= (others => '0');
	    elsif s_count0 = '1' then
		       s_Freq <= Freq; 
       end if;
			 
	 end if;	 
  end process;				
	
process(clk_i)
  begin
    if rising_edge(clk_i) then
	   if s_count = 0 then
		   s_count0 <= '1';
		else 
         s_count0 <= '0';
      end if;			
    end if;
end process;			
		
process(clk_i)
  begin
    if rising_edge(clk_i) then
	   if reset = '0' then s_en_int <= '0';
	   elsif unsigned(s_Freq) = 0 then
		      s_en_int <= '0';
		else 
            s_en_int <= '1';
      end if;			
    end if;
end process;		
--Counter 
process(clk_i)
	begin
	if rising_edge(clk_i) then
      if reset = '0' or s_pulse = '1' then s_count <= (others => '0');
      elsif s_en_int = '1' then
      s_count <= s_count + 1;
		end if;	
	end if;
end process;
--
process(clk_i)
	begin
	if rising_edge(clk_i) then
      if s_en_int = '1' and unsigned(s_Freq) = s_count then 
         s_pulse <= '1';
		else
         s_pulse <= '0';			
		end if;	
	end if;
end process;
--Counter interrupts
process(clk_i)
	begin
	if rising_edge(clk_i) then
      if reset = '0' then s_count_int <= (others => '0');
      elsif s_en_int = '1' and s_pulse = '1' then
      s_count_int <= s_count_int + 1;
		end if;	
	end if;
end process;
--Counter interrupts requests
process(clk_i)
	begin
	if rising_edge(clk_i) then
      if reset = '0' then s_count_req <= (others => '0');
      elsif s_incr = '1' then
            s_count_req <= s_count_req + 1;
		end if;	
	end if;
end process;
--
process(clk_i)
	begin
	if rising_edge(clk_i) then
      if unsigned(Int_Count_i) > s_count_req then
         s_gen_irq <= '1';
      else
         s_gen_irq <= '0';
      end if;			
	end if;
end process;
-- Update current state
process(clk_i)
begin
  if rising_edge(clk_i) then
      if reset = '0' then currs <= IDLE;
      else currs <= nexts;
      end if;	
  end if;
end process;		

process(currs,s_gen_irq,INT_ack,s_Rd_Int_Count_delayed)
begin
   case currs is 
	    when IDLE =>
		     nexts <= CHECK;
			  
		 when CHECK =>
		     if s_gen_irq = '1' then
		        nexts <= INCR;	  
			  else
			     nexts <= CHECK;
			  end if;
		
        when INCR =>
		     nexts <= IRQ;	

		  when IRQ =>
		     nexts <= WAIT_INT_ACK;
			    
		  when WAIT_INT_ACK =>
		     if INT_ack = '0' then
		        nexts <= WAIT_RD;	  
			  else
			     nexts <= WAIT_INT_ACK;
			  end if;
			
		  when WAIT_RD =>
		     if s_Rd_Int_Count_delayed = '1' then
		        nexts <= IDLE;	  
			  else
			     nexts <= WAIT_RD;
			  end if;
	
   end case;

end process;

process(currs)
begin
   case currs is 
	    when IDLE =>
		   s_incr   <= '0';
			s_IRQ_o  <= '0';
		 
		 when CHECK =>
		   s_incr   <= '0';
			s_IRQ_o  <= '0';
		 
		 when INCR =>
		   s_incr   <= '1';
			s_IRQ_o  <= '0';
			
		 when IRQ =>
		   s_incr   <= '0';
			s_IRQ_o  <= '1';
		 
		 when WAIT_INT_ACK =>
		   s_incr   <= '0';
			s_IRQ_o  <= '0';
		 
		 when WAIT_RD =>
		   s_incr   <= '0';
			s_IRQ_o  <= '0';
		 
	end case;	 
end process;
Int_Count_o <= std_logic_vector(s_count_int);
end Behavioral;

