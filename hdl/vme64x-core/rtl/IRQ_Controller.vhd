--____________________________________________________________________________________________________
--                             VME TO WB INTERFACE
--
--                                CERN,BE/CO-HT 
--____________________________________________________________________________________________________
-- File:                           IRQ_Controller.vhd
--____________________________________________________________________________________________________
-- Description:

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.all;


-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity IRQ_Controller is
    Port ( clk_i : in std_logic;
	        reset : in std_logic;  
	        VME_IACKIN_n_i : in  STD_LOGIC;
           VME_AS_n_i : in  STD_LOGIC;
           VME_DS_n_i : in  STD_LOGIC_VECTOR (1 downto 0);
           VME_LWORD_n_i : in  STD_LOGIC;
           VME_ADDR_123 : in  STD_LOGIC_VECTOR (2 downto 0);
           INT_Level : in  STD_LOGIC_VECTOR (7 downto 0);
           INT_Vector : in  STD_LOGIC_VECTOR (7 downto 0);
           INT_Req : in  STD_LOGIC;
           --Read_Int_Source : in  STD_LOGIC;
           VME_IRQ_n_o : out  STD_LOGIC_VECTOR (6 downto 0);
           VME_IACKOUT_n_o : out  STD_LOGIC;
           VME_DTACK_n_o : out  STD_LOGIC;
			  VME_DTACK_OE_o : out  STD_LOGIC;
			  VME_DATA_o : out  STD_LOGIC_VECTOR (31 downto 0);
           DataDir : out  STD_LOGIC);
end IRQ_Controller;

architecture Behavioral of IRQ_Controller is
--input signals
signal VME_AS_n_oversampled : std_logic;
signal VME_IACKIN_n_oversampled : std_logic;
signal VME_DS_n_oversampled : std_logic_vector(1 downto 0);
signal VME_LWORD_n_oversampled : std_logic;
signal VME_ADDR_123_oversampled : std_logic_vector(2 downto 0);
signal VME_RST_n_oversampled : std_logic;
signal INT_Req_sample : std_logic;
--signal Read_Int_Source_sample : std_logic;
--output signals
signal s_DTACK : std_logic;
signal s_DTACK_OE : std_logic;
signal s_DataDir : std_logic;
signal s_IACKOUT : std_logic;
signal s_IRQ : std_logic_vector(6 downto 0);
signal s_Data : std_logic_vector(31 downto 0);
--Edge detection signal
signal AS_FallingEdge : std_logic;
signal IACKIN_FallingEdge : std_logic;
type t_MainFSM is (IDLE, IRQ, WAIT_AS, WAIT_DS, LATCH_DS, ACK_INT, DATA_OUT, DTACK,IACKOUT);
signal currs, nexts : t_MainFSM;
signal s_ack_int : std_logic;
signal s_resetIRQ : std_logic;
signal s_enableIRQ : std_logic;
signal VME_ADDR_123_latched : std_logic_vector(2 downto 0);
signal VME_LWORD_latched : std_logic;
signal VME_DS_latched : std_logic_vector(1 downto 0);
signal DSlatch : std_logic;
signal ADDRmatch : std_logic;

begin
-- Input oversampling & edge detection
ASfallingEdge : entity work.FallingEdgeDetection
    port map (
      sig_i      => VME_AS_n_oversampled,
      clk_i      => clk_i,
      FallEdge_o => AS_FallingEdge
      );
IACKINfallingEdge : entity work.FallingEdgeDetection
    port map (
      sig_i      => VME_IACKIN_n_oversampled,
      clk_i      => clk_i,
      FallEdge_o => IACKIN_FallingEdge
      );		
		
DSinputSample : entity work.DoubleRegInputSample
    generic map(
      width => 2
      )
    port map(
      reg_i => VME_DS_n_i,
      reg_o => VME_DS_n_oversampled,
      clk_i => clk_i
      );		
ADDRinputSample : entity work.DoubleRegInputSample
    generic map(
      width => 3
      )
    port map(
      reg_i => VME_ADDR_123,
      reg_o => VME_ADDR_123_oversampled,
      clk_i => clk_i
      );		
LWORDinputSample : entity work.DoubleSigInputSample
    port map(
      sig_i => VME_LWORD_n_i,
      sig_o => VME_LWORD_n_oversampled,
      clk_i => clk_i
      );				
ASinputSample : entity work.DoubleSigInputSample
    port map(
      sig_i => VME_AS_n_i,
      sig_o => VME_AS_n_oversampled,
      clk_i => clk_i
      );			
IACKINinputSample : entity work.DoubleSigInputSample
    port map(
      sig_i => VME_IACKIN_n_i,
      sig_o => VME_IACKIN_n_oversampled,
      clk_i => clk_i
      );			
RSTinputSample : entity work.DoubleSigInputSample
    port map(
      sig_i => reset,
      sig_o => VME_RST_n_oversampled,
      clk_i => clk_i
      );			
INT_ReqinputSample : entity work.FlipFlopD
    port map(
      sig_i => INT_Req,
      sig_o => INT_Req_sample,
      clk_i => clk_i,
		reset => '0',
		enable => '1'
      );		
	

--Output registers:
DTACKOutputSample : entity work.FlipFlopD
    port map(
      sig_i => s_DTACK,
      sig_o => VME_DTACK_n_o,
      clk_i => clk_i,
		reset => '0',
		enable => '1'
      );		
DataDirOutputSample : entity work.FlipFlopD
    port map(
      sig_i => s_DataDir,
      sig_o => DataDir,
      clk_i => clk_i,
		reset => '0',
		enable => '1'
      );		
IACKOUTOutputSample : entity work.FlipFlopD
    port map(
      sig_i => s_IACKOUT,
      sig_o => VME_IACKOUT_n_o,
      clk_i => clk_i,
		reset => '0',
		enable => '1'
      );		
		
process(clk_i)
  begin
    if rising_edge(clk_i) then
	    if s_resetIRQ = '1' then
		    VME_IRQ_n_o <= (others => '1');
		 elsif s_enableIRQ = '1' then	 
          VME_IRQ_n_o <= s_IRQ; 
       end if;
	 end if;	 
  end process;		
  
process(clk_i)
  begin
    if rising_edge(clk_i) then
	   
      VME_DATA_o <= s_Data; 
    end if;
  end process;		
  
-- Update current state
process(clk_i)
begin
  if rising_edge(clk_i) then
      if VME_RST_n_oversampled = '0' then currs <= IDLE;
      else currs <= nexts;
      end if;	
  end if;
end process;		
-- 
process(currs,INT_Req_sample,IACKIN_FallingEdge,VME_AS_n_oversampled,VME_DS_n_oversampled,s_ack_int)
begin
   case currs is 
	    when IDLE =>
		     if INT_Req_sample = '1' then
		     nexts <= IRQ;
			  else 
			  nexts <= IDLE;
			  end if;
	    
		 when IRQ => 
		     if IACKIN_FallingEdge = '1' then  -- Each Interrupter thet is driving an interrupt request line
			                                    -- low waits for a falling edge to arrive at is IACKIN input -->
															-- the IRQ_Controller have to detect a falling edge on the IACKIN.
		     nexts <= WAIT_AS;
			  else 
			  nexts <= IRQ;
			  end if;

       when WAIT_AS =>
		     if VME_AS_n_oversampled = '0' then  -- NOT USE FALLING EDGE HERE!!!!!!!!!!
		     nexts <= WAIT_DS;
			  else 
			  nexts <= WAIT_AS;
			  end if;

       when WAIT_DS =>
			  if VME_DS_n_oversampled /= "11" then
		     nexts <= LATCH_DS;
			  else 
			  nexts <= WAIT_DS;
			  end if;
		 when LATCH_DS =>	  
			  nexts <= ACK_INT;
			  
		 when ACK_INT =>	  
		     if s_ack_int = '1' then
		     nexts <= DATA_OUT;  -- The Interrupter send the INT_Vector
			  else 
			  nexts <= IACKOUT;   -- and the Interrupter must pass a falling edge on the IACKOUT output
			  end if;
			  
		 when IACKOUT =>	  
			  nexts <= IRQ;
			  
		 when  DATA_OUT=>	  
		     nexts <= DTACK;	 
     
       when  DTACK=>	
           if VME_AS_n_oversampled = '1' then  -- NOT USE RISING EDGE HERE!!!!!!!!!!
		     nexts <= IDLE;
			  else 
			  nexts <= DTACK;
			  end if;		 	  
		 
	end case;

end process;
-- Update Outputs
process(currs)
begin
   case currs is 
	    when IDLE =>
		     s_IACKOUT <= '1';
	        s_DataDir <= '0'; 
			  s_DTACK <= '1';
			  s_enableIRQ <= '0';
			  s_resetIRQ <= '1';
			  DSlatch <= '0';
			  s_DTACK_OE <= '0';
		 when IRQ => 
		     s_IACKOUT <= '1';
	        s_DataDir <= '0'; 
			  s_DTACK <= '1';
			  s_enableIRQ <= '1';
			  s_resetIRQ <= '0';
			  DSlatch <= '0';
           s_DTACK_OE <= '0';
			  
       when WAIT_AS =>
		     s_IACKOUT <= '1';
	        s_DataDir <= '0'; 
			  s_DTACK <= '1';
			  s_enableIRQ <= '0';
			  s_resetIRQ <= '0';
			  DSlatch <= '0';
			  s_DTACK_OE <= '0';
			  
       when WAIT_DS =>
			  s_IACKOUT <= '1';
	        s_DataDir <= '0'; 
			  s_DTACK <= '1';
			  s_enableIRQ <= '0';
			  s_resetIRQ <= '0';
			  DSlatch <= '0';
			  s_DTACK_OE <= '0';
			  
		 when LATCH_DS =>	  
			  s_IACKOUT <= '1';
	        s_DataDir <= '0'; 
			  s_DTACK <= '1';
			  s_enableIRQ <= '0';
			  s_resetIRQ <= '0';
			  DSlatch <= '1';
			  s_DTACK_OE <= '0';
			  
		 when ACK_INT =>	  
		     s_IACKOUT <= '1';
	        s_DataDir <= '0'; 
			  s_DTACK <= '1';
			  s_enableIRQ <= '0';
			  s_resetIRQ <= '0';
			  DSlatch <= '0';
			  s_DTACK_OE <= '0';
			  
		 when  IACKOUT =>
           s_IACKOUT <= '0';
	        s_DataDir <= '0'; 
			  s_DTACK <= '1';
			  s_enableIRQ <= '0';
			  s_resetIRQ <= '0';
			  DSlatch <= '0';
		     s_DTACK_OE <= '0';
			  
		 when  DATA_OUT=>	  
		     	s_IACKOUT <= '1';
	         s_DataDir <= '1'; 
			   s_DTACK <= '1';
			   s_enableIRQ <= '0';
			   s_resetIRQ <= '0';
			   DSlatch <= '0'; 
            s_DTACK_OE <= '1';
	  
       when  DTACK=>	
           	s_IACKOUT <= '1';
	         s_DataDir <= '1'; 
			   s_DTACK <= '0';
			   s_enableIRQ <= '0';
			   s_resetIRQ <= '1';
			   DSlatch <= '0'; 	
            s_DTACK_OE <= '1';				
							 
	end case;

end process;

-- This process creates the IRQ vector
process(INT_Level)
begin
  case (INT_Level) is
       when "00000001" => s_IRQ <= "1111110";
       when "00000010" => s_IRQ <= "1111101";
       when "00000011" => s_IRQ <= "1111011";
       when "00000100" => s_IRQ <= "1110111";
       when "00000101" => s_IRQ <= "1101111";
       when "00000110" => s_IRQ <= "1011111";
       when "00000111" => s_IRQ <= "0111111";
       when others     => s_IRQ <= "1111111";
  end case;
end process;

process(clk_i)
begin
  if rising_edge(clk_i) then
      if VME_RST_n_oversampled = '0' then 
		      VME_ADDR_123_latched <= (others => '0');
				VME_LWORD_latched <= '0';
      elsif AS_FallingEdge = '1' then  
		      VME_ADDR_123_latched <= VME_ADDR_123_oversampled;
				VME_LWORD_latched <= VME_LWORD_n_oversampled;
      end if;	
  end if;
end process;	

process(clk_i)
begin
  if rising_edge(clk_i) then
      if VME_RST_n_oversampled = '0' then 
		      VME_DS_latched <= (others => '0');
      elsif DSlatch = '1' then  
		      VME_DS_latched <= VME_DS_n_oversampled;
      end if;	
  end if;
end process;	
--This process check the A01 A02 A03:
process(clk_i)
begin
  if rising_edge(clk_i) then
      if VME_RST_n_oversampled = '0' then 
		      ADDRmatch <= '0';
      elsif unsigned(INT_Level) = unsigned(VME_ADDR_123_latched) then  
		      ADDRmatch <= '1';
      end if;	
  end if;
end process;	
s_ack_int <= (not(VME_DS_latched(0)))  and ADDRmatch;  --and  (not(VME_LWORD_latched)) and (not(VME_DS_latched(1)))
s_Data <= x"000000" & INT_Vector;
VME_DTACK_OE_o <= s_DTACK_OE;
end Behavioral;

