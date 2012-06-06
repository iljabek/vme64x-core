--____________________________________________________________________________________________________
--                             VME TO WB INTERFACE
--
--                                CERN,BE/CO-HT 
--____________________________________________________________________________________________________
-- File:                           FIFO.vhd
--____________________________________________________________________________________________________
-- Description:





library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.all;
--use work.common_components.all;
--use work.TrueDpBlockRam.all;

entity FIFO is
generic(c_dl     : integer := 64;
        c_al     : integer := 64;
        c_sell   : integer := 8;  -- num.of windows in which is divided the port
        c_psizel : integer := 8); -- 256 words

  port (
    -- Common signals 
    clk_i           : in  std_logic;
    reset_i         : in  std_logic;
    transfer_done_o : out std_logic;
	 transfer_done_i : in std_logic;
    VMEtoWB         : in std_logic;
	 WBtoVME         : in std_logic;
    -- Slave WB with dma support        
    sl_dat_i   : in  std_logic_vector(c_dl -1 downto 0);
    sl_dat_o   : out std_logic_vector(c_dl -1 downto 0);
    sl_adr_i   : in  std_logic_vector(c_al -1 downto 0);
    sl_cyc_i   : in  std_logic;
    sl_err_o   : out std_logic;
    sl_lock_i  : in  std_logic;
    sl_rty_o   : out std_logic;
    sl_sel_i   : in  std_logic_vector(c_sell -1 downto 0);
    sl_stb_i   : in  std_logic;
    sl_ack_o   : out std_logic;
    sl_we_i    : in  std_logic;
    sl_stall_o : out std_logic;

    -- This signals are not WB compatible. Should be connected to 0 if not
    -- used. 
    sl_psize_i       : in std_logic_vector(c_psizel downto 0);
--    sl_buff_access_i : in std_logic;

    -- Master WB port to fabric
    m_dat_i   : in  std_logic_vector(c_dl -1 downto 0);
    m_dat_o   : out std_logic_vector(c_dl -1 downto 0);
    m_adr_o   : out std_logic_vector(c_al -1 downto 0);
    m_cyc_o   : out std_logic;
    m_err_i   : in  std_logic;
    m_lock_o  : out std_logic;
    m_rty_i   : in  std_logic;
    m_sel_o   : out std_logic_vector(c_sell -1 downto 0);
    m_stb_o   : out std_logic;
    m_ack_i   : in  std_logic;
    m_we_o    : out std_logic;
    m_stall_i : in  std_logic

    );    
end FIFO;

architecture Behavioral of FIFO is
type t_FSM is (IDLE, EMPTY, VME_TO_FIFO, WB_TO_FIFO, FULL, FIFO_TO_VME, FIFO_TO_WB);
signal currs, nexts : t_FSM;
signal s_VMEWriteInFifo : std_logic;
signal s_WBWriteInFifo : std_logic;
signal s_download : std_logic;
signal s_EndWriteInWb : std_logic;  -- when s_index_b = s_index_a s_EndWriteInWb <= '1';
------
signal s_reset : std_logic;
signal s_index_a : unsigned(8 downto 0);
signal s_index_b : unsigned(7 downto 0);
signal s_addr : unsigned(7 downto 0);
signal s_reset_index_a : std_logic;
signal s_reset_index_b : std_logic;
--signal s_reset_addr : std_logic;
signal s_latchwr_or_rd : std_logic;
signal s_busy : std_logic;
signal s_error : std_logic;
signal s_blt_limit_error : std_logic;
signal s_ack : std_logic;
signal s_WbtoFifo : std_logic;
signal s_cyc : std_logic;
signal s_stb : std_logic;
signal s_FifotoWb : std_logic;
signal s_sel : std_logic_vector(c_sell -1 downto 0);
signal s_latchsel : std_logic;
signal s_latch : std_logic;
signal s_addrlatched : std_logic_vector(c_al -1 downto 0);
signal s_sellatched : std_logic_vector(c_sell -1 downto 0);
signal s_incr_index_a : std_logic;
signal s_wbbusyflag : std_logic;
signal s_encounter : std_logic;
signal s_counter : unsigned(8 downto 0);
signal s_counter_ack : unsigned(8 downto 0);
--signal s_reset_counter : std_logic;
--signal s_reset_counter_ack : std_logic;
signal s_encounter_ack : std_logic;
signal s_full : std_logic;
signal s_incr_index_b : std_logic;
signal s_enableB : std_logic;
signal s_sel_1 : std_logic_vector(c_sell -1 downto 0); 
signal s_incr_addr : std_logic;
signal sl_sel_we, m_sel_we: std_logic_vector(c_sell -1 downto 0);
signal s_wbbusyflag1 : std_logic;
signal s_temp2 : std_logic;
--COMPONENT TrueDpblockram
--	PORT(
--		clk_a_i : IN std_logic;
--		we_a_i : IN std_logic;
--		a_a_i : IN std_logic_vector(9 downto 0);
--		di_a_i : IN std_logic_vector(63 downto 0);
--		clk_b_i : IN std_logic;
--		we_b_i : IN std_logic;
--		a_b_i : IN std_logic_vector(9 downto 0);
--		di_b_i : IN std_logic_vector(63 downto 0);
--		enableB : IN std_logic;          
--		do_a_o : OUT std_logic_vector(63 downto 0);
--		do_b_o : OUT std_logic_vector(63 downto 0)
--		);
--	END COMPONENT;

begin

process(clk_i)
begin
  if rising_edge(clk_i) then
      if reset_i = '1' then currs <= IDLE;
      else currs <= nexts;
      end if;	
  end if;
end process;
--this process update the status of the main fsm
process(currs,s_VMEWriteInFifo, s_WBWriteInFifo, transfer_done_i, s_download, s_EndWriteInWb, m_err_i, m_rty_i, VMEtoWB, WBtoVME,s_busy,s_counter)
begin
   case currs is 
	    when IDLE =>
		     nexts <= EMPTY;
	    
		 when EMPTY =>
		     if (VMEtoWB = '1') then 
		         nexts <= VME_TO_FIFO;
           elsif (WBtoVME = '1') then
               nexts <= WB_TO_FIFO;
			  else 
               nexts <= EMPTY;			  
           end if;	

       when VME_TO_FIFO =>
		     if (transfer_done_i = '1') then 
		         nexts <= FULL;
           else 
               nexts <= VME_TO_FIFO;			  
           end if;	

       when WB_TO_FIFO =>
		     if (s_download = '1' or m_err_i = '1' or m_rty_i = '1' or s_busy = '1') then 
		         nexts <= FULL;
           else 
               nexts <= WB_TO_FIFO;			  
           end if;	
			  
		 when FULL =>	  
			  if (s_VMEWriteInFifo = '1' and s_counter < 65) then 
		         nexts <= FIFO_TO_WB;
			  elsif(s_counter = 65) then
               nexts <= EMPTY;			  
           elsif (s_WBWriteInFifo = '1') then
               nexts <= FIFO_TO_VME;
			  else 
               nexts <= FULL;			  
           end if;
			  
		 when FIFO_TO_VME =>	  
		     if (transfer_done_i = '1') then 
		         nexts <= EMPTY;
           else 
               nexts <= FIFO_TO_VME;			  
           end if;	
			  
		 when FIFO_TO_WB =>	  
		     if (s_EndWriteInWb = '1') then 
		         nexts <= EMPTY;
           elsif (m_err_i = '1' or m_rty_i = '1') then 
               nexts <= FULL;		
           else 
               nexts <= FIFO_TO_WB;			  
           end if;		  
			    
	end case;

end process;


--this is the output process who generate the control lines
process(currs, s_VMEWriteInFifo,m_err_i,m_rty_i,s_error,s_wbbusyflag)
begin
   case currs is
	     when IDLE =>
		       s_full          <= '0';
		       s_encounter     <= '0';
		       s_reset <= '1';
				-- s_reset_counter_ack <= '1';
				 s_encounter_ack     <= '0';
		       s_wbbusyflag    <= '0';
		       s_error         <= '0';
		       s_latchsel      <= '0';
		       s_latch         <= '0';
		       s_WbtoFifo      <= '0';
				-- s_reset_addr    <= '1';
		       s_reset_index_a <= '1';
				 s_reset_index_b <= '1';
		       s_latchwr_or_rd <= '0';
	          transfer_done_o <= '0';
	          s_busy          <= '0';
				 s_FifotoWb      <= '0';
	     when EMPTY =>
	          --s_reset_counter_ack <= '1';
				 s_encounter_ack     <= '0';	  		  
		       s_full          <= '0';
		       s_encounter     <= '0';
		       s_reset         <= '1';
		       s_wbbusyflag    <= '0';
		       s_error         <= '0';
		       s_latchsel      <= '0';
		       s_latch         <= '0';
		       s_WbtoFifo      <= '0';
				 --s_reset_addr    <= '1';
		       s_reset_index_a <= '1';
				 s_reset_index_b <= '1';
		       s_latchwr_or_rd <= '0';
				 transfer_done_o <= '0';
	          s_busy          <= '0';
				 s_FifotoWb      <= '0';
	     when VME_TO_FIFO =>
		       --s_reset_counter_ack <= '0';
				 s_encounter_ack     <= '0';
		       s_full          <= '0';
		       s_encounter     <= '1';
		       s_reset         <= '0';
		       s_wbbusyflag    <= '0';
		       s_error         <= '0';
		       s_latchsel      <= '1';
		       s_latch         <= '1';
		      -- s_reset_addr    <= '0';
		       s_WbtoFifo      <= '0';
		       s_reset_index_a <= '0';
				 s_reset_index_b <= '0';
				 s_latchwr_or_rd <= '1';
				 transfer_done_o <= '0';
				 s_busy          <= '0';
				 s_FifotoWb      <= '0';
		  when WB_TO_FIFO =>
		       --s_reset_counter_ack <= '0';
				 s_encounter_ack     <= '0';
		       s_full          <= '0';
		       s_encounter     <= '0';
		       s_reset         <= '0';
		       s_latchsel      <= '0';
		       s_latch         <= '1';
		       --s_reset_addr    <= '0';
		       s_WbtoFifo      <= '1';
		       s_reset_index_a <= '0';
				 s_reset_index_b <= '0';
		       s_latchwr_or_rd <= '1';
				 transfer_done_o <= '0';
				 s_busy          <= '0';
				 s_FifotoWb      <= '0';
				 if m_err_i = '1' then
				    s_error <= '1';
				 end if;	 
				 if m_rty_i = '1' then
				    s_wbbusyflag <= '1';
				 end if;	
				 
		  when FULL =>
		      -- s_reset_counter_ack <= '0';
				 s_encounter_ack     <= '0';
		       s_full          <= '1';
		       s_encounter     <= '0';
		       s_reset         <= '0';
		       s_wbbusyflag    <= '0';
		       s_latchsel      <= '0';
		       s_latch         <= '0';
		       --s_reset_addr    <= '0';
		       s_WbtoFifo      <= '0';
		       s_reset_index_a <= '0';
				 s_reset_index_b <= '1';
		       s_latchwr_or_rd <= '0';
				 transfer_done_o <= '1';
				 if s_VMEWriteInFifo = '1' then
				    s_busy       <= '1';
				 else 
                s_busy       <= '0';
             end if;					 
				 s_error         <= s_error;
				 s_FifotoWb      <= '0';
		  when FIFO_TO_VME =>
		      -- s_reset_counter_ack <= '0';
				 s_encounter_ack     <= '0';
		       s_full          <= '0';
		       s_encounter     <= '0';
		       s_reset         <= '0';
		       s_wbbusyflag    <= '0';
		       s_latchsel      <= '0';
		       s_latch         <= '0';
		      -- s_reset_addr    <= '0';
		       s_WbtoFifo      <= '0';
		       s_reset_index_a <= '0';
				 s_reset_index_b <= '0';
				 s_latchwr_or_rd <= '0';
				 transfer_done_o <= '1';
				 s_busy          <= '0';
				 s_error         <= s_error;
				 s_FifotoWb      <= '0';
		  when FIFO_TO_WB =>
		       --s_reset_counter_ack <= '0';
				 s_encounter_ack     <= '1';
		       s_full          <= '0';
		       s_encounter     <= '0';
		       s_reset         <= '0';
		       s_wbbusyflag    <= '0';
		       s_latchsel      <= '0';
		       s_latch         <= '0';
		       --s_reset_addr    <= '0';
		       s_WbtoFifo      <= '0';
		       s_reset_index_a <= '0';
				 s_reset_index_b <= '0';
	          s_latchwr_or_rd <= '0';
				 transfer_done_o <= '1';
				 s_busy          <= '1';
             if m_err_i = '1' then
				    s_error      <= '1';
				 end if;	
             s_FifotoWb      <= '1';
   end case;
end process;
----Latch signals---------------------------------------------------
process(clk_i)
begin
  if rising_edge(clk_i) then
      if s_reset = '1' then s_VMEWriteInFifo <= '0';
      elsif  s_latchwr_or_rd = '1' and s_ack = '1' then
		       s_VMEWriteInFifo <= VMEtoWB;
      end if;	
  end if;
end process;

process(clk_i)
begin
  if rising_edge(clk_i) then
      if s_reset = '1' then s_WBWriteInFifo <= '0';
      elsif  s_latchwr_or_rd = '1' then
		       s_WBWriteInFifo <= WBtoVME;
      end if;	
  end if;
end process;

process(clk_i)
begin
  if rising_edge(clk_i) then
      if s_reset = '1' then 
		       s_addrlatched <= (others => '0'); 
      elsif  s_latch = '1' and s_counter = 0 then
		       s_addrlatched <= sl_adr_i;			 
		else
 		       s_addrlatched <= s_addrlatched;			 
      end if;	
  end if;
end process;

process(clk_i)
begin
  if rising_edge(clk_i) then
      if s_reset = '1' then 
		       s_sellatched <= (others => '0'); 
      elsif  s_latchsel = '1' and s_counter = 0 then
		       s_sellatched <= sl_sel_i;			 
		else
 		       s_sellatched <= s_sellatched;			 
      end if;	
  end if;
end process;


-----------------------------------------------------------------------------
-- Counters for the s_index_a, s_index_b
process(clk_i)
begin
  if rising_edge(clk_i) then
      if s_reset_index_a = '1' then s_index_a <= (to_unsigned(0, s_index_a'length));
      elsif  s_ack = '1' and s_incr_index_a = '1' then
		       s_index_a <= s_index_a + 1;
		else   s_index_a <= s_index_a;		 
      end if;	
  end if;
end process;

process(clk_i)  
begin
  if rising_edge(clk_i) then
      if s_reset_index_b = '1' then s_index_b <= (to_unsigned(0, s_index_b'length));
      elsif s_WbtoFifo = '1' then
		    if  m_ack_i = '1' then
		        s_index_b <= s_index_b + 1;
		    else
      	  	  s_index_b <= s_index_b;
          end if;				  
      elsif s_FifotoWb = '1' then
		      if s_incr_index_b = '1' then
		         s_index_b <= s_index_b + 1;
				else 
				   s_index_b <= s_index_b;
				end if;	
		end if;	
  end if;
end process;
--count of the number of D32 access in the FIFO during VME to FIFO transfer
process(clk_i)  
begin
  if rising_edge(clk_i) then
      if s_reset = '1' then s_counter <= (to_unsigned(0, s_counter'length));
      elsif  s_ack = '1' and s_encounter = '1' then
		       s_counter <= s_counter + 1;
		else   s_counter <= s_counter;		 
      end if;	
  end if;
end process;
--count of the number of data transferred from fifo to wb bus
process(clk_i)  
begin
  if rising_edge(clk_i) then
      if s_reset = '1' then s_counter_ack <= (to_unsigned(0, s_counter'length));
      elsif  m_ack_i = '1' and s_encounter_ack = '1' then
		       s_counter_ack <= s_counter_ack + 1;
		else   s_counter_ack <= s_counter_ack;		 
      end if;	
  end if;
end process;

-- m_addr_o generator-----------------------------------------------
process(clk_i)  
begin
  if rising_edge(clk_i) then
      if s_reset = '1' then s_addr <= (to_unsigned(0, s_addr'length));
      elsif  s_WbtoFifo = '1' then
             if(s_cyc = '1' and s_stb = '1' and m_stall_i = '0') then
		          s_addr <= s_addr + 1;
		       else   
				    s_addr <= s_addr;		 
				 end if;
		elsif  s_FifotoWb = '1' then	
             if(s_cyc = '1' and s_stb = '1' and m_stall_i = '0' and s_incr_addr = '1') then
		          s_addr <= s_addr + 1;
		       else   
				    s_addr <= s_addr;		 
				 end if;
      end if;	
  end if;
end process;


-- Ack generator:
process(clk_i)
begin
  if rising_edge(clk_i) then
       s_ack <= (sl_cyc_i and sl_stb_i);	 -- Ack is asserted also if rty or err are '1' becouse the VME_bus.vhd		                                     -- has to go out from the MEMORY_REQ state!!
  end if;	
end process;
sl_ack_o <= s_ack;
-- Retry generator:
process(clk_i)
begin
  if rising_edge(clk_i) then
       if (sl_cyc_i = '1' and sl_stb_i = '1' and (s_busy = '1' or s_wbbusyflag1 = '1')) then
           sl_rty_o <= '1';
       else 
            sl_rty_o <= '0';	
       end if;				
  end if;	
end process;
-- Error generator:--------------------------------------------------------
process(clk_i)
begin
  if rising_edge(clk_i) then
       if (sl_cyc_i = '1' and sl_stb_i = '1' and s_error = '1') then
           sl_err_o <= '1';
       else 
           sl_err_o <= '0';	
       end if; 
  end if;	
end process;

--s_blt_limit_error <= '1' when to_integer(s_index_a) >= sl_psize_i else '0'; 
-----------------------------------------------------------------------
--Transfer data between Wb and Fifo--------------------------------------
process(clk_i)
begin
  if rising_edge(clk_i) then
       if s_WbtoFifo = '1' then
           if m_err_i = '0' and m_rty_i = '0' and s_busy = '0' then
			     
				  if ((s_addr = unsigned(sl_psize_i) and m_stall_i = '0') or s_addr >= unsigned(sl_psize_i)+1) then
				     s_stb <= '0';
              else					  
				     s_stb <= '1';
              end if;
				  if ((s_index_b = unsigned(sl_psize_i) and m_ack_i = '1')or s_index_b > unsigned(sl_psize_i)) then
				     s_cyc <= '0';
              else					  
				     s_cyc <= '1';
              end if;
				  
			  else 
              s_cyc <= '0';
              s_stb <= '0';
			  end if;	
			  m_we_o <= '0';
			  s_sel <= (others => '1');
	 
		 elsif s_FifotoWb = '1' then
		   if m_err_i = '0' and m_rty_i = '0' then
		      m_we_o <= '1';
			   if (((s_counter_ack = s_counter -2) and m_stall_i = '0') or (s_counter_ack >= s_counter -1)) then
			       s_stb <= '0';
			   else 		
			       s_stb <= '1';
			   end if;		
			   if ((s_counter_ack >= s_counter - 1) and s_stb = '0') then
			       s_cyc <= '0';
			   else 		
			       s_cyc <= '1';
			   end if;
			  
			   if m_stall_i = '0' then
			      s_sel <= s_sel_1;
			   else
               s_sel <= s_sel;		
            end if;	
			 else
             s_cyc <= '0';
             s_stb <= '0';		
				 m_we_o <= '0';
			 end if;	 
		  else
		       s_sel <= (others => '0');
		       m_we_o <= '0';
      		 s_cyc <= '0';
		       s_stb <= '0'; 
       end if;				 
  end if;	
end process;
--1 Tclk delay-------------------------------
process(clk_i)
begin
if rising_edge(clk_i) then
   if s_full = '1' then
	   s_sel_1 <= s_sellatched;
	elsif s_FifotoWb = '1' and (m_stall_i = '0') then
      s_sel_1 <= not s_sel_1;
   else 
      s_sel_1 <= (others => '0');	
   end if;
end if;	
end process;
-- End write in wb
process(clk_i)
begin
if rising_edge(clk_i) then
   if s_FifotoWb = '1' and s_counter = s_counter_ack then
       s_EndWriteInWb <= '1';
   else 
       s_EndWriteInWb <= '0';	
   end if;
end if;	
end process;

process(clk_i)
	begin
	if rising_edge(clk_i) then
      if transfer_done_i = '1' or s_EndWriteInWb = '1' then s_wbbusyflag1 <= '0';
      elsif s_temp2 = '1' then
            s_wbbusyflag1 <= s_wbbusyflag;
	   end if;
	end if;	
end process;
s_temp2 <= not s_wbbusyflag1;


----------------------------------------------------------
sl_stall_o <= '0';
m_cyc_o <= s_cyc;
m_stb_o <= s_stb;
m_sel_o <= s_sel;
m_adr_o <= std_logic_vector(unsigned(s_addrlatched) + s_addr);
s_download <= '1' when s_index_b = (unsigned(sl_psize_i) + 1) else '0';
s_incr_index_a <= sl_sel_i(0) and sl_sel_i(1) and sl_sel_i(2) and sl_sel_i(3);
s_incr_index_b <= s_sel_1(0) and s_sel_1(1) and s_sel_1(2) and s_sel_1(3);
s_enableB <= s_WbtoFifo or (s_FifotoWb and (not m_stall_i));
s_incr_addr <= s_sel(0) and s_sel(1) and s_sel(2) and s_sel(3);


GMEM: for I in 0 to (c_sell -1) generate
  sl_sel_we(I) <= sl_sel_i(I) and sl_we_i and sl_cyc_i and sl_stb_i and (not s_busy); 
  m_sel_we(i) <= m_ack_i and s_WbtoFifo and s_sel(I);

  UTrueDpblockram : entity work.TrueDpblockram
    generic map(dl => c_dl/(c_sell),             -- Length of the data word 
                al => c_psizel)  -- Size of the addr map (10 = 1024 words)
    -- 'nw' has to be coherent with 'al'
    port map(clk_a_i => clk_i,
             we_a_i  => sl_sel_we(I),
             a_a_i   => std_logic_vector(s_index_a(7 downto 0)),
             di_a_i  => sl_dat_i((I+1)*8 -1 downto I*8),
             do_a_o  => sl_dat_o((I+1)*8 -1 downto I*8),
             clk_b_i => clk_i,
             we_b_i  => m_sel_we(I),
             a_b_i   => std_logic_vector(s_index_b),
             di_b_i  => m_dat_i((I+1)*8 -1 downto I*8),
             do_b_o  => m_dat_o((I+1)*8 -1 downto I*8),
				 enableB => s_enableB);
 end generate;
	 
end Behavioral;

