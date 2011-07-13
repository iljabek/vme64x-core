-------------------------------------------------------------------------------
--
-- Title       : WB_bus
-- Design      : VME64xCore
-- Author      : Pablo Alvarez
-- Company     : CERN
--
-------------------------------------------------------------------------------
--
-- File        : wb_dma.vhd
-- Generated   : 25/02/2011
-- From        : interface description file
--
-------------------------------------------------------------------------------
--
-- Description : 
--
-------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.numeric_std.all;
use work.common_components.all;

entity wb_dma is
  generic(c_dl     : integer := 64;
          c_al     : integer := 64;
          c_sell   : integer := 8;
          c_psizel : integer := 10);

  port (
    -- Common signals 
    clk_i           : in  std_logic;
    reset_i         : in  std_logic;
    transfer_done_o : out std_logic;

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
    sl_psize_i       : in std_logic_vector(c_psizel -1 downto 0);
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
end wb_dma;

architecture RTL of wb_dma is

  type t_trans_st is (IDLE, S_TO_M_BUFFERING, S_TO_M_DONE, M_TO_S_BUFFERING,
  M_TO_S_DONE_1, M_TO_S_DONE_2, M_TO_S_WAIT_LAST_ACK);
  signal trans_st, nx_trans_st : t_trans_st;
  signal m_stb_index, m_ack_index, m_index, s_index                            : unsigned(c_psizel - 1 downto 0);  -- fifo indexes
  signal is_sl_index_top, is_m_stb_index_top, is_m_ack_index_top, fifo_ack_empty, fifo_stb_empty : std_logic;
  signal is_sl_index_top_m1 : std_logic;
  signal inc_s_index, inc_m_stb_index, inc_m_ack_index, reset_index       : std_logic;
  signal nx_sl_ack                   : std_logic;
  signal m_stb, nx_m_stb, nx_transfer_done       : std_logic;
  signal m_we, nx_m_we        : std_logic;
  signal nx_sl_stall : std_logic;
  signal latch_psize : std_logic;
  signal psize : unsigned(c_psizel downto 0);
  signal nx_m_adr, m_adr : unsigned(c_al -1 downto 0);
  signal m_latch : std_logic;
  signal nx_sl_dat_o : std_logic_vector(c_dl -1 downto 0);
  signal sl_sel_we, m_sel_we, m_sel : std_logic_vector(c_sell -1 downto 0);
  signal advance_m_adr, inc_m_adr : std_logic;
  signal is_m_ack_index_top_m1 : std_logic;
begin
-------------------------------------------------------------------------------
  process(clk_i)
  begin
    if rising_edge(clk_i) then
    if reset_i = '1' or m_err_i = '1' or sl_cyc_i = '0' then
      trans_st <= IDLE;
    else 
      trans_st <= nx_trans_st;
    end if;
    end if;
  end process;

-----------------------------------------------------------------------------
  process(trans_st, sl_sel_i, sl_stb_i, sl_psize_i, sl_we_i,nx_sl_ack,
  is_sl_index_top, is_m_stb_index_top, is_m_ack_index_top, is_sl_index_top_m1)
  begin
    nx_trans_st <= IDLE;
    case trans_st is
      when IDLE =>
        if unsigned(sl_sel_i) /= 0 and sl_stb_i = '1' then
          if sl_we_i = '1' then
            nx_trans_st <= S_TO_M_BUFFERING;
          else
            nx_trans_st <= M_TO_S_BUFFERING;
          end if;
        else
          nx_trans_st <= IDLE;
        end if;
      when S_TO_M_BUFFERING =>
        if is_sl_index_top = '1' then
          nx_trans_st <= S_TO_M_DONE;
        else
          nx_trans_st <= S_TO_M_BUFFERING;
        end if;
      when S_TO_M_DONE =>
        if is_m_stb_index_top = '1' then
          nx_trans_st <= IDLE;
        else
          nx_trans_st <= S_TO_M_DONE;
        end if;
      when M_TO_S_BUFFERING =>
        if is_m_ack_index_top = '1' then
          nx_trans_st <= M_TO_S_DONE_1 ;
        else
          nx_trans_st <=M_TO_S_BUFFERING ;
        end if;
      when M_TO_S_DONE_1 =>
          nx_trans_st <= M_TO_S_DONE_2;
      when M_TO_S_DONE_2 =>
        if is_sl_index_top_m1 = '1' then
          nx_trans_st <= M_TO_S_WAIT_LAST_ACK;
        else
          nx_trans_st <= M_TO_S_DONE_2;
        end if;
      when M_TO_S_WAIT_LAST_ACK =>
		   if ((sl_stb_i= '1') and (is_m_ack_index_top = '1')) or (psize = 1) then 
          nx_trans_st <= IDLE;
         else
          nx_trans_st <= M_TO_S_WAIT_LAST_ACK;
         end if;			
      when others => nx_trans_st <= IDLE;
    end case;
  end process;
-----------------------------------------------------------------------------
  fifo_stb_empty      <= '1' when m_stb_index = s_index              else '0';
  is_sl_index_top <= '1' when s_index >= psize else '0';
  is_sl_index_top_m1 <= '1' when signed(s_index) >= signed((psize - 1)) else '0';
  is_m_stb_index_top  <= '1' when m_stb_index >= psize else '0';
  is_m_ack_index_top  <= '1' when m_ack_index >= psize else '0';
  is_m_ack_index_top_m1  <= '1' when signed(s_index) >= signed((psize - 1)) else '0';

-----------------------------------------------------------------------------
  process(m_ack_i,inc_m_stb_index, is_m_ack_index_top_m1, m_ack_index, m_stb_index, m_stall_i,m_stb, sl_we_i, sl_stb_i, fifo_stb_empty,
  is_sl_index_top, nx_trans_st, trans_st, is_m_stb_index_top, is_m_ack_index_top, is_sl_index_top_m1)
  begin
    inc_s_index    <= '0';
--    inc_m_stb_index <= '0';
    reset_index    <= '0';
    nx_sl_ack      <= '0';
    nx_m_stb       <= '0';
    nx_transfer_done <= '0';
	 nx_sl_stall <= '0'; 
	 latch_psize <= '0';
	     nx_m_we <= '0';
		  m_latch <= '0';
		  advance_m_adr <= '0';
		  m_index <= m_stb_index; -- default case when the master is writing to the wb slave (s to m)
		  inc_m_adr <= '0';
    case trans_st is
      when IDLE =>
		  if trans_st /= nx_trans_st then
		  latch_psize <= '1';
        inc_s_index    <= sl_stb_i and sl_we_i;
        nx_sl_ack      <= sl_stb_i and sl_we_i;
	     nx_m_we <= sl_we_i;
        reset_index <= '0';
        else
        reset_index <= '1';	  
		  end if;
      when S_TO_M_BUFFERING =>
        nx_m_stb       <= (not is_m_stb_index_top) and (not fifo_stb_empty) and (not m_stall_i);
        inc_s_index    <= sl_stb_i;
        nx_sl_ack      <= sl_stb_i and (not is_sl_index_top);
		  nx_sl_stall <= '0'; 
	     nx_m_we <= '1';
		  m_index <= m_stb_index; --  case when the master is writing to the wb slave (s to m)
        inc_m_adr <= inc_m_stb_index;
        nx_transfer_done <= sl_stb_i and (is_sl_index_top_m1);
      when S_TO_M_DONE =>
        nx_m_stb       <= (not is_m_stb_index_top) and (not fifo_stb_empty) and (not m_stall_i);
	     nx_m_we <= '1';
		  m_index <= m_stb_index; --  case when the master is writing to the wb slave (s to m)
        inc_m_adr <= inc_m_stb_index;
      when M_TO_S_BUFFERING =>
        nx_m_stb       <= (not is_m_stb_index_top)  and (not m_stall_i);
		  m_index <= m_ack_index; --  case when the master is reading from the wb slave (s to m)
        inc_m_adr <= inc_m_stb_index;
		  m_latch <= m_ack_i;
		  advance_m_adr <= '0';
      when M_TO_S_DONE_1 =>
        nx_transfer_done <= is_sl_index_top_m1 and is_m_ack_index_top;
		  nx_sl_stall <= '0'; 
        nx_sl_ack      <= '1';--sl_stb_i and is_m_index_top and (not is_sl_index_top);
        inc_s_index    <= '1';
		  advance_m_adr <= '0';
		  m_index <= m_ack_index; --  case when the master is reading from the wb slave (s to m)
        inc_m_adr <= inc_m_stb_index;
      when M_TO_S_DONE_2 | M_TO_S_WAIT_LAST_ACK =>
        nx_transfer_done <= sl_stb_i and is_m_ack_index_top_m1 and is_sl_index_top_m1;
		  nx_sl_stall <= '0'; 
        nx_sl_ack      <= sl_stb_i and is_m_ack_index_top; --and (not is_sl_index_top);
        inc_s_index    <= sl_stb_i and is_m_ack_index_top;-- and (not is_sl_index_top);
		  advance_m_adr <= '0';
		  m_index <= m_ack_index; --  case when the master is reading from the wb slave (s to m)
        inc_m_adr <= inc_m_stb_index;
      when others =>
    end case;
  end process;
--  aux_sl_ack <= sl_stb_i and is_m_index_top and (not is_sl_index_top);
  -----------------------------------------------------------------------------
  inc_m_stb_index <= nx_m_stb;
  inc_m_ack_index <= m_ack_i;
  -----------------------------------------------------------------------------
  process(clk_i)
  begin
    if rising_edge(clk_i) then
      if reset_i = '1' or reset_index = '1' then
        m_stb_index <= to_unsigned(0, m_stb_index'length);
      elsif inc_m_stb_index = '1' then
        m_stb_index <= m_stb_index + 1;
      end if;
		
      if reset_i = '1' or reset_index = '1' then
        m_ack_index <= to_unsigned(0, m_ack_index'length);
      elsif inc_m_ack_index = '1' then
        m_ack_index <= m_ack_index + 1;
      end if;
		
      if reset_i = '1' or reset_index = '1' then
        s_index <= to_unsigned(0, s_index'length);
      elsif inc_s_index = '1' then
        s_index <= s_index + 1;
      end if;
    end if;
  end process;
-------------------------------------------------------------------------------
  process(m_adr, sl_adr_i, latch_psize, inc_m_adr)
  begin
  	   if latch_psize = '1' then
      nx_m_adr         <= unsigned(sl_adr_i);
		elsif inc_m_adr = '1' then
		nx_m_adr <= m_adr + 1;
		else
		nx_m_adr <= m_adr;
		end if;
  end process;
-------------------------------------------------------------------------------
  process(clk_i)
  begin
    if rising_edge(clk_i) then
	   if latch_psize = '1' then
		   psize <= unsigned('0'&sl_psize_i);
         m_sel <= sl_sel_i;
		end if;
		m_adr <= nx_m_adr;
		if advance_m_adr = '1' then
		   m_adr_o <= std_logic_vector(nx_m_adr);
		else
         m_adr_o <= std_logic_vector(m_adr);
		end if;
      sl_ack_o        <= nx_sl_ack;
		if nx_sl_ack = '1' then
		sl_dat_o <= nx_sl_dat_o;
      end if;		
      m_stb         <= nx_m_stb;
      transfer_done_o <= nx_transfer_done;
      m_cyc_o         <= sl_cyc_i;
		sl_stall_o      <= nx_sl_stall;
      sl_err_o <= m_err_i;
		m_we_o <= nx_m_we;
    end if;
  end process;
  
  m_stb_o <= m_stb;
  m_sel_o <= m_sel; 
-------------------------------------------------------------------------------
GMEM: for I in 0 to (c_sell -1) generate
  sl_sel_we(I) <= sl_sel_i(I) and sl_we_i; 
  m_sel_we(i) <= m_latch and m_sel(I);

  UTrueDpblockram : TrueDpblockram
    generic map(dl => c_dl/(c_sell),             -- Length of the data word 
                al => c_psizel)  -- Size of the addr map (10 = 1024 words)
    -- 'nw' has to be coherent with 'al'
    port map(clk_a_i => clk_i,
             we_a_i  => sl_sel_we(I),
             a_a_i   => std_logic_vector(s_index),
             di_a_i  => sl_dat_i((I+1)*8 -1 downto I*8),
             do_a_o  => nx_sl_dat_o((I+1)*8 -1 downto I*8),
             clk_b_i => clk_i,
             we_b_i  => m_sel_we(I),
             a_b_i   => std_logic_vector(m_index),
             di_b_i  => m_dat_i((I+1)*8 -1 downto I*8),
             do_b_o  => m_dat_o((I+1)*8 -1 downto I*8));
    end generate;
	 
end RTL;
