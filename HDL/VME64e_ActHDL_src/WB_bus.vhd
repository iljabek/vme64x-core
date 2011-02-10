-------------------------------------------------------------------------------
--
-- Title       : WB_bus
-- Design      : VME64xCore
-- Author      : Ziga Kroflic
-- Company     : Cosylab
--
-------------------------------------------------------------------------------
--
-- File        : WB_bus.vhd
-- Generated   : Tue Mar 30 11:59:59 2010
-- From        : interface description file
-- By          : Itf2Vhdl ver. 1.20
--
-------------------------------------------------------------------------------
--
-- Description : 
--
-------------------------------------------------------------------------------

--{{ Section below this comment is automatically maintained
--   and may be overwritten
--{entity {WB_bus} architecture {RTL}}

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_unsigned.all;

entity WB_bus is   
    port (
        clk_i:           in std_logic;
        reset_i:         in std_logic;                        -- propagated from VME
        
        RST_i:           in std_logic;
        DAT_i:           in std_logic_vector(63 downto 0);
        DAT_o:           out std_logic_vector(63 downto 0);
        ADR_o:           out std_logic_vector(63 downto 0);
        CYC_o:           out std_logic;
        ERR_i:           in std_logic;
        LOCK_o:          out std_logic;
        RTY_i:           in std_logic;
        SEL_o:           out std_logic_vector(7 downto 0);
        STB_o:           out std_logic;
        ACK_i:           in std_logic;
        WE_o:            out std_logic;
        STALL_i:         in std_logic;
        
        memReq_i:        in std_logic;                 
        memAck_o:        out std_logic;                  
        locData_o:       out std_logic_vector(63 downto 0); 
        locData_i:       in std_logic_vector(63 downto 0);
        locAddr_i:       in std_logic_vector(63 downto 0);
        sel_i:           in std_logic_vector(7 downto 0);
        RW_i:            in std_logic;                 
        lock_i:          in std_logic;                 
        err_o:           out std_logic;
        rty_o:           out std_logic;
        cyc_i:           in std_logic;
        
        beatCount_i:     in std_logic_vector(7 downto 0);
        
        FIFOrden_o:      out std_logic;
        FIFOwren_o:      out std_logic;
        FIFOdata_i:      in std_logic_vector(63 downto 0);
        FIFOdata_o:      out std_logic_vector(63 downto 0);
        FIFOreset_o:     out std_logic;
        writeFIFOempty_i: in std_logic;
        TWOeInProgress_i: in std_logic;
        WBbusy_o:        out std_logic
        
        );    
end WB_bus;

architecture RTL of WB_bus is

signal s_reset: std_logic;

signal s_locDataOut: std_logic_vector(63 downto 0);       -- local data
SIgnal s_locAddr: std_logic_vector(63 downto 0);          -- local address

signal s_FSMactive: std_logic;          -- signals when SST FIFO is being emptied
signal s_cyc: std_logic;                -- CYC signal (for control in 2eFSM)
signal s_stb: std_logic;                -- STB signal (for control in p_pipSTB)      
signal s_addrLatch: std_logic;          -- store initial address locally 

signal s_pipeCommActive: std_logic;      -- indicates tha 2eFSM is active (transfer is in progress)

signal s_WE: std_logic;                  -- WE signal (for control in 2eFSM)

signal s_runningBeatCount: std_logic_vector(8 downto 0);      -- internal beat counter
signal s_beatCount: std_logic_vector(7 downto 0);             -- registered beat count (received from VME core)
signal s_beatCountEnd: std_logic;                             -- marks that beat counter has reached the final value
signal s_cycleDelay: std_logic;                               -- used for delaying s_pipeCommActive signal for one clock cycle

signal s_ackCount: std_logic_vector(7 downto 0);              -- ACK tick counter
signal s_ackCountEnd: std_logic;                              -- marks that all expected ACK ticks have been received

signal s_FIFOrden: std_logic;                                 -- FIFO read enable

signal s_FIFOreset, s_FIFOreset_1, s_FIFOreset_2: std_logic;          -- Resets FIFO at the end of each transfer

type t_2eFSMstates is ( IDLE, 
                        ADDR_LATCH, 
                        SET_CONTROL_SIGNALS, 
                        DO_PIPELINED_COMM, 
                        WAIT_FOR_END, 
                        FIFO_RESET
                        );
signal s_2eFSMstate: t_2eFSMstates;      

begin 
    
s_reset <= reset_i or RST_i; 


-- WB data latching

p_dataLatch: process(clk_i)
begin
    if rising_edge(clk_i) then
        if ACK_i='1' then
            s_locDataOut <= DAT_i;
         else
            s_locDataOut <= s_locDataOut;
        end if;
    end if;
end process;

locData_o     <= s_locDataOut;
FIFOdata_o    <= DAT_i;

--p_ioRegister: process(clk_i)
--begin
--    if rising_edge(clk_i) then
--        if s_FSMactive='0' then
--            DAT_o    <= locData_i;
--            ADR_o    <= locAddr_i;                      
--            WE_o     <= not RW_i;
--            err_o    <= ERR_i;   
--            rty_o    <= RTY_i;   
--            SEL_o    <= sel_i;   
--            CYC_o    <= cyc_i;
--            STB_o    <= memReq_i;
--            memAck_o <= ACK_i;
--       else
--            DAT_o    <= FIFOdata_i;
--            ADR_o    <= s_locAddr;
--            WE_o     <= s_WE;
--            err_o    <= '0';
--            rty_o    <= '0';
--            SEL_o    <= (others => '1');
--            CYC_o    <= s_cyc;
--            STB_o    <= s_stb;
--            memAck_o <= '0';
--       end if;
--       
--       LOCK_o   <= lock_i;
--   end if;
--end process;


STB_o <= memReq_i         when s_FSMactive='0' else s_stb;
memAck_o <= ACK_i         when s_FSMactive='0' else '0';
        
DAT_o         <= locData_i   when s_FSMactive='0' else FIFOdata_i;

ADR_o <= locAddr_i           when s_FSMactive='0' else s_locAddr;

WE_o     <= not RW_i         when s_FSMactive='0' else s_WE;
LOCK_o   <= lock_i;
err_o    <= ERR_i            when s_FSMactive='0' else '0';
rty_o    <= RTY_i            when s_FSMactive='0' else '0';
SEL_o    <= sel_i            when s_FSMactive='0' else (others => '1');
CYC_o    <= cyc_i            when s_FSMactive='0' else s_cyc;

WBbusy_o <= s_FSMactive; 



    
-- 2e FSM

p_2eFSM: process(clk_i)
begin 
    if rising_edge(clk_i) then
        if s_reset='1' then
            s_FSMactive              <='0';
            s_cyc                    <='0';
            s_pipeCommActive         <='0';
            s_WE                     <='0';
            s_addrLatch              <='0';
            s_FIFOreset              <='0';
            s_2eFSMstate            <= IDLE;
        else
            case s_2eFSMstate is
                
                when IDLE =>
                s_FSMactive          <='0';
                s_cyc                <='0';
                s_WE                 <= not RW_i;
                s_addrLatch          <='0';
                s_pipeCommActive     <='0';
                s_FIFOreset          <='0';
                if TWOeInProgress_i='1' then    
                    s_2eFSMstate    <= ADDR_LATCH;
                end if;
                
                when ADDR_LATCH =>
                s_FSMactive          <='1';
                s_cyc                <='1';
                s_WE                 <= s_WE;
                s_addrLatch          <='1';
                s_pipeCommActive     <='0';
                s_FIFOreset          <='0';
                s_2eFSMstate        <= SET_CONTROL_SIGNALS;
                
                when SET_CONTROL_SIGNALS =>
                s_FSMactive          <='1';
                s_cyc                <='1';
                s_WE                 <= s_WE;
                s_addrLatch          <='0';    
                s_pipeCommActive     <='0'; 
                s_FIFOreset          <='0';
                s_2eFSMstate        <= DO_PIPELINED_COMM;
                
                when DO_PIPELINED_COMM =>
                s_FSMactive          <='1';
                s_cyc                <='1';
                s_WE                 <= s_WE;
                s_addrLatch          <='0';
                s_pipeCommActive     <='1';
                s_FIFOreset          <='0';
                if s_ackCountEnd='1' then
                    s_2eFSMstate   <= WAIT_FOR_END;
                else
                    s_2eFSMstate   <= DO_PIPELINED_COMM;
                end if;
                
                when WAIT_FOR_END =>
                s_FSMactive          <='0';
                s_cyc                <='0';
                s_WE                 <= s_WE;
                s_addrLatch          <='0';
                s_pipeCommActive     <='0';
                s_FIFOreset          <='0';
                if TWOeInProgress_i='0' then
                    s_2eFSMstate   <= FIFO_RESET;
                end if;
                
                when FIFO_RESET =>
                s_FSMactive          <='0';
                s_cyc                <='0';
                s_WE                 <= s_WE;
                s_addrLatch          <='0';
                s_pipeCommActive     <='0';
                s_FIFOreset          <='1';
                s_2eFSMstate         <= IDLE;
                
                when OTHERS =>
                s_FSMactive          <='0';
                s_cyc                <='0';
                s_WE                 <= s_WE;
                s_addrLatch          <='0';
                s_pipeCommActive     <='0';
                s_FIFOreset          <='0';
                s_2eFSMstate        <= IDLE;
            
            end case;
        end if;
    end if;
end process;
                

-- Local address latching & incrementing

p_locAddrHandling: process(clk_i)
begin
    if rising_edge(clk_i) then
        if s_reset='1' then
            s_locAddr <= (others => '0');
        elsif s_addrLatch='1' then
            s_locAddr <= locAddr_i;
        elsif s_pipeCommActive='1' and STALL_i='0' and s_cycleDelay='1' and ((writeFIFOempty_i='0' and s_WE='1') or (s_WE='0')) then                                                                         
            s_locAddr <= s_locAddr + 8;
        else
            s_locAddr <= s_locAddr;
        end if;
    end if;
end process;


-- Beat counter

p_FIFObeatCounter: process(clk_i)
begin
    if rising_edge(clk_i) then
        if s_reset='1' or s_pipeCommActive='0' then
            s_runningBeatCount <= (others => '0');
        elsif s_pipeCommActive='1' and STALL_i='0' and ((writeFIFOempty_i='0' and s_WE='1') or (s_WE='0')) then
            s_runningBeatCount <= s_runningBeatCount + 1;
        else
            s_runningBeatCount <= s_runningBeatCount;
        end if;
    end if;
end process;

s_beatCountEnd <= '0' when s_runningBeatCount < beatCount_i else '1';    
    
    
-- One clock cycle delay

p_cycleDelay: process(clk_i)
begin
    if rising_edge(clk_i) then
        s_cycleDelay <= s_pipeCommActive;
    end if;
end process;  


-- ACK pulse counter

p_ackCounter: process(clk_i)
begin
    if rising_edge(clk_i) then
        if s_reset='1' or s_addrLatch='1' then
            s_ackCount <= (others => '0');
        elsif ACK_i='1' then
            s_ackCount <= s_ackCount + 1;
        else
            s_ackCount <= s_ackCount;
        end if;
    end if;
end process;

s_ackCountEnd <= '1' when s_ackCount=s_beatCount else '0';
 
    
-- Beat count register

p_beatCountRegister: process(clk_i)
begin
    if rising_edge(clk_i) then 
	  if reset_i = '1' then 
            s_beatCount <= (others => '0');
	  else
        if s_addrLatch='1' then
            s_beatCount <= beatCount_i;
        else
            s_beatCount <= s_beatCount;
        end if;
	  end if;
    end if;
end process; 


-- Pipelined transfer STB signal control    

p_pipSTB: process(clk_i)
begin
    if rising_edge(clk_i) then
	  if reset_i = '1' then 
	         s_stb <= '0';
     else
        if s_pipeCommActive='1' and STALL_i='0' and ((writeFIFOempty_i='0' and s_WE='1') or (s_WE='0')) and s_beatCountEnd='0' then
            s_stb <= '1';
        else
            s_stb <= '0';
        end if;
		end if;
    end if;
end process; 

s_FIFOrden <= '1' when s_pipeCommActive='1' and s_WE='1' and STALL_i='0' and writeFIFOempty_i='0' and s_beatCountEnd='0' else '0';
FIFOrden_o <= s_FIFOrden;  

FIFOwren_o <= ACK_i when s_pipeCommActive='1' and s_WE='0' else '0';  
    
    
-- FIFO reset 

p_FIFOresetStretch: process(clk_i)
begin
    if rising_edge(clk_i) then
        s_FIFOreset_1 <= s_FIFOreset;
        s_FIFOreset_2 <= s_FIFOreset_1;
    end if;
end process;

FIFOreset_o <= s_FIFOreset or s_FIFOreset_1 or s_FIFOreset_2;



        
end RTL;
