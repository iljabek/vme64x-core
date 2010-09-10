-------------------------------------------------------------------------------
--
-- Title       : IRQ_controller
-- Design      : VME64xCore
-- Author      : Ziga Kroflic
-- Company     : Cosylab
--
-------------------------------------------------------------------------------
--
-- File        : IRQ_controller.vhd
-- Generated   : Thu Apr  1 08:48:48 2010
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
--{entity {IRQ_controller} architecture {RTL}}

library IEEE;
use IEEE.STD_LOGIC_1164.all; 
use IEEE.STD_LOGIC_unsigned.all;

entity IRQ_controller is
     port(
        clk_i :             in std_logic;
        reset_i :           in std_logic;
        VME_IRQ_n_o :       out std_logic_vector(6 downto 0);
        VME_IACKIN_n_i :    in std_logic;
        VME_IACKOUT_n_o :   out std_logic;
        VME_AS_n_i :        in STD_LOGIC;
        VME_DS_n_i :        in STD_LOGIC_VECTOR(1 downto 0);
        irqDTACK_o :        out std_logic;
        IACKinProgress_o:   out std_logic;
        IRQ_i:              in std_logic;
        locAddr_i:          in std_logic_vector(3 downto 1);
        IDtoData_o:         out std_logic;
        IRQlevelReg_i:      in std_logic_vector(7 downto 0)
        );
end IRQ_controller;

architecture RTL of IRQ_controller is

component RisEdgeDetection is
    port (
        sig_i, clk_i: in std_logic;
        RisEdge_o: out std_logic );
end component;

component SigInputSample is
    port (
        sig_i, clk_i: in std_logic;
        sig_o: out std_logic );
end component;

component RegInputSample is 
    generic(
        width: natural:=8
        );
    port (
        reg_i: in std_logic_vector(width-1 downto 0);
        reg_o: out std_logic_vector(width-1 downto 0):=(others => '0');
        clk_i: in std_logic 
        );
end component; 

signal VME_IACKIN_n_oversampled: std_logic;
signal VME_DS_n_oversampled : STD_LOGIC_VECTOR(1 downto 0);

signal s_reset: std_logic;

signal s_VME_IACKOUT: std_logic;

signal s_irqDTACK: std_logic;                            -- acknowledge of IACK cycle 
signal s_applyIRQmask: std_logic;                        -- clears acknowlegded interrupt
signal s_IDtoData: std_logic;                            -- puts IRQ Status/ID register on data bus
signal s_IACKmatch: std_logic;                           -- signals that an active interrupt is being acknowledged
signal s_wbIRQrisingEdge: std_logic;                     -- rising edge detection on interrupt line 
signal s_IRQenabled: std_logic;                          -- indicates that interrupts are enabled (IRQlevelReg has a valid level value)    
signal s_IRQreg: std_logic;                              -- registers pending interrupt

type t_IRQstates is (   IDLE, 
                        WAIT_FOR_DS, 
                        CHECK_MATCH, 
                        APPLY_MASK_AND_DATA, 
                        PROPAGATE_IACK, 
                        APPLY_DTACK
                        );
signal s_IRQstate: t_IRQstates;

begin 
    
s_reset <= reset_i;

irqDTACK_o <= '0' when s_irqDTACK='0' else 'Z';
VME_IACKOUT_n_o <= '0' when s_VME_IACKOUT='0' else 'Z';    

p_IRQcontrolFSM: process(clk_i)
begin
    if rising_edge(clk_i) then
        if s_reset='1' then 
            s_VME_IACKOUT        <= '1';
            s_irqDTACK           <= '1';
            s_applyIRQmask       <= '0';
            s_IDtoData           <= '0';
            IACKinProgress_o     <= '0';
            s_IRQstate           <= IDLE;
        else
            case s_IRQstate is
                
                when IDLE => 
                s_VME_IACKOUT    <= '1';
                s_irqDTACK       <= '1';
                s_applyIRQmask   <= '0';
                s_IDtoData       <= '0';    
                IACKinProgress_o <= '0';
                if VME_IACKIN_n_oversampled='0' then
                    s_IRQstate   <= WAIT_FOR_DS;
                else
                    s_IRQstate   <= IDLE;
                end if;
                
                when WAIT_FOR_DS =>    
                s_VME_IACKOUT    <= '1';
                s_irqDTACK       <= '1';
                s_applyIRQmask   <= '0';
                s_IDtoData       <= '0';
                IACKinProgress_o <= '0';
                if VME_DS_n_oversampled/="11" then
                    s_IRQstate   <= CHECK_MATCH; 
                else
                    s_IRQstate   <= WAIT_FOR_DS;
                end if;
                
                when CHECK_MATCH =>    
                s_VME_IACKOUT    <= '1';
                s_irqDTACK       <= '1';
                s_applyIRQmask   <= '0';
                s_IDtoData       <= '0';
                IACKinProgress_o <= '0';
                if s_IACKmatch='1' then
                    s_IRQstate   <= APPLY_MASK_AND_DATA;
                else
                    s_IRQstate   <= PROPAGATE_IACK;
                end if;
                
                when APPLY_MASK_AND_DATA =>
                s_VME_IACKOUT    <= '1';
                s_irqDTACK       <= '1';
                s_applyIRQmask   <= '1';
                s_IDtoData       <= '1';
                IACKinProgress_o <= '1';
                s_IRQstate       <= APPLY_DTACK;
                
                when APPLY_DTACK =>
                s_VME_IACKOUT    <= '1';
                s_irqDTACK       <= '0';
                s_applyIRQmask   <= '0';
                s_IDtoData       <= '1';
                IACKinProgress_o <= '1';
                if VME_IACKIN_n_oversampled='1' then
                    s_IRQstate   <= IDLE;
                else
                    s_IRQstate   <= APPLY_DTACK;
                end if;
                
                when PROPAGATE_IACK =>
                s_VME_IACKOUT    <= VME_IACKIN_n_oversampled;
                s_irqDTACK       <= '1';
                s_applyIRQmask   <= '0';
                s_IDtoData       <= '0';
                IACKinProgress_o <= '0';
                if VME_IACKIN_n_oversampled='1' then
                    s_IRQstate   <= IDLE;
                else
                    s_IRQstate   <= PROPAGATE_IACK;
                end if;
                
                when OTHERS =>
                s_VME_IACKOUT    <= '1';
                s_irqDTACK       <= '1';
                s_applyIRQmask   <= '0';
                s_IDtoData       <= '0';
                IACKinProgress_o <= '0';
                s_IRQstate       <= IDLE;
                
            end case;
        end if;
    end if;
end process; 

                    
s_IACKmatch <= '1' when "00000"&locAddr_i = IRQlevelReg_i else '0';
    
s_IRQenabled <= '1' when IRQlevelReg_i < 8 and IRQlevelReg_i /= 0 else '0';
    
IDtoData_o <= s_IDtoData; 


-- Setting and clearing pending interrupt request register

p_IRQregHandling: process(clk_i)
begin
    if rising_edge(clk_i) then
        if s_reset='1' then
            s_IRQreg <= '0';
        elsif s_applyIRQmask='1' then
            s_IRQreg <= '0';
        else
            s_IRQreg <= s_wbIRQrisingEdge and s_IRQenabled;                                                 
        end if;
    end if;
end process; 


-- Driving VME_IRQ lines

gen_IRQoutput: for i in 0 to 6 generate        
    VME_IRQ_n_o(i) <= '0' when s_IRQreg='1' and IRQlevelReg_i=(i+1) else 'Z';
end generate;  


-- Signal input oversample & rising edge detection

IRQrisingEdge: RisEdgeDetection
port map (
        sig_i => IRQ_i, 
        clk_i => clk_i,
        RisEdge_o => s_wbIRQrisingEdge
        );

IACKINinputSample: SigInputSample
    port map(
        sig_i => VME_IACKIN_n_i,
        sig_o => VME_IACKIN_n_oversampled,
        clk_i => clk_i
        ); 
        
DSinputSample: RegInputSample 
    generic map(
        width => 2
        )
    port map(
        reg_i => VME_DS_n_i,
        reg_o => VME_DS_n_oversampled,
        clk_i => clk_i 
        );
        

end RTL;
