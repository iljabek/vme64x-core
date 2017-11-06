--------------------------------------------------------------------------------
-- CERN (BE-CO-HT)
-- VME64x Core
-- http://www.ohwr.org/projects/vme64x-core
--------------------------------------------------------------------------------
--
-- unit name:     VME_IRQ_Controller (VME_IRQ_Controller.vhd)
--
-- author:        Pablo Alvarez Sanchez <pablo.alvarez.sanchez@cern.ch>
--                Davide Pedretti       <davide.pedretti@cern.ch>
--
-- description:
--
--   This block acts as Interrupter. Phases of an interrupt cycle:
--
--   1) The Interrupt Controller receives an interrupt request by the WB bus;
--      this request is a pulse on the INT_Req input
--   2) The Interrupt Controller asserts ('0') one of the 7 VME_IRQ lines;
--      --> request of a service.
--      The Interrupt priority is specificated by the Master writing the
--      INT_Level register in the CR/CSR space
--   3) The Interrupter Controller wait for the falling edge on the VME_IACKIN
--      line.
--   4) When detects VME_IACKIN_n_i = '0' and the Interrupt Handler initiates
--      the Interrupt cycle by asserting AS,the Interrupt Controller check if it
--      is the responding interrupter. Indeed before responding to an interrupt
--      acknowledge cycle the interrupter shall have an interrupt request
--      pending, shall check if the level of that request match the level
--      indicated on the address lines A1, A2 and A3,the data transfer width
--      during the interrupt acknowledge cycle should be equal or greater than
--      the size the it can respond with, and it shall receive a falling edge on
--      its IACKIN*.
--   5) If it is the responding interrupter should send the source/ID on the
--      VME_DATA lines (in our case the source/ID is the INT_Vector that the
--      Master can write in the corresponding register in the CR/CSR space) and
--      it terminates the interrupt cycle with an acknowledge before releasing
--      the IRQ lines. If it isn't the responding interrupter, it should pass a
--      falling edge on down the daisy-chain so other interrupters can respond.
--
--   All the output signals are registered
--
--   To implement the 5 phases before mentioned the follow FSM has been
--   implemented:
--       __________
--   |--| IACKOUT2 |<-|
--   |  |__________|  |
--   |                |
--   |    _________   |  _________     _________     _________
--   |-->|  IDLE   |--->|  IRQ    |-->| WAIT_AS |-->| WAIT_DS |-------------->|
--       |_________|    |_________|   |_________|   |_________|               |
--          |             |                                                   |
--          |             |                       _________      _________    |
--          |             |---------<------------| IACKOUT1| <--| CHECK   |<--|
--          |                                    |_________|    |_________|
--          |                     __________     __________         |
--          |--<-----------------|  DTACK   |<--| DATA_OUT |---<----|
--                               |__________|   |__________|
--
--  The interrupter wait the IACKIN falling edge in the IRQ state, so if the
--  interrupter don't have interrupt pending for sure it will not respond
--  because it is in IDLE.
--  If the slave module does not have an interrupt pending (IDLE state) and it
--  receives a falling edge on the IACKIN, it shall pass the falling edge
--  through the daisy chain.
--  To obtain this the IACKOUT2 state has been added.
--
--  Time constraint:
--
--  Time constraint #35:
--
-- Clk         ____      ____      ____      ____      ____      ____
--       _____|    |____|    |____|    |____|    |____|    |____|    |_____
--
-- VME_AS1_n_i   __________________________________________________________
--       _______|
--
-- VME_AS_n_i                                ______________________________
--       ___________________________________|
--
-- s_AS_RisingEge                                      _________
--       _____________________________________________|         |__________
--
-- s_IACKOUT     __________________________________________________________
--       _______|
--
-- VME_IACKOUT_  __________________________________________________________
--       _______|
--
--       _______________________________________________________  _________
--                                       IACKOUT 1/2            \/ IDLE/IRQ
--       _______________________________________________________/\_________
--
--  To respect the time constraint indicated with the number 35 fig. 55 pag. 183
--  in the "VMEbus Specification" ANSI/IEEE STD1014-1987, is necessary to
--  generate the VME_AS1_n_i signal which is the AS signal not sampled, and
--  assign this signal to the s_IACKOUT signal when the fsm is in the IACKOUTx
--  state.
--
--  The LWORD* input is not used now, since this is a D08(O) Interrupter (see
--  Table 31 page 157 VMEbus specification).
--
--  Since this is a D08 interrupter we do not need to monitor the LWORD* and
--  DS1* lines and the Vector (1 byte) is outputted in the D00-D07 data lines.
--
-- dependencies:
--
--------------------------------------------------------------------------------
-- GNU LESSER GENERAL PUBLIC LICENSE
--------------------------------------------------------------------------------
-- This source file is free software; you can redistribute it and/or modify it
-- under the terms of the GNU Lesser General Public License as published by the
-- Free Software Foundation; either version 2.1 of the License, or (at your
-- option) any later version. This source is distributed in the hope that it
-- will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
-- of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-- See the GNU Lesser General Public License for more details. You should have
-- received a copy of the GNU Lesser General Public License along with this
-- source; if not, download it from http://www.gnu.org/licenses/lgpl-2.1.html
--------------------------------------------------------------------------------
-- last changes: see log.
--------------------------------------------------------------------------------
-- TODO: -
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.vme64x_pack.all;

entity VME_IRQ_Controller is
  generic (
    g_RETRY_TIMEOUT : integer range 1024 to 16777215
  );
  port (
    clk_i           : in  std_logic;
    reset_n_i       : in  std_logic;
    INT_Level_i     : in  std_logic_vector (2 downto 0);
    INT_Req_i       : in  std_logic;
    irq_pending_o   : out std_logic;
    irq_ack_i       : in  std_logic;
    VME_IRQ_n_o     : out std_logic_vector (7 downto 1)
  );
end VME_IRQ_Controller;

architecture Behavioral of VME_IRQ_Controller is

  type t_retry_state is (WAIT_IRQ, WAIT_RETRY);

  signal retry_state      : t_retry_state;
  signal retry_count      : unsigned(23 downto 0);
  signal retry_mask       : std_logic;
  signal s_irq_pending    : std_logic;
begin
  irq_pending_o <= s_irq_pending;

  -- Interrupts are automatically masked for g_RETRY_TIMEOUT (ie 1 ms) once
  -- they are acknowledge by the interrupt handler until they are deasserted
  -- by the interrupter.
  p_retry_fsm : process (clk_i)
  begin
    if rising_edge(clk_i) then
      if reset_n_i = '0' then
        retry_mask  <= '1';
        retry_state <= WAIT_IRQ;
      else
        case retry_state is
          when WAIT_IRQ =>
            if s_irq_pending = '1' and INT_Req_i = '1' then
              retry_state <= WAIT_RETRY;
              retry_count <= (others => '0');
              retry_mask  <= '0';
            else
              retry_mask  <= '1';
            end if;

          when WAIT_RETRY =>
            if INT_Req_i = '0' then
              retry_state <= WAIT_IRQ;
            else
              retry_count <= retry_count + 1;
              if retry_count = g_RETRY_TIMEOUT then
                retry_state <= WAIT_IRQ;
              end if;
            end if;
        end case;
      end if;
    end if;
  end process;

  p_main_fsm : process (clk_i)
  begin
    if rising_edge(clk_i) then
      if reset_n_i = '0' then
        VME_IRQ_n_o     <= (others => '1');
        s_irq_pending   <= '0';
      else
        if s_irq_pending = '0' then
          VME_IRQ_n_o     <= (others => '1');

          if INT_Req_i = '1' and retry_mask = '1' then
            s_irq_pending <= '1';

            -- Explicit decoding
            case INT_Level_i is
              when "001" =>  VME_IRQ_n_o <= "1111110";
              when "010" =>  VME_IRQ_n_o <= "1111101";
              when "011" =>  VME_IRQ_n_o <= "1111011";
              when "100" =>  VME_IRQ_n_o <= "1110111";
              when "101" =>  VME_IRQ_n_o <= "1101111";
              when "110" =>  VME_IRQ_n_o <= "1011111";
              when "111" =>  VME_IRQ_n_o <= "0111111";
              when others =>
                --  Incorrect value for INT_Level_i, ignore it.
                VME_IRQ_n_o <= "1111111";
                s_irq_pending <= '0';
            end case;
          end if;
        else
          if irq_ack_i = '1' then
            s_irq_pending  <= '0';
          end if;
        end if;
      end if;
    end if;
  end process;
end Behavioral;
