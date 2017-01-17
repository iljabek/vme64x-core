--------------------------------------------------------------------------------
-- CERN (BE-CO-HT)
-- VME64x Core
-- http://www.ohwr.org/projects/vme64x-core
--------------------------------------------------------------------------------
--
-- unit name:     VME_User_CSR (VME_User_CSR.vhd)
--
-- author:        Pablo Alvarez Sanchez <pablo.alvarez.sanchez@cern.ch>
--                Davide Pedretti       <davide.pedretti@cern.ch>
--
-- description:
--
--   This module implements the user CSR registers that were added to the
--   reserved area of the defined CSR in previous versions of this core.
--
--   To achieve the previous memory map layout, it is necessary to set the
--   following generics on the VME64xCore_Top:
--
--     g_BEG_USER_CSR => x"07ff33",
--     g_END_USER_CSR => x"07ff5f",
--
--   However, for new designs it would be better to choose somewhere outside
--   the reserved area (from x"7fc00" to x"7ff5f"). For example:
--
--     g_BEG_USER_CSR => x"07fbd3",
--     g_END_USER_CSR => x"07fbff",
--
--   The following registers are implemented:
--                            _
--     IRQ_Vector --> 0x0002F  |--> For the VME_IRQ_Controller
--     IRQ_level  --> 0x0002B _|
--
--     Endian     --> 0x00023 ----> For the VME_swapper
--                            _
--     TIME0_ns   --> 0x0001F  |
--     TIME1_ns   --> 0x0001B  |
--     TIME2_ns   --> 0x00017  |
--     TIME3_ns   --> 0x00013  |--> To calculate the transfer rate
--     TIME4_ns   --> 0x0000F  |    (not currently implemented)
--     BYTES0     --> 0x0000B  |
--     BYTES1     --> 0x00007 _|
--
--     WB32bits   --> 0x00003 ----> If bit 0 is '1' the WB data bus is 32b
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

entity VME_User_CSR is
  generic (
    g_WB_DATA_WIDTH     : integer
  );
  port (
    clk_i               : in  std_logic;
    rst_n_i             : in  std_logic;

    addr_i              : in  std_logic_vector(18 downto 2);
    data_i              : in  std_logic_vector( 7 downto 0);
    data_o              : out std_logic_vector( 7 downto 0);
    we_i                : in  std_logic;

    irq_vector_o        : out std_logic_vector( 7 downto 0);
    irq_level_o         : out std_logic_vector( 7 downto 0);
    endian_o            : out std_logic_vector( 2 downto 0);
    bytes_i             : in  std_logic_vector(15 downto 0);
    time_i              : in  std_logic_vector(39 downto 0)
  );
end VME_User_CSR;

architecture rtl of VME_User_CSR is

  signal s_addr                 : unsigned(18 downto 2);

  signal s_reg_irq_vector       : std_logic_vector(7 downto 0);
  signal s_reg_irq_level        : std_logic_vector(7 downto 0);
  signal s_reg_endian           : std_logic_vector(7 downto 0);
  signal s_reg_wb32bits         : std_logic_vector(7 downto 0);

begin

  s_addr <= unsigned(addr_i);

  s_reg_wb32bits <= x"01" when g_WB_DATA_WIDTH = 32 else x"00";

  -- Write
  process (clk_i)
  begin
    if rising_edge(clk_i) then
      if rst_n_i = '0' then
        s_reg_irq_vector  <= x"00";
        s_reg_irq_level   <= x"00";
        s_reg_endian      <= x"00";
      else
        if we_i = '1' then
          case s_addr is
            when c_ADDR_IRQ_VECTOR(18 downto 2) => s_reg_irq_vector <= data_i;
            when c_ADDR_IRQ_LEVEL(18 downto 2)  => s_reg_irq_level  <= data_i;
            when c_ADDR_ENDIAN(18 downto 2)     => s_reg_endian     <= data_i;
            when others                         => null;
          end case;
        end if;
      end if;
    end if;
  end process;

  irq_vector_o  <= s_reg_irq_vector;
  irq_level_o   <= s_reg_irq_level;
  endian_o      <= s_reg_endian(2 downto 0);

  -- Read
  process (clk_i)
  begin
    if rising_edge(clk_i) then
      if rst_n_i = '0' then
        data_o <= x"00";
      else
        case s_addr is
          when c_ADDR_IRQ_VECTOR(18 downto 2) => data_o <= s_reg_irq_vector;
          when c_ADDR_IRQ_LEVEL(18 downto 2)  => data_o <= s_reg_irq_level;
          when c_ADDR_ENDIAN(18 downto 2)     => data_o <= s_reg_endian;
          when c_ADDR_TIME0_NS(18 downto 2)   => data_o <= time_i( 7 downto  0);
          when c_ADDR_TIME1_NS(18 downto 2)   => data_o <= time_i(15 downto  8);
          when c_ADDR_TIME2_NS(18 downto 2)   => data_o <= time_i(23 downto 16);
          when c_ADDR_TIME3_NS(18 downto 2)   => data_o <= time_i(31 downto 24);
          when c_ADDR_TIME4_NS(18 downto 2)   => data_o <= time_i(39 downto 32);
          when c_ADDR_BYTES0(18 downto 2)     => data_o <= bytes_i( 7 downto 0);
          when c_ADDR_BYTES1(18 downto 2)     => data_o <= bytes_i(15 downto 8);
          when c_ADDR_WB32BITS(18 downto 2)   => data_o <= s_reg_wb32bits;
          when others                         => data_o <= x"ff";
        end case;
      end if;
    end if;
  end process;

end rtl;
