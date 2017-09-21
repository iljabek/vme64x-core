--------------------------------------------------------------------------------
-- CERN (BE-CO-HT)
-- VME64x Core
-- http://www.ohwr.org/projects/vme64x-core
--------------------------------------------------------------------------------
--
-- unit name:     VME_Wb_master (VME_Wb_master.vhd)
--
-- author:        Pablo Alvarez Sanchez <pablo.alvarez.sanchez@cern.ch>
--                Davide Pedretti       <davide.pedretti@cern.ch>
--
-- description:
--
--   This component implements the WB master side in the vme64x core.
--
--   Work mode:
--     PIPELINED
--      SINGLE READ/WRITE
--
--   The WB bus can be 64 bit wide or 32 bit wide and the data organization is
--   BIG ENDIAN --> the most significant byte is carried in the lower position
--   of the bus. Eg:
--    _______________________________________________________________________
--   | Byte(0)| Byte(1)| Byte(2)| Byte(3)| Byte(4)| Byte(5)| Byte(6)| Byte(7)|
--   |________|________|________|________|________|________|________|________|
--    D[63:56] D[55:48] D[47:40] D[39:32] D[31:24] D[23:16] D[15:8]  D[7:0]
--
--   eg of timing diagram with synchronous WB Slave:
--
--   Clk    _____       _____       _____       _____       _____       _____
--    _____|     |_____|     |_____|     |_____|     |_____|     |_____|     |
--
--   cyc_o  ____________________________________________________________
--    _____|                                                            |_____
--
--   stb_o  ________________________________________________
--    _____|                                                |_________________
--
--    __________________________________________
--   stall_i                                    |_____________________________
--
--   ack_i                                                   ___________
--    ______________________________________________________|           |_____
--
--   The ack_i can be asserted with some Tclk of delay, not immediately.
--   This component implements the correct shift of the data in input/output
--   from/to WB bus
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

entity VME_Wb_master is
  generic (
    g_WB_DATA_WIDTH : integer;
    g_WB_ADDR_WIDTH : integer
  );
  port (
    memReq_i        : in  std_logic;
    clk_i           : in  std_logic;
    reset_i         : in  std_logic;
    BERRcondition_i : in  std_logic;
    sel_i           : in  std_logic_vector(3 downto 0);
    locDataInSwap_i : in  std_logic_vector(31 downto 0);
    locDataOut_o    : out std_logic_vector(31 downto 0);
    rel_locAddr_i   : in  std_logic_vector(31 downto 0);
    memAckWb_o      : out std_logic;
    err_o           : out std_logic;
    rty_o           : out std_logic;
    RW_i            : in  std_logic;
    stall_i         : in  std_logic;
    rty_i           : in  std_logic;
    err_i           : in  std_logic;
    cyc_o           : out std_logic;
    stb_o           : out std_logic;
    WBdata_o        : out std_logic_vector(g_WB_DATA_WIDTH-1 downto 0);
    wbData_i        : in  std_logic_vector(g_WB_DATA_WIDTH-1 downto 0);
    locAddr_o       : out std_logic_vector(g_WB_ADDR_WIDTH-1 downto 0);
    memAckWB_i      : in  std_logic;
    WbSel_o         : out std_logic_vector(g_WB_DATA_WIDTH/8-1 downto 0);
    RW_o            : out std_logic
  );
end VME_Wb_master;

architecture Behavioral of VME_Wb_master is
  signal s_AckWithError : std_logic;
begin
  -- stb handler
  process (clk_i)
  begin
    if rising_edge(clk_i) then
      if reset_i = '1' then
        stb_o <= '0';
        cyc_o <= '0';
      else
        if memReq_i = '1' and BERRcondition_i = '0' then
          stb_o <= '1';
          cyc_o <= '1';
        else
          --  One pulse for stb_o
          stb_o <= '0';

          --  But s_cyc is set for the whole cycle
          if memAckWB_i = '1' then
            cyc_o <= '0';
          end if;
        end if;
      end if;
    end if;
  end process;
  
  process (clk_i)
  begin
    if rising_edge(clk_i) then
      RW_o        <= RW_i;
      s_AckWithError <= (memReq_i and BERRcondition_i);
    end if;
  end process;

  -- shift data and address for WB data bus 32 bits
  assert g_WB_DATA_WIDTH = 32 severity failure;
  
  process (clk_i)
  begin
    if rising_edge(clk_i) then
      locAddr_o (31 downto 30) <= (others => '0');
      locAddr_o (29 downto 0) <= rel_locAddr_i (31 downto 2);
    end if;
  end process;

  process (clk_i)
  begin
    if rising_edge(clk_i) then
      WBdata_o <= locDataInSwap_i;
      WbSel_o <= sel_i;
    end if;
  end process;

  err_o <= err_i;
  rty_o <= rty_i;

  -- This process registers the WB data input; this is a warranty that this
  -- data will be stable during all the time the VME_bus component needs to
  -- transfers its to the VME bus.
  process (clk_i)
  begin
    if rising_edge(clk_i) then
      if memAckWB_i = '1' then
        locDataOut_o <= wbData_i;
      end if;
      memAckWb_o <= memAckWB_i or s_AckWithError or rty_i;
    end if;
  end process;
end Behavioral;
