--________________________________________________________________________________________________
--                             VME TO WB INTERFACE
--
--                                CERN,BE/CO-HT 
--________________________________________________________________________________________________
-- File:                         VME_CSR_pack.vhd
--________________________________________________________________________________________________
-- Description: This file defines the default configuration of the CSR space after power-up or software reset.
--______________________________________________________________________________
-- Authors:      Erik Van der Bij      (Erik.Van.der.Bij@cern.ch)                                  
--               Pablo Alvarez Sanchez (Pablo.Alvarez.Sanchez@cern.ch)                             
--               Davide Pedretti       (Davide.Pedretti@cern.ch)  
-- Date         06/2012                                                                           
-- Version      v0.01  
--______________________________________________________________________________
--                               GNU LESSER GENERAL PUBLIC LICENSE                                
--                              ------------------------------------                              
-- This source file is free software; you can redistribute it and/or modify it under the terms of 
-- the GNU Lesser General Public License as published by the Free Software Foundation; either     
-- version 2.1 of the License, or (at your option) any later version.                             
-- This source is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;       
-- without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.     
-- See the GNU Lesser General Public License for more details.                                    
-- You should have received a copy of the GNU Lesser General Public License along with this       
-- source; if not, download it from http://www.gnu.org/licenses/lgpl-2.1.html                     
---------------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.numeric_std.all;

use work.vme64x_pack.all;
package VME_CSR_pack is

constant c_csr_array : 	t_CSRarray :=
(
BAR  => x"00", --CR/CSR BAR
BIT_SET_CLR_REG  => x"10", --Bit set register -- 0x10=module enable
USR_BIT_SET_CLR_REG  => x"00", --Bit clear register
CRAM_OWNER  => x"00", --CRAM_OWNER

FUNC0_ADER_0 =>x"00",  --A32_S   "24"
FUNC0_ADER_1 =>x"00",  --        "00"
FUNC0_ADER_2 =>x"00",  --        "00"
FUNC0_ADER_3 =>x"00",  --        "c0"

FUNC1_ADER_0 =>x"00",  --A24_S   "e4"
FUNC1_ADER_1 =>x"00",  --        "00"
FUNC1_ADER_2 =>x"00",  --        "c0" 
FUNC1_ADER_3 =>x"00",  --        "00"  

FUNC2_ADER_0 =>x"00",   --A16_S  "a4"
FUNC2_ADER_1 =>x"00",  --        "c0"
FUNC2_ADER_2 =>x"00",  --        "00"
FUNC2_ADER_3 =>x"00",  --        "00"

FUNC3_ADER_0 =>x"00",   --A64_S  "04"
FUNC3_ADER_1 =>x"00",
FUNC3_ADER_2 =>x"00",
FUNC3_ADER_3 =>x"00",

FUNC4_ADER_0 =>x"00",   --used for decoding the FUNC3
FUNC4_ADER_1 =>x"00",   --used for decoding the FUNC3
FUNC4_ADER_2 =>x"00",   --used for decoding the FUNC3
FUNC4_ADER_3 =>x"00",   --used for decoding the FUNC3 "c0"

FUNC5_ADER_0 =>x"00",
FUNC5_ADER_1 =>x"00",
FUNC5_ADER_2 =>x"00",
FUNC5_ADER_3 =>x"00",

FUNC6_ADER_0 =>x"00",
FUNC6_ADER_1 =>x"00",
FUNC6_ADER_2 =>x"00",
FUNC6_ADER_3 =>x"00",

IRQ_Vector   =>x"00",  --"00" because each Slot has a different IRQ Vector
                       -- and the VME Master should set this value
IRQ_level    =>x"02",
others => (others => '0'));
end VME_CSR_pack;                                                                







 












