library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.numeric_std.all;

use work.VME_pack.all;
package VME_CSR_pack is
     constant c_csr_array : 	t_cr_array(2**8-1 downto 0) :=
(
16#0FF#  => x"00", --CR/CSR BAR
16#0FD#  => x"00", --Bit set register 
16#0FE#  => x"00", --Bit clear register
16#0FC#  => x"00", --CRAM_OWNER
16#0FB#  => x"00", --User defined bit set register
16#0FA#  => x"01", --User-defined bit clear register
16#0F9#  => x"01", --reserved

--16#0F8# downto  16#0F5# => x"00000000", --Fun7 ADDER
--16#0F4# downto  16#0F1# => x"00000000", --Fun6 ADDER
--16#0F0# downto  16#0ED# => x"00000000", --Fun5 ADDER
--16#0EC# downto  16#0E9# => x"00000000", --Fun4 ADDER
--16#0E8# downto  16#0E5# => x"00000000", --Fun3 ADDER
--16#0E4# downto  16#0E1# => x"00000000", --Fun2 ADDER
--16#0D0# downto  16#0CD# => x"00000000", --Fun1 ADDER
--16#0CC# downto  16#0C9# => x"00000000", --Fun0 ADDER
others => (others => '0'));

end VME_CSR_pack;                                                                







 












