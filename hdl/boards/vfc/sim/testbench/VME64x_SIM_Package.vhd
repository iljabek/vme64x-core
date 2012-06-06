
--------------------------------------------------------------------------------------
---------------------------VME64x_SIM_Package-----------------------------------------
--------------------------------------------------------------------------------------

-- Date        : Fri Mar 03 2012
--
-- Author      : Davide Pedretti
--
-- Company     : CERN
--
-- Description : VME64x procedures for test the VME64x Slave

library IEEE;
library std;
use IEEE.STD_LOGIC_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all;
use IEEE.numeric_std.unsigned;
use work.all;
use work.VME_pack.all;
use work.VME64x.all;
use work.VME_CR_pack.all;
use std.textio.all;
package VME64xSim is
   
   -- All the constants and records are in the VME64x_Package 
   -- Procedures:
   
   -- function <function_name>  (signal <signal_name> : in <type_declaration>) return <type_declaration>;
   procedure WriteCSR	(c_address	: in std_logic_vector(19 downto 0); signal s_dataToSend : in std_logic_vector(31 downto 0);
   signal s_dataTransferType : in t_dataTransferType; signal s_AddressingType : in t_AddressingType; 
   signal VME64xBus_In : in VME64xBusIn_Record; signal VME64xBus_Out : out VME64xBusOut_Record);

   procedure S_Write	(v_address	: in std_logic_vector(63 downto 0); signal s_dataToSend : in std_logic_vector(31 downto 0);  -- this procedure is for A16, A24, A32 address type
   signal s_dataTransferType : in t_dataTransferType; signal s_AddressingType : in t_AddressingType; 
   signal VME64xBus_In : in VME64xBusIn_Record; signal VME64xBus_Out : out VME64xBusOut_Record);

   procedure S_Read (v_address	: in std_logic_vector(63 downto 0); signal s_dataToReceive : in std_logic_vector(31 downto 0);  -- this procedure is for A16, A24, A32 address type
   signal s_dataTransferType : in t_dataTransferType; signal s_AddressingType : in t_AddressingType; 
   signal VME64xBus_In : in VME64xBusIn_Record; signal VME64xBus_Out : out VME64xBusOut_Record);

   procedure Blt_Read(v_address	: in std_logic_vector(63 downto 0); signal s_Buffer_BLT : in t_Buffer_BLT;  -- this procedure is for A16, A24, A32 address type
   signal s_dataTransferType : in t_dataTransferType; signal s_AddressingType : in t_AddressingType; num : in std_logic_vector(8 downto 0); 
   signal VME64xBus_In : in VME64xBusIn_Record; signal VME64xBus_Out : out VME64xBusOut_Record);

   procedure Blt_write(v_address	: in std_logic_vector(63 downto 0); signal s_Buffer_BLT : in t_Buffer_BLT;  -- this procedure is for A16, A24, A32 address type
   signal s_dataTransferType : in t_dataTransferType; signal s_AddressingType : in t_AddressingType; num : in std_logic_vector(8 downto 0); 
   signal VME64xBus_In : in VME64xBusIn_Record; signal VME64xBus_Out : out VME64xBusOut_Record);

   procedure Mblt_write(v_address	: in std_logic_vector(63 downto 0); signal s_Buffer_MBLT : in t_Buffer_MBLT;  -- this procedure is for A16, A24, A32 address type
   signal s_dataTransferType : in t_dataTransferType; signal s_AddressingType : in t_AddressingType; num : in std_logic_vector(8 downto 0); 
   signal VME64xBus_In : in VME64xBusIn_Record; signal VME64xBus_Out : out VME64xBusOut_Record);

   procedure Mblt_Read(v_address	: in std_logic_vector(63 downto 0); signal s_Buffer_MBLT : in t_Buffer_MBLT;  -- this procedure is for A16, A24, A32 address type
   signal s_dataTransferType : in t_dataTransferType; signal s_AddressingType : in t_AddressingType; num : in std_logic_vector(8 downto 0); 
   signal VME64xBus_In : in VME64xBusIn_Record; signal VME64xBus_Out : out VME64xBusOut_Record);





   procedure ReadCR_CSR	(c_address	: in std_logic_vector(19 downto 0); signal s_dataToReceive : in std_logic_vector(31 downto 0);
   signal s_dataTransferType : in t_dataTransferType; signal s_AddressingType : in t_AddressingType; 
   signal VME64xBus_In : in VME64xBusIn_Record; signal VME64xBus_Out : out VME64xBusOut_Record);

   procedure SetAmAddress	(signal s_dataTransferType : in t_dataTransferType;
   signal s_AddressingType : in t_AddressingType; Vme64xAM : out std_logic_vector(5 downto 0);
   DataType : out std_logic_vector(3 downto 0));

   procedure ShiftData	(write_n : in std_logic; signal s_dataTransferType : in t_dataTransferType; signal s_dataToShift : in std_logic_vector(31 downto 0); v_dataToShiftOut : out std_logic_vector(31 downto 0));							 
   procedure SetCrCsrAddress	(c_address : in std_logic_vector(19 downto 0); v_address : out std_logic_vector(31 downto 0));
   procedure SetAddress	(c_address : in std_logic_vector(63 downto 0); signal s_AddressingType : in t_AddressingType; v_address : out std_logic_vector(31 downto 0));
   procedure ControlCR (signal s_dataTransferType : in t_dataTransferType; signal s_AddressingType : in t_AddressingType; signal VME64xBus_In : in VME64xBusIn_Record; 
   signal s_dataToReceive : inout std_logic_vector(31 downto 0); signal VME64xBus_Out : out VME64xBusOut_Record);
end VME64xSim;


package body VME64xSim is
   
   procedure WriteCSR  (c_address : in std_logic_vector(19 downto 0); signal s_dataToSend	: in std_logic_vector(31 downto 0);
   signal s_dataTransferType : in t_dataTransferType; signal s_AddressingType : in t_AddressingType;
   signal VME64xBus_In : in VME64xBusIn_Record; signal VME64xBus_Out : out VME64xBusOut_Record) is

   variable  Vme64xAM : std_logic_vector(5 downto 0);
   variable  DataType : std_logic_vector(3 downto 0) := (others => '1');
   variable  v_address : std_logic_vector(31 downto 0) := (others => '0');
   variable  v_dataToSendOut : std_logic_vector(31 downto 0) := (others => '0');
   variable ti : time;
begin

   ti := now;
   if VME64xBus_In.Vme64xDtackN /='1' or VME64xBus_In.Vme64xBerrN /='0' then 
      wait until VME64xBus_In.Vme64xDtackN ='1' and VME64xBus_In.Vme64xBerrN = '0';
   end if;
   VME64xBus_Out.Vme64xAsN <='1';   --initialisation
   VME64xBus_Out.Vme64xWRITEN <='1';
   VME64xBus_Out.Vme64xDs0N <='1';
   VME64xBus_Out.Vme64xDs1N <='1';
   VME64xBus_Out.Vme64xLWORDN <='1';
   VME64xBus_Out.Vme64xADDR <= (others => '1');
   wait for 10 ns;
   report "Start Address Phase";
   -- Address phase
   SetAmAddress(s_dataTransferType => s_dataTransferType, s_AddressingType => s_AddressingType,
   Vme64xAM => Vme64xAM, DataType => DataType);

   -- controllo
   if s_AddressingType = CR_CSR then
      assert ((DataType = "0101") or (DataType = "1001") or (DataType = "0111") or (DataType = "1011"))report "Error, DataType must be D08!!!" severity failure;		
   end if;

   SetCrCsrAddress(c_address => c_address, v_address => v_address);	

   VME64xBus_Out.Vme64xADDR(31 downto 2) <= v_address(31 downto 2);
   VME64xBus_Out.Vme64xAM <= Vme64xAM;
   VME64xBus_Out.Vme64xLWORDN <= DataType(0);
   VME64xBus_Out.Vme64xADDR(1) <= DataType(1);
   VME64xBus_Out.Vme64xWRITEN <='0';
   report "End Address Phase";
   wait for 35 ns;

   VME64xBus_Out.Vme64xAsN <='0';
   VME64xBus_Out.Vme64xDs1N <=DataType(3);				 
   VME64xBus_Out.Vme64xDs0N <=DataType(2);
   ShiftData(write_n => '0', s_dataTransferType => s_dataTransferType, s_dataToShift => s_dataToSend, v_dataToShiftOut => v_dataToSendOut);   --procedura che prende il dato dai lsb e lo mette nei byte giusti
   VME64xBus_Out.Vme64xDATA <= v_dataToSendOut;
   report "Wait for DTACK";
   wait until (VME64xBus_In.Vme64xDtackN = '0' or (now > (ti + 4 us)) or VME64xBus_In.Vme64xBerrN = '1');
   assert (VME64xBus_In.Vme64xBerrN /= '1') report "THE SLAVE ASSERTED THE Berr LINE" severity error;
   VME64xBus_Out.Vme64xDs0N <= '1';
   VME64xBus_Out.Vme64xDs1N <= '1';
   VME64xBus_Out.Vme64xLWORDN <= '1';
   wait for 10 ns;   -- not necessary

   VME64xBus_Out.Vme64xADDR <= (others => '1');
   VME64xBus_Out.Vme64xAM <= (others => '1');
   VME64xBus_Out.Vme64xDATA <= (others => 'Z');
   VME64xBus_Out.Vme64xAsN <= '1';
   VME64xBus_Out.Vme64xWRITEN <='1';

   
end ;

procedure ReadCR_CSR	(c_address	: in std_logic_vector(19 downto 0); signal s_dataToReceive : in std_logic_vector(31 downto 0);
signal s_dataTransferType : in t_dataTransferType; signal s_AddressingType : in t_AddressingType; 
signal VME64xBus_In : in VME64xBusIn_Record; signal VME64xBus_Out : out VME64xBusOut_Record) is

   variable  Vme64xAM : std_logic_vector(5 downto 0);
   variable  DataType : std_logic_vector(3 downto 0) := (others => '1');
   variable  v_address : std_logic_vector(31 downto 0) := (others => '0');
   variable  v_dataToReceiveOut : std_logic_vector(31 downto 0) := (others => '0');
   variable ti : time;

begin
   ti := now;
   if VME64xBus_In.Vme64xDtackN /='1' or VME64xBus_In.Vme64xBerrN /= '0' then 
      wait until VME64xBus_In.Vme64xDtackN ='1' and VME64xBus_In.Vme64xBerrN = '0';
   end if;
   --initialisation
   VME64xBus_Out.Vme64xAsN <='1';   
   VME64xBus_Out.Vme64xWRITEN <='1';
   VME64xBus_Out.Vme64xDs0N <='1';
   VME64xBus_Out.Vme64xDs1N <='1';
   VME64xBus_Out.Vme64xLWORDN <='1';
   VME64xBus_Out.Vme64xADDR <= (others => '1');
   wait for 10 ns;
   report "Start Address Phase";
   -- Address phase
   SetAmAddress(s_dataTransferType => s_dataTransferType, s_AddressingType => s_AddressingType,
   Vme64xAM => Vme64xAM, DataType => DataType);

   -- controllo
   --if s_AddressingType = CR_CSR then
    --  assert ((DataType = "0101") or (DataType = "1001") or (DataType = "0111") or (DataType = "1011"))report "Error, DataType must be D08!!!" severity failure;		
  -- end if;

   SetCrCsrAddress(c_address => c_address, v_address => v_address);	

   VME64xBus_Out.Vme64xADDR(31 downto 2) <= v_address(31 downto 2);
   VME64xBus_Out.Vme64xAM <= Vme64xAM;
   VME64xBus_Out.Vme64xLWORDN <= DataType(0);
   VME64xBus_Out.Vme64xADDR(1) <= DataType(1);
   VME64xBus_Out.Vme64xWRITEN <='1';
   report "End Address Phase";
   wait for 35 ns;    -- check the min time here...for master is 35 ns and for slave 10 ns.
   report "Master drive the AS low";
   VME64xBus_Out.Vme64xAsN <='0';
   VME64xBus_Out.Vme64xDs1N <=DataType(3);				 
   VME64xBus_Out.Vme64xDs0N <=DataType(2);

   report "Wait for DTACK";
   wait until (VME64xBus_In.Vme64xDtackN = '0' or (now > (ti + 4 us)) or VME64xBus_In.Vme64xBerrN = '1');
   wait for 10 ns;
   if(VME64xBus_In.Vme64xBerrN = '1') then 
      assert(VME64xBus_In.Vme64xBerrN /= '1') report "THE SLAVE ASSERTED THE Berr LINE" severity error;
   else	  
      ShiftData(write_n => '1', s_dataTransferType => s_dataTransferType, s_dataToShift => VME64xBus_In.Vme64xDATA, v_dataToShiftOut => v_dataToReceiveOut);
      assert (v_dataToReceiveOut /= s_dataToReceive)report "CORRECT DATA!!!" severity error;
      assert (v_dataToReceiveOut = s_dataToReceive)report "RECEIVED WRONG DATA!!!" severity failure;
   --assert (VME64xBus_In.Vme64xDATA = s_dataToReceive)report "Error Received Wrong Data" severity failure;
   --wait for 10 ns;
   end if;
   VME64xBus_Out.Vme64xLWORDN <= '1';
   VME64xBus_Out.Vme64xDs0N <= '1';
   VME64xBus_Out.Vme64xDs1N <= '1';
   VME64xBus_Out.Vme64xADDR <= (others => '1');
   VME64xBus_Out.Vme64xAM <= (others => '1');
   -- wait for 0 ns;   -- not necessary
   VME64xBus_Out.Vme64xDATA <= (others => 'Z');
   VME64xBus_Out.Vme64xAsN <= '1';
   VME64xBus_Out.Vme64xWRITEN <='1';
   report "As hight";
end ReadCR_CSR;

procedure SetAmAddress	(signal s_dataTransferType : in t_dataTransferType;
signal s_AddressingType : in t_AddressingType; Vme64xAM : out std_logic_vector (5 downto 0);
DataType : out std_logic_vector (3 downto 0)) is

  begin

     case s_AddressingType is
        when CR_CSR => Vme64xAM    := c_CR_CSR; 
        when A16 => Vme64xAM       := c_A16;
        when A16_LCK => Vme64xAM   := c_A16_LCK;
        when A24 => Vme64xAM       := c_A24_S;
        when A24_BLT => Vme64xAM   := c_A24_BLT;
        when A24_MBLT => Vme64xAM  := c_A24_MBLT;
        when A24_LCK => Vme64xAM   := c_A24_LCK;
        when A32 => Vme64xAM       := c_A32;
        when A32_BLT => Vme64xAM   := c_A32_BLT;
        when A32_MBLT => Vme64xAM  := c_A32_MBLT;
        when A32_LCK => Vme64xAM   := c_A32_LCK;
        when A64 => Vme64xAM       := c_A64;
        when A64_BLT => Vme64xAM   := c_A64_BLT;
        when A64_MBLT => Vme64xAM  := c_A64_MBLT;
        when A64_LCK => Vme64xAM   := c_A64_LCK;
        when A32_2eVME => Vme64xAM := c_TWOedge;
        when A64_2eVME => Vme64xAM := c_TWOedge;
        when A32_2eSST => Vme64xAM := c_TWOedge;
        when A64_2eSST => Vme64xAM := c_TWOedge;
        when others => null;
     end case;

     case s_dataTransferType is
        when D08Byte0  => DataType := "0101";	
        when D08Byte1  => DataType := "1001";
        when D08Byte2  => DataType := "0111";	
        when D08Byte3  => DataType := "1011";
        when D16Byte01 => DataType := "0001";
        when D16Byte23 => DataType := "0011"; 
        when D32       => DataType := "0000";
        when others => null;
     end case;

  end SetAmAddress;

  procedure ShiftData	(write_n : in std_logic; signal s_dataTransferType : in t_dataTransferType; signal s_dataToShift : in std_logic_vector(31 downto 0); v_dataToShiftOut : out std_logic_vector(31 downto 0)) is
  variable v_int : natural := 0;
  variable v_int1 : natural := 0;
  variable dataToShiftOut : std_logic_vector(31 downto 0) := (others => '0');
  begin
     case s_dataTransferType is
        when D08Byte0  => v_int := 1;	
        when D08Byte1  => v_int := 0;
        when D08Byte2  => v_int := 1;	
        when D08Byte3  => v_int := 0;
        when D16Byte01 => v_int := 0;
        when D16Byte23 => v_int := 0; 
        when D32       => v_int := 0;
        when others => null;
     end case;
     v_int1 :=	 v_int * 8;
     dataToShiftOut := s_dataToShift;
     while (v_int1 > 0) loop
        if write_n = '0' then
           dataToShiftOut := dataToShiftOut(30 downto 0) & '0';
        else dataToShiftOut := '0' & dataToShiftOut(31 downto 1);
        end if;
        v_int1 := v_int1 -1;
     end loop;
     v_dataToShiftOut := dataToShiftOut;				 
  end ShiftData;

  procedure SetCrCsrAddress	(c_address : in std_logic_vector(19 downto 0); v_address : out std_logic_vector(31 downto 0)) is
  begin
     v_address := x"00" & not VME_GA(4 downto 0) & c_address(18 downto 0);
  end  SetCrCsrAddress;

  procedure ControlCR (signal s_dataTransferType : in t_dataTransferType; signal s_AddressingType : in t_AddressingType; signal VME64xBus_In : in VME64xBusIn_Record;
  signal s_dataToReceive : inout std_logic_vector(31 downto 0); signal VME64xBus_Out : out VME64xBusOut_Record) is


     file result : text;
     variable sample : line;
     variable address : integer; 
  begin
     address := 0;
     file_open(result, "CR.dat", write_mode);
     while address <= 424 loop
        --if (address = 20) then s_dataToReceive(7 downto 0) <= x"05";  
        --else s_dataToReceive(7 downto 0) <= c_cr_array(address);
        --end if;

        s_dataToReceive(7 downto 0) <= c_cr_array(address);

        ReadCR_CSR(c_address	=> std_logic_vector(to_unsigned((address*4) +3, 20)), s_dataToReceive => s_dataToReceive, s_dataTransferType => s_dataTransferType,
        s_AddressingType => s_AddressingType, VME64xBus_In => VME64xBus_In, 
        VME64xBus_Out => VME64xBus_Out);
        write(sample, address, right, 5);
        --write(sample, "OK", right, 10);
        writeline(result, sample);

        address := address + 1;
     end loop;				
     file_close(result);

     end;  

     procedure S_Write (v_address	: in std_logic_vector(63 downto 0); signal s_dataToSend : in std_logic_vector(31 downto 0);  -- this procedure is for A16, A24, A32 address type
     signal s_dataTransferType : in t_dataTransferType; signal s_AddressingType : in t_AddressingType; 
     signal VME64xBus_In : in VME64xBusIn_Record; signal VME64xBus_Out : out VME64xBusOut_Record) is

        variable  Vme64xAM : std_logic_vector(5 downto 0);
        variable  DataType : std_logic_vector(3 downto 0) := (others => '1');
        variable  v_addressout : std_logic_vector(31 downto 0) := (others => '0');
        variable  v_dataToSendOut : std_logic_vector(31 downto 0) := (others => '0');
        variable ti : time;
     begin

        ti := now;
        if VME64xBus_In.Vme64xDtackN /='1' or VME64xBus_In.Vme64xBerrN /='0' then 
           wait until VME64xBus_In.Vme64xDtackN ='1' and VME64xBus_In.Vme64xBerrN = '0';
        end if;
        VME64xBus_Out.Vme64xAsN <='1';   --initialisation
        VME64xBus_Out.Vme64xWRITEN <='1';
        VME64xBus_Out.Vme64xDs0N <='1';
        VME64xBus_Out.Vme64xDs1N <='1';
        VME64xBus_Out.Vme64xLWORDN <='1';
        VME64xBus_Out.Vme64xADDR <= (others => '1');
        wait for 10 ns;
        report "Start Address Phase";
        -- Address phase
        SetAmAddress(s_dataTransferType => s_dataTransferType, s_AddressingType => s_AddressingType,
        Vme64xAM => Vme64xAM, DataType => DataType);

        SetAddress(c_address => v_address,s_AddressingType => s_AddressingType, v_address => v_addressout);	

        VME64xBus_Out.Vme64xADDR(31 downto 2) <= v_addressout(31 downto 2);
        VME64xBus_Out.Vme64xAM <= Vme64xAM;
        VME64xBus_Out.Vme64xLWORDN <= DataType(0);
        VME64xBus_Out.Vme64xADDR(1) <= DataType(1);
        VME64xBus_Out.Vme64xWRITEN <='0';
        report "End Address Phase";
        wait for 35 ns;

        VME64xBus_Out.Vme64xAsN <='0';
        VME64xBus_Out.Vme64xDs1N <=DataType(3);				 
        VME64xBus_Out.Vme64xDs0N <=DataType(2);
        ShiftData(write_n => '0', s_dataTransferType => s_dataTransferType, s_dataToShift => s_dataToSend, v_dataToShiftOut => v_dataToSendOut);   --procedura che prende il dato dai lsb e lo mette nei byte giusti
        VME64xBus_Out.Vme64xDATA <= v_dataToSendOut;
        report "Wait for DTACK";
        wait until (VME64xBus_In.Vme64xDtackN = '0' or (now > (ti + 4 us)) or VME64xBus_In.Vme64xBerrN = '1');
        assert (VME64xBus_In.Vme64xBerrN /= '1') report "THE SLAVE ASSERTED THE Berr LINE" severity error;
        VME64xBus_Out.Vme64xDs0N <= '1';
        VME64xBus_Out.Vme64xDs1N <= '1';
        VME64xBus_Out.Vme64xLWORDN <= '1';
        wait for 0 ns;   -- not necessary

        VME64xBus_Out.Vme64xADDR <= (others => '1');
        VME64xBus_Out.Vme64xAM <= (others => '1');
        VME64xBus_Out.Vme64xDATA <= (others => 'Z');
        VME64xBus_Out.Vme64xAsN <= '1';
        VME64xBus_Out.Vme64xWRITEN <='1';

        
     end ;	 
	  
	  procedure S_Read (v_address	: in std_logic_vector(63 downto 0); signal s_dataToReceive : in std_logic_vector(31 downto 0);  -- this procedure is for A16, A24, A32 address type
     signal s_dataTransferType : in t_dataTransferType; signal s_AddressingType : in t_AddressingType; 
     signal VME64xBus_In : in VME64xBusIn_Record; signal VME64xBus_Out : out VME64xBusOut_Record) is
	  
	  variable  Vme64xAM : std_logic_vector(5 downto 0);
   variable  DataType : std_logic_vector(3 downto 0) := (others => '1');
   variable  v_addressout : std_logic_vector(31 downto 0) := (others => '0');
   variable  v_dataToReceiveOut : std_logic_vector(31 downto 0) := (others => '0');
   variable ti : time;
	
	  begin
	  ti := now;
   if VME64xBus_In.Vme64xDtackN /='1' or VME64xBus_In.Vme64xBerrN /= '0' then 
      wait until VME64xBus_In.Vme64xDtackN ='1' and VME64xBus_In.Vme64xBerrN = '0';
   end if;
   --initialisation
   VME64xBus_Out.Vme64xAsN <='1';   
   VME64xBus_Out.Vme64xWRITEN <='1';
   VME64xBus_Out.Vme64xDs0N <='1';
   VME64xBus_Out.Vme64xDs1N <='1';
   VME64xBus_Out.Vme64xLWORDN <='1';
   VME64xBus_Out.Vme64xADDR <= (others => '1');
   wait for 10 ns;
   report "Start Address Phase";
   -- Address phase
   SetAmAddress(s_dataTransferType => s_dataTransferType, s_AddressingType => s_AddressingType,
   Vme64xAM => Vme64xAM, DataType => DataType);

   SetAddress(c_address => v_address, s_AddressingType => s_AddressingType, v_address => v_addressout);	

   VME64xBus_Out.Vme64xADDR(31 downto 2) <= v_addressout(31 downto 2);
   VME64xBus_Out.Vme64xAM <= Vme64xAM;
   VME64xBus_Out.Vme64xLWORDN <= DataType(0);
   VME64xBus_Out.Vme64xADDR(1) <= DataType(1);
   VME64xBus_Out.Vme64xWRITEN <='1';
   report "End Address Phase";
   wait for 35 ns;    -- check the min time here...for master is 35 ns and for slave 10 ns.
   report "Master drive the AS low";
   VME64xBus_Out.Vme64xAsN <='0';
   VME64xBus_Out.Vme64xDs1N <=DataType(3);				 
   VME64xBus_Out.Vme64xDs0N <=DataType(2);

   report "Wait for DTACK";
   wait until (VME64xBus_In.Vme64xDtackN = '0' or (now > (ti + 4 us)) or VME64xBus_In.Vme64xBerrN = '1');
  -- wait for 10 ns;
   if(VME64xBus_In.Vme64xBerrN = '1') then 
      assert(VME64xBus_In.Vme64xBerrN /= '1') report "THE SLAVE ASSERTED THE Berr LINE" severity error;
   else	  
      ShiftData(write_n => '1', s_dataTransferType => s_dataTransferType, s_dataToShift => VME64xBus_In.Vme64xDATA, v_dataToShiftOut => v_dataToReceiveOut);
      assert (v_dataToReceiveOut /= s_dataToReceive)report "CORRECT DATA!!!" severity error;
      assert (v_dataToReceiveOut = s_dataToReceive)report "RECEIVED WRONG DATA!!!" severity failure;
   
   end if;
   VME64xBus_Out.Vme64xLWORDN <= '1';
   VME64xBus_Out.Vme64xDs0N <= '1';
   VME64xBus_Out.Vme64xDs1N <= '1';
   VME64xBus_Out.Vme64xADDR <= (others => '1');
   VME64xBus_Out.Vme64xAM <= (others => '1');
   -- wait for 0 ns;   -- not necessary
   VME64xBus_Out.Vme64xDATA <= (others => 'Z');
   VME64xBus_Out.Vme64xAsN <= '1';
   VME64xBus_Out.Vme64xWRITEN <='1';
   report "As hight";
	  end;
--------------------------------------------------------------------------	  
	 procedure Blt_Read(v_address	: in std_logic_vector(63 downto 0); signal s_Buffer_BLT : in t_Buffer_BLT;  -- this procedure is for A16, A24, A32 address type
   signal s_dataTransferType : in t_dataTransferType; signal s_AddressingType : in t_AddressingType; num : in std_logic_vector(8 downto 0); 
   signal VME64xBus_In : in VME64xBusIn_Record; signal VME64xBus_Out : out VME64xBusOut_Record) is
    variable n : integer;
	 variable  Vme64xAM : std_logic_vector(5 downto 0);
   variable  DataType : std_logic_vector(3 downto 0) := (others => '1');
   variable  v_addressout : std_logic_vector(31 downto 0) := (others => '0');
   variable  v_dataToReceiveOut : std_logic_vector(31 downto 0) := (others => '0');
   variable ti : time;
	
	  begin
	  n := 0;
	  ti := now;
     if VME64xBus_In.Vme64xDtackN /='1' or VME64xBus_In.Vme64xBerrN /= '0' then 
     wait until VME64xBus_In.Vme64xDtackN ='1' and VME64xBus_In.Vme64xBerrN = '0';
     end if;
	 --initialisation
     VME64xBus_Out.Vme64xAsN <='1';   
     VME64xBus_Out.Vme64xWRITEN <='1';
     VME64xBus_Out.Vme64xDs0N <='1';
     VME64xBus_Out.Vme64xDs1N <='1';
     VME64xBus_Out.Vme64xLWORDN <='1';
     VME64xBus_Out.Vme64xADDR <= (others => '1');
     wait for 10 ns;
	  SetAmAddress(s_dataTransferType => s_dataTransferType, s_AddressingType => s_AddressingType,
     Vme64xAM => Vme64xAM, DataType => DataType);

     SetAddress(c_address => v_address, s_AddressingType => s_AddressingType, v_address => v_addressout);	
	  VME64xBus_Out.Vme64xADDR(31 downto 2) <= v_addressout(31 downto 2);
     VME64xBus_Out.Vme64xAM <= Vme64xAM;
     VME64xBus_Out.Vme64xLWORDN <= DataType(0);
     VME64xBus_Out.Vme64xADDR(1) <= DataType(1);
     VME64xBus_Out.Vme64xWRITEN <='1';
	  wait for 35 ns;
	  VME64xBus_Out.Vme64xAsN <='0';
	  wait for 10 ns;
	  
	  while  n < (num) loop
     VME64xBus_Out.Vme64xDs1N <=DataType(3);				 
     VME64xBus_Out.Vme64xDs0N <=DataType(2);
	  wait until (VME64xBus_In.Vme64xDtackN = '0' or (now > (ti + 4 us)) or VME64xBus_In.Vme64xBerrN = '1');
	  if(VME64xBus_In.Vme64xBerrN = '1') then 
      assert(VME64xBus_In.Vme64xBerrN /= '1') report "THE SLAVE ASSERTED THE Berr LINE" severity error;
      VME64xBus_Out.Vme64xDs0N <= '1';
      VME64xBus_Out.Vme64xDs1N <= '1';     
	   exit;
	  else	  
	   v_dataToReceiveOut := VME64xBus_In.Vme64xDATA;
      assert (v_dataToReceiveOut /= s_Buffer_BLT(n + 1))report "CORRECT DATA!!!" severity error;
      assert (v_dataToReceiveOut = s_Buffer_BLT(n + 1))report "RECEIVED WRONG DATA!!!" severity failure;
     end if;
      VME64xBus_Out.Vme64xDs0N <= '1';
      VME64xBus_Out.Vme64xDs1N <= '1';
      wait for 40 ns;
      n := n + 1;
	  end loop;

     VME64xBus_Out.Vme64xADDR <= (others => '1');
     VME64xBus_Out.Vme64xAM <= (others => '1');
     VME64xBus_Out.Vme64xDATA <= (others => 'Z');
     VME64xBus_Out.Vme64xAsN <= '1';
     VME64xBus_Out.Vme64xWRITEN <='1';
	 end; 
	  
	  procedure Blt_write(v_address	: in std_logic_vector(63 downto 0); signal s_Buffer_BLT : in t_Buffer_BLT;  -- this procedure is for A16, A24, A32 address type
              signal s_dataTransferType : in t_dataTransferType; signal s_AddressingType : in t_AddressingType; num : in std_logic_vector(8 downto 0); 
              signal VME64xBus_In : in VME64xBusIn_Record; signal VME64xBus_Out : out VME64xBusOut_Record) is
     variable n : integer;
	  variable  Vme64xAM : std_logic_vector(5 downto 0);
     variable  DataType : std_logic_vector(3 downto 0) := (others => '1');
     variable  v_addressout : std_logic_vector(31 downto 0) := (others => '0');
     --variable  v_dataToSendOut : std_logic_vector(31 downto 0) := (others => '0');
     variable ti : time;
	
	  begin
	  n := 0;
	  ti := now;
     if VME64xBus_In.Vme64xDtackN /='1' or VME64xBus_In.Vme64xBerrN /= '0' then 
     wait until VME64xBus_In.Vme64xDtackN ='1' and VME64xBus_In.Vme64xBerrN = '0';
     end if;
	 --initialisation
     VME64xBus_Out.Vme64xAsN <='1';   
     VME64xBus_Out.Vme64xWRITEN <='1';
     VME64xBus_Out.Vme64xDs0N <='1';
     VME64xBus_Out.Vme64xDs1N <='1';
     VME64xBus_Out.Vme64xLWORDN <='1';
     VME64xBus_Out.Vme64xADDR <= (others => '1');
     wait for 10 ns;
	  SetAmAddress(s_dataTransferType => s_dataTransferType, s_AddressingType => s_AddressingType,
     Vme64xAM => Vme64xAM, DataType => DataType);

     SetAddress(c_address => v_address, s_AddressingType => s_AddressingType, v_address => v_addressout);	
	  VME64xBus_Out.Vme64xADDR(31 downto 2) <= v_addressout(31 downto 2);
     VME64xBus_Out.Vme64xAM <= Vme64xAM;
     VME64xBus_Out.Vme64xLWORDN <= DataType(0);
     VME64xBus_Out.Vme64xADDR(1) <= DataType(1);
     VME64xBus_Out.Vme64xWRITEN <='0';
	  wait for 35 ns;
	  VME64xBus_Out.Vme64xAsN <='0';
	  while  n < (num) loop
	  VME64xBus_Out.Vme64xDATA <= s_Buffer_BLT(n);
	  wait for 35 ns;
     VME64xBus_Out.Vme64xDs1N <= DataType(3);				 
     VME64xBus_Out.Vme64xDs0N <= DataType(2);
	  wait until (VME64xBus_In.Vme64xDtackN = '0' or (now > (ti + 15 us)) or VME64xBus_In.Vme64xBerrN = '1');
	  if(VME64xBus_In.Vme64xBerrN = '1') then 
      assert(VME64xBus_In.Vme64xBerrN /= '1') report "THE SLAVE ASSERTED THE Berr LINE" severity error;
      VME64xBus_Out.Vme64xDs0N <= '1';
      VME64xBus_Out.Vme64xDs1N <= '1';
		exit;
     end if;
      VME64xBus_Out.Vme64xDs0N <= '1';
      VME64xBus_Out.Vme64xDs1N <= '1';
      wait for 10 ns;
      n := n + 1;
	  end loop;

     VME64xBus_Out.Vme64xADDR <= (others => '1');
     VME64xBus_Out.Vme64xAM <= (others => '1');
     VME64xBus_Out.Vme64xDATA <= (others => 'Z');
     VME64xBus_Out.Vme64xAsN <= '1';
     VME64xBus_Out.Vme64xWRITEN <='1';
	 end; 
	  
	 procedure Mblt_write(v_address	: in std_logic_vector(63 downto 0); signal s_Buffer_MBLT : in t_Buffer_MBLT;  -- this procedure is for A16, A24, A32 address type
    signal s_dataTransferType : in t_dataTransferType; signal s_AddressingType : in t_AddressingType; num : in std_logic_vector(8 downto 0); 
    signal VME64xBus_In : in VME64xBusIn_Record; signal VME64xBus_Out : out VME64xBusOut_Record) is
    
	  variable n : integer;
	  variable  Vme64xAM : std_logic_vector(5 downto 0);
     variable  DataType : std_logic_vector(3 downto 0) := (others => '1');
     variable  v_addressout : std_logic_vector(31 downto 0) := (others => '0');
     variable ti : time;
	 begin 
	  n := 0;
	  ti := now;
     if VME64xBus_In.Vme64xDtackN /='1' or VME64xBus_In.Vme64xBerrN /= '0' then 
     wait until VME64xBus_In.Vme64xDtackN ='1' and VME64xBus_In.Vme64xBerrN = '0';
     end if;
	 --initialisation
     VME64xBus_Out.Vme64xAsN <='1';   
     VME64xBus_Out.Vme64xWRITEN <='1';
     VME64xBus_Out.Vme64xDs0N <='1';
     VME64xBus_Out.Vme64xDs1N <='1';
     VME64xBus_Out.Vme64xLWORDN <='1';
     VME64xBus_Out.Vme64xADDR <= (others => '1');
     wait for 10 ns;
	  SetAmAddress(s_dataTransferType => s_dataTransferType, s_AddressingType => s_AddressingType,
     Vme64xAM => Vme64xAM, DataType => DataType);

     SetAddress(c_address => v_address, s_AddressingType => s_AddressingType, v_address => v_addressout);	
	  VME64xBus_Out.Vme64xADDR(31 downto 2) <= v_addressout(31 downto 2);
     VME64xBus_Out.Vme64xAM <= Vme64xAM;
     VME64xBus_Out.Vme64xLWORDN <= DataType(0);
     VME64xBus_Out.Vme64xADDR(1) <= DataType(1);
     VME64xBus_Out.Vme64xWRITEN <='0';
	  wait for 35 ns;
	  VME64xBus_Out.Vme64xAsN <='0';
	  wait for 10 ns;
	  VME64xBus_Out.Vme64xDs1N <= DataType(3);				 
     VME64xBus_Out.Vme64xDs0N <= DataType(2);
	  wait until (VME64xBus_In.Vme64xDtackN = '0' or (now > (ti + 5 us)) or VME64xBus_In.Vme64xBerrN = '1');
	  if(VME64xBus_In.Vme64xBerrN = '1' or (now > (ti + 5 us))) then 
      assert(VME64xBus_In.Vme64xBerrN /= '1') report "THE SLAVE ASSERTED THE Berr LINE" severity error;
	   VME64xBus_Out.Vme64xDs0N <= '1';
      VME64xBus_Out.Vme64xDs1N <= '1';
	  else 
      VME64xBus_Out.Vme64xDs0N <= '1';
      VME64xBus_Out.Vme64xDs1N <= '1';
		wait for 35 ns;
		while  n < (num) loop
      VME64xBus_Out.Vme64xDATA <= s_Buffer_MBLT(n)(31 downto 0);
		VME64xBus_Out.Vme64xADDR(31 downto 1) <= s_Buffer_MBLT(n)(63 downto 33);
		VME64xBus_Out.Vme64xLWORDN <= s_Buffer_MBLT(n)(32);
		VME64xBus_Out.Vme64xDs1N <= DataType(3);				 
      VME64xBus_Out.Vme64xDs0N <= DataType(2);
	   wait until (VME64xBus_In.Vme64xDtackN = '0' or (now > (ti + 100 us)) or VME64xBus_In.Vme64xBerrN = '1');
	   if(VME64xBus_In.Vme64xBerrN = '1' or (now > (ti + 100 us))) then 
      assert(VME64xBus_In.Vme64xBerrN /= '1') report "THE SLAVE ASSERTED THE Berr LINE" severity error;
      VME64xBus_Out.Vme64xDs0N <= '1';
      VME64xBus_Out.Vme64xDs1N <= '1';
		exit;
		else
		VME64xBus_Out.Vme64xDs0N <= '1';
      VME64xBus_Out.Vme64xDs1N <= '1';	
	   n := n + 1;
		wait for 35 ns;
		end if;
		end loop;
	  end if;
      
	  VME64xBus_Out.Vme64xADDR <= (others => '1');
     VME64xBus_Out.Vme64xAM <= (others => '1');
     VME64xBus_Out.Vme64xDATA <= (others => 'Z');
     VME64xBus_Out.Vme64xAsN <= '1';
     VME64xBus_Out.Vme64xWRITEN <='1';
	 end;

     procedure Mblt_Read(v_address	: in std_logic_vector(63 downto 0); signal s_Buffer_MBLT : in t_Buffer_MBLT;  -- this procedure is for A16, A24, A32 address type
      signal s_dataTransferType : in t_dataTransferType; signal s_AddressingType : in t_AddressingType; num : in std_logic_vector(8 downto 0); 
      signal VME64xBus_In : in VME64xBusIn_Record; signal VME64xBus_Out : out VME64xBusOut_Record) is
	   variable  n : integer;
	   variable  Vme64xAM : std_logic_vector(5 downto 0);
      variable  DataType : std_logic_vector(3 downto 0) := (others => '1');
      variable  v_addressout : std_logic_vector(31 downto 0) := (others => '0');
      variable  v_dataToReceiveOut : std_logic_vector(63 downto 0) := (others => '0');
      variable  ti : time;
	  begin
	  n := 0;
	  ti := now;
     if VME64xBus_In.Vme64xDtackN /='1' or VME64xBus_In.Vme64xBerrN /= '0' then 
     wait until VME64xBus_In.Vme64xDtackN ='1' and VME64xBus_In.Vme64xBerrN = '0';
     end if;
	 --initialisation
     VME64xBus_Out.Vme64xAsN <='1';   
     VME64xBus_Out.Vme64xWRITEN <='1';
     VME64xBus_Out.Vme64xDs0N <='1';
     VME64xBus_Out.Vme64xDs1N <='1';
     VME64xBus_Out.Vme64xLWORDN <='1';
     VME64xBus_Out.Vme64xADDR <= (others => '1');
     wait for 10 ns;
	  SetAmAddress(s_dataTransferType => s_dataTransferType, s_AddressingType => s_AddressingType,
     Vme64xAM => Vme64xAM, DataType => DataType);

     SetAddress(c_address => v_address, s_AddressingType => s_AddressingType, v_address => v_addressout);	
	  VME64xBus_Out.Vme64xADDR(31 downto 2) <= v_addressout(31 downto 2);
     VME64xBus_Out.Vme64xAM <= Vme64xAM;
     VME64xBus_Out.Vme64xLWORDN <= DataType(0);
     VME64xBus_Out.Vme64xADDR(1) <= DataType(1);
     VME64xBus_Out.Vme64xWRITEN <='1';
	  wait for 35 ns;
	  VME64xBus_Out.Vme64xAsN <='0';	
     wait for 10 ns;
	  VME64xBus_Out.Vme64xDs1N <= DataType(3);				 
     VME64xBus_Out.Vme64xDs0N <= DataType(2);
	  wait until (VME64xBus_In.Vme64xDtackN = '0' or (now > (ti + 5 us)) or VME64xBus_In.Vme64xBerrN = '1');
	  if(VME64xBus_In.Vme64xBerrN = '1' or (now > (ti + 5 us))) then -- if the slave answer, will answer in 5/10 Tclk
      assert(VME64xBus_In.Vme64xBerrN /= '1') report "THE SLAVE ASSERTED THE Berr LINE" severity error;
	   VME64xBus_Out.Vme64xDs0N <= '1';
      VME64xBus_Out.Vme64xDs1N <= '1';
	  else 
      VME64xBus_Out.Vme64xDs0N <= '1';
      VME64xBus_Out.Vme64xDs1N <= '1';
		wait for 35 ns;
		while  n < (num) loop
		VME64xBus_Out.Vme64xDs1N <= DataType(3);				 
      VME64xBus_Out.Vme64xDs0N <= DataType(2);
	   wait until (VME64xBus_In.Vme64xDtackN = '0' or (now > (ti + 100 us)) or VME64xBus_In.Vme64xBerrN = '1');
	   if(VME64xBus_In.Vme64xBerrN = '1' or (now > (ti + 100 us))) then 
       assert(VME64xBus_In.Vme64xBerrN /= '1') report "THE SLAVE ASSERTED THE Berr LINE" severity error;
       VME64xBus_Out.Vme64xDs0N <= '1';
       VME64xBus_Out.Vme64xDs1N <= '1';
		 exit;
		else
	    v_dataToReceiveOut(63 downto 33) := VME64xBus_In.Vme64xADDR;	
       v_dataToReceiveOut(31 downto 0) := VME64xBus_In.Vme64xDATA;  		
		 v_dataToReceiveOut(32) := VME64xBus_In.Vme64xLWORDN;
		 assert (v_dataToReceiveOut /= s_Buffer_MBLT(n))report "CORRECT DATA!!!" severity error;
       assert (v_dataToReceiveOut = s_Buffer_MBLT(n))report "RECEIVED WRONG DATA!!!" severity failure;
		 --NB start to read from the first location written otherwise use n + x
		VME64xBus_Out.Vme64xDs0N <= '1';
      VME64xBus_Out.Vme64xDs1N <= '1';	
	   n := n + 1;
		wait for 35 ns;
		end if;
		end loop;
	  
     end if;
	  VME64xBus_Out.Vme64xADDR <= (others => '1');
     VME64xBus_Out.Vme64xAM <= (others => '1');
     VME64xBus_Out.Vme64xDATA <= (others => 'Z');
     VME64xBus_Out.Vme64xAsN <= '1';
     VME64xBus_Out.Vme64xWRITEN <='1';

     end;
	 
     procedure SetAddress	(c_address : in std_logic_vector(63 downto 0); signal s_AddressingType : in t_AddressingType; v_address : out std_logic_vector(31 downto 0)) is

     begin
        case s_AddressingType is 
           when A16 => v_address := x"0000" & BA(7 downto 3) & c_address(10 downto 2) & "00";		
           when A24 => v_address := x"00" & BA(7 downto 3) & c_address(18 downto 2) & "00";
           when A32 => v_address := BA(7 downto 3) & c_address(26 downto 2) & "00";
           when A32_BLT => v_address := BA(7 downto 3) & c_address(26 downto 2) & "00";
			  when A24_BLT => v_address := x"00" & BA(7 downto 3) & c_address(18 downto 2) & "00";
			  when A32_MBLT => v_address := BA(7 downto 3) & c_address(26 downto 2) & "00";
			  when A24_MBLT => v_address := x"00" & BA(7 downto 3) & c_address(18 downto 2) & "00";
			  
			  
			  when others => null;
        end case;
      end;	
      
		
     end VME64xSim;
