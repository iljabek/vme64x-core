-------------------------------------------------------------------------------
--
-- Title       : VME_bus
-- Design      : VME64xCore
-- Author      : Ziga Kroflic
-- Company     : Cosylab
--
-------------------------------------------------------------------------------
--
-- File        : VME_bus.vhd
-- Generated   : Wed Mar 10 09:27:09 2010
-- From        : interface description file
-- By          : Itf2Vhdl ver. 1.20
--
-------------------------------------------------------------------------------
--
-- Description : 
--
-------------------------------------------------------------------------------



library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.numeric_std.all;

--use IEEE.STD_LOGIC_unsigned.all;
use work.VME_pack.all;
use work.VME_CR_pack.all;
use work.VME_CSR_pack.all;
entity VME_bus is
  port(
    clk_i   : in  std_logic;
    reset_o : out std_logic;

    -- VME signals
    VME_RST_n_i     : in  std_logic;
    VME_AS_n_i      : in  std_logic;
    VME_LWORD_n_b_o : out std_logic;
    VME_LWORD_n_b_i : in  std_logic;

    VME_RETRY_n_o    : out std_logic;
    VME_RETRY_OE_n_o : out std_logic;
    VME_WRITE_n_i    : in  std_logic;
    VME_DS_n_i       : in  std_logic_vector(1 downto 0);
    VME_GA_i         : in  std_logic_vector(5 downto 0);  -- Geographical Address and GA parity
    VME_DTACK_n_o    : out std_logic;
    VME_DTACK_OE_o   : out std_logic;

    VME_BERR_o : out std_logic;

    VME_ADDR_b_i   : in  std_logic_vector(31 downto 1);
    VME_ADDR_b_o   : out std_logic_vector(31 downto 1);
    VME_ADDR_DIR_o : out std_logic;
    VME_ADDR_OE_o  : out std_logic;

    VME_DATA_b_i   : in  std_logic_vector(31 downto 0);
    VME_DATA_b_o   : out std_logic_vector(31 downto 0);
    VME_DATA_DIR_o : out std_logic;
    VME_DATA_OE_o  : out std_logic;

    VME_AM_i       : in std_logic_vector(5 downto 0);
    VME_BBSY_n_i   : in std_logic;
    VME_IACKIN_n_i : in std_logic;


    -- CROM
    CRaddr_o : out std_logic_vector(18 downto 0);
    CRdata_i : in  std_logic_vector(7 downto 0);

    -- CRAM
    CRAMaddr_o : out std_logic_vector(18 downto 0);
    CRAMdata_o : out std_logic_vector(7 downto 0);
    CRAMdata_i : in  std_logic_vector(7 downto 0);
    CRAMwea_o  : out std_logic;

    -- WB signals
    memReq_o    : out std_logic;
    memAckWB_i  : in  std_logic;
    wbData_o    : out std_logic_vector(63 downto 0);
    wbData_i    : in  std_logic_vector(63 downto 0);
    locAddr_o   : out std_logic_vector(63 downto 0);
    wbSel_o     : out std_logic_vector(7 downto 0);
    RW_o        : out std_logic;
    lock_o      : out std_logic;
    cyc_o       : out std_logic;
    err_i       : in  std_logic;
    rty_i       : in  std_logic;
    stall_i     : in std_logic;
    psize_o : out std_logic_vector(7 downto 0);

    -- IRQ controller signals
    irqDTACK_i       : in  std_logic;
    IACKinProgress_i : in  std_logic;
    IDtoData_i       : in  std_logic;
    IRQlevelReg_o    : out std_logic_vector(7 downto 0);

    -- 2e related signals
--    FIFOwren_o       : out std_logic;
--    FIFOdata_o       : out std_logic_vector(63 downto 0);
--    FIFOrden_o       : out std_logic;
--    FIFOdata_i       : in  std_logic_vector(63 downto 0);
transfer_done_i : in std_logic;
    TWOeInProgress_o : out std_logic
    );
end VME_bus;



architecture RTL of VME_bus is

  component RegInputSample is
    generic(
      width : natural := 8
      );
    port (
      reg_i : in  std_logic_vector(width-1 downto 0);
      reg_o : out std_logic_vector(width-1 downto 0) := (others => '0');
      clk_i : in  std_logic
      );
  end component;

  component DoubleRegInputSample is
    generic(
      width : natural := 8
      );
    port (
      reg_i : in  std_logic_vector(width-1 downto 0);
      reg_o : out std_logic_vector(width-1 downto 0) := (others => '0');
      clk_i : in  std_logic
      );
  end component;

  component SigInputSample is
    port (
      sig_i, clk_i : in  std_logic;
      sig_o        : out std_logic);
  end component;

  component DoubleSigInputSample is
    port (
      sig_i, clk_i : in  std_logic;
      sig_o        : out std_logic);
  end component;

  component RisEdgeDetection is
    port (
      sig_i, clk_i : in  std_logic;
      RisEdge_o    : out std_logic);
  end component;

  component FallingEdgeDetection is
    port (
      sig_i, clk_i : in  std_logic;
      FallEdge_o   : out std_logic);
  end component;

  component EdgeDetection is
    port (
      sig_i,
      clk_i     : in  std_logic;
      sigEdge_o : out std_logic := '0'
      );
  end component;

  signal s_reset        : std_logic;
  signal s_moduleEnable : std_logic;

-- Oversampled input signals 
  signal VME_RST_n_oversampled                        : std_logic;
  signal VME_AS_n_oversampled                         : std_logic;
  signal VME_LWORD_n_oversampled                      : std_logic;
--signal VME_RETRY_n_oversampled : STD_LOGIC;
  signal VME_WRITE_n_oversampled                      : std_logic;
  signal VME_DS_n_oversampled, VME_DS_n_oversampled_1 : std_logic_vector(1 downto 0);
  signal VME_GA_oversampled                           : std_logic_vector(5 downto 0);
  signal VME_ADDR_oversampled                         : std_logic_vector(31 downto 1);
  signal VME_DATA_oversampled                         : std_logic_vector(31 downto 0);
  signal VME_AM_oversampled                           : std_logic_vector(5 downto 0);
  signal VME_BBSY_n_oversampled                       : std_logic;
  signal VME_IACKIN_n_oversampled                     : std_logic;

-- Bidirectional signals
  signal s_VMEaddrInput : unsigned(31 downto 1);
--signal s_VMEaddrOutput: unsigned(31 downto 1);
  signal s_VMEdataInput : unsigned(31 downto 0);
--signal s_VMEdataOutput: unsigned(31 downto 0);
  signal s_LWORDinput   : std_logic;
--signal s_LWORDoutput: std_logic;  

-- External latch signals
  signal s_dtackOE : std_logic;
  signal s_dataDir : std_logic;
  signal s_dataOE  : std_logic;
  signal s_addrDir : std_logic;
  signal s_addrOE  : std_logic;

-- Local data & address
  signal s_locDataIn                                   : unsigned(63 downto 0);
  signal s_locDataOut                                  : unsigned(63 downto 0);
  signal s_locData                                     : unsigned(63 downto 0);  -- Local data
  signal s_locAddr                                     : unsigned(63 downto 0);  -- Local address
  signal s_locAddr2e                                   : unsigned(63 downto 0);  -- Local address for 2e transfers
  signal s_locAddrBeforeOffset                         : unsigned(63 downto 0);
  signal s_phase1addr                                  : unsigned(63 downto 0);  -- Stores received address in a certain address phase (for 2e transfers)
  signal s_phase2addr                                  : unsigned(63 downto 0);  --
  signal s_phase3addr                                  : unsigned(63 downto 0);  --
  signal s_addrOffset                                  : unsigned(17 downto 0);  -- Offset to the initial address (for block transfers)
  signal s_CrCsrOffsetAddr, s_CrCsrOffsetAderIndex_aux : unsigned(18 downto 0);  -- Offset to the initial CR/CSR address (for block transfers)
  signal s_CrCsrOffsetAderIndex                        : unsigned(9 downto 0);

-- Latched signals
  signal s_VMEaddrLatched : unsigned(63 downto 1);  -- Stores address on falling edge of AS
  signal s_LWORDlatched   : std_logic;  -- Stores LWORD on falling edge of AS
  signal s_DSlatched      : std_logic_vector(1 downto 0);  -- Stores DS
  signal s_AMlatched      : std_logic_vector(5 downto 0);  -- Stores AM on falling edge of AS            

-- Type of data transfer (depending on VME_DS_n, VME_LWORD_n and VME_ADDR(1))
  type t_typeOfDataTransfer is (D08,
                                  D16,
                                  D32,
                                  UnAl0to2,
                                  UnAl1to3,
                                  UnAl1to2,
                                  TypeError
                                  );
  signal s_typeOfDataTransfer       : t_typeOfDataTransfer;
  signal s_typeOfDataTransferSelect : std_logic_vector(3 downto 0);

-- Addressing type (depending on VME_AM)
  type t_addressingType is (A24,
                               A24_BLT,
                               A24_MBLT,
                               A24_LCK,
                               CR_CSR,
                               A16,
                               A16_LCK,
                               A32,
                               A32_BLT,
                               A32_MBLT,
                               A32_LCK,
                               A64,
                               A64_BLT,
                               A64_MBLT,
                               A64_LCK,
                               TWOedge,
                               AM_Error
                               );
  signal s_addressingType       : t_addressingType;
  signal s_addressingTypeSelect : std_logic_vector(5 downto 0);

  type t_transferType is (SINGLE,
                          BLT,
                          MBLT,
                          LCK,
                          error
                          );
  signal s_transferType : t_transferType;

  type t_XAMtype is (A32_2eVME,
                      A64_2eVME,
                      A32_2eSST,
                      A64_2eSST,
                      A32_2eSSTb,
                      A64_2eSSTb,
                      XAM_error
                      );
  signal s_XAMtype : t_XAMtype;

  type t_2eType is (TWOe_VME,
                     TWOe_SST
                     );
  signal s_2eType : t_2eType;

-- Main FSM signals 
  type t_mainFSMstates is (IDLE,
                           DECODE_ACCESS,
                           WAIT_FOR_DS,
                           LATCH_DS,
                           CHECK_TRANSFER_TYPE,
                           MEMORY_REQ,
                           DATA_TO_BUS,
                           DTACK_LOW,
                           DECIDE_NEXT_CYCLE,
                           INCREMENT_ADDR,
                           SET_DATA_PHASE,
                           ACKNOWLEDGE_LOCK,
                           WAIT_FOR_DS_2e,
                           ADDR_PHASE_1,
                           ADDR_PHASE_2,
                           ADDR_PHASE_3,
                           DECODE_ACCESS_2e,
                           DTACK_PHASE_1,
                           DTACK_PHASE_2,
                           DTACK_PHASE_3,
                           TWOe_FIFO_WRITE,
                           TWOe_TOGGLE_DTACK,
                           TWOe_WAIT_FOR_DS1,
                           TWOe_FIFO_WAIT_READ,
                           TWOe_FIFO_READ,
                           TWOe_CHECK_BEAT,
                           TWOe_RELEASE_DTACK,
                           TWOe_END_1,
                           TWOe_END_2
                           );
  signal s_mainFSMstate : t_mainFSMstates;

  signal s_dataToAddrBus : std_logic;  -- Puts data to VME data and address bus (for D64)
  signal s_dataToOutput  : std_logic;   -- Puts data to VME data bus

  signal s_mainDTACK : std_logic;       -- DTACK driving

  signal s_2eLatchAddr : std_logic_vector(1 downto 0);  -- Stores address in different address phases (for 2e transfers)
--  signal s_readFIFO    : std_logic;     -- FIFO memory request

--signal s_dataWidth: std_logic_vector(1 downto 0);        -- Tells WB the width of valid data
  signal s_addrWidth               : std_logic_vector(1 downto 0);  -- Width of valid address 
  signal s_memAck                  : std_logic;  -- Memory acknowledge (from CR/CSR or from WB)
  signal s_memAckCSR               : std_logic_vector(2 downto 0);  -- Memory acknowledge from CR/CSR (shift register for delaying of the acknowledge)
  signal s_memReq                  : std_logic;  -- Global memory request   
  signal s_VMEaddrLatch            : std_logic;  -- Stores address on falling edge of VME_AS_n_i
  signal s_DSlatch                 : std_logic;  -- Stores data strobes
  signal s_incrementAddr           : std_logic;  -- Increments local address (pulse on rising edge)
  signal s_incrementAddr_1         : std_logic;  --
  signal s_incrementAddrPulse      : std_logic;  --
  signal s_resetAddrOffset         : std_logic;  -- Resets address offset
  signal s_blockTransferLimit      : std_logic;  -- Block transfer is limited to 255 bytes
  signal s_blockTransferLimitPulse : std_logic;  -- Rising edge on s_blockTransferLimit
  signal s_mainFSMreset            : std_logic;  -- Resets main FSM on rising edge of address strobe
  signal s_dataPhase               : std_logic;  -- Indicates that multiplexed transfer is in data phase
  signal s_transferActive          : std_logic;  -- Indicates an active VME transfer
  signal s_setLock                 : std_logic;  -- Sets LOCK towards WB slave
  signal s_TWOeInProgress          : std_logic;  -- Indicates that 2eSST is in progress
  signal s_retry                   : std_logic;  -- RETRY signal
  signal s_berr                    : std_logic;  -- BERR signal
  signal s_berr_1                  : std_logic;  -- Berr condition must be active for at least two cycles
  signal s_berr_2                  : std_logic;  --    

-- Access decode signals
  signal s_confAccess : std_logic;      -- Asserted when CR or CSR is addressed
  signal s_cardSel    : std_logic;  -- Asserted when internal memory space is addressed 
  signal s_lockSel    : std_logic;  -- Asserted when function losk is correctly addressed
--signal s_memAckCaseCondition: std_logic_vector(1 downto 0); -- Used in p_memAck for case condition 

  signal s_XAM : unsigned(7 downto 0);  -- Stores received XAM

--  type t_funcMatch is array (0 to 7) of std_logic;            -- Indicates that a certain function has been sucesfully decoded
  signal s_funcMatch : std_logic_vector(7 downto 0);

-- type t_AMmatch is array (0 to 7) of std_logic;              -- Indicates that received AM matches the one programmed in ADER
  signal s_AMmatch : std_logic_vector(7 downto 0);

-- WishBone signals
  signal s_sel  : std_logic_vector(7 downto 0);  -- SEL WB signal
  signal s_RW   : std_logic;            -- RW WB signal
  signal s_lock : std_logic;            -- LOCK WB signal
  signal s_cyc  : std_logic;  -- CYC WB signal                         

-- 2e related signals
  signal s_beatCount        : unsigned(8 downto 0);  -- cycleCount*2 for 2eVME, cycleCount for 2eSST
  signal s_runningBeatCount : unsigned(8 downto 0);  -- Beat counter
  signal s_beatCountEnd     : std_logic;  -- Indicates that data transfer is over
  signal s_cycleCount       : unsigned(7 downto 0);  -- Stores received cycle count 
  signal s_DS1pulse         : std_logic;  -- Pulse on rising and falling edge of DS1

-- CR/CSR related signals
  signal s_CRaddressed   : std_logic;   -- Indicates tha CR is addressed
  signal s_CRAMaddressed : std_logic;   -- Indicates tha CRAM is addressed
  signal s_CSRaddressed  : std_logic;   -- Indicates tha CSR space is addressed
  signal s_CSRdata       : unsigned(7 downto 0);  -- Carries data for CSR write/read
  signal s_CRdataIn      : std_logic_vector(7 downto 0);  -- CR data bus
  signal s_CRAMdataIn    : std_logic_vector(7 downto 0);  -- CRAM data bus
  signal s_bar_written   : std_logic;
-- Control Status Registers
  signal s_CSRarray      : t_CSRarray;  --:= c_csr_array(t_CSRarray'range);                            -- Array of CSR registers

  signal s_BitSetReg    : unsigned(7 downto 0);  -- Bit set register
  signal s_BitClrReg    : unsigned(7 downto 0);  -- Bit clear register
  signal s_UsrBitSetReg : unsigned(7 downto 0);  -- User bit set register  
  signal s_UsrBitClrReg : unsigned(7 downto 0);  -- User bit clear register

  type t_FUNC_32b_array is array (0 to 7) of unsigned(31 downto 0);  -- ADER register array
  type t_FUNC_64b_array is array (0 to 7) of unsigned(63 downto 0);  -- ADER register array

  type t_FUNC_256b_array is array (0 to 7) of unsigned(255 downto 0);  -- ADER register array

  signal s_FUNC_ADER, s_FUNC_ADEM                     : t_FUNC_32b_array;
  signal s_FUNC_AMCAP, s_FUNC_ADER_64, s_FUNC_ADEM_64 : t_FUNC_64b_array;
  signal s_FUNC_XAMCAP                                : t_FUNC_256b_array;


  signal s_GAparityMatch : std_logic;  -- Indicates that geographical address is valid (parity matches)

-- CR image registers
  signal s_BEG_USER_CSR : unsigned(23 downto 0);
  signal s_END_USER_CSR : unsigned(23 downto 0);
  signal s_BEG_USER_CR  : unsigned(23 downto 0);
  signal s_END_USER_CR  : unsigned(23 downto 0);
  signal s_BEG_CRAM     : unsigned(23 downto 0);
  signal s_END_CRAM     : unsigned(23 downto 0);
  signal s_CRregArray   : t_reg52x8bit;  -- CR image register array
--signal c_CRinitAddr: t_reg52x12bit;

-- Misc. signals
  signal s_BERRcondition : std_logic;   -- Condition for asserting BERR 
  signal s_irqIDdata     : unsigned(7 downto 0);  -- IRQ Status/ID data

-- Initialization signals

  signal s_initInProgress  : std_logic;  -- Indicates that initialization procedure is in progress
  signal s_initReadCounter : unsigned(8 downto 0);  -- range 0 to 52;            -- Counts read operations
  signal s_latchCRdata     : std_logic;  -- Stores read CR data
--signal s_WrRd : std_logic;
  type t_initState is (IDLE,            -- Initialization procedure FSM
                       SET_ADDR,
                       GET_DATA,
                       END_INIT
                       );
  signal s_initState                               : t_initState;
  signal s_locAddr_eq_bar, s_addressingType_CR_CSR : std_logic;
  signal s_amcap_match, s_xamcap_match             : std_logic_vector(7 downto 0);
  signal s_CRaddr, s_CRadd_offset, s_CRaddr_base   : unsigned(18 downto 0);
  signal s_latchCRdataPos                          : std_logic_vector(BEG_USER_CR to FUNC_ADEM);
  signal s_DS1pulse_d : std_logic;
  signal transfer_done_flag : std_logic;
begin
  
  s_reset <= (not VME_RST_n_oversampled);  -- or s_CSRarray(BIT_SET_CLR_REG)(7);     -- hardware reset and software reset
  reset_o <= s_reset;

 -- added by pablo for testing. it was:'1' when IACKinProgress_i='1' else s_dtackOE;
  VME_DATA_DIR_o <= s_dataDir;  -- added by pablo for testing. it was:'1' when IACKinProgress_i='1' else s_dataDir;
  VME_DATA_OE_o  <= '0';  -- added by pablo for testing. it was: '1' when IACKinProgress_i='1' else s_dataOE;
  VME_ADDR_DIR_o <= '0';  -- added by pablo for testing. it was:s_addrDir;
  VME_ADDR_OE_o  <= '0';  -- added by pablo for testing. it was:s_addrOE;


-- Type of data transfer decoder

  s_typeOfDataTransferSelect <= s_DSlatched & s_VMEaddrLatched(1) & s_LWORDlatched;

  process(clk_i)
  begin
  if rising_edge(clk_i) then
    case s_typeOfDataTransferSelect is
      when "0101" => s_typeOfDataTransfer <= D08;
      when "1001" => s_typeOfDataTransfer <= D08;
      when "0111" => s_typeOfDataTransfer <= D08;
      when "1011" => s_typeOfDataTransfer <= D08;
      when "0001" => s_typeOfDataTransfer <= D16;
      when "0011" => s_typeOfDataTransfer <= D16;
      when "0000" => s_typeOfDataTransfer <= D32;
      when "0100" => s_typeOfDataTransfer <= UnAl0to2;
      when "1000" => s_typeOfDataTransfer <= UnAl1to3;
      when "0010" => s_typeOfDataTransfer <= UnAl1to2;
      when others => s_typeOfDataTransfer <= TypeError;
    end case;
	 end if;
  end process;

-- Address modifier decoder    

  s_addressingTypeSelect <= VME_AM_oversampled;

  with s_addressingTypeSelect select
    s_addressingType <= A24 when c_A24,
    A24                     when c_A24_S,
    A24_BLT                 when c_A24_BLT,
    A24_MBLT                when c_A24_MBLT,
    A24_LCK                 when c_A24_LCK,
    CR_CSR                  when c_CR_CSR,
    A16                     when c_A16,
    A16_LCK                 when c_A16_LCK,
    A32                     when c_A32,
    A32_BLT                 when c_A32_BLT,
    A32_MBLT                when c_A32_MBLT,
    A32_LCK                 when c_A32_LCK,
    A64                     when c_A64,
    A64_BLT                 when c_A64_BLT,
    A64_MBLT                when c_A64_MBLT,
    A64_LCK                 when c_A64_LCK,
    TWOedge                 when c_TWOedge,
    AM_Error                when others;

  s_transferType <= SINGLE when s_addressingType = A24 or s_addressingType = CR_CSR or s_addressingType = A16 or s_addressingType = A32 or s_addressingType = A64 else
                      BLT  when s_addressingType = A24_BLT or s_addressingType = A32_BLT or s_addressingType = A64_BLT else
                      MBLT when s_addressingType = A24_MBLT or s_addressingType = A32_MBLT or s_addressingType = A64_MBLT else
                      LCK  when s_addressingType = A16_LCK or s_addressingType = A24_LCK or s_addressingType = A32_LCK or s_addressingType = A64_LCK else
                      error;

  s_addrWidth <= "00" when s_addressingType = A16 or s_addressingType = A16_LCK else
                  "01" when s_addressingType = A24 or s_addressingType = A24_BLT or s_addressingType = A24_MBLT or s_addressingType = CR_CSR or s_addressingType = A24_LCK else
                  "10" when s_addressingType = A32 or s_addressingType = A32_BLT or s_addressingType = A32_MBLT or s_addressingType = A32_LCK else
                  "11";
  
  with s_XAM select
    s_XAMtype <= A32_2eVME when x"01",
    A64_2eVME              when x"02",
    A32_2eSST              when x"11",
    A64_2eSST              when x"12",
    XAM_error              when others;
  
  s_2eType <= TWOe_VME when s_XAMtype = A32_2eVME or s_XAMtype = A64_2eVME else
              TWOe_SST;

--Main FSM

  p_VMEmainFSM : process(clk_i)
  begin
    if rising_edge(clk_i) then
      if s_reset = '1' or s_mainFSMreset = '1' or s_blockTransferLimitPulse = '1' or VME_IACKIN_n_oversampled = '0' then  -- FSM is also reset on rising edge of address strobe (which indicates end of transfer) and on rising edge of block transfer limit signal
        --s_dtackOE                <= '0';
        s_dataDir         <= '0';
        s_dataOE          <= '0';
        s_addrDir         <= '0';
        s_addrOE          <= '0';
        s_mainDTACK       <= '1';       -- it was 'Z'
        s_memReq          <= '0';
        s_DSlatch         <= '1';
        --s_WrRd                   <= '0'; 
        s_incrementAddr   <= '0';
        s_resetAddrOffset <= '1';
        s_dataPhase       <= '0';
        s_dataToOutput    <= '0';
        s_dataToAddrBus   <= '0';
        s_transferActive  <= '0';
        s_setLock         <= '0';
        s_cyc             <= '0';
        s_2eLatchAddr     <= "00";
        s_TWOeInProgress  <= '0';
        --s_readFIFO        <= '0';
        s_retry           <= '0';
        s_berr            <= '0';
        s_mainFSMstate    <= IDLE;
		  				  transfer_done_flag <= '0';

      else
        case s_mainFSMstate is
          
          when IDLE =>
--                if IACKinProgress_i='1' then
--                    --s_dtackOE        <= '1';
--                else
--                    --s_dtackOE        <= '0';
--                end if;
            s_dataDir         <= '0';
            s_dataOE          <= '0';
            s_addrDir         <= '0';
            s_addrOE          <= '0';
            s_mainDTACK       <= '1';
            s_memReq          <= '0';
            s_DSlatch         <= '1';
            --s_WrRd               <= '0'; 
            s_incrementAddr   <= '0';
            s_resetAddrOffset <= '1';
            s_dataPhase       <= '0';
            s_dataToOutput    <= '0';
            s_dataToAddrBus   <= '0';
            s_transferActive  <= '0';
            s_setLock         <= '0';
            s_cyc             <= '0';
            s_2eLatchAddr     <= "00";
            s_TWOeInProgress  <= '0';
            --s_readFIFO        <= '0';
            s_retry           <= '0';
            s_berr            <= '0';
				transfer_done_flag <= '0';
            if s_VMEaddrLatch = '1' then  -- If address strobe goes low, check if this slave is addressed
              s_mainFSMstate <= DECODE_ACCESS;
            else
              s_mainFSMstate <= IDLE;
            end if;
            
          when DECODE_ACCESS =>
            --s_dtackOE            <= '0';
            s_dataDir         <= '0';
            s_dataOE          <= '0';
            s_addrDir         <= '0';
            s_addrOE          <= '0';
            s_mainDTACK       <= '1';
            s_memReq          <= '0';
            s_DSlatch         <= '1';
            --s_WrRd               <= '0'; 
            s_incrementAddr   <= '0';
            s_resetAddrOffset <= '0';
            s_dataPhase       <= '0';
            s_dataToOutput    <= '0';
            s_dataToAddrBus   <= '0';
            s_transferActive  <= '0';
            s_setLock         <= '0';
            s_cyc             <= '0';
            s_2eLatchAddr     <= "00";
            s_TWOeInProgress  <= '0';
            --s_readFIFO        <= '0';
            s_retry           <= '0';
            s_berr            <= '0';
				transfer_done_flag <= '0';

            if s_lockSel = '1' then     -- LOCK request
              s_mainFSMstate <= ACKNOWLEDGE_LOCK;
            elsif s_addressingType = TWOedge then  -- start 2e transfer
              s_mainFSMstate <= WAIT_FOR_DS_2e;
            elsif s_confAccess = '1' or (s_cardSel = '1' and stall_i = '0') then  -- If this slave is addressed, start transfer
              s_mainFSMstate <= WAIT_FOR_DS;
            else
              s_mainFSMstate <= DECODE_ACCESS;
            end if;
            
          when WAIT_FOR_DS =>
            --s_dtackOE            <= '0';
            s_dataDir   <= VME_WRITE_n_oversampled;
            s_dataOE    <= '0';
            s_addrDir   <= VME_WRITE_n_oversampled;
            s_addrOE    <= '0';
            s_mainDTACK <= '1';
            s_memReq    <= '0';
            s_DSlatch   <= '1';
            --s_WrRd               <= '0'; 
            s_incrementAddr   <= '0';
            s_resetAddrOffset <= '0';
            s_dataPhase       <= s_dataPhase;
            s_dataToOutput    <= '0';
            s_dataToAddrBus   <= '0';
            s_transferActive  <= '0';
            s_setLock         <= '0';
            s_cyc             <= '1';
            s_2eLatchAddr     <= "00";
            s_TWOeInProgress  <= '0';
            --s_readFIFO        <= '0';
            s_retry           <= '0';
            s_berr            <= '0';
            if VME_DS_n_oversampled /= "11" then
              s_mainFSMstate <= LATCH_DS;
            else
              s_mainFSMstate <= WAIT_FOR_DS;
            end if;
            
          when LATCH_DS =>
            --s_dtackOE            <= '0';
            s_dataDir   <= VME_WRITE_n_oversampled;
            s_dataOE    <= '0';
            s_addrDir   <= VME_WRITE_n_oversampled;
            s_addrOE    <= '0';
            s_mainDTACK <= '1';
            s_memReq    <= '0';
            s_DSlatch   <= '1';
            --s_WrRd               <= '0'; 

            s_incrementAddr   <= '0';
            s_resetAddrOffset <= '0';
            s_dataPhase       <= s_dataPhase;
            s_dataToOutput    <= '0';
            s_dataToAddrBus   <= '0';
            s_transferActive  <= '0';
            s_setLock         <= '0';
            s_cyc             <= '1';
            s_2eLatchAddr     <= "00";
            s_TWOeInProgress  <= '0';
            --s_readFIFO        <= '0';
            s_retry           <= '0';
            s_berr            <= '0';
            s_mainFSMstate    <= CHECK_TRANSFER_TYPE;
            
          when CHECK_TRANSFER_TYPE =>
            --s_dtackOE            <= '0';
            s_dataDir   <= VME_WRITE_n_oversampled;
            s_dataOE    <= '0';
            s_addrDir   <= VME_WRITE_n_oversampled;
            s_addrOE    <= '0';
            s_mainDTACK <= '1';
            s_memReq    <= '0';
            s_DSlatch   <= '0';

            s_incrementAddr   <= '0';
            s_resetAddrOffset <= '0';
            s_dataPhase       <= s_dataPhase;
            s_dataToOutput    <= '0';
            s_dataToAddrBus   <= '0';
            s_transferActive  <= '1';
            s_setLock         <= '0';
            s_cyc             <= '1';
            s_2eLatchAddr     <= "00";
            s_TWOeInProgress  <= '0';
            --s_readFIFO        <= '0';
            s_retry           <= '0';
            s_berr            <= '0';
            if s_transferType = SINGLE or s_transferType = BLT then
              s_mainFSMstate <= MEMORY_REQ;
            s_memReq    <= '1';
            elsif s_transferType = MBLT and s_dataPhase = '0' then
              s_mainFSMstate <= DTACK_LOW;
            s_memReq    <= '0';
            elsif s_transferType = MBLT and s_dataPhase = '1' then
              s_mainFSMstate <= MEMORY_REQ;
            s_memReq    <= '0';
            end if;
            
          when MEMORY_REQ =>
            --s_dtackOE            <= '1';
            s_dataDir         <= VME_WRITE_n_oversampled;
            s_dataOE          <= '1';
            s_addrDir         <= VME_WRITE_n_oversampled;
            s_addrOE          <= '0';
            s_mainDTACK       <= '1';
            s_memReq          <= '0';
            s_DSlatch         <= '0';
            s_incrementAddr   <= '0';
            s_resetAddrOffset <= '0';
            s_dataPhase       <= '1';
            s_dataToOutput    <= '0';
            s_dataToAddrBus   <= '0';
            s_transferActive  <= '1';
            s_setLock         <= '0';
            s_cyc             <= '1';
            s_2eLatchAddr     <= "00";
            s_TWOeInProgress  <= '0';
            --s_readFIFO        <= '0';
            s_retry           <= '0';
            s_berr            <= '0';
            if s_memAck = '1' and s_RW = '0' then
              s_mainFSMstate <= DTACK_LOW;
                                        --s_WrRd               <= '0'; 
            elsif s_memAck = '1' and s_RW = '1' then
              s_mainFSMstate <= DATA_TO_BUS;
                                        --s_WrRd               <= '0'; 
            else
              s_mainFSMstate <= MEMORY_REQ;
                                        --s_WrRd               <= VME_WRITE_n_oversampled; 
            end if;
            
          when DATA_TO_BUS =>
            --s_dtackOE            <= '1';
            s_dataDir         <= VME_WRITE_n_oversampled;
            s_dataOE          <= '1';
            s_addrDir         <= VME_WRITE_n_oversampled;
            s_addrOE          <= '0';
            s_mainDTACK       <= '1';
            s_memReq          <= '0';
            s_DSlatch         <= '0';
            --s_WrRd               <= '0'; 
            s_incrementAddr   <= '0';
            s_resetAddrOffset <= '0';
            s_dataPhase       <= '1';
            s_setLock         <= '0';
            s_cyc             <= '1';
            s_2eLatchAddr     <= "00";
            s_TWOeInProgress  <= '0';
            --s_readFIFO        <= '0';
            s_retry           <= '0';
            if s_transferType = MBLT then
              s_dataToOutput  <= '0';
              s_dataToAddrBus <= '1';
            else
              s_dataToOutput  <= '1';
              s_dataToAddrBus <= '0';
            end if;
            s_transferActive <= '1';
            s_berr           <= '0';
            s_mainFSMstate   <= DTACK_LOW;
          when DTACK_LOW =>
            --s_dtackOE            <= '1';
            s_dataDir   <= VME_WRITE_n_oversampled;
            s_dataOE    <= '1';
            s_addrDir   <= VME_WRITE_n_oversampled;
            s_addrOE    <= '0';
            s_mainDTACK <= '0';
            s_memReq    <= '0';
            s_DSlatch   <= '0';
            --s_WrRd               <= '0'; 

            s_incrementAddr   <= '0';
            s_resetAddrOffset <= '0';
            s_dataPhase       <= s_dataPhase;
            s_dataToOutput    <= s_dataToOutput;
            s_dataToAddrBus   <= s_dataToAddrBus;
            s_transferActive  <= '1';
            s_setLock         <= '0';
            s_cyc             <= '1';
            s_2eLatchAddr     <= "00";
            s_TWOeInProgress  <= '0';
            --s_readFIFO        <= '0';
            s_retry           <= '0';
            s_berr            <= '0';
            if VME_DS_n_oversampled = "11" then
              s_mainFSMstate <= DECIDE_NEXT_CYCLE;
            else
              s_mainFSMstate <= DTACK_LOW;
            end if;
            
          when DECIDE_NEXT_CYCLE =>
            --s_dtackOE            <= '0';
            s_dataDir   <= VME_WRITE_n_oversampled;
            s_dataOE    <= '0';
            s_addrDir   <= VME_WRITE_n_oversampled;
            s_addrOE    <= '0';
            s_mainDTACK <= '1';
            s_memReq    <= '0';
            s_DSlatch   <= '0';
            --s_WrRd               <= '0'; 

            s_incrementAddr   <= '0';
            s_resetAddrOffset <= '0';
            s_dataPhase       <= s_dataPhase;
            s_dataToOutput    <= '0';
            s_dataToAddrBus   <= '0';
            s_transferActive  <= '1';
            s_setLock         <= '0';
            s_cyc             <= '1';
            s_2eLatchAddr     <= "00";
            s_TWOeInProgress  <= '0';
            --s_readFIFO        <= '0';
            s_retry           <= '0';
            s_berr            <= '0';
            if s_transferType = SINGLE then
              s_mainFSMstate <= WAIT_FOR_DS;
            elsif s_transferType = BLT then
              s_mainFSMstate <= INCREMENT_ADDR;
              
            elsif s_transferType = MBLT and s_dataPhase = '0' then
              s_mainFSMstate <= SET_DATA_PHASE;
            elsif s_transferType = MBLT and s_dataPhase = '1' then
              s_mainFSMstate <= INCREMENT_ADDR;
            end if;
            
          when INCREMENT_ADDR =>
            --s_dtackOE            <= '0';
            s_dataDir         <= VME_WRITE_n_oversampled;
            s_dataOE          <= '0';
            s_addrDir         <= VME_WRITE_n_oversampled;
            s_addrOE          <= '0';
            s_mainDTACK       <= '1';
            s_memReq          <= '0';
            s_DSlatch         <= '0';
            --s_WrRd               <= '0'; 
            s_incrementAddr   <= '1';
            s_resetAddrOffset <= '0';
            s_dataPhase       <= s_dataPhase;
            s_dataToOutput    <= '0';
            s_dataToAddrBus   <= '0';
            s_transferActive  <= '1';
            s_setLock         <= '0';
            s_cyc             <= '1';
            s_2eLatchAddr     <= "00";
            s_TWOeInProgress  <= '0';
            --s_readFIFO        <= '0';
            s_retry           <= '0';
            s_berr            <= '0';
            s_mainFSMstate    <= WAIT_FOR_DS;
            
          when SET_DATA_PHASE =>
            --s_dtackOE            <= '0';
            s_dataDir   <= VME_WRITE_n_oversampled;
            s_dataOE    <= '0';
            s_addrDir   <= VME_WRITE_n_oversampled;
            s_addrOE    <= '0';
            s_mainDTACK <= '1';
            s_memReq    <= '0';
            s_DSlatch   <= '0';
            --s_WrRd               <= '0'; 

            s_incrementAddr   <= '0';
            s_resetAddrOffset <= '0';
            s_dataPhase       <= '1';
            s_dataToOutput    <= '0';
            s_dataToAddrBus   <= '0';
            s_transferActive  <= '1';
            s_setLock         <= '0';
            s_cyc             <= '1';
            s_2eLatchAddr     <= "00";
            s_TWOeInProgress  <= '0';
            --s_readFIFO        <= '0';
            s_retry           <= '0';
            s_berr            <= '0';
            s_mainFSMstate    <= WAIT_FOR_DS;
            
          when ACKNOWLEDGE_LOCK =>
            --s_dtackOE            <= '1';
            s_dataDir <= VME_WRITE_n_oversampled;
            s_dataOE  <= '0';
            s_addrDir <= VME_WRITE_n_oversampled;
            s_addrOE  <= '0';
            if VME_DS_n_oversampled /= "11" then
              s_mainDTACK <= '0';
            else
              s_mainDTACK <= '1';
            end if;
            s_memReq  <= '0';
            s_DSlatch <= '0';
            --s_WrRd               <= '0'; 

            s_incrementAddr   <= '0';
            s_resetAddrOffset <= '0';
            s_dataPhase       <= '0';
            s_dataToOutput    <= '0';
            s_dataToAddrBus   <= '0';
            s_transferActive  <= '0';
            s_setLock         <= '1';
            s_cyc             <= '0';
            s_2eLatchAddr     <= "00";
            s_TWOeInProgress  <= '0';
            --s_readFIFO        <= '0';
            s_retry           <= '0';
            s_berr            <= '0';
            s_mainFSMstate    <= ACKNOWLEDGE_LOCK;  -- wait here until AS goes high, which resets the FSM      
            
          when WAIT_FOR_DS_2e =>
            --s_dtackOE            <= '0';
            s_dataDir         <= '0';
            s_dataOE          <= '1';
            s_addrDir         <= '0';
            s_addrOE          <= '1';
            s_mainDTACK       <= '1';
            s_memReq          <= '0';
            s_DSlatch         <= '0';
            --s_WrRd               <= '0'; 
            s_incrementAddr   <= '0';
            s_resetAddrOffset <= '0';
            s_dataPhase       <= '0';
            s_dataToOutput    <= '0';
            s_dataToAddrBus   <= '0';
            s_transferActive  <= '0';
            s_setLock         <= '0';
            s_cyc             <= '0';
            s_2eLatchAddr     <= "00";
            s_TWOeInProgress  <= '0';
            --s_readFIFO        <= '0';
            s_retry           <= '0';
            s_berr            <= '0';
            if VME_DS_n_oversampled(0) = '0' then
              s_mainFSMstate <= ADDR_PHASE_1;
            end if;
            
          when ADDR_PHASE_1 =>
            --s_dtackOE            <= '0';
            s_dataDir   <= '0';
            s_dataOE    <= '1';
            s_addrDir   <= '0';
            s_addrOE    <= '1';
            s_mainDTACK <= '1';
            --s_WrRd               <= '0'; 

            s_memReq          <= '0';
            s_DSlatch         <= '0';
            s_incrementAddr   <= '0';
            s_resetAddrOffset <= '0';
            s_dataPhase       <= '0';
            s_dataToOutput    <= '0';
            s_dataToAddrBus   <= '0';
            s_transferActive  <= '0';
            s_setLock         <= '0';
            s_cyc             <= '0';
            s_2eLatchAddr     <= "01";
            s_TWOeInProgress  <= '0';
            --s_readFIFO        <= '0';
            s_retry           <= '0';
            s_berr            <= '0';
            s_mainFSMstate    <= DECODE_ACCESS_2e;
            
          when DECODE_ACCESS_2e =>
            --s_dtackOE            <= '1';
            s_dataDir   <= '0';
            s_dataOE    <= '1';
            s_addrDir   <= '0';
            s_addrOE    <= '1';
            s_mainDTACK <= '1';
            --s_WrRd               <= '0'; 

            s_memReq          <= '0';
            s_DSlatch         <= '0';
            s_incrementAddr   <= '0';
            s_resetAddrOffset <= '0';
            s_dataPhase       <= '0';
            s_dataToOutput    <= '0';
            s_dataToAddrBus   <= '0';
            s_transferActive  <= '0';
            s_setLock         <= '0';
            s_cyc             <= '0';
            s_2eLatchAddr     <= "00";
            s_TWOeInProgress  <= '0';
            --s_readFIFO        <= '0';
            s_retry           <= '0';
            if s_XAMtype = XAM_error then
              s_berr <= '1';
            else
              s_berr <= '0';
            end if;
            if s_cardSel = '1' then  -- if module is selected, proceed with DTACK, else wait here until FSM reset by AS going high            
              s_mainFSMstate <= DTACK_PHASE_1;
            end if;
            
          when DTACK_PHASE_1 =>
            --s_dtackOE            <= '1';
            s_dataDir   <= '0';
            s_dataOE    <= '1';
            s_addrDir   <= '0';
            s_addrOE    <= '1';
            s_mainDTACK <= '0';
            --s_WrRd               <= '0'; 

            s_memReq          <= '0';
            s_DSlatch         <= '0';
            s_incrementAddr   <= '0';
            s_resetAddrOffset <= '0';
            s_dataPhase       <= '0';
            s_dataToOutput    <= '0';
            s_dataToAddrBus   <= '0';
            s_transferActive  <= '0';
            s_setLock         <= '0';
            s_cyc             <= '0';
            s_2eLatchAddr     <= "00";
            s_TWOeInProgress  <= '0';
            --s_readFIFO        <= '0';
            s_retry           <= '0';
            s_berr            <= s_berr;
            if VME_DS_n_oversampled(0) = '1' and s_berr = '0' then
              s_mainFSMstate <= ADDR_PHASE_2;
            elsif VME_DS_n_oversampled(0) = '1' and s_berr = '1' then
              s_mainFSMstate <= TWOe_RELEASE_DTACK;
            end if;
            
          when ADDR_PHASE_2 =>
            --s_dtackOE            <= '1';
            s_dataDir         <= '0';
            s_dataOE          <= '1';
            s_addrDir         <= '0';
            s_addrOE          <= '1';
            s_mainDTACK       <= '0';
            --s_WrRd               <= '0'; 
            s_memReq          <= '0';
            s_DSlatch         <= '0';
            s_incrementAddr   <= '0';
            s_resetAddrOffset <= '0';
            s_dataPhase       <= '0';
            s_dataToOutput    <= '0';
            s_dataToAddrBus   <= '0';
            s_transferActive  <= '0';
            s_setLock         <= '0';
            s_cyc             <= '0';
            s_2eLatchAddr     <= "10";
            s_TWOeInProgress  <= '0';
            --s_readFIFO        <= '0';
            s_retry           <= '0';
            s_berr            <= '0';
            s_mainFSMstate    <= DTACK_PHASE_2;
            
          when DTACK_PHASE_2 =>
            --s_dtackOE            <= '1';
            s_dataDir   <= '0';
            s_dataOE    <= '1';
            s_addrDir   <= '0';
            s_addrOE    <= '1';
            s_mainDTACK <= '1';
            --s_WrRd               <= '0'; 

            s_memReq          <= '0';
            s_DSlatch         <= '0';
            s_incrementAddr   <= '0';
            s_resetAddrOffset <= '0';
            s_dataPhase       <= '0';
            s_dataToOutput    <= '0';
            s_dataToAddrBus   <= '0';
            s_transferActive  <= '0';
            s_setLock         <= '0';
            s_cyc             <= '0';
            s_2eLatchAddr     <= "00";
            s_TWOeInProgress  <= '0';
            --s_readFIFO        <= '0';
            s_retry           <= '0';
            s_berr            <= '0';
            if VME_DS_n_oversampled(0) = '0' then
              s_mainFSMstate <= ADDR_PHASE_3;
            end if;
            
          when ADDR_PHASE_3 =>
            --s_dtackOE            <= '1';
            s_dataDir         <= '0';
            s_dataOE          <= '1';
            s_addrDir         <= '0';
            s_addrOE          <= '1';
            s_mainDTACK       <= '1';
            --s_WrRd               <= '0'; 
            s_memReq          <= '0';
            s_DSlatch         <= '0';
            s_incrementAddr   <= '0';
            s_resetAddrOffset <= '0';
            s_dataPhase       <= '0';
            s_dataToOutput    <= '0';
            s_dataToAddrBus   <= '0';
            s_transferActive  <= '0';
            s_setLock         <= '0';
            s_cyc             <= '0';
            s_2eLatchAddr     <= "11";
            s_TWOeInProgress  <= '0';
            --s_readFIFO        <= '0';
            if stall_i = '1' then
              s_retry <= '1';
            else
              s_retry <= '0';
            end if;
            s_berr <= '0';
  --          if WBbusy_i = '0' then -- Removed by pablo 
              s_mainFSMstate <= DTACK_PHASE_3;
   --        end if;
            
          when DTACK_PHASE_3 =>
            --s_dtackOE            <= '1';
            s_dataDir         <= '0';
            s_dataOE          <= '1';
            s_addrDir         <= '0';
            s_addrOE          <= '1';
            s_mainDTACK       <= '0';
            s_DSlatch         <= '0';
            s_incrementAddr   <= '0';
            s_resetAddrOffset <= '0';
            s_dataPhase       <= '0';
            s_dataToOutput    <= '0';
            s_dataToAddrBus   <= '0';
            s_transferActive  <= '0';
            s_setLock         <= '0';
            s_cyc             <= '1';
            s_2eLatchAddr     <= "00";
            s_TWOeInProgress  <= '0';
            --s_readFIFO        <= '0';
            s_retry           <= s_retry;
            s_berr            <= '0';
				
				
            if s_RW = '0' and s_retry = '0' then
              s_mainFSMstate <= TWOe_FIFO_WRITE;
              s_memReq          <= '0';
            elsif s_RW = '1' and s_retry = '0' then
				
				if s_2eType = TWOe_VME then			
              s_mainFSMstate <= TWOe_FIFO_WAIT_READ;
              s_memReq          <= '0';
				  else
              s_mainFSMstate <= TWOe_FIFO_WAIT_READ;
              s_memReq          <= '0';
				  end if;
				

            --s_WrRd               <= VME_WRITE_n_oversampled;                                                                                           
            elsif VME_DS_n_oversampled(0) = '1' and s_retry = '1' then
              s_mainFSMstate <= TWOe_RELEASE_DTACK;
              s_memReq          <= '0';
				else
              s_memReq          <= '0';				
            end if;
            
          when TWOe_FIFO_WRITE =>
            --s_dtackOE            <= '1';
            s_dataDir         <= '0';
            s_dataOE          <= '1';
            s_addrDir         <= '0';
            s_addrOE          <= '1';
            s_mainDTACK       <= s_mainDTACK;
            s_DSlatch         <= '0';
            --s_WrRd               <= '0';                                                                                               
            s_incrementAddr   <= '0';
            s_resetAddrOffset <= '0';
            s_dataPhase       <= '0';
            s_dataToOutput    <= '0';
            s_dataToAddrBus   <= '0';
            s_transferActive  <= '0';
            s_setLock         <= '0';
            s_cyc             <= '1';
            s_2eLatchAddr     <= "00";
            s_TWOeInProgress  <= '1';
            --s_readFIFO        <= '0';
            s_retry           <= '0';
            s_berr            <= '0';
				
            if s_DS1pulse = '1' and s_2eType = TWOe_VME then
            s_memReq          <= '1';
            elsif s_DS1pulse = '1' then --VME_DS_n_oversampled(0) = '1' then
            s_memReq          <= '1';
				else
            s_memReq          <= '0';
            end if;
				
            if s_DS1pulse = '1' and s_2eType = TWOe_VME  then
              s_mainFSMstate <= TWOe_TOGGLE_DTACK;
            elsif VME_DS_n_oversampled(0) = '1' then
              s_mainFSMstate <= TWOe_RELEASE_DTACK;
            end if;
            
          when TWOe_TOGGLE_DTACK =>
            --s_dtackOE            <= '1';
            s_dataDir         <= s_dataDir;
            s_dataOE          <= '1';
            s_addrDir         <= s_addrDir;
            s_addrOE          <= '1';
            s_memReq          <= '0'; 
            s_DSlatch         <= '0';
            --s_WrRd               <= '0';                                                                                               
            s_incrementAddr   <= '0';
            s_resetAddrOffset <= '0';
            s_dataPhase       <= '0';
            s_dataToOutput    <= '0';
            s_dataToAddrBus   <= '0';
            s_transferActive  <= '0';
            s_setLock         <= '0';
            s_cyc             <= '1';
            s_2eLatchAddr     <= "00";
            s_TWOeInProgress  <= '1';
            --s_readFIFO        <= '0';
            s_retry           <= '0';
            s_berr            <= '0';
--				IF s_2eType = TWOe_SST

            if s_RW = '0' then
              s_mainFSMstate <= TWOe_FIFO_WRITE;
				  s_mainDTACK <= not s_mainDTACK;
            elsif s_2eType = TWOe_SST then
              s_mainFSMstate <= TWOe_CHECK_BEAT;	
				  s_mainDTACK       <= s_mainDTACK; --not s_mainDTACK;  
				else				
              s_mainFSMstate <= TWOe_WAIT_FOR_DS1;
				  s_mainDTACK <= not s_mainDTACK;
            end if;

          when TWOe_WAIT_FOR_DS1 =>
            --s_dtackOE            <= '1';
            s_dataDir   <= s_dataDir;
            s_dataOE    <= '1';
            s_addrDir   <= s_addrDir;
            s_addrOE    <= '1';
            s_mainDTACK <= s_mainDTACK;
            s_memReq    <= '0';
            s_DSlatch   <= '0';
            --s_WrRd               <= '0';                                                                                               

            s_incrementAddr   <= '0';
            s_resetAddrOffset <= '0';
            s_dataPhase       <= '0';
            s_dataToOutput    <= '0';
            s_dataToAddrBus   <= '1';
            s_transferActive  <= '0';
            s_setLock         <= '0';
            s_cyc             <= '1';
            s_2eLatchAddr     <= "00";
            s_TWOeInProgress  <= '1';
            --s_readFIFO        <= '0';
            s_retry           <= '0';
            s_berr            <= '0';
            if (s_DS1pulse = '1' and s_2eType = TWOe_VME) or s_2eType = TWOe_SST then
              s_mainFSMstate <= TWOe_CHECK_BEAT;
            end if;
				
          when TWOe_FIFO_WAIT_READ =>
            --s_dtackOE            <= '1';
            s_dataDir         <= '1';
            s_dataOE          <= '1';
            s_addrDir         <= '1';
            s_addrOE          <= '1';
            s_mainDTACK       <= s_mainDTACK;
            s_DSlatch         <= '0';
            --s_WrRd               <= '0';                                                                                               
            s_incrementAddr   <= '0';
            s_resetAddrOffset <= '0';
            s_dataPhase       <= '0';
            s_dataToOutput    <= '0';
            s_dataToAddrBus   <= '0';
            s_transferActive  <= '0';
            s_setLock         <= '0';
            s_cyc             <= '1';
            s_2eLatchAddr     <= "00";
            s_TWOeInProgress  <= '1';
            --s_readFIFO        <= '0';
            s_retry           <= '0';
            s_berr            <= '0';
 --           if readFIFOempty_i = '0' then  --and s_2eType=TWOe_SST then

            if stall_i = '0' then --and s_2eType=TWOe_SST then
               s_mainFSMstate <= TWOe_FIFO_READ;
            end if;
				
				s_memReq          <= not stall_i;  -- access to the wb_dma

				
          when TWOe_FIFO_READ =>
            --s_dtackOE            <= '1';
            s_dataDir         <= '1';
            s_dataOE          <= '1';
            s_addrDir         <= '1';
            s_addrOE          <= '1';
            s_mainDTACK       <= s_mainDTACK;
            s_memReq          <= '0';
            s_DSlatch         <= '0';
            s_incrementAddr   <= '0';
            s_resetAddrOffset <= '0';
            s_dataPhase       <= '0';
            s_dataToOutput    <= '0';
            s_dataToAddrBus   <= memAckWB_i;
            s_transferActive  <= '0';
            s_setLock         <= '0';
            s_cyc             <= '1';
            s_2eLatchAddr     <= "00";
            s_TWOeInProgress  <= '1';
            --s_readFIFO        <= '1';
            s_retry           <= '0';
            s_berr            <= '0';
				if memAckWB_i = '1' then 
            s_mainFSMstate    <= TWOe_TOGGLE_DTACK;
            end if;
				if transfer_done_i = '1' then
				transfer_done_flag <= '1';
				end if;
          when TWOe_CHECK_BEAT =>
            --s_dtackOE            <= '1';
            s_dataDir         <= '1';
            s_dataOE          <= '1';
            s_addrDir         <= '1';
            s_addrOE          <= '1';
--            s_mainDTACK       <= not s_mainDTACK; -- s_mainDTACK;
            s_mainDTACK       <= s_mainDTACK;
            s_memReq          <= '0';
            s_DSlatch         <= '0';
            s_incrementAddr   <= '0';
            s_resetAddrOffset <= '0';
            s_dataPhase       <= '0';
            s_dataToOutput    <= '0';
            s_dataToAddrBus   <= '0';
            s_transferActive  <= '0';
            s_setLock         <= '0';
            s_cyc             <= '1';
            s_2eLatchAddr     <= "00";
            s_TWOeInProgress  <= '1';
            --s_readFIFO        <= '0';
            s_retry           <= '0';
            s_berr            <= '0';
            if transfer_done_flag = '0' then
              s_mainFSMstate <= TWOe_FIFO_WAIT_READ;
				  transfer_done_flag <= '0';
            else
              s_mainFSMstate <= TWOe_END_1;
				  transfer_done_flag <= '0';
            end if;
				s_memReq          <= '0';--not stall_i;  -- access to the wb_dma

          when TWOe_RELEASE_DTACK =>
            --s_dtackOE            <= '0';
            s_dataDir         <= '0';
            s_dataOE          <= '0';
            s_addrDir         <= '0';
            s_addrOE          <= '0';
            s_mainDTACK       <= '1';
            s_memReq          <= '0';
            s_DSlatch         <= '0';
            s_incrementAddr   <= '0';
            s_resetAddrOffset <= '0';
            s_dataPhase       <= '0';
            s_dataToOutput    <= '0';
            s_dataToAddrBus   <= '0';
            s_transferActive  <= '0';
            s_setLock         <= '0';
            s_cyc             <= '0';
            s_2eLatchAddr     <= "00";
            s_TWOeInProgress  <= '0';
            --s_readFIFO        <= '0';
            s_retry           <= '0';
            s_berr            <= '0';
            s_mainFSMstate    <= TWOe_RELEASE_DTACK;
            
          when TWOe_END_1 =>
            --s_dtackOE            <= '1';
            s_dataDir         <= '0';
            s_dataOE          <= '0';
            s_addrDir         <= '0';
            s_addrOE          <= '0';
            s_mainDTACK       <= s_mainDTACK;
            s_memReq          <= '0';
            s_DSlatch         <= '0';
            s_incrementAddr   <= '0';
            s_resetAddrOffset <= '0';
            s_dataPhase       <= '0';
            s_dataToOutput    <= '0';
            s_dataToAddrBus   <= '0';
            s_transferActive  <= '0';
            s_setLock         <= '0';
            s_cyc             <= '0';
            s_2eLatchAddr     <= "00";
            s_TWOeInProgress  <= '0';
            --s_readFIFO        <= '0';
            s_retry           <= '1';
            s_berr            <= '0';
            s_mainFSMstate    <= TWOe_END_2;
            transfer_done_flag <= '0';
          when TWOe_END_2 =>
            --s_dtackOE            <= '1';
            s_dataDir         <= '0';
            s_dataOE          <= '0';
            s_addrDir         <= '0';
            s_addrOE          <= '0';
            s_mainDTACK       <= s_mainDTACK;
            s_memReq          <= '0';
            s_DSlatch         <= '0';
            s_incrementAddr   <= '0';
            s_resetAddrOffset <= '0';
            s_dataPhase       <= '0';
            s_dataToOutput    <= '0';
            s_dataToAddrBus   <= '0';
            s_transferActive  <= '0';
            s_setLock         <= '0';
            s_cyc             <= '0';
            s_2eLatchAddr     <= "00";
            s_TWOeInProgress  <= '0';
            --s_readFIFO        <= '0';
            s_retry           <= '1';
            s_berr            <= '1';
            if VME_DS_n_oversampled = "11" then
              s_mainFSMstate <= TWOe_RELEASE_DTACK;
            end if;
            
          when others =>
            --s_dtackOE            <= '0';
            s_dataDir         <= '0';
            s_dataOE          <= '0';
            s_addrDir         <= '0';
            s_addrOE          <= '1';
            s_mainDTACK       <= '1';
            s_memReq          <= '0';
            s_DSlatch         <= '0';
            s_incrementAddr   <= '0';
            s_resetAddrOffset <= '1';
            s_dataPhase       <= '0';
            s_dataToOutput    <= '0';
            s_dataToAddrBus   <= '0';
            s_transferActive  <= '0';
            s_setLock         <= '0';
            s_cyc             <= '0';
            s_2eLatchAddr     <= "00";
            s_TWOeInProgress  <= '0';
            --s_readFIFO        <= '0';
            s_retry           <= '0';
            s_berr            <= '0';
            s_mainFSMstate    <= IDLE;
            transfer_done_flag <= '0';
           
        end case;
      end if;
    end if;
  end process;

  cyc_o <= s_cyc and s_cardSel and not (s_transferActive and s_BERRcondition);

--  FIFOwren_o       <= s_DS1pulse and s_TWOeInProgress and not s_RW;
--  FIFOrden_o       <= s_readFIFO;
  TWOeInProgress_o <= s_TWOeInProgress;


-- RETRY driver

p_RETRYdriver: process(clk_i)
begin
    if rising_edge(clk_i) then
        if rty_i='1' or s_retry='1' then
            VME_RETRY_n_o <= '1';
                              VME_RETRY_OE_n_o <= '1';
        else
  VME_RETRY_n_o    <= '0';
  VME_RETRY_OE_n_o <= '0';
        end if;
    end if;
end process;


-- BERR driver 

p_BERRdriver: process(clk_i)
begin
    if rising_edge(clk_i) then
        s_berr_1 <= s_berr;    
        s_berr_2 <= s_berr and s_berr_1;
        if (s_transferActive='1' and s_BERRcondition='1') or s_berr_2='1' then
  VME_BERR_o <= '1';
        else
            VME_BERR_o <= '0';
        end if;
    end if;
end process;

  process(clk_i)
  begin
    if rising_edge(clk_i) then
      if s_transferType = error or s_typeOfDataTransfer = TypeError or err_i = '1' then
        if (s_CRaddressed = '1' and s_confAccess = '1' and s_RW = '0') or (s_CSRaddressed = '1' and s_CRaddressed = '1') then
          if (s_CRAMaddressed = '1' and s_CRaddressed = '1') or (s_CRAMaddressed = '1' and s_CSRaddressed = '1') then
            s_BERRcondition <= '1';
          else
            s_BERRcondition <= '0';
          end if;
        else
          s_BERRcondition <= '0';
        end if;
      else
        s_BERRcondition <= '0';
      end if;
    end if;
  end process;

-- LOCK driver

  p_LOCKdriver : process(clk_i)
  begin
    if rising_edge(clk_i) then
      if s_reset = '1' then
        s_lock <= '0';
      elsif VME_BBSY_n_oversampled = '1' then
        s_lock <= '0';
      elsif s_setLock = '1' then
        s_lock <= '1';
      else
        s_lock <= s_lock;
      end if;
    end if;
  end process;

  lock_o <= s_lock;


-- DTACK multiplexing

--  p_DTACKmux : process(clk_i)
--  begin
--    if rising_edge(clk_i) then
----        if IACKinProgress_i='1' then
----            VME_DTACK_n_o <= irqDTACK_i;
----                              s_dtackOE <= not irqDTACK_i;
----        els
--      if s_mainDTACK = '0' then
--        VME_DTACK_n_o <= '0';
--        s_dtackOE     <= '1';
--      else
--        VME_DTACK_n_o <= '1';
--        s_dtackOE     <= '0';
--      end if;
--    end if;
--  end process;
process(clk_i)
begin
   if rising_edge(clk_i) then
      VME_DTACK_n_o <= s_mainDTACK;
		VME_DTACK_OE_o <= s_dataOE; 
   end if;
end process;

--s_dtackOE <= not s_mainDTACK;

-- Bidirectional signal handling 

  s_VMEaddrInput <= unsigned(VME_ADDR_oversampled);
  s_LWORDinput   <= VME_LWORD_n_oversampled;
  s_VMEdataInput <= unsigned(VME_DATA_oversampled);

  p_ADDRmux : process(clk_i)
  begin
    if rising_edge(clk_i) then
--      if s_dataToAddrBus = '1' and s_TWOeInProgress = '1' then
--        VME_ADDR_b_o    <= FIFOdata_i(63 downto 33);
--        VME_LWORD_n_b_o <= FIFOdata_i(32);
--      els
      if s_dataToAddrBus = '1' then
        VME_ADDR_b_o    <= std_logic_vector(s_locData(63 downto 33));
        VME_LWORD_n_b_o <= s_locData(32);
--      else
--        VME_ADDR_b_o    <= (others => '0');
--        VME_LWORD_n_b_o <= '0';
      end if;
    end if;
  end process;

  p_DATAmux : process(clk_i)
  begin
    if rising_edge(clk_i) then
--      if s_dataToAddrBus = '1' and s_TWOeInProgress = '1' then
--        VME_DATA_b_o <= FIFOdata_i(31 downto 0);
--      els
      if s_dataToAddrBus = '1' or s_dataToOutput = '1' then
        VME_DATA_b_o <= std_logic_vector(s_locData(31 downto 0));
      elsif IDtoData_i = '1' then
        VME_DATA_b_o <= "------------------------" & std_logic_vector(s_irqIDdata);
 --     else
 --      VME_DATA_b_o <= (others => '0');
      end if;
    end if;
  end process;

  s_irqIDdata <= s_CSRarray(IRQ_ID);


-- Local address & AM latching 

  p_addrLatching : process(clk_i)
  begin
    if rising_edge(clk_i) then
      if s_reset = '1' then
        s_VMEaddrLatched <= (others => '0');
        s_LWORDlatched   <= '0';
        s_AMlatched      <= (others => '0');
      else
        if s_VMEaddrLatch = '1' then  -- Latching on falling edge of VME_AS_n_i
          s_VMEaddrLatched <= unsigned(s_VMEdataInput) & s_VMEaddrInput;
          s_LWORDlatched   <= s_LWORDinput;
          s_AMlatched      <= VME_AM_oversampled;
        else
          s_VMEaddrLatched <= s_VMEaddrLatched;
          s_LWORDlatched   <= s_LWORDlatched;
          s_AMlatched      <= s_AMlatched;
        end if;
      end if;
    end if;
  end process;


-- Data strobe latching

  p_DSlatching : process(clk_i)
  begin
    if rising_edge(clk_i) then
      if s_DSlatch = '1' then
        s_DSlatched <= VME_DS_n_oversampled;
      else
        s_DSlatched <= s_DSlatched;
      end if;
    end if;
  end process;


-- 2e address phase latching 

  p_2eAddrLatch : process(clk_i)
  begin
    if rising_edge(clk_i) then
      if s_reset = '1' or s_mainFSMreset = '1' then
        s_phase1addr <= (others => '0');
        s_phase2addr <= (others => '0');
        s_phase3addr <= (others => '0');
      else
        case s_2eLatchAddr is
          when "01" => s_phase1addr <= s_VMEdataInput & s_VMEaddrInput & s_LWORDinput;
                          s_phase2addr <= s_phase2addr;
                          s_phase3addr <= s_phase3addr;
          when "10" => s_phase2addr <= s_VMEdataInput & s_VMEaddrInput & s_LWORDinput;
                          s_phase1addr <= s_phase1addr;
                          s_phase3addr <= s_phase3addr;
          when "11" => s_phase3addr <= s_VMEdataInput & s_VMEaddrInput & s_LWORDinput;
                          s_phase1addr <= s_phase1addr;
                          s_phase2addr <= s_phase2addr;
          when others => s_phase1addr <= s_phase1addr;
                          s_phase2addr <= s_phase2addr;
                          s_phase3addr <= s_phase3addr;
        end case;
      end if;
    end if;
  end process;

  s_XAM        <= s_phase1addr(7 downto 0);
  s_cycleCount <= unsigned(s_phase2addr(15 downto 8));


--s_beatCount      <= ((s_cycleCount)&'0') when s_XAMtype=A32_2eVME or s_XAMtype=A64_2eVME else
--                    ('0'&(s_cycleCount));

process(s_cycleCount,s_beatCount,s_XAMtype, s_transferType)
begin
if ((s_XAMtype = A32_2eVME) or (s_XAMtype = A64_2eVME))  then 
  s_beatCount <= (resize(s_cycleCount*2, s_beatCount'length));
elsif s_transferType = SINGLE then 
  s_beatCount <= (to_unsigned(1, s_beatCount'length));
else
  s_beatCount <= ('0'&(s_cycleCount));
end if;  
end process;
--

  psize_o <= std_logic_vector(s_beatCount(7 downto 0));
-- Beat counter

--  p_FIFObeatCounter : process(clk_i)
--  begin
--    if rising_edge(clk_i) then
--      if s_reset = '1' or s_resetAddrOffset = '1' then
--        s_runningBeatCount <= (others => '0');
--      elsif s_readFIFO = '1' then
--        s_runningBeatCount <= s_runningBeatCount + 1;
--      else
--        s_runningBeatCount <= s_runningBeatCount;
--      end if;
--    end if;
--  end process;

 -- s_beatCountEnd <= '0' when s_runningBeatCount < s_beatCount else '1';


-- Local address mapping

  s_locAddrBeforeOffset(63 downto 1) <= x"000000000000" & s_VMEaddrLatched(15 downto 1) when s_addrWidth = "00" else
                                          x"0000000000" & s_VMEaddrLatched(23 downto 1) when s_addrWidth = "01" else
                                          x"00000000" & s_VMEaddrLatched(31 downto 1)   when s_addrWidth = "10" else
                                          s_VMEaddrLatched(63 downto 1);
  
  s_locAddrBeforeOffset(0) <= '0' when (s_DSlatched(1) = '0' and s_DSlatched(0) = '1') else
                              '1' when (s_DSlatched(1) = '1' and s_DSlatched(0) = '0') else
                              '0';
  
  s_locAddr2e <= s_phase1addr(63 downto 8) & s_phase2addr(7 downto 0);
  process(clk_i)
  begin
    if rising_edge(clk_i) then
      if s_typeOfDataTransfer = UnAl1to2 then
        s_locAddr <= s_locAddrBeforeOffset - 1 + s_addrOffset;
      elsif s_addressingType = TWOedge then
        s_locAddr <= s_locAddr2e + s_addrOffset;
      else
        s_locAddr <= s_locAddrBeforeOffset + s_addrOffset;
      end if;
    end if;
  end process;
--  s_locAddr <=    s_locAddrBeforeOffset - 1 + s_addrOffset when s_typeOfDataTransfer=UnAl1to2 else        -- exception for UnAl1to2
--                  s_locAddr2e + s_addrOffset when s_addressingType=TWOedge else
--                  s_locAddrBeforeOffset + s_addrOffset;

  locAddr_o <= std_logic_vector(s_locAddr);


-- Local address incrementing 

  p_addrIncrementing : process(clk_i)
  begin
    if rising_edge(clk_i) then
      
      s_incrementAddr_1    <= s_incrementAddr;
      s_incrementAddrPulse <= s_incrementAddr and (not s_incrementAddr_1);

      if s_resetAddrOffset = '1' or s_reset = '1' then
        s_addrOffset <= (others => '0');
      elsif s_incrementAddrPulse = '1' then
        if s_addressingType = TWOedge then
          s_addrOffset <= s_addrOffset + 8;
        elsif s_typeOfDataTransfer = D08 then
          if s_locAddrBeforeOffset(0) = '1' then
            s_addrOffset <= s_addrOffset + 2;
          else
            s_addrOffset <= s_addrOffset;
          end if;
        elsif s_typeOfDataTransfer = D16 then
          s_addrOffset <= s_addrOffset + 2;
        elsif s_typeOfDataTransfer = D32 then
          if s_transferType = MBLT then
            s_addrOffset <= s_addrOffset + 8;
          else
            s_addrOffset <= s_addrOffset + 4;
          end if;
        else
          s_addrOffset <= s_addrOffset;
        end if;
      end if;
    end if;
  end process;

  s_blockTransferLimit <= s_addrOffset(8);  -- If address offset overflows, limit is reached and main FSM will be reset     

  BlockTransferLimitFSMresetPulse : RisEdgeDetection
    port map(
      sig_i     => s_blockTransferLimit,
      clk_i     => clk_i,
      RisEdge_o => s_blockTransferLimitPulse
      );


-- Memory mapping

  p_memoryMapping : process(clk_i)
  begin
    if rising_edge(clk_i) then
      case s_RW is
        -- Read cycles
        when '1' =>
          case s_typeOfDataTransfer is
            when D08 =>
              case s_DSlatched(1) is
                when '0' =>             -- D08(E)
                  s_sel                   <= "00000001";
                when others =>          -- D08(O)
                  s_sel                  <= "00000001";
              end case;
            when D16 =>                 -- D16
              s_sel                   <= "00000011";
            when D32 =>
              case s_transferType is
                when MBLT =>            -- D64
                  s_sel                  <= "11111111";
                when others =>          -- D32
                  s_sel                   <= "00001111";
              end case;
            when UnAl0to2 =>            -- Unaligned transfer byte(0-2)
              s_sel                   <= "00000111";
            when UnAl1to3 =>            -- Unaligned transfer byte(1-3)
              s_sel                   <= "00000111";
            when UnAl1to2 =>            -- Unaligned transfer byte(1-2)
              s_sel                   <= "00000011";
            when others =>
              s_sel                  <= "11111111";
          end case;

        -- Write cycles
        when others =>
          case s_typeOfDataTransfer is
            when D08 =>
              case s_DSlatched(1) is
                when '0' =>             -- D08(E)
                  s_locDataIn(7 downto 0) <= s_VMEdataInput(15 downto 8);
                  s_sel                   <= "00000001";
                when others =>          -- D08(O)
                  s_locDataIn(7 downto 0) <= s_VMEdataInput(7 downto 0);
                  s_sel                   <= "00000001";
              end case;
            when D16 =>                 -- D16
              s_locDataIn(15 downto 0) <= s_VMEdataInput(15 downto 0);
              s_sel                    <= "00000011";
            when D32 =>
              case s_transferType is
                when MBLT =>            -- D64
                  s_locDataIn(31 downto 0)  <= s_VMEdataInput(31 downto 0);
                  s_locDataIn(63 downto 32) <= s_VMEaddrInput(31 downto 1) & s_LWORDinput;
                  s_sel                     <= "11111111";
                when others =>          -- D32
                  s_locDataIn(31 downto 0) <= s_VMEdataInput(31 downto 0);
                  s_sel                    <= "00001111";
              end case;
            when UnAl0to2 =>            -- Unaligned transfer byte(0-2)
              s_locDataIn(23 downto 0) <= s_VMEdataInput(31 downto 8);
              s_sel                    <= "00000111";
            when UnAl1to3 =>            -- Unaligned transfer byte(1-3)
              s_locDataIn(23 downto 0) <= s_VMEdataInput(23 downto 0);
              s_sel                    <= "00000111";
            when UnAl1to2 =>            -- Unaligned transfer byte(1-2)
              s_locDataIn(15 downto 0) <= s_VMEdataInput(23 downto 8);
              s_sel                    <= "00000011";
            when others =>
              s_locDataIn(31 downto 0)  <= s_VMEdataInput(31 downto 0);
              s_locDataIn(63 downto 32) <= s_VMEaddrInput(31 downto 1) & s_LWORDinput;
              s_sel                     <= "11111111";
          end case;
          
      end case;
    end if;
  end process;

  P_dout_byte_swap : process(clk_i)-- s_locDataOut, s_typeOfDataTransfer, s_DSlatched, s_transferType)
  begin
    if rising_edge(clk_i) then
  --   s_locData <= s_locDataOut;

          case s_typeOfDataTransfer is
            when D08 =>
              case s_DSlatched(1) is
                when '0' =>             -- D08(E)
                  s_locData(15 downto 8)  <= s_locDataOut(7 downto 0);
                  s_locData(63 downto 16) <= (others => '0');
                  s_locData(7 downto 0)   <= (others => '0');
                when others =>          -- D08(O)
                  s_locData(7 downto 0)  <= s_locDataOut(7 downto 0);
                  s_locData(63 downto 8) <= (others => '0');
              end case;
            when D16 =>                 -- D16
              s_locData(15 downto 0)  <= s_locDataOut(15 downto 0);
              s_locData(63 downto 16) <= (others => '0');
            when D32 =>
              case s_transferType is
                when MBLT =>            -- D64
                  s_locData(63 downto 0) <= s_locDataOut(63 downto 0);
                when others =>          -- D32
                  s_locData(31 downto 0)  <= s_locDataOut(31 downto 0);
                  s_locData(63 downto 32) <= (others => '0');
              end case;
            when UnAl0to2 =>            -- Unaligned transfer byte(0-2)
              s_locData(31 downto 8)  <= s_locDataOut(23 downto 0);
              s_locData(63 downto 32) <= (others => '0');
              s_locData(7 downto 0)   <= (others => '0');
            when UnAl1to3 =>            -- Unaligned transfer byte(1-3)
              s_locData(23 downto 0)  <= s_locDataOut(23 downto 0);
              s_locData(63 downto 24) <= (others => '0');
            when UnAl1to2 =>            -- Unaligned transfer byte(1-2)
              s_locData(23 downto 8)  <= s_locDataOut(15 downto 0);
              s_locData(63 downto 24) <= (others => '0');
              s_locData(7 downto 0)   <= (others => '0');
            when others =>
              s_locData(63 downto 0) <= s_locDataOut(63 downto 0);
          end case;
    end if;
  end process;



--  FIFOdata_o <= std_logic_vector(s_VMEaddrInput) & s_LWORDinput & std_logic_vector(s_VMEdataInput);

  s_locDataOut <= unsigned(WBdata_i) when s_cardSel = '1' else
                  resize(unsigned(s_CSRdata), s_locDataOut'length)    when s_confAccess = '1' and s_CSRaddressed = '1' and s_CRAMaddressed = '0' and s_CRaddressed = '0' else
                  resize(unsigned(s_CRdataIn), s_locDataOut'length)   when s_confAccess = '1' and s_CRaddressed = '1' and s_CRAMaddressed = '0' and s_CSRaddressed = '0' else
                  resize(unsigned(s_CRAMdataIn), s_locDataOut'length) when s_confAccess = '1' and s_CRAMaddressed = '1' and s_CRaddressed = '0' and s_CSRaddressed = '0' else
                  (others => '0');
  process(clk_i)
  begin
    if rising_edge(clk_i) then
      WBdata_o <= std_logic_vector(s_locDataIn);
      RW_o     <= s_RW;
      memReq_o <= s_memReq and s_cardSel;  -- memory request to WB only if it is selected with s_cardSel
      wbSel_o  <= s_sel;
    end if;
  end process;
  CRAMdata_o <= std_logic_vector(s_locDataIn(7 downto 0));
  CRAMwea_o  <= '1' when s_confAccess = '1' and s_CRAMaddressed = '1' and s_memReq = '1' and s_RW = '0' else '0';

  s_RW <= VME_WRITE_n_oversampled;  --and s_WrRd;        -- read if s_RW='1', write if s_RW='0'


  s_memAck <= s_memAckCSR(0) or memAckWB_i;


-- Access decode (NOTE: since A64 is supported, there are 4 64-bit FUNC_ADERs, because two consecutive 32-bit FUNC_ADERs are needed to decode a 64 bit address)
  process(clk_i)  --s_moduleEnable, s_funcMatch,s_AMmatch, s_addressingType, s_initInProgress, s_transferType, s_initInProgress )
  begin
    if rising_edge(clk_i) then
      if (s_moduleEnable = '1') and (s_addressingType /= CR_CSR) and (s_initInProgress = '0') then
        for I in s_funcMatch'range loop
          if s_funcMatch(I) = '1' and s_AMmatch(I) = '1' then
            s_cardSel <= '1';
--          else
--            s_cardSel <= '0';
          end if;
        end loop;
      else
        s_cardSel <= '0';

      end if;

      if (s_moduleEnable = '1') and (s_transferType = LCK) and (s_initInProgress = '0') then
        for I in s_funcMatch'range loop
          if s_funcMatch(I) = '1' then
            s_lockSel <= '1';
--          else
--            s_lockSel <= '0';
          end if;
        end loop;
      else
        s_lockSel <= '0';
      end if;
    end if;
  end process;
  s_confAccess <= '1' when unsigned(s_CSRarray(BAR)(7 downto 3)) = s_locAddr(23 downto 19) and s_addressingType = CR_CSR and s_initInProgress = '0' else '0';  -- CR/CSR decode

  s_locAddr_eq_bar        <= '1' when unsigned(s_CSRarray(BAR)(7 downto 3)) = s_locAddr(23 downto 19) else '0';  -- added by pablo to check simulation
  s_addressingType_CR_CSR <= '1' when s_addressingType = CR_CSR                                       else '0';

  p_functMatch : process(clk_i)  -- NOTE: interface will respond to different addressing types and will attempt to decode only the address width that it is given, even though the ADEM and ADER registers may contain a mask, that is greater than the current address width
  begin
    if rising_edge(clk_i) then  -- Added by pablo. Guess it should be clocked as the only signal in the
      -- sensitivity list was clk
      case s_addrWidth is
        when "11" =>
          for i in s_funcMatch'range loop
            if s_addressingType = TWOedge and (s_XAMtype = A32_2eVME or s_XAMtype = A32_2eSST) then
							 			  if s_FUNC_ADEM(i)(31 downto 10) /=0 then

              if (s_FUNC_ADER(i)(31 downto 10) and s_FUNC_ADEM(i)(31 downto 10)) = ((s_locAddr(31 downto 10)) and s_FUNC_ADEM(i)(31 downto 10)) then
                s_funcMatch(i) <= '1';
              else
                s_funcMatch(i) <= '0';
              end if;
              else
                s_funcMatch(i) <= '0';
              end if;
            elsif s_addressingType = TWOedge and (s_XAMtype = A64_2eVME or s_XAMtype = A64_2eSST) then
											 			  if s_FUNC_ADEM(i)(31 downto 10) /=0 then

              if (s_FUNC_ADER_64(i)(63 downto 10) and s_FUNC_ADEM_64(i)(63 downto 10)) = ((s_locAddr(63 downto 10)) and s_FUNC_ADEM_64(i)(63 downto 10)) then
                s_funcMatch(i) <= '1';
              else
                s_funcMatch(i) <= '0';
              end if;
              else
                s_funcMatch(i) <= '0';
              end if;
            else
				  if s_FUNC_ADEM(i)(31 downto 10) /=0 then
              if (s_FUNC_ADER_64(i)(63 downto 8) and s_FUNC_ADEM_64(i)(63 downto 8)) = ((s_locAddr(63 downto 8)) and s_FUNC_ADEM_64(i)(63 downto 8)) then
                s_funcMatch(i) <= '1';
              else
                s_funcMatch(i) <= '0';
              end if;
              else
                s_funcMatch(i) <= '0';
              end if;
            end if;
          end loop;
          
        when "10" =>
          for i in s_funcMatch'range loop
			 			  if s_FUNC_ADEM(i)(31 downto 8) /=0 then

            if (s_FUNC_ADER(i)(31 downto 8) and s_FUNC_ADEM(i)(31 downto 8)) = ((s_locAddr(31 downto 8)) and s_FUNC_ADEM(i)(31 downto 8)) then
              s_funcMatch(i) <= '1';
            else
              s_funcMatch(i) <= '0';
            end if;
				else
              s_funcMatch(i) <= '0';
            end if;

          end loop;
           
        when "01" =>
          for i in s_funcMatch'range loop
			  if s_FUNC_ADEM(i)(23 downto 8) /=0 then
            if (s_FUNC_ADER(i)(23 downto 8) and s_FUNC_ADEM(i)(23 downto 8)) = ((s_locAddr(23 downto 8)) and s_FUNC_ADEM(i)(23 downto 8)) then
              s_funcMatch(i) <= '1';
            else
              s_funcMatch(i) <= '0';
				end if;
				else
              s_funcMatch(i) <= '0';		
            end if;
          end loop;
			 
        when "00" =>

          for i in s_funcMatch'range loop
			   			 if s_FUNC_ADEM(i)(15 downto 8) /=0 then
            if (s_FUNC_ADER(i)(15 downto 8) and s_FUNC_ADEM(i)(15 downto 8)) = ((s_locAddr(15 downto 8)) and s_FUNC_ADEM(i)(15 downto 8)) then
				
              s_funcMatch(i) <= '1';
            else
              s_funcMatch(i) <= '0';
            end if;
			  else 
              s_funcMatch(i) <= '0';
			  end if;
          end loop;
          
        when others =>
          for i in s_funcMatch'range loop
            s_funcMatch(i) <= '0';
          end loop;
      end case;
    end if;
  end process;

------------------------------------------------------
  process(s_FUNC_AMCAP, VME_AM_oversampled)
  begin
    s_amcap_match <= (others => '0');
    for i in 0 to 7 loop
      s_amcap_match(i) <= s_FUNC_AMCAP(i)(to_integer(unsigned(VME_AM_oversampled)));
    end loop;
  end process;
------------------------------------------------------
  process(s_FUNC_XAMCAP, s_XAM)
  begin
    s_xamcap_match <= (others => '0');
    for i in 0 to 3 loop
      s_xamcap_match(i) <= s_FUNC_XAMCAP(i)(to_integer(s_XAM));
    end loop;
  end process;
------------------------------------------------------

  p_AMmatch : process(clk_i)
  begin
    if rising_edge(clk_i) then  -- Added by pablo. Guess it should be clocked as the only signal in the
--                                                                          -- sensitivity list was clk_i
      for i in s_AMmatch'range loop
        if s_FUNC_ADEM(i)(DFS) = '1' then
          if s_FUNC_ADER(i)(XAM_MODE) = '0' then
            if unsigned(s_FUNC_ADER(i)(7 downto 2)) = unsigned(VME_AM_oversampled) then
              s_AMmatch(i) <= '1';
            else
              s_AMmatch(i) <= '0';
            end if;
          else
            if (s_addressingType = TWOedge) and ((s_XAM) = s_FUNC_ADER(i)(9 downto 2)) then
              s_AMmatch(i) <= '1';
            else
              s_AMmatch(i) <= '0';
            end if;
          end if;
        elsif s_addressingType = TWOedge and (s_FUNC_AMCAP(i)(to_integer(unsigned(VME_AM_oversampled))) = '1') then
          s_AMmatch(i) <= '1';          --s_xamcap_match(i);
        else
          s_AMmatch(i) <= s_amcap_match(i);
        end if;
      end loop;
    end if;
  end process;


-- CR/CSR addressing (NOTE: only D08 access is supported)

  s_CrCsrOffsetAddr <= s_locAddr(18 downto 0);

  s_CRaddr <= (s_CrCsrOffsetAddr) when s_initInProgress = '0' else
              (resize(s_initReadCounter, CRaddr_o'length));  -- when s_initInProgress='1' the initialization procedure will hijack this address bus

  CRaddr_o   <= std_logic_vector(s_CRaddr);
  CRAMaddr_o <= std_logic_vector(s_CrCsrOffsetAddr - unsigned(s_BEG_CRAM(18 downto 0)));

  s_CSRaddressed  <= '1' when (s_CrCsrOffsetAddr <= x"7FFFF" and s_CrCsrOffsetAddr >= x"7FC00") xor (s_CrCsrOffsetAddr >= s_BEG_USER_CSR(18 downto 0) and s_CrCsrOffsetAddr <= s_END_USER_CSR(18 downto 0) and s_BEG_USER_CSR < s_END_USER_CSR) else '0';
  s_CRaddressed   <= '1' when (s_CrCsrOffsetAddr <= x"00FFF" and s_CrCsrOffsetAddr >= x"00000") xor (s_CrCsrOffsetAddr >= s_BEG_USER_CR(18 downto 0) and s_CrCsrOffsetAddr <= s_END_USER_CR(18 downto 0) and s_BEG_USER_CR < s_END_USER_CR)     else '0';
  s_CRAMaddressed <= '1' when (s_CrCsrOffsetAddr >= s_BEG_CRAM(18 downto 0) and s_CrCsrOffsetAddr <= s_END_CRAM(18 downto 0) and s_BEG_CRAM < s_END_CRAM)                                                                                       else '0';




-- CR/CSR memory acknowledge
  p_memAckCSR : process(clk_i)
  begin
    if rising_edge(clk_i) then
      if s_reset = '1' then
        s_memAckCSR <= (others => '0');
      else
        if s_memReq = '1' and s_confAccess = '1' then
          s_memAckCSR <= s_memAckCSR (1 downto 0) & '1';
        else
          s_memAckCSR <= (others => '0');
        end if;
      end if;
    end if;
  end process;


-- Control & Status Registers (NOTE: only D08 access is supported)    

  s_GAparityMatch <= '1' when VME_GA_oversampled(5) = not (VME_GA_oversampled(0) xor VME_GA_oversampled(1) xor VME_GA_oversampled(2) xor VME_GA_oversampled(3) xor VME_GA_oversampled(4)) else '0';

  s_moduleEnable <= '1';  -- set by pablo for testing it was: s_CSRarray(BIT_SET_CLR_REG)(4);



-- CSR write
  p_CSR_Write : process(clk_i)
  begin
    if rising_edge(clk_i) then
      if s_reset = '1' then
        if s_GAparityMatch = '1' then
          s_CSRarray(BAR) <= (resize(unsigned(not VME_GA_oversampled(4 downto 0))*8, s_CSRarray(BAR)'length));
        else
          s_CSRarray(BAR) <= (others => '0');
        end if;
        s_bar_written <= '0';
      elsif s_memReq = '1' and s_confAccess = '1' then
        if s_CrCsrOffsetAddr = BAR and s_RW = '0' then
          s_CSRarray(BAR) <= s_locDataIn(7 downto 0);
          s_bar_written   <= '1';
        end if;
        
      elsif s_bar_written = '0' then
        s_CSRarray(BAR)(7 downto 3) <= unsigned(not VME_GA_oversampled(4 downto 0));
        s_CSRarray(BAR)(2 downto 0) <= "000";
      end if;

      if s_reset = '1' then
        s_BitSetReg            <= (others => '0');
        s_BitClrReg            <= (others => '0');
        s_UsrBitClrReg         <= (others => '0');
        s_UsrBitSetReg         <= (others => '0');
        s_CSRarray(CRAM_OWNER) <= (others => '0');
 --       s_CSRarray(BAR)        <= (others => '0');
        for i in CRAM_OWNER downto IRQ_level loop
          s_CSRarray(i) <= c_csr_array(i);
        end loop;
      elsif s_memReq = '1' and s_confAccess = '1' then
        
        case to_integer(s_CrCsrOffsetAddr) is

          -- Base Address Register
          when BAR_addr =>

          -- Bit Set Register
          when BIT_SET_REG_addr =>
            for i in 0 to 7 loop
              if s_BitSetReg(i) = '1' then
                s_CSRarray(BIT_SET_CLR_REG)(i) <= '1';
              end if;
            end loop;
            if s_RW = '0' then
              s_BitSetReg <= s_locDataIn(7 downto 0);
            end if;

          -- Bit Clear Register
          when BIT_CLR_REG_addr =>
            for i in 0 to 7 loop
              if s_BitClrReg(i) = '1' then
                s_CSRarray(BIT_SET_CLR_REG)(i) <= '0';
              end if;
            end loop;
            if s_CSRarray(BIT_SET_CLR_REG)(2) = '0' then
              s_CSRarray(CRAM_OWNER) <= x"00";
            end if;
            if s_RW = '0' then
              s_BitClrReg <= s_locDataIn(7 downto 0);
            end if;

          -- CRAM Owner register    
          when CRAM_OWNER_addr =>
            if s_RW = '0' and s_CSRarray(CRAM_OWNER) = x"00" then
              s_CSRarray(CRAM_OWNER) <= s_locDataIn(7 downto 0);  -- Write register (give ownership) only    if register value is 0
            end if;

          -- User-Defined Bit Set Register
          when USR_BIT_SET_REG_addr =>
            for i in 0 to 7 loop
              if s_UsrBitSetReg(i) = '1' then
                s_CSRarray(USR_BIT_SET_CLR_REG)(i) <= '1';
              end if;
            end loop;
            if s_RW = '0' then
              s_UsrBitSetReg <= s_locDataIn(7 downto 0);
            end if;

          -- User-Defined Bit Clear Register
          when USR_BIT_CLR_REG_addr =>
            for i in 0 to 7 loop
              if s_UsrBitClrReg(i) = '1' then
                s_CSRarray(USR_BIT_SET_CLR_REG)(i) <= '0';
              end if;
            end loop;
            if s_RW = '0' then
              s_UsrBitClrReg <= s_locDataIn(7 downto 0);
            end if;

          --     Function ADER registers           
          when FUNC0_ADER_3_addr to FUNC7_ADER_0_addr =>
            if s_RW = '0' then
              s_CSRarray(to_integer(s_CrCsrOffsetAderIndex)) <= s_locDataIn(7 downto 0);
            end if;
          when IRQ_ID_addr =>
            if s_RW = '0' then
              s_CSRarray(IRQ_ID) <= s_locDataIn(7 downto 0);
            end if;
            
          when IRQ_level_addr =>
            if s_RW = '0' then
              s_CSRarray(IRQ_level) <= s_locDataIn(7 downto 0);
            end if;
            
          when others =>
            
        end case;
        
      else
        if s_transferActive = '1' and s_BERRcondition = '1' then
          s_CSRarray(BIT_SET_CLR_REG)(3) <= '1';  -- BERR flag
        end if;

        if s_CSRarray(CRAM_OWNER) = x"00" then  -- CRAM OWNER flag
          s_CSRarray(BIT_SET_CLR_REG)(2) <= '0';
        else
          s_CSRarray(BIT_SET_CLR_REG)(2) <= '1';
        end if;
        
      end if;
    end if;
  end process;


  s_CrCsrOffsetAderIndex_aux <= s_CrCsrOffsetAddr - FUNC0_ADER_3_addr;
  s_CrCsrOffsetAderIndex     <= unsigned(s_CrCsrOffsetAderIndex_aux(s_CrCsrOffsetAderIndex'left + 2 downto 2))+ FUNC0_ADER_3;
-- CSR read
  process(s_CSRarray, s_CrCsrOffsetAddr)
  begin
    s_CSRdata <= (others => '0');
    case to_integer(s_CrCsrOffsetAddr) is
      when BAR_addr             => s_CSRdata <= s_CSRarray(BAR);
      when BIT_SET_REG_addr     => s_CSRdata <= s_CSRarray(BIT_SET_CLR_REG);
      when BIT_CLR_REG_addr     => s_CSRdata <= s_CSRarray(BIT_SET_CLR_REG);
      when CRAM_OWNER_addr      => s_CSRdata <= s_CSRarray(CRAM_OWNER);
      when USR_BIT_SET_REG_addr => s_CSRdata <= s_CSRarray(USR_BIT_SET_CLR_REG);
      when USR_BIT_CLR_REG_addr => s_CSRdata <= s_CSRarray(USR_BIT_SET_CLR_REG);
      when FUNC7_ADER_0_addr    => s_CSRdata <= s_CSRarray(FUNC7_ADER_0);
      when FUNC7_ADER_1_addr    => s_CSRdata <= s_CSRarray(FUNC7_ADER_1);
      when FUNC7_ADER_2_addr    => s_CSRdata <= s_CSRarray(FUNC7_ADER_2);
      when FUNC7_ADER_3_addr    => s_CSRdata <= s_CSRarray(FUNC7_ADER_3);
      when FUNC6_ADER_0_addr    => s_CSRdata <= s_CSRarray(FUNC6_ADER_0);
      when FUNC6_ADER_1_addr    => s_CSRdata <= s_CSRarray(FUNC6_ADER_1);
      when FUNC6_ADER_2_addr    => s_CSRdata <= s_CSRarray(FUNC6_ADER_2);
      when FUNC6_ADER_3_addr    => s_CSRdata <= s_CSRarray(FUNC6_ADER_3);
      when FUNC5_ADER_0_addr    => s_CSRdata <= s_CSRarray(FUNC5_ADER_0);
      when FUNC5_ADER_1_addr    => s_CSRdata <= s_CSRarray(FUNC5_ADER_1);
      when FUNC5_ADER_2_addr    => s_CSRdata <= s_CSRarray(FUNC5_ADER_2);
      when FUNC5_ADER_3_addr    => s_CSRdata <= s_CSRarray(FUNC5_ADER_3);
      when FUNC4_ADER_0_addr    => s_CSRdata <= s_CSRarray(FUNC4_ADER_0);
      when FUNC4_ADER_1_addr    => s_CSRdata <= s_CSRarray(FUNC4_ADER_1);
      when FUNC4_ADER_2_addr    => s_CSRdata <= s_CSRarray(FUNC4_ADER_2);
      when FUNC4_ADER_3_addr    => s_CSRdata <= s_CSRarray(FUNC4_ADER_3);
      when FUNC3_ADER_0_addr    => s_CSRdata <= s_CSRarray(FUNC3_ADER_0);
      when FUNC3_ADER_1_addr    => s_CSRdata <= s_CSRarray(FUNC3_ADER_1);
      when FUNC3_ADER_2_addr    => s_CSRdata <= s_CSRarray(FUNC3_ADER_2);
      when FUNC3_ADER_3_addr    => s_CSRdata <= s_CSRarray(FUNC3_ADER_3);
      when FUNC2_ADER_0_addr    => s_CSRdata <= s_CSRarray(FUNC2_ADER_0);
      when FUNC2_ADER_1_addr    => s_CSRdata <= s_CSRarray(FUNC2_ADER_1);
      when FUNC2_ADER_2_addr    => s_CSRdata <= s_CSRarray(FUNC2_ADER_2);
      when FUNC2_ADER_3_addr    => s_CSRdata <= s_CSRarray(FUNC2_ADER_3);
      when FUNC1_ADER_0_addr    => s_CSRdata <= s_CSRarray(FUNC1_ADER_0);
      when FUNC1_ADER_1_addr    => s_CSRdata <= s_CSRarray(FUNC1_ADER_1);
      when FUNC1_ADER_2_addr    => s_CSRdata <= s_CSRarray(FUNC1_ADER_2);
      when FUNC1_ADER_3_addr    => s_CSRdata <= s_CSRarray(FUNC1_ADER_3);
      when FUNC0_ADER_0_addr    => s_CSRdata <= s_CSRarray(FUNC0_ADER_0);
      when FUNC0_ADER_1_addr    => s_CSRdata <= s_CSRarray(FUNC0_ADER_1);
      when FUNC0_ADER_2_addr    => s_CSRdata <= s_CSRarray(FUNC0_ADER_2);
      when FUNC0_ADER_3_addr    => s_CSRdata <= s_CSRarray(FUNC0_ADER_3);
      when IRQ_ID_addr          => s_CSRdata <= s_CSRarray(IRQ_ID);
      when IRQ_level_addr       => s_CSRdata <= s_CSRarray(IRQ_level);
      when others =>
    end case;
-----------------------------------------------
-----------------------------------------------
-----------------------------------------------
  end process;
  IRQlevelReg_o <= std_logic_vector(s_CSRarray(IRQ_level));

-- Initialization procedure                

  p_coreInit : process(clk_i)  -- Read important CR data (like FUNC_ADEMs etc.) and store it locally
  begin
    if rising_edge(clk_i) then
      if s_reset = '1' then
        s_initState       <= IDLE;
        s_initReadCounter <= to_unsigned(0, s_initReadCounter'length);
        s_latchCRdata     <= '0';
        for i in 0 to 51 loop
          s_CRregArray(i) <= (others => '0');
        end loop;
      else
        case s_initState is
          when IDLE =>
            s_initReadCounter <= to_unsigned(0, s_initReadCounter'length);
            s_latchCRdata     <= '0';
            s_initState       <= SET_ADDR;
            
          when SET_ADDR =>
            s_initReadCounter <= s_initReadCounter+1;
            s_latchCRdata     <= '0';
            s_initState       <= GET_DATA;
            
          when GET_DATA =>
            s_initReadCounter <= s_initReadCounter;
            s_latchCRdata     <= '1';
            if s_initInProgress = '1' then
              s_initState <= SET_ADDR;
            else
              s_initState <= END_INIT;
            end if;
            
          when END_INIT =>              -- will wait in this state until reset
            s_initReadCounter <= s_initReadCounter;
            s_latchCRdata     <= '0';
            s_initState       <= END_INIT;
            
          when others =>
            s_initState       <= IDLE;
            s_initReadCounter <= to_unsigned(0, s_initReadCounter'length);
            s_latchCRdata     <= '0';
            
        end case;
--        if s_latchCRdata='1' then
--          s_CRregArray(s_initReadCounter) <= unsigned(CRdata_i);
--        end if;
      end if;
    end if;
  end process;

  s_initInProgress <= '1' when (s_initReadCounter <= (428)) else '0';

  s_CRadd_offset <= s_CRaddr - s_CRaddr_base;
  process(s_latchCRdata, s_initReadCounter)
  begin
    s_latchCRdataPos <= (others => '0');
    s_CRaddr_base    <= (others => '0');
    for I in c_CRinitAddr'range loop
      if (s_initReadCounter >= c_CRinitAddr(I).add) and (s_initReadCounter <= (c_CRinitAddr(I).add+(c_CRinitAddr(I).len-1))) then
        s_CRaddr_base       <= to_unsigned(c_CRinitAddr(I).add, s_CRaddr_base'length);
        s_latchCRdataPos(I) <= s_latchCRdata;
        exit;
      end if;
    end loop;
  end process;

  process(clk_i)
  begin
    if rising_edge(clk_i) then
      for I in 0 to 2 loop
        if (s_latchCRdataPos(BEG_USER_CR) = '1') and (unsigned(s_CRadd_offset(1 downto 0)) = I) then
          s_BEG_USER_CR(((3-i)*8 - 1) downto (2-i)*8) <= unsigned(CRdata_i);
        end if;

        if s_latchCRdataPos(END_USER_CR) = '1' and (unsigned(s_CRadd_offset(1 downto 0)) = I) then
          s_END_USER_CR(((3-i)*8 - 1) downto (2-i)*8) <= unsigned(CRdata_i);
        end if;

        if (s_latchCRdataPos(BEG_USER_CSR) = '1') and (unsigned(s_CRadd_offset(1 downto 0)) = I) then
          s_BEG_USER_CSR(((3-i)*8 - 1) downto (2-i)*8) <= unsigned(CRdata_i);
        end if;

        if (s_latchCRdataPos(END_USER_CSR) = '1') and (unsigned(s_CRadd_offset(1 downto 0)) = I) then
          s_END_USER_CSR(((3-i)*8 - 1) downto (2-i)*8) <= unsigned(CRdata_i);
        end if;

        if (s_latchCRdataPos(BEG_CRAM) = '1') and (unsigned(s_CRadd_offset(1 downto 0)) = I) then
          s_BEG_CRAM(((3-i)*8 - 1) downto (2-i)*8) <= unsigned(CRdata_i);
        end if;

        if (s_latchCRdataPos(END_CRAM) = '1') and (unsigned(s_CRadd_offset(1 downto 0)) = I) then
          s_END_CRAM(((3-i)*8 - 1) downto (2-i)*8) <= unsigned(CRdata_i);
        end if;
      end loop;

      for I in 0 to 7 loop

        if (s_latchCRdataPos(FUNC_AMCAP) = '1') and (unsigned(s_CRadd_offset(5 downto 3)) = I) then
          for H in 0 to 7 loop
            if (unsigned(s_CRadd_offset(2 downto 0)) = H) then
              s_FUNC_AMCAP(I)(((8-h)*8 - 1) downto (7-h)*8) <= unsigned(CRdata_i);
            end if;
          end loop;
        end if;
        if (s_latchCRdataPos(FUNC_ADEM) = '1') and (unsigned(s_CRadd_offset(4 downto 2)) = I) then
          for H in 0 to 3 loop
            if (unsigned(s_CRadd_offset(1 downto 0)) = H) then
              s_FUNC_ADEM(I)(((4-h)*8 - 1) downto (3-h)*8) <= unsigned(CRdata_i);
            end if;
          end loop;
        end if;


        if (s_latchCRdataPos(FUNC_XAMCAP) = '1') and (unsigned(s_CRadd_offset(7 downto 5)) = I) then
          for H in 0 to 31 loop
            if (unsigned(s_CRadd_offset(4 downto 0)) = H) then
              s_FUNC_XAMCAP(I)(((32-h)*8 - 1) downto (31-h)*8) <= unsigned(CRdata_i);
            end if;
          end loop;
        end if;

      end loop;
    end if;
  end process;


  GADER_1 : for i in 0 to 7 generate
    GADER_2 : for h in 0 to 3 generate
      s_FUNC_ADER(i)(8*(4-h)-1 downto 8*(3-h)) <= s_CSRarray(FUNC0_ADER_3+(h+i*4));
    end generate GADER_2;
  end generate GADER_1;

  GAD_64 : for i in 0 to 6 generate
    s_FUNC_ADER_64(i) <= s_FUNC_ADER(i+1)&s_FUNC_ADER(i);
    s_FUNC_ADEM_64(I) <= s_FUNC_ADEM(i+1)&s_FUNC_ADEM(i);
  end generate GAD_64;
      s_FUNC_ADER_64(7) <= (others => '0');
      s_FUNC_ADEM_64(7) <= (others => '0');


------------------------------------------------------
-- Input oversampling & edge detection
process(clk_i)
begin
if rising_edge(clk_i) then
s_DS1pulse_d <= s_DS1pulse; 
end if;
end process;
  ASfallingEdge : FallingEdgeDetection
    port map (
      sig_i      => VME_AS_n_oversampled,
      clk_i      => clk_i,
      FallEdge_o => s_VMEaddrLatch
      );

  ASrisingEdge : RisEdgeDetection
    port map (
      sig_i     => VME_AS_n_oversampled,
      clk_i     => clk_i,
      RisEdge_o => s_mainFSMreset
      ); 

  DS1EdgeDetect : EdgeDetection
    port map (
      sig_i     => VME_DS_n_oversampled_1(1),
      clk_i     => clk_i,
      sigEdge_o => s_DS1pulse
      );

  AMinputSample : RegInputSample
    generic map(
      width => 6
      )
    port map(
      reg_i => VME_AM_i,
      reg_o => VME_AM_oversampled,
      clk_i => clk_i
      );

  DATAinputSample : RegInputSample
    generic map(
      width => 32
      )
    port map (
      reg_i => VME_DATA_b_i,
      reg_o => VME_DATA_oversampled,
      clk_i => clk_i
      );

  ADDRinputSample : RegInputSample
    generic map(
      width => 31
      )
    port map(
      reg_i => VME_ADDR_b_i,
      reg_o => VME_ADDR_oversampled,
      clk_i => clk_i
      );

  GAinputSample : RegInputSample
    generic map(
      width => 6
      )
    port map(
      reg_i => VME_GA_i,
      reg_o => VME_GA_oversampled,
      clk_i => clk_i
      );

  DSinputSample : DoubleRegInputSample
    generic map(
      width => 2
      )
    port map(
      reg_i => VME_DS_n_i,
      reg_o => VME_DS_n_oversampled_1,
      clk_i => clk_i
      );

  p_DSsync : process(clk_i)
  begin
    if rising_edge(clk_i) then
      VME_DS_n_oversampled <= VME_DS_n_oversampled_1;
    end if;
  end process;

  CRinputSample : RegInputSample
    generic map(
      width => 8
      )
    port map(
      reg_i => CRdata_i,
      reg_o => s_CRdataIn,
      clk_i => clk_i
      );

  CRAMinputSample : RegInputSample
    generic map(
      width => 8
      )
    port map(
      reg_i => CRAMdata_i,
      reg_o => s_CRAMdataIn,
      clk_i => clk_i
      );

  WRITEinputSample : SigInputSample
    port map(
      sig_i => VME_WRITE_n_i,
      sig_o => VME_WRITE_n_oversampled,
      clk_i => clk_i
      );

  LWORDinputSample : SigInputSample
    port map(
      sig_i => VME_LWORD_n_b_i,
      sig_o => VME_LWORD_n_oversampled,
      clk_i => clk_i
      );

  ASinputSample : DoubleSigInputSample
    port map(
      sig_i => VME_AS_n_i,
      sig_o => VME_AS_n_oversampled,
      clk_i => clk_i
      );

  RSTinputSample : SigInputSample
    port map(
      sig_i => VME_RST_n_i,
      sig_o => VME_RST_n_oversampled,
      clk_i => clk_i
      );

  BBSYinputSample : SigInputSample
    port map(
      sig_i => VME_BBSY_n_i,
      sig_o => VME_BBSY_n_oversampled,
      clk_i => clk_i
      ); 

  IACKINinputSample : SigInputSample
    port map(
      sig_i => VME_IACKIN_n_i,
      sig_o => VME_IACKIN_n_oversampled,
      clk_i => clk_i
      ); 

end RTL;
