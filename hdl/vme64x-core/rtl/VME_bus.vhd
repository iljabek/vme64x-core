--------------------------------------------------------------------------------
-- CERN (BE-CO-HT)
-- VME64x Core
-- http://www.ohwr.org/projects/vme64x-core
--------------------------------------------------------------------------------
--
-- unit name:     VME_bus (VME_bus.vhd)
--
-- author:        Pablo Alvarez Sanchez <pablo.alvarez.sanchez@cern.ch>
--                Davide Pedretti       <davide.pedretti@cern.ch>
--
-- description:
--
--   This block acts as interface between the VMEbus and the CR/CSR space or
--   WBbus.
--
--                      _________VME_bus__________
--                     |  __________________      |
--                     | |                  |  ___|
--                     | |                  | |   |
--                     | |      MAIN        | | W |
--                   V | |                  | | B | W
--                   M | |      FSM         | |   | B
--                   E | |                  | | M |
--                     | |                  | | A | B
--                   B | |__________________| | S | U
--                   U |  __________________  | T | S
--                   S | |                  | | E |
--                     | |   OTHER DATA &   | | R |
--                     | |   ADDR PROCESS   | |___|
--                     | |__________________|     |
--                     |__________________________|
--
--   The Access decode component decodes the address to check if the board is
--   the responding Slave. This component is of fundamental importance, indeed
--   only one Slave can answer to the Master!
--   In the right side you can see the WB Master who implements the Wb Pipelined
--   single read/write protocol.
--   Each VME board plugged in a slot acts as a VME slave module and it has only
--   one CR/CSR space (conforming with the specification) so only one FPGA at
--   time must drive the output lines on the VME bus; only one FPGA at time can
--   carry the vme64x core or other similar VME slave core.
--   Inside each component is possible read a more detailed description.
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

entity VME_bus is
  generic (
    g_CLOCK_PERIOD  : integer;
    g_WB_DATA_WIDTH : integer;
    g_WB_ADDR_WIDTH : integer
  );
  port (
    clk_i           : in  std_logic;
    rst_i           : in  std_logic;

    -- VME signals
    VME_AS_n_i      : in  std_logic;
    VME_LWORD_n_o   : out std_logic := '0';
    VME_LWORD_n_i   : in  std_logic;
    VME_RETRY_n_o   : out std_logic;
    VME_RETRY_OE_o  : out std_logic;
    VME_WRITE_n_i   : in  std_logic;
    VME_DS_n_i      : in  std_logic_vector(1 downto 0);
    VME_DS_ant_n_i  : in  std_logic_vector(1 downto 0);
    VME_DTACK_n_o   : out std_logic;
    VME_DTACK_OE_o  : out std_logic;
    VME_BERR_n_o    : out std_logic;
    VME_ADDR_i      : in  std_logic_vector(31 downto 1);
    VME_ADDR_o      : out std_logic_vector(31 downto 1) := (others => '0');
    VME_ADDR_DIR_o  : out std_logic;
    VME_ADDR_OE_N_o : out std_logic;
    VME_DATA_i      : in  std_logic_vector(31 downto 0);
    VME_DATA_o      : out std_logic_vector(31 downto 0) := (others => '0');
    VME_DATA_DIR_o  : out std_logic;
    VME_DATA_OE_N_o : out std_logic;
    VME_AM_i        : in  std_logic_vector(5 downto 0);
    VME_IACK_n_i    : in  std_logic;  -- USE VME_IACK_n_i and NOT VME_IACKIN_n_i
                                      -- because VME_IACKIN_n_i is delayed the
                                      -- more you are away from Slots 0
    -- WB signals
    stb_o           : out std_logic;
    ack_i           : in  std_logic;
    dat_o           : out std_logic_vector(g_WB_DATA_WIDTH-1 downto 0);
    dat_i           : in  std_logic_vector(g_WB_DATA_WIDTH-1 downto 0);
    adr_o           : out std_logic_vector(g_WB_ADDR_WIDTH-1 downto 0);
    sel_o           : out std_logic_vector(g_WB_DATA_WIDTH/8-1 downto 0);
    we_o            : out std_logic;
    cyc_o           : out std_logic;
    err_i           : in  std_logic;
    rty_i           : in  std_logic;
    stall_i         : in  std_logic;

    -- Function decoder
    addr_decoder_i  : in  std_logic_vector(63 downto 0);
    addr_decoder_o  : out std_logic_vector(63 downto 0);
    decode_o        : out std_logic;
    am_o            : out std_logic_vector( 5 downto 0);
    xam_o           : out std_logic_vector( 7 downto 0);
    sel_i           : in  std_logic;

    --CR/CSR space signals:
    cr_csr_addr_o   : out std_logic_vector(18 downto 2);
    cr_csr_data_i   : in  std_logic_vector( 7 downto 0);
    cr_csr_data_o   : out std_logic_vector( 7 downto 0);
    cr_csr_we_o     : out std_logic;
    endian_i        : in  std_logic_vector(2 downto 0);
    module_enable_i : in  std_logic;
    bar_i           : in  std_logic_vector(4 downto 0)
  );
end VME_bus;

architecture RTL of VME_bus is

  signal s_rw                       : std_logic;

  -- Input signals
  signal s_VMEaddrInput             : unsigned(31 downto 1);
  signal s_VMEdataInput             : unsigned(31 downto 0);
  signal s_LWORDinput               : std_logic;

  -- External buffer signals
  signal s_dtackOE                  : std_logic;
  signal s_dataDir                  : std_logic;
  signal s_dataOE                   : std_logic;
  signal s_addrDir                  : std_logic;
  signal s_addrOE                   : std_logic;

  -- Local data & address
  signal s_locDataIn                : unsigned(63 downto 0);
  signal s_locDataOut               : unsigned(63 downto 0);
  signal s_locData                  : unsigned(63 downto 0);          -- Local data
  signal s_locAddr, s_rel_locAddr   : unsigned(63 downto 0);          -- Local address
  signal s_locAddr2e                : unsigned(63 downto 0);          -- for 2e transfers
  signal s_locAddrBeforeOffset      : unsigned(63 downto 0);
  signal s_phase1addr               : unsigned(63 downto 0);          -- for 2e transfers
  signal s_phase2addr               : unsigned(63 downto 0);          --
  --signal s_phase3addr               : unsigned(63 downto 0);          --
  signal s_addrOffset               : unsigned(17 downto 0);          -- block transfers|
  signal s_DataShift                : unsigned(5 downto 0);
  -- uncomment if 2e is implemented:
  --signal s_2eLatchAddr              : std_logic_vector(1 downto 0);   -- for 2e transfers
  signal s_locDataSwap              : std_logic_vector(63 downto 0);
  signal s_locDataInSwap            : std_logic_vector(63 downto 0);
  signal s_locDataOutWb             : std_logic_vector(63 downto 0);

  -- Latched signals
  signal s_VMEaddrLatched           : unsigned(63 downto 1);          -- Latch on AS falling edge
  signal s_LWORDlatched             : std_logic;                      -- Stores LWORD on falling edge of AS
  signal s_DSlatched                : std_logic_vector(1 downto 0);   -- Stores DS
  signal s_AMlatched                : std_logic_vector(5 downto 0);   -- Latch on AS f. edge

  -- Type of data transfer (depending on VME_DS_n, VME_LWORD_n and VME_ADDR(1))
  signal s_typeOfDataTransfer       : t_typeOfDataTransfer;
  signal s_typeOfDataTransferSelect : std_logic_vector(4 downto 0);

  -- Addressing type (depending on VME_AM_i)
  signal s_addressingType           : t_addressingType;
  signal s_addressingTypeSelect     : std_logic_vector(5 downto 0);
  signal s_transferType             : t_transferType;
  signal s_XAMtype                  : t_XAMtype;
  signal s_2eType                   : t_2eType;
  signal s_addrWidth                : std_logic_vector(1 downto 0);
  signal s_addrWidth1               : std_logic_vector(1 downto 0);

  -- Main FSM signals
  signal s_mainFSMstate             : t_mainFSMstates;
  signal s_dataToAddrBus            : std_logic;                      -- (for D64) --> multiplexed transfer
  signal s_dataToOutput             : std_logic;                      -- Puts data to VME data bus
  signal s_mainDTACK                : std_logic;                      -- DTACK driving
  signal s_memAck                   : std_logic;                      -- Memory acknowledge
  signal s_memAckCSR                : std_logic;                      -- CR/CSR acknowledge
  signal s_memReq                   : std_logic;                      -- Global memory request
  signal s_VMEaddrLatch             : std_logic;                      -- pulse on VME_AS_n_i f.edge
  signal s_DSlatch                  : std_logic;                      -- Stores data strobes
  signal s_incrementAddr            : std_logic;                      -- Increments local address
  signal s_blockTransferLimit       : std_logic;                      -- Block transfer limit
  signal s_mainFSMreset             : std_logic;                      -- Resets main FSM on AS r. edge
  signal s_dataPhase                : std_logic;                      -- for A64 and multipl. transf.
  signal s_transferActive           : std_logic;                      -- active VME transfer
  signal s_retry                    : std_logic;                      -- RETRY signal

  -- uncomment if 2e is implemented:
  --signal s_berr                     : std_logic;                      -- BERR signal
  --signal s_berr_1                   : std_logic;                      --
  --signal s_berr_2                   : std_logic;                      --

  -- Access decode signals
  signal s_conf_sel                 : std_logic;                      -- Asserted when CR or CSR is addressed
  signal s_card_sel                 : std_logic;                      -- Asserted when WB memory is addressed

  -- WishBone signals
  signal s_sel                      : unsigned(7 downto 0);           -- SEL WB signal
  signal s_nx_sel                   : std_logic_vector(7 downto 0);

  -- Error signals
  signal s_BERRcondition            : std_logic;                      -- Condition for asserting BERR
  signal s_wberr1                   : std_logic;
  signal s_rty1                     : std_logic;

  -- Initialization signals
  signal s_prev_VME_AS_n            : std_logic;
  signal s_is_d64                   : std_logic;
  signal s_base_addr                : unsigned(63 downto 0);
  signal s_VMEdata64In              : unsigned(63 downto 0);

  signal s_BERR_out                 : std_logic;
  signal s_sw_reset                 : std_logic;
  signal s_decode                   : std_logic;
  signal s_AckWb                    : std_logic;
  signal s_err                      : std_logic;
  signal s_rty                      : std_logic;

  signal s_wbMaster_rst             : std_logic;
  signal s_num_latchDS              : integer;

  function f_latchDS (clk_period : integer) return integer is
  begin
    for I in 1 to 4 loop
      if (clk_period * I >= 20) then  -- 20 is the max time between the assertion
        return(I);                    -- of the DS lines.
      end if;
    end loop;
    return(4);                        -- works for up to 200 MHz
  end function f_latchDS;

begin

  -- Calculate the number of LATCH DS states necessary to match the timing
  -- rule 2.39 page 113 VMEbus specification ANSI/IEEE STD1014-1987.
  s_num_latchDS <= f_latchDS(g_CLOCK_PERIOD);

  -- Used to drive the VME_ADDR_DIR_o
  s_is_d64      <= '1' when s_sel = "11111111" else '0';

  -- These output signals are connected to the buffers on the board
  -- SN74VMEH22501A Function table:
  --   OEn | DIR | OUTPUT                 OEAB   |   OEBYn   |   OUTPUT
  --    H  |  X  |   Z                      L    |     H     |     Z
  --    L  |  H  | A to B                   H    |     H     |   A to B
  --    L  |  L  | B to A                   L    |     L     |   B to Y
  --                                        H    |     L     |A to B, B to Y |

  VME_DATA_DIR_o  <= s_dataDir;
  VME_DATA_OE_N_o <= s_dataOE;
  VME_ADDR_DIR_o  <= s_addrDir;
  VME_ADDR_OE_N_o <= s_addrOE;
  VME_DTACK_OE_o  <= s_dtackOE;

  -- VME DTACK:
  VME_DTACK_n_o <= s_mainDTACK;

  ------------------------------------------------------------------------------
  -- Access Mode Decoders
  ------------------------------------------------------------------------------
  -- Type of data transfer decoder
  -- VME64 ANSI/VITA 1-1994...Table 2-2 "Signal levels during data transfers"
  -- A2 is used to select the D64 type  (D64 --> MBLT and 2edge cycles)
  -- VME DATA --> BIG ENDIAN

  s_typeOfDataTransferSelect <= s_DSlatched & s_VMEaddrLatched(1) &
                                s_LWORDlatched & s_VMEaddrLatched(2);

  -- These 5 bits are not sufficient to descriminate the D32 and D64 data
  -- transfer type; indeed the D32 access with A2 = '0' (eg 0x010)
  -- fall within D64 access --> The data transfer type have to be evaluated
  -- jointly with the address type.
  --
  -- Bytes position on VMEbus:
  --
  -- A24-31 | A16-23 | A08-15 | A00-07 | D24-31 | D16-23 | D08-15 | D00-07
  --        |        |        |        |        |        | BYTE 0 |
  --        |        |        |        |        |        |        | BYTE 1
  --        |        |        |        |        |        | BYTE 2 |
  --        |        |        |        |        |        |        | BYTE 3
  --        |        |        |        |        |        | BYTE 0 | BYTE 1
  --        |        |        |        |        |        | BYTE 2 | BYTE 3
  --        |        |        |        | BYTE 0 | BYTE 1 | BYTE 2 | BYTE 3
  -- BYTE 0 | BYTE 1 | BYTE 2 | BYTE 3 | BYTE 4 | BYTE 5 | BYTE 6 | BYTE 7
  with s_typeOfDataTransferSelect select s_typeOfDataTransfer <=
    D08_0     when "01010" | "01011",
    D08_1     when "10010" | "10011",
    D08_2     when "01110" | "01111",
    D08_3     when "10110" | "10111",
    D16_01    when "00010" | "00011",
    D16_23    when "00110" | "00111",
    D32       when "00001",
    D64       when "00000",
    TypeError when others;

  with s_typeOfDataTransferSelect select s_DataShift <=
    b"001000" when "01010",
    b"001000" when "01011",
    b"001000" when "01110",
    b"001000" when "01111",
    b"000000" when others;

  -- Address modifier decoder
  -- Either the supervisor or user access modes are supported
  s_addressingTypeSelect <= s_AMlatched;

  with s_addressingTypeSelect select s_addressingType <=
    A24      when c_AM_A24_S_SUP,
    A24      when c_AM_A24_S,
    A24_BLT  when c_AM_A24_BLT,
    A24_BLT  when c_AM_A24_BLT_SUP,
    A24_MBLT when c_AM_A24_MBLT,
    A24_MBLT when c_AM_A24_MBLT_SUP,
    CR_CSR   when c_AM_CR_CSR,
    A16      when c_AM_A16,
    A16      when c_AM_A16_SUP,
    A32      when c_AM_A32,
    A32      when c_AM_A32_SUP,
    A32_BLT  when c_AM_A32_BLT,
    A32_BLT  when c_AM_A32_BLT_SUP,
    A32_MBLT when c_AM_A32_MBLT,
    A32_MBLT when c_AM_A32_MBLT_SUP,
    A64      when c_AM_A64,
    A64_BLT  when c_AM_A64_BLT,
    A64_MBLT when c_AM_A64_MBLT,
    TWOedge  when c_AM_2EVME_6U,
    TWOedge  when c_AM_2EVME_3U,
    AM_Error when others;

  -- Transfer type decoder
  with s_addressingType select s_transferType <=
    SINGLE when A24,
    SINGLE when CR_CSR,
    SINGLE when A16,
    SINGLE when A32,
    SINGLE when A64,
    BLT    when A24_BLT,
    BLT    when A32_BLT,
    BLT    when A64_BLT,
    MBLT   when A24_MBLT,
    MBLT   when A32_MBLT,
    MBLT   when A64_MBLT,
    TWOe   when TWOedge,
    error  when others;

  s_addrWidth <= s_addrWidth1;

  -- To implement the A32 2eVME and A32 2eSST accesses the following logic
  -- must be changed:
  with s_addressingType select s_addrWidth1 <=
    "00" when A16,
    "01" when A24,
    "01" when A24_BLT,
    "01" when A24_MBLT,
    "01" when CR_CSR,
    "10" when A32,
    "10" when A32_BLT,
    "10" when A32_MBLT,
    "11" when others;

  ------------------------------------------------------------------------------
  -- MAIN FSM
  ------------------------------------------------------------------------------
  p_VMEmainFSM : process (clk_i)
  begin
    if rising_edge(clk_i) then
      if rst_i = '1' or s_mainFSMreset = '1' then
        -- FSM resetted after power up,
        -- software reset, manually reset,
        -- on rising edge of AS.
        s_memReq         <= '0';
        s_decode         <= '0';
        s_dtackOE        <= '0';
        s_mainDTACK      <= '1';
        s_dataDir        <= '0';
        s_dataOE         <= '0';
        s_addrDir        <= '0';
        s_addrOE         <= '0';
        s_DSlatch        <= '0';
        s_incrementAddr  <= '0';
        s_dataPhase      <= '0';
        s_dataToOutput   <= '0';
        s_dataToAddrBus  <= '0';
        s_transferActive <= '0';
        --s_2eLatchAddr    <= "00";   -- uncomment if 2e is implemented:
        s_retry          <= '0';
        --s_berr           <= '0';    -- uncomment if 2e is implemented:
        s_BERR_out       <= '0';
        s_mainFSMstate <= IDLE;
      else
        s_memReq         <= '0';
        s_decode         <= '0';
        s_dtackOE        <= '0';
        s_mainDTACK      <= '1';
        s_dataDir        <= '0';
        s_dataOE         <= '0';
        s_addrDir        <= '0';
        s_addrOE         <= '0';
        s_DSlatch        <= '0';
        s_incrementAddr  <= '0';
        s_dataPhase      <= '0';
        s_dataToOutput   <= '0';
        s_dataToAddrBus  <= '0';
        s_transferActive <= '0';
        --s_2eLatchAddr    <= "00";   -- uncomment if 2e is implemented:
        s_retry          <= '0';
        --s_berr           <= '0';    -- uncomment if 2e is implemented:
        s_BERR_out       <= '0';

        case s_mainFSMstate is

          when IDLE =>
            -- During the Interrupt ack cycle the Slave can't be accessed
            -- so if VME_IACK_n_i is asserted the FSM is in IDLE state.
            -- The VME_IACK_n_i signal is asserted by the Interrupt handler
            -- during all the Interrupt cycle.
            if s_VMEaddrLatch = '1' and VME_IACK_n_i = '1' then
              -- if AS falling edge --> go in DECODE_ACCESS
              s_mainFSMstate <= DECODE_ACCESS;
            else
              s_mainFSMstate <= IDLE;
            end if;

          when DECODE_ACCESS =>
            -- check if this slave board is addressed and if it is, check
            -- the access mode
            s_decode  <= '1';
            s_DSlatch <= '1';
            -- uncomment for using 2e modes:
            --if s_addressingType = TWOedge then
            ---- start 2e transfer
            --s_mainFSMstate <= WAIT_FOR_DS_2e;
            if s_conf_sel = '1' or s_card_sel = '1' then
              -- conf_sel = '1' it means CR/CSR space addressed
              -- card_sel = '1' it means WB application addressed
              s_mainFSMstate <= WAIT_FOR_DS;
            else
              s_mainFSMstate <= DECODE_ACCESS;
              -- another board will answer; wait here the rising edge on
              -- VME_AS_i
            end if;

          when WAIT_FOR_DS =>
            -- wait until DS /= "11"
            s_dtackOE        <= '1';
            s_addrDir        <= (s_is_d64) and VME_WRITE_n_i;
            s_DSlatch        <= '1';
            s_dataPhase      <= s_dataPhase;
            s_transferActive <= '1';

            if VME_DS_n_i /= "11" then
              s_mainFSMstate <= LATCH_DS1;
            else
              s_mainFSMstate <= WAIT_FOR_DS;
            end if;

          when LATCH_DS1 =>
            -- this state is necessary indeed the VME master can assert the
            -- DS lines not at the same time
            s_dtackOE        <= '1';
            s_dataDir        <= VME_WRITE_n_i;
            s_addrDir        <= (s_is_d64) and VME_WRITE_n_i;
            s_DSlatch        <= '1';
            s_dataPhase      <= s_dataPhase;
            s_transferActive <= '1';
            if s_num_latchDS = 1 then
              s_mainFSMstate <= CHECK_TRANSFER_TYPE;
            else
              s_mainFSMstate <= LATCH_DS2;
            end if;

          when LATCH_DS2 =>
            -- this state is necessary indeed the VME master can assert the
            -- DS lines not at the same time
            s_dtackOE        <= '1';
            s_dataDir        <= VME_WRITE_n_i;
            s_addrDir        <= (s_is_d64) and VME_WRITE_n_i;
            s_DSlatch        <= '1';
            s_dataPhase      <= s_dataPhase;
            s_transferActive <= '1';
            if s_num_latchDS = 2 then
              s_mainFSMstate <= CHECK_TRANSFER_TYPE;
            else
              s_mainFSMstate <= LATCH_DS3;
            end if;

          when LATCH_DS3 =>
            -- this state is necessary indeed the VME master can assert the
            -- DS lines not at the same time
            s_dtackOE        <= '1';
            s_dataDir        <= VME_WRITE_n_i;
            s_addrDir        <= (s_is_d64) and VME_WRITE_n_i;
            s_DSlatch        <= '1';
            s_dataPhase      <= s_dataPhase;
            s_transferActive <= '1';
            if s_num_latchDS = 3 then
              s_mainFSMstate <= CHECK_TRANSFER_TYPE;
            else
              s_mainFSMstate <= LATCH_DS4;
            end if;

          when LATCH_DS4 =>
            -- this state is necessary indeed the VME master can assert the
            -- DS lines not at the same time
            s_dtackOE        <= '1';
            s_dataDir        <= VME_WRITE_n_i;
            s_addrDir        <= (s_is_d64) and VME_WRITE_n_i;
            s_DSlatch        <= '1';
            s_dataPhase      <= s_dataPhase;
            s_transferActive <= '1';

            s_mainFSMstate <= CHECK_TRANSFER_TYPE;

          when CHECK_TRANSFER_TYPE =>
            s_dtackOE        <= '1';
            s_dataDir        <= VME_WRITE_n_i;
            s_addrDir        <= (s_is_d64) and VME_WRITE_n_i;
            s_dataPhase      <= s_dataPhase;
            s_transferActive <= '1';
            if (s_transferType = SINGLE or s_transferType = BLT) and
               (s_addrWidth /= "11")
            then
              s_mainFSMstate <= MEMORY_REQ;
              s_memReq <= '1';
            elsif (s_transferType = MBLT or s_addrWidth = "11") and
                  (s_dataPhase = '0')
            then
              s_mainFSMstate <= DTACK_LOW;
            elsif (s_transferType = MBLT or s_addrWidth = "11") and
                  (s_dataPhase = '1')
            then
              s_mainFSMstate <= MEMORY_REQ;
              s_memReq <= '1';
            end if;

          when MEMORY_REQ =>
            -- To request the memory CR/CSR or WB memory it is sufficient to
            -- generate a pulse on s_memReq signal
            s_dtackOE        <= '1';
            s_dataDir        <= VME_WRITE_n_i;
            s_addrDir        <= (s_is_d64) and VME_WRITE_n_i;
            s_dataPhase      <= s_dataPhase;
            s_transferActive <= '1';
            if s_memAck = '1' and VME_WRITE_n_i = '0' then
              s_mainFSMstate <= DTACK_LOW;
            elsif s_memAck = '1' and VME_WRITE_n_i = '1' then
              if s_transferType = MBLT then
                s_dataToAddrBus <= '1';
              else
                s_dataToOutput <= '1';
              end if;
              s_mainFSMstate <= DATA_TO_BUS;
            else
              s_mainFSMstate <= MEMORY_REQ;
            end if;

          when DATA_TO_BUS =>
            s_dtackOE        <= '1';
            s_dataDir        <= VME_WRITE_n_i;
            s_addrDir        <= (s_is_d64) and VME_WRITE_n_i;
            s_dataPhase      <= s_dataPhase;
            s_transferActive <= '1';
            s_dataToAddrBus  <= s_dataToAddrBus;
            s_dataToOutput   <= s_dataToOutput;
            s_mainFSMstate   <= DTACK_LOW;

          when DTACK_LOW =>
            s_dtackOE        <= '1';
            s_dataDir        <= VME_WRITE_n_i;
            s_addrDir        <= (s_is_d64) and VME_WRITE_n_i;
            s_dataPhase      <= s_dataPhase;
            s_transferActive <= '1';

            if s_BERRcondition = '0' and s_rty1 = '0' then
              s_mainDTACK     <= '0';
            elsif s_BERRcondition = '0' and s_rty1 = '1' then
              s_retry         <= '1';
            else
              s_BERR_out      <= '1';
            end if;

            if VME_DS_n_i = "11" then
              s_mainFSMstate  <= DECIDE_NEXT_CYCLE;
              s_dataDir       <= '0';
            else
              s_mainFSMstate <= DTACK_LOW;
            end if;

          when DECIDE_NEXT_CYCLE =>
            s_dtackOE        <= '1';
            s_addrDir        <= (s_is_d64) and VME_WRITE_n_i;
            s_dataPhase      <= s_dataPhase;
            s_transferActive <= '1';
            if (s_transferType = SINGLE and s_addrWidth /= "11") or
               (s_transferType = SINGLE and s_addrWidth = "11" and s_dataPhase = '1')
            then
              s_mainFSMstate <= WAIT_FOR_DS;
            elsif (s_transferType = BLT and s_addrWidth /= "11") or
                  (s_transferType = BLT and s_addrWidth = "11" and s_dataPhase = '1') or
                  (s_transferType = MBLT and s_dataPhase = '1')
            then
              s_mainFSMstate <= INCREMENT_ADDR;
            elsif (s_transferType = MBLT or s_addrWidth = "11") and
                  (s_dataPhase = '0')
            then
              s_mainFSMstate <= SET_DATA_PHASE;
            else
              s_mainFSMstate <= DECIDE_NEXT_CYCLE;
            end if;

          when INCREMENT_ADDR =>
            s_dtackOE        <= '1';
            s_addrDir        <= (s_is_d64) and VME_WRITE_n_i;
            s_dataPhase      <= s_dataPhase;
            s_transferActive <= '1';
            s_incrementAddr  <= '1';
            s_mainFSMstate   <= WAIT_FOR_DS;

          when SET_DATA_PHASE =>
            s_dtackOE        <= '1';
            s_addrDir        <= (s_is_d64) and VME_WRITE_n_i;
            s_dataPhase      <= '1';
            s_transferActive <= '1';
            s_mainFSMstate   <= WAIT_FOR_DS;

          when others =>
            s_mainFSMstate <= IDLE;

        end case;
      end if;
    end if;
  end process;

  ------------------------------------------------------------------------------
  -- Retry and Error Drivers
  ------------------------------------------------------------------------------
  -- s_rty is asserted by the WB application
  -- s_retry is used during the 2e cycles.
  -- 2.3.13 Retry Capability...vme64 ANSI/VITA 1-1994 (R2002):
  -- Master supports retry capability, it terminates the bus cycle when it
  -- detects RETRY* low without waiting for either DTACK* or BERR*.
  -- In the case of a busy condition, if the Master does not support Retry
  -- and does not terminate the cycle the slave waits and asserts DTACK* low
  -- once the busy resource becomes available, if the Bus Timer does not
  -- detect a time out condition before.
  -- Please note that the VME_WB_Master component supports single write/read
  -- pipelined cycles, so the WB Slave should drive the stall signal to '1' if
  -- the resource is busy
  p_RETRYdriver : process (clk_i)
  begin
    if rising_edge(clk_i) then
      if s_retry = '1' then
        VME_RETRY_n_o  <= '0';
        VME_RETRY_OE_o <= '1';
      else
        VME_RETRY_n_o  <= '1';
        VME_RETRY_OE_o <= '0';
      end if;
    end if;
  end process;

  -- BERR driver
  -- The slave asserts the Error line during the Decode access phase when an
  -- error condition is detected and the s_BERRcondition is asserted.
  -- When the FSM is in the DTACK_LOW state one of the VME_DTACK and VME_BERR
  -- lines is asserted.
  -- The VME_BERR line can not be asserted by the slave at anytime, but only
  -- during the DTACK_LOW state; this to avoid that one temporary error
  -- condition during the decode access phase causes an undesired assertion of
  -- VME_BERR line.
  p_BERRdriver : process (clk_i)
  begin
    if rising_edge(clk_i) then
      -- uncomment if 2e is implemented:
      --    s_berr_1      <= s_berr;
      --    s_berr_2      <= s_berr and s_berr_1;
      if (s_BERR_out = '1') then
        VME_BERR_n_o <= '0';
      else
        VME_BERR_n_o <= '1';
      end if;
    end if;
  end process;

  -- This process detects an error condition and assert the s_BERRcondition
  -- signal. A transfer cycle is terminated with assertion of this signal if
  -- the VME64x slave does not recognize the data or addressing type used in
  -- the transfer cycle, if a Master attempts to access in BLT mode with D08
  -- or D16, if the master attempts to access in MBLT mode and the WB data bus
  -- is 32 bits, or if the read only memory is addressed during a write only
  -- cycle!
  process (clk_i)
  begin
    if rising_edge(clk_i) then
      if rst_i = '1' then
        s_BERRcondition <= '0';
      elsif
        ((s_transferType = error or s_wberr1 = '1') and s_transferActive = '1') or
        (s_typeOfDataTransfer = TypeError) or
        (s_addressingType = AM_Error) or
        (s_blockTransferLimit = '1') or
        (s_transferType = BLT and (not(s_typeOfDataTransfer = D32 or s_typeOfDataTransfer = D64))) or
        (s_transferType = MBLT and s_typeOfDataTransfer /= D64) or
        (s_is_d64 = '1' and g_WB_DATA_WIDTH = 32)
      then
        s_BERRcondition <= '1';
      else
        s_BERRcondition <= '0';
      end if;
    end if;
  end process;

  -- generate the error condition if block transfer overflows the limit
  -- BLT  --> block transfer limit = 256 bytes  (rule 2.12a ANSI/VITA 1-1994)
  -- MBLT --> block transfer limit = 2048 bytes (rule 2.78  ANSI/VITA 1-1994)
  with s_transferType select s_blockTransferLimit <=
    s_addrOffset(8)  when BLT,
    s_addrOffset(11) when MBLT,
    '0'              when others;

  -- wb err handler
  process (clk_i)
  begin
    if rising_edge(clk_i) then
      if s_mainFSMreset = '1' or rst_i = '1' then
        s_wberr1 <= '0';
      elsif s_err = '1' then
        s_wberr1 <= '1';
      end if;
    end if;
  end process;

  -- wb retry handler
  process (clk_i)
  begin
    if rising_edge(clk_i) then
      if s_mainFSMreset = '1' or rst_i = '1' then
        s_rty1 <= '0';
      elsif s_rty = '1' then
        s_rty1 <= '1';
      end if;
    end if;
  end process;

  -- These two mux are inserted to provide the MBLT access mode
  p_ADDRmux : process (clk_i)
  begin
    if rising_edge(clk_i) then
      if s_dataToAddrBus = '1' then
        VME_ADDR_o    <= s_locDataSwap(63 downto 33);
        VME_LWORD_n_o <= s_locDataSwap(32);
      end if;
    end if;
  end process;

  p_DATAmux : process (clk_i)
  begin
    if rising_edge(clk_i) then
      if s_dataToAddrBus = '1' or s_dataToOutput = '1' then
        if s_addressingType = CR_CSR then
          VME_DATA_o <= std_logic_vector(s_locData(31 downto 0));
        else
          VME_DATA_o <= s_locDataSwap(31 downto 0);
        end if;
      end if;
    end if;
  end process;

  ------------------------------------------------------------------------------
  -- Address Handler Process
  ------------------------------------------------------------------------------
  --Local address & AM & 2e address phase latching
  s_VMEaddrInput <= unsigned(VME_ADDR_i);
  s_LWORDinput   <= VME_LWORD_n_i;
  s_VMEdataInput <= unsigned(VME_DATA_i);

  p_addrLatching : process (clk_i)
  begin
    if rising_edge(clk_i) then
      if rst_i = '1' or s_mainFSMreset = '1' then
        s_VMEaddrLatched <= (others => '0');
        s_LWORDlatched   <= '0';
        s_AMlatched      <= (others => '0');
      else
        if s_VMEaddrLatch = '1' then  -- Latching on falling edge of VME_AS_n_i
          s_VMEaddrLatched <= s_VMEdataInput & s_VMEaddrInput;
          s_LWORDlatched   <= s_LWORDinput;
          s_AMlatched      <= VME_AM_i;
        end if;
      end if;
    end if;
  end process;

  with s_addrWidth select s_locAddrBeforeOffset(63 downto 1) <=
    x"000000000000" & s_VMEaddrLatched(15 downto 1) when "00",
    x"0000000000" & s_VMEaddrLatched(23 downto 1)   when "01",
    x"00000000" & s_VMEaddrLatched(31 downto 1)     when "10",
    s_VMEaddrLatched(63 downto 1)                   when others;

  with s_DSlatched select s_locAddrBeforeOffset(0) <=
    '0' when "01",
    '1' when "10",
    '0' when others;

  s_phase1addr <= (others => '0');
  s_phase2addr <= (others => '0');
  s_locAddr2e <= s_phase1addr(63 downto 8) & s_phase2addr(7 downto 0);

  -- This process generates the s_locAddr that is used during the access decode
  -- process; If the board is addressed the VME_Access_Decode component
  -- generates the s_base_addr.
  -- The s_rel_locAddr is used in the VME_WB_master to address the WB memory
  process (clk_i)
  begin
    if rising_edge(clk_i) then
      if s_addressingType = TWOedge then
        s_rel_locAddr <= s_locAddr2e + s_addrOffset-s_base_addr;
        s_locAddr     <= s_locAddr2e;
      elsif s_addressingType = CR_CSR then
        s_locAddr     <= s_locAddrBeforeOffset;
      else
        s_rel_locAddr <= s_locAddrBeforeOffset + s_addrOffset-s_base_addr;
        s_locAddr     <= s_locAddrBeforeOffset;
      end if;
    end if;
  end process;

  -- Local address incrementing
  -- This process generates the s_addrOffset
  -- The s_addrOffset is /= 0 during BLT, MBLT and 2e access modes, when
  -- the vme64x core increments the address every cycle
  p_addrIncrementing : process (clk_i)
  begin
    if rising_edge(clk_i) then
      if rst_i = '1' or s_mainFSMreset = '1' then
        s_addrOffset <= (others => '0');
      elsif s_incrementAddr = '1' then
        if s_addressingType = TWOedge then
          s_addrOffset <= s_addrOffset + 8;      -- the TWOedge access is D64
        else
          if s_typeOfDataTransfer = D08_0 or
             s_typeOfDataTransfer = D08_1 or
             s_typeOfDataTransfer = D08_2 or
             s_typeOfDataTransfer = D08_3
          then
            s_addrOffset <= s_addrOffset + 1;
          elsif s_typeOfDataTransfer = D16_01 or
                s_typeOfDataTransfer = D16_23
          then
            s_addrOffset <= s_addrOffset + 2;
          elsif s_typeOfDataTransfer = D64 then
            if s_transferType = MBLT then
              s_addrOffset <= s_addrOffset + 8;
            else
              s_addrOffset <= s_addrOffset + 4;  --BLT D32
            end if;
          elsif s_typeOfDataTransfer = D32 then  --BLT D32
            s_addrOffset <= s_addrOffset + 4;
          else
            s_addrOffset <= s_addrOffset;
          end if;
        end if;
      else
        s_addrOffset <= s_addrOffset;
      end if;
    end if;
  end process;

  ------------------------------------------------------------------------------
  -- Data Handler Process
  ------------------------------------------------------------------------------
  -- Data strobe latching
  p_DSlatching : process (clk_i)
  begin
    if rising_edge(clk_i) then
      if rst_i = '1' then
        s_DSlatched <= (others => '0');
      else
        if s_DSlatch = '1' then
          s_DSlatched <= VME_DS_ant_n_i;
        end if;
      end if;
    end if;
  end process;

  s_VMEdata64In(63 downto 33) <= s_VMEaddrInput(31 downto 1);
  s_VMEdata64In(32)           <= s_LWORDinput;
  s_VMEdata64In(31 downto 0)  <= s_VMEdataInput(31 downto 0);

  -- This process aligns the VME data input in the lsb
  process (clk_i)
  begin
    if rising_edge(clk_i) then
      s_locDataIn <= unsigned(s_VMEdata64In) srl to_integer(unsigned(s_DataShift));
    end if;
  end process;

  -- Swap the data during read or write operation
  -- sel= 000 --> No swap
  -- sel= 001 --> Swap Byte                      eg: 01234567 become 10325476
  -- sel= 010 --> Swap Word                      eg: 01234567 become 23016745
  -- sel= 011 --> Swap Word+Swap Byte            eg: 01234567 become 32107654
  -- sel= 100 --> Swap DWord+Swap Word+Swap Byte eg: 01234567 become 76543210
  swapper_write : VME_swapper
    port map (
      d_i => std_logic_vector(s_locDataIn),
      sel => endian_i,
      d_o => s_locDataInSwap
    );

  swapper_read : VME_swapper
    port map (
      d_i => std_logic_vector(s_locData),
      sel => endian_i,
      d_o => s_locDataSwap
    );

  -- Data from WB or CR/CSR to VME bus


  process (clk_i)
  begin
    if rising_edge(clk_i) then
      if s_card_sel = '1' then
        s_locDataOut <= unsigned(s_locDataOutWb);
      elsif s_conf_sel = '1' then
        s_locDataOut <= resize(unsigned(cr_csr_data_i), s_locDataOut'length);
      else
        s_locDataOut <= (others => '0');
      end if;
    end if;
  end process;

  s_locData(63 downto 0) <= s_locDataOut(63 downto 0) sll to_integer(unsigned(s_DataShift));

  ------------------------------------------------------------------------------
  -- Memory Mapping
  ------------------------------------------------------------------------------
  -- WB bus width = 64-bits
  -- Granularity = byte
  -- WB bus --> BIG ENDIAN

  p_memoryMapping : process (clk_i)
  begin
    if rising_edge(clk_i) then
      if s_transferType = TWOe then
        s_nx_sel <= "11111111";
      else
        case s_typeOfDataTransfer is
          when D08_0 =>
            if s_rel_locAddr(2) = '0' then
              s_nx_sel <= "10000000";
            else
              s_nx_sel <= "00001000";
            end if;
          when D08_1 =>
            if s_rel_locAddr(2) = '0' then
              s_nx_sel <= "01000000";
            else
              s_nx_sel <= "00000100";
            end if;
          when D08_2 =>
            if s_rel_locAddr(2) = '0' then
              s_nx_sel <= "00100000";
            else
              s_nx_sel <= "00000010";
            end if;
          when D08_3 =>
            if s_rel_locAddr(2) = '0' then
              s_nx_sel <= "00010000";
            else
              s_nx_sel <= "00000001";
            end if;
          when D16_01 =>
            if s_rel_locAddr(2) = '0' then
              s_nx_sel <= "11000000";
            else
              s_nx_sel <= "00001100";
            end if;
          when D16_23 =>
            if s_rel_locAddr(2) = '0' then
              s_nx_sel <= "00110000";
            else
              s_nx_sel <= "00000011";
            end if;
          when D64 =>
            case s_transferType is
              when MBLT =>                -- D64
                s_nx_sel <= "11111111";
              when others =>              -- D32 BLT or SINGLE
                if s_rel_locAddr(2) = '0' then
                  s_nx_sel <= "11110000";
                else
                  s_nx_sel <= "00001111";
                end if;
            end case;
          when D32 =>
            if s_rel_locAddr(2) = '1' then
              s_nx_sel <= "00001111";
            else
              s_nx_sel <= "11110000";
            end if;
          when others =>
            s_nx_sel <= "00000000";
        end case;
      end if;
    end if;
  end process;

  s_sel <= unsigned(s_nx_sel);

  ------------------------------------------------------------------------------
  -- WB Master
  ------------------------------------------------------------------------------
  -- This component acts as WB master for single read/write PIPELINED mode.
  -- The data and address lines are shifted inside this component.

  s_wbMaster_rst <= rst_i or s_mainFSMreset;

  Inst_Wb_master : VME_Wb_master
    generic map (
      g_WB_DATA_WIDTH => g_WB_DATA_WIDTH,
      g_WB_ADDR_WIDTH => g_WB_ADDR_WIDTH
    )
    port map (
      memReq_i        => s_memReq,
      clk_i           => clk_i,
      cardSel_i       => s_card_sel,
      reset_i         => s_wbMaster_rst,
      BERRcondition_i => s_BERRcondition,
      sel_i           => std_logic_vector(s_sel),
      locDataInSwap_i => s_locDataInSwap,
      locDataOut_o    => s_locDataOutWb,
      rel_locAddr_i   => std_logic_vector(s_rel_locAddr),
      memAckWb_o      => s_AckWb,
      err_o           => s_err,
      rty_o           => s_rty,
      RW_i            => VME_WRITE_n_i,
      stall_i         => stall_i,
      rty_i           => rty_i,
      err_i           => err_i,
      cyc_o           => cyc_o,
      memReq_o        => stb_o,
      WBdata_o        => dat_o,
      wbData_i        => dat_i,
      locAddr_o       => adr_o,
      memAckWB_i      => ack_i,
      WbSel_o         => sel_o,
      RW_o            => s_rw
    );

  we_o <= not s_rw;

  ------------------------------------------------------------------------------
  -- Function Decoder
  ------------------------------------------------------------------------------
  s_base_addr    <= unsigned(addr_decoder_i);
  addr_decoder_o <= std_logic_vector(s_locAddr);
  decode_o       <= s_decode;
  am_o           <= s_amlatched;
  xam_o          <= (others => '0');
  s_card_sel     <= sel_i and module_enable_i;

  -- Decode accesses to CR/CSR
  s_conf_sel <= '1' when s_locAddr(23 downto 19) = unsigned(bar_i) and
                         s_amlatched = c_AM_CR_CSR
                    else '0';

  ------------------------------------------------------------------------------
  -- Acknowledge
  ------------------------------------------------------------------------------
  -- s_memAck should be asserted also if an error condition is detected.
  process (clk_i)
  begin
    if rising_edge(clk_i) then
      s_memAck <= s_memAckCSR or s_AckWb or s_err;
    end if;
  end process;
  -- CR/CSR memory acknowledge

  p_memAckCSR : process (clk_i)
  begin
    if rising_edge(clk_i) then
      if rst_i = '1' then
        s_memAckCSR <= '0';
      else
        if s_memReq = '1' and s_conf_sel = '1' then
          s_memAckCSR <= '1';
        else
          s_memAckCSR <= '0';
        end if;
      end if;
    end if;
  end process;

  ------------------------------------------------------------------------------
  -- CR/CSR In/Out
  ------------------------------------------------------------------------------
  cr_csr_data_o <= std_logic_vector(s_locDataIn(7 downto 0));
  cr_csr_addr_o <= std_logic_vector(s_locAddr(18 downto 2));

  cr_csr_we_o   <= '1' when s_memReq   = '1' and
                            s_conf_sel = '1' and
                            s_RW       = '0'
                            else '0';

  ------------------------------------------------------------------------------
  -- Edge Detection and Sampling
  ------------------------------------------------------------------------------
  process (clk_i)
  begin
    if rising_edge(clk_i) then
      s_prev_VME_AS_n <= VME_AS_n_i;
      s_VMEaddrLatch  <= '0';
      s_mainFSMreset  <= '0';

      if VME_AS_n_i = '0' and s_prev_VME_AS_n = '1' then
        s_VMEaddrLatch <= '1';
      end if;

      if VME_AS_n_i = '1' and s_prev_VME_AS_n = '0' then
        s_mainFSMreset <= '1';
      end if;
    end if;
  end process;

end RTL;
