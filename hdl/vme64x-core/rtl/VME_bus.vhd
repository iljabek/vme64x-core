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
    sel_o           : out std_logic_vector(3 downto 0);
    we_o            : out std_logic;
    cyc_o           : out std_logic;
    err_i           : in  std_logic;
    rty_i           : in  std_logic;
    stall_i         : in  std_logic;

    -- Function decoder
    addr_decoder_i  : in  std_logic_vector(31 downto 0);
    addr_decoder_o  : out std_logic_vector(31 downto 0);
    decode_start_o  : out std_logic;
    decode_done_i   : in std_logic;
    am_o            : out std_logic_vector( 5 downto 0);
    decode_sel_i    : in  std_logic;

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

  -- External buffer signals
  signal s_dtackOE                  : std_logic;
  signal s_dataDir                  : std_logic;
  signal s_addrDir                  : std_logic;

  -- Local data & address
  signal s_locDataIn                : std_logic_vector(63 downto 0);
  signal s_locDataOut               : std_logic_vector(63 downto 0);          -- Local data
  signal s_locAddr                  : std_logic_vector(31 downto 0);          -- Local address
  signal s_locDataOutSwap           : std_logic_vector(63 downto 0);
  signal s_locDataInSwap            : std_logic_vector(63 downto 0);
  signal s_locDataOutWb             : std_logic_vector(63 downto 0);

  -- VME latched signals
  signal s_ADDRlatched              : std_logic_vector(31 downto 1);
  signal s_LWORDlatched_n           : std_logic;
  signal s_DSlatched                : std_logic_vector(1 downto 0);
  signal s_AMlatched                : std_logic_vector(5 downto 0);

  type t_addressingType is (
    A24,
    A24_BLT,
    A24_MBLT,
    CR_CSR,
    A16,
    A32,
    A32_BLT,
    A32_MBLT,
    AM_Error
  );

  type t_transferType is (
    SINGLE,
    BLT,
    MBLT,
    error
  );

  -- Addressing type (depending on VME_AM_i)
  signal s_addressingType           : t_addressingType;
  signal s_transferType             : t_transferType;

  type t_mainFSMstates is (
    -- Wait until AS is asserted.
    IDLE,

    -- Reformat address according to AM.
    REFORMAT_ADDRESS,

    -- Decoding ADDR and AM (selecting card or conf).
    DECODE_ACCESS,

    -- Wait until DS is asserted.
    WAIT_FOR_DS,

    -- Wait until DS is stable (and asserted).
    LATCH_DS,

    -- Decode DS, generate WB request
    CHECK_TRANSFER_TYPE,

    -- Wait for WB reply
    MEMORY_REQ,

    -- For read cycle, put data on the bus
    DATA_TO_BUS,

    -- Assert DTACK
    DTACK_LOW,

    --  Increment address for block transfers
    INCREMENT_ADDR,

    --  Wait until AS is deasserted
    WAIT_END
  );

  -- Main FSM signals
  signal s_mainFSMstate             : t_mainFSMstates;
  signal s_mainDTACK                : std_logic;   -- DTACK driving
  signal s_memReq                   : std_logic;   -- Global memory request
  signal s_WBReq                    : std_logic;   -- WB memory request
  signal s_dataPhase                : std_logic;   -- for MBLT
  signal s_transferActive           : std_logic;   -- active VME transfer
  signal s_retry                    : std_logic;   -- RETRY signal

  -- Access decode signals
  signal s_conf_sel                 : std_logic;   -- CR or CSR is addressed
  signal s_card_sel                 : std_logic;   -- WB memory is addressed

  -- WishBone signals
  signal s_sel                      : std_logic_vector(3 downto 0);  -- SEL WB

  -- Error signals
  signal s_BERRcondition            : std_logic;   -- Condition to set BERR
  signal s_wberr1                   : std_logic;
  signal s_rty1                     : std_logic;

  -- Initialization signals
  signal s_is_d64                   : std_logic;

  signal s_BERR_out                 : std_logic;
  signal s_sw_reset                 : std_logic;

  --  Set on the cycle to decode access (ADDR + AM)
  signal s_AckWb                    : std_logic;
  signal s_err                      : std_logic;
  signal s_rty                      : std_logic;

  signal s_wbMaster_rst             : std_logic;

  -- Calculate the number of LATCH DS states necessary to match the timing
  -- rule 2.39 page 113 VMEbus specification ANSI/IEEE STD1014-1987.
  -- (max skew for the slave is 20 ns)
  constant num_latchDS              : natural range 1 to 8 :=
    (20 + g_CLOCK_PERIOD - 1) / g_CLOCK_PERIOD;

  signal s_DS_latch_count           : unsigned (2 downto 0);

  --  True if endianness converters are supported.
  constant c_SWAPPER_EN : boolean := False;
begin
  --  Consistency check.
  assert g_WB_DATA_WIDTH = 32 report "g_WB_DATA_WIDTH must be set to 32"
    severity failure;
  
  -- These output signals are connected to the buffers on the board
  -- SN74VMEH22501A Function table:  (A is fpga, B is VME connector)
  --   OEn | DIR | OUTPUT                 OEAB   |   OEBYn   |   OUTPUT
  --    H  |  X  |   Z                      L    |     H     |     Z
  --    L  |  H  | A to B                   H    |     H     |   A to B
  --    L  |  L  | B to A                   L    |     L     |   B to Y
  --                                        H    |     L     |A to B, B to Y |

  VME_DATA_DIR_o  <= s_dataDir;
  VME_DATA_OE_N_o <= '0'; -- Driven IFF DIR = 1
  VME_ADDR_DIR_o  <= s_addrDir;
  VME_ADDR_OE_N_o <= '0'; -- Driven IFF DIR = 1
  VME_DTACK_OE_o  <= s_dtackOE;
  VME_DTACK_n_o <= s_mainDTACK;

  ------------------------------------------------------------------------------
  -- Access Mode Decoders
  ------------------------------------------------------------------------------
  -- Type of data transfer decoder
  -- VME64 ANSI/VITA 1-1994...Table 2-2 "Signal levels during data transfers"
  -- A2 is used to select the D64 type  (D64 --> MBLT and 2edge cycles)
  -- VME DATA --> BIG ENDIAN


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

  -- Address modifier decoder
  -- Either the supervisor or user access modes are supported
  with s_AMlatched select s_addressingType <=
    A24      when c_AM_A24_S_SUP | c_AM_A24_S,
    A24_BLT  when c_AM_A24_BLT | c_AM_A24_BLT_SUP,
    A24_MBLT when c_AM_A24_MBLT | c_AM_A24_MBLT_SUP,
    CR_CSR   when c_AM_CR_CSR,
    A16      when c_AM_A16 | c_AM_A16_SUP,
    A32      when c_AM_A32 | c_AM_A32_SUP,
    A32_BLT  when c_AM_A32_BLT | c_AM_A32_BLT_SUP,
    A32_MBLT when c_AM_A32_MBLT | c_AM_A32_MBLT_SUP,
    AM_Error when others;

  -- Transfer type decoder
  with s_addressingType select s_transferType <=
    SINGLE when A24 | CR_CSR | A16 | A32,
    BLT    when A24_BLT | A32_BLT,
    MBLT   when A24_MBLT | A32_MBLT,
    error  when others;

  -- Used to drive the VME_ADDR_DIR_o
  s_is_d64 <= '1' when s_transferType = MBLT else '0';


  ------------------------------------------------------------------------------
  -- MAIN FSM
  ------------------------------------------------------------------------------
  p_VMEmainFSM : process (clk_i) is
    variable addr_word_incr : natural range 0 to 7;
  begin
    if rising_edge(clk_i) then
      if rst_i = '1' or VME_AS_n_i = '1' then
        -- FSM resetted after power up,
        -- software reset, manually reset,
        -- on rising edge of AS.
        s_memReq         <= '0';
        decode_start_o   <= '0';
        s_dtackOE        <= '0';
        s_mainDTACK      <= '1';
        s_dataDir        <= '0';
        s_addrDir        <= '0';
        s_dataPhase      <= '0';
        s_transferActive <= '0';
        s_retry          <= '0';
        s_BERR_out       <= '0';
        s_mainFSMstate <= IDLE;
        s_sel <= "0000";

        s_ADDRlatched    <= (others => '0');
        s_LWORDlatched_n <= '0';
        s_AMlatched      <= (others => '0');

        VME_ADDR_o    <= (others => '0');
        VME_LWORD_n_o <= '1';
        VME_DATA_o    <= (others => '0');

        s_card_sel <= '0';
        s_conf_sel <= '0';
      else
        s_memReq         <= '0';
        decode_start_o   <= '0';
        s_dtackOE        <= '0';
        s_mainDTACK      <= '1';
        s_dataDir        <= '0';
        s_addrDir        <= '0';
        s_dataPhase      <= '0';
        s_transferActive <= '0';
        s_retry          <= '0';
        s_BERR_out       <= '0';

        s_DS_latch_count <= "000";

        case s_mainFSMstate is

          when IDLE =>
            -- During the Interrupt ack cycle the Slave can't be accessed
            -- so if VME_IACK_n_i is asserted the FSM is in IDLE state.
            -- The VME_IACK_n_i signal is asserted by the Interrupt handler
            -- during all the Interrupt cycle.
            if VME_AS_n_i = '0' and VME_IACK_n_i = '1' then
              -- if AS falling edge --> start access
              s_mainFSMstate <= REFORMAT_ADDRESS;

              -- Store ADDR, AM and LWORD
              s_ADDRlatched    <= VME_ADDR_i;
              s_LWORDlatched_n <= VME_LWORD_n_i;
              s_AMlatched      <= VME_AM_i;

            else
              s_mainFSMstate <= IDLE;
            end if;

          when REFORMAT_ADDRESS =>
            -- Reformat address according to the mode (A16, A24, A32)
            -- FIXME: not needed if ADEM are correctly reduced to not compare
            -- MSBs of A16 or A24 addresses.
            case s_addressingType is
              when A16 =>
                s_ADDRlatched (31 downto 16) <= (others => '0');  -- A16
              when A24 | A24_BLT | A24_MBLT =>
                s_ADDRlatched (31 downto 24) <= (others => '0');  -- A24
              when others =>
                null;  -- A32
            end case;

            if s_ADDRlatched(23 downto 19) = bar_i
              and s_AMlatched = c_AM_CR_CSR
            then
              -- conf_sel = '1' it means CR/CSR space addressed
              s_conf_sel <= '1';
              s_mainFSMstate <= WAIT_FOR_DS;
            else
              s_mainFSMstate <= DECODE_ACCESS;
              decode_start_o  <= '1';
            end if;

          when DECODE_ACCESS =>
            -- check if this slave board is addressed and if it is, check
            -- the access mode

            if decode_done_i = '1' then
              if decode_sel_i = '1' and module_enable_i = '1' then
                -- card_sel = '1' it means WB application addressed
                s_card_sel <= '1';
                s_mainFSMstate <= WAIT_FOR_DS;
                -- Keep only the local part of the address
                s_ADDRlatched <= addr_decoder_i (31 downto 1);
              else
                -- another board will answer; wait here the rising edge on
                -- VME_AS_i (done by top if).
                s_mainFSMstate <= WAIT_END;
              end if;
            else
              -- Not yet decoded.
              s_mainFSMstate <= DECODE_ACCESS;
            end if;

          when WAIT_FOR_DS =>
            -- wait until DS /= "11"
            s_dtackOE        <= '1';
            s_addrDir        <= (s_is_d64) and VME_WRITE_n_i;
            s_dataPhase      <= s_dataPhase;
            s_transferActive <= '1';

            if VME_DS_n_i /= "11" then
              s_mainFSMstate <= LATCH_DS;
              s_DS_latch_count <= to_unsigned (num_latchDS - 1, 3);
            else
              s_mainFSMstate <= WAIT_FOR_DS;
            end if;

          when LATCH_DS =>
            -- this state is necessary indeed the VME master can assert the
            -- DS lines not at the same time
            s_dtackOE        <= '1';
            s_dataDir        <= VME_WRITE_n_i;
            s_addrDir        <= (s_is_d64) and VME_WRITE_n_i;
            s_dataPhase      <= s_dataPhase;
            s_transferActive <= '1';
            if s_DS_latch_count = 0 then
              s_mainFSMstate <= CHECK_TRANSFER_TYPE;

              -- Read DS (which is delayed to avoid metastability).
              s_DSlatched    <= VME_DS_n_i;

              -- Read DATA (which are stable)
              s_locDataIn(63 downto 33) <= VME_ADDR_i;
              s_locDataIn(32)           <= VME_LWORD_n_i;
              if s_LWORDlatched_n = '1' and s_ADDRlatched(1) = '0' then
                -- Word/byte access with A1=0
                s_locDataIn(31 downto 16)  <= VME_DATA_i(15 downto 0);
                s_locDataIn(15 downto 0) <= VME_DATA_i(15 downto 0);
              else
                s_locDataIn(31 downto 0)  <= VME_DATA_i;
              end if;
            else
              s_mainFSMstate   <= LATCH_DS;
              s_DS_latch_count <= s_DS_latch_count - 1;
            end if;

          when CHECK_TRANSFER_TYPE =>
            s_dtackOE        <= '1';
            s_dataDir        <= VME_WRITE_n_i;
            s_addrDir        <= (s_is_d64) and VME_WRITE_n_i;
            s_dataPhase      <= s_dataPhase;
            s_transferActive <= '1';

            --  Translate DS+LWORD+ADDR to WB byte selects
            if s_LWORDlatched_n = '0' then
              s_sel <= "1111";
            else
              s_sel <= "0000";
              case s_ADDRlatched(1) is
                when '0' =>
                  s_sel (3 downto 2) <= not s_DSlatched;
                when '1' =>
                  s_sel (1 downto 0) <= not s_DSlatched;
                when others =>
                  null;
              end case;
            end if;

            if s_transferType = SINGLE or s_transferType = BLT then
              s_mainFSMstate <= MEMORY_REQ;
              s_memReq <= '1';
            elsif s_transferType = MBLT and s_dataPhase = '0' then
              s_mainFSMstate <= DTACK_LOW;
            elsif s_transferType = MBLT and s_dataPhase = '1' then
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

            if s_conf_sel = '1' or s_AckWb = '1' or s_err = '1' then
              -- WB ack
              if VME_WRITE_n_i = '0' then
                -- Write cycle
                s_mainFSMstate <= DTACK_LOW;
              else
                -- Read cycle

                -- Mux (CS-CSR or WB)
                s_locDataOut <= (others => '0');      
                if s_card_sel = '1' then
                  if s_LWORDlatched_n = '1' and s_ADDRlatched(1) = '0' then
                    -- Word/byte access with A1 = 0
                    s_locDataOut(15 downto 0) <= s_locDataOutWb(31 downto 16);
                  else
                    s_locDataOut <= s_locDataOutWb;
                  end if;
                elsif s_conf_sel = '1' then
                  s_locDataOut(7 downto 0) <= cr_csr_data_i;
                end if;

                s_mainFSMstate <= DATA_TO_BUS;
              end if;
            else
              s_mainFSMstate <= MEMORY_REQ;
            end if;

          when DATA_TO_BUS =>
            s_dtackOE        <= '1';
            s_dataDir        <= VME_WRITE_n_i;
            s_addrDir        <= (s_is_d64) and VME_WRITE_n_i;
            s_dataPhase      <= s_dataPhase;
            s_transferActive <= '1';
            s_mainFSMstate   <= DTACK_LOW;

            if s_transferType = MBLT then
              VME_ADDR_o    <= s_locDataOutSwap(63 downto 33);
              VME_LWORD_n_o <= s_locDataOutSwap(32);
            end if;

            if s_addressingType = CR_CSR then
              VME_DATA_o <= s_locDataOut(31 downto 0);
            else
              VME_DATA_o <= s_locDataOutSwap(31 downto 0);
            end if;

          when DTACK_LOW =>
            s_dtackOE        <= '1';
            s_dataDir        <= VME_WRITE_n_i;
            s_addrDir        <= (s_is_d64) and VME_WRITE_n_i;
            s_dataPhase      <= s_dataPhase;
            s_transferActive <= '1';

            --  Set DTACK (or retry or berr)
            if s_BERRcondition = '0' and s_rty1 = '0' then
              s_mainDTACK     <= '0';
            elsif s_BERRcondition = '0' and s_rty1 = '1' then
              s_retry         <= '1';
            else
              s_BERR_out      <= '1';
            end if;

            if VME_DS_n_i = "11" then
              s_dataDir       <= '0';
              case s_transferType is
                when SINGLE =>
                  --  Cycle should be finished, but allow another access at
                  --  the same address.
                  s_mainFSMstate <= WAIT_FOR_DS;
                when BLT =>
                  s_mainFSMstate <= INCREMENT_ADDR;
                when MBLT =>
                  if s_dataPhase = '1' then
                    s_mainFSMstate <= INCREMENT_ADDR;
                  else
                    -- Address was got, now transfer data
                    s_dataPhase <= '1';
                    s_mainFSMstate <= WAIT_FOR_DS;
                  end if;
                when error =>
                  null;
              end case;
            else
              s_mainFSMstate <= DTACK_LOW;
            end if;

          when INCREMENT_ADDR =>
            s_dtackOE        <= '1';
            s_addrDir        <= (s_is_d64) and VME_WRITE_n_i;
            s_dataPhase      <= s_dataPhase;
            s_transferActive <= '1';

            if s_LWORDlatched_n = '0' then
              if s_transferType = MBLT then
                addr_word_incr := 4;
              else
                addr_word_incr := 2;
              end if;
            else
              if s_DSlatched (0) = '0' then
                -- Next word for D16 or D08(O)
                addr_word_incr := 1;
              else
                addr_word_incr := 0;
              end if;
            end if;
            -- Only increment within the window, don't check the limit.
            -- BLT  --> limit = 256 bytes  (rule 2.12a ANSI/VITA 1-1994)
            -- MBLT --> limit = 2048 bytes (rule 2.78  ANSI/VITA 1-1994)
            s_ADDRlatched (11 downto 1) <= std_logic_vector
              (unsigned(s_ADDRlatched (11 downto 1)) + addr_word_incr);
            s_mainFSMstate   <= WAIT_FOR_DS;

          when WAIT_END =>
            -- Will stay here until AS is released.
            s_mainFSMstate <= WAIT_END;

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
        (s_addressingType = AM_Error) or
        (s_is_d64 = '1' and g_WB_DATA_WIDTH = 32)
      then
        s_BERRcondition <= '1';
      else
        s_BERRcondition <= '0';
      end if;
    end if;
  end process;

  -- wb err handler
  process (clk_i)
  begin
    if rising_edge(clk_i) then
      if VME_AS_n_i = '1' or rst_i = '1' then
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
      if VME_AS_n_i = '1' or rst_i = '1' then
        s_rty1 <= '0';
      elsif s_rty = '1' then
        s_rty1 <= '1';
      end if;
    end if;
  end process;

  ------------------------------------------------------------------------------
  -- Address Handler Process
  ------------------------------------------------------------------------------
  -- This process generates the s_locAddr that is used during the access decode
  -- process.
  -- The s_locAddr is used in the VME_WB_master to address the WB memory
  s_locAddr <= s_ADDRlatched & '0';

  ------------------------------------------------------------------------------
  -- Data Handler Process
  ------------------------------------------------------------------------------

  gen_swapper_ena: if c_SWAPPER_EN generate
    -- Swap the data during read or write operation
    -- sel= 000 --> No swap
    -- sel= 001 --> Swap Byte                      eg: 01234567 become 10325476
    -- sel= 010 --> Swap Word                      eg: 01234567 become 23016745
    -- sel= 011 --> Swap Word+Swap Byte            eg: 01234567 become 32107654
    -- sel= 100 --> Swap DWord+Swap Word+Swap Byte eg: 01234567 become 76543210
    swapper_write : VME_swapper
      port map (
        d_i => s_locDataIn,
        sel => endian_i,
        d_o => s_locDataInSwap
      );

    swapper_read : VME_swapper
      port map (
        d_i => s_locDataOut,
        sel => endian_i,
        d_o => s_locDataOutSwap
      );
  end generate;

  gen_swapper_dis: if not c_SWAPPER_EN generate
    s_locDataInSwap <= s_locDataIn;
    s_locDataOutSwap <= s_locDataOut;
  end generate;

  ------------------------------------------------------------------------------
  -- WB Master
  ------------------------------------------------------------------------------
  -- This component acts as WB master for single read/write PIPELINED mode.
  -- The data and address lines are shifted inside this component.

  s_wbMaster_rst <= rst_i;

  Inst_Wb_master : VME_Wb_master
    generic map (
      g_WB_DATA_WIDTH => g_WB_DATA_WIDTH,
      g_WB_ADDR_WIDTH => g_WB_ADDR_WIDTH
    )
    port map (
      memReq_i        => s_WBReq,
      clk_i           => clk_i,
      reset_i         => s_wbMaster_rst,
      BERRcondition_i => s_BERRcondition,
      sel_i           => s_sel,
      locDataInSwap_i => s_locDataInSwap (31 downto 0),
      locDataOut_o    => s_locDataOutWb (31 downto 0),
      rel_locAddr_i   => s_locAddr,
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

  s_WBReq <= s_card_sel and s_memReq;
  
  ------------------------------------------------------------------------------
  -- Function Decoder
  ------------------------------------------------------------------------------
  addr_decoder_o <= s_ADDRlatched & '0';
  am_o           <= s_AMlatched;

  ------------------------------------------------------------------------------
  -- CR/CSR In/Out
  ------------------------------------------------------------------------------
  cr_csr_data_o <= s_locDataIn(7 downto 0);
  cr_csr_addr_o <= s_ADDRlatched(18 downto 2);

  cr_csr_we_o   <= '1' when s_memReq   = '1' and
                            s_conf_sel = '1' and
                            s_RW       = '0'
                            else '0';
end RTL;
