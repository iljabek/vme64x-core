--________________________________________________________________________________________________
--                             VME TO WB INTERFACE
--
--                                CERN,BE/CO-HT 
--________________________________________________________________________________________________
-- File:                       VME64xCore_Top.vhd
--________________________________________________________________________________________________
-- Description:
-- The main blocks:
--       ____________________                 _________________
--      |                    |               |                 |
--      |    VME_bus.vhd     |               |      FIFO       |
--      |                    |               |    wb_dma.vhd   |
--      |         |          |               |                 |
--      |  VME    |    WB    |               |                 |
--      | slave   |  master  |               |                 |
--      |         |          |   ______      |                 |
--      |         |          |  |      |     |                 |
--      |         |          |  |  CR  |     |_________________|
--      |                    |  |______|      _________________                
--      |    _____           |   ______      |                 |
--      |   |     |          |  |      |     |  IRQ_Controller |
--      |   | CSR |          |  | CRAM |     |                 |
--      |   |_____|          |  |      |     |                 |
--      |____________________|  |______|     |_________________|
--                                                      
-- The main component is the VME_bus on the left of the block diagram. Into this components
-- you can find the main finite state machine who coordinates all synchronisms. 
-- The WB protocol is more faster than the VME protocol so for making independent
-- the two protocols a FIFO memory has been introduced. 
-- Is convenient use the FIFO??
-- During the block transfer without FIFO the VME_bus access directly at the Wb bus with
-- Single pipelined read/write access mode. If this is the only Wb master this solution is
-- better than the solution with FIFO.
-- If the FIFO is used the VME master, after one read or write operation, before access again at the 
-- Wb master, have to wait that the FIFO is empty otherwise will receive a retry signal.
-- If the FIFO is used during the read block cycle the VME_bus is keept in the 
-- CHECK_TRANSFER_TYPE until the FIFO is full.
-- For not use the FIFO: Put the signal s_FIFO = '0' into the VME_bus component.  
-- For the single read and write cycles the fifo memory is always bypassed by multiplexers.
-- 
-- 

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.numeric_std.all;

use work.VME_CR_pack.all;
use work.common_components.all;
entity VME64xCore_Top is
  port(
    clk_i : in std_logic;               -- 100 MHz clock input

    -- VME                            
    VME_AS_n_i       : in    std_logic;
    VME_RST_n_i      : in    std_logic;
    VME_WRITE_n_i    : in    std_logic;
    VME_AM_i         : in    std_logic_vector(5 downto 0);
    VME_DS_n_i       : in    std_logic_vector(1 downto 0);
    VME_GA_i         : in    std_logic_vector(5 downto 0);
    VME_BERR_o       : out   std_logic;
    VME_DTACK_n_o    : out   std_logic;
    VME_RETRY_n_o    : out   std_logic;
    VME_RETRY_OE_o : out   std_logic;
    VME_LWORD_n_b    : inout std_logic;
    VME_ADDR_b       : inout std_logic_vector(31 downto 1);
    VME_DATA_b       : inout std_logic_vector(31 downto 0);
    VME_BBSY_n_i     : in    std_logic;
    VME_IRQ_n_o      : out   std_logic_vector(6 downto 0);
    VME_IACKIN_n_i   : in    std_logic;
	 VME_IACK_n_i   : in    std_logic;
    VME_IACKOUT_n_o  : out   std_logic;

    -- VME buffers
    VME_DTACK_OE_o : out std_logic;
    VME_DATA_DIR_o : out std_logic;
    VME_DATA_OE_N_o  : out std_logic;
    VME_ADDR_DIR_o : out std_logic;
    VME_ADDR_OE_N_o  : out std_logic;

    -- WishBone
    RST_i   : in  std_logic;
    DAT_i   : in  std_logic_vector(63 downto 0);
    DAT_o   : out std_logic_vector(63 downto 0);
    ADR_o   : out std_logic_vector(63 downto 0);
    CYC_o   : out std_logic;
    ERR_i   : in  std_logic;
    LOCK_o  : out std_logic;
    RTY_i   : in  std_logic;
    SEL_o   : out std_logic_vector(7 downto 0);
    STB_o   : out std_logic;
    ACK_i   : in  std_logic;
    WE_o    : out std_logic;
    STALL_i : in  std_logic;

    -- IRQ
	 INT_ack : out std_logic;
    IRQ_i : in std_logic;
    -- Add by Davide for debug:
	 leds    : out std_logic_vector(7 downto 0)

-- Uncomment this for use of external CR and CRAM               
--        -- CROM
--        CRaddr_o:             out std_logic_vector(18 downto 0);
--        CRdata_i:             in std_logic_vector(7 downto 0);
--        
--        -- CRAM
--        CRAMaddr_o:           out std_logic_vector(18 downto 0);
--        CRAMdata_o:           out std_logic_vector(7 downto 0);
--        CRAMdata_i:           in std_logic_vector(7 downto 0);
--        CRAMwea_o:            out std_logic
    );
end VME64xCore_Top;


architecture RTL of VME64xCore_Top is

  component VME_bus
    port(
      clk_i   : in  std_logic;
      reset_o : out std_logic;

      -- VME signals
      VME_RST_n_i     : in  std_logic;
      VME_AS_n_i      : in  std_logic;
      VME_LWORD_n_b_o : out std_logic;
      VME_LWORD_n_b_i : in  std_logic;

      VME_RETRY_n_o    : out std_logic;
      VME_RETRY_OE_o : out std_logic;
      VME_WRITE_n_i    : in  std_logic;
      VME_DS_n_i       : in  std_logic_vector(1 downto 0);
      VME_GA_i         : in  std_logic_vector(5 downto 0);  -- Geographical Address and GA parity
      VME_DTACK_n_o    : out std_logic;
      VME_DTACK_OE_o   : out std_logic;

      VME_BERR_o : out std_logic;

      VME_ADDR_b_i   : in  std_logic_vector(31 downto 1);
      VME_ADDR_b_o   : out std_logic_vector(31 downto 1);
      VME_ADDR_DIR_o : out std_logic;
      VME_ADDR_OE_N_o  : out std_logic;

      VME_DATA_b_i   : in  std_logic_vector(31 downto 0);
      VME_DATA_b_o   : out std_logic_vector(31 downto 0);
      VME_DATA_DIR_o : out std_logic;
      VME_DATA_OE_N_o  : out std_logic;

      VME_AM_i       : in std_logic_vector(5 downto 0);
      VME_BBSY_n_i   : in std_logic;
      VME_IACKIN_n_i : in std_logic;
		VME_IACK_n_i : in std_logic;


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
      psize_o : out std_logic_vector(8 downto 0);
      
      -- IRQ controller signals
      --irqDTACK_i       : in  std_logic;
      --IACKinProgress_i : in  std_logic;
      INT_Level : out std_logic_vector(7 downto 0);
		INT_Vector : out std_logic_vector(7 downto 0);
		--Int_CounttoData : out std_logic;
		--IDtoData_i       : in  std_logic;
      --IRQlevelReg_o    : out std_logic_vector(7 downto 0);
      
		-- Add by Davide for debug:
	   leds    : out std_logic_vector(7 downto 0);
		data_non_sampled : in std_logic_vector(63 downto 0);
      -- 2e related signals
      TWOeInProgress_o : out std_logic;
		-- FIFO signal
      transfer_done_i : in std_logic;
		transfer_done_o : out std_logic;
		VMEtoWB : out std_logic;
		FifoMux : out std_logic;
		WBtoVME : out std_logic
      );
  end component;
  
  --component gen_buff is
--	port(
--		input : IN std_logic;
--		en : IN std_logic;          
--		output : OUT std_logic
--		);
--	end component;
	

  COMPONENT IRQ_Controller
	PORT(
		clk_i : IN std_logic;
		reset : IN std_logic;
		VME_IACKIN_n_i : IN std_logic;
		VME_AS_n_i : IN std_logic;
		VME_DS_n_i : IN std_logic_vector(1 downto 0);
		VME_LWORD_n_i : IN std_logic;
		VME_ADDR_123 : IN std_logic_vector(2 downto 0);
		INT_Level : IN std_logic_vector(7 downto 0);
		INT_Vector : IN std_logic_vector(7 downto 0);
		INT_Req : IN std_logic;
		--Read_Int_Source : IN std_logic;          
		VME_IRQ_n_o : OUT std_logic_vector(6 downto 0);
		VME_IACKOUT_n_o : OUT std_logic;
		VME_DTACK_n_o : OUT std_logic;
		VME_DATA_o : OUT std_logic_vector(31 downto 0);
		DataDir : OUT std_logic
		);
	END COMPONENT;


  constant c_zeros : std_logic_vector(31 downto 0) := (others => '0');
  constant c_ones  : std_logic_vector(31 downto 0) := (others => '1');


  signal s_CRAMdataOut : std_logic_vector(7 downto 0);
  signal s_CRAMaddr    : std_logic_vector(18 downto 0);
  signal s_CRAMdataIn  : std_logic_vector(7 downto 0);
  signal s_CRAMwea     : std_logic;
  signal s_CRaddr      : std_logic_vector(18 downto 0);
  signal s_CRdata      : std_logic_vector(7 downto 0);
  signal s_RW          : std_logic;
  signal s_lock        : std_logic;
  signal s_locAddr     : std_logic_vector(63 downto 0);
  signal s_WBdataIn    : std_logic_vector(63 downto 0);
  signal s_WBdataOut   : std_logic_vector(63 downto 0);
  signal s_WBsel       : std_logic_vector(7 downto 0);
  signal s_memAckWB    : std_logic;
  signal s_memReq      : std_logic;
--signal s_IRQ: std_logic;
  signal s_cyc         : std_logic;
  signal s_reset       : std_logic;
  signal s_err         : std_logic;
  signal s_rty         : std_logic;

  signal s_irqDTACK       : std_logic;
  signal s_IACKinProgress : std_logic;
  signal s_IRQlevelReg    : std_logic_vector(7 downto 0);
  signal s_IDtoData       : std_logic;

  signal s_FIFOreset      : std_logic;
  signal s_TWOeInProgress : std_logic;
--  signal s_WBbusy         : std_logic;
  signal s_stall : std_logic;
  signal s_psize      : std_logic_vector(8 downto 0);

  signal s_vme_addr_b_o                                 : std_logic_vector(31 downto 1);
  signal s_VME_LWORD_n_b_o                              : std_logic;
  signal s_VME_ADDR_OE_o, s_VME_DATA_OE, s_VME_DATA_DIR : std_logic;
  signal s_VME_DATA_b_o                                 : std_logic_vector(31 downto 0);
 signal s_VME_DATA_IRQ                                 : std_logic_vector(31 downto 0);
 signal s_VME_DATA_VMEbus                                 : std_logic_vector(31 downto 0);
 
 signal s_VME_DATA_b                                 : std_logic_vector(31 downto 0);
  signal s_transfer_done : std_logic;
  
  signal sel_we : std_logic;
  signal s_VME_ADDR_DIR : std_logic;
  signal s_locAddr64 : std_logic_vector(63 downto 0);
  
  -- Added by Davide :
  signal s_DAT_o : std_logic_vector(63 downto 0);
  signal s_ADR_o : std_logic_vector(63 downto 0);
  signal s_CYC_o : std_logic;
  signal s_LOCK_o : std_logic;
  signal s_STB_o : std_logic;
  signal s_WE_o : std_logic;
  signal s_SEL_o : std_logic_vector(7 downto 0);
  signal s_sl_stb_i : std_logic;
  signal s_sl_cyc_i : std_logic;
  signal s_WbErr_o : std_logic;
  signal s_WbRty_o : std_logic;
  signal s_WbStall_o : std_logic;
  signal s_WbData_o : std_logic_vector(63 downto 0);
  signal s_WbAck_o : std_logic;
  signal s_DATi_sample : std_logic_vector(63 downto 0);
  
  signal s_transfer_done2 : std_logic;
  signal vmetowb : std_logic;
  signal s_vmetowb : std_logic;
  signal wbtovme : std_logic;
  signal s_wbtovme : std_logic;
  signal s_fifo : std_logic;
  signal VME_DTACK_VMEbus : std_logic;
  signal VME_DTACK_IRQ : std_logic;
  signal s_VME_DATA_DIR_VMEbus : std_logic;
  signal s_VME_DATA_DIR_IRQ : std_logic;
  signal s_INT_Level : std_logic_vector(7 downto 0);
  signal s_INT_Vector : std_logic_vector(7 downto 0);
  signal s_VME_IRQ_n_o : std_logic_vector(6 downto 0);
  --signal s_Read_Int_Source : std_logic;
  
  begin

-- Uncomment this section for use of external CR and CRAM

--s_CRAMdataOut <= CRAMdata_i;
--CRAMaddr_o <= s_CRAMaddr;
--CRAMdata_o <= s_CRAMdataIn;
--CRAMwea_o <= s_CRAMwea;
--CRaddr_o <= s_CRaddr;
--s_CRdata <= CRdata_i;  

--s_FIFOreset <= s_wbFIFOreset or s_reset;
  VME_bus_1 : VME_bus
    port map(
      VME_AM_i         => VME_AM_i,
      VME_AS_n_i       => VME_AS_n_i,
      VME_DS_n_i       => VME_DS_n_i,
      VME_GA_i         => VME_GA_i,
      VME_RST_n_i      => VME_RST_n_i,
      VME_WRITE_n_i    => VME_WRITE_n_i,
      VME_BERR_o       => VME_BERR_o,
      VME_DTACK_n_o    => VME_DTACK_VMEbus,
      VME_RETRY_n_o    => VME_RETRY_n_o,
      VME_RETRY_OE_o => VME_RETRY_OE_o,

      VME_ADDR_b_o    => s_VME_ADDR_b_o,
      VME_ADDR_b_i    => VME_ADDR_b,
      VME_LWORD_n_b_i => VME_LWORD_n_b,
      VME_LWORD_n_b_o => s_VME_LWORD_n_b_o,
      VME_ADDR_DIR_o  => s_VME_ADDR_DIR,
      VME_ADDR_OE_N_o   => s_VME_ADDR_OE_o,

      VME_DATA_b_o   => s_VME_DATA_VMEbus,
      VME_DATA_b_i   => VME_DATA_b,
      VME_DATA_DIR_o => s_VME_DATA_DIR_VMEbus,
      VME_DATA_OE_N_o  => s_VME_DATA_OE,

      VME_BBSY_n_i   => VME_BBSY_n_i,
      VME_IACKIN_n_i => VME_IACKIN_n_i,
      VME_IACK_n_i => VME_IACK_n_i,
      VME_DTACK_OE_o => VME_DTACK_OE_o,

      clk_i   => clk_i,
      reset_o => s_reset,

      CRAMdata_i => s_CRAMdataOut,
      CRAMaddr_o => s_CRAMaddr,
      CRAMdata_o => s_CRAMdataIn,
      CRAMwea_o  => s_CRAMwea,
      CRaddr_o   => s_CRaddr,
      CRdata_i   => s_CRdata,
      RW_o       => s_RW,
      lock_o     => s_lock,
      cyc_o      => s_cyc,
      stall_i    => s_WbStall_o,
      locAddr_o   => s_locAddr,
      wbData_o    => s_WBdataIn,
      wbData_i    => s_WbData_o,
      wbSel_o     => s_WBsel,
      memAckWB_i  => s_WbAck_o,
      memReq_o    => s_memReq,
      err_i       => s_WbErr_o,
      rty_i       => s_WbRty_o,
      psize_o => s_psize,

      --irqDTACK_i       => s_irqDTACK,
      --IACKinProgress_i => s_IACKinProgress,
      INT_Level        => s_INT_Level,
		INT_Vector       => s_INT_Vector,
		--Int_CounttoData => s_Read_Int_Source,
		--IDtoData_i       => s_IDtoData,
     -- IRQlevelReg_o    => s_IRQlevelReg,
      -- Add by Davide for debug:
	   leds             => leds,
		data_non_sampled     => DAT_i,  
      --      FIFOwren_o =>         s_FIFOwriteWren,   
      --      FIFOdata_o =>         s_FIFOwriteDin, 
      --      FIFOrden_o =>         s_FIFOreadRden,
      --      FIFOdata_i =>         s_FIFOreadDout, 
      TWOeInProgress_o => s_TWOeInProgress,
      transfer_done_i => s_transfer_done,
		transfer_done_o => s_transfer_done2,
		VMEtoWB         => s_vmetowb,
		WBtoVME         => s_wbtovme,
		FifoMux         => s_fifo
 --     WBbusy_i         => s_WBbusy
 --     readFIFOempty_i  => s_FIFOreadEmpty

      );
  buffData : entity work.gen_buff
  generic map(n  => 32)
  port map(
          input => s_VME_DATA_b_o,
		    en => s_VME_DATA_DIR,
		    output => VME_DATA_b
	       );
  
  
  
  VME_ADDR_b     <= s_VME_ADDR_b_o    when s_VME_ADDR_DIR = '1' else (others => 'Z');
  VME_LWORD_n_b  <= s_VME_LWORD_n_b_o when s_VME_ADDR_DIR = '1' else 'Z';
 -- VME_DATA_b     <= s_VME_DATA_b;  --    when s_VME_DATA_DIR = '1'  else (others => 'Z');
  
  VME_DATA_OE_N_o  <= s_VME_DATA_OE;
  VME_ADDR_OE_N_o  <= s_VME_ADDR_OE_o;
  VME_DATA_DIR_o <= s_VME_DATA_DIR;
 
  VME_ADDR_DIR_o <= s_VME_ADDR_DIR;
  
  sel_we <= not s_RW; 
 -- s_locAddr64 <= "000"&s_locAddr(63 downto 3);
  
  Fifo : entity work.FIFO
    generic map(c_dl     => s_WBdataIn'length,
                c_al     => s_locAddr'length,  --it was: s_locAddr64'length, modified by Davide
                c_sell   => s_WBsel'length,
                c_psizel => s_psize'length - 1) --it was: s_psize'length, modified by Davide

    port map(
      -- Common signals
      clk_i           => clk_i,
      reset_i         => s_reset,
      transfer_done_o => s_transfer_done,
      transfer_done_i => s_transfer_done2,
		VMEtoWB         => vmetowb,
		WBtoVME         => wbtovme,
      -- Slave WB with dma support        
      sl_dat_i   => s_WBdataIn,
      sl_dat_o   => s_WBdataOut,
      sl_adr_i   => s_locAddr,    --it was: s_locAddr64, modified by Davide
      sl_cyc_i   => s_sl_cyc_i,
      sl_err_o   => s_err,
      sl_lock_i => s_lock,
      sl_rty_o   => s_rty,
      sl_sel_i   => s_WBsel,
      sl_stb_i   => s_sl_stb_i,
      sl_ack_o   => s_memAckWB,
      sl_we_i    => sel_we,
      sl_stall_o => s_stall,

      sl_psize_i => s_psize,
--    sl_buff_access_i : in std_logic;

      -- Master WB port to fabric
      m_dat_i   => DAT_i,
      m_dat_o   => s_DAT_o,
      m_adr_o   => s_ADR_o,
      m_cyc_o   => s_CYC_o,
      m_err_i   => err_i,
      m_lock_o  => s_LOCK_o,
      m_rty_i   => rty_i,
      m_sel_o   => s_SEL_o,
      m_stb_o   => s_STB_o,
      m_ack_i   => ACK_i,
      m_we_o    => s_WE_o,
      m_stall_i => STALL_i
      );    

-------------------------------------------------------------------------------
--VME_IACKOUT_n_o <= VME_IACKIN_n_i;  --add by Davide for test
VME_IRQ_n_o     <= not s_VME_IRQ_n_o; --The buffers will invert again the logic level

DAT_o  <= s_WBdataIn when s_fifo = '0' else s_DAT_o;  --to_integer(unsigned(s_psize)) = 1
ADR_o  <= s_locAddr when s_fifo = '0' else s_ADR_o;
CYC_o <= s_cyc when s_fifo = '0' else s_CYC_o;
LOCK_o <= s_lock when s_fifo = '0' else s_LOCK_o;
STB_o <= s_memReq when s_fifo = '0' else s_STB_o;
WE_o <= sel_we when s_fifo = '0' else s_WE_o;
SEL_o <= s_WBsel when s_fifo = '0' else s_SEL_o;
s_sl_stb_i <= '0' when s_fifo = '0' else s_memReq;
s_sl_cyc_i <= '0' when s_fifo = '0' else s_cyc;
s_WbErr_o <= ERR_i when s_fifo = '0' else s_err;
s_WbRty_o <= RTY_i when s_fifo = '0' else s_rty;
s_WbStall_o <= STALL_i when s_fifo = '0' else s_stall;
s_WbData_o <= s_DATi_sample when s_fifo = '0' else s_WBdataOut;
--s_WbData_o <= DAT_i when s_fifo = '0' else s_WBdataOut;
s_WbAck_o <= ACK_i when s_fifo = '0' else s_memAckWB;
vmetowb <= '0' when s_fifo = '0' else s_vmetowb;
wbtovme <= '0' when s_fifo = '0' else s_wbtovme;
s_VME_DATA_b_o <= s_VME_DATA_VMEbus WHEN  VME_IACK_n_i ='1' ELSE 
            s_VME_DATA_IRQ;
VME_DTACK_n_o  <= VME_DTACK_VMEbus WHEN  VME_IACK_n_i ='1' ELSE 
            VME_DTACK_IRQ;				
s_VME_DATA_DIR	<= s_VME_DATA_DIR_VMEbus WHEN  VME_IACK_n_i ='1' ELSE 
            s_VME_DATA_DIR_IRQ;				
				
-------------------------------------------------------------------------------

Inst_IRQ_Controller: IRQ_Controller PORT MAP(
		clk_i => clk_i,
		reset => VME_RST_n_i,
		VME_IACKIN_n_i => VME_IACKIN_n_i,
		VME_AS_n_i => VME_AS_n_i,
		VME_DS_n_i => VME_DS_n_i,
		VME_LWORD_n_i => VME_LWORD_n_b,
		VME_ADDR_123 => VME_ADDR_b(3 downto 1),
		INT_Level => s_INT_Level,
		INT_Vector => s_INT_Vector ,
		INT_Req => IRQ_i,
		--Read_Int_Source => s_Read_Int_Source,
		VME_IRQ_n_o => s_VME_IRQ_n_o,
		VME_IACKOUT_n_o => VME_IACKOUT_n_o,
		VME_DTACK_n_o => VME_DTACK_IRQ,
		VME_DATA_o => s_VME_DATA_IRQ,
		DataDir => s_VME_DATA_DIR_IRQ
	);

-------------------------------------------------------------------------------
s_irqDTACK       <= '0';
s_IACKinProgress <= '0';
s_IDtoData       <= '0';
INT_ack          <= VME_DTACK_IRQ;
--s_IRQlevelReg    <= (others => '0');
-------------------------------------------------------------------------------
process(clk_i)
begin
  if rising_edge(clk_i) then
    s_CRdata <= c_cr_array(to_integer(unsigned(s_CRaddr(11 downto 0))));
  end if;
end process;

process(clk_i)
begin
  if rising_edge(clk_i) then
     if ACK_i = '1' then --and to_integer(unsigned(s_psize)) = 1 then
	      s_DATi_sample <= DAT_i;
	  end if;
  end if;
end process; 


 
-------------------------------------------------------------------------------
CRAM_1 : dpblockram
  generic map(dl => 8,                  -- Length of the data word 
              al => 9,  -- Size of the addr map (10 = 1024 words)
              nw => 2**9)               -- Number of words
                               -- 'nw' has to be coherent with 'al'
  port map(clk => clk_i,                -- Global Clock
           we  => s_CRAMwea,            -- Write Enable
           aw  => s_CRAMaddr(8 downto 0),  -- Write Address 
           ar  => c_zeros(8 downto 0),  -- Read Address
           di  => s_CRAMdataIn,         -- Data input
           dw  => s_CRAMdataOut,        -- Data write, normaly open
           do  => open);                -- Data output
end RTL;
