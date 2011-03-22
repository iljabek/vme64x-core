-------------------------------------------------------------------------------
--
-- Title       : VME64xCore_Top
-- Design      : VME64xCore
-- Author      : Ziga Kroflic
-- Company     : Cosylab
--
-------------------------------------------------------------------------------
--
-- File        : VME64xCore_Top.vhd
-- Generated   : Tue Mar 30 09:41:05 2010
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
    VME_RETRY_OE_n_o : out   std_logic;
    VME_LWORD_n_b    : inout std_logic;
    VME_ADDR_b       : inout std_logic_vector(31 downto 1);
    VME_DATA_b       : inout std_logic_vector(31 downto 0);
    VME_BBSY_n_i     : in    std_logic;
    VME_IRQ_n_o      : out   std_logic_vector(6 downto 0);
    VME_IACKIN_n_i   : in    std_logic;
    VME_IACKOUT_n_o  : out   std_logic;

    -- VME buffers
    VME_DTACK_OE_o : out std_logic;
    VME_DATA_DIR_o : out std_logic;
    VME_DATA_OE_o  : out std_logic;
    VME_ADDR_DIR_o : out std_logic;
    VME_ADDR_OE_o  : out std_logic;

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
    IRQ_i : in std_logic


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
      TWOeInProgress_o : out std_logic;
      transfer_done_i : in std_logic
      );
  end component;

  component WB_bus is
    port (
      clk_i   : in std_logic;
      reset_i : in std_logic;

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

      memReq_i  : in  std_logic;
      memAck_o  : out std_logic;
      locData_o : out std_logic_vector(63 downto 0);
      locData_i : in  std_logic_vector(63 downto 0);
      locAddr_i : in  std_logic_vector(63 downto 0);
      sel_i     : in  std_logic_vector(7 downto 0);
      RW_i      : in  std_logic;
      lock_i    : in  std_logic;
      err_o     : out std_logic;
      rty_o     : out std_logic;
      cyc_i     : in  std_logic;

      psize_o : in std_logic_vector(7 downto 0);

--      FIFOrden_o       : out std_logic;
--      FIFOwren_o       : out std_logic;
--      FIFOdata_i       : in  std_logic_vector(63 downto 0);
--      FIFOdata_o       : out std_logic_vector(63 downto 0);
--      FIFOreset_o      : out std_logic;
--      writeFIFOempty_i : in  std_logic;
      TWOeInProgress_i : in  std_logic;
      WBbusy_o         : out std_logic

      );    
  end component;

  component IRQ_controller is
    port(
      clk_i            : in  std_logic;
      reset_i          : in  std_logic;
      VME_IRQ_n_o      : out std_logic_vector(6 downto 0);
      VME_IACKIN_n_i   : in  std_logic;
      VME_IACKOUT_n_o  : out std_logic;
      VME_AS_n_i       : in  std_logic;
      VME_DS_n_i       : in  std_logic_vector(1 downto 0);
      irqDTACK_o       : out std_logic;
      IACKinProgress_o : out std_logic;
      IRQ_i            : in  std_logic;
      locAddr_i        : in  std_logic_vector(3 downto 1);
      IDtoData_o       : out std_logic;
      IRQlevelReg_i    : in  std_logic_vector(7 downto 0)
      );
  end component;


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
  signal s_psize      : std_logic_vector(7 downto 0);

  signal s_vme_addr_b_o                                 : std_logic_vector(31 downto 1);
  signal s_VME_LWORD_n_b_o                              : std_logic;
  signal s_VME_ADDR_OE_o, s_VME_DATA_OE, s_VME_DATA_DIR : std_logic;
  signal s_VME_DATA_b_o                                 : std_logic_vector(31 downto 0);
  signal s_transfer_done : std_logic;
  signal sel_we : std_logic;
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
      VME_DTACK_n_o    => VME_DTACK_n_o,
      VME_RETRY_n_o    => VME_RETRY_n_o,
      VME_RETRY_OE_n_o => VME_RETRY_OE_n_o,

      VME_ADDR_b_o    => s_VME_ADDR_b_o,
      VME_ADDR_b_i    => VME_ADDR_b,
      VME_LWORD_n_b_i => VME_LWORD_n_b,
      VME_LWORD_n_b_o => s_VME_LWORD_n_b_o,
      VME_ADDR_DIR_o  => VME_ADDR_DIR_o,
      VME_ADDR_OE_o   => s_VME_ADDR_OE_o,

      VME_DATA_b_o   => s_VME_DATA_b_o,
      VME_DATA_b_i   => VME_DATA_b,
      VME_DATA_DIR_o => s_VME_DATA_DIR,
      VME_DATA_OE_o  => s_VME_DATA_OE,

      VME_BBSY_n_i   => VME_BBSY_n_i,
      VME_IACKIN_n_i => VME_IACKIN_n_i,

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
      stall_i    => s_stall,
      locAddr_o   => s_locAddr,
      wbData_o    => s_WBdataIn,
      wbData_i    => s_WBdataOut,
      wbSel_o     => s_WBsel,
      memAckWB_i  => s_memAckWB,
      memReq_o    => s_memReq,
      err_i       => s_err,
      rty_i       => s_rty,
      psize_o => s_psize,

      irqDTACK_i       => s_irqDTACK,
      IACKinProgress_i => s_IACKinProgress,
      IDtoData_i       => s_IDtoData,
      IRQlevelReg_o    => s_IRQlevelReg,

      --      FIFOwren_o =>         s_FIFOwriteWren,   
      --      FIFOdata_o =>         s_FIFOwriteDin, 
      --      FIFOrden_o =>         s_FIFOreadRden,
      --      FIFOdata_i =>         s_FIFOreadDout, 
      TWOeInProgress_o => s_TWOeInProgress,
      transfer_done_i => s_transfer_done
 --     WBbusy_i         => s_WBbusy
 --     readFIFOempty_i  => s_FIFOreadEmpty

      );

  VME_ADDR_b     <= s_VME_ADDR_b_o    when s_VME_ADDR_OE_o = '1' else (others => 'Z');
  VME_LWORD_n_b  <= s_VME_LWORD_n_b_o when s_VME_ADDR_OE_o = '1' else 'Z';
  VME_DATA_b     <= s_VME_DATA_b_o    when s_VME_DATA_DIR = '1'  else (others => 'Z');
  VME_DATA_OE_o  <= s_VME_DATA_OE;
  VME_ADDR_OE_o  <= s_VME_ADDR_OE_o;
  VME_DATA_DIR_o <= s_VME_DATA_DIR;

  sel_we <= not s_RW; 

  Uwb_dma : wb_dma
    generic map(c_dl     => s_WBdataIn'length,
                c_al     => s_locAddr'length,
                c_sell   => s_WBsel'length,
                c_psizel => s_psize'length)

    port map(
      -- Common signals
      clk_i           => clk_i,
      reset_i         => s_reset,
      transfer_done_o => s_transfer_done,

      -- Slave WB with dma support        
      sl_dat_i   => s_WBdataIn,
      sl_dat_o   => s_WBdataOut,
      sl_adr_i   => s_locAddr,
      sl_cyc_i   => s_cyc,
      sl_err_o   => s_err,
      sl_lock_i => s_lock,
      sl_rty_o   => s_rty,
      sl_sel_i   => s_WBsel,
      sl_stb_i   => s_memReq,
      sl_ack_o   => s_memAckWB,
      sl_we_i    => sel_we,
      sl_stall_o => s_stall,

      sl_psize_i => s_psize,
--    sl_buff_access_i : in std_logic;

      -- Master WB port to fabric
      m_dat_i   => DAT_i,
      m_dat_o   => DAT_o,
      m_adr_o   => ADR_o,
      m_cyc_o   => cyc_o,
      m_err_i   => err_i,
      m_lock_o  => lock_o,
      m_rty_i   => rty_i,
      m_sel_o   => SEL_o,
      m_stb_o   => STB_o,
      m_ack_i   => ACK_i,
      m_we_o    => WE_o,
      m_stall_i => STALL_i
      );    

-------------------------------------------------------------------------------
VME_IACKOUT_n_o <= '1';
VME_IRQ_n_o     <= (others => '0');
-------------------------------------------------------------------------------

--IRQ_controller_1: IRQ_controller
--     port map(
--         clk_i =>            clk_i,
--         reset_i =>          s_reset,
--        VME_IRQ_n_o =>       VME_IRQ_n_o,    
--        VME_IACKIN_n_i =>    VME_IACKIN_n_i,        
--        VME_IACKOUT_n_o =>   VME_IACKOUT_n_o,        
--        VME_AS_n_i =>        VME_AS_n_i,            
--        VME_DS_n_i =>        VME_DS_n_i,    
--        irqDTACK_o =>        s_irqDTACK,
--        IACKinProgress_o =>  s_IACKinProgress,
--        IRQ_i =>             IRQ_i,
--        locAddr_i =>         s_locAddr(3 downto 1),
--        IDtoData_o =>        s_IDtoData,
--        IRQlevelReg_i =>     s_IRQlevelReg
--        );

-------------------------------------------------------------------------------
s_irqDTACK       <= '0';
s_IACKinProgress <= '0';
s_IDtoData       <= '0';
s_IRQlevelReg    <= (others => '0');
-------------------------------------------------------------------------------
process(clk_i)
begin
  if rising_edge(clk_i) then
    s_CRdata <= c_cr_array(to_integer(unsigned(s_CRaddr(11 downto 0))));
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
