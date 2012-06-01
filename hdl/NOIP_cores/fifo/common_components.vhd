library IEEE;
use IEEE.STD_LOGIC_1164.ALL;


----------------------------------------------------------------------------
-- --
-- CERN, BE  --
-- --
-------------------------------------------------------------------------------
--
-- unit name: common_components
--
--! @brief  The common_components package defines some common components. 
--! 
--! @date 24\01\2009
--
--! @version 1
--
--! @details
--! 
--! <b>Dependencies:</b>\n
--! 
--!
--! <b>References:</b>\n
--!  \n
--! <reference two>
--!
--! <b>Modified by:</b>\n
--! Author: <name>
-------------------------------------------------------------------------------
--! \n\n<b>Last changes:</b>\n
--! 24\01\2009 paas header included\n
--! <extended description>
-------------------------------------------------------------------------------
--! @todo Adapt vhdl sintax to ohr standard\n
--! <another thing to do> \n
--
-------------------------------------------------------------------------------

 package common_components is

 component  dpblockram
 generic (dl : integer := 42; 		-- Length of the data word 
 			 al : integer := 10;			-- Size of the addr map (10 = 1024 words)
			 nw : integer := 1024);    -- Number of words
			 									-- 'nw' has to be coherent with 'al'

 port (clk  : in std_logic; 			-- Global Clock
 	we   : in std_logic; 				-- Write Enable
 	aw    : in std_logic_vector(al - 1 downto 0); -- Write Address 
 	ar : in std_logic_vector(al - 1 downto 0); 	 -- Read Address
 	di   : in std_logic_vector(dl - 1 downto 0);  -- Data input
 	dw  : out std_logic_vector(dl - 1 downto 0);  -- Data write, normaly open
 	do  : out std_logic_vector(dl - 1 downto 0)); 	 -- Data output
 end component dpblockram; 

 component TrueDpblockram 
 generic (dl : integer := 42; 		-- Length of the data word 
 			 al : integer := 10);			-- Size of the addr map (10 = 1024 words)

			 									-- 'nw' has to be coherent with 'al'

 port (clk_a_i  : in std_logic; 			-- Global Clock
 	we_a_i   : in std_logic; 				-- Write Enable
 	a_a_i    : in std_logic_vector(al - 1 downto 0); -- Write Address 
 	di_a_i   : in std_logic_vector(dl - 1 downto 0);  -- Data input
 	do_a_o  : out std_logic_vector(dl - 1 downto 0);  -- Data write, normaly open

clk_b_i  : in std_logic; 			-- Global Clock
 	we_b_i   : in std_logic; 				-- Write Enable
 	a_b_i    : in std_logic_vector(al - 1 downto 0); -- Write Address 
 	di_b_i   : in std_logic_vector(dl - 1 downto 0);  -- Data input
 	do_b_o  : out std_logic_vector(dl - 1 downto 0));  -- Data write, normaly open
           

 end component TrueDpblockram; 
 

component QuadPortRam
generic(Al : integer := 8;
nw : integer := 256;
dl : integer := 32
);

port(
Clk : in std_logic;
RstN : in std_logic;

Mux : in std_logic;

DAdd : in std_logic_vector(Al-1 downto 0);
DDataIn : in std_logic_vector(dl -1 downto 0);
DWrEn : in std_logic;
DDataOut : out std_logic_vector(dl -1 downto 0);

AAdd : in std_logic_vector(Al-1 downto 0);
ADataIn : in std_logic_vector(dl -1 downto 0);
AWrEn : in std_logic;

BAdd : in std_logic_vector(Al-1 downto 0);
BDataOut : out std_logic_vector(dl -1 downto 0);

CAdd : in std_logic_vector(Al-1 downto 0);
CDataOut : out std_logic_vector(dl -1 downto 0));
end component QuadPortRam;

component TriPortRamWr_RdRD 
generic(Al : integer := 8;
nw : integer := 256;
dl : integer := 32);

port(
Clk : in std_logic;
RstN : in std_logic;

MuxB_CN : in std_logic; 


AAdd : in std_logic_vector(Al-1 downto 0);
ADataIn : in std_logic_vector(dl - 1  downto 0);
AWrEn : in std_logic;

BAdd : in std_logic_vector(Al-1 downto 0);
BDataOut : out std_logic_vector(dl - 1  downto 0);

CAdd : in std_logic_vector(Al-1 downto 0);
CDataOut : out std_logic_vector(dl - 1 downto 0));
end component TriPortRamWr_RdRD;


component Fifo 
   generic(g_ADDR_LENGTH : integer := 8;
		     g_DATA_LENGTH : integer := 32);
      port(
         Rst : in std_logic; 
         Clk : in std_logic;
			Mux : in std_logic;
					-- NotUsed mux ='1' else FifoWrEn
					--BusRead mux ='1' else FifoRead
         DataRdEn : in std_logic; 
         Addr : in std_logic_vector(g_ADDR_LENGTH - 1 downto 0);
			
--			DataOutRec : out DataOutRecordType;
			data_o : out std_logic_vector(g_DATA_LENGTH - 1 downto 0);
			RdDone : out std_logic;			
			
--        DataRdDone : in std_logic;
-- 		 DataRdEn and DataRdDone should be synch with Mux in a top level entity

         Index : out std_logic_vector(g_DATA_LENGTH - 1 downto 0);
					
--         FifoIn : in FifoInRecordType;
			data_i : in std_logic_vector(g_DATA_LENGTH - 1 downto 0);
			WrEn : in std_logic;

			
         GetNewData : in std_logic; --Resquests new data from the FIFO. It should			                           -- be synch with Mux in a top level entity
--			FifoControl : out FifoOutRecordType
			Empty :  out std_logic;
         NewFifoDataReady : out std_logic;
			FifoDataOut : out std_logic_vector(g_DATA_LENGTH - 1 downto 0);
			FifoOverFlow : out std_logic);
end component Fifo;

component wb_dma
  generic(c_dl     : integer := 64;
          c_al     : integer := 64;
          c_sell   : integer := 8;
          c_psizel : integer := 10);

  port (
    -- Common signals
    clk_i           : in  std_logic;
    reset_i         : in  std_logic;
    transfer_done_o : out std_logic;

    -- Slave WB with dma support        
    sl_dat_i   : in  std_logic_vector(c_dl -1 downto 0);
    sl_dat_o   : out std_logic_vector(c_dl -1 downto 0);
    sl_adr_i   : in  std_logic_vector(c_al -1 downto 0);
    sl_cyc_i   : in  std_logic;
    sl_err_o   : out std_logic;
    sl_lock_i  : in  std_logic;
    sl_rty_o   : out std_logic;
    sl_sel_i   : in  std_logic_vector(c_sell -1 downto 0);
    sl_stb_i   : in  std_logic;
    sl_ack_o   : out std_logic;
    sl_we_i    : in  std_logic;
    sl_stall_o : out std_logic;

    -- This signals are not WB compatible. Should be connected to 0 if not
    -- used. 
    sl_psize_i       : in std_logic_vector(c_psizel -1 downto 0);
--    sl_buff_access_i : in std_logic;

    -- Master WB port to fabric
    m_dat_i   : in  std_logic_vector(c_dl -1 downto 0);
    m_dat_o   : out std_logic_vector(c_dl -1 downto 0);
    m_adr_o   : out std_logic_vector(c_al -1 downto 0);
    m_cyc_o   : out std_logic;
    m_err_i   : in  std_logic;
    m_lock_o  : out std_logic;
    m_rty_i   : in  std_logic;
    m_sel_o   : out std_logic_vector(c_sell -1 downto 0);
    m_stb_o   : out std_logic;
    m_ack_i   : in  std_logic;
    m_we_o    : out std_logic;
    m_stall_i : in  std_logic


    );    
end component wb_dma;


component ddr3_ctrl

  generic(
    --! Core's clock period in ps
    g_MEMCLK_PERIOD      : integer := 3000;
    --! Core's reset polarity (1=active low, 0=active high)
    g_RST_ACT_LOW        : integer := 1;
    --! Core's clock type (SINGLE_ENDED or DIFFERENTIAL)
    g_INPUT_CLK_TYPE     : string  := "SINGLE_ENDED";
    --! Set to TRUE for simulation
    g_SIMULATION         : string  := "FALSE";
    --! If TRUE, uses Xilinx calibration core (Input term, DQS centering)
    g_CALIB_SOFT_IP      : string  := "TRUE";
    --! User ports addresses maping (BANK_ROW_COLUMN or ROW_BANK_COLUMN)
    g_MEM_ADDR_ORDER     : string  := "ROW_BANK_COLUMN";
    --! DDR3 data port width
    g_NUM_DQ_PINS        : integer := 16;
    --! DDR3 address port width
    g_MEM_ADDR_WIDTH     : integer := 14;
    --! DDR3 bank address width
    g_MEM_BANKADDR_WIDTH : integer := 3;
    --! Wishbone port 0 data mask size (8-bit granularity)
    g_P0_MASK_SIZE       : integer := 4;
    --! Wishbone port 0 data width
    g_P0_DATA_PORT_SIZE  : integer := 32;
    --! Wishbone port 1 data mask size (8-bit granularity)
    g_P1_MASK_SIZE       : integer := 4;
    --! Wishbone port 1 data width
    g_P1_DATA_PORT_SIZE  : integer := 32
    );

  port(
    ----------------------------------------------------------------------------
    -- Clocks and reset
    ----------------------------------------------------------------------------
    --! Core's differential clock input (pos)
    --clk_p_i : in std_logic;
    --! Core's differential clock input (neg)
    --clk_n_i : in std_logic;
    --! Core's clock input
    clk_i   : in std_logic;
    --! Core's reset input (active low)
    rst_n_i : in std_logic;

    ----------------------------------------------------------------------------
    -- Status
    ----------------------------------------------------------------------------
    --! Indicates end of calibration sequence at startup
    calib_done : out std_logic;

    ----------------------------------------------------------------------------
    -- DDR3 interface
    ----------------------------------------------------------------------------
    --! DDR3 data bus
    ddr3_dq_b     : inout std_logic_vector(g_NUM_DQ_PINS-1 downto 0);
    --! DDR3 address bus
    ddr3_a_o      : out   std_logic_vector(g_MEM_ADDR_WIDTH-1 downto 0);
    --! DDR3 bank address
    ddr3_ba_o     : out   std_logic_vector(g_MEM_BANKADDR_WIDTH-1 downto 0);
    --! DDR3 row address strobe
    ddr3_ras_n_o  : out   std_logic;
    --! DDR3 column address strobe
    ddr3_cas_n_o  : out   std_logic;
    --! DDR3 write enable
    ddr3_we_n_o   : out   std_logic;
    --! DDR3 on-die termination
    ddr3_odt_o    : out   std_logic;
    --! DDR3 reset
    ddr3_rst_n_o  : out   std_logic;
    --! DDR3 clock enable
    ddr3_cke_o    : out   std_logic;
    --! DDR3 lower byte data mask
    ddr3_dm_o     : out   std_logic;
    --! DDR3 upper byte data mask
    ddr3_udm_o    : out   std_logic;
    --! DDR3 lower byte data strobe (pos)
    ddr3_dqs_p_b  : inout std_logic;
    --! DDR3 lower byte data strobe (neg)
    ddr3_dqs_n_b  : inout std_logic;
    --! DDR3 upper byte data strobe (pos)
    ddr3_udqs_p_b : inout std_logic;
    --! DDR3 upper byte data strobe (pos)
    ddr3_udqs_n_b : inout std_logic;
    --! DDR3 clock (pos)
    ddr3_clk_p_o  : out   std_logic;
    --! DDR3 clock (neg)
    ddr3_clk_n_o  : out   std_logic;
    --! MCB internal termination calibration resistor
    ddr3_rzq_b    : inout std_logic;
    --! MCB internal termination calibration
    ddr3_zio_b    : inout std_logic;

    ----------------------------------------------------------------------------
    -- Wishbone bus - Port 0
    ----------------------------------------------------------------------------
    --! Wishbone bus clock
    wb0_clk_i   : in  std_logic;
    --! Wishbone bus byte select
    wb0_sel_i   : in  std_logic_vector(g_P0_MASK_SIZE - 1 downto 0);
    --! Wishbone bus cycle select
    wb0_cyc_i   : in  std_logic;
    --! Wishbone bus cycle strobe
    wb0_stb_i   : in  std_logic;
    --! Wishbone bus write enable
    wb0_we_i    : in  std_logic;
    --! Wishbone bus address
    wb0_addr_i  : in  std_logic_vector(29 downto 0);
    --! Wishbone bus data input
    wb0_data_i  : in  std_logic_vector(g_P0_DATA_PORT_SIZE - 1 downto 0);
    --! Wishbone bus data output
    wb0_data_o  : out std_logic_vector(g_P0_DATA_PORT_SIZE - 1 downto 0);
    --! Wishbone bus acknowledge
    wb0_ack_o   : out std_logic;
    --! Wishbone bus stall (for pipelined mode)
    wb0_stall_o : out std_logic;

    ----------------------------------------------------------------------------
    -- Wishbone bus - Port 1
    ----------------------------------------------------------------------------
    --! Wishbone bus clock
    wb1_clk_i   : in  std_logic;
    --! Wishbone bus byte select
    wb1_sel_i   : in  std_logic_vector(g_P0_MASK_SIZE - 1 downto 0);
    --! Wishbone bus cycle select
    wb1_cyc_i   : in  std_logic;
    --! Wishbone bus cycle strobe
    wb1_stb_i   : in  std_logic;
    --! Wishbone bus write enable
    wb1_we_i    : in  std_logic;
    --! Wishbone bus address
    wb1_addr_i  : in  std_logic_vector(29 downto 0);
    --! Wishbone bus data input
    wb1_data_i  : in  std_logic_vector(g_P0_DATA_PORT_SIZE - 1 downto 0);
    --! Wishbone bus data output
    wb1_data_o  : out std_logic_vector(g_P0_DATA_PORT_SIZE - 1 downto 0);
    --! Wishbone bus acknowledge
    wb1_ack_o   : out std_logic;
    --! Wishbone bus stall (for pipelined mode)
    wb1_stall_o : out std_logic
    );

end component ddr3_ctrl;
function log2_f(n : in integer) return integer ;

end package common_components;
 
package body common_components is 


   function log2_f(n : in integer) return integer is
      variable i : integer := 0;
   begin
      while (2**i <= n) loop
         i := i + 1;
      end loop;
      return i-1;
   end log2_f;


end package body common_components;