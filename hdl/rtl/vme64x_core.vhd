----------------------------------------------------------------
-- This file was automatically generated by vhdl-unwrap for 
-- entity xvme64x_core.
-- DO NOT EDIT.
----------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use work.wishbone_pkg.all;
use work.vme64x_pkg.all;

entity vme64x_core is
  generic (
    g_CLOCK_PERIOD    : natural;
    g_DECODE_AM       : boolean := true;
    g_ENABLE_CR_CSR   : boolean := true;
    g_USER_CSR_EXT    : boolean := false;
    g_WB_GRANULARITY  : t_wishbone_address_granularity;
    g_MANUFACTURER_ID : std_logic_vector(23 downto 0);
    g_BOARD_ID        : std_logic_vector(31 downto 0);
    g_REVISION_ID     : std_logic_vector(31 downto 0);
    g_PROGRAM_ID      : std_logic_vector(7 downto 0);
    g_ASCII_PTR       : std_logic_vector(23 downto 0) := x"000000";
    g_BEG_USER_CR     : std_logic_vector(23 downto 0) := x"000000";
    g_END_USER_CR     : std_logic_vector(23 downto 0) := x"000000";
    g_BEG_CRAM        : std_logic_vector(23 downto 0) := x"000000";
    g_END_CRAM        : std_logic_vector(23 downto 0) := x"000000";
    g_BEG_USER_CSR    : std_logic_vector(23 downto 0) := x"07ff33";
    g_END_USER_CSR    : std_logic_vector(23 downto 0) := x"07ff5f";
    g_BEG_SN          : std_logic_vector(23 downto 0) := x"000000";
    g_END_SN          : std_logic_vector(23 downto 0) := x"000000";
    g_DECODER_0_ADEM  : std_logic_vector(31 downto 0) := x"ff000000";
    g_DECODER_0_AMCAP : std_logic_vector(63 downto 0) := x"00000000_0000ff00";
    g_DECODER_0_DAWPR : std_logic_vector(7 downto 0)  := x"84";
    g_DECODER_1_ADEM  : std_logic_vector(31 downto 0) := x"fff80000";
    g_DECODER_1_AMCAP : std_logic_vector(63 downto 0) := x"ff000000_00000000";
    g_DECODER_1_DAWPR : std_logic_vector(7 downto 0)  := x"84";
    g_DECODER_2_ADEM  : std_logic_vector(31 downto 0) := x"00000000";
    g_DECODER_2_AMCAP : std_logic_vector(63 downto 0) := x"00000000_00000000";
    g_DECODER_2_DAWPR : std_logic_vector(7 downto 0)  := x"84";
    g_DECODER_3_ADEM  : std_logic_vector(31 downto 0) := x"00000000";
    g_DECODER_3_AMCAP : std_logic_vector(63 downto 0) := x"00000000_00000000";
    g_DECODER_3_DAWPR : std_logic_vector(7 downto 0)  := x"84";
    g_DECODER_4_ADEM  : std_logic_vector(31 downto 0) := x"00000000";
    g_DECODER_4_AMCAP : std_logic_vector(63 downto 0) := x"00000000_00000000";
    g_DECODER_4_DAWPR : std_logic_vector(7 downto 0)  := x"84";
    g_DECODER_5_ADEM  : std_logic_vector(31 downto 0) := x"00000000";
    g_DECODER_5_AMCAP : std_logic_vector(63 downto 0) := x"00000000_00000000";
    g_DECODER_5_DAWPR : std_logic_vector(7 downto 0)  := x"84";
    g_DECODER_6_ADEM  : std_logic_vector(31 downto 0) := x"00000000";
    g_DECODER_6_AMCAP : std_logic_vector(63 downto 0) := x"00000000_00000000";
    g_DECODER_6_DAWPR : std_logic_vector(7 downto 0)  := x"84";
    g_DECODER_7_ADEM  : std_logic_vector(31 downto 0) := x"00000000";
    g_DECODER_7_AMCAP : std_logic_vector(63 downto 0) := x"00000000_00000000";
    g_DECODER_7_DAWPR : std_logic_vector(7 downto 0)  := x"84");
  port (
    clk_i           : std_logic;
    rst_n_i         : std_logic;
    rst_n_o         : out std_logic;
    vme_as_n_i      : std_logic;
    vme_rst_n_i     : std_logic;
    vme_write_n_i   : std_logic;
    vme_am_i        : std_logic_vector(5 downto 0);
    vme_ds_n_i      : std_logic_vector(1 downto 0);
    vme_ga_i        : std_logic_vector(5 downto 0);
    vme_lword_n_i   : std_logic;
    vme_data_i      : std_logic_vector(31 downto 0);
    vme_addr_i      : std_logic_vector(31 downto 1);
    vme_iack_n_i    : std_logic;
    vme_iackin_n_i  : std_logic;
    vme_iackout_n_o : out std_logic;
    vme_dtack_n_o   : out std_logic;
    vme_dtack_oe_o  : out std_logic;
    vme_lword_n_o   : out std_logic;
    vme_data_o      : out std_logic_vector(31 downto 0);
    vme_data_dir_o  : out std_logic;
    vme_data_oe_n_o : out std_logic;
    vme_addr_o      : out std_logic_vector(31 downto 1);
    vme_addr_dir_o  : out std_logic;
    vme_addr_oe_n_o : out std_logic;
    vme_retry_n_o   : out std_logic;
    vme_retry_oe_o  : out std_logic;
    vme_berr_n_o    : out std_logic;
    vme_irq_n_o     : out std_logic_vector(6 downto 0);
    wb_ack_i        : std_logic;
    wb_err_i        : std_logic;
    wb_rty_i        : std_logic;
    wb_stall_i      : std_logic;
    wb_dat_i        : t_wishbone_data;
    wb_cyc_o        : out std_logic;
    wb_stb_o        : out std_logic;
    wb_adr_o        : out t_wishbone_address;
    wb_sel_o        : out t_wishbone_byte_select;
    wb_we_o         : out std_logic;
    wb_dat_o        : out t_wishbone_data;
    int_i           : std_logic := '0';
    irq_ack_o       : out std_logic;
    irq_level_i     : std_logic_vector(2 downto 0) := (others => '0');
    irq_vector_i    : std_logic_vector(7 downto 0) := (others => '0');
    user_csr_addr_o : out std_logic_vector(18 downto 2);
    user_csr_data_i : std_logic_vector(7 downto 0) := (others => '0');
    user_csr_data_o : out std_logic_vector(7 downto 0);
    user_csr_we_o   : out std_logic;
    user_cr_addr_o  : out std_logic_vector(18 downto 2);
    user_cr_data_i  : std_logic_vector(7 downto 0) := (others => '0'));
end vme64x_core;

architecture unwrap of vme64x_core is
begin
  inst : entity work.xvme64x_core
    generic map (
      g_CLOCK_PERIOD     => g_CLOCK_PERIOD,
      g_DECODE_AM        => g_DECODE_AM,
      g_ENABLE_CR_CSR    => g_ENABLE_CR_CSR,
      g_USER_CSR_EXT     => g_USER_CSR_EXT,
      g_WB_GRANULARITY   => g_WB_GRANULARITY,
      g_MANUFACTURER_ID  => g_MANUFACTURER_ID,
      g_BOARD_ID         => g_BOARD_ID,
      g_REVISION_ID      => g_REVISION_ID,
      g_PROGRAM_ID       => g_PROGRAM_ID,
      g_ASCII_PTR        => g_ASCII_PTR,
      g_BEG_USER_CR      => g_BEG_USER_CR,
      g_END_USER_CR      => g_END_USER_CR,
      g_BEG_CRAM         => g_BEG_CRAM,
      g_END_CRAM         => g_END_CRAM,
      g_BEG_USER_CSR     => g_BEG_USER_CSR,
      g_END_USER_CSR     => g_END_USER_CSR,
      g_BEG_SN           => g_BEG_SN,
      g_END_SN           => g_END_SN,
      g_DECODER(0).adem  => g_DECODER_0_ADEM,
      g_DECODER(0).amcap => g_DECODER_0_AMCAP,
      g_DECODER(0).dawpr => g_DECODER_0_DAWPR,
      g_DECODER(1).adem  => g_DECODER_1_ADEM,
      g_DECODER(1).amcap => g_DECODER_1_AMCAP,
      g_DECODER(1).dawpr => g_DECODER_1_DAWPR,
      g_DECODER(2).adem  => g_DECODER_2_ADEM,
      g_DECODER(2).amcap => g_DECODER_2_AMCAP,
      g_DECODER(2).dawpr => g_DECODER_2_DAWPR,
      g_DECODER(3).adem  => g_DECODER_3_ADEM,
      g_DECODER(3).amcap => g_DECODER_3_AMCAP,
      g_DECODER(3).dawpr => g_DECODER_3_DAWPR,
      g_DECODER(4).adem  => g_DECODER_4_ADEM,
      g_DECODER(4).amcap => g_DECODER_4_AMCAP,
      g_DECODER(4).dawpr => g_DECODER_4_DAWPR,
      g_DECODER(5).adem  => g_DECODER_5_ADEM,
      g_DECODER(5).amcap => g_DECODER_5_AMCAP,
      g_DECODER(5).dawpr => g_DECODER_5_DAWPR,
      g_DECODER(6).adem  => g_DECODER_6_ADEM,
      g_DECODER(6).amcap => g_DECODER_6_AMCAP,
      g_DECODER(6).dawpr => g_DECODER_6_DAWPR,
      g_DECODER(7).adem  => g_DECODER_7_ADEM,
      g_DECODER(7).amcap => g_DECODER_7_AMCAP,
      g_DECODER(7).dawpr => g_DECODER_7_DAWPR)
    port map (
      clk_i           => clk_i,
      rst_n_i         => rst_n_i,
      rst_n_o         => rst_n_o,
      vme_i.as_n      => vme_as_n_i,
      vme_i.rst_n     => vme_rst_n_i,
      vme_i.write_n   => vme_write_n_i,
      vme_i.am        => vme_am_i,
      vme_i.ds_n      => vme_ds_n_i,
      vme_i.ga        => vme_ga_i,
      vme_i.lword_n   => vme_lword_n_i,
      vme_i.data      => vme_data_i,
      vme_i.addr      => vme_addr_i,
      vme_i.iack_n    => vme_iack_n_i,
      vme_i.iackin_n  => vme_iackin_n_i,
      vme_o.iackout_n => vme_iackout_n_o,
      vme_o.dtack_n   => vme_dtack_n_o,
      vme_o.dtack_oe  => vme_dtack_oe_o,
      vme_o.lword_n   => vme_lword_n_o,
      vme_o.data      => vme_data_o,
      vme_o.data_dir  => vme_data_dir_o,
      vme_o.data_oe_n => vme_data_oe_n_o,
      vme_o.addr      => vme_addr_o,
      vme_o.addr_dir  => vme_addr_dir_o,
      vme_o.addr_oe_n => vme_addr_oe_n_o,
      vme_o.retry_n   => vme_retry_n_o,
      vme_o.retry_oe  => vme_retry_oe_o,
      vme_o.berr_n    => vme_berr_n_o,
      vme_o.irq_n     => vme_irq_n_o,
      wb_i.ack        => wb_ack_i,
      wb_i.err        => wb_err_i,
      wb_i.rty        => wb_rty_i,
      wb_i.stall      => wb_stall_i,
      wb_i.dat        => wb_dat_i,
      wb_o.cyc        => wb_cyc_o,
      wb_o.stb        => wb_stb_o,
      wb_o.adr        => wb_adr_o,
      wb_o.sel        => wb_sel_o,
      wb_o.we         => wb_we_o,
      wb_o.dat        => wb_dat_o,
      int_i           => int_i,
      irq_ack_o       => irq_ack_o,
      irq_level_i     => irq_level_i,
      irq_vector_i    => irq_vector_i,
      user_csr_addr_o => user_csr_addr_o,
      user_csr_data_i => user_csr_data_i,
      user_csr_data_o => user_csr_data_o,
      user_csr_we_o   => user_csr_we_o,
      user_cr_addr_o  => user_cr_addr_o,
      user_cr_data_i  => user_cr_data_i);
end unwrap;

