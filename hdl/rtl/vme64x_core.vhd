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
    g_clock_period    : natural;
    g_decode_am       : boolean;
    g_user_csr_ext    : boolean;
    g_wb_granularity  : t_wishbone_address_granularity;
    g_manufacturer_id : std_logic_vector(23 downto 0);
    g_board_id        : std_logic_vector(31 downto 0);
    g_revision_id     : std_logic_vector(31 downto 0);
    g_program_id      : std_logic_vector(7 downto 0);
    g_ascii_ptr       : std_logic_vector(23 downto 0);
    g_beg_user_cr     : std_logic_vector(23 downto 0);
    g_end_user_cr     : std_logic_vector(23 downto 0);
    g_beg_cram        : std_logic_vector(23 downto 0);
    g_end_cram        : std_logic_vector(23 downto 0);
    g_beg_user_csr    : std_logic_vector(23 downto 0);
    g_end_user_csr    : std_logic_vector(23 downto 0);
    g_beg_sn          : std_logic_vector(23 downto 0);
    g_end_sn          : std_logic_vector(23 downto 0);
    g_decoder_0_adem  : std_logic_vector(31 downto 0);
    g_decoder_0_amcap : std_logic_vector(63 downto 0);
    g_decoder_0_dawpr : std_logic_vector(7 downto 0);
    g_decoder_1_adem  : std_logic_vector(31 downto 0);
    g_decoder_1_amcap : std_logic_vector(63 downto 0);
    g_decoder_1_dawpr : std_logic_vector(7 downto 0);
    g_decoder_2_adem  : std_logic_vector(31 downto 0);
    g_decoder_2_amcap : std_logic_vector(63 downto 0);
    g_decoder_2_dawpr : std_logic_vector(7 downto 0);
    g_decoder_3_adem  : std_logic_vector(31 downto 0);
    g_decoder_3_amcap : std_logic_vector(63 downto 0);
    g_decoder_3_dawpr : std_logic_vector(7 downto 0);
    g_decoder_4_adem  : std_logic_vector(31 downto 0);
    g_decoder_4_amcap : std_logic_vector(63 downto 0);
    g_decoder_4_dawpr : std_logic_vector(7 downto 0);
    g_decoder_5_adem  : std_logic_vector(31 downto 0);
    g_decoder_5_amcap : std_logic_vector(63 downto 0);
    g_decoder_5_dawpr : std_logic_vector(7 downto 0);
    g_decoder_6_adem  : std_logic_vector(31 downto 0);
    g_decoder_6_amcap : std_logic_vector(63 downto 0);
    g_decoder_6_dawpr : std_logic_vector(7 downto 0);
    g_decoder_7_adem  : std_logic_vector(31 downto 0);
    g_decoder_7_amcap : std_logic_vector(63 downto 0);
    g_decoder_7_dawpr : std_logic_vector(7 downto 0));
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
    int_i           : std_logic;
    irq_ack_o       : out std_logic;
    irq_level_i     : std_logic_vector(2 downto 0);
    irq_vector_i    : std_logic_vector(7 downto 0);
    user_csr_addr_o : out std_logic_vector(18 downto 2);
    user_csr_data_i : std_logic_vector(7 downto 0);
    user_csr_data_o : out std_logic_vector(7 downto 0);
    user_csr_we_o   : out std_logic;
    user_cr_addr_o  : out std_logic_vector(18 downto 2);
    user_cr_data_i  : std_logic_vector(7 downto 0));
end vme64x_core;

architecture unwrap of vme64x_core is
begin
  inst : entity work.xvme64x_core
    generic map (
      g_clock_period     => g_clock_period,
      g_decode_am        => g_decode_am,
      g_user_csr_ext     => g_user_csr_ext,
      g_wb_granularity   => g_wb_granularity,
      g_manufacturer_id  => g_manufacturer_id,
      g_board_id         => g_board_id,
      g_revision_id      => g_revision_id,
      g_program_id       => g_program_id,
      g_ascii_ptr        => g_ascii_ptr,
      g_beg_user_cr      => g_beg_user_cr,
      g_end_user_cr      => g_end_user_cr,
      g_beg_cram         => g_beg_cram,
      g_end_cram         => g_end_cram,
      g_beg_user_csr     => g_beg_user_csr,
      g_end_user_csr     => g_end_user_csr,
      g_beg_sn           => g_beg_sn,
      g_end_sn           => g_end_sn,
      g_decoder(0).adem  => g_decoder_0_adem,
      g_decoder(0).amcap => g_decoder_0_amcap,
      g_decoder(0).dawpr => g_decoder_0_dawpr,
      g_decoder(1).adem  => g_decoder_1_adem,
      g_decoder(1).amcap => g_decoder_1_amcap,
      g_decoder(1).dawpr => g_decoder_1_dawpr,
      g_decoder(2).adem  => g_decoder_2_adem,
      g_decoder(2).amcap => g_decoder_2_amcap,
      g_decoder(2).dawpr => g_decoder_2_dawpr,
      g_decoder(3).adem  => g_decoder_3_adem,
      g_decoder(3).amcap => g_decoder_3_amcap,
      g_decoder(3).dawpr => g_decoder_3_dawpr,
      g_decoder(4).adem  => g_decoder_4_adem,
      g_decoder(4).amcap => g_decoder_4_amcap,
      g_decoder(4).dawpr => g_decoder_4_dawpr,
      g_decoder(5).adem  => g_decoder_5_adem,
      g_decoder(5).amcap => g_decoder_5_amcap,
      g_decoder(5).dawpr => g_decoder_5_dawpr,
      g_decoder(6).adem  => g_decoder_6_adem,
      g_decoder(6).amcap => g_decoder_6_amcap,
      g_decoder(6).dawpr => g_decoder_6_dawpr,
      g_decoder(7).adem  => g_decoder_7_adem,
      g_decoder(7).amcap => g_decoder_7_amcap,
      g_decoder(7).dawpr => g_decoder_7_dawpr)
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

