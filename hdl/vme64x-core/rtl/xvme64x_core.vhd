--------------------------------------------------------------------------------
-- CERN (BE-CO-HT)
-- VME64x Core
-- http://www.ohwr.org/projects/vme64x-core
--------------------------------------------------------------------------------
--
-- unit name:     xvme64x_core (xvme64x_core.vhd)
--
-- author:        Tomasz Wlostowski <tomasz.wlostowski@cern.ch>
--
-- description:   Wrapped VME64x Core
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
use work.wishbone_pkg.all;
use work.vme64x_pack.all;

entity xvme64x_core is
  generic (
    g_clock_period    : integer                         := c_clk_period;
    g_wb_data_width   : integer                         := c_wishbone_data_width;
    g_wb_addr_width   : integer                         := c_wishbone_address_width;

    -- CR/CSR
    g_manufacturer_id : std_logic_vector(23 downto 0)   := c_cern_id;
    g_board_id        : std_logic_vector(31 downto 0)   := c_svec_id;
    g_revision_id     : std_logic_vector(31 downto 0)   := c_revision_id;
    g_program_id      : std_logic_vector(7 downto 0)    := c_program_id;

    g_ascii_ptr       : std_logic_vector(23 downto 0)   := x"000000";

    g_beg_user_cr     : std_logic_vector(23 downto 0)   := x"000000";
    g_end_user_cr     : std_logic_vector(23 downto 0)   := x"000000";

    g_beg_cram        : std_logic_vector(23 downto 0)   := x"001003";
    g_end_cram        : std_logic_vector(23 downto 0)   := x"0013ff";

    g_beg_user_csr    : std_logic_vector(23 downto 0)   := x"07ff33";
    g_end_user_csr    : std_logic_vector(23 downto 0)   := x"07ff5f";

    g_beg_sn          : std_logic_vector(23 downto 0)   := x"000000";
    g_end_sn          : std_logic_vector(23 downto 0)   := x"000000";

    g_f0_adem         : std_logic_vector( 31 downto 0)  := x"ff000000";
    g_f0_amcap        : std_logic_vector( 63 downto 0)  := x"00000000_0000bb00";
    g_f0_xamcap       : std_logic_vector(255 downto 0)  := x"00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000";
    g_f0_dawpr        : std_logic_vector(  7 downto 0)  := x"84";

    g_f1_adem         : std_logic_vector( 31 downto 0)  := x"fff80000";
    g_f1_amcap        : std_logic_vector( 63 downto 0)  := x"bb000000_00000000";
    g_f1_xamcap       : std_logic_vector(255 downto 0)  := x"00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000";
    g_f1_dawpr        : std_logic_vector(  7 downto 0)  := x"84";

    g_f2_adem         : std_logic_vector( 31 downto 0)  := x"00000000";
    g_f2_amcap        : std_logic_vector( 63 downto 0)  := x"00000000_00000000";
    g_f2_xamcap       : std_logic_vector(255 downto 0)  := x"00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000";
    g_f2_dawpr        : std_logic_vector(  7 downto 0)  := x"84";

    g_f3_adem         : std_logic_vector( 31 downto 0)  := x"00000000";
    g_f3_amcap        : std_logic_vector( 63 downto 0)  := x"00000000_00000000";
    g_f3_xamcap       : std_logic_vector(255 downto 0)  := x"00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000";
    g_f3_dawpr        : std_logic_vector(  7 downto 0)  := x"84";

    g_f4_adem         : std_logic_vector( 31 downto 0)  := x"00000000";
    g_f4_amcap        : std_logic_vector( 63 downto 0)  := x"00000000_00000000";
    g_f4_xamcap       : std_logic_vector(255 downto 0)  := x"00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000";
    g_f4_dawpr        : std_logic_vector(  7 downto 0)  := x"84";

    g_f5_adem         : std_logic_vector( 31 downto 0)  := x"00000000";
    g_f5_amcap        : std_logic_vector( 63 downto 0)  := x"00000000_00000000";
    g_f5_xamcap       : std_logic_vector(255 downto 0)  := x"00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000";
    g_f5_dawpr        : std_logic_vector(  7 downto 0)  := x"84";

    g_f6_adem         : std_logic_vector( 31 downto 0)  := x"00000000";
    g_f6_amcap        : std_logic_vector( 63 downto 0)  := x"00000000_00000000";
    g_f6_xamcap       : std_logic_vector(255 downto 0)  := x"00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000";
    g_f6_dawpr        : std_logic_vector(  7 downto 0)  := x"84";

    g_f7_adem         : std_logic_vector( 31 downto 0)  := x"00000000";
    g_f7_amcap        : std_logic_vector( 63 downto 0)  := x"00000000_00000000";
    g_f7_xamcap       : std_logic_vector(255 downto 0)  := x"00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000";
    g_f7_dawpr        : std_logic_vector(  7 downto 0)  := x"84"
  );
  port (
    clk_i   : in std_logic;
    rst_n_i : in std_logic;

    VME_AS_n_i      : in  std_logic;
    VME_RST_n_i     : in  std_logic;
    VME_WRITE_n_i   : in  std_logic;
    VME_AM_i        : in  std_logic_vector(5 downto 0);
    VME_DS_n_i      : in  std_logic_vector(1 downto 0);
    VME_GA_i        : in  std_logic_vector(5 downto 0);
    VME_BERR_o      : out std_logic;
    VME_DTACK_n_o   : out std_logic;
    VME_RETRY_n_o   : out std_logic;
    VME_RETRY_OE_o  : out std_logic;
    VME_LWORD_n_b_i : in  std_logic;
    VME_LWORD_n_b_o : out std_logic;
    VME_ADDR_b_i    : in  std_logic_vector(31 downto 1);
    VME_ADDR_b_o    : out std_logic_vector(31 downto 1);
    VME_DATA_b_i    : in  std_logic_vector(31 downto 0);
    VME_DATA_b_o    : out std_logic_vector(31 downto 0);
    VME_IRQ_n_o     : out std_logic_vector(6 downto 0);
    VME_IACKIN_n_i  : in  std_logic;
    VME_IACK_n_i    : in  std_logic;
    VME_IACKOUT_n_o : out std_logic;
    VME_DTACK_OE_o  : out std_logic;
    VME_DATA_DIR_o  : out std_logic;
    VME_DATA_OE_N_o : out std_logic;
    VME_ADDR_DIR_o  : out std_logic;
    VME_ADDR_OE_N_o : out std_logic;

    master_o : out t_wishbone_master_out;
    master_i : in  t_wishbone_master_in;

    irq_i     : in  std_logic;
    irq_ack_o : out std_logic
  );

end xvme64x_core;

architecture wrapper of xvme64x_core is

  signal dat_out,
         dat_in           : std_logic_vector(31 downto 0);
  signal adr_out          : std_logic_vector(31 downto 0);
  signal irq_vector,
         irq_level        : std_logic_vector( 7 downto 0);
  signal endian           : std_logic_vector( 2 downto 0);
  signal user_csr_addr    : std_logic_vector(18 downto 2);
  signal user_csr_data_i,
         user_csr_data_o  : std_logic_vector( 7 downto 0);
  signal user_csr_we      : std_logic;
  signal rst              : std_logic;

begin  -- wrapper

  rst <= not rst_n_i;

  U_Wrapped_VME : VME64xCore_Top
    generic map (
      g_clock           => g_clock_period,
      g_wb_data_width   => g_wb_data_width,
      g_wb_addr_width   => g_wb_addr_width,
      g_manufacturer_id => g_manufacturer_id,
      g_board_id        => g_board_id,
      g_revision_id     => g_revision_id,
      g_program_id      => g_program_id,
      g_ascii_ptr       => g_ascii_ptr,
      g_beg_user_cr     => g_beg_user_cr,
      g_end_user_cr     => g_end_user_cr,
      g_beg_cram        => g_beg_cram,
      g_end_cram        => g_end_cram,
      g_beg_user_csr    => g_beg_user_csr,
      g_end_user_csr    => g_end_user_csr,
      g_beg_sn          => g_beg_sn,
      g_end_sn          => g_end_sn,
      g_f0_adem         => g_f0_adem,
      g_f0_amcap        => g_f0_amcap,
      g_f0_xamcap       => g_f0_xamcap,
      g_f0_dawpr        => g_f0_dawpr,
      g_f1_adem         => g_f1_adem,
      g_f1_amcap        => g_f1_amcap,
      g_f1_xamcap       => g_f1_xamcap,
      g_f1_dawpr        => g_f1_dawpr,
      g_f2_adem         => g_f2_adem,
      g_f2_amcap        => g_f2_amcap,
      g_f2_xamcap       => g_f2_xamcap,
      g_f2_dawpr        => g_f2_dawpr,
      g_f3_adem         => g_f3_adem,
      g_f3_amcap        => g_f3_amcap,
      g_f3_xamcap       => g_f3_xamcap,
      g_f3_dawpr        => g_f3_dawpr,
      g_f4_adem         => g_f4_adem,
      g_f4_amcap        => g_f4_amcap,
      g_f4_xamcap       => g_f4_xamcap,
      g_f4_dawpr        => g_f4_dawpr,
      g_f5_adem         => g_f5_adem,
      g_f5_amcap        => g_f5_amcap,
      g_f5_xamcap       => g_f5_xamcap,
      g_f5_dawpr        => g_f5_dawpr,
      g_f6_adem         => g_f6_adem,
      g_f6_amcap        => g_f6_amcap,
      g_f6_xamcap       => g_f6_xamcap,
      g_f6_dawpr        => g_f6_dawpr,
      g_f7_adem         => g_f7_adem,
      g_f7_amcap        => g_f7_amcap,
      g_f7_xamcap       => g_f7_xamcap,
      g_f7_dawpr        => g_f7_dawpr
    )
    port map (
      clk_i           => clk_i,
      rst_n_i         => rst_n_i,
      VME_AS_n_i      => VME_AS_n_i,
      VME_RST_n_i     => VME_RST_n_i,
      VME_WRITE_n_i   => VME_WRITE_n_i,
      VME_AM_i        => VME_AM_i,
      VME_DS_n_i      => VME_DS_n_i,
      VME_GA_i        => VME_GA_i,
      VME_BERR_o      => VME_BERR_o,
      VME_DTACK_n_o   => VME_DTACK_n_o,
      VME_RETRY_n_o   => VME_RETRY_n_o,
      VME_RETRY_OE_o  => VME_RETRY_OE_o,
      VME_LWORD_n_i   => VME_LWORD_n_b_i,
      VME_LWORD_n_o   => VME_LWORD_n_b_o,
      VME_ADDR_i      => VME_ADDR_b_i,
      VME_ADDR_o      => VME_ADDR_b_o,
      VME_DATA_i      => VME_DATA_b_i,
      VME_DATA_o      => VME_DATA_b_o,
      VME_IRQ_o       => VME_IRQ_n_o,
      VME_IACKIN_n_i  => VME_IACKIN_n_i,
      VME_IACK_n_i    => VME_IACK_n_i,
      VME_IACKOUT_n_o => VME_IACKOUT_n_o,
      VME_DTACK_OE_o  => VME_DTACK_OE_o,
      VME_DATA_DIR_o  => VME_DATA_DIR_o,
      VME_DATA_OE_N_o => VME_DATA_OE_N_o,
      VME_ADDR_DIR_o  => VME_ADDR_DIR_o,
      VME_ADDR_OE_N_o => VME_ADDR_OE_N_o,

      DAT_i           => dat_in,
      DAT_o           => dat_out,
      ADR_o           => adr_out,
      CYC_o           => master_o.cyc,
      ERR_i           => master_i.err,
      RTY_i           => master_i.rty,
      SEL_o           => open,
      STB_o           => master_o.stb,
      ACK_i           => master_i.ack,
      WE_o            => master_o.we,
      STALL_i         => master_i.stall,

      endian_i        => endian,

      user_csr_addr_o => user_csr_addr,
      user_csr_data_i => user_csr_data_o,
      user_csr_data_o => user_csr_data_i,
      user_csr_we_o   => user_csr_we,
      user_cr_addr_o  => open,
      user_cr_data_i  => x"00",

      irq_i           => irq_i,
      irq_ack_o       => irq_ack_o,
      irq_vector_i    => irq_vector,
      irq_level_i     => irq_level
    );

  master_o.dat <= dat_out(31 downto 0);
  master_o.sel <= (others => '1');
  master_o.adr <= adr_out(29 downto 0) & "00";
  dat_in       <= master_i.dat;

  U_User_CSR : VME_User_CSR
    generic map (
      g_wb_data_width => g_wb_data_width
    )
    port map (
      clk_i           => clk_i,
      reset_i         => rst,
      addr_i          => user_csr_addr,
      data_i          => user_csr_data_i,
      data_o          => user_csr_data_o,
      we_i            => user_csr_we,
      irq_vector_o    => irq_vector,
      irq_level_o     => irq_level,
      endian_o        => endian,
      time_i          => x"0000000000",
      bytes_i         => x"0000"
    );

end wrapper;
