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
    g_CLOCK_PERIOD    : integer                    := c_CLOCK_PERIOD;
    g_WB_DATA_WIDTH   : integer                    := c_wishbone_data_width;
    g_WB_ADDR_WIDTH   : integer                    := c_wishbone_address_width;
    g_DECODE_AM       : boolean                    := true;
    g_USER_CSR_EXT    : boolean                    := false;

    -- CR/CSR
    g_MANUFACTURER_ID : std_logic_vector(23 downto 0)   := c_CERN_ID;
    g_BOARD_ID        : std_logic_vector(31 downto 0)   := c_SVEC_ID;
    g_REVISION_ID     : std_logic_vector(31 downto 0)   := c_REVISION_ID;
    g_PROGRAM_ID      : std_logic_vector(7 downto 0)    := c_PROGRAM_ID;

    g_ASCII_PTR       : std_logic_vector(23 downto 0)   := x"000000";

    g_BEG_USER_CR     : std_logic_vector(23 downto 0)   := x"000000";
    g_END_USER_CR     : std_logic_vector(23 downto 0)   := x"000000";

    g_BEG_CRAM        : std_logic_vector(23 downto 0)   := x"000000";
    g_END_CRAM        : std_logic_vector(23 downto 0)   := x"000000";

    g_BEG_USER_CSR    : std_logic_vector(23 downto 0)   := x"07ff33";
    g_END_USER_CSR    : std_logic_vector(23 downto 0)   := x"07ff5f";

    g_BEG_SN          : std_logic_vector(23 downto 0)   := x"000000";
    g_END_SN          : std_logic_vector(23 downto 0)   := x"000000";

    g_F0_ADEM         : std_logic_vector( 31 downto 0)  := x"ff000000";
    g_F0_AMCAP        : std_logic_vector( 63 downto 0)  := x"00000000_0000ee00";
    g_F0_DAWPR        : std_logic_vector(  7 downto 0)  := x"84";

    g_F1_ADEM         : std_logic_vector( 31 downto 0)  := x"fff80000";
    g_F1_AMCAP        : std_logic_vector( 63 downto 0)  := x"ee000000_00000000";
    g_F1_DAWPR        : std_logic_vector(  7 downto 0)  := x"84";

    g_F2_ADEM         : std_logic_vector( 31 downto 0)  := x"00000000";
    g_F2_AMCAP        : std_logic_vector( 63 downto 0)  := x"00000000_00000000";
    g_F2_DAWPR        : std_logic_vector(  7 downto 0)  := x"84";

    g_F3_ADEM         : std_logic_vector( 31 downto 0)  := x"00000000";
    g_F3_AMCAP        : std_logic_vector( 63 downto 0)  := x"00000000_00000000";
    g_F3_DAWPR        : std_logic_vector(  7 downto 0)  := x"84";

    g_F4_ADEM         : std_logic_vector( 31 downto 0)  := x"00000000";
    g_F4_AMCAP        : std_logic_vector( 63 downto 0)  := x"00000000_00000000";
    g_F4_DAWPR        : std_logic_vector(  7 downto 0)  := x"84";

    g_F5_ADEM         : std_logic_vector( 31 downto 0)  := x"00000000";
    g_F5_AMCAP        : std_logic_vector( 63 downto 0)  := x"00000000_00000000";
    g_F5_DAWPR        : std_logic_vector(  7 downto 0)  := x"84";

    g_F6_ADEM         : std_logic_vector( 31 downto 0)  := x"00000000";
    g_F6_AMCAP        : std_logic_vector( 63 downto 0)  := x"00000000_00000000";
    g_F6_DAWPR        : std_logic_vector(  7 downto 0)  := x"84";

    g_F7_ADEM         : std_logic_vector( 31 downto 0)  := x"00000000";
    g_F7_AMCAP        : std_logic_vector( 63 downto 0)  := x"00000000_00000000";
    g_F7_DAWPR        : std_logic_vector(  7 downto 0)  := x"84"
  );
  port (
    clk_i           : in  std_logic;
    rst_n_i         : in  std_logic;
    rst_n_o         : out std_logic;

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
    VME_IRQ_n_o     : out std_logic_vector( 7 downto 1);
    VME_IACKIN_n_i  : in  std_logic;
    VME_IACK_n_i    : in  std_logic;
    VME_IACKOUT_n_o : out std_logic;
    VME_DTACK_OE_o  : out std_logic;
    VME_DATA_DIR_o  : out std_logic;
    VME_DATA_OE_N_o : out std_logic;
    VME_ADDR_DIR_o  : out std_logic;
    VME_ADDR_OE_N_o : out std_logic;

    master_o        : out t_wishbone_master_out;
    master_i        : in  t_wishbone_master_in;

    irq_i           : in  std_logic;
    irq_ack_o       : out std_logic;

    irq_level_i     : in  std_logic_vector( 7 downto 0) := (others => '0');
    irq_vector_i    : in  std_logic_vector( 7 downto 0) := (others => '0');
    endian_i        : in  std_logic_vector( 2 downto 0) := (others => '0');

    user_csr_addr_o : out std_logic_vector(18 downto 2);
    user_csr_data_i : in  std_logic_vector( 7 downto 0) := (others => '0');
    user_csr_data_o : out std_logic_vector( 7 downto 0);
    user_csr_we_o   : out std_logic;

    user_cr_addr_o  : out std_logic_vector(18 downto 2);
    user_cr_data_i  : in  std_logic_vector( 7 downto 0) := (others => '0')
  );

end xvme64x_core;

architecture wrapper of xvme64x_core is

  signal dat_out,
         dat_in             : std_logic_vector(31 downto 0);
  signal adr_out            : std_logic_vector(31 downto 0);

begin  -- wrapper

  U_Wrapped_VME : VME64xCore_Top
    generic map (
      g_CLOCK_PERIOD    => g_CLOCK_PERIOD,
      g_WB_DATA_WIDTH   => g_WB_DATA_WIDTH,
      g_WB_ADDR_WIDTH   => g_WB_ADDR_WIDTH,
      g_DECODE_AM       => g_DECODE_AM,
      g_USER_CSR_EXT    => g_USER_CSR_EXT,
      g_MANUFACTURER_ID => g_MANUFACTURER_ID,
      g_BOARD_ID        => g_BOARD_ID,
      g_REVISION_ID     => g_REVISION_ID,
      g_PROGRAM_ID      => g_PROGRAM_ID,
      g_ASCII_PTR       => g_ASCII_PTR,
      g_BEG_USER_CR     => g_BEG_USER_CR,
      g_END_USER_CR     => g_END_USER_CR,
      g_BEG_CRAM        => g_BEG_CRAM,
      g_END_CRAM        => g_END_CRAM,
      g_BEG_USER_CSR    => g_BEG_USER_CSR,
      g_END_USER_CSR    => g_END_USER_CSR,
      g_BEG_SN          => g_BEG_SN,
      g_END_SN          => g_END_SN,
      g_F0_ADEM         => g_F0_ADEM,
      g_F0_AMCAP        => g_F0_AMCAP,
      g_F0_DAWPR        => g_F0_DAWPR,
      g_F1_ADEM         => g_F1_ADEM,
      g_F1_AMCAP        => g_F1_AMCAP,
      g_F1_DAWPR        => g_F1_DAWPR,
      g_F2_ADEM         => g_F2_ADEM,
      g_F2_AMCAP        => g_F2_AMCAP,
      g_F2_DAWPR        => g_F2_DAWPR,
      g_F3_ADEM         => g_F3_ADEM,
      g_F3_AMCAP        => g_F3_AMCAP,
      g_F3_DAWPR        => g_F3_DAWPR,
      g_F4_ADEM         => g_F4_ADEM,
      g_F4_AMCAP        => g_F4_AMCAP,
      g_F4_DAWPR        => g_F4_DAWPR,
      g_F5_ADEM         => g_F5_ADEM,
      g_F5_AMCAP        => g_F5_AMCAP,
      g_F5_DAWPR        => g_F5_DAWPR,
      g_F6_ADEM         => g_F6_ADEM,
      g_F6_AMCAP        => g_F6_AMCAP,
      g_F6_DAWPR        => g_F6_DAWPR,
      g_F7_ADEM         => g_F7_ADEM,
      g_F7_AMCAP        => g_F7_AMCAP,
      g_F7_DAWPR        => g_F7_DAWPR
    )
    port map (
      clk_i           => clk_i,
      rst_n_i         => rst_n_i,
      rst_n_o         => rst_n_o,

      VME_AS_n_i      => VME_AS_n_i,
      VME_RST_n_i     => VME_RST_n_i,
      VME_WRITE_n_i   => VME_WRITE_n_i,
      VME_AM_i        => VME_AM_i,
      VME_DS_n_i      => VME_DS_n_i,
      VME_GA_i        => VME_GA_i,
      VME_BERR_o      => VME_BERR_o,
      VME_DTACK_n_o   => VME_DTACK_n_o,
      VME_RETRY_n_o   => VME_RETRY_n_o,
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
      VME_RETRY_OE_o  => VME_RETRY_OE_o,

      DAT_i           => dat_in,
      DAT_o           => dat_out,
      ADR_o           => adr_out,
      CYC_o           => master_o.cyc,
      ERR_i           => master_i.err,
      SEL_o           => open,
      STB_o           => master_o.stb,
      ACK_i           => master_i.ack,
      WE_o            => master_o.we,
      STALL_i         => master_i.stall,

      irq_level_i     => irq_level_i,
      irq_vector_i    => irq_vector_i,
      user_csr_addr_o => user_csr_addr_o,
      user_csr_data_i => user_csr_data_i,
      user_csr_data_o => user_csr_data_o,
      user_csr_we_o   => user_csr_we_o,
      user_cr_addr_o  => user_cr_addr_o,
      user_cr_data_i  => user_cr_data_i,

      irq_ack_o       => irq_ack_o,
      irq_i           => irq_i
    );

  master_o.dat <= dat_out(31 downto 0);
  master_o.sel <= (others => '1');
  master_o.adr <= adr_out(29 downto 0) & "00";
  dat_in       <= master_i.dat;

  assert master_i.rty = '0' report "rty not supported";
  assert endian_i = "000" report "endian_i not supported";
end wrapper;
