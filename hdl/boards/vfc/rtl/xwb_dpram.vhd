-------------------------------------------------------------------------------
-- Title      : Dual-port RAM for WR core
-- Project    : WhiteRabbit
-------------------------------------------------------------------------------
-- File       : wrc_dpram.vhd
-- Author     : Grzegorz Daniluk
-- Company    : Elproma
-- Created    : 2011-02-15
-- Last update: 2011-09-26
-- Platform   : FPGA-generics
-- Standard   : VHDL '93
-------------------------------------------------------------------------------
-- Description:
--
-- Dual port RAM with wishbone interface
-------------------------------------------------------------------------------
-- Copyright (c) 2011 Grzegorz Daniluk
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author          Description
-- 2011-02-15  1.0      greg.d          Created
-- 2011-06-09  1.01     twlostow        Removed unnecessary generics
-- 2011-21-09  1.02     twlostow        Struct-ized version
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


library work;
use work.genram_pkg.all;
use work.wishbone_pkg.all;

entity xwb_dpram is
  generic(
    g_size                  : natural := 256;
    g_init_file             : string  := "";
    g_must_have_init_file   : boolean := false;
    g_slave1_interface_mode : t_wishbone_interface_mode;
   -- g_slave2_interface_mode : t_wishbone_interface_mode;
    g_slave1_granularity    : t_wishbone_address_granularity
    --g_slave2_granularity    : t_wishbone_address_granularity
    );
  port(
    clk_sys_i : in std_logic;
    rst_n_i   : in std_logic;
	 INT_ack   : in std_logic;
    slave1_i : in  t_wishbone_slave_in;
    slave1_o : out t_wishbone_slave_out
    --slave2_i : in  t_wishbone_slave_in;
   -- slave2_o : out t_wishbone_slave_out
    );
end xwb_dpram;

architecture struct of xwb_dpram is

  function f_zeros(size : integer)
    return std_logic_vector is
  begin
    return std_logic_vector(to_unsigned(0, size));
  end f_zeros;


  signal s_wea  : std_logic;
  --signal s_web  : std_logic;
  signal s_bwea :  std_logic_vector(c_wishbone_data_width/8-1 downto 0);    -- it was: std_logic_vector(3 downto 0);
 -- signal s_bweb : std_logic_vector(3 downto 0);

  signal slave1_in  : t_wishbone_slave_in;
  signal slave1_out : t_wishbone_slave_out;
  --signal slave2_in  : t_wishbone_slave_in;
  --signal slave2_out : t_wishbone_slave_out;
  signal s_cyc  : std_logic;
  signal s_stb  : std_logic;

COMPONENT IRQ_generator
	PORT(
		clk_i : IN std_logic;
		reset : IN std_logic;
		Freq : IN std_logic_vector(31 downto 0);
		Int_Count_i : IN std_logic_vector(31 downto 0);
		Read_Int_Count : IN std_logic;
		INT_ack : IN std_logic;          
		IRQ_o : OUT std_logic;
		Int_Count_o : OUT std_logic_vector(31 downto 0)
		);
END COMPONENT;

signal s_INT_COUNT : std_logic_vector(31 downto 0);
signal s_FREQ : std_logic_vector(31 downto 0); 
signal s_q_o : std_logic_vector(63 downto 0);
signal s_q_o1 : std_logic_vector(63 downto 0);
signal s_en_Freq : std_logic;
signal s_sel_IntCount : std_logic;
--signal s_IRQ : std_logic;
signal s_Int_Count_o : std_logic_vector(31 downto 0);
signal s_Int_Count_o1 : std_logic_vector(31 downto 0);
signal s_Read_IntCount : std_logic;
signal s_rst : std_logic;
begin
s_rst <= not(rst_n_i);
s_q_o1 <= s_INT_COUNT & s_FREQ;
s_en_Freq <= '1' when (unsigned(slave1_i.adr(f_log2_size(g_size)-1 downto 0)) = 0 and s_bwea = "00001111") else '0';
s_Int_Count_o1 <= slave1_i.dat(63 downto 32) when (s_bwea = "11110000" and (unsigned(slave1_i.adr(f_log2_size(g_size)-1 downto 0))) = 0) else s_Int_Count_o;
s_Read_IntCount <= '1' when (slave1_i.we = '0' and slave1_i.sel = "11110000" and (unsigned(slave1_i.adr(f_log2_size(g_size)-1 downto 0))) = 0 and slave1_out.ack = '1') else '0';


-- Reg INT_COUNT
INT_COUNT : entity work.Reg32bit
    port map(
      reset => s_rst,
		enable => '1',
		di => s_Int_Count_o1,
      do => s_INT_COUNT,
      clk_i => clk_sys_i
      );			
-- Reg FREQ
FREQ : entity work.Reg32bit
    port map(
      reset => s_rst,
		enable => s_en_Freq,
		di => slave1_i.dat(31 downto 0),
      do => s_FREQ,
      clk_i => clk_sys_i
      );	
		
Inst_IRQ_generator: IRQ_generator PORT MAP(
		clk_i => clk_sys_i,
		reset => s_rst,
		Freq => s_FREQ,
		Int_Count_i => s_INT_COUNT,
		Read_Int_Count => s_Read_IntCount,
		INT_ack => INT_ack,
		IRQ_o => slave1_o.int,
		Int_Count_o => s_Int_Count_o
	);



  U_DPRAM : entity work.spram
    generic map(
      -- standard parameters
      g_data_width               => 64,
      g_size                     => 256,
      g_with_byte_enable         =>  true,
      g_init_file                => "",
      g_addr_conflict_resolution => "read_first"
      )
    port map(
	 
  --    rst_n_i => rst_n_i,
      clk_i   => clk_sys_i,
      bwe_i   => s_bwea,
     -- we_i    => s_wea,
      a_i     => slave1_i.adr(f_log2_size(g_size)-1 downto 0),                                 -- it was slave1_in.adr
      d_i     => slave1_i.dat,                                  -- it was slave1_in.adr
      q_o     => s_q_o	                                  -- it was slave1_out.dat
	 );
	 
	 
	 
	 
  --    rst_n_i => rst_n_i,
      -- Port A
  --    clka_i  => clk_sys_i,
  --    bwea_i  => s_bwea,
  --    wea_i   => s_wea,
   --   aa_i    => slave1_in.adr(f_log2_size(g_size)-1 downto 0),
  --    da_i    => slave1_in.dat,
  --    qa_o    => slave1_out.dat,
  --    -- Port B
   --   clkb_i  => clk_sys_i,
   --   bweb_i  => s_bweb,
  --    web_i   => s_web,
  --    ab_i    => slave2_in.adr(f_log2_size(g_size)-1 downto 0),
  --    db_i    => slave2_in.dat,
  --    qb_o    => slave2_out.dat
  --    );

  -- I know this looks weird, but otherwise ISE generates distributed RAM instead of block
  -- RAM
  s_bwea <= slave1_i.sel when s_wea = '1' else f_zeros(c_wishbone_data_width/8);    --it was slave1_in.sel
 -- s_bweb <= slave2_in.sel when s_web = '1' else f_zeros(c_wishbone_data_width/8);

  s_wea <= slave1_i.we and slave1_i.cyc and slave1_i.stb;               -- it was slave1_in.we and slave1_in.stb and slave1_in.cyc;
  --s_web <= slave2_in.we and slave2_in.stb and slave2_in.cyc;

  process(clk_sys_i)
  begin
    if(rising_edge(clk_sys_i)) then
      if(s_rst = '0') then
        slave1_out.ack <= '0';      -- it was slave1_out.ack and slave1_in in all the process
      --  slave2_out.ack <= '0';
      else
        if(slave1_out.ack = '1' and g_slave1_interface_mode = CLASSIC) then
          slave1_out.ack <= '0';
        else
          slave1_out.ack <= slave1_i.cyc and slave1_i.stb;
        end if;

       -- if(slave2_out.ack = '1' and g_slave2_interface_mode = CLASSIC) then
       --   slave2_out.ack <= '0';
       -- else
       --   slave2_out.ack <= slave2_in.cyc and slave2_in.stb;
       -- end if;
      end if;
    end if;
  end process;

  slave1_o.dat <= s_q_o1 when  unsigned(slave1_i.adr(f_log2_size(g_size)-1 downto 0)) = 0 else s_q_o;
  slave1_o.stall <= '0';
 -- slave2_out.stall <= '0';
  slave1_o.err <= '0';
  --slave2_out.err <= '0';
  slave1_o.rty <= '0';
 -- slave2_out.rty <= '0';
  slave1_o.ack <= slave1_out.ack;
end struct;

