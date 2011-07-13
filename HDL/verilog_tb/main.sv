

`timescale 1ns/1ns

`include "vme_master.sv"

module main();

supply0 Gnd;
supply1 Vcc;

/* -----\/----- EXCLUDED -----\/-----
wire    [6:0] VmeIrq_b7;
wire    [31:0] VmeD_b32;          
wire    VmeSysClk_k;
wire    VmeAs_n;
wire    [31:1] VmeA_b31;
wire    VmeDtAck_n;
wire    VmeWrite_n;
wire    VmeIack_n;
wire    [2:1] VmeDs_nb2;
wire    VmeLword_n;
wire    VmeRetry_n;
wire    VmeBerr_n;
wire    [5:0] VmeAM_b6; 
wire    VmeSysReset_n; 
wire    VmeTck;
wire    VmeTrst;
wire    VmeTdo;
wire    VmeTdi; 
wire    VmeTms;
 -----/\----- EXCLUDED -----/\----- */

   pullup i_DtAckPU(vm.master.DtAck_i);
   pullup i_RetryPU(vm.master.Retry_i);
   pullup i_BerrPU(vm.master.Berr_i);
//   pullup i_IackPU(vm.master.IAck_o);
   // Clock generator
   reg Clock;
   
  always
  begin
    #12 Clock = 1;
    #12 Clock = 0;
  end 

   //####################################
   // VME master
   //####################################

   IVMEMaster vm ();

   //####################################
   // VFC board in SLOT 4
   //####################################

   wire GapSlot4_n    = ^(5'd4);    
   wire [4:0] GaSlot4_nb5 = ~(5'd4);
   wire [5:0] Ga = {GaSlot4_nb5,GapSlot4_n};
    //  wire VmeIackInSlot4_n = VmeIack_n;
   wire VmeIackOutSlot4_n;
   wire [4:1] FpGpIo_b4 = 'hz;
   assign VmeIack_n  = vm.master.IAck_o;
  wire VmeIackInSlot4_n = VmeIack_n;
   wire Berr;
   wire Retry;
   wire DTACK_n_o;
   
       assign vm.master.Berr_i = ~Berr;
   assign vm.master.Retry_i = ~Retry;
   
   
   
	// Instantiate the Unit Under Test (UUT)
	vme64xcore_top_reg uut (
		.clk_i(Clock), 
		.VME_AS_n_i(vm.master.As_o), 
		.VME_RST_n_i(vm.master.SysReset_o), 
		.VME_WRITE_n_i(vm.master.Write_o), 
		.VME_AM_i(vm.master.Am_o), 
		.VME_DS_n_i({vm.master.Ds2_o, vm.master.Ds1_o}), 
		.VME_GA_i(Ga), 
		.VME_BERR_o(Berr), 
		.VME_DTACK_n_o(vm.master.DtAck_i), 
		.VME_RETRY_n_o(Retry), 
		.VME_LWORD_n_b(vm.master.LWord_b), 
		.VME_ADDR_b(vm.master.A_b), 
		.VME_DATA_b(vm.master.D_b), 
		.VME_IRQ_n_o(vm.master.Irq_i), 
		.FpLed_onb8_5(FpLed_onb8_5), 
		.FpLed_onb8_6(FpLed_onb8_6), 
		.VME_DTACK_OE_o(VME_DTACK_OE_o), 
		.VME_DATA_DIR_o(VME_DATA_DIR_o), 
		.VME_DATA_OE_o(VME_DATA_OE_o), 
		.VME_ADDR_DIR_o(VME_ADDR_DIR_o), 
		.VME_ADDR_OE_o(VME_ADDR_OE_o)
	);

     
      
initial begin
   int i;
   
   reg[7:0] q;
   reg[31:0] r;
   reg [63:0] d [255:0];
   reg	     [7:0]count;
   reg [63:0] 	      dr[255:0];
 
   
   @(posedge vm.master.SysReset_o);

   
   #100us;

/* -----\/----- EXCLUDED -----\/-----
   #(300ns);
   vm.write('h80000000, 'h09, 'h1, q);
   #(300ns);
   vm.write('h80000000, 'h09, 'h0, q);
   #(300ns);
   vm.write('h80000000, 'h09, 'h1, q);
   #(300ns);
   vm.write('h80000008, 'h09, 'h12345678, q);

   #(300ns);
   vm.read('h80000008, 'h09, r, q);
   #(300ns);
   vm.read('h80000000, 'h09, r, q);
  #(300ns);
   vm.read('h80000000, 'h09, r, q);
 -----/\----- EXCLUDED -----/\----- */

   #(300ns);
   for (i = 0; i < 256; i = i +1) begin
    	  	d[i]=i;	 
        $display("Data in main.sv:d[%d]= %d",i,d[i]);
    	end
   #1;
   
    vm.SSTwr(4, 'h80000000,40,8,1,d,q);
#300
   vm.SSTrd(4,'h80000000,40,8,1,dr,q);

end
   
   
endmodule
