`timescale 1ns/1ns

//this module don't take into account the power supply nor the geographical
//addressing of the various slots. Those connections if required must be
//implemented for each module.
//The same is true for U.D. pins on P2 and all the P0 connections.

  parameter XA64_D64 = 8'b00010001;
  parameter XA64_D64_B = 8'b00100001;

interface IVMEMaster;
   
   reg Trst_o;
   reg Tck_o;
   wire Tdo_i;
   reg 	Tdi_o;
   wire Tms_o;
   wire [31:0] D_b;
   reg 	       SysClk_ok;
   reg 	       Ds1_o;
   reg 	       Ds2_o;
   reg 	       Write_o;
   wire        DtAck_i;
   reg 	       As_o;
   reg 	       IAck_o; 
   reg [5:0]   Am_o;
   wire [31:1] A_b;
   wire [7:1]  Irq_i;
   wire        Retry_i;
   wire        Berr_i;
   reg 	       SysReset_o;
   wire        LWord_b;
   reg        a_Ds1;
   reg        a_Ds2;
   

   parameter g_verbose  = 1;

   initial begin
      a_Ds2             = 1'b1; 
      a_Ds1             = 1'b1;
      As_o              = 1'b1;
      IAck_o            = 1'b1;
      SysReset_o        = 1'b1;
      Write_o           = 1'b1;
      
   end
   

   reg [31:0] a_D = 32'hz;
   reg [31:1] a_A = 31'hz;
   reg 	      a_LWord = 1'bz;

   assign D_b      = a_D;
   assign A_b      = a_A;
   assign LWord_b  = a_LWord;

   assign Ds1_o = a_Ds1;
   assign Ds2_o = a_Ds2;
   
   //**********************************
//VME read cycle
//**********************************

   task read(
             input [31:0] Address,
             input [5:0] AddressModifier,
             output [31:0] DataRead,
             output [7:0] ExitCode);
      // 0=Done, 1=Timeout, 2=BusError, 3=Retry    
      integer i;
      
      a_A         = Address[31:1];
      a_D         = 32'hz;
      a_LWord	    = 1'b0;
      Am_o        = AddressModifier;
      Write_o     = 1'b1;
      # 10ns;
      As_o	    = 1'b0;
      #10ns;
      a_Ds1       = 1'b0;
      a_Ds2       = 1'b0;
      i=0;
      while (DtAck_i && ~(i==1000)) #10ns i=i+1;	
      if (DtAck_i) begin
         if (g_verbose) $display("VMEMaster: No module response to the vme read request @ 32'h%h : time out", Address);
        ExitCode    = 8'd1;
    	As_o	    = 1'b1;
    	a_Ds1       = 1'b1;
    	a_Ds2       = 1'b1;
    end else begin		
	    DataRead    = D_b;
	    # 10; 
	    As_o	    = 1'b1;
	    a_Ds1       = 1'b1;
	    a_Ds2       = 1'b1;
	    i=0;
	    while (~DtAck_i && ~(i==1000)) #10 i=i+1;	
	    if (~DtAck_i) begin
            if (g_verbose) $display("VMEMaster: Unable to close the vme read in a usefull time: time out");
            ExitCode = 8'd2;
        end else begin
             if (g_verbose) $display("VMEMaster: VME read cycle: d = %h at address %h of A32 address space at %t", DataRead, Address,$time);
             ExitCode   = 8'd0;
	    end
    end
    a_A         = 31'bz;
    a_LWord     = 1'bz;
    a_D     	= 32'bz;
   endtask // read
   
   //**********************************
   //VME write cycle
   //**********************************
   task write(input [31:0] Address, input [5:0] AddressModifier, input [31:0] Data, output [7:0] ExitCode); // 0=Done, 1=Timeout, 2=BusError, 3=Retry
      integer i;		
      a_A         = Address[31:1];
      a_LWord     = 1'b0;
      Am_o 	= AddressModifier;
      a_D 	= Data;
      Write_o	= 1'b0;
      #10ns;
      As_o	= 1'b0;
      #10ns;
      a_Ds1       = 1'b0;
      a_Ds2       = 1'b0;
      i=0;
      while (DtAck_i && Retry_i && Berr_i && ~(i==1000)) #10ns i=i+1; 
      if (~Berr_i) begin
	 if (g_verbose) $display("VMEMaster: Bus Error asserted");
	 ExitCode    = 8'h2;
	 As_o	    = 1'b1;
	 a_Ds1       = 1'b1;
	 a_Ds2       = 1'b1;
	 a_D	        = 32'hz;
         a_A         = 31'hz;
        
      
      
      end // if (~Berr_i)
      
 else if (~Retry_i) begin
	 if (g_verbose) $display("VMEMaster: Slave asserted retry request");
	 ExitCode    = 8'h3;
	 As_o	    = 1'b1;
	 a_Ds1       = 1'b1;
	 a_Ds2       = 1'b1;
	 a_D	        = 32'hz;
         a_A         = 31'hz;
      
      

      
 end // if (~Retry_i)
      
      

  
      else if (DtAck_i) begin
	 
	 if (g_verbose) $display("VMEMaster: No module response to the vme write request: time out");
	 ExitCode    = 8'h1;
	 As_o	    = 1'b1;
	 a_Ds1       = 1'b1;
	 a_Ds2       = 1'b1;
	 a_D	        = 32'hz;
	 a_A         = 31'hz;
 
      


      end // if (DtAck_i)
      
      
	 
       else begin		
	 # 10ns; 
	 As_o        = 1'b1;
	 a_Ds1       = 1'b1;
	 a_Ds2       = 1'b1;
	 a_D         = 32'hz;
	 a_A         = 31'hz;
	 i=0;
	 while (~DtAck_i && ~(i==1000)) #10ns i=i+1;	
	 if (~DtAck_i) begin
	    if (g_verbose)  $display("VMEMaster: Unable to close the vme write in a usefull time: time out");
	   
	    
	    
     	    ExitCode    = 8'h1;
	 
         end
	  
	  
	  
	  else begin 
            ExitCode =8'h0;
	    if (g_verbose) $display("VMEMaster: VME write cycle: d = %h at address %h address at %t",Data ,Address, $time);            
	  
	  end // else: !if(~DtAck_i)
	  
	        
      a_A         = 31'bz;
      a_LWord     = 1'bz;
      a_D     	= 32'bz;
       end // else: !if(DtAck_i)
      
       endtask // write
   
   
//* -----\/----- EXCLUDED -----\/-----

   //-**********************************
   //VME64x address phase for U6 format
   //-**********************************

   task APU6(
             input [63:0] Address,
	     input [7:0] Xam,
             input [5:0] AddressModifier,
             input [6:0] GA,
             input [7:0]BeatCount,
	     input [7:0]SubUnitMaster,
             output [7:0] ExitCode);
      
      // 0=Done, 1=Timeout, 2=BusError, 3=Retry    
      integer 		  i;

      // Start of Phase 1 Set XAM, and Base Address 
      a_A[31:8]= Address[31:8];
      a_A[7:1] = Xam[7:1];
      a_LWord = Xam[0];
      
      if ((Xam == XA64_D64) || (Xam == XA64_D64_B)) begin
         a_D = Address[63:32];
      end else begin
	 //	a_D = 32'hzzzzzzzz;
      end
      

 Am_o        = AddressModifier;
//Write_o     = wr;
# 10ns;
As_o	    = 1'b0;
#10ns;
a_Ds1       = 1'b0;
a_Ds2       = 1'b1;
// Wait for Dtack
i=0;  

ExitCode = 8'd0;
      
while (DtAck_i && ~(i==1000)) #10ns i=i+1;	
if (DtAck_i) begin
   //Dtack has not arrived on time. I display an error. 
   if (g_verbose) $display("VMEMaster: No module response to the vme read request @ 32'h%h : time out", Address);
   ExitCode    = 8'd1;
   As_o	    = 1'b1;
   a_Ds1       = 1'b1;
   a_Ds2       = 1'b1;
   
		 end
      
      
	 

 else begin

      // If Dtack arrived I start Phase 2
      // Define internal Addres A[7:0], beat count (number of words transmited),
      // Master's GA and subunit number in Master
      
      //    As_o	    = 1'b1;
      a_A[31:24]         = SubUnitMaster;
      a_A[20:16] = GA;
    a_A[15:8]=BeatCount;
    a_A[7:1] = Address[7:1];
    
    
      a_LWord = Address[0];    
      #10
      a_Ds1       = 1'b1;  // togle DS1 to indicate PH2 is valid  
      a_Ds2       = 1'b1;
      //a_A[31:8]         = Address[31:8];
     // a_A[7:1] = Address[7:1];
     // a_LWord = Address[0];
      
      // Wait for end of PH2
      i=0;
      while (~DtAck_i && ~(i==1000)) #10 i=i+1;	
      if (~DtAck_i) begin
	 // Dtack has not arrivwed on time
         if (g_verbose) $display("VMEMaster: Unable to close the vme read in a usefull time: time out");
         ExitCode = 8'd2;
      end else begin
	 // Slave activates Dtack on time. 
         // I enter in PH3 just by togglin DS1 again
	 a_Ds1       = 1'b0;  // togle DS1 to indicate PH2 is valid
	 // Wait for end of PH3
	 i=0;
	 while (~DtAck_i && ~(i==1000)) #10 i=i+1;	
	 if (~DtAck_i) begin
	    // Dtack has not arrivwed on time
            if (g_verbose) $display("VMEMaster: Unable to close the vme read in a usefull time: time out");
            ExitCode = 8'd2;
         end else begin
            // Slave activates Dtack on time. 
            // I exit the address phase. Data phase continues in a different funtion
	    
            ExitCode   = 8'd0;
	 end

	 
      end // else: !if(~DtAck_i)
 end // else: !if(DtAck_i)
	 
      a_A         = 31'bz;
      a_LWord     = 1'bz;
      a_D     	= 32'bz;
 endtask // APU6
   

   task wrOneWordSST(
		     input [63:0]Data,
                     input time Delay,
		     output [7:0] ExitCode);
      
      // for the moment I will do it simple.
      // I do not check retry or berr
      As_o	    = 1'b0;
      a_A = Data[63:33];    
      a_LWord = Data[32];
      a_D = Data[31:0];	 
      $display("Data= %d",Data);
      #Delay;
      a_Ds2 = ~ a_Ds2;
      #Delay;
      
      ExitCode =  8'd0;
   endtask // wrOneWordSST
   
   
   
   task SSTwrDataPH(
		    input [63:0]Data [255:0],
		    input [7:0]BeatCount,
		    input time Delay,
		    output [7:0] ExitCode);
      integer 			 i;
      i=0;
      // in principle RETRY and BERR should be checked in wrOneWordSST
      
      while (~(i==BeatCount)) begin
	 wrOneWordSST(Data[i], Delay, ExitCode);
	 $display("Data[i]= %d",Data[i]);

	 i=i+1;
      end
      
      i=0;
      a_Ds1 = 1'b1;
      a_Ds2 = 1'b1;
      
      while (~DtAck_i && ~(i==1000)) #10 i=i+1;
      if (~DtAck_i) begin
	 // Dtack has not arrivwed on time
         if (g_verbose) $display("VMEMaster: Unable to close the vme SST vme write in a usefull time: time out");
         ExitCode = 8'd2;
      end else begin
         // Slave activates Dtack on time. 
         ExitCode   = 8'd0;
      end
    
      
          
   endtask // SSTwrDataPH
   

   task rdOneWordSST(                    
					 output [63:0]Data,
					 output [7:0] ExitCode);
      
      // for the moment I will do it simple.
      // I do not check retry or berr
      integer 					      auxDtack;
      integer 					      i;
      
      auxDtack = DtAck_i;
      
 
      a_A = Data[63:33];    
      a_LWord = Data[32];
      a_D = Data[31:0];
      i = 0;
      while (DtAck_i == auxDtack && ~ (i==1000) ) #1 i=i+1;
      if (i==1000) begin
	 // Dtack has not arrivwed on time
         if (g_verbose) $display("VMEMaster: Unable to close the vme SST vme write in a usefull time: time out");
         ExitCode = 8'd2;
         end

    else begin
         // Slave activates Dtack on time. 
         ExitCode   = 8'd0;
      end
      
   endtask // rdOneWordSST
   
   
   task SSTrdDataPH(
		    output [63:0]Data [255:0],
		    input [7:0]BeatCount,
		    input time Delay,
		    output [7:0] ExitCode);
      integer 			 i;
      i=0;    
      ExitCode   = 8'd0;
      // in principle RETRY and BERR should be checked in wrOneWordSST
      
      while (~(i==BeatCount)) begin
	 rdOneWordSST(Data[i], ExitCode);
	 i=i+1;
      end
      
      i=0;
      a_Ds1 = 1'b1;
      a_Ds2 = 1'b1;
      
      while (~DtAck_i && ~(i==1000)) #10 i=i+1;
      if (~DtAck_i) begin
	 // Dtack has not arrivwed on time
         if (g_verbose) $display("VMEMaster: Unable to close the vme SST vme read in a usefull time: time out");
         ExitCode = 8'd2;
      end else begin
         // Slave activates Dtack on time. 
         ExitCode   = 8'd0;
      end
  
   endtask // SSTrdDataPH
   
   task SSTrd(         input [6:0] GA,
                       input [63:0] Address,
		       input time Delay,
		       input [7:0]BeatCount,
		       input [7:0]SubUnitMaster,
		       output [63:0]Data [255:0],
		       output [7:0] ExitCode);
      Write_o     = 1'b1;
      APU6(Address,8'h11,8'h20,GA,BeatCount,SubUnitMaster, ExitCode);
      
      if (ExitCode == 0) begin
	 SSTrdDataPH( Data, BeatCount, Delay,ExitCode);
      end
      As_o = 1'b1;         
      Write_o     = 1'bz;
   endtask // SSTrd
   
   task SSTwr(         input [6:0] GA,
		       input [63:0] Address,
		       input time Delay,
		       input [7:0]BeatCount,
		       input [7:0]SubUnitMaster,
		       input [63:0]Data[255:0],
		       output [7:0] ExitCode);
      Write_o     = 1'b0;
      APU6(Address,8'h11,8'h20,GA,BeatCount,SubUnitMaster, ExitCode);
     
      if (ExitCode == 0) begin
	 	 $display("[63:0]Data[1]= %d",Data[1]);

	 SSTwrDataPH(Data, BeatCount, Delay, ExitCode);
      end
             As_o = 1'b1;            
      Write_o     = 1'bz;
   endtask // SSTwr
 //-----/\----- EXCLUDED -----/\----- 
   
   modport master
     (
      output Trst_o,
      output  Tck_o,
      input Tdo_i,
      output  Tdi_o,
      output Tms_o,
      inout D_b,
      output  SysClk_ok,
      output  Ds1_o,
      output  Ds2_o,
      output  Write_o,
      input DtAck_i,
      output  As_o,
      output  IAck_o, 
      output Am_o,
      inout  A_b,
      input  Irq_i,
      input Retry_i,
      input Berr_i,
      output  SysReset_o,
      inout LWord_b
      );

   initial begin
      SysReset_o = 1'b1;
      #10000;
      SysReset_o = 1'b0;
      #500;
      SysReset_o = 1'b1;
      #200;
   end

   
   
endinterface // IVmeMaster

