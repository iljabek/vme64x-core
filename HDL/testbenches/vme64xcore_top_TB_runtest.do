SetActiveLib -work
comp -include "C:\Users\tom\CSL\vme64\FAIR-VME64ext\trunk\HDL\VME64e_ActHDL_src\VME64xCore_Top.vhd" 
comp -include "$dsn\src\vme64xcore_top_TB.vhd" 
asim TESTBENCH_FOR_vme64xcore_top 
wave 
wave -noreg clk_i
wave -noreg VME_AS_n_i
wave -noreg VME_RST_n_i
wave -noreg VME_WRITE_n_i
wave -noreg VME_AM_i
wave -noreg VME_DS_n_i
wave -noreg VME_GA_i
wave -noreg VME_BERR_n_o
wave -noreg VME_DTACK_n_o
wave -noreg VME_RETRY_n_o
wave -noreg VME_LWORD_n_b
wave -noreg VME_ADDR_b
wave -noreg VME_DATA_b
wave -noreg VME_BBSY_n_i
wave -noreg VME_IRQ_n_o
wave -noreg VME_IACKIN_n_i
wave -noreg VME_IACKOUT_n_o
wave -noreg RST_i
wave -noreg DAT_i
wave -noreg DAT_o
wave -noreg ADR_o
wave -noreg TGA_o
wave -noreg TGC_o
wave -noreg CYC_o
wave -noreg ERR_i
wave -noreg LOCK_o
wave -noreg RTY_i
wave -noreg SEL_o
wave -noreg STB_o
wave -noreg ACK_i
wave -noreg WE_o
wave -noreg IRQ_i
# The following lines can be used for timing simulation
# acom <backannotated_vhdl_file_name>
# comp -include "$dsn\src\TestBench\vme64xcore_top_TB_tim_cfg.vhd" 
# asim TIMING_FOR_vme64xcore_top 
