asim VME64xCore_Top

wave clk_i
wave VME_RST_n_i
wave VME_AS_n_i 	
wave VME_LWORD_n_b
wave VME_RETRY_n_o
wave VME_WRITE_n_i
wave VME_DS_n_i 	
wave VME_GA_i 	
wave VME_DTACK_n_o
wave VME_BERR_n_o	
wave VME_ADDR_b 	
wave VME_DATA_b 	
wave VME_AM_i

#wave CRAMaddr_o
#wave CRAMdata_i
#wave CRAMdata_o
#wave CRAMwea_o

wave s_CRaddr
wave s_CRdata

force clk_i 0 0, 1 0.5 ns -r 1 ns

force VME_RST_n_i 1 0

force VME_AS_n_i 1 0
force VME_DS_n_i "11" 0

force VME_AM_i "16#2F" 0
force VME_ADDR_b "16#000FFF" 0
force VME_LWORD_n_b 1 0
force VME_WRITE_n_i 1 0

force VME_GA_i "011111"

run 130 ns

endsim