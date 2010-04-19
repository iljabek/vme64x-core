asim VME_bus

wave clk_i
wave VME_RST_n_i
wave s_reset
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

wave s_mainFSMstate
wave s_mainFSMreset
#wave s_VMEaddrLatch
#wave s_memReq
#wave s_memAckWB
#wave s_dataToOutput
#wave s_dataToAddrBus
wave s_locAddr
#wave s_locAddrBeforeOffset
#wave s_addrOffset 
wave s_CrCsrOffsetAddr
#wave s_incrementAddr

wave s_cardSel
wave s_confAccess

wave s_CRaddressed
wave s_CSRaddressed
wave s_CRAMaddressed

wave s_locDataIn
wave s_locDataOut

#wave s_funcMatch(0)
#wave s_funcMatch(1)
#wave s_funcMatch(2)
#wave s_funcMatch(3)

#wave CRAMaddr_o
#wave CRAMdata_i
#wave CRAMdata_o
#wave CRAMwea_o

wave s_GAparityMatch

wave s_initState
wave s_initReadCounter
wave s_latchCRdata
wave s_initInProgress
wave CRaddr_o
wave s_CRregArray

force clk_i 0 0, 1 0.5 ns -r 1 ns

force VME_RST_n_i 1 0, 0 25 ns, 1 28 ns

force VME_AS_n_i 1 0
force VME_DS_n_i "11" 0

force s_memAckWB 0 0

force s_FUNC_ADER(0) "16#FFFFFFFFE0000000" 0
force s_FUNC_ADEM(0) "16#FFFFFFFFE0000000" 0
force s_FUNC_ADER(1) "16#FFFFFFFFFFFFFFFF" 0
force s_FUNC_ADEM(1) "16#FFFFFFFFFFFFFFFF" 0
force s_FUNC_ADER(2) "16#FFFFFFFFFFFFFFFF" 0
force s_FUNC_ADEM(2) "16#FFFFFFFFFFFFFFFF" 0
force s_FUNC_ADER(3) "16#FFFFFFFFFFFFFFFF" 0
force s_FUNC_ADEM(3) "16#FFFFFFFFFFFFFFFF" 0 

#force s_BEG_CRAM "16#01FFF" 0
#force s_END_CRAM "16#0FFFF" 0
#
force CRdata_i "16#AB" 0

force VME_AM_i "16#2F" 0
force VME_ADDR_b "16#000FFF" 0
force VME_LWORD_n_b 1 0
force VME_WRITE_n_i 1 0

force VME_GA_i "011111"

run 130 ns

endsim