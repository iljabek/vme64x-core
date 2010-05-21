asim VME64xCore_Top

wave clk_i
wave VME_RST_n_i
wave VME_bus_1/s_reset
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
wave VME_BBSY_n_i

wave VME_DTACK_OE_o
wave VME_DATA_DIR_o
wave VME_DATA_OE_o
wave VME_ADDR_DIR_o
wave VME_ADDR_OE_o

wave VME_bus_1/s_mainFSMstate
wave VME_bus_1/s_transferType
wave VME_bus_1/s_typeOfDataTransfer

wave s_CRAMaddr
wave s_CRAMdataIn
wave s_CRAMdataOut
wave s_CRAMwea

wave s_CRaddr
wave s_CRdata

wave VME_bus_1/s_CSRarray
wave VME_bus_1/s_CRregArray
wave VME_bus_1/s_cardSel
wave VME_bus_1/s_confAccess
wave s_locAddr

wave VME_bus_1/s_FUNC_ADER
wave VME_bus_1/s_FUNC_ADEM

wave VME_bus_1/s_moduleEnabl
wave VME_bus_1/s_FUNC_AM(0)

wave s_memReq
wave s_memAckWB
wave VME_bus_1/s_memAckCSR(2)
wave VME_bus_1/s_CSRaddressed
wave VME_bus_1/s_CRAMaddressed
wave VME_bus_1/s_CRaddressed 

wave VME_bus_1/s_BEG_CRAM
wave VME_bus_1/s_END_CRAM

wave WB_bus_1/IRQ_i
wave VME_IRQ_n_o
wave VME_IACKIN_n_i
wave VME_IACKOUT_n_o

wave IRQ_controller_1/s_IRQstate
wave IRQ_controller_1/s_IRQclearMask
wave IRQ_controller_1/s_IRQreg
wave IRQ_controller_1/s_wbIRQrisingEdge	
wave IRQ_controller_1/IDtoData_o

wave STB_o
wave ACK_i
wave CYC_o
wave WE_o
wave SEL_o
wave LOCK_o

wave VME_bus_1/s_phase1addr
wave VME_bus_1/s_phase2addr
wave VME_bus_1/s_phase3addr

wave VME_bus_1/s_incrementAddrPulse

wave VME_bus_1/s_addressingType	
wave VME_bus_1/s_XAMtype
wave VME_bus_1/s_berr
wave VME_bus_1/s_retry
wave VME_bus_1/s_addrOffset

wave s_FIFOempty
wave s_beatCount
wave WB_bus_1/s_2eFSMstate
wave DAT_o
wave DAT_i
wave ADR_o 


force clk_i 0 0, 1 0.5 ns -r 1 ns

force VME_RST_n_i 0 0, 1 2 ns

force VME_AS_n_i 1 0, 0 110 ns, 1 165 ns, 0 180 ns, 1 198 ns, 0 200 ns, 1 211 ns, 0 225 ns, 1 275 ns, 0 285 ns, 1 310 ns, 0 320 ns, 1 345 ns, 0 365 ns, 1 380 ns, 0 425 ns, 1 435 ns, 0 445 ns, 1 455 ns, 0 470 ns, 1 520 ns, 0 530 ns
force VME_DS_n_i "11" 0, "10" 120 ns, "11" 135 ns, "10" 145 ns, "11" 160 ns, "10" 181 ns, "11" 192 ns, "10" 201 ns, "11" 210 ns, "00" 226 ns, "11" 270 ns, "10" 290 ns, "11" 305 ns, "10" 325 ns, "11" 454 ns, "10" 472 ns, "11" 481 ns, "10" 488 ns, "00" 496 ns, "10" 504 ns, "00" 512 ns, "11" 518 ns, "10" 532 ns, "11" 541 ns, "10" 548 ns, "00" 556 ns

force VME_AM_i "16#2F" 0, "16#0D" 220 ns, "16#2F" 280 ns, "16#05" 420 ns, "16#0D" 440 ns, "16#20" 469 ns
force VME_ADDR_b "16#03FFFD" 0, "16#03FFB1" 170 ns, "16#03FFB7" 195 ns, "16#03800000" 220 ns, "16#007FFF" 280 ns, "16#7F800000" 420 ns, "16#7F800409" 529 ns, "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ" 553 ns
force VME_LWORD_n_b 1 0, 0 150 ns, 1 170 ns, 0 216 ns, 1 280 ns, 1 469 ns, 0 529 ns, Z 553 ns
force VME_WRITE_n_i 1 0, 0 144 ns, 1 220 ns, 0 280 ns, 1 315 ns, 0 496 ns, 1 529 ns

force VME_BBSY_n_i 1 1, 0 420 ns, 1 460 ns

force VME_DATA_b "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ" 0, "16#10" 144 ns, "16#FF" 169 ns, "16#49" 200 ns, "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ" 220 ns, "16#AB" 280 ns, "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ" 315 ns, "16#00000000" 469 ns, "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ" 553 ns
																					  #"16#34"
																					  #"16#05"
force VME_GA_i "011111"	0															  

#force s_WBdataOut "16#FFFFFFFF" 0
#force s_memAckWB 0 0, 1 250 ns, 0 260 ns
force WB_bus_1/ACK_i 0 0, 1 243 ns, 0 245 ns, 1 501 ns, 0 502 ns, 1 510 ns, 0 511 ns, 1 565 ns, 0 573 ns

force WB_bus_1/DAT_i "16#0123456789ABCDEF" 243 ns	

force WB_bus_1/IRQ_i "0000000" 0, "1000000" 350 ns	
force VME_IACKIN_n_i 1 0, 0 360 ns, 1 395 ns 



run 650 ns

endsim