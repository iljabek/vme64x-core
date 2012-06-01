asim WB_bus

wave clk_i
wave reset_i

wave STB_o
wave ACK_i
wave CYC_o
wave WE_o
wave SEL_o
wave LOCK_o
wave STALL_i

wave RW_i
wave FIFOrden_o       
wave FIFOwren_o       
wave FIFOdata_i       
wave FIFOdata_o       
wave writeFIFOempty_i
wave TWOeInProgress_i 
wave WBbusy_o
wave beatCount_i
wave s_runningBeatCount
wave s_beatCountEnd
wave s_addrLatch
wave locAddr_i
wave s_locAddr
wave s_ackCount
wave s_ackCountEnd

wave s_2eFSMstate

force clk_i 0 0, 1 0.5 ns -r 1 ns
force reset_i 1 0, 0 2 ns

force locAddr_i 0 0
force writeFIFOempty_i 0 0, 1 7 ns, 0 8 ns
force STALL_i 0 0, 1 10 ns, 0 12 ns, 1 48 ns, 0 55 ns
force RW_i 0 0, 1 40 ns
force ACK_i 0 0, 1 20 ns, 0 29 ns, 1 62 ns, 0 71 ns

force TWOeInProgress_i 1 0, 0 25 ns, 1 40 ns, 0 60 ns

force beatCount_i "16#09" 0



run 100 ns

endsim