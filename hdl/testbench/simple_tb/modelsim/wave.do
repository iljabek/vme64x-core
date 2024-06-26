onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/clk_i
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/rst_i
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/VME_AS_n_i
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/VME_LWORD_n_o
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/VME_LWORD_n_i
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/VME_RETRY_n_o
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/VME_RETRY_OE_o
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/VME_WRITE_n_i
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/VME_DS_n_i
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/VME_DTACK_n_o
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/VME_DTACK_OE_o
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/VME_BERR_n_o
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/VME_ADDR_i
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/VME_ADDR_o
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/VME_ADDR_DIR_o
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/VME_ADDR_OE_N_o
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/VME_DATA_i
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/VME_DATA_o
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/VME_DATA_DIR_o
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/VME_DATA_OE_N_o
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/VME_AM_i
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/VME_IACKIN_n_i
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/VME_IACK_n_i
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/VME_IACKOUT_n_o
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/stb_o
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/ack_i
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/dat_o
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/dat_i
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/adr_o
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/sel_o
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/we_o
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/cyc_o
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/err_i
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/stall_i
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/addr_decoder_i
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/addr_decoder_o
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/decode_start_o
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/decode_done_i
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/am_o
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/decode_sel_i
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/cr_csr_addr_o
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/cr_csr_data_i
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/cr_csr_data_o
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/cr_csr_we_o
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/module_enable_i
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/bar_i
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/INT_Level_i
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/INT_Vector_i
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/irq_pending_i
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/irq_ack_o
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/s_locDataIn
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/s_locDataOut
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/s_ADDRlatched
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/s_LWORDlatched_n
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/s_DSlatched_n
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/s_AMlatched
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/s_WRITElatched_n
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/s_vme_addr_reg
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/s_vme_data_reg
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/s_vme_lword_n_reg
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/s_vme_addr_dir
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/s_addressingType
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/s_transferType
add wave -noupdate -height 16 /top_tb/vme64xcore/Inst_VME_bus/s_mainFSMstate
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/s_conf_req
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/s_dataPhase
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/s_MBLT_Data
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/s_conf_sel
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/s_card_sel
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/s_irq_sel
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/s_err
add wave -noupdate /top_tb/vme64xcore/Inst_VME_bus/s_DS_latch_count
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {5417751726 ps} 0}
configure wave -namecolwidth 150
configure wave -valuecolwidth 100
configure wave -justifyvalue left
configure wave -signalnamewidth 0
configure wave -snapdistance 10
configure wave -datasetprefix 0
configure wave -rowmargin 4
configure wave -childrowmargin 2
configure wave -gridoffset 0
configure wave -gridperiod 1
configure wave -griddelta 40
configure wave -timeline 0
configure wave -timelineunits ns
update
WaveRestoreZoom {0 ps} {21356173874 ps}
