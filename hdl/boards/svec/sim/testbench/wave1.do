onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate -expand -group Top /vme64x_tb/uut/clk_i
add wave -noupdate -expand -group Top /vme64x_tb/uut/Reset
add wave -noupdate -expand -group Top /vme64x_tb/uut/VME_AS_n_i
add wave -noupdate -expand -group Top /vme64x_tb/uut/VME_RST_n_i
add wave -noupdate -expand -group Top /vme64x_tb/uut/VME_WRITE_n_i
add wave -noupdate -expand -group Top /vme64x_tb/uut/VME_AM_i
add wave -noupdate -expand -group Top /vme64x_tb/uut/VME_DS_n_i
add wave -noupdate -expand -group Top /vme64x_tb/uut/VME_GA_i
add wave -noupdate -expand -group Top /vme64x_tb/uut/VME_BERR_o
add wave -noupdate -expand -group Top /vme64x_tb/uut/VME_DTACK_n_o
add wave -noupdate -expand -group Top /vme64x_tb/uut/VME_RETRY_n_o
add wave -noupdate -expand -group Top /vme64x_tb/uut/VME_LWORD_n_b
add wave -noupdate -expand -group Top /vme64x_tb/uut/VME_ADDR_b
add wave -noupdate -expand -group Top /vme64x_tb/uut/VME_DATA_b
add wave -noupdate -expand -group Top /vme64x_tb/uut/VME_IRQ_n_o
add wave -noupdate -expand -group Top /vme64x_tb/uut/VME_IACKIN_n_i
add wave -noupdate -expand -group Top /vme64x_tb/uut/VME_IACKOUT_n_o
add wave -noupdate -expand -group Top /vme64x_tb/uut/VME_IACK_n_i
add wave -noupdate -expand -group Top /vme64x_tb/uut/VME_RETRY_OE_o
add wave -noupdate -expand -group Top /vme64x_tb/uut/VME_DTACK_OE_o
add wave -noupdate -expand -group Top /vme64x_tb/uut/VME_DATA_DIR_o
add wave -noupdate -expand -group Top /vme64x_tb/uut/VME_DATA_OE_N_o
add wave -noupdate -expand -group Top /vme64x_tb/uut/VME_ADDR_DIR_o
add wave -noupdate -expand -group Top /vme64x_tb/uut/VME_ADDR_OE_N_o
add wave -noupdate -expand -group Top /vme64x_tb/uut/leds
add wave -noupdate -expand -group Top /vme64x_tb/uut/WbDat_i
add wave -noupdate -expand -group Top /vme64x_tb/uut/WbDat_o
add wave -noupdate -expand -group Top /vme64x_tb/uut/WbAdr_o
add wave -noupdate -expand -group Top /vme64x_tb/uut/WbCyc_o
add wave -noupdate -expand -group Top /vme64x_tb/uut/WbErr_i
add wave -noupdate -expand -group Top /vme64x_tb/uut/WbRty_i
add wave -noupdate -expand -group Top /vme64x_tb/uut/WbSel_o
add wave -noupdate -expand -group Top /vme64x_tb/uut/WbStb_o
add wave -noupdate -expand -group Top /vme64x_tb/uut/WbAck_i
add wave -noupdate -expand -group Top /vme64x_tb/uut/WbWe_o
add wave -noupdate -expand -group Top /vme64x_tb/uut/WbStall_i
add wave -noupdate -expand -group Top /vme64x_tb/uut/WbIrq_i
add wave -noupdate -expand -group Top /vme64x_tb/uut/WbMemDat_i
add wave -noupdate -expand -group Top /vme64x_tb/uut/WbMemDat_o
add wave -noupdate -expand -group Top /vme64x_tb/uut/WbMemAdr_i
add wave -noupdate -expand -group Top /vme64x_tb/uut/WbMemCyc_i
add wave -noupdate -expand -group Top /vme64x_tb/uut/WbMemErr_o
add wave -noupdate -expand -group Top /vme64x_tb/uut/WbMemRty_o
add wave -noupdate -expand -group Top /vme64x_tb/uut/WbMemSel_i
add wave -noupdate -expand -group Top /vme64x_tb/uut/WbMemStb_i
add wave -noupdate -expand -group Top /vme64x_tb/uut/WbMemAck_o
add wave -noupdate -expand -group Top /vme64x_tb/uut/WbMemWe_i
add wave -noupdate -expand -group Top /vme64x_tb/uut/WbMemStall_o
add wave -noupdate -expand -group Top /vme64x_tb/uut/clk_in_buf
add wave -noupdate -expand -group Top /vme64x_tb/uut/clk_in
add wave -noupdate -expand -group Top /vme64x_tb/uut/s_locked
add wave -noupdate -expand -group Top /vme64x_tb/uut/s_fb
add wave -noupdate -expand -group Top /vme64x_tb/uut/s_INT_ack
add wave -noupdate -expand -group Top /vme64x_tb/uut/s_rst_n
add wave -noupdate -expand -group Top /vme64x_tb/uut/s_VME_DATA_b_o
add wave -noupdate -expand -group Top /vme64x_tb/uut/s_VME_DATA_DIR
add wave -noupdate -expand -group Top /vme64x_tb/uut/s_VME_ADDR_DIR
add wave -noupdate -expand -group Top /vme64x_tb/uut/s_VME_ADDR_b_o
add wave -noupdate -expand -group Top /vme64x_tb/uut/s_VME_LWORD_n_b_o
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {8963000 ps} 0}
configure wave -namecolwidth 150
configure wave -valuecolwidth 100
configure wave -justifyvalue left
configure wave -signalnamewidth 1
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
WaveRestoreZoom {97409 ns} {100137 ns}
