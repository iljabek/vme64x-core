vdel -all

acom -work vme64xcore -2002 "$DSN/../../VME64e_ActHDL_src/SharedComps.vhd"
acom -work vme64xcore -2002 "$DSN/../../VME64e_ActHDL_src/VME_pack.vhd"
acom -work vme64xcore -2002 "$DSN/../../VME64e_ActHDL_src/VME_bus.vhd"
acom -work vme64xcore -2002 "$DSN/../../VME64e_ActHDL_src/WB_bus.vhd"
acom -work vme64xcore -2002 "$DSN/../../VME64e_ActHDL_src/IRQ_controller.vhd"

acom -work vme64xcore -2002 "$DSN/../../IP_cores/CR.vhd"
acom -work vme64xcore -2002 "$DSN/../../IP_cores/CRAM.vhd"
acom -work vme64xcore -2002 "$DSN/../../IP_cores/FIFO.vhd"

acom -work vme64xcore -2002 "$DSN/../../VME64e_ActHDL_src/VME64xCore_Top.vhd"