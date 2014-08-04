vlib work
make
vsim -t 1ps -L unisim -c work.vme64x_tb
do wave1.do
run 100us