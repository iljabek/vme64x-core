
echo "###########################################################################"
echo "running scenario 1, change gg_scenario in the script to run other scenarios"
echo "###########################################################################"

vsim -gg_scenario=1 top_tb
do wave.do
radix -hexadecimal
run 5us
wave zoomfull
radix -hexadecimal
