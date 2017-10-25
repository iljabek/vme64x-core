#!/bin/sh
set -e

function check_log()
{
  if grep -F '# ** ' sim.log | grep -E -v 'Note|Warning'; then
    echo "Simulation failed!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
    exit 1
  fi
}

for i in 1 2 3 4 5 6 7 8 9; do
  echo
  echo "Scenario $i"
  vsim -gg_scenario=$i -c -do "run 5us; quit" top_tb | tee sim.log
  check_log
done

echo "OK!"
