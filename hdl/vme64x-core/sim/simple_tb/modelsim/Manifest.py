action = "simulation"
sim_tool = "modelsim"
sim_top = "top_tb"

# for general-cores
target = None

sim_post_cmd = "vsim -i top_tb"

modules = {
    "local": [ ".." ],
}
