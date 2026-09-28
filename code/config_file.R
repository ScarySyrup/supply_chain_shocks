#options(error = NULL)
options(scipen=999)

JONES_ACT <<- TRUE


Shock_rail <<- FALSE
Shock_ships <<- FALSE
Shock_pipes <<- TRUE



Inland_ships <<- TRUE
Include_multi_mode <<- FALSE
options(scipen=999)

parent_direct <- "/Users/josephtarr/Documents/allfed/supply_chain_shocks/"
#eg  "/Users/josephtarr/Documents/supply_chain_shocks/"


#This runs the model for each state
source(paste(parent_direct,"code/master.R",sep=""),local=.GlobalEnv)






#This plots the output
source(paste(parent_direct,"code/plot/plotting.R",sep=""))

