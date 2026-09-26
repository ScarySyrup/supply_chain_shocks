#options(error = NULL)
options(scipen=999)

JONES_ACT <<- TRUE


Shock_rail <<- FALSE
Shock_ships <<- FALSE
Shock_pipes <<- TRUE



Inland_ships <<- TRUE
Include_multi_mode <<- FALSE
options(scipen=999)

parent_direct <- "/Users/josephtarr/Documents/allfed/refactored/refactored_allfed/"

#This runs the model for each state
source(paste(parent_direct,"code/master.R",sep=""),local=.GlobalEnv)




#This plots the output
source(paste(parent_direct,"code/plotting/plotting.R",sep=""))

