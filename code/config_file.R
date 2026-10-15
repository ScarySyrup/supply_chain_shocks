#options(error = NULL)
options(scipen=999)

JONES_ACT <<- TRUE
Shock_rail <<- FALSE
Shock_ships <<- TRUE
Shock_pipes <<- FALSE



#Inland_ships <<- TRUE
Include_multi_mode <<- FALSE
options(scipen=999)

parent_direct <- "/Users/josephtarr/Documents/supply_chain_shocks/"
#eg  "/Users/josephtarr/Documents/supply_chain_shocks/"


#This runs the model for each state
run_for_every_state <- function(){
  source(paste(parent_direct,"code/master.R",sep=""),local=.GlobalEnv)
  run_for_all()
}


run_for_one_state <- function(state){
  source(paste(parent_direct,"code/master.R",sep=""),local=.GlobalEnv)
  run_shock_for_state(state)
}


#This plots on the us map
plot_us_map <- function(){
  source(paste(parent_direct,"code/plot/map_of_us_visualiser.R",sep=""))
}




#This plots the output
plot_output <- function(){
  source(paste(parent_direct,"code/plot/plotting.R",sep=""))
}




