#options(error = NULL)
options(scipen=999)
JONES_ACT <<- TRUE
Shock_rail <<- FALSE
Shock_ships <<- FALSE
Shock_pipes <<- TRUE
Inland_ships <<- TRUE
options(scipen=999)

parent_direct <- "/Users/josephtarr/Documents/allfed/refactored/refactored_allfed/"

unchanged_trucking_capacity <<- read.csv(paste(parent_direct,'data/Truck capacity (1).csv',sep=""))
unchanged_shipping_capacity <<- read.csv(paste(parent_direct,'data/Port capacity (5).csv',sep=""))
unchanged_shipping_capacity$ALL.CARGO <- 100*unchanged_shipping_capacity$ALL.CARGO


faf <<- read.csv(paste(parent_direct,"data/All freight.csv",sep=""))
faf$origin_state <-
  vapply(faf$dms_orig, getting_rid_of_number, character(1))
faf$destination_state <-
  vapply(faf$dms_dest, getting_rid_of_number, character(1))

transport_links <- read.csv(paste(parent_direct,'data/Transport links.csv',sep=""))


basic_states_data_frame <- read.csv("/Users/josephtarr/Documents/allfed/refactored/refactored_allfed/data/basic_states.csv")
basic_states <- basic_states_data_frame$x
source(paste(parent_direct,"code/city_to_state.R",sep=""),local=.GlobalEnv)
source(paste(parent_direct,"code/closest_port_and_rail.R",sep=""),local=.GlobalEnv)
source(paste(parent_direct,"code/category_of_cargo.R",sep=""),local=.GlobalEnv)
source(paste(parent_direct,"code/outputs.R"),sep="",local=.GlobalEnv)


run_shock_for_state <- function(state_name){
  STATE <<- state_name
  print(STATE)
  source(paste(parent_direct,"code/cleaner.R",sep=""),local=.GlobalEnv)
  source(paste(parent_direct,"code/process.R",sep=""),local=.GlobalEnv)
  outputs$tonne_mile_trucking_increase[state_index] <<- sum(processing_faf$trucking_distance*processing_faf$thousand.tons.in.2023*1000,na.rm = TRUE)
  outputs$percent_of_demand_satisfied[state_index] <<- sum(processing_faf$million.ton.miles.in.2023*(processing_faf$shocked_mode == "Unsatisfied demand"))/sum(processing_faf$million.ton.miles.in.2023)
  outputs$trucking_capacity[state_index] <<- sum(trucking_capacity$Surplus.trucks...20.ton.capacity)/sum(unchanged_trucking_capacity$Surplus.trucks...20.ton.capacity)
  outputs$shipping_capacity[state_index] <<- sum(capacity_shipping[2:6])/sum(unchanged_shipping_capacity[2:6])
  outputs$rail_capacity[state_index] <<- sum(rail_capacity)/sum(unchanged_rail_capacity)
  
}


for (state_index in 1:length(basic_states)){
  run_shock_for_state(basic_states[state_index])
  
}