outputs <<- data.frame(
  states = basic_states,
  tonne_mile_trucking_increase = rep(NA,length(basic_states)),
  percent_of_demand_satisfied = rep(NA,length(basic_states)),
  trucking_capacity = rep(NA,length(basic_states)),
  shipping_capacity = rep(NA,length(basic_states)),
  rail_capacity = rep(NA,length(basic_states))
)