closest_operating_port <- function(state,tonnes,shocked_ships,cargo){
  states_to_ship_from <- truck_transport_links
  states_to_ship_from <- states_to_ship_from[states_to_ship_from$dms_dest %in% state,]
  if (shocked_ships == TRUE){
    states_to_ship_from <- states_to_ship_from[!states_to_ship_from$dms_orig %in% state,]
  }
  for (k in 1:nrow(states_to_ship_from)){
    port <- states_to_ship_from$dms_orig[k]
    if (capacity_shipping[getting_rid_of_number(capacity_shipping$SURPLUS....000.tonnes)==port,type_of_cargo("ship",cargo)+1]- tonnes > 0){
      return(c(port = as.character(states_to_ship_from$dms_orig[k]), distance = as.character(states_to_ship_from$Average.distance[k])))
    }
  }
  #inland ships stuff goes here
  return(c(NA,NA))
}

closest_operating_rail <- function(state,tonnes,shocked_rail,cargo){
  states_to_rail_from <- truck_transport_links
  states_to_rail_from <- states_to_rail_from[states_to_rail_from$dms_orig %in% state,]
  if (shocked_rail == TRUE){
    states_to_rail_from <- states_to_rail_from[!states_to_rail_from$dms_dest %in% state,]
  }
  for (i2 in 1:nrow(states_to_rail_from)){
    station <- states_to_rail_from$dms_dest[i2]
    if (rail_capacity[basic_states==station]- tonnes > 0){
      return(c(station = as.character(states_to_rail_from$dms_orig[i2]), distance = as.character(states_to_rail_from$Average.distance[i2])))
    }
  }
  return(c(NA,NA))
}