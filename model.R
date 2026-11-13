library(dplyr)
processing_faf <<- read.csv(paste(parent_direct,"data/cleaned_faf.csv",sep=""))

capacity_shipping <<- unchanged_shipping_capacity
capacity_trucking <<- unchanged_truck_capacity
capacity_rail_cars <<- unchanged_rail_car_capacity
capacity_rail <<- unchanged_rail_capacity




column_for_shipping_cargo <- function(cargo){
  column_for_cargo <- if (cargo == "16-Crude petroleum") {
    3}else if (cargo == "19-Natural gas and other fossil products"){
      4}else if (cargo %in% c("17-Gasoline", "18-Fuel oils")){ 
        5}else if (cargo == "02-Cereal grains"){
          6}else{
            2}
  return(column_for_cargo)
}




new_cols <- c("shocked_mode", "trucking_distance", "Outward_port_or_rail",
              "Inward_port_or_rail", "truck_type", "number_of_trucks_first_leg",
              "number_of_trucks_second_leg", "train_car_type",
              "num_of_train_cars", "different_export_import")

processing_faf[new_cols] <- NA

num_of_trucks <- function(distance,tonnes){
  return((((tonnes)/(20))*((as.numeric(distance)*2)/(525))+(6/(24*365)))/365)
}

num_of_rail_cars <- function(distance,tonnes){
  return((((tonnes)/(90))*((as.numeric(distance)*2)/(541))+(6/(24*0.37)))/365)
}

add_shipping_info <- function(row_index,orig_state,dest_state){
  processing_faf$shocked_mode[row_index] <<- "Ship and truck"
  processing_faf$Inward_port_or_rail[row_index] <<- dest_state
  processing_faf$Outward_port_or_rail[row_index] <<- orig_state
}

add_rail_info <- function(row_index,orig_state,dest_state,rail_car_type,rail_car_num){
  processing_faf$shocked_mode[row_index] <<- "Rail and truck"
  processing_faf$Inward_port_or_rail[row_index] <<- dest_state
  processing_faf$Outward_port_or_rail[row_index] <<- orig_state
  processing_faf$train_car_type[row_index] <<- rail_car_type
  processing_faf$num_of_train_cars[row_index] <<- rail_car_num
}




reset_capacities_and_clear <- function(row_index){
  capacity_shipping <<- capacity_shipping_before_reset 
  capacity_rail_cars <<- capacity_rail_cars_before_reset 
  capacity_rail <<- capacity_rail_before_reset 
  capacity_trucking <<- capacity_trucking_before_reset
  
  processing_faf[row_index,new_cols] <<- NA
}

add_trucking_info <- function(row_index,dest_state,type_of_truck,leg_one_or_two,num_of_trucks,only_mode,distance){
  if (only_mode == TRUE){
    processing_faf$shocked_mode[row_index] <<- "Trucking only"
  }
  if (leg_one_or_two == "one"){
    processing_faf$number_of_trucks_first_leg[row_index] <<- num_of_trucks
    processing_faf$trucking_distance[row_index] <<- distance
  }
  else if (leg_one_or_two == "two"){
    processing_faf$number_of_trucks_second_leg[row_index] <<- num_of_trucks
    processing_faf$trucking_distance[row_index] <<-  processing_faf$trucking_distance[row_index] + distance
  }
  processing_faf$truck_type[row_index] <<- type_of_truck
}


row_and_capacities_valid <- function(row_index){
  if (is.na(processing_faf$trucking_distance[row_index])){
    return(FALSE)
  }
  if(any(capacity_shipping[,2:6] < 0,na.rm = TRUE)){
    return(FALSE)
  }
  if (any(capacity_trucking$Surplus.trucks...20.ton.capacity<0,na.rm = TRUE)){
      return(FALSE)
  }
  if (any(capacity_rail < 0,na.rm = TRUE)){
    return(FALSE)
  }
  if (any(capacity_rail_cars$Surplus.cars...90.tons.basis<0,na.rm=TRUE) ){
    return(FALSE)
  }
  return(TRUE)
}


add_capacity <- function(cargo,tonnes,state,mode,distance){
  if (mode == "3-Water"){
    column_for_cargo <- column_for_shipping_cargo(cargo)
    port_index <- match(state,capacity_shipping$SURPLUS....000.tonnes)
    capacity_shipping[port_index,column_for_cargo] <<- capacity_shipping[port_index,column_for_cargo] - (tonnes/1000)
  }
  if (mode == "2-Rail"){
    
    rail_car_num <- num_of_rail_cars(distance = distance,tonnes = tonnes)
    row_for_cargo <- match(cargo_types$Train.category[(match(cargo,cargo_types$Cargo.type))],capacity_rail_cars$Train.car.type)
    capacity_rail_cars$Surplus.cars...90.tons.basis[row_for_cargo] <<- capacity_rail_cars$Surplus.cars...90.tons.basis[row_for_cargo] - rail_car_num 

    rail_index <- match(state,basic_states)
    capacity_rail[rail_index] <<- capacity_rail[rail_index] - tonnes
  }
  if (mode == "1-Truck" ){
    num_of_trucks  <- num_of_trucks(distance = distance,tonnes = tonnes)
    truck_type <- cargo_types$Truck.category[match(cargo,cargo_types$Cargo.type)]
    capacity_trucking$Surplus.trucks...20.ton.capacity[match(truck_type,capacity_trucking$Cargo.type)] <<- capacity_trucking$Surplus.trucks...20.ton.capacity[match(truck_type,capacity_trucking$Cargo.type)] - num_of_trucks
    
  }
  
}


try_ship_then_rail_then_truck <- function(cargo,dest_state,orig_state,tonnes,row_index,different_export_import){
  
  
  capacity_shipping_before_reset <<- capacity_shipping
  capacity_rail_cars_before_reset <<- capacity_rail_cars
  capacity_rail_before_reset <<- capacity_rail
  capacity_trucking_before_reset <<- capacity_trucking
  
  
  
  trucking_options <- filter(truck_links,dms_orig == dest_state,dms_dest == orig_state)
  base_trucking_distance <- as.numeric(trucking_options$Average.distance)
  
  
  
  routed <<- FALSE
  if (!routed & !scenario$JONES_ACT){
    closest_port_in <- closest_port_that_can_handle_this_load(orig_state,tonnes,cargo)
    closest_port_out <- closest_port_that_can_handle_this_load(dest_state,tonnes,cargo)
    


    
    if ((!anyNA(closest_port_out))&(!anyNA(closest_port_in))){
      
      add_shipping_info(row_index,closest_port_in["port"],closest_port_out["port"])
      add_capacity(cargo,tonnes,closest_port_in["port"],"3-Water",NA)
      add_capacity(cargo,tonnes,closest_port_out["port"],"3-Water",NA)
      
      
      add_trucking_info(row_index,
                        closest_port_in["port"],
                        type_of_truck = cargo_types$Truck.category[match(cargo,cargo_types$Cargo.type)],
                        leg = "one",
                        num_of_trucks = num_of_trucks(distance = as.numeric(closest_port_in["distance"]),tonnes = tonnes),
                        only_mode = FALSE,
                        distance = as.numeric(closest_port_in["distance"])
                        )
      add_capacity(cargo,tonnes,NA,"1-Truck",as.numeric(closest_port_in["distance"]))
      
      add_trucking_info(row_index,
                        closest_port_out["port"],
                        type_of_truck = cargo_types$Truck.category[match(cargo,cargo_types$Cargo.type)],
                        leg = "two",
                        num_of_trucks = num_of_trucks(distance = as.numeric(closest_port_out["distance"]),tonnes = tonnes),
                        only_mode = FALSE,
                        distance = as.numeric(closest_port_out["distance"])
      )
      add_capacity(cargo,tonnes,NA,"1-Truck",as.numeric(closest_port_out["distance"]))
      
    if (row_and_capacities_valid(row_index)){
      routed <<- TRUE
    }else{
      reset_capacities_and_clear(row_index)
    }
    }
  }
  if (!routed){
    closest_rail_in <<- closest_rail_that_can_handle_this_load(orig_state,tonnes,cargo)
    closest_rail_out <<- closest_rail_that_can_handle_this_load(dest_state,tonnes,cargo)
    if ((!anyNA(closest_rail_in))&(!anyNA(closest_rail_out))){
      
      
      
      rail_cargo_type <<- cargo_types$Train.category[(match(cargo,cargo_types$Cargo.type))]
      if (is.na(rail_cargo_type)){
        rail_cargo_type <- "Bulk"
      }
      temp <- filter(rail_links, dms_orig == dest_state, dms_dest == orig_state)$Average.distance
      distance_over_railway <- suppressWarnings(as.numeric(temp))
      distance_over_railway[is.na(distance_over_railway)] <- 0
      
      add_rail_info(row_index,
                    closest_rail_in["station"],
                    closest_rail_out["station"],
                    rail_cargo_type,
                    num_of_rail_cars(distance = distance_over_railway,tonnes = tonnes))
  
      add_capacity(cargo,tonnes,closest_rail_in["station"],"2-Rail",as.numeric(closest_rail_in["distance"]))
      add_capacity(cargo,tonnes,closest_rail_out["station"],"2-Rail",as.numeric(closest_rail_out["distance"]))
      
      
      
      
      
      
      add_trucking_info(row_index,
                        closest_rail_in["station"],
                        type_of_truck = cargo_types$Truck.category[match(cargo,cargo_types$Cargo.type)],
                        leg = "one",
                        num_of_trucks = num_of_trucks(distance = as.numeric(closest_rail_in["distance"]),tonnes = tonnes),
                        only_mode = FALSE,
                        distance = as.numeric(closest_rail_in["distance"]))
      
      add_capacity(cargo,tonnes,NA,"1-Truck",as.numeric(closest_rail_in["distance"]))
      
      add_trucking_info(row_index,
                        closest_rail_out["station"],
                        type_of_truck = cargo_types$Truck.category[match(cargo,cargo_types$Cargo.type)],
                        leg = "two",
                        num_of_trucks = num_of_trucks(distance = as.numeric(closest_rail_out["distance"]),tonnes = tonnes),
                        only_mode = FALSE,
                        distance =  as.numeric(closest_rail_out["distance"]))
      
      add_capacity(cargo,tonnes,NA,"1-Truck",as.numeric(closest_rail_out["distance"]))
      
      
      if (row_and_capacities_valid(row_index)){
        routed <<- TRUE
      } else{
        reset_capacities_and_clear(row_index)
      }
    }
  }
  if (is.na(processing_faf$trucking_distance[row_index]) ||
      processing_faf$trucking_distance[row_index] >= base_trucking_distance || !routed){
    reset_capacities_and_clear(row_index)
    add_trucking_info(row_index,
                      NA,
                      type_of_truck = cargo_types$Truck.category[match(cargo,cargo_types$Cargo.type)],
                      leg = "one",
                      num_of_trucks = num_of_trucks(base_trucking_distance,tonnes),
                      only_mode = TRUE,
                      distance = base_trucking_distance
    )
    add_capacity(cargo,tonnes,NA,"1-Truck",base_trucking_distance)
    if (row_and_capacities_valid(row_index)){
      routed <<- TRUE
    } else{
      reset_capacities_and_clear(row_index)
    }
    
  }
  processing_faf$different_export_import[row_index] <<- different_export_import
}









closest_port_that_can_handle_this_load <- function(state,tonnes,cargo){
  states_to_ship_from <- filter(truck_links,dms_orig==state)
  states_to_ship_from <- states_to_ship_from[order(as.numeric(states_to_ship_from$Average.distance)),]
  if (scenario$Shock_ships == TRUE){
    states_to_ship_from <- filter(states_to_ship_from,!dms_dest %in% scenario$states_shocked)
  }
  for (port_index in 1:nrow(states_to_ship_from)){
    port <- states_to_ship_from$dms_dest[port_index]
    column_for_cargo <- if (cargo == "16-Crude petroleum") {
                        3}else if (cargo == "19-Natural gas and other fossil products"){
                        4}else if (cargo %in% c("17-Gasoline", "18-Fuel oils")){ 
                        5}else if (cargo == "02-Cereal grains"){
                        6}else{
                        2}
    if (capacity_shipping[match(port,capacity_shipping$SURPLUS....000.tonnes),column_for_cargo] - (tonnes/1000) > 0){
      return(c(port = as.character(states_to_ship_from$dms_dest[port_index]), distance = as.numeric(states_to_ship_from$Average.distance[port_index])))
    }
  }
  return(c(port = NA, distance = NA))
}

closest_rail_that_can_handle_this_load <- function(state,tonnes,cargo){
  states_to_rail_from <- filter(truck_links,dms_orig==state)
  states_to_rail_from <- states_to_rail_from[order(as.numeric(states_to_rail_from$Average.distance)),]
  if (scenario$Shock_rail == TRUE){
    states_to_rail_from <- filter(states_to_rail_from,!dms_dest %in% scenario$states_shocked)
  }
  for (rail_index in 1:nrow(states_to_rail_from)){
    
    temp <- filter(rail_links, dms_orig == state, dms_dest == states_to_rail_from$dms_dest[rail_index])$Average.distance
    distance_over_railway <- suppressWarnings(as.numeric(temp))
    distance_over_railway[is.na(distance_over_railway)] <- 0
    
    
    
    rail_car_num <- num_of_rail_cars(distance = distance_over_railway,tonnes = tonnes)
    row_for_cargo <- match(cargo_types$Train.category[(match(cargo,cargo_types$Cargo.type))],capacity_rail_cars$Train.car.type)
    if (is.na(row_for_cargo)){
      row_for_cargo <- 1
    }
    if (!(anyNA(capacity_rail_cars$Surplus.cars...90.tons.basis))){
      if (capacity_rail_cars$Surplus.cars...90.tons.basis[row_for_cargo] - rail_car_num > 0){
        if (capacity_rail[match(states_to_rail_from$dms_dest[rail_index],basic_states)]- tonnes > 0){
          return(c(station = as.character(states_to_rail_from$dms_dest[rail_index]),distance = as.numeric(states_to_rail_from$Average.distance[rail_index])))
        }
      }
    }
  }
  return(c(station = NA, distance = NA))
}
paste


for (row_index in 1:nrow(processing_faf)){
  if (row_index %% 100 == 0){
    print(paste(round(row_index/nrow(processing_faf)*100),"%",sep=""))
  }
  
  
  cargo <- processing_faf$sctg2[row_index]
  dest_state <- processing_faf$dms_dest[row_index]
  orig_state <- processing_faf$dms_orig[row_index]
  tonnes <- processing_faf$thousand.tons.in.2023[row_index] * 1000
  
  row_satisfied <<- FALSE
  if (processing_faf$Export.import.[row_index] %in% c("Export","Import")){
    if (processing_faf$Export.import.[row_index] == "Export"){
      diff_state <<- dest_state
    }else{
      diff_state <<- orig_state
    }
    
    new_ports <- filter(truck_links,dms_orig == diff_state,!dms_dest %in% scenario$states_shocked)
    new_ports <- new_ports[order(as.numeric(new_ports$Average.distance)),]
 
    for (new_port in new_ports$dms_dest){
      
      if (processing_faf$Export.import.[row_index] == "Export"){
        dest_state <<- new_port
        
      }else{
        orig_state <<- new_port
      }
      if (capacity_shipping[match(new_port,capacity_shipping$SURPLUS....000.tonnes),column_for_shipping_cargo(cargo)]-(tonnes/1000)>0){
        try_ship_then_rail_then_truck(cargo,dest_state,orig_state,tonnes,row_index,different_export_import=new_port)
      }
      
      if (row_and_capacities_valid(row_index)){
        if (!processing_faf$dms_mode[row_index] == "3-Water"){
          capacity_shipping[match(new_port,capacity_shipping$SURPLUS....000.tonnes),column_for_shipping_cargo(cargo)] <- capacity_shipping[match(new_port,capacity_shipping$SURPLUS....000.tonnes),column_for_shipping_cargo(cargo)]-(tonnes/1000)
        }
        break
      }else{
        reset_capacities_and_clear(row_index)
      }
    }
    
  }
  else if (processing_faf$Export.import.[row_index] %in% c("Domestic")){
    try_ship_then_rail_then_truck(cargo,dest_state,orig_state,tonnes,row_index,different_export_import=NA)
  }
}



for (i in 1:nrow(processing_faf)){
  if (is.na(processing_faf$shocked_mode[i])){
    processing_faf$shocked_mode[i] <- "Unsatisfied demand"
  }
}