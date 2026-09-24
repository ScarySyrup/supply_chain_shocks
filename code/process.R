library(dplyr)
processing_faf <<- read.csv(paste(parent_direct,"data/cleaned_faf.csv",sep=""))

source(paste(parent_direct,"code/capacity_rail_and_shipping.R",sep = ""),local=.GlobalEnv)
processing_faf$shocked_mode <- rep(NA,nrow(processing_faf))
processing_faf$trucking_distance <- rep(NA,nrow(processing_faf))
processing_faf$Outward_port_or_rail <- rep(NA,nrow(processing_faf))
processing_faf$Inward_port_or_rail <- rep(NA,nrow(processing_faf))
processing_faf$number_of_trucks_first_leg <- rep(NA,nrow(processing_faf))
processing_faf$number_of_trucks_second_leg <- rep(NA,nrow(processing_faf))
processing_faf$train_car_type <- rep(NA,nrow(processing_faf))
processing_faf$num_of_train_cars <- rep(NA,nrow(processing_faf))


some_variable_out_of_bounds <- function(){
  if (any(is.na(trucking_capacity$Surplus.trucks...20.ton.capacity)) | any(trucking_capacity$Surplus.trucks...20.ton.capacity<0)){
    return(TRUE)
  }
  if (any(rail_capacity<0)){
    return(TRUE)
  }
  if (any(capacity_shipping[2:6]<0)){
    return(TRUE)
  }
  return(FALSE)
}


reset_capacities_and_row <- function(row_index){
  capacity_shipping <<- capacity_shipping_before_running
  train_car_capacity <<- train_car_capacity_before_running
  trucking_capacity  <<- trucking_capacity_before_running
  rail_capacity <<- rail_capacity_before_running
  processing_faf$shocked_mode[row_index] <<- NA
  processing_faf$trucking_distance[row_index] <<- NA
  processing_faf$Outward_port_or_rail[row_index] <<- NA
  processing_faf$Inward_port_or_rail[row_index] <<- NA
  processing_faf$number_of_trucks_first_leg[row_index] <<- NA
  processing_faf$number_of_trucks_second_leg[row_index] <<- NA
  processing_faf$train_car_type[row_index] <<- NA
  processing_faf$num_of_train_cars[row_index] <<- NA
  
  
}

try_ship_then_rail_then_truck <- function(cargo,dest_state,orig_state,tonnes,row_index){
  found_route <<- FALSE
  
  base_trucking_distance_options <- transport_links[transport_links$dms_mode=="1-Truck",]
  base_trucking_distance_options <- base_trucking_distance_options[getting_rid_of_number(transport_links$dms_orig)==dest_state&getting_rid_of_number(transport_links$dms_dest)==orig_state,]
  base_trucking_distance <- as.numeric(base_trucking_distance_options$Average.distance[!is.na(base_trucking_distance_options$Average.distance)])
  
  
  #capacity_shipping_before_running <<- capacity_shipping
  #train_car_capacity_before_running <<- train_car_capacity
  #trucking_capacity_before_running <<- trucking_capacity
  #rail_capacity_before_running <<- rail_capacity
  

  
  closest_rail_in <- closest_operating_rail(orig_state,tonnes,Shock_rail,cargo)
  closest_rail_out <- closest_operating_rail(dest_state,tonnes,Shock_rail,cargo)
  
  
  closest_port_in <- closest_operating_port(orig_state,tonnes,Shock_ships,cargo)
  closest_port_out <- closest_operating_port(dest_state,tonnes,Shock_ships,cargo)
  if (!found_route){
    
    
    capacity_shipping_before_running <<- capacity_shipping
    trucking_capacity_before_running <<- trucking_capacity
    
    add_trucking_info(row_index = row_index,
                      only_mode = FALSE,
                      distance = as.numeric(closest_port_in["distance"]),
                      tonnes = tonnes,
                      cargo = cargo
      
    )
    add_trucking_info(row_index = row_index,
                      only_mode = FALSE,
                      distance = as.numeric(closest_port_out["distance"]),
                      tonnes = tonnes,
                      cargo = cargo
    )
    add_shipping_info(row_index = row_index,
                      closest_port_in["port"],
                      closest_port_out["port"],
                      cargo=cargo,
                      tonnes = tonnes
                      
                      
                      
      
    )
    if (some_variable_out_of_bounds()|processing_faf$trucking_distance[row_index]>base_trucking_distance){
      reset_capacities_and_row(row_index)
    }else{
      found_route <<- TRUE
    }
  }
  if (!found_route){
    train_car_capacity_before_running <<- train_car_capacity
    trucking_capacity_before_running <<- trucking_capacity
    rail_capacity_before_running <<- rail_capacity
    
    add_trucking_info(row_index = row_index,
                      only_mode = FALSE,
                      distance = as.numeric(closest_rail_in["distance"]),
                      tonnes = tonnes,
                      cargo = cargo
    )
    add_trucking_info(row_index = row_index,
                      only_mode = TRUE,
                      distance = as.numeric(closest_rail_out["distance"]),
                      tonnes = tonnes,
                      cargo = cargo
    )
    #print(filter(transport_links,dms_mode == "2-Rail",getting_rid_of_number(dms_orig) == closest_rail_out["station"],getting_rid_of_number(dms_dest) == closest_rail_in["station"])$Average.distance)
    add_rail_info(row_index = row_index,
      closest_rail_out["station"],
      closest_rail_in["station"],
      cargo,
      tonnes,
      distance = as.numeric(filter(transport_links,dms_mode == "2-Rail",getting_rid_of_number(dms_orig) == closest_rail_out["station"],getting_rid_of_number(dms_dest) == closest_rail_in["station"])$Average.distance)
      
    )
    if (some_variable_out_of_bounds()|processing_faf$trucking_distance[row_index]>base_trucking_distance){
      reset_capacities_and_row(row_index)
    }else{
      found_route <<- TRUE
    }
  }
  if (!found_route){
    trucking_capacity_before_running <<- trucking_capacity
    add_trucking_info(row_index = row_index,
                      only_mode = TRUE,
                      distance = base_trucking_distance,
                      tonnes = tonnes,
                      cargo = cargo
    )
    if (some_variable_out_of_bounds()){
      reset_capacities_and_row(row_index)
    }else{
      found_route <<- TRUE
    }
  }
  if (!found_route){
    processing_faf$shocked_mode[row_index] <<- "Unsatisfied demand"
  }
}






add_trucking_info <- function(row_index,only_mode,distance,tonnes,cargo){
  usable_trucks <- trucking_capacity
  usable_trucks <- filter(usable_trucks,Cargo.type == type_of_cargo("truck",cargo))
               #20 is truck capa.   #525 is distance per day.   #6hours to reload
  truck_no <<- (((tonnes)/(20))*((distance*2)/(525))+(6/(24*265)))/365
  if (only_mode == TRUE){
    processing_faf$shocked_mode[row_index] <<- "Trucking only"
    processing_faf$trucking_distance[row_index] <<- distance
    processing_faf$number_of_trucks_first_leg[row_index] <<- truck_no
    trucking_capacity$Surplus.trucks...20.ton.capacity[trucking_capacity$Cargo.type == type_of_cargo("truck",cargo)] <<- trucking_capacity$Surplus.trucks...20.ton.capacity[trucking_capacity$Cargo.type == type_of_cargo("truck",cargo)] - truck_no
  }
  if (only_mode == FALSE){
    if (is.na(processing_faf$number_of_trucks_first_leg[row_index])){
      processing_faf$number_of_trucks_first_leg[row_index] <<- truck_no
      processing_faf$trucking_distance[row_index] <<- distance
      trucking_capacity$Surplus.trucks...20.ton.capacity[trucking_capacity$Cargo.type == type_of_cargo("truck",cargo)] <<- trucking_capacity$Surplus.trucks...20.ton.capacity[trucking_capacity$Cargo.type == type_of_cargo("truck",cargo)] - truck_no
    }else{
      processing_faf$number_of_trucks_second_leg[row_index] <<- truck_no
      processing_faf$trucking_distance[row_index] <<- processing_faf$trucking_distance[row_index]+distance
      trucking_capacity$Surplus.trucks...20.ton.capacity[trucking_capacity$Cargo.type == type_of_cargo("truck",cargo)] <<- trucking_capacity$Surplus.trucks...20.ton.capacity[trucking_capacity$Cargo.type == type_of_cargo("truck",cargo)] - truck_no
    }
  }
}


add_shipping_info <- function(row_index,dest_state,orig_state,cargo,tonnes){
  processing_faf$Outward_port_or_rail[row_index] <<- orig_state
  processing_faf$Inward_port_or_rail[row_index] <<- dest_state
  processing_faf$shocked_mode[row_index] <<- "Ship and truck"
  column <- type_of_cargo("ship", cargo) + 1
  row1 <- getting_rid_of_number(capacity_shipping$SURPLUS....000.tonnes) %in% dest_state
  row2 <- getting_rid_of_number(capacity_shipping$SURPLUS....000.tonnes) %in% orig_state
  capacity_shipping[row1,column] <<- capacity_shipping[row1,column] - tonnes
  capacity_shipping[row2,column] <<- capacity_shipping[row2,column] - tonnes
  }

add_rail_info <- function(row_index,dest_state,orig_state,cargo,tonnes,distance){
  processing_faf$Outward_port_or_rail[row_index] <<- orig_state
  processing_faf$Inward_port_or_rail[row_index] <<- dest_state
  processing_faf$shocked_mode[row_index] <<- "Rail and truck"
  if (!is.na(orig_state)&!is.na(dest_state)){
    rail_capacity[basic_states==dest_state] <<- rail_capacity[basic_states==dest_state]- tonnes
    rail_capacity[basic_states==orig_state] <<- rail_capacity[basic_states==orig_state]- tonnes
    
    
    rail_truck_type <- type_of_cargo("train",cargo)
    r_t_t_b_v <- train_car_capacity$Train.car.type == rail_truck_type
    rail_truck_num <- (tonnes)
    
    
    #20 is truck capa.   #525 is distance per day
    rail_truck_num  <- (((tonnes)/(90))*((distance*2)/(541))+(6/(24*0.37)))/365
    train_car_capacity[r_t_t_b_v,]$Surplus.cars...90.tons.basis <-  train_car_capacity[r_t_t_b_v,]$Surplus.cars...90.tons.basis - rail_truck_num
    processing_faf$train_car_type[row_index] <<- train_car_capacity[r_t_t_b_v,]$Train.car.type
    processing_faf$num_of_train_cars[row_index] <<- rail_truck_num
    }

}


for (i1 in 1:nrow(processing_faf)){
  cargo <- processing_faf$sctg2[i1]
  orig_state <- processing_faf$origin_state[i1]
  dest_state <- processing_faf$destination_state[i1]
  tonnes <- processing_faf$thousand.tons.in.2023[i1]*1000
  if (nrow(processing_faf) > 0){
    if (processing_faf$Export.import.[i1] %in% c("Export","Import")){
      try_ship_then_rail_then_truck(cargo,dest_state,orig_state,tonnes,i1)
        
    }
    else if (processing_faf$Export.import.[i1] %in% c("Domestic")){
      try_ship_then_rail_then_truck(cargo,dest_state,orig_state,tonnes,i1)
    }
  }
}