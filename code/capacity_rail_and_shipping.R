capacity_shipping <<- read.csv(paste(parent_direct,'data/Port capacity (5).csv',sep=""))
capacity_shipping$ALL.CARGO <- 100*capacity_shipping$ALL.CARGO
train_car_capacity <<- read.csv(paste(parent_direct,'data/Train car capacity.csv',sep=""))
train_car_capacity <- train_car_capacity[1:3,]
trucking_capacity <<- read.csv(paste(parent_direct,'data/Truck capacity (1).csv',sep=""))




rail_capacity <<- rep(0,length(basic_states))
for (i7 in 1:length(basic_states)){
  rail_capacity[i7] <- (sum(faf[faf$origin_state == basic_states[i7] | faf$destination_state == basic_states[i7],]$thousand.tons.in.2023))/2
}
unchanged_rail_capacity <<- rail_capacity