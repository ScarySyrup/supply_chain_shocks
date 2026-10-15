cap_ship_fac <- 1000
capacity_shipping <<- read.csv(paste(parent_direct,'data/Port capacity (5).csv',sep=""))
capacity_shipping$ALL.CARGO <- capacity_shipping$ALL.CARGO * cap_ship_fac




capacity_shipping$X16.Crude.petroleum <- capacity_shipping$X16.Crude.petroleum * cap_ship_fac
capacity_shipping$X19.Natural.gas.and.other.fossil.products <- capacity_shipping$X19.Natural.gas.and.other.fossil.products * cap_ship_fac
capacity_shipping$Other.fuels.liquids <- capacity_shipping$Other.fuels.liquids * cap_ship_fac
capacity_shipping$X02.Cereal.grains <- capacity_shipping$X02.Cereal.grains * cap_ship_fac

train_car_capacity <<- read.csv(paste(parent_direct,'data/Train car capacity.csv',sep=""))
train_car_capacity <- train_car_capacity[1:3,]
trucking_capacity <<- read.csv(paste(parent_direct,'data/Truck capacity (1).csv',sep=""))



  
rail_capacity <<- rep(0,length(basic_states))
for (i7 in 1:length(basic_states)){
  rail_capacity[i7] <- 1000*0.2*((sum(faf[faf$origin_state == basic_states[i7] | faf$destination_state == basic_states[i7],]$thousand.tons.in.2023))/2)
}