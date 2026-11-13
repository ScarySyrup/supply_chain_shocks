library(dplyr)
faf <<- read.csv("Documents/refactored_allfed/data/All freight.csv")

faf <<- faf[!faf$dms_orig %in% c("11-Washington DC", "02-Alaska", "15-Hawaii"),]
faf <<- faf[!faf$dms_dest %in% c("11-Washington DC", "02-Alaska", "15-Hawaii"),]

cargo_types <<- read.csv("Documents/refactored_allfed/data/Cargo types (1).csv")
faf$cargo_value <- cargo_types$Cargo.value.ton[match(faf$sctg2, cargo_types$Cargo.type)]


faf$dms_orig <- getting_rid_of_number(faf$dms_orig)
faf$dms_dest <- getting_rid_of_number(faf$dms_dest)

basic_states <<- unique(faf$dms_orig)


unchanged_shipping_capacity <<- read.csv("Documents/refactored_allfed/data/Portcapacity.csv")
unchanged_shipping_capacity$SURPLUS....000.tonnes <- getting_rid_of_number(unchanged_shipping_capacity$SURPLUS....000.tonnes)
unchanged_rail_car_capacity <<- read.csv("Documents/refactored_allfed/data/Train car capacity.csv")[1:3,]



unchanged_rail_capacity <<- rep(0,length(basic_states))
for (index in 1:length(basic_states)){
  unchanged_rail_capacity[index] <- 1000*0.2*((sum(faf[faf$dms_orig == basic_states[index] | faf$dms_dest == basic_states[index],]$thousand.tons.in.2023))/2)
} 

unchanged_truck_capacity <<- read.csv("Documents/refactored_allfed/data/Truck capacity (1).csv")


transport_links <- read.csv("Documents/refactored_allfed/data/Transport links.csv")


coastal_regions <- read.csv("Documents/refactored_allfed/data/Coastal regions.csv")
coastal_regions$STATE <- getting_rid_of_number(coastal_regions$STATE)
coastal_regions$STATE[!is.na(coastal_regions$West.Coast)]

west_coast <<- coastal_regions$STATE[!is.na(coastal_regions$West.Coast)]
east_coast <<- coastal_regions$STATE[!is.na(coastal_regions$East.Coast)]
gulf_coast <<- coastal_regions$STATE[!is.na(coastal_regions$Gulf.Coast)]


truck_links <<- filter(transport_links,dms_mode == "1-Truck")
truck_links$dms_orig <- getting_rid_of_number(truck_links$dms_orig)
truck_links$dms_dest <- getting_rid_of_number(truck_links$dms_dest)
truck_links <- truck_links[truck_links$dms_orig %in% basic_states & truck_links$dms_dest %in% basic_states,]

rail_links <<- filter(transport_links,dms_mode == "2-Rail")
rail_links$dms_orig <- getting_rid_of_number(rail_links$dms_orig)
rail_links$dms_dest <- getting_rid_of_number(rail_links$dms_dest)
rail_links <- rail_links[rail_links$dms_orig %in% basic_states & rail_links$dms_dest %in% basic_states,]


shocked_things <<- c((if (scenario$Shock_ships) "3-Water" else NULL),(if (scenario$Shock_pipes) "6-Pipeline" else NULL),(if (scenario$Shock_rail) "2-Rail" else NULL))