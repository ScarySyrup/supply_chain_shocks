library(dplyr)

basic_states_data_frame <- read.csv(paste(parent_direct,"data/basic_states.csv",sep = ""))
basic_states <- basic_states_data_frame$x




shocked_things <- c((if (Shock_ships) "3-Water" else NULL),(if (Shock_pipes) "6-Pipeline" else NULL),(if (Shock_rail) "2-Rail" else NULL))
if (Include_multi_mode){
  shocked_things <- c(shocked_things,"5-Multiple modes & mail")
}
cleaned_faf <- filter(faf,dms_mode %in% shocked_things)




cleaned_faf <- relocate(cleaned_faf,origin_state,destination_state)
cleaned_faf <- cleaned_faf[cleaned_faf$origin_state %in% basic_states&cleaned_faf$destination_state %in% basic_states,]


cleaned_faf <- cleaned_faf[cleaned_faf$destination_state %in% c(STATE)|cleaned_faf$origin_state %in% c(STATE),]



write.csv(cleaned_faf,paste(parent_direct,"/data/cleaned_faf.csv",sep=""))