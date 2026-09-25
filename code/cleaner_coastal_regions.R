
regions <- list(coastal_regions$West.Coast,coastal_regions$East.Coast,coastal_regions$Gulf.Coast)
region_index <- 3
list_of_region_states <- na.omit(coastal_regions$STATE[unlist(regions[region_index])==1&!is.na(unlist(regions[region_index])==1)])
processing_faf <<- filter(faf,(dms_orig %in% list_of_region_states|dms_dest %in% list_of_region_states))


shocked_things <- c((if (Shock_ships) "3-Water" else NULL),(if (Shock_pipes) "6-Pipeline" else NULL),(if (Shock_rail) "2-Rail" else NULL))
if (Include_multi_mode){
  shocked_things <- c(shocked_things,"5-Multiple modes & mail")
}

processing_faf <<- filter(processing_faf,dms_mode %in% shocked_things)
write.csv(processing_faf,paste(parent_direct,"data/cleaned_faf.csv",sep=""))