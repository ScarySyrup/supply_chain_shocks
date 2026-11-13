library(dplyr)
cleaned_faf <- faf



"dms_orig" %in% names(cleaned_faf)
grep("orig", names(cleaned_faf), value = TRUE)

water_stuff <- cleaned_faf[0, ]
if ("3-Water" %in% shocked_things){
  water_stuff <<- rbind(
    filter(cleaned_faf,Import.export.method.=="3-Water",dms_orig %in% scenario$states_shocked & Export.import. == "Import",dms_mode != "3-Water"),
    filter(cleaned_faf,Import.export.method.=="3-Water",dms_dest %in% scenario$states_shocked & Export.import. == "Export",dms_mode != "3-Water")
  )
}


rail_stuff <- cleaned_faf[0, ]
if ("2-Rail" %in% shocked_things){
  rail_stuff <<- rbind(
    filter(cleaned_faf,Import.export.method.=="2-Rail",dms_orig %in% scenario$states_shocked & Export.import. == "Import",dms_mode != "2-Rail"),
    filter(cleaned_faf,Import.export.method.=="2-Rail",dms_dest %in% scenario$states_shocked & Export.import. == "Export",dms_mode != "2-Rail")
  )
}



cleaned_faf <- filter(cleaned_faf,dms_mode %in% shocked_things)
cleaned_faf <- cleaned_faf[cleaned_faf$dms_dest %in% scenario$states_shocked|cleaned_faf$dms_orig %in% scenario$states_shocked,]


cleaned_faf <<- rbind(cleaned_faf,water_stuff,rail_stuff)

cleaned_faf <<- cleaned_faf[order(-cleaned_faf$cargo_value),]

write.csv(cleaned_faf,paste(parent_direct,"/data/cleaned_faf.csv",sep=""))