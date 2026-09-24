specific_pipeline_faf <- faf
specific_pipeline_faf <- specific_pipeline_faf[specific_pipeline_faf$dms_mode == "6-Pipeline",]

#pipeline_shocked <- "Enbridge Pipeline System"
#pipeline_shocked <- "Enbridge Mainline"
#pipeline_shocked <- "Keystone"
pipeline_shocked <- "Dakota Access (DAPL)/Bakken system"
#pipeline_shocked <- "Colonial/Plantation"
#pipeline_shocked <- "Transco (Transcontinental)"


enbridge_system_states <- c("17-Illinois","22-Louisiana","26-Michigan","27-Minnesota","40-Oklahoma","48-Texas","55-Wisconsin","56-Wyoming")
enbridge_main_states <- c("17-Illinois","26-Michigan","27-Minnesota","55-Wisconsin")
keystone_states <- c("17-Illinois","31-Nebraska","40-Oklahoma","48-Texas")
dapl_states <- c("17-Illinois","19-Iowa","38-North Dakota","46-South Dakota","48-Texas")
col_states <- c("51-Virginia","48-Texas","47-Tennessee","45-South Carolina","42-Pennsylvania","37-North Carolina","34-New Jersey","28-Mississippi","24-Maryland","22-Louisiana","13-Georgia","10-Delaware","01-Alabama")
transco_states <- c("51-Virginia","48-Texas","45-South Carolina","42-Pennsylvania","37-North Carolina","36-New York","34-New Jersey","28-Mississippi","24-Maryland","22-Louisiana","13-Georgia","01-Alabama")

active_system_states <- dapl_states

if (pipeline_shocked %in% c("Enbridge Pipeline System","Enbridge Mainline", "Keystone","Dakota Access (DAPL)/Bakken system")){
  specific_pipeline_faf <<- specific_pipeline_faf[specific_pipeline_faf$sctg2 == "16-Crude petroleum",]
}

if (pipeline_shocked %in% c("Colonial/Plantation")){
  specific_pipeline_faf <<- specific_pipeline_faf[specific_pipeline_faf$sctg2 == "18-Fuel oils",]
}


if (pipeline_shocked %in% c("Transco (Transcontinental)")){
  specific_pipeline_faf<<- specific_pipeline_faf[specific_pipeline_faf$sctg2 == "19-Natural gas and other fossil products",]
}

#


processing_faf <<- filter(specific_pipeline_faf,(dms_orig %in% active_system_states|dms_dest %in% active_system_states))
write.csv(processing_faf,paste(parent_direct,"data/cleaned_faf.csv",sep=""))