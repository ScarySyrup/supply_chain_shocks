to_check_unsatisfied <- "19-Natural gas and other fossil products"


#"02-Cereal grains" 
#"16-Crude petroleum"
#


1-(sum((filter(processing_faf,sctg2==to_check_unsatisfied)$shocked_mode == "Unsatisfied demand")*(filter(processing_faf,sctg2==to_check_unsatisfied)$thousand.tons.in.2023))/sum((filter(processing_faf,sctg2==to_check_unsatisfied)$thousand.tons.in.2023)))

outputs$a[match(scenario$states_shocked),basic_states] <- ((sum((filter(processing_faf,sctg2==to_check_unsatisfied)$shocked_mode == "Unsatisfied demand")*(filter(processing_faf,sctg2==to_check_unsatisfied)$thousand.tons.in.2023)))/sum(processing_faf$thousand.tons.in.2023))


exports_data_frame <- function(){
  export_faf <- processing_faf
  export_faf <- filter(export_faf,shocked_mode == "Unsatisfied demand")
  exports_faf <- filter(export_faf,Export.import. == "Export")
  return(aggregate(thousand.tons.in.2023 ~ Partner.country,
            data = exports_faf[exports_faf$Export.import. == "Export", ],
            FUN = sum))
}