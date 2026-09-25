sum((!processing_faf$shocked_mode=="Unsatisfied demand")*processing_faf$thousand.tons.in.2023)/sum(processing_faf$thousand.tons.in.2023)
#percent of weight demand satisfied


cereal_grains_processing_faf <- processing_faf[processing_faf$sctg2 == "02-Cereal grains",]
sum((!cereal_grains_processing_faf$shocked_mode=="Unsatisfied demand")*cereal_grains_processing_faf$thousand.tons.in.2023)/sum(cereal_grains_processing_faf$thousand.tons.in.2023)


crude_petr_processing_faf <- processing_faf[processing_faf$sctg2 == "16-Crude petroleum",]
sum((!crude_petr_processing_faf$shocked_mode=="Unsatisfied demand")*crude_petr_processing_faf$thousand.tons.in.2023)/sum(crude_petr_processing_faf$thousand.tons.in.2023)



nat_gas_processing_faf <- processing_faf[processing_faf$sctg2 == "19-Natural gas and other fossil products",]
sum((!nat_gas_processing_faf$shocked_mode=="Unsatisfied demand")*nat_gas_processing_faf$thousand.tons.in.2023)/sum(nat_gas_processing_faf$thousand.tons.in.2023)