options(scipen = 999)
scenario <<- list(
  name = "placeholder",
  JONES_ACT = FALSE,
  Shock_rail = FALSE,
  Shock_ships = TRUE,
  Shock_pipes = FALSE,
  Include_multi_mode = FALSE,
  states_shocked = c("Louisiana")
)

parent_direct <<- "~/Documents/refactored_allfed/"

source("~/Documents/refactored_allfed/code/utility.R")


source("~/Documents/refactored_allfed/code/prepare_everything.R")
source("~/Documents/refactored_allfed/code/prepare_for_shock.R")




run_model_for_state <- function(state){
  scenario$states_shocked <- c(state)
  source("~/Documents/refactored_allfed/code/model.R")
}



outputs <- data.frame(
  a = rep(0,length(basic_states)),
  b = rep(0,length(basic_states))
  
)


for_all_states <- function(){
  for (basic_state in basic_states){
    print(basic_state)
    scenario$states_shocked <- c(basic_state)
    source("~/Documents/refactored_allfed/code/model.R")
    
    source("~/Documents/refactored_allfed/code/diagnostics.R")
  }
}



