city_to_state <- function(String_of_State){
  if (substr(String_of_State,nchar(String_of_State)-1,nchar(String_of_State)) %in% states$state.teritory){
    row <- states[states$state.teritory == substr(String_of_State,nchar(String_of_State)-1,nchar(String_of_State)),]
    
    return(row$Name[1])
  }
  else{
    if (substr(String_of_State,1,1) %in% c(0:9,'-')){
      
      destinations_to_state(substr(String_of_State,2,nchar(String_of_State)))
    }
    else{
      if (substr(String_of_State,nchar(String_of_State)-7,nchar(String_of_State)-6) %in% states$state.teritory){
        row <- states[states$state.teritory == substr(String_of_State,nchar(String_of_State)-7,nchar(String_of_State)-6),]
        
        return(row$Name[1])
      }
      else{
        if (String_of_State %in% c("Buffalo NY  CFS Area","Albany NY  CFS Area")){
          return("New York")
        }
        if (String_of_State == "Minneapolis-St. Paul MN-WI (MN Part)"){
          return("Minnesota")
        }
        else{
          
          return(String_of_State)
        }
      }
    }
  }
}

