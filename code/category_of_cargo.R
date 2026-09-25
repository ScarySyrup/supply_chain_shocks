type_of_cargo <- function(train_or_truck,cargo){
  if (train_or_truck %in% c("train","truck")){
    if (cargo %in% c("16-Crude petroleum","17-Gasoline","18-Fuel oils","19-Natural gas and other fossil products","20-Basic chemicals","23-Chemical prods.")){
        if (train_or_truck == "truck"){
          return("Liquid tankers")
        }
        else if (train_or_truck == "train"){
          return("Tank")
        }
    }
    else if (cargo %in% c("23-Chemical prods.","05-Meat/seafood")){
      if (train_or_truck == "train"){
        return("Box")
      }
      if (train_or_truck == "truck"){
        if (cargo %in% c("05-Meat/seafood")){
          return("Refrigerated")
        }
        if (cargo %in% c("01-Live animals/fish")){
          return("Other")
        }
      }
    }
    else{
      if (train_or_truck == "truck"){
        return("Other")
      }
      if (train_or_truck == "train"){
        return("Box")
      }
    }
  }
  else if (train_or_truck == "ship"){
    if (cargo == "16-Crude petroleum"){
      return(2) #column 2
    }
    if (cargo == "17-Gasoline"){
      return(3)
    }
    if (cargo %in% c("18-Fuel oils","19-Natural gas and other fossil products" )){
      return(4)
    }
    if (cargo == "02-Cereal grains"){
      return(5)
    }else{
      return(1)
    }
  }
  
}