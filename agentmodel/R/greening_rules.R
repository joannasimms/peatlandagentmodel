#' Greening
#' 
#' Gives the current govenment subsidies for peatland conservation
#' @export

greening <- function(fields, row) {
  if (sum(fields[fields$farm == row, c("farm", "peat")][,c(2)])<2) {
    return(1862) #eruo per ha
  } else if (sum(fields[fields$farm == row, c("farm", "peat")][,c(2)])>=2 && (sum(fields[fields$farm == row, c("farm", "peat")][,c(2)])<=5)) {
    return(1108) #eruo per ha
  } else if (sum(fields[fields$farm == row, c("farm", "peat")][,c(2)])>=6) {
    return(754) #euro per ha
  } else {
    stop("There is an NA value in fields$peatland, or the bounds are not closed.")
  }
  if (fields$greening.sub[row] >= 11669) {
    return(11669)
  }
  return(fields)
}

#' Greening Alt
#' 
#' Gives a higher insentive, but uses the same structure as the curent
#' peatland insentives, however each payout has been increased by 100 euro
#' per field
#' 
#' Output is per ha
#' @export

greening.alt <- function(fields, row) {
  if (sum(fields[fields$farm == row, c("farm", "peat")][,c(2)])<2) {
    return(1962) #eruo per ha
  } else if (sum(fields[fields$farm == row, c("farm", "peat")][,c(2)])>=2 && (sum(fields[fields$farm == row, c("farm", "peat")][,c(2)])<=5)) {
    return(1208) #eruo per ha
  } else if (sum(fields[fields$farm == row, c("farm", "peat")][,c(2)])>=6) {
    return(854) #euro per ha
  } else {
    stop("There is an NA value in fields$peatland, or the bounds are not closed.")
  }
  if (fields$greening.sub[row] >= 11669) {
    return(25669)
  }
  return(fields)
}

#' Greening with a Land Quota
#' 
#' Although this is very similar to the BAU above, this allows smaller farms 
#' to be able to afford the transition into peatland. As they have less spare land
#' to convert.
#' 
#' DON'T THINK I NEED THIS DO IT ON A NATIONAL LEVEL

#greening.lq <- function(fields, row) {
#  if (pop[fields$farm[row],c("fields_owned")] <= 11) {
#    if (sum(fields[fields$farm == row, c("farm", "rewetted.peat")][,c(2)]) >= 0.2*pop[fields$farm[row],c("fields_owned")]) {
#      # 20% as this is approximately the amount of land that would naturally be wetland
#      # 2000 as slightly higher than the original ones
#      return(2000) #eruo per ha
#    } else {
#      stop("Amount of fields made of peatland is unacounted for")
#    }
#  } else {
#    return(0) # As farm to big to qualify
#  }
#  return(fields)
#}


#' Greening with Nature Based Solutions
#' 
#' Allows for the inclusion of nature based solutions.
#' 
#' Biodiversity values taken from the national scheme now
#' Water values taken from the ground water value, average amount of peat in a field and water retention
#' 2 * 10 000 * 1.41 * 0.9 - 0.6 * 1.34 / 4  = 2 Ha of peat volume with 90 - 60% saturation (difference between mineral and peat) multiplied by the groundwater rate divided by 4 as not all of it will be used
#' Carbon taken from ETC
#' 
#' If using more detailed data could vary the water payment
#' 
#' @export

greening.lq <- function(fields, row) {
  if (sum(fields[fields$farm == row, c("farm", "peat")][,c(2)]) < 0.1*pop[fields[row, c("farm")],c("fields_owned")]) {
    return(0.001)
  } else if ((sum(fields[fields$farm == row, c("farm", "peat")][,c(2)])>= 0.1*pop[fields[row, c("farm")],c("fields_owned")]) && (sum(fields[fields$farm == row, c("farm", "peat")][,c(2)])<= 0.2*pop[fields[row, c("farm")],c("fields_owned")])) {
    return(1108 + 2834 + fields$emissions.tax.CO2[row] + fields$emissions.tax.CH4[row] + fields$emissions.tax.N2O[row]) #eruo per ha
  } else if (sum(fields[fields$farm == row, c("farm", "peat")][,c(2)]) >= 0.2*pop[fields[row, c("farm")],c("fields_owned")]) {
    return(754 + 2834 + fields$emissions.tax.CO2[row] + fields$emissions.tax.CH4[row] + fields$emissions.tax.N2O[row]) #euro per ha
  } else {
    stop("There is an NA value in fields$peatland, or the bounds are not closed.")
  }
  if (fields$greening.sub[row] >= 11669) {
    return(11669)
  }
  return(fields)
}



