#' Farm emissions money
#' 
#' Adds the emissions and their associated taxes
#' 
#' Hard programmed to have 2 ha packages.
#' @export

farm.emissions.money <- function(fields, ghg.mapping.data) {
  for (ww in 1:nrow(fields)) {
    ####
    # Emission category
    ####
    if (fields$crop[ww] %in% c("Barley", "Barley.Organic") & fields$type[ww] != "Peatland.with.produce") {
      fields$land.type.ghg[ww] <- "cropland"
    } else if (fields$crop[ww] %in% c("Barley", "Barley.Organic") & fields$type[ww] == "Peatland.with.produce") {
      fields$land.type.ghg[ww] <- "Peatland with Produce"
    } else if (fields$crop[ww] %in% c("Silage", "Silage.Organic") & fields$type[ww] != "Peatland.with.produce") {
      fields$land.type.ghg[ww] <- "grassland"
    } else if (fields$crop[ww] %in% c("Silage", "Silage.Organic") & fields$type[ww] == "Peatland.with.produce") {
      fields$land.type.ghg[ww] <- "Peatland with Produce"
    } else if (fields$crop[ww] %in% c("Peatland.with.produce", "RCG")) {
      fields$land.type.ghg[ww] <- "Peatland with Produce"
    } else if (fields$crop[ww] %in% c("Rewetting", "100% Conservation", "BAU", "BAU Higher rates", "Land Quota")) {
      fields$land.type.ghg[ww] <- "peatland"
    } else if (fields$crop[ww] %in% c("GH")) {
      fields$land.type.ghg[ww] <- "GH"
    } else {
      fields$land.type.ghg[ww] <- "other"
    }
  }
  
    ####
    # Emissions and penalty
    ####
  
  
    # check the units, the units in the ghg gass table should be in co2 eq so only need to multiply by the CO2 constant
  for (j in 1:nrow(fields)) {
    ##################### Cropland
    if (fields$land.type.ghg[j] == "cropland" && (fields$mineral[j] == T)) {
      # 2ha per package of land
      # CO2
      fields$emissions.land.CO2[j] <- 2*ghg.mapping.data$CO2[1]
      fields$emissions.tax.CO2[j] <- fields$emissions.land.CO2[j]*euro.per.tonne.C02
      # CH4
      fields$emissions.land.CH4[j] <- 2*ghg.mapping.data$CH4[1]
      fields$emissions.tax.CH4[j] <- fields$emissions.land.CH4[j]*euro.per.tonne.C02
      # N2O
      fields$emissions.land.N2O[j] <- 2*ghg.mapping.data$N2O[1]
      fields$emissions.tax.N2O[j] <- fields$emissions.land.N2O[j]*euro.per.tonne.C02
    } else if (fields$land.type.ghg[j] == "cropland" &&  (fields$converted.peat[j] == T)) {
      # 2ha per package of land
      # CO2
      fields$emissions.land.CO2[j] <- 2*ghg.mapping.data$CO2[2]
      fields$emissions.tax.CO2[j] <- fields$emissions.land.CO2[j]*euro.per.tonne.C02
      # CH4
      fields$emissions.land.CH4[j] <- 2*ghg.mapping.data$CH4[2]
      fields$emissions.tax.CH4[j] <- fields$emissions.land.CH4[j]*euro.per.tonne.C02
      # N2O
      fields$emissions.land.N2O[j] <- 2*ghg.mapping.data$N2O[2]
      fields$emissions.tax.N2O[j] <- fields$emissions.land.N2O[j]*euro.per.tonne.C02
    
    ####################### Grassland
    } else if (fields$land.type.ghg[j] == "grassland" && (fields$mineral[j] == T)) {
      # CO2
      fields$emissions.land.CO2[j] <- 2*ghg.mapping.data$CO2[3]
      fields$emissions.tax.CO2[j] <- fields$emissions.land.CO2[j]*euro.per.tonne.C02
      # CH4
      fields$emissions.land.CH4[j] <- 2*ghg.mapping.data$CH4[3]
      fields$emissions.tax.CH4[j] <- fields$emissions.land.CH4[j]*euro.per.tonne.C02
      # N2O
      fields$emissions.land.N2O[j] <- 2*ghg.mapping.data$N2O[3]
      fields$emissions.tax.N2O[j] <- fields$emissions.land.N2O[j]*euro.per.tonne.C02
    } else if (fields$land.type.ghg[j] == "grassland" && (fields$converted.peat[j] == T)) {
      # CO2
      fields$emissions.land.CO2[j] <- 2*ghg.mapping.data$CO2[4]
      fields$emissions.tax.CO2[j] <- fields$emissions.land.CO2[j]*euro.per.tonne.C02
      # CH4
      fields$emissions.land.CH4[j] <- 2*ghg.mapping.data$CH4[4]
      fields$emissions.tax.CH4[j] <- fields$emissions.land.CH4[j]*euro.per.tonne.C02
      # N2O
      fields$emissions.land.N2O[j] <- 2*ghg.mapping.data$N2O[4]
      fields$emissions.tax.N2O[j] <- fields$emissions.land.N2O[j]*euro.per.tonne.C02
    
    ######################### Peatland with Produce and Peatland scenarios
    } else if (fields$land.type.ghg[j] == "Peatland with Produce") {
      # CO2
      fields$emissions.land.CO2[j] <- 2*ghg.mapping.data$CO2[8]
      fields$emissions.tax.CO2[j] <- fields$emissions.land.CO2[j]*euro.per.tonne.C02
      # CH4
      fields$emissions.land.CH4[j] <- 2*ghg.mapping.data$CH4[8]
      fields$emissions.tax.CH4[j] <- fields$emissions.land.CH4[j]*euro.per.tonne.C02
      # N2O
      fields$emissions.land.N2O[j] <- 2*ghg.mapping.data$N2O[8]
      fields$emissions.tax.N2O[j] <- fields$emissions.land.N2O[j]*euro.per.tonne.C02
    } else if (fields$land.type.ghg[j] %in% c("peatland") && fields$peat[j] == T) { # peatland values check
      # CO2
      fields$emissions.land.CO2[j] <- 2*ghg.mapping.data$CO2[9]
      fields$emissions.tax.CO2[j] <- fields$emissions.land.CO2[j]*euro.per.tonne.C02
      # CH4
      fields$emissions.land.CH4[j] <- 2*ghg.mapping.data$CH4[9]
      fields$emissions.tax.CH4[j] <- fields$emissions.land.CH4[j]*euro.per.tonne.C02
      # N2O
      fields$emissions.land.N2O[j] <- 2*ghg.mapping.data$N2O[9]
      fields$emissions.tax.N2O[j] <- fields$emissions.land.N2O[j]*euro.per.tonne.C02
    } else if (fields$land.type.ghg[j] %in% c("Rewetted land") && fields$rewetted.peat[j] == T) { # rewetted for conservation
      # CO2
      fields$emissions.land.CO2[j] <- 2*ghg.mapping.data$CO2[7]
      fields$emissions.tax.CO2[j] <- fields$emissions.land.CO2[j]*euro.per.tonne.C02
      # CH4
      fields$emissions.land.CH4[j] <- 2*ghg.mapping.data$CH4[7]
      fields$emissions.tax.CH4[j] <- fields$emissions.land.CH4[j]*euro.per.tonne.C02
      # N2O
      fields$emissions.land.N2O[j] <- 2*ghg.mapping.data$N2O[7]
      fields$emissions.tax.N2O[j] <- fields$emissions.land.N2O[j]*euro.per.tonne.C02
      
    ########################## Greenhouse and exceptions
    } else if (fields$land.type.ghg[j] %in% c("other") && fields$peat[j] == F) {
      # CO2
      fields$emissions.land.CO2[j] <- 0
      fields$emissions.tax.CO2[j] <- 0
      # CH4
      fields$emissions.land.CH4[j] <- 0
      fields$emissions.tax.CH4[j] <- 0
      # N2O
      fields$emissions.land.N2O[j] <- 0
      fields$emissions.tax.N2O[j] <- 0
    } else if (fields$land.type.ghg[j] %in% c("other") && fields$peat[j] == T) {
      # CO2
      fields$emissions.land.CO2[j] <- 0
      fields$emissions.tax.CO2[j] <- 0
      # CH4
      fields$emissions.land.CH4[j] <- 0
      fields$emissions.tax.CH4[j] <- 0
      # N2O
      fields$emissions.land.N2O[j] <- 0
      fields$emissions.tax.N2O[j] <- 0
    } else if (fields$land.type.ghg[j] %in% c("GH")) {
      # CO2
      fields$emissions.land.CO2[j] <- 0
      fields$emissions.tax.CO2[j] <- 0
      # CH4
      fields$emissions.land.CH4[j] <- 0
      fields$emissions.tax.CH4[j] <- 0
      # N2O
      fields$emissions.land.N2O[j] <- 0
      fields$emissions.tax.N2O[j] <- 0
    } else {
      stop("unclasified land or peatland that shouldn't have been converted")
    }
  }
  print(">")
  return(fields)
}




