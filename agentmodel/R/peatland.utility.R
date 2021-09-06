#' Peatland Utility
#' 
#' Generates the Peatland Utility. 
#' 
#' U(land parcel) = profit  - f_{1} * opinion  * (GHG emissions_{old scenario}} - GHG emissions_{new scenario}) - f_{2} * opinion * (time taken to pay the original peatland loan back)
#' 
#' Each scenario is currently hard coded but this should be changed later. 
#'
#' @export

peatland.utility <- function(fields, pop, row, scenario) {
  ut <- NA
  a <- 100
  b <- 1
  
  if (fields$peatland.approval[row] == T) {
    if (fields$crop[row] %in% c("BAU", "BAU Higher rates", "Land Quota")) {
      if (scenario == "BAU") {ut <- fields$peatland.bau.yearly.potential.profit[row] - 
        (1/b) * pop[fields[c(row), c("farm")] , c("opinion")] * (2*ghg.mapping.data$CO2[9]*euro.per.tonne.C02 + 2*ghg.mapping.data$CH4[9]*euro.per.tonne.C02 + 2*ghg.mapping.data$CO2[9]*euro.per.tonne.C02
                                                                 - fields$emissions.tax.CO2[row] - fields$emissions.tax.CH4[row] - fields$emissions.tax.N2O[row])}
      if (scenario == "BAU Higher rates") {ut <- fields$peatland.bau.hr.yearly.potential.profit[row] -
        (1/b) * pop[fields[c(row), c("farm")] , c("opinion")] * (2*ghg.mapping.data$CO2[9]*euro.per.tonne.C02 + 2*ghg.mapping.data$CH4[9]*euro.per.tonne.C02 + 2*ghg.mapping.data$CO2[9]*euro.per.tonne.C02
                                                                 - fields$emissions.tax.CO2[row] - fields$emissions.tax.CH4[row] - fields$emissions.tax.N2O[row])}
      if (scenario == "Peatland with Barley") {ut <- fields$peatland.pwb.yearly.potential.profit[row] - 
        (1/b) * pop[fields[c(row), c("farm")] , c("opinion")] * (2*ghg.mapping.data$CO2[8]*euro.per.tonne.C02 + 2*ghg.mapping.data$CH4[8]*euro.per.tonne.C02 + 2*ghg.mapping.data$CO2[8]*euro.per.tonne.C02
                                                                 - fields$emissions.tax.CO2[row] - fields$emissions.tax.CH4[row] - fields$emissions.tax.N2O[row])}
      if (scenario == "Peatland with Silage") {ut <- fields$peatland.pws.yearly.potential.profit[row] - 
        (1/b) * pop[fields[c(row), c("farm")] , c("opinion")] * (2*ghg.mapping.data$CO2[8]*euro.per.tonne.C02 + 2*ghg.mapping.data$CH4[8]*euro.per.tonne.C02 + 2*ghg.mapping.data$CO2[8]*euro.per.tonne.C02
                                                                 - fields$emissions.tax.CO2[row] - fields$emissions.tax.CH4[row] - fields$emissions.tax.N2O[row])}
      if (scenario == "Peatland with RCG") {ut <- fields$peatland.pwr.yearly.potential.profit[row] - 
        (1/b) * pop[fields[c(row), c("farm")] , c("opinion")] * (2*ghg.mapping.data$CO2[8]*euro.per.tonne.C02 + 2*ghg.mapping.data$CH4[8]*euro.per.tonne.C02 + 2*ghg.mapping.data$CO2[8]*euro.per.tonne.C02
                                                                 - fields$emissions.tax.CO2[row] - fields$emissions.tax.CH4[row] - fields$emissions.tax.N2O[row])}
      if (scenario == "Land Quota") {ut <- fields$peatland.lq.yearly.potential.profit[row] - 
        (1/b) * pop[fields[c(row), c("farm")] , c("opinion")] * (2*ghg.mapping.data$CO2[9]*euro.per.tonne.C02 + 2*ghg.mapping.data$CH4[9]*euro.per.tonne.C02 + 2*ghg.mapping.data$CO2[9]*euro.per.tonne.C02
                                                                 - fields$emissions.tax.CO2[row] - fields$emissions.tax.CH4[row] - fields$emissions.tax.N2O[row])}
    } else {
      if (scenario == "BAU") {ut <- fields$peatland.bau.yearly.potential.profit[row] - 
        (1/b) * pop[fields[c(row), c("farm")] , c("opinion")] * (2*ghg.mapping.data$CO2[9]*euro.per.tonne.C02 + 2*ghg.mapping.data$CH4[9]*euro.per.tonne.C02 + 2*ghg.mapping.data$CO2[9]*euro.per.tonne.C02
                                                                 - fields$emissions.tax.CO2[row] - fields$emissions.tax.CH4[row] - fields$emissions.tax.N2O[row])  - 
        a * pop[fields[c(row), c("farm")] , c("opinion")] * fields$peatland.bau.time.to.pay.back[row]}
      if (scenario == "BAU Higher rates") {ut <- fields$peatland.bau.hr.yearly.potential.profit[row] -
        (1/b) * pop[fields[c(row), c("farm")] , c("opinion")] * (2*ghg.mapping.data$CO2[9]*euro.per.tonne.C02 + 2*ghg.mapping.data$CH4[9]*euro.per.tonne.C02 + 2*ghg.mapping.data$CO2[9]*euro.per.tonne.C02
                                                                 - fields$emissions.tax.CO2[row] - fields$emissions.tax.CH4[row] - fields$emissions.tax.N2O[row])  -  
        a * pop[fields[c(row), c("farm")] , c("opinion")] * fields$peatland.bau.hr.time.to.pay.back[row]}
      if (scenario == "Peatland with Barley") {ut <- fields$peatland.pwb.yearly.potential.profit[row] - 
        (1/b) * pop[fields[c(row), c("farm")] , c("opinion")] * (2*ghg.mapping.data$CO2[8]*euro.per.tonne.C02 + 2*ghg.mapping.data$CH4[8]*euro.per.tonne.C02 + 2*ghg.mapping.data$CO2[8]*euro.per.tonne.C02
                                                                 - fields$emissions.tax.CO2[row] - fields$emissions.tax.CH4[row] - fields$emissions.tax.N2O[row])  - 
        a * pop[fields[c(row), c("farm")] , c("opinion")] * fields$peatland.pwb.time.to.pay.back[row]}
      if (scenario == "Peatland with Silage") {ut <- fields$peatland.pws.yearly.potential.profit[row] - 
        (1/b) * pop[fields[c(row), c("farm")] , c("opinion")] * (2*ghg.mapping.data$CO2[8]*euro.per.tonne.C02 + 2*ghg.mapping.data$CH4[8]*euro.per.tonne.C02 + 2*ghg.mapping.data$CO2[8]*euro.per.tonne.C02
                                                                 - fields$emissions.tax.CO2[row] - fields$emissions.tax.CH4[row] - fields$emissions.tax.N2O[row])  -  
        a * pop[fields[c(row), c("farm")] , c("opinion")] * fields$peatland.pws.time.to.pay.back[row]}
      if (scenario == "Peatland with RCG") {ut <- fields$peatland.pwr.yearly.potential.profit[row] - 
        (1/b) * pop[fields[c(row), c("farm")] , c("opinion")] * (2*ghg.mapping.data$CO2[8]*euro.per.tonne.C02 + 2*ghg.mapping.data$CH4[8]*euro.per.tonne.C02 + 2*ghg.mapping.data$CO2[8]*euro.per.tonne.C02
                                                                 - fields$emissions.tax.CO2[row] - fields$emissions.tax.CH4[row] - fields$emissions.tax.N2O[row])  - 
        a * pop[fields[c(row), c("farm")] , c("opinion")] * fields$peatland.pwr.time.to.pay.back[row]}
      if (scenario == "Land Quota") {ut <- fields$peatland.lq.yearly.potential.profit[row] - 
        (1/b) * pop[fields[c(row), c("farm")] , c("opinion")] * (2*ghg.mapping.data$CO2[9]*euro.per.tonne.C02 + 2*ghg.mapping.data$CH4[9]*euro.per.tonne.C02 + 2*ghg.mapping.data$CO2[9]*euro.per.tonne.C02
                                                                 - fields$emissions.tax.CO2[row] - fields$emissions.tax.CH4[row] - fields$emissions.tax.N2O[row])  - 
        a * pop[fields[c(row), c("farm")] , c("opinion")] * fields$peatland.lq.time.to.pay.back[row]}
    }
  } else {
    ut <- 0
  }
  return(ut)
}


# profit - a * (current - peatland) - b * (payback)
