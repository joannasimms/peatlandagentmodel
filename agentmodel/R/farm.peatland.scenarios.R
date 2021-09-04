#' Farm Peatland Scenarios
#' 
#' Reads the economic properties of each peatland scenarios.
#' 
#' Relies on Peatland Scenarios Data function. Could be replaced.
#' @export

farm.peatland.scenarios <- function(fields, peatland.scenarios, y, considered.subsidies) {
  fields$peatland.bau.yearly.potential.profit <- NA
  fields$peatland.bau.time.to.pay.back <- NA
  fields$peatland.bau.hr.yearly.potential.profit <- NA
  fields$peatland.bau.hr.time.to.pay.back <- NA
  fields$peatland.pwb.yearly.potential.profit <- NA
  fields$peatland.pwb.time.to.pay.back <- NA
  fields$peatland.pws.yearly.potential.profit <- NA
  fields$peatland.pws.time.to.pay.back <- NA
  fields$peatland.pwr.yearly.potential.profit <- NA
  fields$peatland.pwr.time.to.pay.back <- NA
  #fields$peatland.pww.yearly.potential.profit <- NA
  #fields$peatland.pww.time.to.pay.back <- NA
  fields$peatland.lq.yearly.potential.profit <- NA
  fields$peatland.lq.time.to.pay.back <- NA
  
  for (q in 1:nrow(fields)) {
    if (fields$peatland.approval[q]) {
      if ("BAU" %in% names(considered.subsidies)) {
        fields$peatland.bau.yearly.potential.profit[q] <- after.tax(greening(fields, q))
        fields$peatland.bau.time.to.pay.back[q] <- peatland.scenarios[c("BAU"),c("initial.costs")]%/%200
        
      }
      if ("BAU Higher rates" %in% names(considered.subsidies)) {
        fields$peatland.bau.hr.yearly.potential.profit[q] <- after.tax(greening.alt(fields, q))
        fields$peatland.bau.hr.time.to.pay.back[q] <- peatland.scenarios[c("BAU Higher rates"),c("initial.costs")]%/%200
        
      }
      if ("Peatland with Barley" %in% names(considered.subsidies)) {
        fields$peatland.pwb.yearly.potential.profit[q] <- after.tax(0.7*yield[y,c("Barley.Organic")]*(expenses.income[c("Barley"), c("Price")]*12) - expenses.income[c("Barley"), c("Expenses")]*12 - 200  + fields$subsidy[q])
        fields$peatland.pwb.time.to.pay.back[q] <- peatland.scenarios[c("Peatland with Barley"),c("initial.costs")]%/%200
        
      }
      if ("Peatland with Silage" %in% names(considered.subsidies)) {
        fields$peatland.pws.yearly.potential.profit[q] <- after.tax(0.7*yield[y,c("Silage.Organic")]*(expenses.income[c("Silage"), c("Price")]*12) - expenses.income[c("Silage"), c("Expenses")]*12 - 200 + fields$subsidy[q])
        fields$peatland.pws.time.to.pay.back[q] <- peatland.scenarios[c("Peatland with Silage"),c("initial.costs")]%/%200
        
      }
      if ("Peatland with RCG" %in% names(considered.subsidies)) {
        fields$peatland.pwr.yearly.potential.profit[q] <- after.tax(0.7*yield[y,c("RCG.Organic")]*(expenses.income[c("RCG"), c("Price")]*12) - expenses.income[c("RCG"), c("Expenses")]*12 - 200  + fields$subsidy[q])
        fields$peatland.pwr.time.to.pay.back[q] <- peatland.scenarios[c("Peatland with RCG"),c("initial.costs")]%/%200
        
      }
      #if ("Peatland with Willow" %in% names(considered.subsidies)) {
      #  fields$peatland.pww.yearly.potential.profit[q] <- after.tax(0.7*yield[y,c("Willow.Organic")]*(expenses.income[c("Willow"), c("Price")]*12) - expenses.income[c("Willow"), c("Expenses")]*129  + fields$subsidy[q])
      #  fields$peatland.pww.time.to.pay.back[q] <- pay.back(peatland.scenarios, fields, q, "Peatland with Willow")
      #  
      #}
      if ("Land Quota" %in% names(considered.subsidies)) {
        fields$peatland.lq.yearly.potential.profit[q] <- after.tax(greening.lq(fields, q))
        fields$peatland.lq.time.to.pay.back[q] <-  peatland.scenarios[c("Land Quota"),c("initial.costs")]%/%200
        
      }
    }
  }
  print(">>>>")
  return(fields)
}


#' Payment function
#' 
#' Calculates the payment amount of the loan based on year
#' 
#' @export

payment <- function(year, scen) {
  if (scen %in% c("BAU", "BAU Higher rates", "Land Quota")) {
    if (year < peatland.scenarios[c("BAU"),c("initial.costs")]%/%200) {pay <- 200} else {pay <- 0}
  } else {
    if (year < peatland.scenarios[c("Peatland with RCG"),c("initial.costs")]%/%200) {pay <- 200} else {pay <- 0}
  }
  return(pay)
}




