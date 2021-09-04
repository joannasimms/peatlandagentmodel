#' Pay Back Function
#' 
#' Checks if the peatland subsidy investment will make a profit within 50 years
#' 
#' @export



pay.back <- function(peatland.scenarios, fields, qq, sub) {
  if (sub == "BAU") {
    if (((peatland.scenarios[c("BAU"), c("initial.costs")]/(50*fields$peatland.bau.yearly.potential.profit[qq]-50*peatland.scenarios[c("BAU"), c("maintanence.costs")])) > 50) || ((peatland.scenarios[c("BAU"), c("initial.costs")]/(50*fields$peatland.bau.yearly.potential.profit[qq]-50*peatland.scenarios[c("BAU"), c("maintanence.costs")])) < 0)) {
      return(9999)
    } else {
      return(peatland.scenarios[c("BAU"), c("initial.costs")]/(50*fields$peatland.bau.yearly.potential.profit[qq]-50*peatland.scenarios[c("BAU"), c("maintanence.costs")]))
    }
  } else if (sub == "BAU Higher rates") {
    if ((peatland.scenarios[c("BAU Higher rates"), c("initial.costs")]/(50*fields$peatland.bau.hr.yearly.potential.profit[qq]-50*peatland.scenarios[c("BAU Higher rates"), c("maintanence.costs")]) > 50) || (peatland.scenarios[c("BAU Higher rates"), c("initial.costs")]/(50*fields$peatland.bau.hr.yearly.potential.profit[qq]-50*peatland.scenarios[c("BAU Higher rates"), c("maintanence.costs")]) < 0)) {
      return(9999)
    } else {
      return(peatland.scenarios[c("BAU Higher rates"), c("initial.costs")]/(50*fields$peatland.bau.hr.yearly.potential.profit[qq]-50*peatland.scenarios[c("BAU Higher rates"), c("maintanence.costs")]))
    }
  } else if (sub == "Peatland with Barley") {
    if ((peatland.scenarios[c("Peatland with Barley"), c("initial.costs")]/(50*fields$peatland.pwb.yearly.potential.profit[qq]-50*peatland.scenarios[c("Peatland with Barley"), c("maintanence.costs")]) > 50) || (peatland.scenarios[c("Peatland with Barley"), c("initial.costs")]/(50*fields$peatland.pwb.yearly.potential.profit[qq]-50*peatland.scenarios[c("Peatland with Barley"), c("maintanence.costs")]) < 0)) {
      return(9999)
    } else {
      return(peatland.scenarios[c("Peatland with Barley"), c("initial.costs")]/(50*fields$peatland.pwb.yearly.potential.profit[qq]-50*peatland.scenarios[c("Peatland with Barley"), c("maintanence.costs")]))
    }
  } else if (sub == "Peatland with Silage") {
    if ((peatland.scenarios[c("Peatland with Silage"), c("initial.costs")]/(50*fields$peatland.pws.yearly.potential.profit[qq]-50*peatland.scenarios[c("Peatland with Silage"), c("maintanence.costs")]) > 50) || (peatland.scenarios[c("Peatland with Silage"), c("initial.costs")]/(50*fields$peatland.pws.yearly.potential.profit[qq]-50*peatland.scenarios[c("Peatland with Silage"), c("maintanence.costs")]) < 0)) {
      return(9999)
    } else {
      return(peatland.scenarios[c("Peatland with Silage"), c("initial.costs")]/(50*fields$peatland.pws.yearly.potential.profit[qq]-50*peatland.scenarios[c("Peatland with Silage"), c("maintanence.costs")]))
    }
  } else if (sub == "Peatland with RCG") {
    if ((peatland.scenarios[c("Peatland with RCG"), c("initial.costs")]/(50*fields$peatland.pwr.yearly.potential.profit[qq]-50*peatland.scenarios[c("Peatland with RCG"), c("maintanence.costs")]) > 50) || (peatland.scenarios[c("Peatland with RCG"), c("initial.costs")]/(50*fields$peatland.pwr.yearly.potential.profit[qq]-50*peatland.scenarios[c("Peatland with RCG"), c("maintanence.costs")]) < 0)) {
      return(9999)
    } else {
      return(peatland.scenarios[c("Peatland with RCG"), c("initial.costs")]/(50*fields$peatland.pwr.yearly.potential.profit[qq]-50*peatland.scenarios[c("Peatland with RCG"), c("maintanence.costs")]))
    }
  } else if (sub == "Peatland with Willow") {
    if ((peatland.scenarios[c("Peatland with Willow"), c("initial.costs")]/(50*fields$peatland.pww.yearly.potential.profit[qq]-50*peatland.scenarios[c("Peatland with Willow"), c("maintanence.costs")]) > 50) || (peatland.scenarios[c("Peatland with Willow"), c("initial.costs")]/(50*fields$peatland.pww.yearly.potential.profit[qq]-50*peatland.scenarios[c("Peatland with Willow"), c("maintanence.costs")]) < 0)) {
      return(9999)
    } else {
      return(peatland.scenarios[c("Peatland with Willow"), c("initial.costs")]/(50*fields$peatland.pww.yearly.potential.profit[qq]-50*peatland.scenarios[c("Peatland with Willow"), c("maintanence.costs")]))
    }
  } else if (sub == "Land Quota") {
    if ((peatland.scenarios[c("Land Quota"), c("initial.costs")]/(50*fields$peatland.lq.yearly.potential.profit[qq]-50*peatland.scenarios[c("Land Quota"), c("maintanence.costs")]) > 50) || (peatland.scenarios[c("Land Quota"), c("initial.costs")]/(50*fields$peatland.lq.yearly.potential.profit[qq]-50*peatland.scenarios[c("Land Quota"), c("maintanence.costs")]) < 0)) {
      return(9999)
    } else {
      return(peatland.scenarios[c("Land Quota"), c("initial.costs")]/(50*fields$peatland.lq.yearly.potential.profit[qq]-50*peatland.scenarios[c("Land Quota"), c("maintanence.costs")]))
    }
  }
}

