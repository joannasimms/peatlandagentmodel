#' Farm Running Money
#' 
#' Adds the taxes and subsidies (as of 12-03-2021) with respect to Barley. Silage, Reed Canary Grass and Willow.
#' Input should be fields, with organic and crop specified.
#' 
#' This considered the current scenario that the farm is in.
#' @export


farm.running.money <- function(fields, expenses.income) {
  for (j in 1:nrow(fields)) {
    # Profit, subsidy, tax and expenses
    # INCLUDING THE PEATLAND SCENARIOS WE ARE CONSIDERING
    ################ Mineral soils
    if (fields$crop[j] == "Barley" && fields$mineral[j]) { #non organic, mineral, barley
      fields$subsidy[j] <- 2*sum(subs[c("Barley"),])
      fields$profit[j] = after.tax(yield[y,c("Barley")]*(expenses.income[c("Barley"), c("Price")]*12) - expenses.income[c("Barley"), c("Expenses")]*12 + fields$subsidy[j])
      fields$next.profit[j] = after.tax(yield[y+1,c("Barley")]*(expenses.income[c("Barley"), c("Price")]*12) - expenses.income[c("Barley"), c("Expenses")]*12 + fields$subsidy[j])
      fields$avoided.losses[j]

    } else if (fields$crop[j] == "Barley.Organic" && fields$mineral[j]) { # organic, mineral, barley
      fields$subsidy[j] <- 2*sum(subs[c("Barley.Organic"),])
      fields$profit[j] = after.tax(yield[y,c("Barley.Organic")]*(expenses.income[c("Barley"), c("Price")]*12) - expenses.income[c("Barley"), c("Expenses")]*12 + fields$subsidy[j])
      fields$next.profit[j] = after.tax(yield[y+1,c("Barley.Organic")]*(expenses.income[c("Barley"), c("Price")]*12) - expenses.income[c("Barley"), c("Expenses")]*12 + fields$subsidy[j])

    } else if (fields$crop[j] == "Silage" && fields$mineral[j]) { # non organic, mineral, silage
      fields$subsidy[j] <- sum(subs[c("Silage"),])
      fields$profit[j] = after.tax(yield[y,c("Silage")]*(expenses.income[c("Silage"), c("Price")]*12) - expenses.income[c("Silage"), c("Expenses")]*12 + fields$subsidy[j])
      fields$next.profit[j] = after.tax(yield[y+1,c("Silage")]*(expenses.income[c("Silage"), c("Price")]*12) - expenses.income[c("Silage"), c("Expenses")]*12 + fields$subsidy[j])

    } else if (fields$crop[j] == "Silage.Organic" && fields$mineral[j]) { # organic, mineral, silage
      fields$subsidy[j] <- sum(subs[c("Silage.Organic"),])
      fields$profit[j] = after.tax(yield[y,c("Silage.Organic")]*(expenses.income[c("Silage"), c("Price")]*12) - expenses.income[c("Silage"), c("Expenses")]*12 + fields$subsidy[j])
      fields$next.profit[j] = after.tax(yield[y+1,c("Silage.Organic")]*(expenses.income[c("Silage"), c("Price")]*12) - expenses.income[c("Silage"), c("Expenses")]*12 + fields$subsidy[j])
    
      ############### Drained peat
    } else if (fields$crop[j] == "Barley" && fields$converted.peat[j]) { # organic, drained peat, barley
      fields$subsidy[j] <- 2*sum(subs[c("Barley"),])
      fields$profit[j] = after.tax(yield[y,c("Barley")]*(expenses.income[c("Barley"), c("Price")]*12) - expenses.income[c("Barley"), c("Expenses")]*12 + fields$subsidy[j])
      fields$next.profit[j] = after.tax(yield[y+1,c("Barley")]*(expenses.income[c("Barley"), c("Price")]*12) - expenses.income[c("Barley"), c("Expenses")]*12 + fields$subsidy[j])
      
    } else if (fields$crop[j] == "Barley.Organic" && fields$converted.peat[j]) { # organic, drained peat, barley
      fields$subsidy[j] <- 2*sum(subs[c("Barley.Organic"),])
      fields$profit[j] = after.tax(yield[y,c("Barley.Organic")]*(expenses.income[c("Barley"), c("Price")]*12) - expenses.income[c("Barley"), c("Expenses")]*12 + fields$subsidy[j])
      fields$next.profit[j] = after.tax(yield[y+1,c("Barley.Organic")]*(expenses.income[c("Barley"), c("Price")]*12) - expenses.income[c("Barley"), c("Expenses")]*12 + fields$subsidy[j])
      
    } else if (fields$crop[j] == "Silage" && fields$converted.peat[j]) { # non organic, drained peat, silage
      fields$subsidy[j] <- sum(subs[c("Silage"),])
      fields$profit[j] = after.tax(yield[y,c("Silage")]*(expenses.income[c("Silage"), c("Price")]*12) - expenses.income[c("Silage"), c("Expenses")]*12 + fields$subsidy[j])
      fields$next.profit[j] = after.tax(yield[y+1,c("Silage")]*(expenses.income[c("Silage"), c("Price")]*12) - expenses.income[c("Silage"), c("Expenses")]*12 + fields$subsidy[j])
      
    } else if (fields$crop[j] == "Silage.Organic" && fields$converted.peat[j]) { # organic, drained peat, silage
      fields$subsidy[j] <- sum(subs[c("Silage.Organic"),])
      fields$profit[j] = after.tax(yield[y,c("Silage.Organic")]*(expenses.income[c("Silage"), c("Price")]*12) - expenses.income[c("Silage"), c("Expenses")]*12 + fields$subsidy[j])
      fields$next.profit[j] = after.tax(yield[y+1,c("Silage.Organic")]*(expenses.income[c("Silage"), c("Price")]*12) - expenses.income[c("Silage"), c("Expenses")]*12 + fields$subsidy[j])
      
      ################ No crops
    } else if (fields$crop[j] == "BAU") { # rewetted
      fields$subsidy[j] <- greening(fields, j)
      fields$profit[j] <- after.tax(fields$subsidy[j] - payment(y, "BAU"))
      fields$next.profit[j] <- after.tax(fields$subsidy[j] - payment(y, "BAU"))
    
    } else if (fields$crop[j] == "BAU Higher rates") { # rewetted
      fields$subsidy[j] <- greening.alt(fields, j)
      fields$profit[j] <- after.tax(fields$subsidy[j] - payment(y, "BAU Higher rates"))
      fields$next.profit[j] <- after.tax(fields$subsidy[j] - payment(y, "BAU Higher rates"))
      
    } else if (fields$crop[j] == "Land Quota") { # rewetted
      fields$subsidy[j] <- greening.lq(fields, j)
      fields$profit[j] <- after.tax(fields$subsidy[j] - payment(y, "Land Quota"))
      fields$next.profit[j] <- after.tax(fields$subsidy[j] - payment(y, "Land Quota"))

      ################## Paldiculture
    } else if (fields$crop[j] == "Barley" && fields$peat[j]) { # Paludiculture
      fields$subsidy[j] <- 2*sum(subs[c("Barley.Organic"),])
      fields$profit[j] = after.tax(yield[y,c("Barley.Peat")]*(expenses.income[c("Barley"), c("Price")]*12) - expenses.income[c("Barley"), c("Expenses")]*12 + fields$subsidy[j] - payment(y, "Peatlands with Barley"))
      fields$next.profit[j] = after.tax(yield[y+1,c("Barley.Peat")]*(expenses.income[c("Barley"), c("Price")]*12) - expenses.income[c("Barley"), c("Expenses")]*12 + fields$subsidy[j] - payment(y, "Peatlands with Barley"))
      
    } else if (fields$crop[j] == "Silage" && fields$peat[j]) { # Paludiculture
      fields$subsidy[j] <- 2*sum(subs[c("Silage.Organic"),])
      fields$profit[j] = after.tax(yield[y,c("Silage.Peat")]*(expenses.income[c("Silage"), c("Price")]*12) - expenses.income[c("Silage"), c("Expenses")]*12 + fields$subsidy[j] - payment(y, "Peatlands with Silage"))
      fields$next.profit[j] = after.tax(yield[y,c("Silage.Peat")]*(expenses.income[c("Silage"), c("Price")]*12) - expenses.income[c("Silage"), c("Expenses")]*12 + fields$subsidy[j] - payment(y, "Peatlands with Silage"))
      
    } else if (fields$crop[j] == "RCG" && fields$peat[j]) { # Paludiculture
      fields$subsidy[j] <- 2*sum(subs[c("RCG.Organic"),])
      fields$profit[j] = after.tax(yield[y,c("RCG.Peat")]*(expenses.income[c("RCG"), c("Price")]*12) - expenses.income[c("RCG"), c("Expenses")]*12 + fields$subsidy[j] - payment(y, "Peatland with RCG"))
      fields$next.profit[j] = after.tax(yield[y,c("RCG.Peat")]*(expenses.income[c("RCG"), c("Price")]*12) - expenses.income[c("RCG"), c("Expenses")]*12 + fields$subsidy[j] - payment(y, "Peatland with RCG"))
      
    # } else if (fields$crop[j] == "Willow" && fields$peat[j]) { # Not Organic
    #  fields$subsidy[j] <- 2*sum(subs[c("Willow"),])
    #  fields$profit[j] = after.tax(0.7*yield[y,c("Willow")]*(expenses.income[c("Willow"), c("Price")]*12) - expenses.income[c("Willow"), c("Expenses")]*12  + fields$subsidy[j])
    #  fields$next.profit[j] = after.tax(0.7*yield[y,c("Willow")]*(expenses.income[c("Willow"), c("Price")]*12) - expenses.income[c("Willow"), c("Expenses")]*12  + fields$subsidy[j])
    #  
    # } else if (fields$crop[j] == "Willow.Organic" && fields$peat[j]) { # Organic
    #  fields$subsidy[j] <- 2*sum(subs[c("Willow.Organic"),])
    #  fields$profit[j] = after.tax(0.7*yield[y,c("Willow.Organic")]*(expenses.income[c("Willow"), c("Price")]*12) - expenses.income[c("Willow"), c("Expenses")]*12 + fields$subsidy[j])
    #  fields$next.profit[j] = after.tax(0.7*yield[y,c("Willow.Organic")]*(expenses.income[c("Willow"), c("Price")]*12) - expenses.income[c("Willow"), c("Expenses")]*12 + fields$subsidy[j])
    
    } else if (fields$crop[j] == "GH") { # Greenhouse
      fields$subsidy[j] <- 0
      fields$profit[j] = 0
      fields$next.profit[j] = 0
      
    } else {
      stop("unconsidered catagory")
    }
  }
  print(">>")
  return(fields)
}

