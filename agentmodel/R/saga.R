#' Existence Need Satisfaction
#' 
#' the SAGA taken method to create a measure to base the agent decisions on.
#' 
#' Not be edited by farmer type as all farmers need to make a living
#' @export

ens <- function(fields, subsidy) {
  # Original: income / potential income
  if (subsidy == "BAU") {
    ens <- fields$profit/fields$peatland.bau.yearly.potential.profit
    if (fields$profit < 0 & fields$peatland.bau.yearly.potential.profit > 0) {ens = -999999}
    if (is.na(ens)) {ens = -999999}
    if (is.nan(ens)) {ens = -999999}
  } else if (subsidy == "BAU Higher rates") {
    ens <- fields$profit/fields$peatland.bau.hr.yearly.potential.profit
    if (fields$profit < 0 & fields$peatland.bau.hr.yearly.potential.profit > 0) {ens = -999999}
    if (is.na(ens)) {ens = -999999}
    if (is.nan(ens)) {ens = -999999}
  } else if (subsidy == "Peatland with Barley") {
    ens <- fields$profit/fields$peatland.pwb.yearly.potential.profit
    if (fields$profit < 0 & fields$peatland.pwb.yearly.potential.profit > 0) {ens = -999999}
    if (is.na(ens)) {ens = -999999}
    if (is.nan(ens)) {ens = -999999}
  } else if (subsidy == "Peatland with Silage") {
    ens <- fields$profit/fields$peatland.pws.yearly.potential.profit
    if (fields$profit < 0 & fields$peatland.pws.yearly.potential.profit > 0) {ens = -999999}
    if (is.na(ens)) {ens = -999999}
    if (is.nan(ens)) {ens = -999999}
  } else if (subsidy == "Peatland with RCG") {
    ens <- fields$profit/fields$peatland.pwr.yearly.potential.profit
    if (fields$profit < 0 & fields$peatland.pwr.yearly.potential.profit > 0) {ens = -999999}
    if (is.na(ens)) {ens = -999999}
    if (is.nan(ens)) {ens = -999999}
  } else if (subsidy == "Peatland with Willow") {
    ens <- fields$profit/fields$peatland.pww.yearly.potential.profit
    if (fields$profit < 0 & fields$peatland.pww.yearly.potential.profit > 0) {ens = -999999}
    if (is.na(ens)) {ens = -999999}
    if (is.nan(ens)) {ens = -999999}
  } else if (subsidy == "Land Quota") {
    ens <- fields$profit/fields$peatland.lq.yearly.potential.profit
    if (fields$profit < 0 & fields$peatland.lq.yearly.potential.profit > 0) {ens = -999999}
    if (is.na(ens)) {ens = -999999}
    if (is.nan(ens)) {ens = -999999}
  } else {
    stop("peatland not identified")
  }
  return(ens)
}

#' Uncertainty Level
#' 
#' the SAGA taken method to create a measure to base the agent decisions on.
#' 
#' Not be edited by farmer type as all farmers need to make a living
#' @export

ut <- function(fields) {
  # This is considering that the farmer would stay the same for the next year so subsidy and type irrelevant
  ut = 1 - (fields$profit/fields$next.profit)
  if (is.infinite(ut)) {ut = -999999}
  if (is.nan(ut)) {ut = -999999}
  return(ut)
}

#' Row correlation 
#' 
#' To find the correlation between rows compared to a certain farm.
#' 
#' @export

row.correlation <- function(pop, row) {
  
  comp <- NA
  for (i in 1:nrow(pop)) {
    comp[i] <- if (compare::compare(pop[i,], pop[row,])[[1]]) {comp[i] <- 7} else {comp[i] <- sum(compare::compare(pop[i,], pop[row,])[[2]])}
  }
  
  return(comp)
}

#' Strong Link True
#' 
#' This function compares the fields in terms of similarity via correlation.
#' It then considers the farms that have a 70% correlation and checks what the
#' subsidies and land uses the similar farms have chosen.
#' @export

strong.link.true <- function(pop, fields, row) {
  #ham.dist <- ham.dist[[row]]
  cor <- row.correlation(pop, row)
  p <- rep.int(0, times = nrow(fields))
  # which are the most similar farms
  for (h in which(cor > 5 - pop[row, c("opinion")])) {
    for (con in 1:length(considered.subsidies)) {
      if (names(considered.subsidies)[con] %in% fields[fields$farm == h, c("crop")]) {
        p[con] = T
      }
    }
  }
  if (sum(p) > 3) {
    return(T)
  } else {
    return(F)
  }
}

#' Strong Link True
#' 
#' It then considers all of the farms (unlike stong.link.true) that and checks what the
#' subsidies and land uses the similar farms have chosen.
#' @export

weak.link.true <- function(pop, fields, row) {
  p <-  NA
  for (h in 1:nrow(pop)) {
    for (con in 1:length(considered.subsidies)) {
      if (names(considered.subsidies)[con] %in% fields[fields$farm == h, c("crop")]) {
        p[con] = T
      }
    }
  }
  if (sum(p) > 3) {
    return(T)
  } else {
    return(F)
  }
}


#' Social Action
#' 
#' This is the function that controls the agent behavior. The two 
#' functions ens and ut are checked and based on these numbers four actions can happen.
#' If these actions are chosen the input dataframe "fields" is updated with the farmer utility
#' for the said peatland scenario. If not the value is se to 0.
#' 
#' @export

social.action <- function(fields, pop) { # TODO: go through when opinions added
  for (r in 1:nrow(fields)) {
    if (fields$peatland.approval[r]) {
      print(r)
      for (sub in names(considered.subsidies)) {
        if (fields$crop[r] %in% c("BAU", "BAU Higher rates", "Land Quota") & sub %in% c("BAU", "BAU Higher rates", "Land Quota")) {
          fields[r, considered.subsidies[[sub]][3]] = peatland.utility(fields, pop, r, sub)
        } else {
          if (abs(fields[r, c(considered.subsidies[[sub]][1])]) >= opinion.to.results(pop, fields$farm[r], "profit difference")) {# This is that the profit is smaller the next year 
            # Repetition
            if (fields$profit[r] >= opinion.to.results(pop, fields$farm[r], "loses")) {# losses not too bad
              print("repitition")
              # If agent is satisfied repeats previous behavior
              fields[r, considered.subsidies[[sub]][3]] = -999999
            } else {
              # Imitation
              if (strong.link.true(pop, fields, fields$farm[r])) {
                print("imitation")
                fields[r, considered.subsidies[[sub]][3]] = peatland.utility(fields, pop, r, sub)
              } else {
                print("Not Approved")
                fields[r, considered.subsidies[[sub]][3]] = -999999
              }
            }
          } else {
            if (fields[r, c(considered.subsidies[[sub]][1])] < opinion.to.results(pop, fields$farm[r], "profit difference")) {
              # Deliberation
              if (fields[r, considered.subsidies[[sub]][2]] < opinion.to.results(pop, fields$farm[r], "time to pay back")) { # time to pay back too long (12, 25) current range
                print("deliberation")
                fields[r, considered.subsidies[[sub]][3]] = peatland.utility(fields, pop, r, sub)
              } else {
                print("Not Approved")
                fields[r, considered.subsidies[[sub]][3]] = -999999
              }
            } else {
              # Social Comparison
              if (weak.link.true(pop, fields, fields$farm[r])) {
                print("social comparison")
                fields[r, considered.subsidies[[sub]][3]] = peatland.utility(fields, pop, r, sub)
              } else {
                print("Not Approved")
                fields[r, considered.subsidies[[sub]][3]] = -999999
              }
            }
          }
        }
      }
    }
  }
  print(">>>>>>")
  return(fields)
}


# make the levels related to the farmers opinion

#' Opinion to results
#'
#' Creates the parameters for the SAGA process based on the opinion of the farmer
#'
#'
#' @export

opinion.to.results <- function(pop, row, param) {
  if (param == "loses") {out = -7500 - 600  * pop$opinion[row]}
  if (param == "time to pay back") {out = 26}
  if (param == "profit difference") {out = pop$opinion[row]/4} # dicided by 4 rather than 5 as 5 people are more likely to take risks for the environment
  return(out)
}

