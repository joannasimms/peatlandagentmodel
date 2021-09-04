#' Government Optimisation
#'
#' Checks the iterations of the main model to see if the agree
#' with Finish climate and economic policies. Also optimises
#' the subsidy scenario that the government would choose.
#' 
#' Gives statistics on land in conservation.
#'
#' @export

govenment.optimisation <- function(fields.history) {
  bau <- 0
  bau.hr <- 0
  pwb <- 0
  pws <- 0
  pwr <- 0
  pww <- 0
  ns <- 0
  
  for (i in 2:length(fields.history)) {
    for (j in 1:nrow(fields)) {
      if (fields.history[[i]]$next.year[j] == "BAU") {
        bau = bau + fields.history[[i]][j, c("peatland.approval.bau")]
      } else if (fields.history[[i]]$next.year[j] == "BAU Higher rates") {
        bau.hr = bau.hr + fields.history[[i]][j, c("peatland.approval.bau.hr")]
      } else if (fields.history[[i]]$next.year[j] == "Peatland with Barley") {
        pwb = pwb + fields.history[[i]][j, c("peatland.approval.pwb")]
      } else if (fields.history[[i]]$next.year[j] == "Peatland with Silage") {
        pws = pws + fields.history[[i]][j, c("peatland.approval.pws")]
      } else if (fields.history[[i]]$next.year[j] == "Peatland with RCG") {
        pwr = pwr + fields.history[[i]][j, c("peatland.approval.pwr")]
      } else if (fields.history[[i]]$next.year[j] == "Peatland with Willow") {
        pww = pww + fields.history[[i]][j, c("peatland.approval.pww")]
      } else if (fields.history[[i]]$next.year[j] == "Land Quota") {
        ns = ns + fields.history[[i]][j, c("peatland.approval.ns")]
      }
    }
  }
  
  stats <- data.frame(row.names = names(considered.subsidies))
  stats$totals <- c(bau, bau.hr, pwb, pws, pwr, pww, ns) #euros
  
  for (g in 1:nrow(stats)) {
    if (stats$total[g] > (eu.subsidies.2019 + national.subsidies.2017)*50) {
      stats$combined.budget[g] = F
      stats$amount.gvnt.pays.year[g] = (stats$total[g] - eu.subsidies.2019*50)/50
    } else {
      stats$combined.budget[g] = T
      stats$amount.gvnt.pays.year[g] = (stats$total[g] - eu.subsidies.2019*50)/50
    }
  }
  
  if (length(which(stats$totals == min(stats$totals))) == 1) {
    stats$best.policy <- rownames(stats)[which(stats$totals == min(stats$totals))]
  } else {
    for (qq in 1:length(which(stats$totals == min(stats$totals)))) {
      stats$best.policy <- paste(stats$best.policy, rownames(stats)[which(stats$totals == min(stats$totals))[qq]])
    }
  }
  
  # Is the land quota rule met
  
  
  # TODO: report the change in materials before and after
  materials <- data.frame(row.names = c("BAU", "Algo Output"))
  for (d in 1:nrow(pop)) {
    
  }
  for (i in 2:length(fields.history)) {
    
  }
  
  co2 <- 0
  ch4 <- 0
  n2o <- 0
  co2_bau <- 40 # TODO: update when yield data known
  ch4_bau <- 40 # TODO: update when yield data known
  n2o_bau <- 40 # TODO: update when yield data known
  for (i in 2:length(fields.history)) {
    for (j in 1:nrow(fields)) {
      co2 = co2 + fields.history[[i]][j, c("emissions.land.CO2")]
      ch4 = ch4 + fields.history[[i]][j, c("emissions.land.CH4")]
      n2o = n2o + fields.history[[i]][j, c("emissions.land.N2O")]
    }
  }
  carbon <- data.frame(row.names = c("BAU", "Algo Output"))
  carbon$co2 <- c(co2_bau, co2)
  carbon$ch4 <- c(ch4_bau, ch4)
  carbon$n2o <- c(n2o_bau, n2o)
  
  land.cover <- data.frame(row.names = c("percentage"))
  land.cover$peatland <- sum(fields.history[[length(fields.history)]]$peat)/nrow(fields.history[[length(fields.history)]])
  land.cover$mineral <- 1 - land.cover$peatland
  
  # TODO: add statistics for each separate peatland subsidy scenario
  
  results <- list(stats, materials, carbon, land.cover)
  print(rownames(stats)[which(stats$totals == min(stats$totals))])
  return(results)
}


      