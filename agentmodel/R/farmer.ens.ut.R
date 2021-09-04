#' Farmer Ens Ut
#'
#' Calculating the ens and ut for all fields.
#' 
#' @export


farmer.ens.ut <- function(fields) {
  for (j in 1:nrow(fields)) {
    if (fields$peatland.approval[j]) {
      if ("BAU" %in% names(considered.subsidies)) {fields$bau.ens[j] <- ens(fields[j,], "BAU")}
      if ("BAU Higher rates" %in% names(considered.subsidies)) {fields$bau.hr.ens[j] <- ens(fields[j,], "BAU Higher rates")}
      if ("Peatland with Barley" %in% names(considered.subsidies)) {fields$pwb.ens[j] <- ens(fields[j,], "Peatland with Barley")}
      if ("Peatland with Silage" %in% names(considered.subsidies)) {fields$pws.ens[j] <- ens(fields[j,], "Peatland with Silage")}
      if ("Peatland with RCG" %in% names(considered.subsidies)) {fields$pwr.ens[j] <- ens(fields[j,], "Peatland with RCG")}
      # fields$pww.ens[j] <- ens(fields[j,], "Peatland with Willow")
      if ("Land Quota" %in% names(considered.subsidies)) {fields$lq.ens[j] <- ens(fields[j,], "Land Quota")}
      
      if ("BAU" %in% names(considered.subsidies)) {fields$bau.ut[j] <- ut(fields[j,])}
      if ("BAU Higher rates" %in% names(considered.subsidies)) {fields$bau.hr.ut[j] <- ut(fields[j,])}
      if ("Peatland with Barley" %in% names(considered.subsidies)) {fields$pwb.ut[j] <- ut(fields[j,])}
      if ("Peatland with Silage" %in% names(considered.subsidies)) {fields$pws.ut[j] <- ut(fields[j,])}
      if ("Peatland with RCG" %in% names(considered.subsidies)) {fields$pwr.ut[j] <- ut(fields[j,])}
      # fields$pww.ut[j] <- ut(fields[j,])
      if ("Land Quota" %in% names(considered.subsidies)) {fields$lq.ut[j] <- ut(fields[j,])}
    } else { # Numbers too small to be counted in the maximisation function
      if ("BAU" %in% names(considered.subsidies)) {fields$bau.ens[j] <- -999999}
      if ("BAU Higher rates" %in% names(considered.subsidies)) {fields$bau.hr.ens[j] <- -999999}
      if ("Peatland with Barley" %in% names(considered.subsidies)) {fields$pwb.ens[j] <- -999999}
      if ("Peatland with Silage" %in% names(considered.subsidies)) {fields$pws.ens[j] <- -999999}
      if ("Peatland with RCG" %in% names(considered.subsidies)) {fields$pwr.ens[j] <- -999999}
      #fields$pww.ens[j] <- -999999
      if ("Land Quota" %in% names(considered.subsidies)) {fields$lq.ens[j] <- -999999}
      
      if ("BAU" %in% names(considered.subsidies)) {fields$bau.ut[j] <- -999999}
      if ("BAU Higher rates" %in% names(considered.subsidies)) {fields$bau.hr.ut[j] <- -999999}
      if ("Peatland with Barley" %in% names(considered.subsidies)) {fields$pwb.ut[j] <- -999999}
      if ("Peatland with Silage" %in% names(considered.subsidies)) {fields$pws.ut[j] <- -999999}
      if ("Peatland with RCG" %in% names(considered.subsidies)) {fields$pwr.ut[j] <- -999999}
      #fields$pww.ut[j] <- -999999
      if ("Land Quota" %in% names(considered.subsidies)) {fields$lq.ut[j] <- -999999}
    }
  }
  print(">>>>>")
  return(fields)
}
