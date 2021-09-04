#' Agent Optimisation
#' 
#' This function chooses the subsidy scenario with the highest utility.
#' Then the parcel soil type is updated.
#' 
#' @export

agent.optimisation <- function(fields) {
  # after the disqualifications above, choose the scheme which has the most utility
  fields$next.year <- NA
  scens <- NA
  for (con in 1:length(considered.subsidies)) {
    scens[con] <- considered.subsidies[[con]][3]
  }
  for (r in 1:nrow(fields)) {
    scens <- NA
    for (con in 1:length(considered.subsidies)) {
      scens[con] <- considered.subsidies[[con]][3]
    }
    if (fields$peatland.approval[r]) {
      if (sum(fields[c(r), scens] == seq(-999999, -999999, length.out = length(scens))) != length(scens)) {
        if (fields[c(r), c("crop")] %in% c("BAU", "BAU Higher rates", "Land Quota")) {
          scens <- scens[!scens %in% c("peatland.approval.pwb", "peatland.approval.pws", "peatland.approval.pwr")]
        }
        if (length(which(fields[c(r),scens] == max(fields[c(r), scens]))) == 1) {
          fields$next.year[r] <- rownames(peatland.scenarios)[which(fields[c(r),scens] == max(fields[c(r), scens]))]
          if (fields$next.year[r] %in% c("BAU", "BAU Higher rates", "Land Quota")) { # Become natural peatland
            fields$peat[r] <- T
            fields$converted.peat[r] <- F
            fields$mineral[r] <- F
            fields$rewetted.peat[r] <- T
            if (fields$next.year[r] == "BAU") {
              fields$crop[r] <- "BAU"
            } else if (fields$next.year[r] == "BAU Higher rates") {
              fields$crop[r] <- "BAU Higher rates"
            } else if (fields$next.year[r] == "Land Quota") {
              fields$crop[r] <- "Land Quota"
            }
          } else if (fields$next.year[r] %in% c("Peatland with Barley", "Peatland with Silage", "Peatland with RCG", "Peatland with Willow")) {
            fields$type[r] <- "Peatland.with.produce"
            fields$peat[r] <- T
            fields$converted.peat[r] <- F
            fields$mineral[r] <- F
            fields$rewetted.peat[r] <- F
            if (pop$crop[fields$farm[r]] != "GH") {
              if (fields$next.year[r] == "Peatland with Silage") {
                fields$crop[r] = "Silage"
                #if (pop$organic[fields$farm[r]]) {fields$crop[r] = "Silage.Organic"} else {fields$crop[r] = "Silage"}
              }
              else if (fields$next.year[r] == "Peatland with Barley") {
                fields$crop[r] = "Barley"
                #if (pop$organic[fields$farm[r]]) {fields$crop[r] = "Barley.Organic"} else {fields$crop[r] = "Barley"}
              }
              else if (fields$next.year[r] == "Peatland with RCG") {
                fields$crop[r] = "RCG"
                #if (pop$organic[fields$farm[r]]) {fields$crop[r] = "RCG.Organic"} else {fields$crop[r] = "RCG"}
              }
              else if (fields$next.year[r] == "Peatland with Willow") {
                fields$crop[r] = "Willow"
                #if (pop$organic[fields$farm[r]]) {fields$crop[r] = "Willow.Organic"} else {fields$crop[r] = "Willow"}
              }
              else {stop("No crop avaliable")}
            }
          } else {
            stop("Scenario not chosen!")
          }
        } else if (length(which(fields[c(r),scens] == max(fields[c(r), scens]))) > 1) {
          stop("Draw, make code to deal with it")
        } else {
          fields$next.year[r] <- "same"
        }
      }
    }
  }
  return(fields)
}
