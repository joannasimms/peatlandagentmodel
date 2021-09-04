#' Fields Peatland Subsidies
#' 
#' Checks the suitability of a field parcel for conversion into peatland.
#' 
#' 25-06-2021: Current check is whether the parcel was or is peatland or humus soil.
#' 
#' @export

fields.peatland.sub <- function(fields) {
  ### Land qualification for peatland conversion or peatland subsidies
  
  # If it was or is peatland can be conserved. This means that you cannot convert
  # mineral soils that didn't used to be peatland. Means that resources wuch as water
  # should be present or easier to put back
  for (i in 1:nrow(fields)) {
    if (fields$soil.type[i] %in% c("Peatland", "Humus Soils")) {
      fields$peatland.approval[i] <- T
    } else {
      fields$peatland.approval[i] <- F
    }
    if (fields$crop[i] %in% c("GH")) {fields$peatland.approval[i] = F}
  }
  
  # So entire farms cannot be purely conservation
#  for (h in 1:nrow(fields)) {
#    if (fields$peatland.approval[h]) {
#      if (sum(fields[h, c("farm", "rewetted.peat")][,c(2)]) > (pop$fields_owned[fields[h, c("farm")]]/2)) {
#        fields$peatland.approval[h] <- F
#      } else {
#        fields$peatland.approval[h] <- T
#      }
#    }
#  }
  print(">>>")
  return(fields)
}


