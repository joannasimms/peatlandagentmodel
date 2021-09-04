#' Novel crop modifier
#' 
#' Demographic changes taken from LUKE.
#'
#' @export

novel.modi <- function(pop) {
  scaler = rep(1, nrow(pop))
  
  for (n in 1:nrow(pop)) {
    
    if (is.na(pop[n, c("average_land")])) {
      scaler[n] = scaler[n]
    } else {
      # size
      if (pop[n, c("average_land")] < 30) {scaler[n] = scaler[n] * 1.3} 
      else if (pop[n, c("average_land")] <= 30 & pop[n, c("average_land")] < 50) {scaler[n] = scaler[n] * 1.2}
      else if (pop[n, c("average_land")] <= 50 & pop[n, c("average_land")] < 99) {scaler[n] = scaler[n] * 0.8}
      else if (pop[n, c("average_land")] > 100) {scaler = scaler * 0.7}
    }
    
    # age
    if (pop[n, c("age")] == "<29") {scaler[n] = scaler[n] * 1.2} 
    else if (pop[n, c("age")] == "30-49") {scaler[n] = scaler[n] * 1.15}
    else if (pop[n, c("age")] == "50-69") {scaler[n] = scaler[n] * 0.8}
    else if (pop[n, c("age")] == ">70") {scaler[n] = scaler[n] * 0.75}
  }
  
  return(scaler)
}

#' Biodiversity modifier
#' 
#' Demographic changes taken from LUKE.
#'
#' @export

bio.modi <- function(pop) {
  scaler <-  rep(1, nrow(pop))
  
  for (n in 1:nrow(pop)) {
    
    # size
    if (is.na(pop[n, c("average_land")])) {
      scaler[n] = scaler[n]
    } else {
      if (pop[n, c("average_land")] < 30) {scaler[n] = scaler[n] * 1.3} 
      else if (pop[n, c("average_land")] <= 30 & pop[n, c("average_land")] < 50) {scaler[n] = scaler[n] * 1.15}
      else if (pop[n, c("average_land")] <= 50 & pop[n, c("average_land")] < 99) {scaler[n] = scaler[n] * 0.8}
      else if (pop[n, c("average_land")] > 100) {scaler[n] = scaler[n] * 0.7}
    }
    
    # age
    if (pop[n, c("age")] == "<29") {scaler[n] = scaler[n] * 0.7} 
    else if (pop[n, c("age")] == "30-49") {scaler[n] = scaler[n] * 0.8}
    else if (pop[n, c("age")] == "50-69") {scaler[n] = scaler[n] * 1}
    else if (pop[n, c("age")] == ">70") {scaler[n] = scaler[n] * 1.3}
    
    # organic
    if (is.na(pop[n, c("organic")])) {
      scaler[n] = scaler[n]
    } else {
      if (pop[n, c("organic")] == T) {scaler[n] = scaler[n] * 1.3} else {scaler[n] = scaler[n] * 0.7}
    }
  }
  
  return(scaler)
}
