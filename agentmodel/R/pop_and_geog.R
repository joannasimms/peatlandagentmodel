#' Population and Geography builder
#' 
#' Build the population and geography based on Luke data.
#'
#' @export

pop_and_geog <- function(farm.soil.data, luke_age_dist, luke_voc, luke_organic_probs, av_farm_size_per_type) {
  pop <- data.frame(row.names =  seq(1,farm.soil.data[c("Lappi"), c("Number.of.farms")]))
  pop$age <- sample(x=names(luke_age_dist)[-c(1)], size=nrow(pop), replace = T, prob = luke_age_dist[,-c(1)]/luke_age_dist[,c("total")])
  pop$vocation <- sample(x = names(luke_voc), size = nrow(pop), replace = T, prob = luke_voc[c("Lapland"),])
  # Lapland considered as I am only considering Lapland at first
  
  pop$use <- sample(x=rownames(farm.use)[-c(2,8,9)], size=nrow(pop), replace = T, prob = farm.use$percentage_2019[-c(2,8,9)])
  
  pop$organic <- NA
  pop$organic[which(pop$use == "Milk.production")] <- sample(x=c(T, F), size = length(pop$organic[which(pop$use == "Milk.production")]), replace = T, prob = c(luke_organic_probs["Cattle"], 1-luke_organic_probs["Cattle"]))
  pop$organic[which(pop$use == "Beef.production")] <- sample(x=c(T, F), size = length(pop$organic[which(pop$use == "Beef.production")]), replace = T, prob = c(luke_organic_probs["Cattle"], 1-luke_organic_probs["Cattle"]))
  pop$organic[which(pop$use == "Other.cattle.husbandry.2.")] <- sample(x=c(T, F), size = length(pop$organic[which(pop$use == "Other.cattle.husbandry.2.")]), replace = T, prob = c(luke_organic_probs["Cattle"], 1-luke_organic_probs["Cattle"]))
  pop$organic[which(pop$use == "Other.grazing.livestock.2.")] <- sample(x=c(T, F), size = length(pop$organic[which(pop$use == "Other.grazing.livestock.2.")]), replace = T, prob = c(luke_organic_probs["Sheep"], 1-luke_organic_probs["Sheep"]))
  pop$organic[which(pop$use == "Other.grazing.livestock.3.")] <- sample(x=c(T, F), size = length(pop$organic[which(pop$use == "Other.grazing.livestock.3.")]), replace = T, prob = c(luke_organic_probs["Sheep"], 1-luke_organic_probs["Sheep"]))
  
  pop$organic[which(pop$use == "Cereals.production")] <- sample(x=c(T, F), size = length(pop$organic[which(pop$use == "Cereals.production")]), replace = T, prob = c(luke_organic_probs["Barley"], 1-luke_organic_probs["Barley"]))
  # Barley is the only crop considered in the cereals production section
  pop$organic[which(pop$use == "Mixed.production")] <- sample(x=c(T, F), size = length(pop$organic[which(pop$use == "Mixed.production")]), replace = T, prob = c(luke_organic_probs["Barley"], 1-luke_organic_probs["Barley"]))
  pop$organic[which(pop$use == "Other.plant.production.1.")] <- sample(x=c(T, F), size = length(pop$organic[which(pop$use == "Other.plant.production.1.")]), replace = T, prob = c(luke_organic_probs["Barley"], 1-luke_organic_probs["Barley"]))
  pop$organic[which(pop$use == "Outdoor.production.1.")] <- sample(x=c(T, F), size = length(pop$organic[which(pop$use == "Outdoor.production.1.")]), replace = T, prob = c(luke_organic_probs["Barley"], 1-luke_organic_probs["Barley"]))
  pop$organic[which(pop$use == "Other.outdoor.production")] <- sample(x=c(T, F), size = length(pop$organic[which(pop$use == "Outdoor.production")]), replace = T, prob = c(luke_organic_probs["Barley"], 1-luke_organic_probs["Barley"]))
  pop$organic[which(pop$use == "Other.plant.production")] <- sample(x=c(T, F), size = length(pop$organic[which(pop$use == "Other.plant.production")]), replace = T, prob = c(luke_organic_probs["Barley"], 1-luke_organic_probs["Barley"]))
  
  pop$crop <- NA
  pop$crop[which(pop$use == "Milk.production" & pop$organic == F)] <- "Silage"
  pop$crop[which(pop$use == "Milk.production" & pop$organic == T)] <- "Silage.Organic"
  pop$crop[which(pop$use == "Beef.production" & pop$organic == F)] <- "Silage"
  pop$crop[which(pop$use == "Beef.production" & pop$organic == T)] <- "Silage.Organic"
  pop$crop[which(pop$use == "Other.cattle.husbandry.2." & pop$organic == F)] <- "Silage"
  pop$crop[which(pop$use == "Other.cattle.husbandry.2." & pop$organic == T)] <- "Silage.Organic"
  pop$crop[which(pop$use == "Other.grazing.livestock.2." & pop$organic == F)] <- "Silage"
  pop$crop[which(pop$use == "Other.grazing.livestock.2." & pop$organic == T)] <- "Silage.Organic"
  pop$crop[which(pop$use == "Other.grazing.livestock.3." & pop$organic == F)] <- "Silage"
  pop$crop[which(pop$use == "Other.grazing.livestock.3." & pop$organic == T)] <- "Silage.Organic"
  
  pop$crop[which(pop$use == "Cereals.production" & pop$organic == F)] <- "Barley"
  pop$crop[which(pop$use == "Cereals.production" & pop$organic == T)] <- "Barley.Organic"
  pop$crop[which(pop$use == "Mixed.production" & pop$organic == F)] <- "Barley"
  pop$crop[which(pop$use == "Mixed.production" & pop$organic == T)] <- "Barley.Organic"
  pop$crop[which(pop$use == "Other.plant.production.1." & pop$organic == F)] <- "Barley"
  pop$crop[which(pop$use == "Other.plant.production.1." & pop$organic == T)] <- "Barley.Organic"
  pop$crop[which(pop$use == "Outdoor.production.1." & pop$organic == F)] <- "Barley"
  pop$crop[which(pop$use == "Outdoor.production.1." & pop$organic == T)] <- "Barley.Organic"
  pop$crop[which(pop$use == "Other.outdoor.production" & pop$organic == F)] <- "Barley"
  pop$crop[which(pop$use == "Other.outdoor.production" & pop$organic == T)] <- "Barley.Organic"
  pop$crop[which(pop$use == "Other.plant.production" & pop$organic == F)] <- "Barley"
  pop$crop[which(pop$use == "Other.plant.production" & pop$organic == T)] <- "Barley.Organic"
  
  pop$crop[which(pop$use == "Greenhouse.production")] <- "GH"
  
  pop$average_land <- NA
  pop$average_land[which(pop$use == "Milk.production")] <- sample(seq(0, 70, by = 0.02), length(which(pop$use == "Milk.production")), replace = T, prob = PDF(seq(0, 70, by = 0.02), 15, 6, 0, 70)) 
  pop$average_land[which(pop$use == "Beef.production")] <- sample(seq(0, 70, by = 0.02), length(which(pop$use == "Beef.production")), replace = T, prob = PDF(seq(0, 70, by = 0.02), 15, 6, 0, 70)) 
  pop$average_land[which(pop$use == "Other.cattle.husbandry.2.")] <- sample(seq(0, 70, by = 0.02), length(which(pop$use == "Other.cattle.husbandry.2.")), replace = T, prob = PDF(seq(0, 70, by = 0.02), 15, 6, 0, 70)) 
  pop$average_land[which(pop$use == "Other.grazing.livestock.2.")] <- sample(seq(0, 70, by = 0.02), length(which(pop$use == "Other.grazing.livestock.2.")), replace = T, prob = PDF(seq(0, 70, by = 0.02), 15, 6, 0, 70)) 
  pop$average_land[which(pop$use == "Other.grazing.livestock.3.")] <- sample(seq(0, 70, by = 0.02), length(which(pop$use == "Other.grazing.livestock.3.")), replace = T, prob = PDF(seq(0, 70, by = 0.02), 15, 6, 0, 70)) 
  pop$average_land[which(pop$use == "Cereals.production")] <- sample(seq(2, 20, by = 0.02), length(which(pop$use == "Cereals.production")), replace = T, prob = PDF(seq(2, 20, by = 0.02), 4, 2, 2, 20)) 
  pop$average_land[which(pop$use == "Mixed.production")] <- sample(seq(2, 20, by = 0.02), length(which(pop$use == "Mixed.production")), replace = T, prob = PDF(seq(2, 20, by = 0.02), 4, 2, 2, 20)) 
  pop$average_land[which(pop$use == "Other.plant.production.1.")] <- sample(seq(2, 20, by = 0.02), length(which(pop$use == "Other.plant.production.1.")), replace = T, prob = PDF(seq(2, 20, by = 0.02), 4, 2, 2, 20)) 
  pop$average_land[which(pop$use == "Outdoor.production.1.")] <- sample(seq(2, 20, by = 0.02), length(which(pop$use == "Outdoor.production.1.")), replace = T, prob = PDF(seq(2, 20, by = 0.02), 4, 2, 2, 20)) 
  pop$average_land[which(pop$use == "Other.outdoor.production")] <- sample(seq(2, 20, by = 0.02), length(which(pop$use == "Other.outdoor.production")), replace = T, prob = PDF(seq(2, 20, by = 0.02), 4, 2, 2, 20)) 
  pop$average_land[which(pop$use == "Other.plant.production")] <- sample(seq(2, 20, by = 0.02), length(which(pop$use == "Other.plant.production")), replace = T, prob = PDF(seq(2, 20, by = 0.02), 4, 2, 2, 20)) 
  
  pop$average_land[which(pop$use == "Greenhouse.production")] <- 0
  
  mitigation.op = (sample(1:5, nrow(pop), replace = T, prob = c(3, 8, 29, 16, 4)/50)) # i can mitigate climate emission at my own farm
                  #   sample(1:5, nrow(pop), replace = T, prob = c(3, 6, 25-9, 44-25, 6)/50) + # practices made by the farmers can mitigate climate change in Finalnd
                  #   sample(1:5, nrow(pop), replace = T, prob = c(3, 4, 23-7, 42-23, 8)/50) + # my farming choices influence climate emissions
                  #   sample(1:5, nrow(pop), replace = T, prob = c(1, 4, 13, 24, 8)/50) + # farms can mitigate climate change with farming practices
                  #   sample(1:5, nrow(pop), replace = T, prob = c(2, 7, 16, 22, 4)/50) + # farmers can adapt to the adverse impact caused by climate change
                  #   sample(1:5, nrow(pop), replace = T, prob = c(2, 7, 20, 18, 3)/50) + # I can adapt to the adverse impact caused by climate change on my farm
                  #   sample(1:5, nrow(pop), replace = T, prob = c(1, 6, 18, 22, 5)/50) + # agriculture can adapt to the adverse impact caused by climate change
                  #   sample(1:5, nrow(pop), replace = T, prob = c(1, 2, 14, 50, 33)/100))/(8) # Climate is change not happening, not evidence, climate change happening natural, climate change natural and human emissions, human activities
  
  biodiversity.op = # (sample(1:5, nrow(pop), replace = T, prob = c(2, 5, 15, 26, 16) / 65)) # maintaining biodiversity
                       sample(1:5, nrow(pop), replace = T, prob = c(5, 9, 19, 18, 8) / 65) # sparing poorly performing fields as habitats for fauna and flora
                    #   sample(1:5, nrow(pop), replace = T, prob = c(8, 12, 20, 17, 7) / 65) + # maintaining diverse flora in field edges
                    #   sample(1:5, nrow(pop), replace = T, prob = c(17, 19, 18, 11, 5) / 65))/(4) # leaving patches of forest between field parcels or in the middle of them
  
  novel.op = sample(1:5, nrow(pop), replace = T, prob = c(1, 5, 19, 31, 7)/65)
  
  pop$opinion <- (mitigation.op + novel.modi(pop) * novel.op + bio.modi(pop) * biodiversity.op)/3
  
  # Parcels arbitrary decided to be 2 ha
  pop$fields_owned <- round(pop$average_land/2)
  
  fields <- data.frame(row.names = 1:sum(pop$fields_owned, na.rm = T))
  a = 1
  farm <- 0
  farm[1:pop$fields_owned[1]] <- 1
  for (cc in 2:nrow(pop)) {
    if (is.na(pop$fields_owned[cc]) == F) {
      farm <- append(x = farm,  values = rep(x = a, times = pop$fields_owned[cc]), after = max(which(farm > 0)))
    }
    a = a + 1
  }
  fields$farm <- farm
  for (dd in 1:nrow(fields)) {
    fields$type[dd] <- pop$use[fields$farm[dd]]
    fields$crop[dd] <- pop$crop[fields$farm[dd]]
  }
  
  fields$peat <- sample(c(T, F), nrow(fields), replace = T, prob = c(0.31, 1-0.31)) # percentage from kekkonen 2019, share of organic cultervated, agrees with LUKE
  # CONVERTED PEAT IS MINERAL SOIL! DRAINED ORGANIC SOIL!
  fields$rewetted.peat <- F
  for (ff in 1:nrow(fields)) {if (fields$peat[ff] != T && fields$rewetted.peat[ff] != T) {fields$mineral[ff] = T} else {fields$mineral[ff] = F}}
  for (gg in 1:nrow(fields)) {if (fields$peat[gg] == T) {fields$soil.type[gg] = sample(c("Peatland", "Humus Soils"), 1, replace = T, prob = c(15/31.6, 17.364/31.6))} else (fields$soil.type[gg] = "Rough Mineral")} # as clay is only 0.1% of the mineral soil content of lapland, luke soil data
  for (ee in 1:nrow(fields)) {if (fields$peat[ee] == T && fields$soil.type[ee] == "Humus Soils") {fields$converted.peat[ee] = T} else {fields$converted.peat[ee] <- F}} #turunen2008development mainly in northwest
  fields$crop[fields$soil.type == "Peatland"] <- "BAU"
  return(list(fields, pop))
}

