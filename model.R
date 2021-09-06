########
## The current model procees
########
# TODO: make into a function

# Build the population from population.R
info <- pop_and_geog(farm.soil.data, luke_age_dist, luke_voc, luke_organic_probs, av_farm_size_per_type)
pop <- info[[2]]
fields <- info[[1]]
# Basics of a forest geography build if needed later, but in GIT history

considered.subsidies <- list("BAU" = c("bau.ens", "peatland.bau.time.to.pay.back", "peatland.approval.bau", "peatland.bau.yearly.potential.profit"), 
                             "BAU Higher rates" = c("bau.hr.ens", "peatland.bau.hr.time.to.pay.back", "peatland.approval.bau.hr", "peatland.bau.hr.yearly.potential.profit"),
                             "Peatland with Barley" = c("pwb.ens", "peatland.pwb.time.to.pay.back", "peatland.approval.pwb", "peatland.pwb.yearly.potential.profit"),
                             "Peatland with Silage" = c("pws.ens", "peatland.pws.time.to.pay.back", "peatland.approval.pws", "peatland.pws.yearly.potential.profit"),
                             "Peatland with RCG" = c("pwr.ens", "peatland.pwr.time.to.pay.back", "peatland.approval.pwr", "peatland.pwr.yearly.potential.profit"),
                             "Land Quota" = c("lq.ens", "peatland.lq.time.to.pay.back", "peatland.approval.lq", "peatland.lq.yearly.potential.profit"))

# fields <- fields[1:100,]

####
# From here on the process is iterated for the number of years
####

fields.history <- list("Original" = fields)
for (y in 1:5) { # remember need the next years profit to do the finial iteration!
  print(paste("year =", y))
  # retrieving the right farms file
  if (y == 1) {
    fields <- fields.history$Original
  } else { 
    fields <- fields.history[[y-1]]
  }
  
  # Money related matters from money.R
  fields <- farm.emissions.money(fields, ghg.mapping.data) #>
  fields <- farm.running.money(fields, expenses.income) #>>
  
  fields <- fields.peatland.sub(fields) # >>>
  fields <- farm.peatland.scenarios(fields, peatland.scenarios, y, considered.subsidies) #>>>>
  # check the values and units match
  
  # Peatland subsidies from main_optimisation.R
  
  fields <- farmer.ens.ut(fields) #>>>>>
  fields <- social.action(fields, pop) #>>>>>>
  
  # Agent based optimisation
  fields <- agent.optimisation(fields)
  
  fields.history[[y+1]] <- fields
}


