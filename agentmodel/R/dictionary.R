dictionary <- function(word) {
  sanasto <- list()
  
  #### crops
  sanasto["RCG.Peat"] <- "Reed canary grass that is grown on drained peatland. See Crop.Peat"
  sanasto["Barley.Peat"] <- "Barley that is grown on drained peatland. See Crop.Peat"
  sanasto["Silage.Peat"] <- "Silage that is grown on drained peatland. See Crop.Peat"
  sanasto["Crop.Peat"] <- "Should be the same as the crop e.g. \"Barley\" and converted.peatland == True."
  
  sanasto["RCG"] <- "Reed canary grass, paludiculture, there is no mineral alternative"
  
  # Population variables
  sanasto["converted.peatland"] <- "Soil that used to be peatland, but now has been drained and converted into \"normal\" soil"
  sanasto[""]
  
  
  return(sanasto[[word]])
  
}


