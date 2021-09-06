##########
## Current packages needed to make updates to the package. 
##########

library(remotes)
library(devtools)

# setwd("location") - set your location
Sys.setenv(R_REMOTES_STANDALONE="true")
remotes::install_local("agentmodel", force = T) 
library(agentmodel)

# RUN THIS IF PACKAGE UPDATES WERE MADE
# setwd("location") - set your location
devtools::document()
