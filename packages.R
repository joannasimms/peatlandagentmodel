library(remotes)
library(devtools)

setwd("C:/Users/Käyttäjä/OneDrive/Documents/Uni/Aalto University/Thesis/agentmodel")
Sys.setenv(R_REMOTES_STANDALONE="true") # solves my folder issue
remotes::install_local("agentmodel", force = T) 
library(agentmodel)

# RUN THIS IF PACKAGE UPDATES WERE MADE
setwd("agentmodel/")
devtools::document()
