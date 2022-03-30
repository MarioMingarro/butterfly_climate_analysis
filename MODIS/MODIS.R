# Clean and load packages ----
closeAllConnections()
rm(list=(ls()[ls()!="TMED"]))
gc(reset=TRUE)
source("Dependencies/Functions.R")

# load packages  
library(sf) 
library(ncdf4)
library(raster)
library(MODIStsp)



# MODIS----
## Download MODIS data -----
# library(remotes)
# install_github("ropensci/MODIStsp")

MODIStsp()

