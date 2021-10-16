# ------------------

rm(list=(ls()[ls()!="v"]))
gc(reset=TRUE)
source("Dependencies/Functions.R")


#Download CHELSA dataset 
# ------------------
#Monthly dataset of precipitation, maximum-, minimum-, and mean temperatures at 30 arc sec resolution for the earths land surface areas.
#There are separate files for each month starting January 1979.

# https://envicloud.wsl.ch/#/?prefix=chelsa%2Fchelsa_V2%2FGLOBAL%2Fmonthly%2F


CHELSA_dwld_paths <- readLines("CHELSA_dwld_paths.txt")

data_rep <- "T:/CHELSA_DATA/" 

library(stringr)

for (i in 1:length(CHELSA_dwld_paths)){
  download.file(CHELSA_dwld_paths[i],
                dest = paste0(data_rep,
                              str_sub(CHELSA_dwld_paths[i], 
                                      unlist(gregexpr("tas_", CHELSA_dwld_paths[i])), 
                                      unlist(gregexpr("_V.2", CHELSA_dwld_paths[i])) - 1), ".tif"),
                mode="wb")
}

