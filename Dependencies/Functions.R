## ---------------------------------------------------------------------------------------------------------
## ---------------------------------------------------------------------------------------------------------
##
## C.M.I.P. Data Request and Processing
## Muffins 'n' Code
## https://github.com/jorgeassis
##
## ---------------------------------------------------------------------------------------------------------
## ---------------------------------------------------------------------------------------------------------
## Dependencies

packages.to.use <- c("tictoc", "raster", "devtools", "VoCC","rgeos",
                     "rasterVis","gridExtra","doParallel","foreach",
                     "scales","data.table","mapplots","repmis","sf",
                     "rnaturalearth", "rnaturalearthdata", "viridis", 
                     "tidyverse","rgdal", "writexl","ggpubr")

packages.to.use <- unique(packages.to.use)

for(package in packages.to.use) {
  print(package)
  if( ! package %in% rownames(installed.packages()) ) { install.packages(package ) }
  if( ! package %in% rownames(installed.packages()) ) { stop("Error on package instalation") }
  suppressWarnings( library(package, character.only = TRUE) )
}


## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------




