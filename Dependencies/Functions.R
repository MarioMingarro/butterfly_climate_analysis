## Dependencies

packages.to.use <- c("tictoc", "raster", "devtools", "VoCC","rgeos",
                     "rasterVis","gridExtra","doParallel","foreach",
                     "scales","data.table","mapplots","repmis","sf",
                     "rnaturalearth", "rnaturalearthdata", "viridis", 
                     "tidyverse","rgdal", "writexl","ggpubr", "diffeR", 
                     "readxl",  "NicheMapR", "microclima", "rgdal","sf","magrittr",
                     "tidyverse","tmap",  "lubridate", "PupillometryR")

packages.to.use <- unique(packages.to.use)

for(package in packages.to.use) {
  print(package)
  if( ! package %in% rownames(installed.packages()) ) { install.packages(package ) }
  if( ! package %in% rownames(installed.packages()) ) { stop("Error on package instalation") }
  suppressWarnings( library(package, character.only = TRUE) )
}


## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------




