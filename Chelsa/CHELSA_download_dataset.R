
# Clean and load packages ----
closeAllConnections()
rm(list=(ls()[ls()!="v"]))
gc(reset=TRUE)
source("Dependencies/Functions.R")



# Download CHELSA dataset ----

# Monthly dataset of precipitation, maximum-, minimum-, and mean temperatures at 30 arc sec resolution for the earths land surface areas.
# There are separate files for each month starting January 1979.

# https://envicloud.wsl.ch/#/?prefix=chelsa%2Fchelsa_V2%2FGLOBAL%2Fmonthly%2F


## Mean temperature ----

# Load .txt with the entire directory which download
CHELSA_dwld_paths <- readLines("Data/CHELSA_dwld_paths_tmed.txt")

# Load and reproject iberian peninsula mask
mask <- shapefile("Data/Peninsula_Iberica_mask.shp")
mask <- spTransform(mask, "+init=epsg:4326")

# Directory to save all downloaded files
data_rep <- "B:/CHELSA_DATA/TMED/" 

# Loop to download all files
for (i in 1:length(CHELSA_dwld_paths)){
  download.file(CHELSA_dwld_paths[i],
                dest = "raster.tif",
                mode="wb")
  raster <- raster("raster.tif")
  raster <- raster %>%
    crop(mask) %>%
    mask(mask)
  raster <- raster/10
  raster <- raster - 273.15
  
  writeRaster(raster,
              paste0(data_rep,
                     str_sub(CHELSA_dwld_paths[i],
                             unlist(gregexpr("tas_", CHELSA_dwld_paths[i])),
                             unlist(gregexpr("_V.2", CHELSA_dwld_paths[i])) - 1), ".tif"))
}

# Rename all files from "tas_01_1979" to "1979_01"
file.rename(paste0("B:/CHELSA_DATA/TMED/",list.files("B:/CHELSA_DATA/TMED")),
            paste0("B:/CHELSA_DATA/TMED/",
                   str_sub(list.files("B:/CHELSA_DATA/TMED"), 8,11),
                   "_",
                   str_sub(list.files("B:/CHELSA_DATA/TMED"), 5,6),
                   ".tif"
            ))



## Monthly precipitation ----

# Same steps as mean temperature (above)
CHELSA_dwld_paths <- readLines("Data/CHELSA_dwld_paths_pcp.txt")

data_rep <- "B:/CHELSA_DATA/PCP/" 

for (i in 1:length(CHELSA_dwld_paths)){
  download.file(CHELSA_dwld_paths[i],
                dest = "raster.tif",
                mode="wb")
  raster <- raster("raster.tif")
  raster <- raster %>%
    crop(mask) %>%
    mask(mask)
  raster <- raster/100
  
  writeRaster(raster,
              paste0(data_rep,
                     str_sub(CHELSA_dwld_paths[i],
                             unlist(gregexpr("pr_", CHELSA_dwld_paths[i])),
                             unlist(gregexpr("_V.2", CHELSA_dwld_paths[i])) - 1), ".tif"))
}

file.rename(paste0("B:/CHELSA_DATA/PCP/",list.files("B:/CHELSA_DATA/PCP")),
            paste0("B:/CHELSA_DATA/PCP/",
                   str_sub(list.files("B:/CHELSA_DATA/PCP"), 7,10),
                   "_",
                   str_sub(list.files("B:/CHELSA_DATA/PCP"), 4,5),
                   ".tif"
            ))




## Maximum temperature ----

# Load .txt with the entire directory which download
CHELSA_dwld_paths <- readLines("Data/CHELSA_dwld_paths_tmax.txt")

# Load and reproject iberian peninsula mask
mask <- shapefile("Data/Peninsula_Iberica_mask.shp")
mask <- spTransform(mask, "+init=epsg:4326")

# Directory to save all downloaded files
data_rep <- "B:/CHELSA_DATA/TMAX/" 

# Loop to download all files 
for (i in 1:length(CHELSA_dwld_paths)){
  download.file(CHELSA_dwld_paths[i],
                dest = "raster.tif",
                mode="wb")
  raster <- raster("raster.tif")
  raster <- raster %>%
    crop(mask) %>%
    mask(mask)
  raster <- raster/10
  raster <- raster - 273.15
  
  writeRaster(raster,
              paste0(data_rep,
                     str_sub(CHELSA_dwld_paths[i],
                             unlist(gregexpr("max_", CHELSA_dwld_paths[i])),
                             unlist(gregexpr("_V.2", CHELSA_dwld_paths[i])) - 1), ".tif"))
}

# Rename all files from "tas_01_1979" to "1979_01"
file.rename(paste0("B:/CHELSA_DATA/TMAX/",list.files("B:/CHELSA_DATA/TMAX")),
            paste0("B:/CHELSA_DATA/TMAX/",
                   str_sub(list.files("B:/CHELSA_DATA/TMAX"), 8,11),
                   "_",
                   str_sub(list.files("B:/CHELSA_DATA/TMAX"), 5,6),
                   ".tif"
            ))



## Minimum temperature ----

# Load .txt with the entire directory which download
CHELSA_dwld_paths <- readLines("Data/CHELSA_dwld_paths_tmin.txt")

# Load and reproject iberian peninsula mask
mask <- shapefile("Data/Peninsula_Iberica_mask.shp")
mask <- spTransform(mask, "+init=epsg:4326")

# Directory to save all downloaded files
data_rep <- "B:/CHELSA_DATA/TMIN/" 

# Loop to download all files 
for (i in 1:length(CHELSA_dwld_paths)){
  download.file(CHELSA_dwld_paths[i],
                dest = "raster.tif",
                mode="wb")
  raster <- raster("raster.tif")
  raster <- raster %>%
    crop(mask) %>%
    mask(mask)
  raster <- raster/10
  raster <- raster - 273.15
  
  writeRaster(raster,
              paste0(data_rep,
                     str_sub(CHELSA_dwld_paths[i],
                             unlist(gregexpr("min_", CHELSA_dwld_paths[i])),
                             unlist(gregexpr("_V.2", CHELSA_dwld_paths[i])) - 1), ".tif"))
}

# Rename all files from "tas_01_1979" to "1979_01"
file.rename(paste0("B:/CHELSA_DATA/TMIN/",list.files("B:/CHELSA_DATA/TMIN")),
            paste0("B:/CHELSA_DATA/TMIN/",
                   str_sub(list.files("B:/CHELSA_DATA/TMIN"), 8,11),
                   "_",
                   str_sub(list.files("B:/CHELSA_DATA/TMIN"), 5,6),
                   ".tif"
            ))





