# Clean and load packages ----
closeAllConnections()
rm(list=(ls()[ls()!="TMED"]))
gc(reset=TRUE)
source("Dependencies/Functions.R")

library(remotes)
install_github("ropensci/MODIStsp")
library(MODIStsp)
MODIStsp()

library(raster)
TMED <- raster::stack()

for (i in 2000:2018){
  raster <- calc(raster::stack(list.files("B:/DATA/MODIS/Surf_Temp_8Days_1Km_v61/LST_Day_1km", pattern = paste0(i), full.names = TRUE)), na.rm = TRUE, mean) # MEAN
  TMED <- raster::stack(TMED, raster)
}
names(TMED) <- paste0("Y_", seq(2000, 2018, by = 1))

TMED <- (TMED - 273.15) * 0.02 / 10
# ---------------------
plot(TMED[[1]])

plot(k)
## Prepare transects data ----

transect <- readOGR("Data/TRANSECTS_2021_v2.kml")
Transects_with_elevations <- read_excel("Data/Transects_name_elevations.xlsx")
Transects_with_elevations <- Transects_with_elevations %>% 
  select(c(Name, Name_new, Alt, CODIGO, ZONE))

### Get the centroids ----
transect_centr <- gCentroid(transect, byid = TRUE)
transect_centr <- SpatialPointsDataFrame(transect_centr, data = transect@data)
transect_centr@data <- left_join(transect_centr@data, Transects_with_elevations, by = "Name")
