# Clean and load packages ----
closeAllConnections()
rm(list=(ls()[ls()!="v"]))
gc(reset=TRUE)
source("Dependencies/Functions.R")

## Prepare transects data ----

transect <- readOGR("Data/TRANSECTS_2021_v2.kml")
Transects_with_elevations <- read_excel("Data/Transects_name_elevations.xlsx")
Transects_with_elevations <- Transects_with_elevations %>% 
  select(c(Name, Name_new, Alt, CODIGO, ZONE))

### Get the centroids ----
transect_centr <- gCentroid(transect, byid = TRUE)
transect_centr <- SpatialPointsDataFrame(transect_centr, data = transect@data)
transect_centr@data <- left_join(transect_centr@data, Transects_with_elevations, by = "Name")

### Select centroids of Aragon ----
library(mapSpain)
Aragon <- as_Spatial(esp_get_ccaa(epsg = "4326",
                                  ccaa = c("Aragon")))

### Same RS
proj4string(transect_centr)
proj4string(Aragon)

test <- crop(transect_centr, Aragon)
plot(test)

test@data <- cbind(test@data,coordinates(test))
shapefile(test,"ARAGON/TRANSECTS_ARAGON.shp")
