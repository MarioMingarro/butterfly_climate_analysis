library(raster)
library(rgdal)
library(readxl)
library(tidyverse)
library(sf)

## Prepare transects data ----
rm(list=(ls()[ls()!="v"]))

Transects_with_elevations <- read_excel("Data/Transects_name_elevations.xlsx")
Transects_with_elevations <- Transects_with_elevations %>% 
  select(c(Name, Name_new, Alt, CODIGO, ZONE))


transect_buf@data <- left_join(transect_buf@data, Transects_with_elevations, by = "Name")



###---------------------

transect = spTransform(readOGR("Data/TRANSECTS_2021_v2.kml"), CRS("+init=epsg:25830"))

transect <- st_as_sf(transect, crs = 25830)


transect_buf <- vector("list", nrow(transect))
for (i in 1:nrow(transect)){
  a <- st_buffer(transect$geometry[i],10)
  a$Name = transect$Name[i]
  transect_buf[[i]] <- a
}
transect_buf <- do.call("rbind", transect_buf)
transect_buf <- as.data.frame(transect_buf)
names(transect_buf) <- c("geometry", "Name") 

transect_buf <- st_as_sf(transect_buf, crs = 25830)

slope <- raster("B:/DATA/MDT/hillsade.tif")


plot(slope
     )
kk <- mask(slope, transect_buf[1,2])

slope_transect = aggregate(slope, transect, mean, na.rm = TRUE)

plot(kk)

####-------------------
plot(transect_buf[1,2])

plot(kk)
