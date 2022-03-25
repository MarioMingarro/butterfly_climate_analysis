# Clean and load packages ----
closeAllConnections()
rm(list=(ls()[ls()!="TMED"]))
gc(reset=TRUE)
source("Dependencies/Functions.R")

# Download MODIS data
library(remotes)
install_github("ropensci/MODIStsp")
library(MODIStsp)
MODIStsp()


TMED <- raster::stack()

for (i in 2000:2018){
  raster <- calc(raster::stack(list.files("B:/DATA/MODIS/Surf_Temp_8Days_1Km_v61/LST_Day_1km", pattern = paste0(2000), full.names = TRUE)), na.rm = TRUE, mean) # MEAN
  TMED <- raster::stack(TMED, raster)
}
names(TMED) <- paste0("Y_", seq(2000, 2018, by = 1))

TMED <- (TMED - 273.15) * 0.02 / 10

## Prepare transects data ----

transect <- readOGR("Data/TRANSECTS_2021_v2.kml")
Transects_with_elevations <- read_excel("Data/Transects_name_elevations.xlsx")
Transects_with_elevations <- Transects_with_elevations %>% 
  select(c(Name, Name_new, Alt, CODIGO, ZONE))

### Get the centroids ----
transect_centr <- gCentroid(transect, byid = TRUE)
transect_centr <- SpatialPointsDataFrame(transect_centr, data = transect@data)
transect_centr@data <- left_join(transect_centr@data, Transects_with_elevations, by = "Name")
# ---------------------

transect_centr_TMED <- transect_centr

proj4string(transect_centr_TMED)

projection(TMED)
rTrans = projectRaster(TMED, crs = "+proj=longlat +datum=WGS84 +no_defs")



pp <- raster::extract(TMED,
                      transect_centr_TMED, buffer = NULL ,exact = TRUE)
pp <- as.data.frame(pp)
pp$NAME <- transect_centr@data$Name_new
pp$ZONA <- transect_centr@data$ZONE

aa <- melt(pp)
ggplot(aa, aes(variable, value, group=aa$ZONA, color=aa$ZONA))+
  geom_point(alpha =0.2)+
  geom_smooth(aes(fill=aa$ZONA))+
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2")+
  scale_x_discrete(labels = seq(2000, 2018, 1))+
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(size = 6)
  )

#CHELSA
## TMED ----
### Monthly data to annual average ----
TMED_chelsa <- raster::stack()

for (i in 2000:2018){
  raster <- calc(raster::stack(list.files("B:/CHELSA_DATA/TMED", pattern = paste0(i), full.names = TRUE)), mean) # MEAN
  TMED_chelsa <- raster::stack(TMED_chelsa, raster)
}
names(TMED_chelsa) <- paste0("Y_", seq(2000, 2018, by = 1))


chelsa_2000_2018 <- raster::extract(TMED,
                      transect_centr_TMED, buffer = NULL ,exact = TRUE)
chelsa_2000_2018 <- as.data.frame(chelsa_2000_2018)
chelsa_2000_2018$NAME <- transect_centr@data$Name_new
chelsa_2000_2018$ZONA <- transect_centr@data$ZONE

bb <- melt(chelsa_2000_2018)
ggplot(bb, aes(variable, value, group=ZONA, color=ZONA))+
  geom_point(alpha =0.2)+
  geom_smooth(aes(fill=ZONA))+
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2")+
  scale_x_discrete(labels = seq(2000, 2018, 1))+
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(size = 6)
  )



kk <- brick(stack((list.files("B:/DATA/MODIS/Surf_Temp_8Days_1Km_v61/LST_Day_1km", pattern = paste0(2000), full.names = TRUE))))
plot(kk$MOD11A2_LST_Day_1km_2000_049)
kk$MOD11A2_LST_Day_1km_2000_049@data

#MICROCLIMA
a <- raster::stack(list.files("B:/CHELSA_DATA/JAVALAMBRE/TMAX", 
                              pattern = "x_1|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018" , full.names = TRUE))
