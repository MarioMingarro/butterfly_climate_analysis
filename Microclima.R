# Clean and load packages ----
closeAllConnections()
rm(list=(ls()[ls()!="v"]))
gc(reset=TRUE)
source("Dependencies/Functions.R")


# Transects------
transect <- readOGR("Data/TRANSECTS_2021_v2.kml")
Transects_with_elevations <- read_excel("Data/Transects_name_elevations.xlsx")
Transects_with_elevations <- Transects_with_elevations %>% 
                             select(c(Name, Name_new, Alt, CODIGO, ZONE))

## Get the centroids ----
transect_centr <- gCentroid(transect, byid = TRUE)
transect_centr <- SpatialPointsDataFrame(transect_centr, data = transect@data)
transect_centr@data <- left_join(transect_centr@data, Transects_with_elevations, by = "Name")


## Get lat and long ----

lat_long <- coordinates(spTransform(transect_centr, CRS("+proj=longlat + datum=WGS84")))
lat_comp <- lat_long[,2]
long_comp <- lat_long[,1]

####

cent <- read.csv2("./cent_cuadriculas_micro.txt")

## Javalambre (DONE)
lat_comp <- as.numeric(c("40.11", "40.15"))
long_comp <- as.numeric(c("-1.02", "-1.03"))

lat_comp <- round(c(filter(cent, ZONE =="JAVALAMBRE")[,2]), 2)
long_comp <- round(c(filter(cent, ZONE =="JAVALAMBRE")[,1]), 2)

## Albarracin (DONE)

lat_comp <- round(c(filter(cent, ZONE =="ALBARRACIN")[,2]), 2)
long_comp <- round(c(filter(cent, ZONE =="ALBARRACIN")[,1]), 2)

## Meridional (DONE)
lat_comp <- round(c(filter(cent, ZONE =="MERIDIONAL")[,2]), 2)
long_comp <- round(c(filter(cent, ZONE =="MERIDIONAL")[,1]), 2)

## Guadarrama()
lat_comp <- round(c(filter(cent, ZONE =="GUADARRAMA")[,2]), 2)
long_comp <- round(c(filter(cent, ZONE =="GUADARRAMA")[,1]), 2)

## Get dates ----
#####1980 1996 2009
fi <- seq(as.Date("2009-01-01"), length=120, by="month")
ff <- seq(as.Date("2009-02-01"), length=120, by="month")-1


f_inicio <- data.frame(fecha_mal = fi) %>% 
  separate(fecha_mal, into = c("dia", "mes", "año")) %>%
  mutate(fecha_bien = paste(año, mes, dia, sep = "/")) %>%
  dplyr::select(fecha_bien)


f_fin <- data.frame(fecha_mal = ff) %>% 
  separate(fecha_mal, into = c("dia", "mes", "año")) %>%
  mutate(fecha_bien = paste(año, mes, dia, sep = "/")) %>%
  dplyr::select(fecha_bien)

## Run microclima ----

tic("Tiempo ejecucion total: ") 

for (j in 8:10){
  lat <- lat_comp[j]
  long <- long_comp[j]
  mdt <- microclima::get_dem(lat = lat, long = long, resolution = 30)
  for (i in 1:nrow(f_fin)){
    temp <- runauto(mdt,
                    f_inicio[i,], f_fin[i,], 
                    hgt = 0.1, 
                    l = NA, x = NA,
                    coastal = FALSE,
                    habitat = 7, ######## MODIFICAR #########
                    plot.progress = FALSE, save.memory = FALSE)
    tmax <- temp$tmax
    tmin <-temp$tmin
    tmed <- temp$tmean
    writeRaster(tmax, paste0("B:/CHELSA_DATA/GUADARRAMA/TMAX/tmax_", j, "_", gsub("/","_", substr(f_inicio[i,], 4,10)),".tif"))
    writeRaster(tmin, paste0("B:/CHELSA_DATA/GUADARRAMA/TMIN/tmin_", j, "_", gsub("/","_", substr(f_inicio[i,], 4,10)),".tif"))
    writeRaster(tmed, paste0("B:/CHELSA_DATA/GUADARRAMA/TMED/tmed_", j, "_", gsub("/","_", substr(f_inicio[i,], 4,10)),".tif"))
  }
}
toc()


#number                         descriptor
# 1        Evergreen needleleaf forest
# 2         Evergreen Broadleaf forest
# 3        Deciduous needleleaf forest
# 4         Deciduous broadleaf forest
# 5                       Mixed forest
# 6                  Closed shrublands
# 7                    Open shrublands
# 8                     Woody savannas
# 9                           Savannas
#10                   Short grasslands
#11                    Tall grasslands
#12                 Permanent wetlands
#13                          Croplands
#14                 Urban and built-up
#15 Cropland/Natural vegetation mosaic
#16       Barren or sparsely vegetated
#17                         Open water

##--------------------------------------------------------------------------------------------------------------------------------------
aa <- raster::stack(list.files("B:/CHELSA_DATA/MICRO/TMAX", pattern = "01_1980", full.names = TRUE))
aa <- as(extent(aa), "SpatialPolygons")
proj4string(aa) <-  CRS("+proj=longlat +datum=WGS84 +no_defs")
shapefile(aa, "B:/CHELSA_DATA/MICRO/ext.shp")
##MICROCLIMA

TMED_m <- raster::stack()
for (i in 1980:1989){
  raster <- calc(raster::stack(list.files("B:/CHELSA_DATA/MICRO/TMAX", pattern = paste0(i), full.names = TRUE)), mean) # MEAN
  TMED_m <- raster::stack(TMED_m, raster)
}
names(TMED_m) <- paste0("Y_", seq(1980, 1989, by = 1))

TMED_mean_1980_1989_m <- calc(TMED_m, mean)

TMED_mean_1980_1989_m <-  projectRaster(TMED_mean_1980_1989_m, crs = projection(TMED_mean_1980_1989))




##CHELSA
TMED <- raster::stack()
for (i in 1980:1989){
  raster <- calc(raster::stack(list.files("B:/CHELSA_DATA/TMAX", pattern = paste0(i), full.names = TRUE)), mean) # MEAN
  TMED <- raster::stack(TMED, raster)
}
names(TMED) <- paste0("Y_", seq(1980, 1989, by = 1))

TMED_mean_1980_1989 <- calc(TMED, mean)

TMED_mean_1980_1989 <- crop(TMED_mean_1980_1989,TMED_mean_1980_1989_m)


plot(TMED_mean_1980_1989)
plot(transect, add=T)
plot(TMED_mean_1980_1989_m)
plot(transect, add=T)

summary(TMED_mean_1980_1989_m)
summary(TMED_mean_1980_1989)



##MICROCLIMA

raster_m <- raster::stack(list.files("B:/CHELSA_DATA/MICRO/TMIN", full.names = TRUE))
raster_m <-  projectRaster(raster_m, crs = "+proj=longlat +datum=WGS84 +no_defs")


##CHELSA
raster_c <- raster::stack()
for (i in 1980:1989){
  raster <- raster::stack(list.files("B:/CHELSA_DATA/TMIN", pattern = paste0(i), full.names = TRUE))
  raster_c <- raster::stack(raster_c, raster)
}

raster_c <- crop(raster_c,raster_m[[1]])

##Comparation
ext <- as(extent(raster_m[[1]]), "SpatialPolygons")
random_points <- spsample(ext, n=5, type='random')
proj4string(random_points) = CRS("+proj=longlat +datum=WGS84 +no_defs")

m_results <- raster::extract(raster_m,
                              random_points,
                              df=TRUE)
c_results <- raster::extract(raster_c,
                             random_points,
                             df=TRUE)

write_xlsx(m_results, "Results/test_MICRO_MIN.xlsx")
write_xlsx(c_results, "Results/test_CHELSA_MIN.xlsx")



##CHELSA OLD
raster_co <- raster::stack()
for (i in 1980:1989){
  raster <- raster::stack(list.files("B:/DATA/CHELSA/SPAIN/TMAX", pattern = paste0(i), full.names = TRUE))
  raster_co <- raster::stack(raster_co, raster)
}

raster_co <- crop(raster_co,raster_m[[1]])
raster_co <- raster_co/10
plot(raster_c[[1]])
plot(raster_co[[1]])
##Comparation


co_results <- raster::extract(raster_co,
                             random_points,
                             df=TRUE)

write_xlsx(co_results, "Results/test_chelsa_o.xlsx")

