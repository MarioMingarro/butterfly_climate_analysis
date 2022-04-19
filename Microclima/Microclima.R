# Clean and load packages ----
closeAllConnections()
rm(list=(ls()[ls()!="v"]))
gc(reset=TRUE)
source("Dependencies/Functions.R")

setwd("C:/GITHUB_REP/butterfly_climate_analysis")
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

cent <- read.csv2("./Data/cent_cuadriculas_micro.txt")

## Javalambre (DONE)
# lat_comp <- as.numeric(c("40.11", "40.15"))
# long_comp <- as.numeric(c("-1.02", "-1.03"))

lat_comp <- round(c(filter(cent, ZONE =="JAVALAMBRE")[,2]), 2)
long_comp <- round(c(filter(cent, ZONE =="JAVALAMBRE")[,1]), 2)

## Albarracin (DONE)

lat_comp <- round(c(filter(cent, ZONE =="ALBARRACIN")[,2]), 2)
long_comp <- round(c(filter(cent, ZONE =="ALBARRACIN")[,1]), 2)

## Meridional 
lat_comp <- round(c(filter(cent, ZONE =="MERIDIONAL")[,2]), 2)
long_comp <- round(c(filter(cent, ZONE =="MERIDIONAL")[,1]), 2)

## Guadarrama
lat_comp <- round(c(filter(cent, ZONE =="GUADARRAMA")[,2]), 2)
long_comp <- round(c(filter(cent, ZONE =="GUADARRAMA")[,1]), 2)

## Gredos RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR
lat_comp <- round(c(filter(cent, ZONE == "GREDOS")[,2]), 2)
long_comp <- round(c(filter(cent, ZONE =="GREDOS")[,1]), 2)

## Get dates ----
#####1980 1996 2009
fi <- seq(as.Date("2006-01-01"), length=36, by="month")
ff <- seq(as.Date("2006-02-01"), length=36, by="month")-1

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

for (j in 1:12){
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
    writeRaster(tmax, paste0("B:/CHELSA_DATA/GREDOS/TMAX/tmax_", j, "_", gsub("/","_", substr(f_inicio[i,], 4,10)),".tif"))
    writeRaster(tmin, paste0("B:/CHELSA_DATA/GREDOS/TMIN/tmin_", j, "_", gsub("/","_", substr(f_inicio[i,], 4,10)),".tif"))
    writeRaster(tmed, paste0("B:/CHELSA_DATA/GREDOS/TMED/tmed_", j, "_", gsub("/","_", substr(f_inicio[i,], 4,10)),".tif"))
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
