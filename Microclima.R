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
rm(transect)
rm(Transects_with_elevations)




lat_long <- coordinates(spTransform(transect_centr, CRS("+proj=longlat + datum=WGS84")))
lat_comp <- lat_long[,2]
long_comp <- lat_long[,1]

fi <- seq(as.Date("1980-01-01"), length=120, by="month")
ff <- seq(as.Date("1980-02-01"), length=120, by="month")-1


f_inicio <- data.frame(fecha_mal = fi) %>% 
  separate(fecha_mal, into = c("dia", "mes", "año")) %>%
  mutate(fecha_bien = paste(año, mes, dia, sep = "/")) %>%
  dplyr::select(fecha_bien)

f_fin <- data.frame(fecha_mal = ff) %>% 
  separate(fecha_mal, into = c("dia", "mes", "año")) %>%
  mutate(fecha_bien = paste(año, mes, dia, sep = "/")) %>%
  dplyr::select(fecha_bien)


tic("Tiempo ejecucion total: ") 

for (j in 1){
  lat <- lat_comp[j]
  long <- long_comp[j]
  mdt <- microclima::get_dem(lat = lat, long = long, resolution = 30)
  for (i in 1:nrow(f_fin)){
    temp <- runauto(mdt,
                    f_inicio[i,], f_fin[i,], 
                    hgt = 0.1, 
                    l = NA, x = NA,
                    coastal = FALSE,
                    habitat = 2, ######## MODIFICAR #########
                    plot.progress = FALSE, save.memory = FALSE)
    tmax <- temp$tmax
    tmin <-temp$tmin
    tmed <- temp$tmean
    writeRaster(tmax, paste0("B:/CHELSA_DATA/MICRO/tmax_",
                             gsub("/","_", paste0(f_inicio[i,],"_",f_fin[i,],"_","_H2",".tif"))), overwrite=TRUE)
    writeRaster(tmin, paste0("B:/CHELSA_DATA/MICRO/tmin_",
                             gsub("/","_", paste0(f_inicio[i,],"_",f_fin[i,],"_","_H2",".tif"))), overwrite=TRUE)
    writeRaster(tmed, paste0("B:/CHELSA_DATA/MICRO/tmed_",    
                             gsub("/","_", paste0(f_inicio[i,],"_",f_fin[i,],"_","_H2",".tif"))), overwrite=TRUE)
  }
}
toc()

"B:/CHELSA_DATA/chelsa_test"

raster <- raster::stack(list.files("B:/CHELSA_DATA/MICRO/", pattern = "tmed", full.names = TRUE))

transect_centr_test <- transect_centr 

transect_centr_test

for (i in c(1,2,20)){
  aa <- raster::extract(raster, transect_centr_test[i], buffer = NULL ,exact = TRUE)
  transect_centr_test@data <- cbind(transect_centr_test@data, aa)
}

view(transect_centr_test@data)
write_xlsx(transect_centr_test@data, "Results/test_microclima_transects_results.xlsx")


raster <- raster::stack(list.files("B:/CHELSA_DATA/chelsa_test", full.names = TRUE))

transect_centr_test <- transect_centr 

transect_centr_test

for (i in c(1,2,20)){
  aa <- raster::extract(raster, transect_centr_test[i], buffer = NULL ,exact = TRUE)
  transect_centr_test@data <- cbind(transect_centr_test@data, aa)
}

write_xlsx(transect_centr_test@data, "Results/test_chelsa_transects_results.xlsx")


plot(raster)
names(TMED) <- paste0("Y_", seq(1979, 2019, by = 1))

#--------------------------------------------
file.ls <- list.files("B:/CHELSA_DATA/MICRO/", pattern= "tmax", full.names = TRUE)
Filter(function(x) grepl(paste0(i), x), file.ls)
paste0("tmax*", i)
#--------------------------------------
for (j in 31:length(transect_centr)){
  lat <- lat_comp[j]
  long <- long_comp[j]
  mdt <- microclima::get_dem(lat = lat, long = long, resolution = 30)
  e <- extent(mdt)
  p <- as(e, 'SpatialPolygons') 
  projection(p) = CRS(proj4string(transect_centr))
  shapefile(p, paste0("B:/CHELSA_DATA/POLY/pol_Rascafria_Raso de la Cierva.shp" ))
}

proj4string(transect_centr) = CRS("+init=epsg:4326")
shapefile(transect_centr, paste0("B:/CHELSA_DATA/POLY/transect_centr.shp" ))


transect_centr$Name
###ESTADISTICAS

Nombres<- c("30TUK15B","30TVL11C","30TXK17D","30TXK64D")
temperaturas<- c("tmax", "tmin", "tmed")


setwd("A:/MCROCLIMA_TRANSECTOS/2010_2019")
for(k in temperaturas){
  for (j in Nombres){
    for (i in 10:12){
      lista_datos <- intersect(
        list.files(pattern = paste0(k,"_01_", i)), 
        list.files(pattern = j))
      dir.create(file.path(paste0("Statistics_", k)))
      brick_datos <- brick(stack(lista_datos))
      media <- calc(brick_datos,mean)
      
      std <- calc(brick_datos, sd)
      
      rango <- calc(brick_datos, function(x) max(x) - min(x))
      
      writeRaster(media, paste0("./Statistics_",k,"/media_",k,"_",i ,"_", j,".tif"),overwrite=TRUE)
      writeRaster(std, paste0("./Statistics_",k,"/std_",k,"_",i,"_", j,".tif"),overwrite=TRUE)
      writeRaster(rango, paste0("./Statistics_",k,"/rango_", k, "_",i,"_", j,".tif"),overwrite=TRUE)
    }
  }
  
}
