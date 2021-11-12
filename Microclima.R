# Clean and load packages ----
closeAllConnections()
rm(list=(ls()[ls()!="v"]))
gc(reset=TRUE)
source("Dependencies/Functions.R")
devtools::install_github('mrke/NicheMapR')

# Transects------
transect <- readOGR("Data/TRANSECTS_2021_v2.kml")
Transects_with_elevations <- read_excel("Data/Transects_name_elevations.xlsx")
Transects_with_elevations <- Transects_with_elevations %>% 
                             select(c(Name, Name_new, Alt, CODIGO, ZONE))

## Get the centroids ----
transect_centr <- gCentroid(transect, byid = TRUE)
transect_centr <- SpatialPointsDataFrame(transect_centr, data = transect@data)
transect_centr@data <- left_join(transect_centr@data, Transects_with_elevations, by = "Name")
CRS(transect_centr)
writeOGR(transect_centr,)

C_1x1KM <- readOGR("./Cuadriculas_seleccionadas.shp") %>%
  spTransform(CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))

UTM_ALT <- as.tibble(mutate(as.data.frame(C_1x1KM$COD5X5),as.data.frame(C_1x1KM$Shape_Area)))%>%
  rename(COD_UTM = `C_1x1KM$COD5X5`,
         AREA = `C_1x1KM$Shape_Area`)
nombre_UTM <- data.frame(UTM_ALT[,1])

centr <- st_as_sf(C_1x1KM) %>%
  st_centroid()%>%
  as_Spatial()%>%
  spTransform(CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))

lat_long <- coordinates(spTransform(centr, CRS("+proj=longlat + datum=WGS84")))
lat_comp <- lat_long[,2]
long_comp <- lat_long[,1]

fi <- seq(as.Date("2019-01-01"), length=120, by="month")
ff <- seq(as.Date("2019-02-01"), length=120, by="month")-1


f_inicio <- data.frame(fecha_mal = fi) %>% 
  separate(fecha_mal, into = c("dia", "mes", "a?o")) %>%
  mutate(fecha_bien = paste(a?o, mes, dia, sep = "/")) %>%
  dplyr::select(fecha_bien)

f_fin <- data.frame(fecha_mal = ff) %>% 
  separate(fecha_mal, into = c("dia", "mes", "a?o")) %>%
  mutate(fecha_bien = paste(a?o, mes, dia, sep = "/")) %>%
  dplyr::select(fecha_bien)

tic("Tiempo ejecuci?n total: ") 

for (j in 4){
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
    writeRaster(tmax, paste0("./tmax_",
                             gsub("/","_", paste0(f_inicio[i,],"_",f_fin[i,],"_",nombre_UTM[j,],"_H2",".tif"))), overwrite=TRUE)
    writeRaster(tmin, paste0("./tmin_",
                             gsub("/","_", paste0(f_inicio[i,],"_",f_fin[i,],"_",nombre_UTM[j,],"_H2",".tif"))), overwrite=TRUE)
    writeRaster(tmed, paste0("./tmed_",
                             gsub("/","_", paste0(f_inicio[i,],"_",f_fin[i,],"_",nombre_UTM[j,],"_H2",".tif"))), overwrite=TRUE)
  }
}
toc()

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
