# ------------------

rm(list=(ls()[ls()!="v"]))
gc(reset=TRUE)
source("Dependencies/Functions.R")


#Download CHELSA dataset 
# ------------------
#Monthly dataset of precipitation, maximum-, minimum-, and mean temperatures at 30 arc sec resolution for the earths land surface areas.
#There are separate files for each month starting January 1979.

# https://envicloud.wsl.ch/#/?prefix=chelsa%2Fchelsa_V2%2FGLOBAL%2Fmonthly%2F


CHELSA_dwld_paths <- readLines("CHELSA_dwld_paths.txt")
mask <- shapefile("Data/Peninsula_Iberica_mask.shp")
mask <- spTransform(mask, "+init=epsg:4326")



data_rep <- "B:/CHELSA_DATA/" 

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
                             unlist(gregexpr("tas_", CHELSA_dwld_paths[i])),
                             unlist(gregexpr("_V.2", CHELSA_dwld_paths[i])) - 1), ".tif"))
}

file.rename(paste0("B:/CHELSA_DATA/",list.files("B:/CHELSA_DATA")),
            paste0("B:/CHELSA_DATA/",
                   str_sub(list.files("B:/CHELSA_DATA"), 8,11),
                   "_",
                   str_sub(list.files("B:/CHELSA_DATA"), 5,6),
                   ".tif"
            ))


## Climatic Velocity
# ------------------


datos <- raster::stack(list.files("B:/CHELSA_DATA", full.names = TRUE))


# monthly to annual averages
r <- sumSeries(datos, p = "1979-02/2019-12", yr0 = "1979-02-01", l = nlayers(datos), 
               fun = function(x) colMeans(x, na.rm = TRUE), freqin = "months", freqout = "years")
# temporal trend
vt <- tempTrend(r, th = 10)
# spatial gradient
vg <- spatGrad(r, th = 0.0001, projected = FALSE)
# climate velocity
gv <- gVoCC(vt, vg)

plot(vel)
plot(vg)
plot(vt)

vel <- gv[[1]] # Velocity
ang <- gv[[2]] # Angle
mn <- calc(r, mean, na.rm = T) # All period Temperature average

# Create a data frame with the centroid for the trajectories and associated input data
lonlat <- data.frame(xyFromCell(vel, 1:ncell(vel)))
lonlat$vel <- raster::extract(vel, lonlat)
lonlat$ang <- raster::extract(ang, lonlat[,1:2])
lonlat$mn <- raster::extract(mn, lonlat[,1:2])
lonlat <- na.omit(lonlat)


# Calculate the trajectories with parallel processing
cores <-  detectCores()
ncores<- cores[1]-2 
cuts <- cut(1:nrow(lonlat), ncores)
cl <- makeCluster(ncores)
tic("Execution time: ")
registerDoParallel(cl)
traj <- foreach(x = levels(cuts), 
                .combine = rbind, 
                .packages = c('raster','sp','rgeos','geosphere','rgdal','VoCC'), 
                .multicombine = TRUE) %dopar% {
                  voccTraj(lonlat[cuts == x,], 
                           vel, 
                           ang, 
                           mn, 
                           tyr = 47, 
                           trajID = as.numeric(rownames(lonlat[cuts == x,])), correct = TRUE)}

stopCluster(cl)

toc()

plot(traj)

tic("Execution time: ")
traj_cl <- trajClas(traj, vel, ang, mn, trajSt = 4, tyr = 47, nmL = 20, smL = 50, Nend = 40, Nst = 15, NFT = 70)
toc()
#The trajectory classes ("TrajClas") 
#(1) non-moving, (2) slow-moving, (3) internal sinks, 
#(4) boundary sinks, (5) sources, (6) relative sinks, 
#(7) corridors, (8) divergence and (9) convergence.
# ------------------
TrajClas <- traj_cl$TrajClas

TrajClas_c <- crop(TrajClas, 
                   extent(x_min, x_max, y_min, y_max))






nombres <- c("Pto. Navacerrada (Segovia)"   ,
             "Valsain II"                   ,
             "HOYOCASERO"                   ,
             "NAVACEPEDA DE TORMES"         ,
             "NAVALGUIJO"                   ,
             "NAVALPERAL DE TORMES"         ,
             "NAVARREDONDA DE GREDOS"       ,
             "PUERTO DE LA PEÑA NEGRA"     ,
             "PLATAFORMA DE GREDOS"         ,
             "PUERTO DEL PICO"              ,
             "EL HORNILLO"                  ,
             "CANDELEDA"                    ,
             "CERRO DEL AGUILA"             ,
             "COLLADO DE LA CENTENERA"      ,
             "MINGO FERNANDO"               ,
             "SOTO DE EL ARENAL"            ,
             "Arroyo (Pto. Morcuera)"       ,
             "Moralzarzal"                  ,
             "Puerto Fuenfria"              ,
             "Valsain I"                    ,
             "La Granja - Pto. Navacerrada" ,
             "Embalse del Ponton"           ,
             "La Fonda Real"                ,
             "Las Vueltas de la Barranca"   ,
             "La Bola del Mundo"            ,
             "Fuente de la Campanilla"      ,
             "Cerro Piñonero"              ,
             "Mirador Cabeza Lijar"         ,
             "Collado de la Mina"           ,
             "Rascafria / Raso de la Cierva",
             "Raso de la Cierva"            ,
             "Las Calderuelas"              ,
             "La Flecha"                    ,
             "Ermita de Santa Ana"          ,
             "Collado Najarra"              ,
             "Collado Valdemartin"          ,
             "Angostura 2 - Aparcamiento"   ,
             "Pista mantenimiento Valdesqui",
             "Pradera de Majalasna"         ,
             "Alto del Leon"                ,
             "Pradera de Navarrulaque"      ,
             "Los pozuelos - Cañada Real"  ,
             "La Angostura"                 ,
             "Camino de las Encinillas 1"   ,
             "Restaurante La Isla"          ,
             "Gea de Albarracín"         ,
             "Cuenca"                       ,
             "Valdecuenca"                  ,
             "Las Majadas"                  ,
             "Uña"                         ,
             "Moscardón"                   ,
             "Tragacete"                    ,
             "Griegos"                      ,
             "Tramacastilla"                ,
             "Villar del Cobo"              ,
             "Monterde de Albarracín"    ,
             "Cella"                        ,
             "Masegosa"                     ,
             "Vega del Codorno"             ,
             "Nacimiento del Rio Cuervo"    ,
             "Nacimiento del Rio Tajo"      ,
             "El Recuenco"                  ,
             "Pinilla de Molina"            ,
             "Salmerón"                    ,
             "Torremocha del Pinar"         ,
             "JAV1"                         ,
             "JAV2"                         ,
             "JAV3"                         ,
             "JAV4"                         ,
             "JAV5"                         ,
             "JAV6"                         ,
             "JAV7"                         ,
             "JAV8"                         ,
             "JAV9"                         ,
             "JAV10"                        ,
             "CEBEDILLA"                    ,
             "PRADO DE LAS POZAS"           ,
             "Collado La Gasca"             )


zona <- c("GUA",
          "GUA",
          "GRE",
          "GRE",
          "GRE",
          "GRE",
          "GRE",
          "GRE",
          "GRE",
          "GRE",
          "GRE",
          "GRE",
          "GRE",
          "GRE",
          "GRE",
          "GRE",
          "GUA",
          "GUA",
          "GUA",
          "GUA",
          "GUA",
          "GUA",
          "GUA",
          "GUA",
          "GUA",
          "GUA",
          "GUA",
          "GUA",
          "GUA",
          "GUA",
          "GUA",
          "GUA",
          "GUA",
          "GUA",
          "GUA",
          "GUA",
          "GUA",
          "GUA",
          "GUA",
          "GUA",
          "GUA",
          "GUA",
          "GUA",
          "GUA",
          "GUA",
          "SM",
          "SM",
          "SM",
          "SM",
          "SM",
          "SM",
          "SM",
          "SM",
          "SM",
          "SM",
          "SM",
          "SM",
          "SM",
          "SM",
          "SM",
          "SM",
          "SM",
          "SM",
          "SM",
          "SM",
          "JA",
          "JA",
          "JA",
          "JA",
          "JA",
          "JA",
          "JA",
          "JA",
          "JA",
          "JA",
          "GRE",
          "GRE",
          "GUA")