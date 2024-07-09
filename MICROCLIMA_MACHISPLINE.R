# Clean and load packages ----
closeAllConnections()
rm(list=(ls()[ls()!="v"]))
gc(reset=TRUE)
source("Dependencies/Functions.R")


cent <- read.csv2("B:/CLIMA_DOWNSCALING/random_points_10km.txt")


lat_comp <- round(cent[,3], 2)
long_comp <- round(cent[,4], 2)

## Get dates ----
#####1980 1996 2009
fi <- seq(as.Date("2012-08-01"), length=1, by="month")
ff <- seq(as.Date("2012-09-01"), length=1, by="month")-1

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
#meridional 1
#gredos 7
#primavera 4

for (j in 1:length(lat_comp)){
  lat <- lat_comp[j]
  long <- long_comp[j]
  mdt <- microclima::get_dem(lat = lat, long = long, resolution = 30)
  for (i in 1:nrow(f_fin)){
    temp <- runauto(mdt,
                    f_inicio[i,], f_fin[i,], 
                    hgt = 1.5, 
                    l = NA, x = NA,
                    coastal = FALSE,
                    habitat = 7, ######## MODIFICAR #########
                    plot.progress = FALSE, save.memory = FALSE)
    tmax <- temp$tmax
    tmin <-temp$tmin
    tmed <- temp$tmean
    writeRaster(tmax, paste0("B:/CLIMA_DOWNSCALING/MICROCLIMA/tmax_", j, "_", gsub("/","_", substr(f_inicio[i,], 4,10)),".tif"))
    writeRaster(tmin, paste0("B:/CLIMA_DOWNSCALING/MICROCLIMA/tmin_", j, "_", gsub("/","_", substr(f_inicio[i,], 4,10)),".tif"))
    writeRaster(tmed, paste0("B:/CLIMA_DOWNSCALING/MICROCLIMA/tmed_", j, "_", gsub("/","_", substr(f_inicio[i,], 4,10)),".tif"))
  }
}
toc()

