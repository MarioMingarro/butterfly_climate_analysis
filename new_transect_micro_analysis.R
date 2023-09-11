library(raster)
library(tidyverse)

transect <- shapefile("B:/MICROCLIMA/All_transect_cent_WGS84.shp")
plot(transect)

names <- transect$Name
raster_names <- list.files("B:/MICROCLIMA/Datos/DRAGO/2021_2022", pattern = "tmax" , full.names = TRUE)

kk2 <- as.data.frame(matrix(nrow = 166, ncol = 1))

#c("01","02","03","04","05","06","07","08","09","10","11", "12"))
for (j in c("01","02","03","04","05","06","07","08","09","10","11", "12")){
  raster_names_2 <- raster_names[grepl(paste0("_",j,".*\\_2021"), raster_names)]
  salida <- as.data.frame(matrix(nrow = 166, ncol = 1))
  salida2 <- salida
for (i in 1:length(raster_names_2)){
  raster <- raster(raster_names_2[i])
  salida <- as.data.frame(raster::extract(raster, transect, buffer = NULL ,exact = TRUE))
  salida2 <- cbind(salida2, salida)
}
kk <- as.data.frame(rowMeans(salida2, na.rm = TRUE))
colnames(kk) <- paste0("Y_", substr(raster_names_2[1], start = 51, stop = 57))
kk2 <- cbind(kk2, kk)
}

kk2$V1 <- names
resultados <- kk2
resultados <- cbind(resultados, kk2)


writexl::write_xlsx(resultados, "B:/MICROCLIMA/Datos/DRAGO/RESULTADOS/1986.xlsx")

