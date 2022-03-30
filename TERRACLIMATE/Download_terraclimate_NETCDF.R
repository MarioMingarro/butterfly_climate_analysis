# DOWNLOAD TERRACLIMATE

extension <- raster::stack("B:/CHELSA_DATA/TMAX/1979_02.tif")
for (i in 1958:2020){
  URL_TERRACLIMATE <- paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/TERRACLIMATE_ALL/data/TerraClimate_tmax_",i,".nc")
  URL_TERRACLIMATE <- brick(URL_TERRACLIMATE)
  URL_TERRACLIMATE <- crop(URL_TERRACLIMATE,extension)
  URL_TERRACLIMATE <- calc(URL_TERRACLIMATE, mean)
  writeRaster(URL_TERRACLIMATE, paste0("B:/CHELSA_DATA/TERRACLIMATE/TMAX/TMAX_", i),format="GTiff")
}

## Monthly to yearly ----
TMAX_TERRACLIMATE <- raster::stack()
for (i in 2000:2018){
  raster <- raster::stack(list.files("B:/CHELSA_DATA/TERRACLIMATE/TMAX", pattern = paste0(i), full.names = TRUE))
  TMAX_TERRACLIMATE <- raster::stack(TMAX_TERRACLIMATE, raster)
}
names(TMAX_TERRACLIMATE) <- paste0("Y_", seq(2000,2018, 1))
