# Clean and load packages ----
closeAllConnections()
rm(list=(ls()[ls()!="v"]))
gc(reset=TRUE)

source("Dependencies/Functions.R")

mask <- shapefile("Data/Peninsula_Iberica_mask.shp")
mask <- spTransform(mask, "+init=epsg:4326")
TERRACLIMATE_dwld_paths <- readLines("TERRACLIMATE_dwld_paths_srad.txt")
data_rep <- "B:/DATA/SOLAR/SPAIN/"

library(ncdf4)#:length(TERRACLIMATE_dwld_paths)
for (k in 1:2){
  download.file(TERRACLIMATE_dwld_paths[k],
                dest = "A:/CCMAR/TEST/raster.nc",
                mode="wb")
  
  nc_data <- nc_open("raster.nc")
  lon <- ncvar_get(nc_data, "lon")
  lat <- ncvar_get(nc_data, "lat", verbose = F)
  t <- ncvar_get(nc_data, "time")
  
  #solar_array <- ncvar_get(nc_data, "srad","long_name")
  solar_array <- ncvar_get(nc_data, attributes(nc_data$var)$names)
  solar_stack <-  raster::stack()
  
  for (i in 1:12){
    solar_array.slice <- solar_array[, , i] 
    r <- raster(t(solar_array.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
    r <- crop(r, mask)
    r <- raster::mask(r, mask)
    solar_stack <- raster::stack(solar_stack, r)
    
  }
  solar_stack <- calc(solar_stack, sum)
  writeRaster(solar_stack,  paste0(data_rep,
                                   str_sub(TERRACLIMATE_dwld_paths[k],
                                           unlist(gregexpr("srad_", TERRACLIMATE_dwld_paths[k])),
                                           unlist(gregexpr(".nc",TERRACLIMATE_dwld_paths[k])) - 1), ".tif"))
  nc_close(nc_data)
}
class(nc_data)
nc_close(nc_data)
