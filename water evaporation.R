library(ncdf4)
nc_data <- nc_open("B:/DATA/SOLAR/agg_terraclimate_aet_1958_CurrentYear_GLOBE.nc")
lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)
t <- ncvar_get(nc_data, "time")

print(nc_data)

kk <- ncvar_get(nc_data, attributes(nc_data$var)$names)



dim(kk)

kk.slice <- kk[, , 756] 
r <- raster(t(kk.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
#r <- flip(r, direction='y')
plot(r)
res(r)
view(t)


TERRACLIMATE_dwld_paths <- readLines("TERRACLIMATE_dwld_paths_srad.txt")
# Load and reproject iberian peninsula mask
mask <- shapefile("Data/Peninsula_Iberica_mask.shp")
mask <- spTransform(mask, "+init=epsg:4326")

# Directory to save all downloaded files
data_rep <- "B:/CHELSA_DATA/TMED/" 

# Loop to download all files
for (i in 1:length(CHELSA_dwld_paths)){
  download.file(CHELSA_dwld_paths[i],
                dest = "raster.tif",
                mode="wb")
  raster <- raster("kk.nc")
  raster <- raster %>%
    crop(mask) %>%
    mask(mask)
  writeRaster(raster,
              paste0(data_rep,
                     str_sub(CHELSA_dwld_paths[i],
                             unlist(gregexpr("tas_", CHELSA_dwld_paths[i])),
                             unlist(gregexpr("_V.2", CHELSA_dwld_paths[i])) - 1), ".tif"))
}

download.file(TERRACLIMATE_dwld_paths[1],
             dest = "kk.nc",
             mode="wb")
raster <- raster("raster.tif")
plot(raster[[1]])
projection(raster) <-  CRS("+init=epsg:4326")

nc_data <- nc_open("kk.nc")
lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)
t <- ncvar_get(nc_data, "time")

print(nc_data)

kk <- ncvar_get(nc_data, attributes(nc_data$var)$names)



dim(kk)

kk.slice <- kk[, , 12] 
r <- raster(t(kk.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
plot(r[[2]])
