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
