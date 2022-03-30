# load packages  
library(sf) 
library(ncdf4)
library(raster)
library(rasterVis)
library(RColorBrewer)

tmp_raster <- brick(list.files("B:/DATA/STEAD/DATOS_ORIGINALES", pattern = "tmax_pen.nc", full.names = TRUE))
names(tmp_raster) <- paste0("Y_",seq(as.Date("1971/1/1"), by = "day", length.out = 41638))
tail(names(tmp_raster))

## Daly to monthly
monthly_dates <- format(seq(as.Date("1971/1/1"), by = "month", length.out = 528),"%Y.%m")

tmp_monthly<- raster::stack()
for (i in monthly_dates){
  raster <- calc(raster::subset(tmp_raster, grep(paste0(i), names(tmp_raster), value = T)), mean) # MEAN
  tmp_monthly <- raster::stack(tmp_monthly, raster)
}
names(tmp_monthly) <- paste0("Y_",monthly_dates)


