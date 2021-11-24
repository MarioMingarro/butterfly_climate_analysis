# Clean and load packages ----
closeAllConnections()
rm(list=(ls()[ls()!="v"]))
gc(reset=TRUE)
source("Dependencies/Functions.R")

# Working with data ----

## Prepare transects data ----

transect <- readOGR("Data/TRANSECTS_2021_v2.kml")
Transects_with_elevations <- read_excel("Data/Transects_name_elevations.xlsx")
Transects_with_elevations <- Transects_with_elevations %>% 
  select(c(Name, Name_new, Alt, CODIGO, ZONE))

### Get the centroids ----
transect_centr <- gCentroid(transect, byid = TRUE)
transect_centr <- SpatialPointsDataFrame(transect_centr, data = transect@data)
transect_centr@data <- left_join(transect_centr@data, Transects_with_elevations, by = "Name")

kk <- expand.grid(a=c("_1_"), b=seq(1980, 1989, 1))
list.files("B:/CHELSA_DATA/JAVALAMBRE/TMAX", pattern =  "_1_*.*1980", full.names = TRUE)



a <- raster::stack(list.files("B:/CHELSA_DATA/JAVALAMBRE/TMAX", pattern = "_1_" , full.names = TRUE))
b <- raster::stack(list.files("B:/CHELSA_DATA/JAVALAMBRE/TMAX", pattern = "_2_" , full.names = TRUE))
names(a[[300]])
names(b[[300]])
mos <- mosaic(a[[1]], b[[1]], fun = mean, tolerance=0.3)


test <- raster::stack()
for (i in 1:nlayers(a)){
  pp <- mosaic(a[[i]], b[[i]], fun = mean, tolerance=0.3)
  test <-  stack(test, pp)
}
names(test) <- names(a)


## TXM ----
### Monthly data to annual average ----
TXM <- raster::stack()
for (j in 2009:2018){ ##1980-1989, 1996-2005, 2009-2018
  raster <- calc(raster::subset(test, grep(paste0(j), names(test), value = T)), mean)
  TXM <- raster::stack(TXM, raster)
}
names(TXM) <- paste0("Y_", seq(2009,2018, by = 1))


### Select data for specific periods ----
TXM_1980_1989 <- raster::subset(TXM, grep(c("1980|1981|1982|1983|1984|1985|1986|1987|1988|1989"), names(TXM), value = T))

TXM_1996_2005 <- raster::subset(TXM, grep(c("1996|1997|1998|1999|2000|2001|2002|2003|2004|2005"), names(TXM), value = T))

TXM_2009_2018 <- raster::subset(TXM, grep(c("2009|2010|2011|2012|2013|2014|2015|2016|2017|2018"), names(TXM), value = T))

### Calculate mean a standard deviation for diferent periods ----
TXM_mean_1980_1989 <- calc(TXM_1980_1989, mean)
TXM_sd_1980_1989 <- calc(TXM_1980_1989, sd)

TXM_mean_1996_2005 <- calc(TXM_1996_2005, mean)
TXM_sd_1996_2005 <- calc(TXM_1996_2005, sd)

TXM_mean_2009_2018 <- calc(TXM_2009_2018, mean)
TXM_sd_2009_2018 <- calc(TXM_2009_2018, sd)


### Extract data for each centroid ----
transect_centr_TXM <- transect_centr

transect_centr_TXM$mean_1980_1989 <- raster::extract(TXM_mean_1980_1989,
                                                     transect_centr_TXM, buffer = NULL ,exact = TRUE)
transect_centr_TXM$sd_1980_1989 <- raster::extract(TXM_sd_1980_1989,
                                                   transect_centr_TXM, buffer = NULL ,exact = TRUE)
transect_centr_TXM$mean_1996_2005 <- raster::extract(TXM_mean_1996_2005,
                                                     transect_centr_TXM, buffer = NULL ,exact = TRUE)
transect_centr_TXM$sd_1996_2005 <- raster::extract(TXM_sd_1996_2005,
                                                   transect_centr_TXM, buffer = NULL ,exact = TRUE)
transect_centr_TXM$mean_2009_2018 <- raster::extract(TXM_mean_2009_2018,
                                                     transect_centr_TXM, buffer = NULL ,exact = TRUE)
transect_centr_TXM$sd_2009_2018 <- raster::extract(TXM_sd_2009_2018,
                                                   transect_centr_TXM, buffer = NULL ,exact = TRUE)

write_xlsx(transect_centr_TXM@data, "Results/Mean_Max_temp_transects_results.xlsx")
