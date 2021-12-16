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


#list.files("B:/CHELSA_DATA/JAVALAMBRE/TMAX", pattern =  "_1_*.*1980", full.names = TRUE)


a <- raster::stack(list.files("B:/CHELSA_DATA/ALBARRACIN/TMAX", pattern = "_1_" , full.names = TRUE))
b <- raster::stack(list.files("B:/CHELSA_DATA/ALBARRACIN/TMAX", pattern = "_2_" , full.names = TRUE))
c <- raster::stack(list.files("B:/CHELSA_DATA/ALBARRACIN/TMAX", pattern = "_3_" , full.names = TRUE))
d <- raster::stack(list.files("B:/CHELSA_DATA/ALBARRACIN/TMAX", pattern = "_4_" , full.names = TRUE))
e <- raster::stack(list.files("B:/CHELSA_DATA/ALBARRACIN/TMAX", pattern = "_5_" , full.names = TRUE))
f <- raster::stack(list.files("B:/CHELSA_DATA/ALBARRACIN/TMAX", pattern = "_6_" , full.names = TRUE))
g <- raster::stack(list.files("B:/CHELSA_DATA/ALBARRACIN/TMAX", pattern = "_7_" , full.names = TRUE))
h <- raster::stack(list.files("B:/CHELSA_DATA/ALBARRACIN/TMAX", pattern = "_8_" , full.names = TRUE))


names(h[[60]])
names(h[[1]])


test <- raster::stack()
for (i in 1:nlayers(a)){
  pp <- mosaic(a[[i]],
               b[[i]],
               c[[i]],
               d[[i]],
               e[[i]],
               f[[i]],
               g[[i]],
               h[[i]], fun = mean, tolerance=1)
  test <-  stack(test, pp)
}
names(test) <- names(a)


## TXM ----
### Monthly data to annual average ----

TXM_1980_1989 <- raster::stack()
for (j in 1980:1989){ ##1980-1989, 1996-2005, 2009-2018
  raster <- calc(raster::subset(test, grep(paste0(j), names(test), value = T)), mean)
  TXM_1980_1989 <- raster::stack(TXM_1980_1989, raster)
}
names(TXM_1980_1989) <- paste0("Y_", seq(1980,1989, by = 1))

TXM_1996_2005 <- raster::stack()
for (j in 1996:2005){ ##1980-1989, 1996-2005, 2009-2018
  raster <- calc(raster::subset(test, grep(paste0(j), names(test), value = T)), mean)
  TXM_1996_2005 <- raster::stack(TXM_1996_2005, raster)
}
names(TXM_1996_2005) <- paste0("Y_", seq(1996, 2005, by = 1))

TXM_2009_2018 <- raster::stack()
for (j in 2009:2018){ ##1980-1989, 1996-2005, 2009-2018
  raster <- calc(raster::subset(test, grep(paste0(j), names(test), value = T)), mean)
  TXM_2009_2018 <- raster::stack(TXM_2009_2018, raster)
}
names(TXM_2009_2018) <- paste0("Y_", seq(2009,2018, by = 1))


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

write_xlsx(transect_centr_TXM@data, "Results/Mean_Max_temp_transects_albarracin_results.xlsx")



kk<- melt(transect_centr_TXM@data)
nn <- na.omit(filter(kk, kk$variable == "mean_1980_1989" | kk$variable == "mean_2009_2018" ))

ggplot(nn, aes(x= variable, y =value, fill=variable))+
  geom_boxplot()+
  theme_cleveland()
