## TXM ----
### Monthly data to annual average ----

TXM_1980_1989 <- raster::stack()
for (z in 1980:1989){ ##1980-1989, 1996-2005, 2009-2018
  raster <- raster::calc(raster::subset(test, grep(paste0(z), names(test), value = T)), mean)
  TXM_1980_1989 <- raster::stack(TXM_1980_1989, raster)
}
names(TXM_1980_1989) <- paste0("Y_", seq(1980,1989, by = 1))

TXM_1996_2005 <- raster::stack()
for (z in 1996:2005){ ##1980-1989, 1996-2005, 2009-2018
  raster <- calc(raster::subset(test, grep(paste0(z), names(test), value = T)), mean)
  TXM_1996_2005 <- raster::stack(TXM_1996_2005, raster)
}
names(TXM_1996_2005) <- paste0("Y_", seq(1996, 2005, by = 1))

TXM_2009_2018 <- raster::stack()
for (z in 2009:2018){ ##1980-1989, 1996-2005, 2009-2018
  raster <- calc(raster::subset(test, grep(paste0(z), names(test), value = T)), mean)
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