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

#JAVALAMBRE
a <- raster::stack(list.files("B:/CHELSA_DATA/JAVALAMBRE/TMAX", pattern = "x_1_" , full.names = TRUE))
b <- raster::stack(list.files("B:/CHELSA_DATA/JAVALAMBRE/TMAX", pattern = "x_2_" , full.names = TRUE))


test <- raster::stack()
for (s in 1:nlayers(a)){
  pp <- mosaic(a[[s]],
               b[[s]],
               fun = mean, tolerance=10000)
  test <-  stack(test, pp)
}
names(test) <- names(a)

source("TMM.R")

kk<- melt(transect_centr_TXM@data)
nn <- na.omit(filter(kk, kk$variable == "mean_1980_1989" | kk$variable == "mean_2009_2018" ))

library(PupillometryR)
javalambre <- ggplot(nn, aes(x= variable, y =value, fill=variable))+
  geom_flat_violin(aes(fill = variable),position = position_nudge(x = .1, y = 0), trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = variable, y = value, colour = variable),position = position_jitter(width = .2), size = 2, shape = 20)+
  geom_boxplot(aes(x= variable, y =value, fill=variable),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  scale_y_continuous("ºC",c(10, 12.5, 15, 17.5, 20,22.5,25,27.5,30), limits = c(12,30))+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("Javalambre")+
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank()
  )

#ALBARRACIN
a <- raster::stack(list.files("B:/CHELSA_DATA/ALBARRACIN/TMAX", pattern = "x_1_" , full.names = TRUE))
b <- raster::stack(list.files("B:/CHELSA_DATA/ALBARRACIN/TMAX", pattern = "x_2_" , full.names = TRUE))
c <- raster::stack(list.files("B:/CHELSA_DATA/ALBARRACIN/TMAX", pattern = "x_3_" , full.names = TRUE))
d <- raster::stack(list.files("B:/CHELSA_DATA/ALBARRACIN/TMAX", pattern = "x_4_" , full.names = TRUE))
e <- raster::stack(list.files("B:/CHELSA_DATA/ALBARRACIN/TMAX", pattern = "x_5_" , full.names = TRUE))
f <- raster::stack(list.files("B:/CHELSA_DATA/ALBARRACIN/TMAX", pattern = "x_6_" , full.names = TRUE))
g <- raster::stack(list.files("B:/CHELSA_DATA/ALBARRACIN/TMAX", pattern = "x_7_" , full.names = TRUE))
h <- raster::stack(list.files("B:/CHELSA_DATA/ALBARRACIN/TMAX", pattern = "x_8_" , full.names = TRUE))
#MERIDIONAL
a <- raster::stack(list.files("B:/CHELSA_DATA/MERIDIONAL/TMAX", pattern = "x_1_" , full.names = TRUE))
b <- raster::stack(list.files("B:/CHELSA_DATA/MERIDIONAL/TMAX", pattern = "x_2_" , full.names = TRUE))
c <- raster::stack(list.files("B:/CHELSA_DATA/MERIDIONAL/TMAX", pattern = "x_3_" , full.names = TRUE))
d <- raster::stack(list.files("B:/CHELSA_DATA/MERIDIONAL/TMAX", pattern = "x_4_" , full.names = TRUE))
e <- raster::stack(list.files("B:/CHELSA_DATA/MERIDIONAL/TMAX", pattern = "x_5_" , full.names = TRUE))
f <- raster::stack(list.files("B:/CHELSA_DATA/MERIDIONAL/TMAX", pattern = "x_6_" , full.names = TRUE))
g <- raster::stack(list.files("B:/CHELSA_DATA/MERIDIONAL/TMAX", pattern = "x_7_" , full.names = TRUE))
h <- raster::stack(list.files("B:/CHELSA_DATA/MERIDIONAL/TMAX", pattern = "x_8_" , full.names = TRUE))
i <- raster::stack(list.files("B:/CHELSA_DATA/MERIDIONAL/TMAX", pattern = "x_9_" , full.names = TRUE))
j <- raster::stack(list.files("B:/CHELSA_DATA/MERIDIONAL/TMAX", pattern = "x_10_" , full.names = TRUE))
k <- raster::stack(list.files("B:/CHELSA_DATA/MERIDIONAL/TMAX", pattern = "x_11_" , full.names = TRUE))
l <- raster::stack(list.files("B:/CHELSA_DATA/MERIDIONAL/TMAX", pattern = "x_12_" , full.names = TRUE))
#GUADARRAMA
a <- raster::stack(list.files("B:/CHELSA_DATA/GUADARRAMA/TMAX", pattern = "x_1_" , full.names = TRUE))
b <- raster::stack(list.files("B:/CHELSA_DATA/GUADARRAMA/TMAX", pattern = "x_2_" , full.names = TRUE))
c <- raster::stack(list.files("B:/CHELSA_DATA/GUADARRAMA/TMAX", pattern = "x_3_" , full.names = TRUE))
d <- raster::stack(list.files("B:/CHELSA_DATA/GUADARRAMA/TMAX", pattern = "x_4_" , full.names = TRUE))
e <- raster::stack(list.files("B:/CHELSA_DATA/GUADARRAMA/TMAX", pattern = "x_5_" , full.names = TRUE))
f <- raster::stack(list.files("B:/CHELSA_DATA/GUADARRAMA/TMAX", pattern = "x_6_" , full.names = TRUE))
g <- raster::stack(list.files("B:/CHELSA_DATA/GUADARRAMA/TMAX", pattern = "x_7_" , full.names = TRUE))
h <- raster::stack(list.files("B:/CHELSA_DATA/GUADARRAMA/TMAX", pattern = "x_8_" , full.names = TRUE))
i <- raster::stack(list.files("B:/CHELSA_DATA/GUADARRAMA/TMAX", pattern = "x_9_" , full.names = TRUE))
j <- raster::stack(list.files("B:/CHELSA_DATA/GUADARRAMA/TMAX", pattern = "x_10_" , full.names = TRUE))
k <- raster::stack(list.files("B:/CHELSA_DATA/GUADARRAMA/TMAX", pattern = "x_11_" , full.names = TRUE))
l <- raster::stack(list.files("B:/CHELSA_DATA/GUADARRAMA/TMAX", pattern = "x_12_" , full.names = TRUE))
#GREDOS
a <- raster::stack(list.files("B:/CHELSA_DATA/GREDOS/TMAX", pattern = "x_1_" , full.names = TRUE))
b <- raster::stack(list.files("B:/CHELSA_DATA/GREDOS/TMAX", pattern = "x_2_" , full.names = TRUE))
c <- raster::stack(list.files("B:/CHELSA_DATA/GREDOS/TMAX", pattern = "x_3_" , full.names = TRUE))
d <- raster::stack(list.files("B:/CHELSA_DATA/GREDOS/TMAX", pattern = "x_4_" , full.names = TRUE))
e <- raster::stack(list.files("B:/CHELSA_DATA/GREDOS/TMAX", pattern = "x_5_" , full.names = TRUE))
f <- raster::stack(list.files("B:/CHELSA_DATA/GREDOS/TMAX", pattern = "x_6_" , full.names = TRUE))
g <- raster::stack(list.files("B:/CHELSA_DATA/GREDOS/TMAX", pattern = "x_7_" , full.names = TRUE))
h <- raster::stack(list.files("B:/CHELSA_DATA/GREDOS/TMAX", pattern = "x_8_" , full.names = TRUE))
i <- raster::stack(list.files("B:/CHELSA_DATA/GREDOS/TMAX", pattern = "x_9_" , full.names = TRUE))
j <- raster::stack(list.files("B:/CHELSA_DATA/GREDOS/TMAX", pattern = "x_10_" , full.names = TRUE))
k <- raster::stack(list.files("B:/CHELSA_DATA/GREDOS/TMAX", pattern = "x_11_" , full.names = TRUE))
l <- raster::stack(list.files("B:/CHELSA_DATA/GREDOS/TMAX", pattern = "x_12_" , full.names = TRUE))

names(l[[1]])
plot(pp)
nlayers()




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

write_xlsx(transect_centr_TXM@data, "Results/Mean_Max_temp_transects_meridional_results.xlsx")



kk<- melt(transect_centr_TXM@data)
nn <- na.omit(filter(kk, kk$variable == "mean_1980_1989" | kk$variable == "mean_2009_2018" ))

ggplot(nn, aes(x= variable, y =value, fill=variable))+
  geom_boxplot()+
  theme_cleveland()
library(PupillometryR)
javalambre <- ggplot(nn, aes(x= variable, y =value, fill=variable))+
  geom_flat_violin(aes(fill = variable),position = position_nudge(x = .1, y = 0), trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = variable, y = value, colour = variable),position = position_jitter(width = .2), size = 2, shape = 20)+
  geom_boxplot(aes(x= variable, y =value, fill=variable),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  scale_y_continuous("ºC",c(10, 12.5, 15, 17.5, 20,22.5,25,27.5,30), limits = c(12,30))+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("Javalambre")+
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank()
  )


legend <- get_legend(ggplot(nn, aes(x= variable, y =value, fill=variable))+
                     geom_boxplot(aes(x= variable, y =value, fill=variable),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
                     scale_fill_brewer(palette = "Dark2", name = "Maximun Mean\nTemperature"))

legend <- as_ggplot(legend)
ggarrange(Meridional, javalambre, albarracin, legend, legend = FALSE)

