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

#JAVALAMBRE -----
a <- raster::stack(list.files("B:/CHELSA_DATA/JAVALAMBRE/TMAX", pattern = "x_1_" , full.names = TRUE))
b <- raster::stack(list.files("B:/CHELSA_DATA/JAVALAMBRE/TMAX", pattern = "x_2_" , full.names = TRUE))


n.cores <- parallel::detectCores() - 1
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)
doParallel::registerDoParallel(cl = my.cluster)
test <- raster::stack()
test <- foreach(s = 1:360
) %dopar% {
  raster::stack(test, raster::mosaic(a[[s]],
                                     b[[s]],
                                     fun = mean, tolerance = 1))
} 
parallel::stopCluster(cl = my.cluster)
test <- stack(unlist(test))
names(test) <- names(a)

source("Microclima/TMM.R")
write_xlsx(transect_centr_TXM@data, "Results/Mean_Max_temp_transects_javalambre_results.xlsx")

kk<- melt(transect_centr_TXM@data)
javalambre_d <- na.omit(filter(kk, kk$variable == "mean_1980_1989" | kk$variable == "mean_1996_2005" | kk$variable == "mean_2009_2018" ))

javalambre <- ggplot(javalambre_d, aes(x= variable, y =value, fill=variable))+
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

#ALBARRACIN-----------
a <- raster::stack(list.files("B:/CHELSA_DATA/ALBARRACIN/TMAX", pattern = "x_1_" , full.names = TRUE))
b <- raster::stack(list.files("B:/CHELSA_DATA/ALBARRACIN/TMAX", pattern = "x_2_" , full.names = TRUE))
c <- raster::stack(list.files("B:/CHELSA_DATA/ALBARRACIN/TMAX", pattern = "x_3_" , full.names = TRUE))
d <- raster::stack(list.files("B:/CHELSA_DATA/ALBARRACIN/TMAX", pattern = "x_4_" , full.names = TRUE))
e <- raster::stack(list.files("B:/CHELSA_DATA/ALBARRACIN/TMAX", pattern = "x_5_" , full.names = TRUE))
f <- raster::stack(list.files("B:/CHELSA_DATA/ALBARRACIN/TMAX", pattern = "x_6_" , full.names = TRUE))
g <- raster::stack(list.files("B:/CHELSA_DATA/ALBARRACIN/TMAX", pattern = "x_7_" , full.names = TRUE))
h <- raster::stack(list.files("B:/CHELSA_DATA/ALBARRACIN/TMAX", pattern = "x_8_" , full.names = TRUE))

n.cores <- parallel::detectCores() - 1
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)
doParallel::registerDoParallel(cl = my.cluster)
test <- raster::stack()
test <- foreach(s = 1:360
) %dopar% {
  raster::stack(test, raster::mosaic(a[[s]],
                                     b[[s]],
                                     c[[s]],
                                     d[[s]],
                                     e[[s]],
                                     f[[s]],
                                     g[[s]],
                                     h[[s]],
                                     fun = mean, tolerance = 1000))
} 
parallel::stopCluster(cl = my.cluster)
test <- stack(unlist(test))
  
names(test) <- names(a)

source("TMM.R")
write_xlsx(transect_centr_TXM@data, "Results/Mean_Max_temp_transects_albarracin_results.xlsx")

kk<- melt(transect_centr_TXM@data)
albarracin_d <- na.omit(filter(kk, kk$variable == "mean_1980_1989" | kk$variable == "mean_2009_2018" ))

albarracin <- ggplot(albarracin_d, aes(x= variable, y =value, fill=variable))+
  geom_flat_violin(aes(fill = variable),position = position_nudge(x = .1, y = 0), trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = variable, y = value, colour = variable),position = position_jitter(width = .2), size = 2, shape = 20)+
  geom_boxplot(aes(x= variable, y =value, fill=variable),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  scale_y_continuous("ºC",c(10, 12.5, 15, 17.5, 20,22.5,25,27.5,30), limits =  c(12,30))+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("Albarracin")+
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()
  )

#MERIDIONAL-----------
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

n.cores <- parallel::detectCores() - 1
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)
doParallel::registerDoParallel(cl = my.cluster)
test <- raster::stack()
test <- foreach(s = 1:360
) %dopar% {
  raster::stack(test, raster::mosaic(a[[s]],
                                     b[[s]],
                                     c[[s]],
                                     d[[s]],
                                     e[[s]],
                                     f[[s]],
                                     g[[s]],
                                     h[[s]],
                                     i[[s]],
                                     j[[s]],
                                     k[[s]],
                                     l[[s]],
                                     fun = mean, tolerance = 1000))
} 
parallel::stopCluster(cl = my.cluster)
test <- stack(unlist(test))

names(test) <- names(a)

source("TMM.R")
write_xlsx(transect_centr_TXM@data, "Results/Mean_Max_temp_transects_meridional_results.xlsx")

kk<- melt(transect_centr_TXM@data)
meridional_d <- na.omit(filter(kk, kk$variable == "mean_1980_1989" | kk$variable == "mean_2009_2018" ))

meridional <- ggplot(meridional_d, aes(x= variable, y =value, fill=variable))+
  geom_flat_violin(aes(fill = variable),position = position_nudge(x = .1, y = 0), trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = variable, y = value, colour = variable),position = position_jitter(width = .2), size = 2, shape = 20)+
  geom_boxplot(aes(x= variable, y =value, fill=variable),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  scale_y_continuous("ºC",c(10, 12.5, 15, 17.5, 20,22.5,25,27.5,30), limits =  c(12,30))+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("Meridional")+
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()
  )
#GUADARRAMA-----------
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
m <- raster::stack(list.files("B:/CHELSA_DATA/GUADARRAMA/TMAX", pattern = "x_13_" , full.names = TRUE))

n.cores <- parallel::detectCores() - 1
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)
doParallel::registerDoParallel(cl = my.cluster)
test <- raster::stack()
test <- foreach(s = 1:360
) %dopar% {
  raster::stack(test, raster::mosaic(a[[s]],
                                     b[[s]],
                                     c[[s]],
                                     d[[s]],
                                     e[[s]],
                                     f[[s]],
                                     g[[s]],
                                     h[[s]],
                                     i[[s]],
                                     j[[s]],
                                     k[[s]],
                                     l[[s]],
                                     m[[s]],
                                     fun = mean, tolerance = 1000))
} 
parallel::stopCluster(cl = my.cluster)
test <- stack(unlist(test))

names(test) <- names(a)

source("TMM.R")
write_xlsx(transect_centr_TXM@data, "Results/Mean_Max_temp_transects_guadarrama_results.xlsx")

kk<- melt(transect_centr_TXM@data)
guadarrama_d <- na.omit(filter(kk, kk$variable == "mean_1980_1989" | kk$variable == "mean_2009_2018" ))

guadarrama <- ggplot(guadarrama_d, aes(x= variable, y =value, fill=variable))+
  geom_flat_violin(aes(fill = variable),position = position_nudge(x = .1, y = 0), trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = variable, y = value, colour = variable),position = position_jitter(width = .2), size = 2, shape = 20)+
  geom_boxplot(aes(x= variable, y =value, fill=variable),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  scale_y_continuous("ºC",c(10, 12.5, 15, 17.5, 20,22.5,25,27.5,30), limits = c(12,30))+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("Guadarrama")+
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()
  )

#GREDOS---------------
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

n.cores <- parallel::detectCores() - 1
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)
doParallel::registerDoParallel(cl = my.cluster)
test <- raster::stack()
test <- foreach(s = 1:240
) %dopar% {
  raster::stack(test, raster::mosaic(a[[s]],
                                     b[[s]],
                                     c[[s]],
                                     d[[s]],
                                     e[[s]],
                                     f[[s]],
                                     g[[s]],
                                     h[[s]],
                                     i[[s]],
                                     j[[s]],
                                     k[[s]],
                                     l[[s]],
                                     fun = mean, tolerance = 1000))
} 
parallel::stopCluster(cl = my.cluster)
test <- stack(unlist(test))
names(test) <- names(a)

source("TMM.R")
write_xlsx(transect_centr_TXM@data, "Results/Mean_Max_temp_transects_gredos_results.xlsx")

kk<- melt(transect_centr_TXM@data)
gredos_d <- na.omit(filter(kk, kk$variable == "mean_1980_1989" | kk$variable == "mean_2009_2018" ))

gredos <- ggplot(gredos_d, aes(x= variable, y =value, fill=variable))+
  geom_flat_violin(aes(fill = variable),position = position_nudge(x = .1, y = 0), trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = variable, y = value, colour = variable),position = position_jitter(width = .2), size = 2, shape = 20)+
  geom_boxplot(aes(x= variable, y =value, fill=variable),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  scale_y_continuous("ºC",c(10, 12.5, 15, 17.5, 20,22.5,25,27.5,30), limits = c(12,30))+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("Gredos")+
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank()
  )

#PLOT-------------


legend <- get_legend(ggplot(gredos_d, aes(x= variable, y =value, fill=variable))+
                     geom_boxplot(aes(x= variable, y =value, fill=variable),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
                     scale_fill_brewer(palette = "Dark2", name = "Maximun Mean\nTemperature"))

legend <- as_ggplot(legend)
ggarrange(javalambre, meridional, albarracin, gredos, guadarrama, legend, legend = FALSE)

Mean_Max_temp_transects_ALL_results <- read_excel("Results/Mean_Max_temp_transects_ALL_results.xlsx",
                                                  col_types = c("text", "numeric", "text", 
                                                                "numeric", "text", "text", "numeric", 
                                                                "numeric", "numeric", "numeric", 
                                                                "text"))
Mean_Max_temp_transects_ALL_results <- Mean_Max_temp_transects_ALL_results[,-2]

Mean_Max_temp_transects_ALL_results <- na.omit(Mean_Max_temp_transects_ALL_results)

ggplot(Mean_Max_temp_transects_ALL_results, aes(x=mean_1980_1989, y = mean_2009_2018))+
  geom_point(aes(col = ZONE))+
  geom_smooth(method = lm, se = FALSE, col= "black")+
  scale_color_brewer(palette = "Dark2")
