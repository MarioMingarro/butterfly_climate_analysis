# Clean and load packages ----
closeAllConnections()
rm(list=(ls()[ls()!="v"]))
gc(reset=TRUE)
source("Dependencies/Functions.R")

## Prepare transects data ----

transect <- readOGR("Data/TRANSECTS_2021_v2.kml")
Transects_with_elevations <- read_excel("Data/Transects_name_elevations.xlsx")
Transects_with_elevations <- Transects_with_elevations %>% 
  select(c(Name, Name_new, Alt, CODIGO, ZONE))

### Get the centroids ----
transect_centr <- gCentroid(transect, byid = TRUE)
transect_centr <- SpatialPointsDataFrame(transect_centr, data = transect@data)
transect_centr@data <- left_join(transect_centr@data, Transects_with_elevations, by = "Name")

# Load and reproject iberian peninsula mask
mask <- shapefile("Data/Peninsula_Iberica_mask.shp")
mask <- spTransform(mask, "+init=epsg:4326")

# Directory to save all downloaded files
data_rep <- "B:/CHELSA_DATA/WORLDCLIM/DATA/SPAIN/"

names <- list.files("B:/CHELSA_DATA/WORLDCLIM/DATA", pattern = ".tif", full.names = TRUE)
# Loop to download all files
for (i in 1:length(list.files("B:/CHELSA_DATA/WORLDCLIM/DATA", pattern = ".tif"))){
  raster <- raster(names[i])
  raster <- raster %>%
    crop(mask) %>%
    mask(mask)
  
  writeRaster(raster,
              paste0(data_rep,str_sub(names[i], unlist(gregexpr("tmax_", names[i])), unlist(gregexpr("-", names[i])) +2), ".tif"))
}


## TXM ----
### Monthly data to annual average ----
TXM <- raster::stack()
for (i in c(1980:1989, 2010:2019)){
  raster <- calc(raster::stack(list.files("B:/CHELSA_DATA/WORLDCLIM/DATA/SPAIN", pattern = paste0(i), full.names = TRUE)), mean) # MEAN
  TXM <- raster::stack(TXM, raster)
}
names(TXM) <- paste0("Y_", c(1980:1989, 2010:2019))

### Select data for specific periods ----
TXM_1980_1989 <- raster::subset(TXM, grep(c("1980|1981|1982|1983|1984|1985|1986|1987|1988|1989"), names(TXM), value = T))

TXM_2010_2019 <- raster::subset(TXM, grep(c("2010|2011|2012|2013|2014|2015|2016|2017|2018|2019"), names(TXM), value = T))

### Calculate mean a standard deviation for diferent periods ----
TXM_mean_1980_1989 <- calc(TXM_1980_1989, mean)

TXM_mean_2010_2019 <- calc(TXM_2010_2019, mean)



### Extract data for each centroid ----
transect_centr_TXM <- transect_centr

transect_centr_TXM$mean_1980_1989 <- raster::extract(TXM_mean_1980_1989,
                                                     transect_centr_TXM, buffer = NULL ,exact = TRUE)

transect_centr_TXM$mean_2010_2019 <- raster::extract(TXM_mean_2010_2019,
                                                     transect_centr_TXM, buffer = NULL ,exact = TRUE)


write_xlsx(transect_centr_TXM@data, "Results/Excel/Mean_Max_temp_transects_worldclim_results.xlsx")

# Plot -----
data <- melt(transect_centr_TXM@data)
data <- na.omit(filter(data, data$variable == "mean_1980_1989" | data$variable == "mean_2010_2019"))
gredos <- filter(data, data$ZONE == "GRE")
guadarrama <- filter(data, data$ZONE == "GUA")
meridional <- filter(data, data$ZONE == "SM")
javalambre <- filter(data, data$ZONE == "JA")

gredos_p <- ggplot(gredos, aes(x= variable, y =value, fill=variable))+
  geom_flat_violin(aes(fill = variable),position = position_nudge(x = .1, y = 0), trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = variable, y = value, colour = variable),position = position_jitter(width = .2), size = 2, shape = 20)+
  geom_boxplot(aes(x= variable, y =value, fill=variable),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  scale_y_continuous("ºC",c(10, 12.5, 15, 17.5, 20,23), limits = c(10,23))+
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

guadarrama_p <- ggplot(guadarrama, aes(x= variable, y =value, fill=variable))+
  geom_flat_violin(aes(fill = variable),position = position_nudge(x = .1, y = 0), trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = variable, y = value, colour = variable),position = position_jitter(width = .2), size = 2, shape = 20)+
  geom_boxplot(aes(x= variable, y =value, fill=variable),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  scale_y_continuous("ºC",c(10, 12.5, 15, 17.5, 20,23), limits = c(10,23))+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("Guadarrama")+
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank()
  )

meridional_p <- ggplot(meridional, aes(x= variable, y =value, fill=variable))+
  geom_flat_violin(aes(fill = variable),position = position_nudge(x = .1, y = 0), trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = variable, y = value, colour = variable),position = position_jitter(width = .2), size = 2, shape = 20)+
  geom_boxplot(aes(x= variable, y =value, fill=variable),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  scale_y_continuous("ºC",c(10, 12.5, 15, 17.5, 20,23), limits = c(10,23))+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("Meridional")+
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank()
  )

javalambre_p <- ggplot(javalambre, aes(x= variable, y =value, fill=variable))+
  geom_flat_violin(aes(fill = variable),position = position_nudge(x = .1, y = 0), trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = variable, y = value, colour = variable),position = position_jitter(width = .2), size = 2, shape = 20)+
  geom_boxplot(aes(x= variable, y =value, fill=variable),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  scale_y_continuous("ºC",c(10, 12.5, 15, 17.5, 20,23), limits = c(10,23))+
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

legend <- get_legend(ggplot(gredos, aes(x= variable, y =value, fill=variable))+
                       geom_boxplot(aes(x= variable, y =value, fill=variable),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
                       scale_fill_brewer(palette = "Dark2", name = "Maximun Mean\nTemperature"))

legend <- as_ggplot(legend)
ggarrange(javalambre_p, meridional_p, gredos_p, guadarrama_p, legend, legend = FALSE)
