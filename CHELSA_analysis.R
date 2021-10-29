

rm(list=(ls()[ls()!="v"]))
gc(reset=TRUE)
source("Dependencies/Functions.R")



# Download CHELSA dataset ----

# Monthly dataset of precipitation, maximum-, minimum-, and mean temperatures at 30 arc sec resolution for the earths land surface areas.
# There are separate files for each month starting January 1979.

# https://envicloud.wsl.ch/#/?prefix=chelsa%2Fchelsa_V2%2FGLOBAL%2Fmonthly%2F


## Mean temperature ----

# Load .txt with the entire directory which download
CHELSA_dwld_paths <- readLines("CHELSA_dwld_paths_tmed.txt")

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
  raster <- raster("raster.tif")
  raster <- raster %>%
    crop(mask) %>%
    mask(mask)
  raster <- raster/10
  raster <- raster - 273.15
  
  writeRaster(raster,
              paste0(data_rep,
                     str_sub(CHELSA_dwld_paths[i],
                             unlist(gregexpr("tas_", CHELSA_dwld_paths[i])),
                             unlist(gregexpr("_V.2", CHELSA_dwld_paths[i])) - 1), ".tif"))
}

# Rename all files from "tas_01_1979" to "1979_01"
file.rename(paste0("B:/CHELSA_DATA/TMED/",list.files("B:/CHELSA_DATA/TMED")),
            paste0("B:/CHELSA_DATA/TMED/",
                   str_sub(list.files("B:/CHELSA_DATA/TMED"), 8,11),
                   "_",
                   str_sub(list.files("B:/CHELSA_DATA/TMED"), 5,6),
                   ".tif"
            ))



## Monthly precipitation ----

# Same steps as mean temperature (above)
CHELSA_dwld_paths <- readLines("CHELSA_dwld_paths_pcp.txt")

data_rep <- "B:/CHELSA_DATA/PCP/" 

for (i in 1:length(CHELSA_dwld_paths)){
  download.file(CHELSA_dwld_paths[i],
                dest = "raster.tif",
                mode="wb")
  raster <- raster("raster.tif")
  raster <- raster %>%
    crop(mask) %>%
    mask(mask)
  raster <- raster/100
  
  writeRaster(raster,
              paste0(data_rep,
                     str_sub(CHELSA_dwld_paths[i],
                             unlist(gregexpr("pr_", CHELSA_dwld_paths[i])),
                             unlist(gregexpr("_V.2", CHELSA_dwld_paths[i])) - 1), ".tif"))
}

file.rename(paste0("B:/CHELSA_DATA/PCP/",list.files("B:/CHELSA_DATA/PCP")),
            paste0("B:/CHELSA_DATA/PCP/",
                   str_sub(list.files("B:/CHELSA_DATA/PCP"), 7,10),
                   "_",
                   str_sub(list.files("B:/CHELSA_DATA/PCP"), 4,5),
                   ".tif"
            ))




## Maximum temperature ----

# Load .txt with the entire directory which download
CHELSA_dwld_paths <- readLines("CHELSA_dwld_paths_tmax.txt")

# Load and reproject iberian peninsula mask
mask <- shapefile("Data/Peninsula_Iberica_mask.shp")
mask <- spTransform(mask, "+init=epsg:4326")

# Directory to save all downloaded files
data_rep <- "B:/CHELSA_DATA/TMAX/" 

# Loop to download all files 
for (i in 1:length(CHELSA_dwld_paths)){
  download.file(CHELSA_dwld_paths[i],
                dest = "raster.tif",
                mode="wb")
  raster <- raster("raster.tif")
  raster <- raster %>%
    crop(mask) %>%
    mask(mask)
  raster <- raster/100
  
  writeRaster(raster,
              paste0(data_rep,
                     str_sub(CHELSA_dwld_paths[i],
                             unlist(gregexpr("max_", CHELSA_dwld_paths[i])),
                             unlist(gregexpr("_V.2", CHELSA_dwld_paths[i])) - 1), ".tif"))
}

# Rename all files from "tas_01_1979" to "1979_01"
file.rename(paste0("B:/CHELSA_DATA/TMAX/",list.files("B:/CHELSA_DATA/TMAX")),
            paste0("B:/CHELSA_DATA/TMAX/",
                   str_sub(list.files("B:/CHELSA_DATA/TMAX"), 8,11),
                   "_",
                   str_sub(list.files("B:/CHELSA_DATA/TMAX"), 5,6),
                   ".tif"
            ))



## Minimum temperature ----

# Load .txt with the entire directory which download
CHELSA_dwld_paths <- readLines("CHELSA_dwld_paths_tmin.txt")

# Load and reproject iberian peninsula mask
mask <- shapefile("Data/Peninsula_Iberica_mask.shp")
mask <- spTransform(mask, "+init=epsg:4326")

# Directory to save all downloaded files
data_rep <- "B:/CHELSA_DATA/TMIN/" 

# Loop to download all files 
for (i in 1:length(CHELSA_dwld_paths)){
  download.file(CHELSA_dwld_paths[i],
                dest = "raster.tif",
                mode="wb")
  raster <- raster("raster.tif")
  raster <- raster %>%
    crop(mask) %>%
    mask(mask)
  raster <- raster/100
  
  writeRaster(raster,
              paste0(data_rep,
                     str_sub(CHELSA_dwld_paths[i],
                             unlist(gregexpr("min_", CHELSA_dwld_paths[i])),
                             unlist(gregexpr("_V.2", CHELSA_dwld_paths[i])) - 1), ".tif"))
}

# Rename all files from "tas_01_1979" to "1979_01"
file.rename(paste0("B:/CHELSA_DATA/TMIN/",list.files("B:/CHELSA_DATA/TMIN")),
            paste0("B:/CHELSA_DATA/TMIN/",
                   str_sub(list.files("B:/CHELSA_DATA/TMIN"), 8,11),
                   "_",
                   str_sub(list.files("B:/CHELSA_DATA/TMIN"), 5,6),
                   ".tif"
            ))


##TMAX_cal
##TMIN_FRIO

for (i in 1979:2019){
  TMCM <- calc(raster::stack(list.files("B:/CHELSA_DATA/TMIN", pattern = paste0(i), full.names = TRUE)), min)
  
}






# Working with data ----

## Monthly data to annual average ----


### TMED ----
TMED_annual <- raster::stack()
for (i in 1979:2019){
  raster <- calc(raster::stack(list.files("B:/CHELSA_DATA/TMED", pattern = paste0(i), full.names = TRUE)), mean) # MEAN
  TMED_annual <- raster::stack(TMED_annual, raster)
}
names(TMED_annual) <- paste0("Y_", seq(1979, 2019, by = 1))


### PCP ----
PCP_annual <- raster::stack()
for (i in 1979:2018){
  raster <- calc(raster::stack(list.files("B:/CHELSA_DATA/TMED", pattern = paste0(i), full.names = TRUE)), sum)
  PCP_annual <- raster::stack(PCP_annual, raster)
}

### TXMC ----
TMAX_annual <- raster::stack()
for (i in 1979:2019){
  raster <- calc(raster::stack(list.files("B:/CHELSA_DATA/TMAX", pattern = paste0(i), full.names = TRUE)), max) # MAX
  TMAX_annual <- raster::stack(TMAX_annual, raster)
}
### TNMF ----
TMIN_annual <- raster::stack()
for (i in 1979:2019){
  raster <- calc(raster::stack(list.files("B:/CHELSA_DATA/TMIN", pattern = paste0(i), full.names = TRUE)), min) # MIN
  TMIN_annual <- raster::stack(TMIN_annual, raster)
}

## TMED ----

### Select data for specific periods ----

Tmed_1985_1989 <- raster::subset(TMED_annual, grep(c("1985|1986|1987|1988|1989"), names(TMED_annual), value = T))

Tmed_2000_2004 <- raster::subset(TMED_annual, grep(c("2000|2001|2002|2003|2004"), names(TMED_annual), value = T))

Tmed_2015_2019 <- raster::subset(TMED_annual, grep(c("2015|2016|2017|2018|2019"), names(TMED_annual), value = T))

### Calculate mean a standard deviation for diferent periods ----

Tmed_mean_1985_1989 <- calc(Tmed_1985_1989, mean)
Tmed_sd_1985_1989 <- calc(Tmed_1985_1989, sd)

Tmed_mean_2000_2004 <- calc(Tmed_2000_2004, mean)
Tmed_sd_2000_2004 <- calc(Tmed_2000_2004, sd)

Tmed_mean_2015_2019 <- calc(Tmed_2015_2019, mean)
Tmed_sd_2015_2019 <- calc(Tmed_2015_2019, sd)

### Comparing TMED 2015-2018 vs 2015-2019 ----

Tmed_2015_2018 <- raster::subset(TMED_annual, grep(c("2015|2016|2017|2018"), names(TMED_annual), value = T))
Tmed_2015_2019 <- raster::subset(TMED_annual, grep(c("2015|2016|2017|2018|2019"), names(TMED_annual), value = T))

Tmed_mean_2015_2018 <- calc(Tmed_2015_2018, mean)
Tmed_sd_2015_2018 <- calc(Tmed_2015_2018, sd)

Tmed_mean_2015_2019 <- calc(Tmed_2015_2019, mean)
Tmed_sd_2015_2019 <- calc(Tmed_2015_2019, sd)

library(diffeR)

CrossTab_mean <- as.tibble(crosstabm(Tmed_mean_2015_2019, Tmed_mean_2015_2018, percent = TRUE))
write_xlsx(CrossTab_mean, "Results/CrossTab_tmed_2015_2018_vs_2015_2019.xlsx")
MADscatterplot(Tmed_mean_2015_2018,Tmed_mean_2015_2019)
CrossTab_sd <- as.tibble(crosstabm(Tmed_sd_2015_2019, Tmed_sd_2015_2018, percent = TRUE))
write_xlsx(CrossTab_sd, "Results/CrossTab_tmed_sd_2015_2018_vs_2015_2019.xlsx")
MADscatterplot(Tmed_sd_2015_2018,Tmed_sd_2015_2019)

mask <- shapefile("Data/Peninsula_Iberica_mask.shp")
mask <- spTransform(mask, "+init=epsg:4326")

random_points <- spsample(mask, n = 3, type='random')
proj4string(random_points) = CRS("+init=epsg:4326")
results <- raster::extract(PCP_annual,
                           random_points,
                           df=TRUE)
results <- t(results)
results <-  as.data.frame(results)
results <- results[-1,]
results <-  as.data.frame(results)
results <- mutate(results, year = seq(1979,2018, by = 1))

ggplot(results)+
  geom_point(aes(year, V1), col= "red")+
  geom_smooth(aes(year, V1),method = "loess", col = "red", se= FALSE)+
  geom_point(aes(year, V2), col= "blue")+
  geom_smooth(aes(year, V2),method = "loess", col = "blue", se= FALSE)+
  geom_point(aes(year, V3), col= "green")+
  geom_smooth(aes(year, V3),method = "loess", col = "green", se= FALSE)+
  theme(axis.title = element_blank())

plot(mask)
plot(random_points, add=T)

# Working with the transect ----

transect <- readOGR("Data/TRANSECTS_2021_v2.kml")
rm(Temperature_transects_with_elevations)

library(readxl)
Transects_with_elevations <- read_excel("Temperature_transects_with_elevations.xlsx")
Transects_with_elevations <- Transects_with_elevations %>% 
  select(c(Name, Name_new, Alt, CODIGO, ZONE))


## Get the centroids ----
transect_centr <- gCentroid(transect, byid = TRUE)
transect_centr <- SpatialPointsDataFrame(transect_centr, data = transect@data)
transect_centr@data <- left_join(transect_centr@data, Transects_with_elevations, by = "Name")


## Extract data for each centroid ----
transect_centr$mean_1985_1989 <- raster::extract(mean_1985_1989,
                                                 transect_centr, buffer = NULL ,exact = TRUE)
transect_centr$sd_1985_1989 <- raster::extract(sd_1985_1989,
                                                 transect_centr, buffer = NULL ,exact = TRUE)
transect_centr$mean_2000_2004 <- raster::extract(mean_2000_2004,
                                                 transect_centr, buffer = NULL ,exact = TRUE)
transect_centr$sd_2000_2004 <- raster::extract(sd_2000_2004,
                                                 transect_centr, buffer = NULL ,exact = TRUE)
transect_centr$mean_2015_2019 <- raster::extract(mean_2015_2019,
                                                 transect_centr, buffer = NULL ,exact = TRUE)
transect_centr$sd_2015_2019 <- raster::extract(sd_2015_2019,
                                                 transect_centr, buffer = NULL ,exact = TRUE)

## Save data in excel ----
write_xlsx(transect_centr@data, "Results/Temperature_transects_results.xlsx")


# Some fast plots ----



# Create a theme
theme_plot <- theme(legend.position = "none",
                    axis.title.x = element_blank(),
                    axis.text.x = element_blank(),
                    axis.title.y = element_blank())

## MEAN Temperature plot

# Create a legend
legend_mean <- get_legend(ggplot(transect_centr@data, 
                            aes(x = ZONE, 
                                y = sum_2015_2019, 
                                fill = ZONE))+
                       geom_violin()+
                       scale_fill_discrete(name = "Annual\nprecipitation"))

legend_mean <- as_ggplot(legend_mean)

ggarrange(
        ggplot(transect_centr@data, 
               aes(x = ZONE, 
                   y = sum_1985_1989, 
                   fill = ZONE))+
          geom_violin(aes(col=ZONE))+
          geom_boxplot(width=0.1)+
          labs(y="ºC",
               title="1985-1989")+
          theme_plot, 
        ggplot(transect_centr@data, 
               aes(x = ZONE,
                   y = sum_2000_2004,
                   fill = ZONE))+
          geom_violin(aes(col=ZONE))+
          geom_boxplot(width=0.1)+
          labs(y="ºC",
               title="2000-2004")+
          theme_plot,
        ggplot(transect_centr@data, 
               aes(x = ZONE, 
                   y = sum_2015_2019, 
                   fill = ZONE))+
          geom_violin(aes(col=ZONE))+
          geom_boxplot(width=0.1)+
          labs(y="ºC",
               title="2015-2018")+
          theme_plot,
    legend_mean, ncol = 2, nrow = 2
)

##SD plot

legend_sd <- get_legend(ggplot(transect_centr@data, 
                            aes(x = ZONE, 
                                y = sum_2015_2018, 
                                fill = ZONE))+
                       geom_violin()+
                       scale_fill_discrete(name = "SD\nPrecipitation"))

legend_sd <- as_ggplot(legend_sd)

ggarrange(
  ggplot(transect_centr@data, 
         aes(x = ZONE, 
             y = sd_1985_1989, 
             fill = ZONE))+
    geom_violin(aes(col=ZONE))+
    geom_boxplot(width=0.1)+
    labs(y="ºC",
         title="1985-1989")+
    theme_plot, 
  ggplot(transect_centr@data, 
         aes(x = ZONE, 
             y = sd_2000_2004, 
             fill = ZONE))+
    geom_violin(aes(col=ZONE))+
    geom_boxplot(width=0.1)+
    labs(y="ºC",
         title="2000-2004")+
    theme_plot,
  ggplot(transect_centr@data, 
         aes(x = ZONE, 
             y = sd_2015_2018, 
             fill = ZONE))+
    geom_violin(aes(col=ZONE))+
    geom_boxplot(width=0.1)+
    labs(y="ºC",
         title="2015-2019")+
    theme_plot,
  legend_sd, ncol = 2, nrow = 2
)

