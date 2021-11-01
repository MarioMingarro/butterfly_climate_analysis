

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
  raster <- raster/10
  raster <- raster - 273.15
  
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
  raster <- raster/10
  raster <- raster - 273.15
  
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
  raster <- calc(raster::stack(list.files("B:/CHELSA_DATA/PCP", pattern = paste0(i), full.names = TRUE)), sum)#SUM
  PCP_annual <- raster::stack(PCP_annual, raster)
}
names(PCP_annual) <- paste0("Y_", seq(1979, 2018, by = 1))
plot(PCP_annual$Y_1997)

### TXMC ----
TMAX_annual <- raster::stack()
for (i in 1979:2019){
  raster <- calc(raster::stack(list.files("B:/CHELSA_DATA/TMAX", pattern = paste0(i), full.names = TRUE)), max) # MAX
  TMAX_annual <- raster::stack(TMAX_annual, raster)
}
names(TMAX_annual) <- paste0("Y_", seq(1979, 2019, by = 1))

### TNMF ----
TMIN_annual <- raster::stack()
for (i in 1979:2019){
  raster <- calc(raster::stack(list.files("B:/CHELSA_DATA/TMIN", pattern = paste0(i), full.names = TRUE)), min) # MIN
  TMIN_annual <- raster::stack(TMIN_annual, raster)
}
names(TMIN_annual) <- paste0("Y_", seq(1979, 2019, by = 1))

## Prepare transects data ----

transect <- readOGR("Data/TRANSECTS_2021_v2.kml")
Transects_with_elevations <- read_excel("Data/Transects_name_elevations.xlsx")
Transects_with_elevations <- Transects_with_elevations %>% 
  select(c(Name, Name_new, Alt, CODIGO, ZONE))

### Get the centroids ----
transect_centr <- gCentroid(transect, byid = TRUE)
transect_centr <- SpatialPointsDataFrame(transect_centr, data = transect@data)
transect_centr@data <- left_join(transect_centr@data, Transects_with_elevations, by = "Name")
## TMED ----
### Select data for specific periods ----

TMED_1985_1989 <- raster::subset(TMED_annual, grep(c("1985|1986|1987|1988|1989"), names(TMED_annual), value = T))

TMED_2000_2004 <- raster::subset(TMED_annual, grep(c("2000|2001|2002|2003|2004"), names(TMED_annual), value = T))

TMED_2015_2019 <- raster::subset(TMED_annual, grep(c("2015|2016|2017|2018|2019"), names(TMED_annual), value = T))


TMED_1981_1990 <- raster::subset(TMED_annual, grep(c("1981|1982|1983|1984|1985|1986|1987|1988|1989|1990"), names(TMED_annual), value = T))

TMED_1996_2005 <- raster::subset(TMED_annual, grep(c("1996|1997|1998|1999|2000|2001|2002|2003|2004|2005"), names(TMED_annual), value = T))

TMED_2010_2019 <- raster::subset(TMED_annual, grep(c("2010|2011|2012|2013|2014|2015|2016|2017|2018|2019"), names(TMED_annual), value = T))

### Calculate mean a standard deviation for diferent periods ----

TMED_mean_1985_1989 <- calc(TMED_1985_1989, mean)
TMED_sd_1985_1989 <- calc(TMED_1985_1989, sd)

TMED_mean_2000_2004 <- calc(TMED_2000_2004, mean)
TMED_sd_2000_2004 <- calc(TMED_2000_2004, sd)

TMED_mean_2015_2019 <- calc(TMED_2015_2019, mean)
TMED_sd_2015_2019 <- calc(TMED_2015_2019, sd)

TMED_mean_1981_1990 <- calc(TMED_1981_1990, mean)
TMED_sd_1981_1990 <- calc(TMED_1981_1990, sd)

TMED_mean_1996_2005 <- calc(TMED_1996_2005, mean)
TMED_sd_1996_2005 <- calc(TMED_1996_2005, sd)

TMED_mean_2010_2019 <- calc(TMED_2010_2019, mean)
TMED_sd_2010_2019 <- calc(TMED_2010_2019, sd)


### Comparing TMED 2015-2018 vs 2015-2019 ----

TMED_2015_2018 <- raster::subset(TMED_annual, grep(c("2015|2016|2017|2018"), names(TMED_annual), value = T))
TMED_2015_2019 <- raster::subset(TMED_annual, grep(c("2015|2016|2017|2018|2019"), names(TMED_annual), value = T))

TMED_mean_2015_2018 <- calc(TMED_2015_2018, mean)
TMED_sd_2015_2018 <- calc(TMED_2015_2018, sd)

TMED_mean_2015_2019 <- calc(TMED_2015_2019, mean)
TMED_sd_2015_2019 <- calc(TMED_2015_2019, sd)

##package diffeR

CrossTab_mean <- as.tibble(crosstabm(TMED_mean_2015_2019, TMED_mean_2015_2018, percent = TRUE))
write_xlsx(CrossTab_mean, "Results/CrossTab_tmed_2015_2018_vs_2015_2019.xlsx")
MADscatterplot(TMED_mean_2015_2018,TMED_mean_2015_2019)
CrossTab_sd <- as.tibble(crosstabm(TMED_sd_2015_2019, TMED_sd_2015_2018, percent = TRUE))
write_xlsx(CrossTab_sd, "Results/CrossTab_tmed_sd_2015_2018_vs_2015_2019.xlsx")
MADscatterplot(TMED_sd_2015_2018,TMED_sd_2015_2019)





### Extract data for each centroid ----
transect_centr_TMED <- transect_centr

transect_centr_TMED$mean_1985_1989 <- raster::extract(TMED_mean_1985_1989,
                                                 transect_centr_TMED, buffer = NULL ,exact = TRUE)
transect_centr_TMED$sd_1985_1989 <- raster::extract(TMED_sd_1985_1989,
                                                 transect_centr_TMED, buffer = NULL ,exact = TRUE)
transect_centr_TMED$mean_2000_2004 <- raster::extract(TMED_mean_2000_2004,
                                                 transect_centr_TMED, buffer = NULL ,exact = TRUE)
transect_centr_TMED$sd_2000_2004 <- raster::extract(TMED_sd_2000_2004,
                                                 transect_centr_TMED, buffer = NULL ,exact = TRUE)
transect_centr_TMED$mean_2015_2019 <- raster::extract(TMED_mean_2015_2019,
                                                 transect_centr_TMED, buffer = NULL ,exact = TRUE)
transect_centr_TMED$sd_2015_2019 <- raster::extract(TMED_sd_2015_2019,
                                                 transect_centr_TMED, buffer = NULL ,exact = TRUE)

transect_centr_TMED$mean_1981_1990 <- raster::extract(TMED_mean_1981_1990,
                                                     transect_centr_TMED, buffer = NULL ,exact = TRUE)
transect_centr_TMED$sd_1981_1990 <- raster::extract(TMED_sd_1981_1990,
                                                   transect_centr_TMED, buffer = NULL ,exact = TRUE)
transect_centr_TMED$mean_1996_2005 <- raster::extract(TMED_mean_1996_2005,
                                                     transect_centr_TMED, buffer = NULL ,exact = TRUE)
transect_centr_TMED$sd_1996_2005 <- raster::extract(TMED_sd_1996_2005,
                                                   transect_centr_TMED, buffer = NULL ,exact = TRUE)
transect_centr_TMED$mean_2010_2019 <- raster::extract(TMED_mean_2010_2019,
                                                     transect_centr_TMED, buffer = NULL ,exact = TRUE)
transect_centr_TMED$sd_2010_2019 <- raster::extract(TMED_sd_2010_2019,
                                                   transect_centr_TMED, buffer = NULL ,exact = TRUE)

write_xlsx(transect_centr_TMED@data, "Results/Temperature_transects_results.xlsx")


## PCP ----
### Select data for specific periods ----


PCP_1985_1989 <- raster::subset(PCP_annual, grep(c("1985|1986|1987|1988|1989"), names(PCP_annual), value = T))

PCP_2000_2004 <- raster::subset(PCP_annual, grep(c("2000|2001|2002|2003|2004"), names(PCP_annual), value = T))

PCP_2015_2018 <- raster::subset(PCP_annual, grep(c("2015|2016|2017|2018"), names(PCP_annual), value = T))



PCP_1981_1990 <- raster::subset(PCP_annual, grep(c("1981|1982|1983|1984|1985|1986|1987|1988|1989|1990"), names(PCP_annual), value = T))

PCP_1996_2005 <- raster::subset(PCP_annual, grep(c("1996|1997|1998|1999|2000|2001|2002|2003|2004|2005"), names(PCP_annual), value = T))

PCP_2009_2018 <- raster::subset(PCP_annual, grep(c("2009|2010|2011|2012|2013|2014|2015|2016|2017|2018"), names(PCP_annual), value = T))

### Calculate mean a standard deviation for diferent periods ----

PCP_mean_1985_1989 <- calc(PCP_1985_1989, mean)
PCP_sd_1985_1989 <- calc(PCP_1985_1989, sd)

PCP_mean_2000_2004 <- calc(PCP_2000_2004, mean)
PCP_sd_2000_2004 <- calc(PCP_2000_2004, sd)

PCP_mean_2015_2018 <- calc(PCP_2015_2018, mean)
PCP_sd_2015_2018 <- calc(PCP_2015_2018, sd)


PCP_mean_1981_1990 <- calc(PCP_1981_1990, mean)
PCP_sd_1981_1990 <- calc(PCP_1981_1990, sd)

PCP_mean_1996_2005 <- calc(PCP_1996_2005, mean)
PCP_sd_1996_2005 <- calc(PCP_1996_2005, sd)

PCP_mean_2009_2018 <- calc(PCP_2009_2018, mean)
PCP_sd_2009_2018 <- calc(PCP_2009_2018, sd)

### Extract data for each centroid ----
transect_centr_PCP <- transect_centr

transect_centr_PCP$mean_1985_1989 <- raster::extract(PCP_mean_1985_1989,
                                                      transect_centr_PCP, buffer = NULL ,exact = TRUE)
transect_centr_PCP$sd_1985_1989 <- raster::extract(PCP_sd_1985_1989,
                                                    transect_centr_PCP, buffer = NULL ,exact = TRUE)
transect_centr_PCP$mean_2000_2004 <- raster::extract(PCP_mean_2000_2004,
                                                      transect_centr_PCP, buffer = NULL ,exact = TRUE)
transect_centr_PCP$sd_2000_2004 <- raster::extract(PCP_sd_2000_2004,
                                                    transect_centr_PCP, buffer = NULL ,exact = TRUE)
transect_centr_PCP$mean_2015_2018 <- raster::extract(PCP_mean_2015_2018,
                                                      transect_centr_PCP, buffer = NULL ,exact = TRUE)
transect_centr_PCP$sd_2015_2018 <- raster::extract(PCP_sd_2015_2018,
                                                    transect_centr_PCP, buffer = NULL ,exact = TRUE)


transect_centr_PCP$mean_1981_1990 <- raster::extract(PCP_mean_1981_1990,
                                                     transect_centr_PCP, buffer = NULL ,exact = TRUE)
transect_centr_PCP$sd_1981_1990 <- raster::extract(PCP_sd_1981_1990,
                                                   transect_centr_PCP, buffer = NULL ,exact = TRUE)
transect_centr_PCP$mean_1996_2005 <- raster::extract(PCP_mean_1996_2005,
                                                     transect_centr_PCP, buffer = NULL ,exact = TRUE)
transect_centr_PCP$sd_1996_2005 <- raster::extract(PCP_sd_1996_2005,
                                                   transect_centr_PCP, buffer = NULL ,exact = TRUE)
transect_centr_PCP$mean_2009_2018 <- raster::extract(PCP_mean_2009_2018,
                                                     transect_centr_PCP, buffer = NULL ,exact = TRUE)
transect_centr_PCP$sd_2009_2018 <- raster::extract(PCP_sd_2009_2018,
                                                   transect_centr_PCP, buffer = NULL ,exact = TRUE)

write_xlsx(transect_centr_PCP@data, "Results/Precipitation_transects_results.xlsx")

view(transect_centr_PCP@data)


# Some fast plots ----

# Create a theme
theme_plot <- theme(legend.position = "none",
                    axis.title.x = element_blank(),
                    axis.text.x = element_blank(),
                    axis.title.y = element_blank())

# Create plots
pcp <- transect_centr_PCP@data

names <- c("mean_1985_1989", "mean_2000_2004","mean_2015_2018",
           "mean_1981_1990","mean_1996_2005" ,"mean_2009_2018" )
for (i in names){
  pcp2 <-  select(pcp, Name, ZONE, i)
  pcp_long <- melt(pcp2)
  p <- ggplot(pcp_long, aes(ZONE,value))+
    geom_violin(aes(fill= ZONE))+
    geom_boxplot(width=0.1, fill = "gray80")+
    labs(y="mm",
         title=paste0("p_",i))+
    scale_y_continuous("mm",c(400,600,800,1000,1200,1400,1600, 1800,2000), limits = c(400,2000))+
    theme_plot
  assign(paste0("p_",i), p)
}

# Create a legend
legend_mean <- get_legend(ggplot(pcp_long, aes(ZONE,value))+
                            geom_violin(aes(fill= ZONE))+
                            scale_fill_discrete(name = "Annual\nprecipitation"))

legend_mean <- as_ggplot(legend_mean)

# multiple plot
ggarrange(p_mean_1981_1990, p_mean_1985_1989, p_mean_1996_2005, p_mean_2000_2004, p_mean_2009_2018, p_mean_2015_2018,
          ncol = 3, nrow = 3, legend_mean)

