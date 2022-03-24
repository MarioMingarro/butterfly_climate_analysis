## Maximum temperature ----

# Load .txt with the entire directory which download
CHELSA_dwld_paths <- readLines("Data/CHELSA_dwld_paths_tmax.txt")

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

## TXM ----
### Monthly data to annual average ----
TXM <- raster::stack()
for (i in 1979:2019){
  raster <- calc(raster::stack(list.files("B:/CHELSA_DATA/TMAX", pattern = paste0(i), full.names = TRUE)), mean) # MEAN
  TXM <- raster::stack(TXM, raster)
}
names(TXM) <- paste0("Y_", seq(1979, 2019, by = 1))

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

write_xlsx(transect_centr_TXM@data, "Results/Excel/Mean_Max_temp_transects_CHELSA_results2.xlsx")