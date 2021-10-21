# ------------------

rm(list=(ls()[ls()!="v"]))
gc(reset=TRUE)
source("Dependencies/Functions.R")


# ------------------
# Download CHELSA dataset 
# ------------------

# Monthly dataset of precipitation, maximum-, minimum-, and mean temperatures at 30 arc sec resolution for the earths land surface areas.
# There are separate files for each month starting January 1979.

# https://envicloud.wsl.ch/#/?prefix=chelsa%2Fchelsa_V2%2FGLOBAL%2Fmonthly%2F

# Mean temperature
# ------------------

CHELSA_dwld_paths <- readLines("CHELSA_dwld_paths_tmed.txt")
mask <- shapefile("Data/Peninsula_Iberica_mask.shp")
mask <- spTransform(mask, "+init=epsg:4326")



data_rep <- "B:/CHELSA_DATA/TMED/" 

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
                             unlist(gregexpr("tas_", CHELSA_dwld_paths[i])),
                             unlist(gregexpr("_V.2", CHELSA_dwld_paths[i])) - 1), ".tif"))
}

file.rename(paste0("B:/CHELSA_DATA/TMED/",list.files("B:/CHELSA_DATA/TMED")),
            paste0("B:/CHELSA_DATA/TMED/",
                   str_sub(list.files("B:/CHELSA_DATA/TMED"), 8,11),
                   "_",
                   str_sub(list.files("B:/CHELSA_DATA/TMED"), 5,6),
                   ".tif"
            ))

# Monthly precipitation
# ------------------

CHELSA_dwld_paths <- readLines("CHELSA_dwld_paths_pcp.txt")
mask <- shapefile("Data/Peninsula_Iberica_mask.shp")
mask <- spTransform(mask, "+init=epsg:4326")


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


# ------------------
## Working with data
# ------------------


datos <- raster::stack(list.files("B:/CHELSA_DATA", full.names = TRUE))


# monthly to annual averages
annual_data <- sumSeries(datos, p = "1979-02/2019-12", yr0 = "1979-02-01", l = nlayers(datos), 
               fun = function(x) colMeans(x, na.rm = TRUE), freqin = "months", freqout = "years")

# Select data for specific periods
data_1985_1989 <- raster::subset(annual_data, grep(c("1985|1986|1987|1988|1989"), names(annual_data), value = T))

data_2000_2004 <- raster::subset(annual_data, grep(c("2000|2001|2002|2003|2004"), names(annual_data), value = T))

data_2015_2019 <- raster::subset(annual_data, grep(c("2015|2016|2017|2018|2019"), names(annual_data), value = T))

#Calculate mean a standard deviation for diferent periods
mean_1985_1989 <- calc(data_1985_1989, mean)
sd_1985_1989 <- calc(data_1985_1989, sd)

mean_2000_2004 <- calc(data_2000_2004, mean)
sd_2000_2004 <- calc(data_2000_2004, sd)

mean_2015_2019 <- calc(data_2015_2019, mean)
sd_2015_2019 <- calc(data_2015_2019, sd)



##Delete some irrelevant data and reduce RAM usage

rm(data_1985_1989)
rm(data_2000_2004)
rm(data_2015_2019)
rm(annual_data)
rm(datos)

gc(reset=TRUE)

## Working with the transect
# ------------------
transect <- readOGR("Data/TRANSECTS_2021_v2.kml")

# Assing zones to transects
ZONE <- c("GUA","GUA","GRE","GRE","GRE","GRE","GRE","GRE","GRE","GRE","GRE","GRE",
          "GRE","GRE","GRE","GRE","GUA","GUA","GUA","GUA","GUA","GUA","GUA","GUA",
          "GUA","GUA","GUA","GUA","GUA","GUA","GUA","GUA","GUA","GUA","GUA","GUA",
          "GUA","GUA","GUA","GUA","GUA","GUA","GUA","GUA","GUA","SM","SM","SM","SM",
          "SM","SM","SM","SM","SM","SM","SM","SM","SM","SM","SM","SM","SM","SM","SM",
          "SM","JA","JA","JA","JA","JA","JA","JA","JA","JA","JA","GRE","GRE","GUA")

# Get the centroids
transect_centr <- gCentroid(transect, byid = TRUE)
transect_centr <- SpatialPointsDataFrame(transect_centr, data = transect@data)
transect_centr@data <- mutate(transect_centr@data, ZONE)

# Extract data for each centroid
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

# Save data in excel
write_xlsx(transect_centr@data, "Results/Temperature_transects_results.xlsx")

## Some fast plots
# ------------------


# Create a theme
theme_plot <- theme(legend.position = "none",
                    axis.title.x = element_blank(),
                    axis.text.x = element_blank(),
                    axis.title.x = element_blank())

## MEAN Temperature plot

# Create a legend
legend_mean <- get_legend(ggplot(transect_centr@data, 
                            aes(x = ZONE, 
                                y = mean_2015_2019, 
                                fill = ZONE))+
                       geom_violin()+
                       scale_fill_discrete(name = "Mean\nTemperature"))

legend_mean <- as_ggplot(legend_mean)

ggarrange(
        ggplot(transect_centr@data, 
               aes(x = ZONE, 
                   y = mean_1985_1989, 
                   fill = ZONE))+
          geom_violin(aes(col=ZONE))+
          geom_boxplot(width=0.1)+
          labs(y="ºC",
               title="1985-1989")+
          theme_plot, 
        ggplot(transect_centr@data, 
               aes(x = ZONE,
                   y = mean_2000_2004,
                   fill = ZONE))+
          geom_violin(aes(col=ZONE))+
          geom_boxplot(width=0.1)+
          labs(y="ºC",
               title="2000-2004")+
          theme_plot,
        ggplot(transect_centr@data, 
               aes(x = ZONE, 
                   y = mean_2015_2019, 
                   fill = ZONE))+
          geom_violin(aes(col=ZONE))+
          geom_boxplot(width=0.1)+
          labs(y="ºC",
               title="2015-2019")+
          theme_plot,
    legend_mean, ncol = 2, nrow = 2
)

##SD plot

legend_sd <- get_legend(ggplot(transect_centr@data, 
                            aes(x = ZONE, 
                                y = mean_2015_2019, 
                                fill = ZONE))+
                       geom_violin()+
                       scale_fill_discrete(name = "SD\nTemperature"))

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
             y = sd_2015_2019, 
             fill = ZONE))+
    geom_violin(aes(col=ZONE))+
    geom_boxplot(width=0.1)+
    labs(y="ºC",
         title="2015-2019")+
    theme_plot,
  legend_sd, ncol = 2, nrow = 2
)


## Climate velocity (NOT RUN, IN PROGRESS)

# ------------------
# Temporal trend
vt <- tempTrend(r, th = 10)
# Spatial gradient
vg <- spatGrad(r, th = 0.0001, projected = FALSE)
# Climate velocity
gv <- gVoCC(vt, vg)

plot(vel)
plot(vg)
plot(vt)

vel <- gv[[1]] # Velocity
ang <- gv[[2]] # Angle
mn <- calc(r, mean, na.rm = T) # All period Temperature average

# Create a data frame with the centroid for the trajectories and associated input data
lonlat <- data.frame(xyFromCell(vel, 1:ncell(vel)))
lonlat$vel <- raster::extract(vel, lonlat)
lonlat$ang <- raster::extract(ang, lonlat[,1:2])
lonlat$mn <- raster::extract(mn, lonlat[,1:2])
lonlat <- na.omit(lonlat)


# Calculate the trajectories with parallel processing
cores <-  detectCores()
ncores<- cores[1]-2 
cuts <- cut(1:nrow(lonlat), ncores)
cl <- makeCluster(ncores)
tic("Execution time: ")
registerDoParallel(cl)
traj <- foreach(x = levels(cuts), 
                .combine = rbind, 
                .packages = c('raster','sp','rgeos','geosphere','rgdal','VoCC'), 
                .multicombine = TRUE) %dopar% {
                  voccTraj(lonlat[cuts == x,], 
                           vel, 
                           ang, 
                           mn, 
                           tyr = 47, 
                           trajID = as.numeric(rownames(lonlat[cuts == x,])), correct = TRUE)}

stopCluster(cl)

toc()

plot(traj)

tic("Execution time: ")
traj_cl <- trajClas(traj, vel, ang, mn, trajSt = 4, tyr = 47, nmL = 20, smL = 50, Nend = 40, Nst = 15, NFT = 70)
toc()
#The trajectory classes ("TrajClas") 
#(1) non-moving, (2) slow-moving, (3) internal sinks, 
#(4) boundary sinks, (5) sources, (6) relative sinks, 
#(7) corridors, (8) divergence and (9) convergence.
# ------------------
TrajClas <- traj_cl$TrajClas

TrajClas_c <- crop(TrajClas, 
                   extent(x_min, x_max, y_min, y_max))


