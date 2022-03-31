# Transects ----
## Prepare transects data ----
transect <- readOGR("Data/TRANSECTS_2021_v2.kml")
Transects_with_elevations <- read_excel("Data/Transects_name_elevations.xlsx")
Transects_with_elevations <- Transects_with_elevations %>% 
  select(c(Name, Name_new, Alt, CODIGO, ZONE))

### Get the centroids ----
transect_centr <- gCentroid(transect, byid = TRUE)
transect_centr <- SpatialPointsDataFrame(transect_centr, data = transect@data)
transect_centr@data <- left_join(transect_centr@data, Transects_with_elevations, by = "Name")

# MODIS----

## Monthly to annual
TMED_MODIS <- raster::stack()

for (i in 2000:2018){
  raster <- calc(raster::stack(list.files("B:/DATA/MODIS/Surf_Temp_8Days_1Km_v61/LST_Day_1km", pattern = paste0(i), full.names = TRUE)), na.rm = TRUE, mean) # MEAN
  TMED_MODIS <- raster::stack(TMED_MODIS, raster)
}
names(TMED_MODIS) <- paste0("Y_", seq(2000, 2018, by = 1))
TMED_MODIS <- (TMED_MODIS - 273.15) * 0.02 / 10

## calc----
transect_centr_MODIS <- transect_centr

projectRaster(TMED_MODIS, crs = proj4string(transect_centr_MODIS))


MODIS_data <- raster::extract(TMED_MODIS,
                              transect_centr_MODIS,
                              buffer = NULL,
                              exact = TRUE)

MODIS_data <- as.data.frame(MODIS_data)
MODIS_data$NAME <- transect_centr_MODIS@data$Name_new
MODIS_data$ZONA <- transect_centr_MODIS@data$ZONE


aa <- melt(MODIS_data)
MODIS_plot <- ggplot(aa, aes(variable, value, group=aa$ZONA, color=aa$ZONA))+
  geom_point(alpha =0.2)+
  geom_smooth(aes(fill=aa$ZONA))+
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2")+
  scale_x_discrete(labels = seq(2000, 2018, 1))+
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(size = 6)
  )

# CHELSA ----
### Monthly data to annual average
TMAX_CHELSA <- raster::stack()
for (i in 2000:2018){
  raster <- calc(raster::stack(list.files("B:/CHELSA_DATA/TMAX", pattern = paste0(i), full.names = TRUE)), mean) # MEAN
  TMAX_CHELSA <- raster::stack(TMAX_CHELSA, raster)
}
names(TMAX_CHELSA) <- paste0("Y_", seq(2000, 2018, by = 1))

## calc----
transect_centr_CHELSA <- transect_centr

projectRaster(TMAX_CHELSA, crs = proj4string(transect_centr_CHELSA))


CHELSA_data <- raster::extract(TMAX_CHELSA,
                               transect_centr_CHELSA,
                               buffer = NULL,
                               exact = TRUE)

CHELSA_data <- as.data.frame(CHELSA_data)
CHELSA_data$NAME <- transect_centr_CHELSA@data$Name_new
CHELSA_data$ZONA <- transect_centr_CHELSA@data$ZONE

aa <- melt(CHELSA_data)
CHELSA_plot <-
  ggplot(aa, aes(variable, value, group = aa$ZONA, color = aa$ZONA)) +
  geom_point(alpha = 0.2) +
  geom_smooth(aes(fill = aa$ZONA)) +
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_discrete(labels = seq(2000, 2018, 1)) +
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(size = 6)
  )


# STEAD ----
tmp_raster <- brick("B:/DATA/STEAD/DATOS_ORIGINALES/tmax_pen.nc")
names(tmp_raster) <- paste0("Y_",seq(as.Date("1901/1/1"), by = "day", length.out = nlayers(tmp_raster)))


### Monthly data to annual average
TMAX_STEAD <- raster::stack()
for (i in 2000:2014){
  raster <- calc(raster::subset(tmp_raster, grep(paste0(i), names(tmp_raster), value = T)), mean) # MEAN
  TMAX_STEAD <- raster::stack(TMAX_STEAD, raster)
}
names(TMAX_STEAD) <- paste0("Y_", seq(2000, 2014, by = 1))

## calc----
transect_centr_STEAD <- transect_centr

projectRaster(TMAX_STEAD, crs = proj4string(transect_centr_STEAD))


STEAD_data <- raster::extract(TMAX_STEAD,
                              transect_centr_STEAD,
                              buffer = NULL,
                              exact = TRUE)

STEAD_data <- as.data.frame(STEAD_data)
STEAD_data$NAME <- transect_centr_STEAD@data$Name_new
STEAD_data$ZONA <- transect_centr_STEAD@data$ZONE

aa <- melt(STEAD_data)
STEAD_plot <-
  ggplot(aa, aes(variable, value, group = aa$ZONA, color = aa$ZONA)) +
  geom_point(alpha = 0.2) +
  geom_smooth(aes(fill = aa$ZONA)) +
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_discrete(labels = seq(2000, 2018, 1)) +
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(size = 6)
  )


# MICROCLIMA ----
years_microclima <- c(2000,2001,2002,2003,2004,2005,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018)

## JAVALAMBRE ----
TMAX_JAVALAMBRE <- raster::stack()
for (i in years_microclima){
  raster <- calc(raster::stack(list.files("B:/CHELSA_DATA/JAVALAMBRE/TMAX/RASTER", pattern = paste0(i), full.names = TRUE)), mean) # MEAN
  TMAX_JAVALAMBRE <- raster::stack(TMAX_JAVALAMBRE, raster)
}
names(TMAX_JAVALAMBRE) <- paste0("Y_", years_microclima)

transect_centr_JAVALAMBRE <- transect_centr

projectRaster(TMAX_JAVALAMBRE, crs = proj4string(transect_centr_JAVALAMBRE))


JAVALAMBRE_data <- raster::extract(TMAX_JAVALAMBRE,
                                   transect_centr_JAVALAMBRE,
                                   buffer = NULL,
                                   exact = TRUE)

JAVALAMBRE_data <- as.data.frame(JAVALAMBRE_data)
JAVALAMBRE_data$NAME <- transect_centr_JAVALAMBRE@data$Name_new
JAVALAMBRE_data$ZONA <- transect_centr_JAVALAMBRE@data$ZONE
JAVALAMBRE_data <- na.omit(JAVALAMBRE_data)

## ALBARRACIN ----
TMAX_ALBARRACIN <- raster::stack()
for (i in years_microclima){
  raster <- calc(raster::stack(list.files("B:/CHELSA_DATA/ALBARRACIN/TMAX/RASTER", pattern = paste0(i), full.names = TRUE)), mean) # MEAN
  TMAX_ALBARRACIN <- raster::stack(TMAX_ALBARRACIN, raster)
}
names(TMAX_ALBARRACIN) <- paste0("Y_", years_microclima)

transect_centr_ALBARRACIN <- transect_centr

projectRaster(TMAX_ALBARRACIN, crs = proj4string(transect_centr_ALBARRACIN))


ALBARRACIN_data <- raster::extract(TMAX_ALBARRACIN,
                                   transect_centr_ALBARRACIN,
                                   buffer = NULL,
                                   exact = TRUE)

ALBARRACIN_data <- as.data.frame(ALBARRACIN_data)
ALBARRACIN_data$NAME <- transect_centr_ALBARRACIN@data$Name_new
ALBARRACIN_data$ZONA <- transect_centr_ALBARRACIN@data$ZONE
ALBARRACIN_data <- na.omit(ALBARRACIN_data)

## MERIDIONAL ----
TMAX_MERIDIONAL <- raster::stack()

for (i in years_microclima){
  raster <- calc(raster::stack(list.files("B:/CHELSA_DATA/MERIDIONAL/TMAX/RASTER", pattern = paste0(i), full.names = TRUE)), mean) # MEAN
  TMAX_MERIDIONAL <- raster::stack(TMAX_MERIDIONAL, raster)
}
names(TMAX_MERIDIONAL) <- paste0("Y_", years_microclima)

transect_centr_MERIDIONAL <- transect_centr

projectRaster(TMAX_MERIDIONAL, crs = proj4string(transect_centr_MERIDIONAL))


MERIDIONAL_data <- raster::extract(TMAX_MERIDIONAL,
                                   transect_centr_MERIDIONAL,
                                   buffer = NULL,
                                   exact = TRUE)

MERIDIONAL_data <- as.data.frame(MERIDIONAL_data)
MERIDIONAL_data$NAME <- transect_centr_MERIDIONAL@data$Name_new
MERIDIONAL_data$ZONA <- transect_centr_MERIDIONAL@data$ZONE
MERIDIONAL_data <- na.omit(MERIDIONAL_data)

## GUADARRAMA ----
TMAX_GUADARRAMA <- raster::stack()
for (i in years_microclima){
  raster <- calc(raster::stack(list.files("B:/CHELSA_DATA/GUADARRAMA/TMAX/RASTER", pattern = paste0(i), full.names = TRUE)), mean) # MEAN
  TMAX_GUADARRAMA <- raster::stack(TMAX_GUADARRAMA, raster)
}
names(TMAX_GUADARRAMA) <- paste0("Y_", years_microclima)

transect_centr_GUADARRAMA <- transect_centr

projectRaster(TMAX_GUADARRAMA, crs = proj4string(transect_centr_GUADARRAMA))


GUADARRAMA_data <- raster::extract(TMAX_GUADARRAMA,
                                   transect_centr_GUADARRAMA,
                                   buffer = NULL,
                                   exact = TRUE)

GUADARRAMA_data <- as.data.frame(GUADARRAMA_data)
GUADARRAMA_data$NAME <- transect_centr_GUADARRAMA@data$Name_new
GUADARRAMA_data$ZONA <- transect_centr_GUADARRAMA@data$ZONE
GUADARRAMA_data <- na.omit(GUADARRAMA_data)

## GREDOS ----
TMAX_GREDOS <- raster::stack()
for (i in years_microclima){
  raster <- calc(raster::stack(list.files("B:/CHELSA_DATA/GREDOS/TMAX/RASTER", pattern = paste0(i), full.names = TRUE)), mean) # MEAN
  TMAX_GREDOS <- raster::stack(TMAX_GREDOS, raster)
}
names(TMAX_GREDOS) <- paste0("Y_", years_microclima)

transect_centr_GREDOS <- transect_centr

projectRaster(TMAX_GREDOS, crs = proj4string(transect_centr_GREDOS))


GREDOS_data <- raster::extract(TMAX_GREDOS,
                               transect_centr_GREDOS,
                               buffer = NULL,
                               exact = TRUE)

GREDOS_data <- as.data.frame(GREDOS_data)
GREDOS_data$NAME <- transect_centr_GREDOS@data$Name_new
GREDOS_data$ZONA <- transect_centr_GREDOS@data$ZONE
GREDOS_data <- na.omit(GREDOS_data)

MICROCLIMA_data <- rbind(JAVALAMBRE_data, ALBARRACIN_data, MERIDIONAL_data, GUADARRAMA_data, GREDOS_data)
aa <- melt(MICROCLIMA_data)
MICROCLIMA_plot <-
  ggplot(aa, aes(variable, value, group = aa$ZONA, color = aa$ZONA)) +
  geom_point(alpha = 0.2) +
  geom_smooth(aes(fill = aa$ZONA)) +
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_discrete(labels = seq(2000, 2018, 1)) +
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(size = 6)
  )




#------------------------------------------------------------

# TERRACLIMATE ----
extension <- raster::stack("B:/CHELSA_DATA/TMAX/1979_02.tif")
for (i in 1958:2020){
URL_TERRACLIMATE <- paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/TERRACLIMATE_ALL/data/TerraClimate_tmax_",i,".nc")
URL_TERRACLIMATE <- brick(URL_TERRACLIMATE)
URL_TERRACLIMATE <- crop(URL_TERRACLIMATE,extension)
URL_TERRACLIMATE <- calc(URL_TERRACLIMATE, mean)
writeRaster(URL_TERRACLIMATE, paste0("B:/CHELSA_DATA/TERRACLIMATE/TMAX/TMAX_", i),format="GTiff")
}

## Monthly to yearly ----
TMAX_TERRACLIMATE <- raster::stack()
for (i in 2000:2018){
  raster <- raster::stack(list.files("B:/CHELSA_DATA/TERRACLIMATE/TMAX", pattern = paste0(i), full.names = TRUE))
  TMAX_TERRACLIMATE <- raster::stack(TMAX_TERRACLIMATE, raster)
}
names(TMAX_TERRACLIMATE) <- paste0("Y_", seq(2000,2018, 1))


transect_centr_TERRACLIMATE <- transect_centr

projectRaster(TMAX_TERRACLIMATE, crs = proj4string(transect_centr_TERRACLIMATE))


TERRACLIMATE_data <- raster::extract(TMAX_TERRACLIMATE,
                               transect_centr_TERRACLIMATE,
                               buffer = NULL,
                               exact = TRUE)

TERRACLIMATE_data <- as.data.frame(TERRACLIMATE_data)
TERRACLIMATE_data$NAME <- transect_centr_TERRACLIMATE@data$Name_new
TERRACLIMATE_data$ZONA <- transect_centr_TERRACLIMATE@data$ZONE
TERRACLIMATE_data <- na.omit(TERRACLIMATE_data)

aa <- melt(TERRACLIMATE_data)
TERRACLIMATE_plot <-
  ggplot(aa, aes(variable, value, group = aa$ZONA, color = aa$ZONA)) +
  geom_point(alpha = 0.2) +
  geom_smooth(aes(fill = aa$ZONA)) +
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_discrete(labels = seq(2000, 2018, 1)) +
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(size = 6)
  )

###########
#ALL --------
MODIS_2 <- melt(MODIS_data)
MODEL <- rep("MODIS",nrow(MODIS_2))
MODIS_2 <- cbind(MODIS_2, MODEL)

CHELSA_2 <- melt(CHELSA_data)
MODEL <- rep("CHELSA",nrow(CHELSA_2))
CHELSA_2 <- cbind(CHELSA_2, MODEL)

STEAD_2 <- melt(STEAD_data)
MODEL <- rep("STEAD",nrow(STEAD_2))
STEAD_2 <- cbind(STEAD_2, MODEL)

MICROCLIMA_2 <- melt(MICROCLIMA_data)
MODEL <- rep("MICROCLIMA",nrow(MICROCLIMA_2))
MICROCLIMA_2 <- cbind(MICROCLIMA_2, MODEL)

TERRACLIMATE_2 <- melt(TERRACLIMATE_data)
MODEL <- rep("TERRACLIMATE",nrow(TERRACLIMATE_2))
TERRACLIMATE_2 <- cbind(TERRACLIMATE_2, MODEL)

ALL_DATA_MELTED <- bind_rows(MODIS_2,CHELSA_2,STEAD_2,MICROCLIMA_2,TERRACLIMATE_2)
ALL_DATA_MELTED <- left_join(ALL_DATA_MELTED, as.data.frame(Transects_with_elevations), by = c("NAME"="Name_new"))
ALL_DATA_MELTED$variable <- as.numeric(gsub("Y_","",as.character(ALL_DATA_MELTED$variable)))
ALL_DATA_MELTED <- dplyr::rename(ALL_DATA_MELTED, YEAR=variable)

# All tmax vs year with trends 
ggplot(ALL_DATA_MELTED, aes(x=YEAR, y=value))+
  geom_point(aes(col= MODEL), alpha = 0.2)+
  geom_smooth(aes(group = MODEL),method = lm, se = FALSE,color="black")+
  geom_smooth(aes(group = MODEL, col=MODEL, fill =MODEL))+
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank()
  )
# All tmax vs elevation with trends 
ggplot(ALL_DATA_MELTED, aes(x=Alt, y=value))+
  geom_point(aes(col= MODEL), alpha = 0.2)+
  geom_smooth(aes(group = MODEL),method = lm, se = FALSE,color="black")+
  geom_smooth(aes(group = MODEL, col=MODEL, fill =MODEL))+
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank()
  )

filt <- filter(ALL_DATA_MELTED, MODEL == "MODIS")
ggplot(filt,aes(x=Alt, y=value))+
  geom_point(aes(col= ZONA), alpha = 0.2)+
  geom_smooth(aes(group = ZONA, col=ZONA, fill =ZONA),method = lm, se = FALSE,)+
  ggtitle("MODIS")+
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank()
  )

filt <- filter(ALL_DATA_MELTED, MODEL == "STEAD")
ggplot(filt,aes(x=Alt, y=value))+
  geom_point(aes(col= ZONA), alpha = 0.2)+
  geom_smooth(aes(group = ZONA, col=ZONA, fill =ZONA),method = lm, se = FALSE,)+
  ggtitle("STEAD")+
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank()
  )

filt <- filter(ALL_DATA_MELTED, MODEL == "TERRACLIMATE")
ggplot(filt,aes(x=Alt, y=value))+
  geom_point(aes(col= ZONA), alpha = 0.2)+
  geom_smooth(aes(group = ZONA, col=ZONA, fill =ZONA),method = lm, se = FALSE,)+
  ggtitle("TERRACLIMATE")+
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank()
  )

filt <- filter(ALL_DATA_MELTED, MODEL == "CHELSA")
ggplot(filt,aes(x=Alt, y=value))+
  geom_point(aes(col= ZONA), alpha = 0.2)+
  geom_smooth(aes(group = ZONA, col=ZONA, fill =ZONA),method = lm, se = FALSE,)+
  ggtitle("CHELSA")+
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank()
  )
filt <- filter(ALL_DATA_MELTED, MODEL == "MICROCLIMA")
ggplot(filt,aes(x=Alt, y=value))+
  geom_point(aes(col= ZONA), alpha = 0.2)+
  geom_smooth(aes(group = ZONA, col=ZONA, fill =ZONA),method = lm, se = FALSE,)+
  ggtitle("MICROCLIMA")+
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank()
  )

#####
## To excel----
MICROCLIMA_data <- rbind(rep("MICROCLIMA", length(MICROCLIMA_data)),MICROCLIMA_data)
MODIS_data <- rbind(rep("MODIS", length(MODIS_data)),MODIS_data)
STEAD_data <- rbind(rep("STEAD", length(STEAD_data)),STEAD_data)
CHELSA_data <- rbind(rep("CHELSA", length(CHELSA_data)),CHELSA_data)
TERRACLIMATE_data <- rbind(rep("TERRACLIMATE", length(TERRACLIMATE_data)),TERRACLIMATE_data)

ALL_DATA <- left_join(MICROCLIMA_data,MODIS_data, by ="NAME")
ALL_DATA <- left_join(ALL_DATA,STEAD_data, by ="NAME")
ALL_DATA <- left_join(ALL_DATA,CHELSA_data, by ="NAME")
ALL_DATA <- left_join(ALL_DATA,TERRACLIMATE_data, by ="NAME")



ALL_DATA[1,19:38] <- rep("MODIS", length(MODIS_data))
ALL_DATA[1,39:54] <- rep("STEAD", length(STEAD_data))
ALL_DATA[1,55:74] <- rep("CHELSA", length(CHELSA_data))
ALL_DATA[1,75:length(ALL_DATA)] <- rep("TERRACLIMATE", length(TERRACLIMATE_data))



write.csv(ALL_DATA,"C:/GITHUB_REP/butterfly_climate_analysis/Results/All_2000_2018.csv" )


kk <- read_excel("Results/All_2000_2018.xls", 
                 col_names = FALSE)

kk <- melt(kk)


pp <- select(kk,contains("TERRACLIMATE"))




chelsa_2000_2018 <- raster::extract(TMED,
                                    transect_centr_TMED, buffer = NULL ,exact = TRUE)
chelsa_2000_2018 <- as.data.frame(chelsa_2000_2018)
chelsa_2000_2018$NAME <- transect_centr@data$Name_new
chelsa_2000_2018$ZONA <- transect_centr@data$ZONE

bb <- melt(chelsa_2000_2018)
ggplot(bb, aes(variable, value, group=ZONA, color=ZONA))+
  geom_point(alpha =0.2)+
  geom_smooth(aes(fill=ZONA))+
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2")+
  scale_x_discrete(labels = seq(2000, 2018, 1))+
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(size = 6)
  )



kk <- brick(stack((list.files("B:/DATA/MODIS/Surf_Temp_8Days_1Km_v61/LST_Day_1km", pattern = paste0(2000), full.names = TRUE))))
plot(kk$MOD11A2_LST_Day_1km_2000_049)
kk$MOD11A2_LST_Day_1km_2000_049@data







# ---------------------





