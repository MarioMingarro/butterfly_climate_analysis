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

## TMED ----
### Monthly data to annual average ----
TMED <- raster::stack()
for (i in 1979:2019){
  raster <- calc(raster::stack(list.files("B:/CHELSA_DATA/TMED", pattern = paste0(i), full.names = TRUE)), mean) # MEAN
  TMED <- raster::stack(TMED, raster)
}
names(TMED) <- paste0("Y_", seq(1979, 2019, by = 1))

### Select data for specific periods ----

TMED_1980_1989 <- raster::subset(TMED, grep(c("1980|1981|1982|1983|1984|1985|1986|1987|1988|1989"), names(TMED), value = T))

TMED_1996_2005 <- raster::subset(TMED, grep(c("1996|1997|1998|1999|2000|2001|2002|2003|2004|2005"), names(TMED), value = T))

TMED_2009_2018 <- raster::subset(TMED, grep(c("2009|2010|2011|2012|2013|2014|2015|2016|2017|2018"), names(TMED), value = T))

### Calculate mean a standard deviation for diferent periods ----

TMED_mean_1980_1989 <- calc(TMED_1980_1989, mean)
TMED_sd_1980_1989 <- calc(TMED_1980_1989, sd)

TMED_mean_1996_2005 <- calc(TMED_1996_2005, mean)
TMED_sd_1996_2005 <- calc(TMED_1996_2005, sd)

TMED_mean_2009_2018 <- calc(TMED_2009_2018, mean)
TMED_sd_2009_2018 <- calc(TMED_2009_2018, sd)


### Extract data for each centroid ----
transect_centr_TMED <- transect_centr

transect_centr_TMED$mean_1980_1989 <- raster::extract(TMED_mean_1980_1989,
                                                      transect_centr_TMED, buffer = NULL ,exact = TRUE)
transect_centr_TMED$sd_1980_1989 <- raster::extract(TMED_sd_1980_1989,
                                                    transect_centr_TMED, buffer = NULL ,exact = TRUE)
transect_centr_TMED$mean_1996_2005 <- raster::extract(TMED_mean_1996_2005,
                                                      transect_centr_TMED, buffer = NULL ,exact = TRUE)
transect_centr_TMED$sd_1996_2005 <- raster::extract(TMED_sd_1996_2005,
                                                    transect_centr_TMED, buffer = NULL ,exact = TRUE)
transect_centr_TMED$mean_2009_2018 <- raster::extract(TMED_mean_2009_2018,
                                                      transect_centr_TMED, buffer = NULL ,exact = TRUE)
transect_centr_TMED$sd_2009_2018 <- raster::extract(TMED_sd_2009_2018,
                                                    transect_centr_TMED, buffer = NULL ,exact = TRUE)

write_xlsx(transect_centr_TMED@data, "Results/Temperature_transects_results.xlsx")

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







## PCP ----
### Monthly data to annual average ----
PCP_annual <- raster::stack()
for (i in 1979:2018){
  raster <- calc(raster::stack(list.files("B:/CHELSA_DATA/PCP", pattern = paste0(i), full.names = TRUE)), sum)#SUM
  PCP_annual <- raster::stack(PCP_annual, raster)
}
names(PCP_annual) <- paste0("Y_", seq(1979, 2018, by = 1))

### Select data for specific periods ----

PCP_1980_1989 <- raster::subset(PCP_annual, grep(c("1980|1981|1982|1983|1984|1985|1986|1987|1988|1989"), names(PCP_annual), value = T))

PCP_1996_2005 <- raster::subset(PCP_annual, grep(c("1996|1997|1998|1999|2000|2001|2002|2003|2004|2005"), names(PCP_annual), value = T))

PCP_2009_2018 <- raster::subset(PCP_annual, grep(c("2009|2010|2011|2012|2013|2014|2015|2016|2017|2018"), names(PCP_annual), value = T))

### Calculate mean a standard deviation for diferent periods ----
PCP_mean_1980_1989 <- calc(PCP_1980_1989, mean)
PCP_sd_1980_1989 <- calc(PCP_1980_1989, sd)

PCP_mean_1996_2005 <- calc(PCP_1996_2005, mean)
PCP_sd_1996_2005 <- calc(PCP_1996_2005, sd)

PCP_mean_2009_2018 <- calc(PCP_2009_2018, mean)
PCP_sd_2009_2018 <- calc(PCP_2009_2018, sd)

### Extract data for each centroid ----
transect_centr_PCP <- transect_centr

transect_centr_PCP$mean_1980_1989 <- raster::extract(PCP_mean_1980_1989,
                                                     transect_centr_PCP, buffer = NULL ,exact = TRUE)
transect_centr_PCP$sd_1980_1989 <- raster::extract(PCP_sd_1980_1989,
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


## TXMC ----
### Monthly data to annual average ----
TXMC <- raster::stack()
for (i in 1979:2019){
  raster <- calc(raster::stack(list.files("B:/CHELSA_DATA/TMAX", pattern = paste0(i), full.names = TRUE)), max) # MAX
  TXMC <- raster::stack(TXMC, raster)
}
names(TXMC) <- paste0("Y_", seq(1979, 2019, by = 1))

### Select data for specific periods ----
TXMC_1980_1989 <- raster::subset(TXMC, grep(c("1980|1981|1982|1983|1984|1985|1986|1987|1988|1989"), names(TXMC), value = T))

TXMC_1996_2005 <- raster::subset(TXMC, grep(c("1996|1997|1998|1999|2000|2001|2002|2003|2004|2005"), names(TXMC), value = T))

TXMC_2009_2018 <- raster::subset(TXMC, grep(c("2009|2010|2011|2012|2013|2014|2015|2016|2017|2018"), names(TXMC), value = T))

### Calculate mean a standard deviation for diferent periods ----
TXMC_mean_1980_1989 <- calc(TXMC_1980_1989, mean)
TXMC_sd_1980_1989 <- calc(TXMC_1980_1989, sd)

TXMC_mean_1996_2005 <- calc(TXMC_1996_2005, mean)
TXMC_sd_1996_2005 <- calc(TXMC_1996_2005, sd)

TXMC_mean_2009_2018 <- calc(TXMC_2009_2018, mean)
TXMC_sd_2009_2018 <- calc(TXMC_2009_2018, sd)


### Extract data for each centroid ----
transect_centr_TXMC <- transect_centr

transect_centr_TXMC$mean_1980_1989 <- raster::extract(TXMC_mean_1980_1989,
                                                      transect_centr_TXMC, buffer = NULL ,exact = TRUE)
transect_centr_TXMC$sd_1980_1989 <- raster::extract(TXMC_sd_1980_1989,
                                                    transect_centr_TXMC, buffer = NULL ,exact = TRUE)
transect_centr_TXMC$mean_1996_2005 <- raster::extract(TXMC_mean_1996_2005,
                                                      transect_centr_TXMC, buffer = NULL ,exact = TRUE)
transect_centr_TXMC$sd_1996_2005 <- raster::extract(TXMC_sd_1996_2005,
                                                    transect_centr_TXMC, buffer = NULL ,exact = TRUE)
transect_centr_TXMC$mean_2009_2018 <- raster::extract(TXMC_mean_2009_2018,
                                                      transect_centr_TXMC, buffer = NULL ,exact = TRUE)
transect_centr_TXMC$sd_2009_2018 <- raster::extract(TXMC_sd_2009_2018,
                                                    transect_centr_TXMC, buffer = NULL ,exact = TRUE)

write_xlsx(transect_centr_TXMC@data, "Results/Max_temp_warmest_month_transects_results.xlsx")

## TNMF ----
### Monthly data to annual average ----
TNMF <- raster::stack()
for (i in 1979:2019){
  raster <- calc(raster::stack(list.files("B:/CHELSA_DATA/TMIN", pattern = paste0(i), full.names = TRUE)), min) # MIN
  TNMF <- raster::stack(TNMF, raster)
}
names(TNMF) <- paste0("Y_", seq(1979, 2019, by = 1))

### Select data for specific periods ----
TNMF_1980_1989 <- raster::subset(TNMF, grep(c("1980|1981|1982|1983|1984|1985|1986|1987|1988|1989"), names(TNMF), value = T))

TNMF_1996_2005 <- raster::subset(TNMF, grep(c("1996|1997|1998|1999|2000|2001|2002|2003|2004|2005"), names(TNMF), value = T))

TNMF_2009_2018 <- raster::subset(TNMF, grep(c("2009|2010|2011|2012|2013|2014|2015|2016|2017|2018"), names(TNMF), value = T))

### Calculate mean a standard deviation for diferent periods ----
TNMF_mean_1980_1989 <- calc(TNMF_1980_1989, mean)
TNMF_sd_1980_1989 <- calc(TNMF_1980_1989, sd)

TNMF_mean_1996_2005 <- calc(TNMF_1996_2005, mean)
TNMF_sd_1996_2005 <- calc(TNMF_1996_2005, sd)

TNMF_mean_2009_2018 <- calc(TNMF_2009_2018, mean)
TNMF_sd_2009_2018 <- calc(TNMF_2009_2018, sd)


### Extract data for each centroid ----
transect_centr_TNMF <- transect_centr

transect_centr_TNMF$mean_1980_1989 <- raster::extract(TNMF_mean_1980_1989,
                                                      transect_centr_TNMF, buffer = NULL ,exact = TRUE)
transect_centr_TNMF$sd_1980_1989 <- raster::extract(TNMF_sd_1980_1989,
                                                    transect_centr_TNMF, buffer = NULL ,exact = TRUE)
transect_centr_TNMF$mean_1996_2005 <- raster::extract(TNMF_mean_1996_2005,
                                                      transect_centr_TNMF, buffer = NULL ,exact = TRUE)
transect_centr_TNMF$sd_1996_2005 <- raster::extract(TNMF_sd_1996_2005,
                                                    transect_centr_TNMF, buffer = NULL ,exact = TRUE)
transect_centr_TNMF$mean_2009_2018 <- raster::extract(TNMF_mean_2009_2018,
                                                      transect_centr_TNMF, buffer = NULL ,exact = TRUE)
transect_centr_TNMF$sd_2009_2018 <- raster::extract(TNMF_sd_2009_2018,
                                                    transect_centr_TNMF, buffer = NULL ,exact = TRUE)

write_xlsx(transect_centr_TNMF@data, "Results/Min_temp_coldest_month_transects_results.xlsx")


# Some fast plots ----

## TMED ----

TMED_T <- transect_centr_TMED@data

# Create a legend
legend <- get_legend(ggplot(TMED_T_s, aes(ZONE,value))+
                       geom_violin(aes(fill= ZONE))+
                       scale_fill_discrete(name = "Mean\nTemperature"))

legend <- as_ggplot(legend)

### Mean ----
names <- c("mean_1985_1989", "mean_2000_2004","mean_2015_2019",
           "mean_1980_1989","mean_1996_2005" ,"mean_2009_2018" )
#### Boxplot----

for (i in names){
  TMED_T_s <-  select(TMED_T, Name, ZONE, i)
  TMED_T_s <- melt(TMED_T_s)
  p <- ggplot(TMED_T_s, aes(ZONE,value))+
    geom_violin(aes(fill= ZONE))+
    geom_boxplot(width=0.1, fill = "gray80")+
    labs(y="ºC",
         title=paste0("T_",i))+
    scale_y_continuous("ºC",c(4,6,8,10,12,14,16,18), limits = c(4,18))+
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank())
  assign(paste0("p_",i), p)
}


ggarrange(p_mean_1980_1989, p_mean_1985_1989, p_mean_1996_2005, p_mean_2000_2004, p_mean_2009_2018, p_mean_2015_2019,
          ncol = 3, nrow = 2)

#### Scatterplot tmed_vs elevation ----
for (i in 1:6){
  p <- ggplot(TMED_T, aes_string("Alt", names[i], col = "ZONE", fill = "ZONE"))+
    geom_point()+
    geom_smooth()+
    facet_wrap(~ZONE)+
    labs(y = "ºC",
         x = "m",
         title=paste0("T_",names[i]))+
    theme(legend.position = "none")
  assign(paste0("p_",names[i]), p)
}


ggarrange(p_mean_1980_1989, p_mean_1985_1989, p_mean_1996_2005, p_mean_2000_2004, p_mean_2009_2018, p_mean_2015_2019,
          ncol = 3, nrow = 2)

### Sd ----
names <- c("sd_1985_1989", "sd_2000_2004","sd_2015_2019",
           "sd_1980_1989","sd_1996_2005" ,"sd_2009_2018" )

#### Boxplot ----
for (i in names){
  TMED_T_s <-  select(TMED_T, Name, ZONE, i)
  TMED_T_s <- melt(TMED_T_s)
  p <- ggplot(TMED_T_s, aes(ZONE,value))+
    geom_violin(aes(fill= ZONE))+
    geom_boxplot(width=0.1, fill = "gray80")+
    labs(y="mm",
         title=paste0("T_",i))+
    scale_y_continuous("ºC", limits = c(0.17,1.2))+
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank())
  assign(paste0("p_",i), p)
}

ggarrange(p_sd_1980_1989, p_sd_1985_1989, p_sd_1996_2005, p_sd_2000_2004, p_sd_2009_2018, p_sd_2015_2019,
          ncol = 3, nrow = 2)

#### Scatterplot tmed_vs elevation ----
for (i in 1:6){
  p <- ggplot(TMED_T, aes_string("Alt", names[i], col = "ZONE", fill = "ZONE"))+
    geom_point()+
    geom_smooth()+
    facet_wrap(~ZONE)+
    labs(y = "ºC",
         x = "m",
         title=paste0("T_",names[i]))+
    theme(legend.position = "none")
  assign(paste0("p_",names[i]), p)
}


ggarrange(p_sd_1980_1989, p_sd_1985_1989, p_sd_1996_2005, p_sd_2000_2004, p_sd_2009_2018, p_sd_2015_2019,
          ncol = 3, nrow = 2)


## PCP ----

PCP_T <- transect_centr_PCP@data

### Mean ----
names <- c("mean_1985_1989", "mean_2000_2004","mean_2015_2018",
           "mean_1980_1989","mean_1996_2005" ,"mean_2009_2018" )
#### Boxplot----
for (i in names){
  PCP_T_s <-  select(PCP_T, Name, ZONE, i)
  PCP_T_s <- melt(PCP_T_s)
  p <- ggplot(PCP_T_s, aes(ZONE,value))+
    geom_violin(aes(fill= ZONE))+
    geom_boxplot(width=0.1, fill = "gray80")+
    labs(y="mm",
         title=paste0("PCP_",i))+
    scale_y_continuous("mm",c(400,600,800,1000,1200,1400,1600,1800,2000), limits = c(400,2000))+
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank())
  assign(paste0("p_",i), p)
}


# Multiple plot
ggarrange(p_mean_1980_1989, p_mean_1985_1989, p_mean_1996_2005, p_mean_2000_2004, p_mean_2009_2018, p_mean_2015_2018,
          ncol = 3, nrow = 2)

#### Scatterplot tmed_vs elevation ----
for (i in 1:6){
  p <- ggplot(PCP_T, aes_string("Alt", names[i], col = "ZONE", fill = "ZONE"))+
    geom_point()+
    geom_smooth()+
    facet_wrap(~ZONE)+
    labs(y = "mm",
         x = "m",
         title=paste0("PCP_",names[i]))+
    theme(legend.position = "none")
  assign(paste0("p_",names[i]), p)
}


ggarrange(p_mean_1980_1989, p_mean_1985_1989, p_mean_1996_2005, p_mean_2000_2004, p_mean_2009_2018, p_mean_2015_2018,
          ncol = 3, nrow = 2)

### Sd ----
names <- c("sd_1985_1989", "sd_2000_2004","sd_2015_2018",
           "sd_1980_1989","sd_1996_2005" ,"sd_2009_2018" )
#### Boxplot ----

for (i in names){
  PCP_T_s <-  select(PCP_T, Name, ZONE, i)
  PCP_T_s <- melt(PCP_T_s)
  p <- ggplot(PCP_T_s, aes(ZONE,value))+
    geom_violin(aes(fill= ZONE))+
    geom_boxplot(width=0.1, fill = "gray80")+
    labs(y="mm",
         title=paste0("PCP_",i))+
    scale_y_continuous("mm", limits = c(20,680))+
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank())
  assign(paste0("p_",i), p)
}


# Multiple plot
ggarrange(p_sd_1980_1989, p_sd_1985_1989, p_sd_1996_2005, p_sd_2000_2004, p_sd_2009_2018, p_sd_2015_2018,
          ncol = 3, nrow = 2)

#### Scatterplot tmed_vs elevation ----

for (i in 1:6){
  p <- ggplot(PCP_T, aes_string("Alt", names[i], col = "ZONE", fill = "ZONE"))+
    geom_point()+
    geom_smooth()+
    facet_wrap(~ZONE)+
    labs(y = "ºC",
         x = "m",
         title=paste0("PCP_",names[i]))+
    theme(legend.position = "none")
  assign(paste0("p_",names[i]), p)
}


ggarrange(p_sd_1980_1989, p_sd_1985_1989, p_sd_1996_2005, p_sd_2000_2004, p_sd_2009_2018, p_sd_2015_2018,
          ncol = 3, nrow = 2)

## TXMC ----

TXMC_T <- transect_centr_TXMC@data

### Mean ----
names <- c("mean_1985_1989", "mean_2000_2004","mean_2015_2019",
           "mean_1980_1989","mean_1996_2005" ,"mean_2009_2018" )
#### Boxplot----

for (i in names){
  TXMC_T_s <-  select(TXMC_T, Name, ZONE, i)
  TXMC_T_s <- melt(TXMC_T_s)
  p <- ggplot(TXMC_T_s, aes(ZONE,value))+
    geom_violin(aes(fill= ZONE))+
    geom_boxplot(width=0.1, fill = "gray80")+
    labs(title=paste0("TXWM_",i))+
    scale_y_continuous("ºC",limits = c(20,35))+
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank())
  assign(paste0("p_",i), p)
}


ggarrange(p_mean_1980_1989, p_mean_1985_1989, p_mean_1996_2005, p_mean_2000_2004, p_mean_2009_2018, p_mean_2015_2019,
          ncol = 3, nrow = 2)

#### Scatterplot TXMC_vs elevation ----
for (i in 1:6){
  p <- ggplot(TXMC_T, aes_string("Alt", names[i], col = "ZONE", fill = "ZONE"))+
    geom_point()+
    geom_smooth()+
    facet_wrap(~ZONE)+
    labs(y = "ºC",
         x = "m",
         title=paste0("TXWM_",names[i]))+
    theme(legend.position = "none")
  assign(paste0("p_",names[i]), p)
}


ggarrange(p_mean_1980_1989, p_mean_1985_1989, p_mean_1996_2005, p_mean_2000_2004, p_mean_2009_2018, p_mean_2015_2019,
          ncol = 3, nrow = 2)

### Sd ----
names <- c("sd_1985_1989", "sd_2000_2004","sd_2015_2019",
           "sd_1980_1989","sd_1996_2005" ,"sd_2009_2018" )

#### Boxplot ----
for (i in names){
  TXMC_T_s <-  select(TXMC_T, Name, ZONE, i)
  TXMC_T_s <- melt(TXMC_T_s)
  p <- ggplot(TXMC_T_s, aes(ZONE,value))+
    geom_violin(aes(fill= ZONE))+
    geom_boxplot(width=0.1, fill = "gray80")+
    labs(title=paste0("TXWM_",i))+
    scale_y_continuous("ºC", limits = c(0.3,1.5))+
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank())
  assign(paste0("p_",i), p)
}

ggarrange(p_sd_1980_1989, p_sd_1985_1989, p_sd_1996_2005, p_sd_2000_2004, p_sd_2009_2018, p_sd_2015_2019,
          ncol = 3, nrow = 2)

#### Scatterplot TXMC_vs elevation ----
for (i in 1:6){
  p <- ggplot(TXMC_T, aes_string("Alt", names[i], col = "ZONE", fill = "ZONE"))+
    geom_point()+
    geom_smooth()+
    facet_wrap(~ZONE)+
    labs(y = "ºC",
         x = "m",
         title=paste0("TXWM_",names[i]))+
    theme(legend.position = "none")
  assign(paste0("p_",names[i]), p)
}


ggarrange(p_sd_1980_1989, p_sd_1985_1989, p_sd_1996_2005, p_sd_2000_2004, p_sd_2009_2018, p_sd_2015_2019,
          ncol = 3, nrow = 2)

## TNMF ----

TNMF_T <- transect_centr_TNMF@data

### Mean ----
names <- c("mean_1985_1989", "mean_2000_2004","mean_2015_2019",
           "mean_1980_1989","mean_1996_2005" ,"mean_2009_2018" )
#### Boxplot----

for (i in names){
  TNMF_T_s <-  select(TNMF_T, Name, ZONE, i)
  TNMF_T_s <- melt(TNMF_T_s)
  p <- ggplot(TNMF_T_s, aes(ZONE,value))+
    geom_violin(aes(fill= ZONE))+
    geom_boxplot(width=0.1, fill = "gray80")+
    labs(title=paste0("TNCM_",i))+
    scale_y_continuous("ºC", limits = c(-9,3))+
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank())
  assign(paste0("p_",i), p)
}


ggarrange(p_mean_1980_1989, p_mean_1985_1989, p_mean_1996_2005, p_mean_2000_2004, p_mean_2009_2018, p_mean_2015_2019,
          ncol = 3, nrow = 2)

#### Scatterplot TNMF_vs elevation ----
for (i in 1:6){
  p <- ggplot(TNMF_T, aes_string("Alt", names[i], col = "ZONE", fill = "ZONE"))+
    geom_point()+
    geom_smooth()+
    facet_wrap(~ZONE)+
    labs(y = "ºC",
         x = "m",
         title=paste0("TNCM_",names[i]))+
    theme(legend.position = "none")
  assign(paste0("p_",names[i]), p)
}


ggarrange(p_mean_1980_1989, p_mean_1985_1989, p_mean_1996_2005, p_mean_2000_2004, p_mean_2009_2018, p_mean_2015_2019,
          ncol = 3, nrow = 2)

### Sd ----
names <- c("sd_1985_1989", "sd_2000_2004","sd_2015_2019",
           "sd_1980_1989","sd_1996_2005" ,"sd_2009_2018" )

#### Boxplot ----
for (i in names){
  TNMF_T_s <-  select(TNMF_T, Name, ZONE, i)
  TNMF_T_s <- melt(TNMF_T_s)
  p <- ggplot(TNMF_T_s, aes(ZONE,value))+
    geom_violin(aes(fill= ZONE))+
    geom_boxplot(width=0.1, fill = "gray80")+
    labs(title=paste0("TNCM_",i))+
    scale_y_continuous("ºC", limits = c(0.2,2.2))+
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank())
  assign(paste0("p_",i), p)
}

ggarrange(p_sd_1980_1989, p_sd_1985_1989, p_sd_1996_2005, p_sd_2000_2004, p_sd_2009_2018, p_sd_2015_2019,
          ncol = 3, nrow = 2)

#### Scatterplot TNMF_vs elevation ----
for (i in 1:6){
  p <- ggplot(TNMF_T, aes_string("Alt", names[i], col = "ZONE", fill = "ZONE")) +
    geom_point() +
    geom_smooth() +
    facet_wrap(~ ZONE) +
    labs(y = "ºC",
         x = "m",
         title = paste0("TNCM_", names[i])) +
    theme(legend.position = "none")
  assign(paste0("p_", names[i]), p)
}

ggarrange(p_sd_1980_1989, p_sd_1985_1989, p_sd_1996_2005, p_sd_2000_2004, p_sd_2009_2018, p_sd_2015_2019,
          ncol = 3, nrow = 2)
