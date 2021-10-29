rm(list=(ls()[ls()!="v"]))
gc(reset=TRUE)
source("Dependencies/Functions.R")

# ------------------

# CHELSA dataset comparation 

# ------------------

V_2_1 <- raster::stack(list.files("A:/CHELSA_COMPARATION/CHELSA_2.1", full.names = TRUE ))
V_cruts <- raster::stack(list.files("A:/CHELSA_COMPARATION/CHELSAcruts", full.names = TRUE ))

plot(V_2_1)
plot(V_cruts)


mask <- shapefile("Data/Peninsula_Iberica_mask.shp")
mask <- spTransform(mask, "+init=epsg:4326")

projection(V_2_1)
projection(V_cruts)


V_2_1_m <- V_2_1 %>%
  crop(mask) %>%
  mask(mask)
V_2_1_m <- V_2_1_m/10
V_2_1_m <- V_2_1_m - 273.15

plot(V_2_1_m)


V_cruts_m <- V_cruts %>%
  crop(mask) %>%
  mask(mask)
V_cruts_m <- V_cruts_m/10

plot(V_cruts_m)

# ------------------

random_points <- spsample(mask, n=100, type='random')
proj4string(random_points) = CRS("+init=epsg:4326")

random_points@proj4string

plot(random_points)
plot(mask, add=T)
V_2_1_results <- raster::extract(V_2_1_m,
                                 random_points,
                                 df=TRUE)
V_cruts_results <- raster::extract(V_cruts_m,
                                   random_points,
                                   df=TRUE)
results <- mutate(V_2_1_results,V_cruts_results)

# ------------------

m <- lm(results$tasmax_04_2013_V.2.1 ~ results$tmax_4_2013_V.1.0)
m <- summary(m)
ggplot(results, aes(tasmax_04_2013_V.2.1, tmax_4_2013_V.1.0))+
  geom_point()+
  scale_x_continuous("tasmax_04_2013_V.2.1",c(5,10,15,20,25))+
  scale_y_continuous("tmax_4_2013_V.1.0",c(5,10,15,20,25))+
  geom_smooth(se = FALSE, method = lm)+
  geom_text(x = 15, y = 20, label = paste0("R^2 = ", m$adj.r.squared), parse = TRUE)

ggplot(results)+
  geom_density(aes(tasmax_04_2013_V.2.1), fill = "gray80", col= "red", alpha =0.20)+
  geom_density(aes(tmax_4_2013_V.1.0), fill = "gray80", col= "blue", alpha =0.20)+
  theme(axis.title = element_blank())

m <- lm(results$tasmin_12_1989_V.2.1 ~ results$tmin_12_1989_V.1.0)
m <- summary(m)
ggplot(results, aes(tasmin_12_1989_V.2.1, tmin_12_1989_V.1.0))+
  geom_point()+
  scale_x_continuous("tasmin_12_1989_V.2.1",c(5,10,15,20,25))+
  scale_y_continuous("tmin_12_1989_V.1.0",c(5,10,15,20,25))+
  geom_smooth(se = FALSE, method = lm)+
  geom_text(x = 0, y = 10, label = paste0("R^2 = ", m$adj.r.squared), parse = TRUE)

ggplot(results)+
  geom_density(aes(tasmin_12_1989_V.2.1), fill = "gray80", col= "red", alpha =0.20)+
  geom_density(aes(tmin_12_1989_V.1.0), fill = "gray80", col= "blue", alpha =0.20)+
  theme(axis.title = element_blank())

kk <- corLocal(V_2_1_m[[1]],V_cruts_m[[2]])
plot(kk)

data_rep <- "B:/CHELSA_DATA/TMED"



ggplot(results)+
  geom_point(aes(year, V1),col= "red")
geom_smooth(method = "loess", col = "red", se= FALSE)
geom_point(aes(x=results$V2), col= "blue")












data <- raster::stack(list.files("B:/CHELSA_DATA/PCP", full.names = TRUE ))

PCP_annual <- raster::stack()
for (i in 1979:2018){
  raster <- calc(raster::stack(list.files("B:/CHELSA_DATA/PCP", pattern = paste0(i), full.names = TRUE)), sum)
  PCP_annual <- raster::stack(PCP_annual, raster)
}
plot(PCP_annual)

mask <- shapefile("Data/Peninsula_Iberica_mask.shp")
mask <- spTransform(mask, "+init=epsg:4326")

random_points <- spsample(mask, n=3, type='random')
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
