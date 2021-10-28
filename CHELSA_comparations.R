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
