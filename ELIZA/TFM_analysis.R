library(tidyverse)
UTM_transectos <- readxl::read_xlsx("C:/A_TRABAJO/ELIZA/DATA/TRANSECTOS/UTM_transectos_pirineos_2024.xlsx")
# TMAX ----
climate_data_P1 <- readxl::read_xlsx("C:/A_TRABAJO/ELIZA/DATA/CLIMA/Transectos_Chelsa.xlsx", sheet = "tmax_transect_centr_1901_2016")
Tmax_P1 <- left_join(UTM_transectos, climate_data_P1, by = "Name")
Tmax_P1 <- Tmax_P1[, c(1:6, 775:894)]

climate_data_P2 <- readxl::read_xlsx("C:/A_TRABAJO/ELIZA/DATA/CLIMA/Transectos_Chelsa.xlsx", sheet = "tmax_transect_centr_1979_2019")
Tmax_P2 <- left_join(UTM_transectos, climate_data_P2, by = "Name")
Tmax_P2 <- Tmax_P2[, c(1:6, 378:497)]

p1_mean <- rowMeans(Tmax_P1[, 7:length(Tmax_P1)])
p2_mean <- rowMeans(Tmax_P2[, 7:length(Tmax_P2)])

res <- cbind(UTM_transectos, "mean_p1" = round(p1_mean, 2), "mean_p2" = round(p2_mean, 2))
