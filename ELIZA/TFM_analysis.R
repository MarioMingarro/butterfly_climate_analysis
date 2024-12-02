library(tidyverse)
UTM_transectos <- readxl::read_xlsx("C:/A_TRABAJO/ELIZA/DATA/TRANSECTOS/UTM_transectos_pirineos_2024.xlsx")
# TMAX ----
climate_data_P1 <- readxl::read_xlsx("C:/A_TRABAJO/ELIZA/DATA/CLIMA/Transectos_Chelsa.xlsx", sheet = "tmax_transect_centr_1901_2016")
Tmax_P1 <- left_join(UTM_transectos, climate_data_P1, by = "Name")
Tmax_P1 <- Tmax_P1[, c(1:6, 775:894)]

climate_data_P2 <- readxl::read_xlsx("C:/A_TRABAJO/ELIZA/DATA/CLIMA/Transectos_Chelsa.xlsx", sheet = "tmax_transect_centr_1979_2019")
Tmax_P2 <- left_join(UTM_transectos, climate_data_P2, by = "Name")
Tmax_P2 <- Tmax_P2[, c(1:6, 378:497)]

p1_mean_max <- rowMeans(Tmax_P1[, 7:length(Tmax_P1)])
p2_mean_max <- rowMeans(Tmax_P2[, 7:length(Tmax_P2)])

res_max <- cbind(UTM_transectos, "mean_p1_max" = round(p1_mean_max, 2), "mean_p2_max" = round(p2_mean_max, 2))

wilcox.test(res_max$mean_p1_max, res_max$mean_p2_max, paired= T)
res_m_max <- reshape2::melt(res_max)

ggplot(res_m_max, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot()

# TMIN ----
climate_data_P1 <- readxl::read_xlsx("C:/A_TRABAJO/ELIZA/DATA/CLIMA/Transectos_Chelsa.xlsx", sheet = "tmin_transect_centr_1901_2016")
Tmin_P1 <- left_join(UTM_transectos, climate_data_P1, by = "Name")
Tmin_P1 <- Tmin_P1[, c(1:6, 775:894)]

climate_data_P2 <- readxl::read_xlsx("C:/A_TRABAJO/ELIZA/DATA/CLIMA/Transectos_Chelsa.xlsx", sheet = "tmin_transect_centr_1979_2019")
Tmin_P2 <- left_join(UTM_transectos, climate_data_P2, by = "Name")
Tmin_P2 <- Tmin_P2[, c(1:6, 378:497)]

p1_mean_min <- rowMeans(Tmin_P1[, 7:length(Tmin_P1)])
p2_mean_min <- rowMeans(Tmin_P2[, 7:length(Tmin_P2)])

res_min <- cbind(UTM_transectos, "mean_p1_min" = round(p1_mean_min, 2), "mean_p2_min" = round(p2_mean_min, 2))

wilcox.test(res_min$mean_p1_min, res_min$mean_p2_min, paired= T)
res_m_min <- reshape2::melt(res_min)

ggplot() +
  geom_boxplot(data=res_m_max, aes(x = variable, y = value, fill = variable))+
  geom_boxplot(data=res_m_min, aes(x = variable, y = value, fill = variable))
