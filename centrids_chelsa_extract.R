# Clean and load packages ----
closeAllConnections()
rm(list=(ls()[ls()!="v"]))
gc(reset=TRUE)


# Cargar las bibliotecas necesarias
library(sf)
library(raster)
library(dplyr)

library(ggplot2)
library(dplyr)
library(tidyr)

# Cargar la capa de puntos
transect_centr <- sf::read_sf("D:/A_MNCN/A_ROB/cent_5_system_WGS84.shp")


# 1901_2016 ----
# Lista de archivos raster
raster_files <- list.files("D:/A_DATA/CHELSA/MONTHLY_1901_2016/TMAX/", pattern = ".tif", full.names = TRUE)

# Convertir a un data frame para añadir valores de ráster fácilmente
transect_centr_df <- as.data.frame(transect_centr)

# Iterar sobre cada ráster y extraer los valores
for (raster_path in raster_files) {
    # Cargar el ráster actual
    raster_layer <- raster(raster_path)
    
    # Extraer el valor del ráster para cada punto
    raster_values <- raster::extract(raster_layer, transect_centr)
    
    # Crear un nombre de columna único basado en el nombre del archivo de ráster, simplificado
    raster_name <- gsub("CHELSAcruts_tmax_|_V.1.0|\\.tif", "", basename(raster_path))
    
    # Añadir la columna con los valores extraídos al data frame
    transect_centr_df[[raster_name]] <- raster_values/10
  }


# Extraer los nombres de las columnas
colnames_original <- colnames(transect_centr_df)

# Filtrar las columnas que corresponden a los datos de temperatura, que empiezan con "tmax"
tmax_cols <- grep("^tmax_\\d+_\\d+$", colnames_original, value = TRUE)

# Extraer mes y año de cada columna en formato numérico
# Dividir las columnas en una tabla con mes y año para cada columna
tmax_split <- do.call(rbind, strsplit(gsub("tmax_", "", tmax_cols), "_"))
mes <- as.numeric(tmax_split[,1])
anio <- as.numeric(tmax_split[,2])

# Crear un data.frame temporal para ordenar mes y año
df_temp <- data.frame(mes = mes, anio = anio, col = tmax_cols)

# Ordenar primero por año, luego por mes
df_temp <- df_temp[order(df_temp$anio, df_temp$mes), ]

# Reordenar las columnas en el orden correcto
col_order <- c(setdiff(colnames_original, tmax_cols), df_temp$col)
transect_centr_df <- transect_centr_df[, col_order]


transect_centr_df_1901_2016 <- transect_centr_df


# 1979_2019 ----
# Lista de archivos raster
raster_files <- list.files("D:/A_DATA/CHELSA/MONTHLY_1979_2019/TMAX/", pattern = ".tif", full.names = TRUE)

# Convertir a un data frame para añadir valores de ráster fácilmente
transect_centr_df <- as.data.frame(transect_centr)

# Iterar sobre cada ráster y extraer los valores
for (raster_path in raster_files) {
  # Cargar el ráster actual
  raster_layer <- raster(raster_path)
  
  # Extraer el valor del ráster para cada punto
  raster_values <- raster::extract(raster_layer, transect_centr)
  
  # Crear un nombre de columna único basado en el nombre del archivo de ráster, simplificado
  raster_name <- gsub("CHELSAcruts_|_V.1.0|\\.tif", "", basename(raster_path))
  
  # Añadir la columna con los valores extraídos al data frame
  transect_centr_df[[raster_name]] <- raster_values
}


# Extraer los nombres de las columnas
colnames_original <- colnames(transect_centr_df)

# Filtrar las columnas que corresponden a los datos de temperatura, que empiezan con "tmax"
tmax_cols <- grep("^tmax_\\d+_\\d+$", colnames_original, value = TRUE)

# Extraer mes y año de cada columna en formato numérico
# Dividir las columnas en una tabla con mes y año para cada columna
tmax_split <- do.call(rbind, strsplit(gsub("tmax_", "", tmax_cols), "_"))
mes <- as.numeric(tmax_split[,1])
anio <- as.numeric(tmax_split[,2])

# Crear un data.frame temporal para ordenar mes y año
df_temp <- data.frame(mes = mes, anio = anio, col = tmax_cols)

# Ordenar primero por año, luego por mes
df_temp <- df_temp[order(df_temp$anio, df_temp$mes), ]

# Reordenar las columnas en el orden correcto
col_order <- c(setdiff(colnames_original, tmax_cols), df_temp$col)
transect_centr_df <- transect_centr_df[, col_order]


transect_centr_df_1979_2019 <- transect_centr_df





# plotear----

transect_centr_df_1901_2016$Name

# Filtrar la fila donde Name es "Astún"
tran_data_1901_2016 <- transect_centr_df_1901_2016 %>%
  filter(Name == "Las Tiesas") %>%
  pivot_longer(cols = 6:ncol(transect_centr_df_1901_2016),  # Selecciona solo columnas que comienzan con "tmax" (asumiendo que están desde la columna 6)
               names_to = "Fecha",
               values_to = "Temperatura")

# Separar la columna "Fecha" en mes y año
tran_data_1901_2016 <- tran_data_1901_2016 %>%
  separate(Fecha, into = c("mes", "anio"), sep = "_", convert = TRUE) %>%
  mutate(fecha = as.Date(paste(anio, mes, "01", sep = "-"))) %>%
  arrange(fecha)



# Filtrar la fila donde Name es "Astún"
tran_data_1979_2019 <- transect_centr_df_1979_2019 %>%
  filter(Name == "Las Tiesas") %>%
  pivot_longer(cols = 6:ncol(transect_centr_df_1979_2019),  # Selecciona solo columnas que comienzan con "tmax" (asumiendo que están desde la columna 6)
               names_to = "Fecha",
               values_to = "Temperatura")

# Separar la columna "Fecha" en mes y año
tran_data_1979_2019 <- tran_data_1979_2019 %>%
  separate(Fecha, into = c("anio", "mes"), sep = "_", convert = TRUE) %>%
  mutate(fecha = as.Date(paste(anio, mes, "01", sep = "-"))) %>%
  arrange(fecha)

# Graficar las temperaturas frente a las fechas
ggplot() +
  geom_line(data=tran_data_1901_2016, aes(x = fecha, y = Temperatura),color = "red") +
  geom_line(data=tran_data_1979_2019, aes(x = fecha, y = Temperatura),color = "blue") +
  labs(x = "Fecha",
       y = "Temperatura (°C)") +
  theme_minimal()

write.csv2(transect_centr_df_1901_2016,"C:/A_TRABAJO/ELIZA/CHELSA_transectos/transect_centr_df_1901_2016.csv")
write.csv2(transect_centr_df_1979_2019,"C:/A_TRABAJO/ELIZA/CHELSA_transectos/transect_centr_df_1979_2019.csv")
