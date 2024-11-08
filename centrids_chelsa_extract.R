# Clean and load packages ----
closeAllConnections()
rm(list=(ls()[ls()!="v"]))
gc(reset=TRUE)

# Cargar bibliotecas necesarias
library(sf)
library(raster)
library(tidyverse)

# Cargar la capa de puntos
transect_centr <- sf::read_sf("D:/A_MNCN/A_ROB/cent_5_system_WGS84.shp")

# Procesar para 1901_2016 ----
# Lista de archivos raster
raster_files_1901_2016 <- list.files("D:/A_DATA/CHELSA/MONTHLY_1901_2016/TMAX/", pattern = ".tif", full.names = TRUE)

# Convertir a un data frame
transect_centr_df <- as.data.frame(transect_centr)

# Extraer valores de cada ráster y crear nombres de columna uniformes
for (raster_path in raster_files_1901_2016) {
  # Cargar ráster
  raster_layer <- raster(raster_path)
  
  # Extraer valores del ráster para cada punto
  raster_values <- raster::extract(raster_layer, transect_centr)
  
  # Crear nombre de columna con formato MM_YYYY
  raster_name <- gsub("CHELSAcruts_tmax_|_V.1.0|\\.tif", "", basename(raster_path))
  split_name <- unlist(strsplit(raster_name, "_"))
  mes <- sprintf("%02d", as.numeric(split_name[1]))  # Asegurar formato de dos dígitos
  anio <- split_name[2]
  transect_centr_df[[paste0(mes, "_", anio)]] <- raster_values / 10
}

# Extraer los nombres de las columnas
colnames_original <- colnames(transect_centr_df)

# Filtrar las columnas que contienen datos de temperatura, que empiezan con el formato `MM_YYYY`
cols <- grep("^[0-9]{2}_[0-9]{4}$", colnames_original, value = TRUE)

# Extraer mes y año de cada columna en formato numérico
temp_split <- do.call(rbind, strsplit(cols, "_"))
mes <- as.numeric(temp_split[, 1])
anio <- as.numeric(temp_split[, 2])

# Crear un data.frame temporal para ordenar por año y mes
df_temp <- data.frame(mes = mes, anio = anio, col = cols)

# Ordenar primero por año, luego por mes
df_temp <- df_temp[order(df_temp$anio, df_temp$mes), ]

# Reordenar las columnas en el orden correcto
col_order <- c(setdiff(colnames_original, cols), df_temp$col)
transect_centr_df <- transect_centr_df[, col_order]

# Guardar en un nuevo data frame
transect_centr_df_1901_2016 <- transect_centr_df

# Limpiar el entorno para evitar conflictos
rm(list = setdiff(ls(), c("transect_centr_df_1901_2016", "transect_centr")))


# Procesar para 1979_2019 ----
# Lista de archivos raster
raster_files_1979_2019 <- list.files("D:/A_DATA/CHELSA/MONTHLY_1979_2019/TMAX/", pattern = ".tif", full.names = TRUE)

# Convertir a un data frame
transect_centr_df <- as.data.frame(transect_centr)

# Extraer valores de cada ráster y crear nombres de columna uniformes
for (raster_path in raster_files_1979_2019) {
  # Cargar ráster
  raster_layer <- raster(raster_path)
  
  # Extraer valores del ráster para cada punto
  raster_values <- raster::extract(raster_layer, transect_centr)
  
  # Crear nombre de columna con formato MM_YYYY
  raster_name <- gsub("\\.tif", "", basename(raster_path))
  split_name <- unlist(strsplit(raster_name, "_"))
  anio <- split_name[1]
  mes <- sprintf("%02d", as.numeric(split_name[2]))  # Asegurar formato de dos dígitos
  transect_centr_df[[paste0(mes, "_", anio)]] <- raster_values
}

# Extraer los nombres de las columnas
colnames_original <- colnames(transect_centr_df)

# Filtrar las columnas que contienen datos de temperatura, que empiezan con el formato `MM_YYYY`
cols <- grep("^[0-9]{2}_[0-9]{4}$", colnames_original, value = TRUE)

# Extraer mes y año de cada columna en formato numérico
temp_split <- do.call(rbind, strsplit(cols, "_"))
mes <- as.numeric(temp_split[, 1])
anio <- as.numeric(temp_split[, 2])

# Crear un data.frame temporal para ordenar por año y mes
df_temp <- data.frame(mes = mes, anio = anio, col = cols)

# Ordenar primero por año, luego por mes
df_temp <- df_temp[order(df_temp$anio, df_temp$mes), ]

# Reordenar las columnas en el orden correcto
col_order <- c(setdiff(colnames_original, cols), df_temp$col)
transect_centr_df <- transect_centr_df[, col_order]

# Guardar en un nuevo data frame
transect_centr_df_1979_2019 <- transect_centr_df

# Limpiar el entorno y conservar solo los objetos deseados
rm(list = setdiff(ls(), c("transect_centr_df_1901_2016", "transect_centr", "transect_centr_df_1979_2019")))


write.csv2(transect_centr_df_1901_2016,"C:/A_TRABAJO/ELIZA/CHELSA_transectos/transect_centr_df_1901_2016.csv",  
           row.names = FALSE, fileEncoding = "ISO-8859-1")
write.csv2(transect_centr_df_1979_2019,"C:/A_TRABAJO/ELIZA/CHELSA_transectos/transect_centr_df_1979_2019.csv",  
           row.names = FALSE, fileEncoding = "ISO-8859-1")



# Crear un data.frame reestructurado en formato largo para análisis
transect_long_df <- transect_centr_df_1901_2016 %>%
  pivot_longer(cols = all_of(transect_centr_df_1901_2016),
               names_to = "Fecha",
               values_to = "Temperatura") %>%
  mutate(mes = as.numeric(sub("_.*", "", Fecha)),
         anio = as.numeric(sub("^[0-9]+_", "", Fecha))) %>%
  select(-Fecha) # eliminar la columna original "Fecha" si no es necesaria

# Verificar el resultado
head(transect_long_df)

# Crear un data.frame temporal para ordenar mes y año
df_temp <- data.frame(mes = mes, anio = anio, col = tmax_cols)

# Ordenar primero por año, luego por mes
df_temp <- df_temp[order(df_temp$anio, df_temp$mes), ]

# Reordenar las columnas en el orden correcto
col_order <- c(setdiff(colnames_original, tmax_cols), df_temp$col)
transect_centr_df <- transect_centr_df[, col_order]
colnames(transect_centr_df) <- 

transect_centr_df_1979_2019 <- transect_centr_df





# Comparación periodo común (1980-2016) ----

pivot_df_1901_2016 <- transect_centr_df_1901_2016 %>%
  pivot_longer(cols = 6:ncol(transect_centr_df_1901_2016),  # Selecciona solo columnas que comienzan con "tmax" (asumiendo que están desde la columna 6)
               names_to = "Fecha",
               values_to = "Temperatura") %>%
  separate(Fecha, into = c("mes", "anio"), sep = "_", convert = TRUE) %>%
  mutate(fecha = as.Date(paste(anio, mes, "01", sep = "-"))) %>%
  arrange(fecha) %>% 
  filter(anio >= "1980" & anio <= "2016")

pivot_df_1979_2019 <- transect_centr_df_1979_2019 %>%
  pivot_longer(cols = 6:ncol(transect_centr_df_1979_2019),  
               names_to = "Fecha",
               values_to = "Temperatura") %>%
  separate(Fecha, into = c("mes", "anio"), sep = "_", convert = TRUE) %>%
  mutate(fecha = as.Date(paste(anio, mes, "01", sep = "-"))) %>%
  arrange(fecha)%>% 
  filter(anio >= "1980" & anio <= "2016")

summary(pivot_df_1901_2016$Temperatura)
summary(pivot_df_1979_2019$Temperatura)
cor(x=pivot_df_1901_2016$Temperatura, y=pivot_df_1979_2019$Temperatura)

# Regiones ----
#Ver todos las regiones
unique(transect_centr_df_1901_2016$Region)

# Filtrar por nombre de region
filtro_df_1901_2016 <- pivot_df_1901_2016 %>% 
  filter(Region == "Gredos")
filtro_df_1979_2019 <- pivot_df_1979_2019 %>% 
  filter(Region == "Gredos")


ggplot() +
  geom_line(data=filtro_df_1901_2016, aes(x = fecha, y = Temperatura),color = "red", alpha = .5) +
  geom_line(data=filtro_df_1979_2019, aes(x = fecha, y = Temperatura),color = "blue", alpha = .5) +
  geom_smooth(data=filtro_df_1901_2016, aes(x = fecha, y = Temperatura),color = "red", fill = "red", alpha = .5) +
  geom_smooth(data=filtro_df_1979_2019, aes(x = fecha, y = Temperatura),color = "blue", fill = "blue", alpha = .5) +
  labs(title = paste0(filtro_df_1901_2016$Region),
       x = "Fecha",
       y = "Temperatura (°C)") +
  theme_minimal()

summary(filtro_df_1901_2016$Temperatura)
summary(filtro_df_1979_2019$Temperatura)
cor(x=filtro_df_1901_2016$Temperatura, y=filtro_df_1979_2019$Temperatura)

# Transectos ----
#Ver todos los transectos
unique(transect_centr_df_1901_2016$Name)

# Filtrar por nombre de transecto
filtro_df_1901_2016 <- pivot_df_1901_2016 %>% 
filter(Name == "TRG")
filtro_df_1979_2019 <- pivot_df_1979_2019 %>% 
filter(Name == "TRG")

filtro_df_1901_2016 <- pivot_df_1901_2016 %>% 
  filter(Region == "Pirineos")
filtro_df_1979_2019 <- pivot_df_1979_2019 %>% 
  filter(Region == "Pirineos")


ggplot() +
  geom_line(data=filtro_df_1901_2016, aes(x = fecha, y = Temperatura),color = "red", alpha = .5) +
  geom_line(data=filtro_df_1979_2019, aes(x = fecha, y = Temperatura),color = "blue", alpha = .5) +
  geom_smooth(data=filtro_df_1901_2016, aes(x = fecha, y = Temperatura),color = "red", fill = "red", alpha = .5) +
  geom_smooth(data=filtro_df_1979_2019, aes(x = fecha, y = Temperatura),color = "blue", fill = "blue", alpha = .5) +
  labs(title = paste0(filtro_df_1901_2016$Name),
       subtitle = paste0(filtro_df_1901_2016$Region),
       x = "Fecha",
       y = "Temperatura (°C)") +
  theme_minimal()

summary(filtro_df_1901_2016$Temperatura)
summary(filtro_df_1979_2019$Temperatura)
cor(x=filtro_df_1901_2016$Temperatura, y=filtro_df_1979_2019$Temperatura)





