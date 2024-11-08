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
# TMAX----
## Procesar para 1901_2016 ----
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


## Procesar para 1979_2019 ----
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


write.csv2(transect_centr_df_1901_2016,"C:/A_TRABAJO/ELIZA/CHELSA_transectos/tmax_transect_centr_df_1901_2016.csv",  
           row.names = FALSE, fileEncoding = "ISO-8859-1")
write.csv2(transect_centr_df_1979_2019,"C:/A_TRABAJO/ELIZA/CHELSA_transectos/tmax_transect_centr_df_1979_2019.csv",  
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





## Comparación periodo común (1980-2016) ----

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

## Regiones ----
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
  geom_smooth(data=filtro_df_1901_2016, aes(x = fecha, y = Temperatura),method = lm, color = "red", fill = "red", alpha = .5) +
  geom_smooth(data=filtro_df_1979_2019, aes(x = fecha, y = Temperatura),method = lm, color = "blue", fill = "blue", alpha = .5) +
  labs(title = paste0(filtro_df_1901_2016$Region),
       x = "Fecha",
       y = "Temperatura (°C)") +
  theme_minimal()

summary(filtro_df_1901_2016$Temperatura)
summary(filtro_df_1979_2019$Temperatura)
cor(x=filtro_df_1901_2016$Temperatura, y=filtro_df_1979_2019$Temperatura)

### Todas las regiones----
resultados_fin <- data.frame(
  Region = "a",
  Periodo = "a",
  Min = 1,
  Q1 = 1,
  Median = 1,
  Mean = 1,
  Q3 = 1,
  Max = 1,
  Correlacion = 1,
  stringsAsFactors = FALSE
)

# Lista de regiones
regiones <- unique(transect_centr_df_1901_2016$Region)

# Bucle para iterar sobre cada región
for (region in regiones) {
  resultados <- data.frame(
    Region = "a",
    Periodo = "a",
    Min = 1,
    Q1 = 1,
    Median = 1,
    Mean = 1,
    Q3 = 1,
    Max = 1,
    Correlacion = 1,
    stringsAsFactors = FALSE
  )
  # Filtrar los datos de cada región para los periodos 1901-2016 y 1979-2019
  filtro_df_1901_2016 <- subset(pivot_df_1901_2016, Region == region)
  filtro_df_1979_2019 <- subset(pivot_df_1979_2019, Region == region)
  
  # Calcular resumen estadístico para cada periodo
  resumen_1901_2016 <- summary(filtro_df_1901_2016$Temperatura)
  resumen_1979_2019 <- summary(filtro_df_1979_2019$Temperatura)
  
  # Extraer las estadísticas de los resúmenes de forma robusta usando nombres
  
  min_1901_2016 <- resumen_1901_2016["Min."]
  q1_1901_2016 <- resumen_1901_2016["1st Qu."]
  median_1901_2016 <- resumen_1901_2016["Median"]
  mean_1901_2016 <- resumen_1901_2016["Mean"]
  q3_1901_2016 <- resumen_1901_2016["3rd Qu."]
  max_1901_2016 <- resumen_1901_2016["Max."]
  
  min_1979_2019 <- resumen_1979_2019["Min."]
  q1_1979_2019 <- resumen_1979_2019["1st Qu."]
  median_1979_2019 <- resumen_1979_2019["Median"]
  mean_1979_2019 <- resumen_1979_2019["Mean"]
  q3_1979_2019 <- resumen_1979_2019["3rd Qu."]
  max_1979_2019 <- resumen_1979_2019["Max."]
  
  # Calcular la correlación entre los dos periodos
  correlacion <- cor(filtro_df_1901_2016$Temperatura, filtro_df_1979_2019$Temperatura)
  
  # Añadir los resultados de 1901-2016
  
  resultados[1,1] = region
  resultados[1,2] = "1901-2016"
  resultados[1,3]  = min_1901_2016
  resultados[1,4] = q1_1901_2016
  resultados[1,5] = median_1901_2016
  resultados[1,6] = mean_1901_2016
  resultados[1,7] = q3_1901_2016
  resultados[1,8] = max_1901_2016
  resultados[1,9] = correlacion
  resultados[2,1] = region
  resultados[2,2] = "1979_2019"
  resultados[2,3]  = min_1979_2019
  resultados[2,4] = q1_1979_2019
  resultados[2,5] = median_1979_2019
  resultados[2,6] = mean_1979_2019
  resultados[2,7] = q3_1979_2019
  resultados[2,8] = max_1979_2019
  
  resultados_fin <- rbind(resultados_fin, resultados)
}

resultados_fin <- resultados_fin[-1,]


## Transectos ----
#Ver todos los transectos
unique(transect_centr_df_1901_2016$Name)

# Filtrar por nombre de transecto
filtro_df_1901_2016 <- pivot_df_1901_2016 %>% 
  filter(Name == "TRG")
filtro_df_1979_2019 <- pivot_df_1979_2019 %>% 
  filter(Name == "TRG")


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


### Todos los transectos ----
resultados_fin <- data.frame(
  Transect = "a",
  Periodo = "a",
  Min = 1,
  Q1 = 1,
  Median = 1,
  Mean = 1,
  Q3 = 1,
  Max = 1,
  Correlacion = 1,
  stringsAsFactors = FALSE
)

# Lista de regiones
transects <- unique(transect_centr_df_1901_2016$Name)

# Bucle para iterar sobre cada región
for (aa in transects) {
  resultados <- data.frame(
    Transect = "a",
    Periodo = "a",
    Min = 1,
    Q1 = 1,
    Median = 1,
    Mean = 1,
    Q3 = 1,
    Max = 1,
    Correlacion = 1,
    stringsAsFactors = FALSE
  )
  # Filtrar los datos de cada región para los periodos 1901-2016 y 1979-2019
  filtro_df_1901_2016 <- subset(pivot_df_1901_2016, Name == aa)
  filtro_df_1979_2019 <- subset(pivot_df_1979_2019, Name == aa)
  
  # Calcular resumen estadístico para cada periodo
  resumen_1901_2016 <- summary(filtro_df_1901_2016$Temperatura)
  resumen_1979_2019 <- summary(filtro_df_1979_2019$Temperatura)
  
  # Extraer las estadísticas de los resúmenes de forma robusta usando nombres
  
  min_1901_2016 <- resumen_1901_2016["Min."]
  q1_1901_2016 <- resumen_1901_2016["1st Qu."]
  median_1901_2016 <- resumen_1901_2016["Median"]
  mean_1901_2016 <- resumen_1901_2016["Mean"]
  q3_1901_2016 <- resumen_1901_2016["3rd Qu."]
  max_1901_2016 <- resumen_1901_2016["Max."]
  
  min_1979_2019 <- resumen_1979_2019["Min."]
  q1_1979_2019 <- resumen_1979_2019["1st Qu."]
  median_1979_2019 <- resumen_1979_2019["Median"]
  mean_1979_2019 <- resumen_1979_2019["Mean"]
  q3_1979_2019 <- resumen_1979_2019["3rd Qu."]
  max_1979_2019 <- resumen_1979_2019["Max."]
  
  # Calcular la correlación entre los dos periodos
  correlacion <- cor(filtro_df_1901_2016$Temperatura, filtro_df_1979_2019$Temperatura)
  
  # Añadir los resultados de 1901-2016
  
  resultados[1,1] = aa
  resultados[1,2] = "1901-2016"
  resultados[1,3]  = min_1901_2016
  resultados[1,4] = q1_1901_2016
  resultados[1,5] = median_1901_2016
  resultados[1,6] = mean_1901_2016
  resultados[1,7] = q3_1901_2016
  resultados[1,8] = max_1901_2016
  resultados[1,9] = correlacion
  resultados[2,1] = aa
  resultados[2,2] = "1979_2019"
  resultados[2,3]  = min_1979_2019
  resultados[2,4] = q1_1979_2019
  resultados[2,5] = median_1979_2019
  resultados[2,6] = mean_1979_2019
  resultados[2,7] = q3_1979_2019
  resultados[2,8] = max_1979_2019
  
  resultados_fin <- rbind(resultados_fin, resultados)
}

resultados_fin <- resultados_fin[-1,]

# TMIN----
## Procesar para 1901_2016 ----
# Lista de archivos raster
raster_files_1901_2016 <- list.files("D:/A_DATA/CHELSA/MONTHLY_1901_2016/TMIN/", pattern = ".tif", full.names = TRUE)

# Convertir a un data frame
transect_centr_df <- as.data.frame(transect_centr)

# Extraer valores de cada ráster y crear nombres de columna uniformes
for (raster_path in raster_files_1901_2016) {
  # Cargar ráster
  raster_layer <- raster(raster_path)
  
  # Extraer valores del ráster para cada punto
  raster_values <- raster::extract(raster_layer, transect_centr)
  
  # Crear nombre de columna con formato MM_YYYY
  raster_name <- gsub("CHELSAcruts_tmin_|_V.1.0|\\.tif", "", basename(raster_path))
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


## Procesar para 1979_2019 ----
# Lista de archivos raster
raster_files_1979_2019 <- list.files("D:/A_DATA/CHELSA/MONTHLY_1979_2019/TMIN/", pattern = ".tif", full.names = TRUE)

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


write.csv2(transect_centr_df_1901_2016,"C:/A_TRABAJO/ELIZA/CHELSA_transectos/tmin_transect_centr_df_1901_2016.csv",  
           row.names = FALSE, fileEncoding = "ISO-8859-1")
write.csv2(transect_centr_df_1979_2019,"C:/A_TRABAJO/ELIZA/CHELSA_transectos/tmin_transect_centr_df_1979_2019.csv",  
           row.names = FALSE, fileEncoding = "ISO-8859-1")







## Comparación periodo común (1980-2016) ----

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

## Regiones ----
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
  geom_smooth(data=filtro_df_1901_2016, aes(x = fecha, y = Temperatura),method = lm, color = "red", fill = "red", alpha = .5) +
  geom_smooth(data=filtro_df_1979_2019, aes(x = fecha, y = Temperatura),method = lm, color = "blue", fill = "blue", alpha = .5) +
  labs(title = paste0(filtro_df_1901_2016$Region),
       x = "Fecha",
       y = "Temperatura (°C)") +
  theme_minimal()

summary(filtro_df_1901_2016$Temperatura)
summary(filtro_df_1979_2019$Temperatura)
cor(x=filtro_df_1901_2016$Temperatura, y=filtro_df_1979_2019$Temperatura)

### Todas las regiones----
resultados_fin <- data.frame(
  Region = "a",
  Periodo = "a",
  Min = 1,
  Q1 = 1,
  Median = 1,
  Mean = 1,
  Q3 = 1,
  Max = 1,
  Correlacion = 1,
  stringsAsFactors = FALSE
)

# Lista de regiones
regiones <- unique(transect_centr_df_1901_2016$Region)

# Bucle para iterar sobre cada región
for (region in regiones) {
  resultados <- data.frame(
    Region = "a",
    Periodo = "a",
    Min = 1,
    Q1 = 1,
    Median = 1,
    Mean = 1,
    Q3 = 1,
    Max = 1,
    Correlacion = 1,
    stringsAsFactors = FALSE
  )
  # Filtrar los datos de cada región para los periodos 1901-2016 y 1979-2019
  filtro_df_1901_2016 <- subset(pivot_df_1901_2016, Region == region)
  filtro_df_1979_2019 <- subset(pivot_df_1979_2019, Region == region)
  
  # Calcular resumen estadístico para cada periodo
  resumen_1901_2016 <- summary(filtro_df_1901_2016$Temperatura)
  resumen_1979_2019 <- summary(filtro_df_1979_2019$Temperatura)
  
  # Extraer las estadísticas de los resúmenes de forma robusta usando nombres
  
  min_1901_2016 <- resumen_1901_2016["Min."]
  q1_1901_2016 <- resumen_1901_2016["1st Qu."]
  median_1901_2016 <- resumen_1901_2016["Median"]
  mean_1901_2016 <- resumen_1901_2016["Mean"]
  q3_1901_2016 <- resumen_1901_2016["3rd Qu."]
  max_1901_2016 <- resumen_1901_2016["Max."]
  
  min_1979_2019 <- resumen_1979_2019["Min."]
  q1_1979_2019 <- resumen_1979_2019["1st Qu."]
  median_1979_2019 <- resumen_1979_2019["Median"]
  mean_1979_2019 <- resumen_1979_2019["Mean"]
  q3_1979_2019 <- resumen_1979_2019["3rd Qu."]
  max_1979_2019 <- resumen_1979_2019["Max."]
  
  # Calcular la correlación entre los dos periodos
  correlacion <- cor(filtro_df_1901_2016$Temperatura, filtro_df_1979_2019$Temperatura)
  
  # Añadir los resultados de 1901-2016
  
  resultados[1,1] = region
  resultados[1,2] = "1901-2016"
  resultados[1,3]  = min_1901_2016
  resultados[1,4] = q1_1901_2016
  resultados[1,5] = median_1901_2016
  resultados[1,6] = mean_1901_2016
  resultados[1,7] = q3_1901_2016
  resultados[1,8] = max_1901_2016
  resultados[1,9] = correlacion
  resultados[2,1] = region
  resultados[2,2] = "1979_2019"
  resultados[2,3]  = min_1979_2019
  resultados[2,4] = q1_1979_2019
  resultados[2,5] = median_1979_2019
  resultados[2,6] = mean_1979_2019
  resultados[2,7] = q3_1979_2019
  resultados[2,8] = max_1979_2019
  
  resultados_fin <- rbind(resultados_fin, resultados)
}

resultados_fin <- resultados_fin[-1,]


## Transectos ----
#Ver todos los transectos
unique(transect_centr_df_1901_2016$Name)

# Filtrar por nombre de transecto
filtro_df_1901_2016 <- pivot_df_1901_2016 %>% 
  filter(Name == "TRG")
filtro_df_1979_2019 <- pivot_df_1979_2019 %>% 
  filter(Name == "TRG")


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


### Todos los transectos ----
resultados_fin <- data.frame(
  Transect = "a",
  Periodo = "a",
  Min = 1,
  Q1 = 1,
  Median = 1,
  Mean = 1,
  Q3 = 1,
  Max = 1,
  Correlacion = 1,
  stringsAsFactors = FALSE
)

# Lista de regiones
transects <- unique(transect_centr_df_1901_2016$Name)

# Bucle para iterar sobre cada región
for (aa in transects) {
  resultados <- data.frame(
    Transect = "a",
    Periodo = "a",
    Min = 1,
    Q1 = 1,
    Median = 1,
    Mean = 1,
    Q3 = 1,
    Max = 1,
    Correlacion = 1,
    stringsAsFactors = FALSE
  )
  # Filtrar los datos de cada región para los periodos 1901-2016 y 1979-2019
  filtro_df_1901_2016 <- subset(pivot_df_1901_2016, Name == aa)
  filtro_df_1979_2019 <- subset(pivot_df_1979_2019, Name == aa)
  
  # Calcular resumen estadístico para cada periodo
  resumen_1901_2016 <- summary(filtro_df_1901_2016$Temperatura)
  resumen_1979_2019 <- summary(filtro_df_1979_2019$Temperatura)
  
  # Extraer las estadísticas de los resúmenes de forma robusta usando nombres
  
  min_1901_2016 <- resumen_1901_2016["Min."]
  q1_1901_2016 <- resumen_1901_2016["1st Qu."]
  median_1901_2016 <- resumen_1901_2016["Median"]
  mean_1901_2016 <- resumen_1901_2016["Mean"]
  q3_1901_2016 <- resumen_1901_2016["3rd Qu."]
  max_1901_2016 <- resumen_1901_2016["Max."]
  
  min_1979_2019 <- resumen_1979_2019["Min."]
  q1_1979_2019 <- resumen_1979_2019["1st Qu."]
  median_1979_2019 <- resumen_1979_2019["Median"]
  mean_1979_2019 <- resumen_1979_2019["Mean"]
  q3_1979_2019 <- resumen_1979_2019["3rd Qu."]
  max_1979_2019 <- resumen_1979_2019["Max."]
  
  # Calcular la correlación entre los dos periodos
  correlacion <- cor(filtro_df_1901_2016$Temperatura, filtro_df_1979_2019$Temperatura)
  
  # Añadir los resultados de 1901-2016
  
  resultados[1,1] = aa
  resultados[1,2] = "1901-2016"
  resultados[1,3]  = min_1901_2016
  resultados[1,4] = q1_1901_2016
  resultados[1,5] = median_1901_2016
  resultados[1,6] = mean_1901_2016
  resultados[1,7] = q3_1901_2016
  resultados[1,8] = max_1901_2016
  resultados[1,9] = correlacion
  resultados[2,1] = aa
  resultados[2,2] = "1979_2019"
  resultados[2,3]  = min_1979_2019
  resultados[2,4] = q1_1979_2019
  resultados[2,5] = median_1979_2019
  resultados[2,6] = mean_1979_2019
  resultados[2,7] = q3_1979_2019
  resultados[2,8] = max_1979_2019
  
  resultados_fin <- rbind(resultados_fin, resultados)
}

resultados_fin <- resultados_fin[-1,]

