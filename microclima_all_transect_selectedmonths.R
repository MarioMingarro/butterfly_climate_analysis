library(foreach)
library(doMC)
library(microclima)
library(tidyverse)
library(NicheMapR)

registerDoMC(48) # registra MC y le informa de que se dispone de 48 nucleos

cent <- data.frame("long" = c(-5.070127436,-5.170109927,-5.239909025,-5.49,-5.110012635,-5.270092418,-5.300006317,-4.910047653,-5.02046239,
                             -5.071395802,-5.118995159,-3.979948917,-3.909880325,-4.15,-3.840081227,-3.909880325,-3.461602399,-3.492211683,
                             -3.643791341,-3.46054724,-3.636635796,-3.522429683,-3.716501122,-3.553333075,-3.776362664,-3.569538531,
                             -3.491154653,-3.805420754,-3.811682227,-3.475079266,-3.571360808,-3.502564948,-3.858955442,-3.654592381,
                             -3.708193609,-3.688713541,-3.76757135,-3.807165188,-3.858493912,-3.770682191,-3.964844458,-4.100252729,
                             -4.114863092,-4.003498569,-4.193231034,-4.170503626,-4.166874919,-4.161933971,-4.242243283,-4.110713378,
                             -4.196318172,-3.819948654,-3.755618179,-4.038015384,-3.970644646,-4.018350586,-4.055822103,-3.89998228,
                             -4.032411612,-1.030095003,-1.019353448,-2.069926534,-1.940030144,-1.400052527,-2.030041335,-1.7,-1.58007491,
                             -1.830076353,-1.359897833,-1.910116245,-1.609988809,-1.7,-1.380109927,-1.920087544,-1.480092418,-2.040012635,
                             -2.42,-2.359902707,-1.869961552,-2.049983934, -4.025419),
                   "lat" = c(40.15999689,40.18005069,40.26990924,40.28002371,40.34995755,40.36005994,40.42002435,40.44000053,40.31992738,
                              40.29049265,40.26007377,40.65999688,40.68995348,40.70000436,40.89006723,40.92993528,41.19929815,41.09175647,
                              41.08914862,40.97975073,40.97605089,40.95029474,40.93776192,40.87971744,40.88064552,41.03713895,41.05118584,
                              40.99370849,40.92851811,40.87650336,40.82704562,40.84031848,40.99287339,40.86873882,40.86779651,40.81028852,
                              40.84041915,40.83888717,40.81249331,40.78807136,40.75240797,40.81424523,40.74254605,40.7095287,40.65945886,
                              40.61261721,40.54782187,40.49531165,40.44823507,40.43908453,40.39199006,40.66628591,40.70748324,40.80979262,
                              40.7925944,40.75099596,40.76685855,40.83815886,40.88532513,40.14985318,40.11055409,40.09999689,40.21000718,
                              40.26999689,40.28992059,40.31993508,40.31993508,40.34005016,40.39992364,40.40992134,40.41001848,40.41990583,
                              40.46005813,40.47992528,40.49998053,40.5500613,40.58006761,40.64003954,40.65994972,40.87999829, 40.867588),
                   "zone" = c("Gredos","Gredos","Gredos","Gredos","Gredos","Gredos","Gredos","Gredos","Gredos","Gredos","Gredos","Guadarrama",
                              "Guadarrama","Guadarrama","Guadarrama","Guadarrama","Guadarrama","Guadarrama","Guadarrama","Guadarrama",
                              "Guadarrama","Guadarrama","Guadarrama","Guadarrama","Guadarrama","Guadarrama","Guadarrama","Guadarrama",
                              "Guadarrama","Guadarrama","Guadarrama","Guadarrama","Guadarrama","Guadarrama","Guadarrama","Guadarrama",
                              "Guadarrama","Guadarrama","Guadarrama","Guadarrama","Guadarrama","Guadarrama","Guadarrama","Guadarrama",
                              "Guadarrama","Guadarrama","Guadarrama","Guadarrama","Guadarrama","Guadarrama","Guadarrama","Guadarrama",
                              "Guadarrama","Guadarrama","Guadarrama","Guadarrama","Guadarrama","Guadarrama","Guadarrama","Javalambre",
                              "Javalambre","Meridional","Meridional","Meridional","Meridional","Meridional","Meridional","Meridional",
                              "Meridional","Meridional","Meridional","Meridional","Meridional","Meridional","Meridional","Meridional",
                              "Meridional","Meridional","Meridional","Meridional","Guadarrama"))


lat_comp <- round(c(cent[,2]), 2)
long_comp <- round(c(cent[,1]), 2)
zona <- cent[,3]


fi <- data.frame(fecha_mal = seq(as.Date("1984-9-01"), length=24, by="month"))  
fi <- data.frame(fecha_mal = seq(as.Date("1990-9-01"), length=12, by="month")) 
fi <- data.frame(fecha_mal = seq(as.Date("2003-9-01"), length=12, by="month"))
fi <- data.frame(fecha_mal = seq(as.Date("2004-9-01"), length=12, by="month"))
fi <- data.frame(fecha_mal = seq(as.Date("2016-9-01"), length=12, by="month"))  
fi <- data.frame(fecha_mal = seq(as.Date("2019-9-01"), length=12, by="month"))
fi <- data.frame(fecha_mal = seq(as.Date("2021-9-01"), length=12, by="month"))

fi <- rbind(a,b,c,d,e)

ff <- data.frame(fecha_mal = seq(as.Date("1984-10-01"), length=24, by="month"))-1  
ff <- data.frame(fecha_mal = seq(as.Date("1990-10-01"), length=12, by="month"))-1 
ff <- data.frame(fecha_mal = seq(as.Date("2003-10-01"), length=12, by="month"))-1 
ff <- data.frame(fecha_mal = seq(as.Date("2004-10-01"), length=12, by="month"))-1  
ff <- data.frame(fecha_mal = seq(as.Date("2016-10-01"), length=12, by="month"))-1  
ff <- data.frame(fecha_mal = seq(as.Date("2019-10-01"), length=12, by="month"))-1  
ff <- data.frame(fecha_mal = seq(as.Date("2021-10-01"), length=12, by="month"))-1 

ff <- rbind(a,b,c,d,e)

rm(a,b,c,d,e)


f_inicio <- fi %>% 
  separate(fecha_mal, into = c("dia", "mes", "year")) %>%
  mutate(fecha_bien = paste(year, mes, dia, sep = "/")) %>%
  dplyr::select(fecha_bien)


f_fin <- ff %>% 
  separate(fecha_mal, into = c("dia", "mes", "year")) %>%
  mutate(fecha_bien = paste(year, mes, dia, sep = "/")) %>%
  dplyr::select(fecha_bien)


foreach (j=1:length(lat_comp)) %dopar% {
  lat <- lat_comp[j]
  long <- long_comp[j]
  mdt <- microclima::get_dem(lat = lat, long = long, resolution = 30)
  for (i in 1:nrow(f_fin))  {
    temp <- runauto(mdt,
                    f_inicio[i,], f_fin[i,], 
                    hgt = 0.1, 
                    l = NA, x = NA,
                    coastal = FALSE,
                    habitat = 7, ######## MODIFICAR #########
                    plot.progress = FALSE, save.memory = FALSE)
    tmax <- temp$tmax
    tmin <-temp$tmin
    tmed <- temp$tmean
    writeRaster(tmax, paste0("B:/MICROCLIMA/Datos/DATOS_PC/tmax_", zona[j], "_", j, "_", gsub("/","_", substr(f_inicio[i,], 4,10)),".tif"))
    writeRaster(tmin, paste0("B:/MICROCLIMA/Datos/DATOS_PC/tmin_", zona[j], "_", j, "_", gsub("/","_", substr(f_inicio[i,], 4,10)),".tif"))
    writeRaster(tmed, paste0("B:/MICROCLIMA/Datos/DATOS_PC/tmed_", zona[j], "_", j, "_", gsub("/","_", substr(f_inicio[i,], 4,10)),".tif"))
  }
}

