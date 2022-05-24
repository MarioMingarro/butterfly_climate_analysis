library(readxl)
library(tidyverse)
library(sf)

BD_Mariposas <- read_excel("B:/MARIPOSAS/115_UTM/ATLAS/BD_Mariposas.xlsx")

spp <- c("A. pyrenaicus","A. zullichi","A. morronensis","E. hispania","E. palarica","E. rondoui","E. zapateri","E. bazae",
         "A. hesperica","L. bleusei","L. caelestissima","P. fassei","P. fulgens","P. golgus","P. nivescens","P. violetae","P. panoptes")
spp <- tolower(spp)

endemism <- na.omit(subset(BD_Mariposas,BD_Mariposas$`ESPECIE 2017` %in% spp))
write.csv(endemism, "B:/MARIPOSAS/ENDEMISM/endemism.csv")

##### MARIA

mariposas <-  read_excel("A:/MARIA/Lepidoptera_revised_AdrianMay_43sp_junto 18-5_with_julian_dates.xlsx", 
                              col_types = c("numeric", "text", "text", 
                                            "text", "text", "text", "text", "text", 
                                            "text", "text", "text", "numeric", 
                                            "text", "numeric", "numeric", "numeric", 
                                            "numeric", "text", "text", "text", 
                                            "text", "text", "numeric", "numeric", 
                                            "numeric", "text"))


TMED <- raster::stack()

for (i in 1901:2016){
  raster <- calc(raster::stack(list.files("B:/DATA/CHELSA/SPAIN/TMED", pattern = paste0(i), full.names = TRUE)), mean) # MEAN
  TMED <- raster::stack(TMED, raster)
}
names(TMED) <- paste0("Y_", seq(1901, 2019, by = 1))


TMED2 <- raster::stack()
for (i in 2017:2019){
raster <- calc(raster::stack(list.files("B:/CHELSA_DATA/TMED", pattern = paste0(i), full.names = TRUE)), mean) # MEAN
TMED2 <- raster::stack(TMED2, raster)
}
names(TMED2) <- paste0("Y_", seq(2017, 2019, by = 1))


r1 <- raster(vals=values(TMED2[[3]]),ext=extent(TMED),crs=crs(TMED),
              nrows=dim(TMED)[1],ncols=dim(TMED)[2])

TMED <- raster::stack(TMED, r1)

#1900-1944, 1945-1974, y 1975-2018
seq(1975, 2018, 1)
1975|1976|1977|1978|1979|1980|1981|1982|1983|1984|1985|1986|1987|1988|1989|1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006
|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018

rm(TMAX_1900_1944)
### Select data for specific periods ----

TMED_1900_1944 <-
  raster::subset(TMED, grep(
    c("1901|1902|1903|1904|1905|1906|1907|1908|1909|1910|1911|1912|1913|1914|
        1915|1916|1917|1918|1919|1920|1921|1922|1923|1924|1925|1926|1927|1928|
        1929|1930|1931|19321933|1934|1935|1936|1937|1938|1939|1940|1941|1942|1943|1944"
    ),
    names(TMED),
    value = T
  ))

TMED_1945_1974 <- raster::subset(TMED, grep(c("1945|1946|1947|1948|1949|1950|1951|1952|1953|1954|
                                              1955|1956|1957|1958|1959|1960|1961|1962|1963|1964|
                                              1965|1966|1967|1968|1969|1970|1971|1972|1973|1974"), names(TMED), value = T))

TMED_1975_2018 <- raster::subset(TMED, grep(c("1975|1976|1977|1978|1979|1980|1981|1982|1983|1984|
                                              1985|1986|1987|1988|1989|1990|1991|1992|1993|1994|1995|
                                              1996|1997|1998|1999|2000|2001|2002|2003|2004|2005|2006
                                              |2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018"), names(TMED), value = T))

### Calculate mean a standard deviation for diferent periods ----

TMED_mean_1900_1944 <- calc(TMED_1900_1944, mean)

TMED_mean_1945_1974 <- calc(TMED_1945_1974, mean)

TMED_mean_1975_2018 <- calc(TMED_1975_2018, mean)


writeRaster(TMED_mean_1900_1944, paste0("A:/MARIA/TMED_mean_1900_1944.tif"))
writeRaster(TMED_mean_1945_1974, paste0("A:/MARIA/TMED_mean_1945_1974.tif"))
writeRaster(TMED_mean_1975_2018, paste0("A:/MARIA/TMED_mean_1975_2018.tif"))

plot(TMED_mean_1900_1944)
plot(TMED_mean_1945_1974)
plot(TMED_mean_1975_2018)
