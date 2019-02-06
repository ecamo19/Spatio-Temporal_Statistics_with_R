---
title: "Exploring Spatio-Temporal Data Lab2.1"
author: "Erick Calderon-Morales"
output:
  html_document:
    code_folding: show
    fig_height: 30
    fig_width: 30
    keep_md: yes
    toc: yes
    toc_depth: 2
    toc_float: yes
  pdf_document:
    toc: yes
    toc_depth: '2'
  word_document:
    toc: yes
    toc_depth: '2'
---


#Lab 2.1: Data Wrangling


```r
library(tidyverse)
library(STRbook)
```

###Para entender los objetos STIDF y STFDF ver [Pebesma 2012](https://cran.r-project.org/web/packages/spacetime/vignettes/jss816.pdf)

##NOAA data set

There are six data tables:

+ Stationinfo.dat 328x3

+ Times_1990.dat 1461x4

+ Tmax_1990.dat 1461x328 na=-9999

+ Tmin_1990.dat 1461x328 na=-9999

+ TDP_1990.dat 1461x328 na=-999.90001

+ Precip_1990.dat 1461x328 na=-99.989998


##First task: Reconcile all data sets into one object

```r
#Cargar data
#The system.file sirve para localizar los datos dentro del paquete

locs <- read.table(system.file("extdata","Stationinfo.dat",
                               package = "STRbook"),
                   col.names = c("id","lat","lon"))


times <- read.table(system.file("extdata","Times_1990.dat",
                                package = "STRbook"),
                    col.names = c("julian","year","month","day"))

tmax <- read.table(system.file("extdata", "Tmax_1990.dat",
                               package = "STRbook"))
names(tmax) <- locs$id


#Attach tmax y times
tmax <- cbind(times,tmax)
#Ahora tmax contiene la informacion de temperatura (col=328) y la info de tiempo (col=4)


#Cambiando el formato de tmax a long format
#Deje igual las columnas julian year month day
#Key= id 
#values= z
tmax_long <- gather(tmax,id,z,-julian,-year,-month,-day)
head(tmax_long)
```

```
##   julian year month day   id  z
## 1 726834 1990     1   1 3804 35
## 2 726835 1990     1   2 3804 42
## 3 726836 1990     1   3 3804 49
## 4 726837 1990     1   4 3804 59
## 5 726838 1990     1   5 3804 41
## 6 726839 1990     1   6 3804 45
```

```r
#tratar id como integer y no como character
tmax_long$id <- as.integer(tmax_long$id)
```




```r
#Filter out missing data
#na= -9999
#Es mejor filtar los nas con un less than (<=) que un equal (==) esto debido a que este ultimo #genera un error de truncacion 

nrow(tmax_long)
```

```
## [1] 479208
```

```r
tmax_long <- tmax_long %>% 
  #filter los calores que no sean menores o iguales a -9998
  filter(!(z<=-9998))

nrow(tmax_long)
```

```
## [1] 196253
```



```r
#Agregar una columna proc
tmax_long <- mutate(tmax_long, proc= "Tmax")
head(tmax_long)
```

```
##   julian year month day   id  z proc
## 1 726834 1990     1   1 3804 35 Tmax
## 2 726835 1990     1   2 3804 42 Tmax
## 3 726836 1990     1   3 3804 49 Tmax
## 4 726837 1990     1   4 3804 59 Tmax
## 5 726838 1990     1   5 3804 41 Tmax
## 6 726839 1990     1   6 3804 45 Tmax
```




```r
#Agregar los otros data sets

data("Tmin_long", package = "STRbook")
head(Tmin_long)
```

```
##   julian year month day   id  z proc
## 1 726834 1990     1   1 3804 35 Tmin
## 2 726835 1990     1   2 3804 42 Tmin
## 3 726836 1990     1   3 3804 49 Tmin
## 4 726837 1990     1   4 3804 59 Tmin
## 5 726838 1990     1   5 3804 41 Tmin
## 6 726839 1990     1   6 3804 45 Tmin
```

```r
data("TDP_long", package = "STRbook")
head(TDP_long)
```

```
##   julian year month day   id    z proc
## 1 726834 1990     1   1 3812 22.7  TDP
## 2 726835 1990     1   2 3812 21.9  TDP
## 3 726836 1990     1   3 3812 30.2  TDP
## 4 726837 1990     1   4 3812 47.3  TDP
## 5 726838 1990     1   5 3812 43.2  TDP
## 6 726839 1990     1   6 3812 40.3  TDP
```

```r
data("Precip_long", package = "STRbook")
head(Precip_long)
```

```
##   julian year month day   id    z   proc
## 1 726834 1990     1   1 3804 0.00 Precip
## 2 726835 1990     1   2 3804 0.00 Precip
## 3 726836 1990     1   3 3804 0.01 Precip
## 4 726837 1990     1   4 3804 0.19 Precip
## 5 726838 1990     1   5 3804 0.00 Precip
## 6 726839 1990     1   6 3804 0.00 Precip
```


##Juntar todos los data sets en uno solo NOAA_df_1990

```r
NOAA_df_1990 <- rbind(tmax_long,Tmin_long,TDP_long,Precip_long)
```

#Summarizing the data

```r
summ <- group_by(NOAA_df_1990, year, proc) %>% 
  summarise(mean_proc = mean(z))

#Medias por año y proceso
head(summ)
```

```
## # A tibble: 6 x 3
## # Groups:   year [2]
##    year proc   mean_proc
##   <int> <chr>      <dbl>
## 1  1990 Precip     0.126
## 2  1990 TDP       45.2  
## 3  1990 Tmax      67.4  
## 4  1990 Tmin      67.4  
## 5  1991 Precip     0.116
## 6  1991 TDP       45.2
```




```r
#Numero de dias en los que no cayo lluvia

#Precipitacion del mes 6
noaa_prepci <- filter(NOAA_df_1990, proc == "Precip" & month==6)

summ <- group_by(noaa_prepci,year, id) %>% 
  summarise(days_no_rain_june = sum(z == 0))

head(summ)
```

```
## # A tibble: 6 x 3
## # Groups:   year [1]
##    year    id days_no_rain_june
##   <int> <int>             <int>
## 1  1990  3804                19
## 2  1990  3810                26
## 3  1990  3811                21
## 4  1990  3812                24
## 5  1990  3813                25
## 6  1990  3816                23
```



```r
#Arrange columns
noaa_df_sorted <- arrange(NOAA_df_1990, julian,id)
```




#Agregar coordenadas de las estaciones a los datos


```r
NOAA_df_1990 <- left_join(NOAA_df_1990, locs, by="id")
```


#Working with Spatio-Temporal Data Classes

Los datos se tienen que convertir a objetos tipo __STIDF (Spatio Temporal Data Frames)__ y __STFDF__ los cuales estan definidos en el paquete __spacetime__


```r
library(sp)
library(spacetime)
```


#Constructing an STIDF Object

Spatio temporal object for irregular data (STIDF), __los puntos no estan fijos en el tiempo__ 


##Primero definir un formato de fecha (año/mes/day)


```r
#Crear un sola columna con las fechas 
NOAA_df_1990$date <- with(NOAA_df_1990,
                          paste(year,month,day, sep="-")) 
#Convertirlo a formato fecha
class(NOAA_df_1990$date <- as.Date(NOAA_df_1990$date))
```

```
## [1] "Date"
```

##Construir el objeto spatio temporal (STIDF) para la variable de max temp

```r
#Se selecciona Tmax
tmax_long2 <- filter(NOAA_df_1990, proc == "Tmax")

#STIDF object: Se especifica en la funcion cuales son la coordenadas
stobj <- stConstruct(x = tmax_long2,
                     space = c("lon","lat"),
                     time = "date"
                     )
```



##Construir el objeto spatialPoints para la variable de max temp


```r
spat_part <- SpatialPoints(coords = tmax_long2[, c("lon","lat")])
temp_part <- tmax_long2$date

stobj2 <- STIDF(sp=spat_part,
                time = temp_part,
                data = select(tmax_long2,-date,-lon,-lat))
class(stobj2)
```

```
## [1] "STIDF"
## attr(,"package")
## [1] "spacetime"
```
 
#Constructing an STFDF Object

Spatio temporal object for fixesd data (STFDF), __Los puntos estan fijo en el tiempo__

Para estos tipo de objetos solo se tiene especificar las coordenadas espaciales, en este caso solo las coordenadas de las estaciones.


```r
spat_part <- SpatialPoints(coords = locs[, c("lon", "lat")])
temp_part <- with(times,
                  paste(year,month,day, sep="-"))
temp_part <- as.Date(temp_part)
```


```r
#Formato largo (long format)
tmax_long3 <- gather(tmax, id,z, -julian,-year,-month, -day)
head(tmax_long3)
```

```
##   julian year month day   id  z
## 1 726834 1990     1   1 3804 35
## 2 726835 1990     1   2 3804 42
## 3 726836 1990     1   3 3804 49
## 4 726837 1990     1   4 3804 59
## 5 726838 1990     1   5 3804 41
## 6 726839 1990     1   6 3804 45
```




```r
str(tmax_long3)
```

```
## 'data.frame':	479208 obs. of  6 variables:
##  $ julian: int  726834 726835 726836 726837 726838 726839 726840 726841 726842 726843 ...
##  $ year  : int  1990 1990 1990 1990 1990 1990 1990 1990 1990 1990 ...
##  $ month : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ day   : int  1 2 3 4 5 6 7 8 9 10 ...
##  $ id    : chr  "3804" "3804" "3804" "3804" ...
##  $ z     : int  35 42 49 59 41 45 46 42 54 43 ...
```

```r
tmax_long3$id <- as.integer(tmax_long3$id)

#Ordenar los datos
tmax_long3 <- arrange(tmax_long3, julian, id)

#revisar que la ordenacion spacial esta correcta
all(unique(tmax_long3$id)==locs$id)
```

```
## [1] TRUE
```


```r
#The STFDF object
stobj3 <- STFDF(sp=spat_part,
                time=temp_part,
                data = tmax_long3)
```

##Setting the coordinate reference system

```r
proj4string(stobj3) <- CRS("+proj=longlat +ellps=WGS84")
```



```r
#Replace -9999 por NA
stobj3$z[stobj3$z == -9999] <- NA
```

































