---
title: "Project1"
author: "DM"
date: "20/10/2020"
output: 
  html_document: 
    keep_md: yes
---

Codigo para leer datos que fueron descargados en la biblioteca Documents local


```r
library(tidyverse)
```

```
## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --
```

```
## v ggplot2 3.3.2     v purrr   0.3.4
## v tibble  3.0.4     v dplyr   1.0.2
## v tidyr   1.1.2     v stringr 1.4.0
## v readr   1.4.0     v forcats 0.5.0
```

```
## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
datos <- read_csv("activity.csv")
```

```
## 
## -- Column specification --------------------------------------------------------
## cols(
##   steps = col_double(),
##   date = col_date(format = ""),
##   interval = col_double()
## )
```

What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.


1. Calculate the total number of steps taken per day
2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
3. Calculate and report the mean and median of the total number of steps taken per day

Numero total de pasos por dia

```r
total_steps_per_day <- datos %>% filter(steps>=0) %>% select(date,steps) %>% group_by(date) %>% 
    summarize(total_steps=sum(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
#TOTAL DE PASOS POR DIA
total_steps_per_day
```

```
## # A tibble: 53 x 2
##    date       total_steps
##    <date>           <dbl>
##  1 2012-10-02         126
##  2 2012-10-03       11352
##  3 2012-10-04       12116
##  4 2012-10-05       13294
##  5 2012-10-06       15420
##  6 2012-10-07       11015
##  7 2012-10-09       12811
##  8 2012-10-10        9900
##  9 2012-10-11       10304
## 10 2012-10-12       17382
## # ... with 43 more rows
```

```r
#Histograma
hist(total_steps_per_day$total_steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
#Informe que calcula la media, mediana de pasos por dia
mean(total_steps_per_day$total_steps)
```

```
## [1] 10766.19
```

```r
median(total_steps_per_day$total_steps)
```

```
## [1] 10765
```

What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)




```r
ave_daily <- datos %>% select(steps,interval) %>% group_by(interval) %>% 
    summarize(average=mean(steps,na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
ggplot(ave_daily,aes(interval,average)) +
    geom_line() + xlab("Intervalos de 5 minutos") +
    ylab("Pasos promedios realizados")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
filter(ave_daily,ave_daily$average==max(ave_daily$average))
```

```
## # A tibble: 1 x 2
##   interval average
##      <dbl>   <dbl>
## 1      835    206.
```

Imputing missing values


```r
#Calculate and report the total number of missing values in the dataset
nrow(datos[is.na(datos$steps),])
```

```
## [1] 2304
```

Completando valores faltante y creando nuevo dataset con los datos completados

```r
#Se completan los NA con la media de todos los registros, se realiza una copia

datos1 <- datos
datos1$steps <- ifelse(is.na(datos1$steps),mean(datos1$steps,na.rm =
                                                    TRUE),datos$steps)
```

Realizando EDA con el nuevo conjunto de datos


```r
total_steps_per_day1 <- datos1 %>% filter(steps>=0) %>% select(date,steps) %>% group_by(date) %>% 
    summarize(total_steps=sum(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
#TOTAL DE PASOS POR DIA
total_steps_per_day1
```

```
## # A tibble: 61 x 2
##    date       total_steps
##    <date>           <dbl>
##  1 2012-10-01      10766.
##  2 2012-10-02        126 
##  3 2012-10-03      11352 
##  4 2012-10-04      12116 
##  5 2012-10-05      13294 
##  6 2012-10-06      15420 
##  7 2012-10-07      11015 
##  8 2012-10-08      10766.
##  9 2012-10-09      12811 
## 10 2012-10-10       9900 
## # ... with 51 more rows
```

```r
#Histograma
hist(total_steps_per_day1$total_steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
#Informe que calcula la media, mediana de pasos por dia
mean(total_steps_per_day1$total_steps)
```

```
## [1] 10766.19
```

```r
median(total_steps_per_day1$total_steps)
```

```
## [1] 10766.19
```

Al imputar valores faltantes se dan diferencias entre exluirlos y completar dichos
valores. Para excluir valores faltantes se debe tomar en cuenta el tipo de estudio o analisis que se este realizando.

Are there differences in activity patterns between weekdays and weekends?

```r
datos1$weekday <- ifelse(weekdays(datos1$date)=="sábado" |
                             weekdays(datos1$date)=="domingo","Weekday","weekend")

datos1$weekday <- as.factor(datos1$weekday)

weekday_steps_per_day <- datos1 %>% select(weekday,interval,steps) %>% group_by(weekday,interval) %>% 
    summarize(total_steps=mean(steps))
```

```
## `summarise()` regrouping output by 'weekday' (override with `.groups` argument)
```

```r
ggplot(weekday_steps_per_day,aes(interval,total_steps)) +
    geom_line() + facet_wrap( ~ weekday) + xlab("Intervalos 5 minutos") +
    ylab("Pasos promedios por intervalos")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->



