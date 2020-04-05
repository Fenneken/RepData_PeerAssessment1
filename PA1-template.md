---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



## Loading and preprocessing the data


Code for reading in the dataset and/or processing the data



```r
#Download the zipfile
#first makesure that the directory data exists
if (!file.exists("data")){
     dir.create("data")
}
###This code is for Checking whether data file exists. If not, it will unzip the file
#setwd("./data") # move to the directory of the data
#https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip

if (!file.exists("./data/NEI_data.zip")) { 
     fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
     download.file(fileUrl, destfile = "./data/repdata_data_activity.zip",method="curl")
     unzip("./data/repdata_data_activity.zip",exdir="./data") #unzip the file
}
```

Then read in the activity file

```r
library(data.table)
ActivityMonitoringData <- fread("./data/activity.csv")
#format date as date
ActivityMonitoringData$date <- as.Date(ActivityMonitoringData$date,"%Y-%m-%d")
```



## What is mean total number of steps taken per day?

Total number of steps taken each day



```r
#Histogram
#aggregate per day
StepsperDay <- aggregate(ActivityMonitoringData$steps, by=list(ActivityMonitoringData$date), sum)
names(StepsperDay) <- c("Date","Steps")
str(StepsperDay)
```

```
## 'data.frame':	61 obs. of  2 variables:
##  $ Date : Date, format: "2012-10-01" "2012-10-02" ...
##  $ Steps: int  NA 126 11352 12116 13294 15420 11015 NA 12811 9900 ...
```

```r
#Histogram number of steps per day
library(ggplot2)
histogram <- ggplot(StepsperDay,aes(Date,Steps))
histogram <- histogram + geom_bar(stat="identity")

print(histogram)
```

```
## Warning: Removed 8 rows containing missing values (position_stack).
```

![](PA1-template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

Mean and median number of steps taken each day

Because on a number of days the steps are 'NA', they have to be removed before the Mean and Median are calculated.

```r
mean(StepsperDay$Steps,na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(StepsperDay$Steps,na.rm=TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

Time series plot of the average number of steps taken


First average the number of steps per 5-minute interval; remove the NA's when calculating the mean.


```r
#aggregate per interval
StepsperInterval <- aggregate(ActivityMonitoringData$steps, 
                    by=list(ActivityMonitoringData$interval), mean, na.rm=TRUE)
names(StepsperInterval) <- c("Interval","Average_Steps")
head(StepsperInterval)
```

```
##   Interval Average_Steps
## 1        0     1.7169811
## 2        5     0.3396226
## 3       10     0.1320755
## 4       15     0.1509434
## 5       20     0.0754717
## 6       25     2.0943396
```

Then make a time series plot of it


```r
#Timeseries average steps per interval
library(ggplot2)
ts_plot <- ggplot(StepsperInterval,aes(Interval,Average_Steps))
ts_plot <- ts_plot + geom_line()
print(ts_plot)
```

![](PA1-template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->



The 5-minute interval that, on average, contains the maximum number of steps


To find the interval with the maximum number of average steps, we use the function which.max()

```r
StepsperInterval[which.max(StepsperInterval$Average_Steps),]
```

```
##     Interval Average_Steps
## 104      835      206.1698
```

## Imputing missing values

Code to describe and show a strategy for imputing missing data

First find out how many NAs there are, and how many in the column Steps

```r
sum(is.na(ActivityMonitoringData))
```

```
## [1] 2304
```

```r
sum(is.na(ActivityMonitoringData$steps))
```

```
## [1] 2304
```
So there are only missing values in the column steps.


There are 2 strategies dor imputing missing data:

1. Remove the missing values. This is done in the previous steps.

2. Replace the missing values by 0; which in this case could make sense.

In this case: first replace the missing values by zero.


```r
ActivityMonitoringDataZero <- ActivityMonitoringData
ActivityMonitoringDataZero[is.na(ActivityMonitoringDataZero)] <- 0
```


Then you re-do the graphs before with the zeroed dataset.

Histogram of the total number of steps taken each day after missing values are imputed



```r
#Histogram
#aggregate per day
StepsperDayZero <- aggregate(ActivityMonitoringDataZero$steps, by=list(ActivityMonitoringDataZero$date), sum)
names(StepsperDayZero) <- c("Date","Steps")

#Histogram number of steps per day
library(ggplot2)
histogramZero <- ggplot(StepsperDayZero,aes(Date,Steps))
histogramZero <- histogramZero + geom_bar(stat="identity")

print(histogramZero)
```

![](PA1-template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

Then also calculate the Mean and Median, that shows the bias that was caused by leaving out the NAs:

```r
mean(StepsperDayZero$Steps)
```

```
## [1] 9354.23
```

```r
median(StepsperDayZero$Steps)
```

```
## [1] 10395
```


## Are there differences in activity patterns between weekdays and weekends?
Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends


First make a column with the day of the week, and a factor column to indicate whether it is a weekday.


```r
ActivityMonitoringDataZero$Weekday <- weekdays(ActivityMonitoringDataZero$date)
ActivityMonitoringDataZero$WeekdayFactor <- ifelse(
     ActivityMonitoringDataZero$Weekday %in% c("zaterdag","zondag","saturday","sunday"),"Weekend", "Weekday")
```

Please note: this is using Dutch and English language settings; you may want to change that depending on your own language settings

Calculate the average steps per interval, for weekdays and weekends


```r
#aggregate per interval and weekdayindicator
StepsperIntervalWeekday <- aggregate(ActivityMonitoringDataZero$steps, 
     by=list(ActivityMonitoringDataZero$WeekdayFactor,ActivityMonitoringDataZero$interval), 
     mean)
names(StepsperIntervalWeekday) <- c("Weekdayfactor","Interval","Average_Steps")
head(StepsperIntervalWeekday)
```

```
##   Weekdayfactor Interval Average_Steps
## 1       Weekday        0     2.0222222
## 2       Weekend        0     0.0000000
## 3       Weekday        5     0.4000000
## 4       Weekend        5     0.0000000
## 5       Weekday       10     0.1555556
## 6       Weekend       10     0.0000000
```

Then create a panel plot 



```r
#Timeseries average steps per interval
library(ggplot2)
ts_plot <- ggplot(StepsperIntervalWeekday,aes(Interval,Average_Steps))
ts_plot <- ts_plot + geom_line()
ts_plot <- ts_plot + facet_grid(Weekdayfactor ~ ., scales = "free") # separate the weekday types
print(ts_plot)
```

![](PA1-template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

Conclusion: there are differences in activity between weekdays and weekends:

* Weekdays, activity starts earlier than in weekends
* In weekends, activity level during the day is higher
