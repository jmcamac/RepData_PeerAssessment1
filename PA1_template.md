---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



## Loading and preprocessing the data
load dependencies 

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:lubridate':
## 
##     intersect, setdiff, union
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(lattice)
```

Read the data.

```r
activity_df <- read.csv("activity.csv")
```

Change activity_df$data from a factor to a lubridate object.

```r
activity_df$date <- ymd(activity_df$date)
```

## What is mean total number of steps taken per day?

Make histogram using the base ploting system.

```r
hist(activity_df$steps, xlab = "No. of steps taken", ylab = "Frequency", col = "red", 
     main = "No. of Steps Taken Each Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Calculate the mean and median number of steps taken per day.


```r
meanSteps <- mean(activity_df$steps, na.rm = TRUE)
print(paste("The mean number of steps per day is", meanSteps))
```

```
## [1] "The mean number of steps per day is 37.3825995807128"
```

```r
medianSteps <- median(activity_df$steps, na.rm = TRUE)
print(paste("The median number of steps per day is", medianSteps))
```

```
## [1] "The median number of steps per day is 0"
```


## What is the average daily activity pattern?
Find the avg. no. of steps accross all five days per interval using tapply.

```r
avgStepsPerInterval <- tapply(activity_df$steps, activity_df$interval, mean,na.rm = TRUE)
xcoord <- as.numeric(unlist(dimnames(avgStepsPerInterval)))
```

Make time series plot.

```r
plot(xcoord, avgStepsPerInterval, xlab = "Interval", ylab = "Avg. No. of Steps", type = "l", 
     main = "Avg. No. Steps v. Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

What is the maximum 5-minute interval, on average across all the days in the dataset? The first number is the interval; the second is the index.

```r
which.max(avgStepsPerInterval)
```

```
## 835 
## 104
```

## Imputing missing values
Calculate the total number of missing values in the data

```r
sum(is.na(activity_df))
```

```
## [1] 2304
```

Add an 'average' column to data frame

```r
new_df <- mutate(activity_df, average = rep(avgStepsPerInterval, 61))
```

Replace na values with average.

```r
new_df$steps[is.na(new_df$steps)] <- new_df$average
```

```
## Warning in new_df$steps[is.na(new_df$steps)] <- new_df$average: number of
## items to replace is not a multiple of replacement length
```

Make a histrogram of total number of steps taken each day of new_df side.

```r
hist(new_df$steps, xlab = "No. of steps taken", ylab = "Frequency", col = "blue", 
     main = "No. of Steps Taken Each Day (Imputed Values)")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

Side by side comparison with original histogram.

```r
par(mfrow = c(2, 1))
hist(activity_df$steps, xlab = "No. of steps taken", ylab = "Frequency", col = "red", 
     main = "No. of Steps Taken Each Day (Missing Values)")
hist(new_df$steps, xlab = "No. of steps taken", ylab = "Frequency", col = "blue", 
     main = "No. of Steps Taken Each Day (Imputed Values)")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
Notice the range of each plot. The histogram with the imputed values has greater frequency.

However, notice that this does not affect the mean or median.


```r
meanSteps <- mean(new_df$steps, na.rm = TRUE)
print(paste("The mean number of steps per day of the dataframe with imputed values is", meanSteps))
```

```
## [1] "The mean number of steps per day of the dataframe with imputed values is 37.3825995807128"
```

```r
medianSteps <- median(new_df$steps, na.rm = TRUE)
print(paste("The median number of steps per day is", medianSteps))
```

```
## [1] "The median number of steps per day is 0"
```

## Are there differences in activity patterns between weekdays and weekends?
Lets massage the data!

```r
new_df <- mutate(new_df, day = chron::is.weekend(date))
new_df$day <- as.factor(new_df$day)
new_df <- mutate(new_df, day = recode(day, 'FALSE' = 'weekday', 'TRUE' = 'weekend'))

weekday_df <- filter(new_df, day == 'weekday')
weekend_df <- filter(new_df, day =='weekend')

weekdaysAvg <- tapply(weekday_df$steps, weekday_df$interval, mean,na.rm = TRUE)
weekendsAvg <- tapply(weekend_df$steps, weekend_df$interval, mean,na.rm = TRUE)

weekday_df <- mutate(weekday_df, average = rep(weekdaysAvg, 45))
weekend_df <- mutate(weekend_df, average = rep(weekendsAvg, 16))

joined_df <- rbind(weekday_df, weekend_df)
```

Plot of just the weekends.

```r
xyplot(average ~ interval, data = weekend_df[1:288,], main="Avg. # of Steps v. Interval on Weekends ", 
       xlab="5-min Intervals",  ylab="Number of Steps",type= 'l')
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png)<!-- -->
Plor of just the weekdays.

```r
xyplot(average ~ interval, data= weekday_df[1:288, ], main="Avg. # of Steps v. Interval on Weekdays ", 
       xlab="5-min Intervals",  ylab="Number of Steps",type= 'l')
```

![](PA1_template_files/figure-html/unnamed-chunk-17-1.png)<!-- -->
Side by side comparison

```r
xyplot(average ~ interval|factor(day), data = joined_df, main="Avg. # of Steps v. Interval", 
       xlab="5-min Intervals",  ylab="Number of Steps",type= 'l', layout = c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-18-1.png)<!-- -->
Notice, that the max number of steps occurs on the weekdays around interval 800, but the weekends have a higher number of steps throughout the day. 
