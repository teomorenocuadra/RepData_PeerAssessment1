---
title: "Analyzing Activity Patterns"
author: "_Teo Moreno_"
date: "_08 Apr 2017_"
output:
  html_document: default
  pdf_document: default
---

### Summary

Data from a personal activity monitoring device are analyzed to identify patterns. The device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November of 2012 and include the number of steps taken in 5 minute intervals each day.

### Loading, reading and preprocessing the data

Downloading the data.


```r
library(downloader)
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
filename <- "repdata%2Fdata%2Factivity.zip"
download(url, destfile = filename)
unzip("repdata%2Fdata%2Factivity.zip")
```

Loading the dataset.


```r
activity <- read.csv("activity.csv")
```

Saving the dataset as data frame.


```r
activity <- data.frame(activity)
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

### What is the total number of steps taken per day?

Calculating the number of steps taken per day.


```r
activity_date <- aggregate(steps ~ date, activity, sum)
str(activity_date)
```

```
## 'data.frame':	53 obs. of  2 variables:
##  $ date : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 2 3 4 5 6 7 9 10 11 12 ...
##  $ steps: int  126 11352 12116 13294 15420 11015 12811 9900 10304 17382 ...
```

Histogram of the total number of steps taken each day.


```r
hist(activity_date$steps, xlab = "Steps per Day",
     main = "Total Number of Steps", col = "dodgerblue", breaks = 10)
```

![plot of chunk histogram](figure/histogram-1.png)

Calculating the mean and median of the total number of steps taken per day.


```r
round(mean(activity_date$steps, na.rm = TRUE))
```

```
## [1] 10766
```

```r
round(median(activity_date$steps, na.rm = TRUE))
```

```
## [1] 10765
```

### What is the average daily activity pattern?

Calculating the average daily activity by 5-minute interval.


```r
activity_interval <- aggregate(steps ~ interval, activity, mean)
str(activity_interval)
```

```
## 'data.frame':	288 obs. of  2 variables:
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
```

Plotting steps versus the 5-minute interval. Time series plot of the average number of steps taken.


```r
plot(activity_interval$interval, activity_interval$steps,
     xlab= "Interval", ylab= "Average Steps", 
     main = "Activity Pattern", type = "l", col = "dodgerblue")
```

![plot of chunk plot](figure/plot-1.png)

Calculating the 5-minute interval that, on average, contains the maximum number of steps.


```r
activity_interval$interval[which.max(activity_interval$steps)]
```

```
## [1] 835
```

### Imputing missing values

Calculating and reporting the total number of missing values in the dataset.


```r
sum(!complete.cases(activity))
```

```
## [1] 2304
```

Filling all the missing values in the dataset with the mean across intervals.


```r
activity_na <- activity[which(!complete.cases(activity)), ]
activity_na$steps <- mean(activity_interval$steps, na.rm = TRUE)
str(activity_na)
```

```
## 'data.frame':	2304 obs. of  3 variables:
##  $ steps   : num  37.4 37.4 37.4 37.4 37.4 ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

Creating a new dataset combining complete data an filled missing data.


```r
activity_comp <- activity[which(complete.cases(activity)), ]
str(activity_comp)
```

```
## 'data.frame':	15264 obs. of  3 variables:
##  $ steps   : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
activity_fill <- rbind(activity_comp, activity_na)
index <- order(activity_fill$date)
activity_fill <- activity_fill[index, ]
str(activity_fill)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  37.4 37.4 37.4 37.4 37.4 ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

New calculatiion of the number of steps taken per day.


```r
activity_fill_date <-aggregate(steps ~ date, activity_fill, sum)
str(activity_fill_date)
```

```
## 'data.frame':	61 obs. of  2 variables:
##  $ date : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 2 3 4 5 6 7 8 9 10 ...
##  $ steps: num  10766 126 11352 12116 13294 ...
```

New Histogram of the total number of steps taken each day.


```r
hist(activity_fill_date$steps, xlab = "Steps per Day",
     main = "New Total Number of Steps", col = "dodgerblue", breaks = 10)
```

![plot of chunk histogram2](figure/histogram2-1.png)

New mean and median of the total number of steps taken per day.


```r
round(mean(activity_fill_date$steps))
```

```
## [1] 10766
```

```r
round(median(activity_fill_date$steps))
```

```
## [1] 10766
```

### Are there differences in activity patterns between weekdays and weekends?

Adding a weekday column to the activity set.


```r
activity_fill$weekday <- weekdays(as.Date(activity_fill$date))
```

Adding a daytype column to the activity set.


```r
activity_fill$daytype <- ifelse(activity_fill$weekday == "Saturday" | 
                                       activity_fill$weekday == "Sunday", "Weekend", "Weekday") 
```

Calculating the average activity by day type (weekday or weekend).


```r
activity_fill_daytype <- aggregate(steps ~ interval + daytype, activity_fill, mean)
str(activity_fill_daytype)
```

```
## 'data.frame':	576 obs. of  3 variables:
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ daytype : chr  "Weekday" "Weekday" "Weekday" "Weekday" ...
##  $ steps   : num  7.01 5.38 5.14 5.16 5.07 ...
```

Creating a time series panel plot comparing the average number of steps taken per 5-minute interval accross weekdays and weekends.


```r
library(lattice)
xyplot(steps ~ interval | daytype, activity_fill_daytype, layout = c(1,2),
       xlab= "Interval", ylab= "Average Steps", main = "Activity Pattern",
       type = "l")
```

![plot of chunk plot2](figure/plot2-1.png)
