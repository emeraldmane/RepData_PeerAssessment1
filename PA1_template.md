---
title: "PA1_template.Rmd"
author: "emeraldmane"
date: "Sunday, September 14, 2014"
output: html_document
---

# Reproducible Research: Peer Assessment 1

##Loading and preprocessing the data


```r
activity <- read.csv("activity.csv", header = TRUE)
activity <- transform(activity, date = as.Date(date))

library(ggplot2)
library(lattice)
library(plyr)
```


##What is mean total number of steps taken per day?


```r
# Plot with missing values
totalsteps <- ddply(activity, .(date), summarise, steps = sum(steps))
hist(totalsteps$steps, main="Number of Steps", 
     xlab="Total number of steps taken each day", 
     breaks = 10, 
     xlim = c(0, 25000), 
     ylim = c(0, 20), 
     col="light blue")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
# Mean and median total number of steps taken per day
mean(totalsteps$steps, na.rm=T)
```

```
## [1] 10766
```

```r
median(totalsteps$steps, na.rm=T)
```

```
## [1] 10765
```


##What is the average daily activity pattern?


```r
AvgStepsPerInterval <- aggregate(steps ~ interval, activity, mean)

plot(AvgStepsPerInterval$interval, AvgStepsPerInterval$steps, type="l", 
     col="light blue",
     xlab="5-minute Intervals", 
     ylab="Avg # of Steps",
     main="Average Daily activity pattern")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
AvgStepsPerInterval[which.max(AvgStepsPerInterval$steps),]
```

```
##     interval steps
## 104      835 206.2
```

##Imputing missing values


```r
# Total number of missing values in the dataset
summary(activity$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##     0.0     0.0     0.0    37.4    12.0   806.0    2304
```

```r
# Imputing NA values
na.replace <- function(actvty) {
  ddply(actvty, .(interval), 
        function(ddat) {
    steps <- ddat$steps
    ddat$steps[is.na(steps)] <- mean(steps, na.rm = TRUE)
    return(ddat)
  })
}

# Plots with imputed NA values
NewActivity <- na.replace(activity)

NewStepsPerDay <- ddply(NewActivity, .(date), summarise, steps = sum(steps))
hist(NewStepsPerDay$steps, main="Number of Steps", 
     xlab="Total number of steps taken each day", 
     breaks = 10, 
     xlim = c(0, 25000), 
     ylim = c(0, 25),
     col="light green")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```r
# Mean and median total number of steps taken per day
mean(NewStepsPerDay$steps, na.rm=T)
```

```
## [1] 10766
```

```r
median(NewStepsPerDay$steps, na.rm=T)
```

```
## [1] 10766
```

Mean values did not change, but median value changed slightly.

##Are there differences in activity patterns between weekdays and weekends?


```r
# Defining weekends & weekdays
weekParts <- c("Weekday", "Weekend")
PartitionedWeek <- function(date) {
  day <- weekdays(date)
  part <- factor("Weekday", weekParts)
  if (day %in% c("Saturday", "Sunday"))
    part <- factor("Weekend", weekParts)
  return(part)
}

# Plots differentiating weekend activity & weekday activity
NewActivity$weekpart <- sapply(NewActivity$date, PartitionedWeek)

DateAvgSteps <- ddply(NewActivity,
                  .(interval, weekpart),
                  summarise,
                  mean = mean(steps))

p <- ggplot(DateAvgSteps, aes(x = interval, y = mean))
p <- p + geom_line() + facet_grid(. ~ weekpart, )
p <- p + ggtitle("Activity patterns on weekends and weekdays")
p + xlab("Interval") + ylab("Number of steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

