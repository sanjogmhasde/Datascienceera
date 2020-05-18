---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
*Loading and processing data to a suitable format for analysis*

```r
unzip('activity.zip')
data <- read.csv('activity.csv', colClasses = c('integer', 'Date', 'factor'))
data$month <- as.numeric(format(data$date, '%m'))
clean <- na.omit(data)
rownames(clean) <- 1:nrow(clean)
```


## What is mean total number of steps taken per day?
*Total number of steps taken per day*

```r
total_steps <- aggregate(clean$steps, list(Date = clean$date), FUN  = 'sum')$x
total_steps
```

```
##  [1]   126 11352 12116 13294 15420 11015 12811  9900 10304 17382 12426 15098
## [13] 10139 15084 13452 10056 11829 10395  8821 13460  8918  8355  2492  6778
## [25] 10119 11458  5018  9819 15414 10600 10571 10439  8334 12883  3219 12608
## [37] 10765  7336    41  5441 14339 15110  8841  4472 12787 20427 21194 14478
## [49] 11834 11162 13646 10183  7047
```
*Histogram of total steps taken per day*

```r
library(ggplot2)
ggplot(clean, aes(date, steps)) + geom_bar(stat = 'identity' ) + labs(title = 'histogram of total number of steps taken per day', x = 'Date', y = 'Number of steps')
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
*Mean of total steps*

```r
 ogmean <- mean(total_steps)
ogmean
```

```
## [1] 10766.19
```
 *Median of total steps*

```r
ogmedian <- median(total_steps)
ogmedian
```

```
## [1] 10765
```
## What is the average daily activity pattern?
*Time series plot of the 5-minute interval and the average number of steps taken, averaged across all days

```r
Average_tests <- aggregate(clean$steps, list(interval = as.numeric(as.character(clean$interval))), FUN = 'mean')
names(Average_tests)[2] <- 'meanofsteps'
ggplot(Average_tests, aes(interval, meanofsteps)) + geom_line(color = 'black') + labs(title = "Time Series Plot of the 5-minute Interval", x = "5-minute intervals", y = "Average Number of Steps Taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
```
## Imputing missing values
*Total number of missing values in the data set*

```r
sum(is.na(data))
```

```
## [1] 2304
```
*Replacing all missing values with the median and creating a new data set containing these new values*

```r
newData <- data 
for (i in 1:nrow(newData)) {
  if (is.na(newData$steps[i])) {
    newData$steps[i] <- Average_tests[which(newData$interval[i] == Average_tests$interval), ]$meanofsteps
  }
}
```
*Creating a histogram with the new data, which has the missing values filled in *

```r
ggplot(newData, aes(date, steps)) + geom_bar(stat = "identity") + labs(title = "Histogram of Total Number of Steps Taken Each Day (no missing data)", x = "Date", y = "Total number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->
*Difference in the values of mean and median as compared to previous data*

```r
newTotalSteps <- aggregate(newData$steps,list(Date = newData$date), FUN = "sum")$x

newmean <- mean(newTotalSteps)
newmean
```

```
## [1] 10766.19
```

```r
newmedian <- median(newTotalSteps)
newmedian
```

```
## [1] 10766.19
```

```r
newmean - ogmean
```

```
## [1] 0
```

```r
newmedian - ogmedian
```

```
## [1] 1.188679
```
## Are there differences in activity patterns between weekdays and weekends?
*Creating an new factor variable indication whether a day is a weekend or weekday

```r
newData$weekdays <- factor(format(newData$date, "%A"))
levels(newData$weekdays)
```

```
## [1] "Friday"    "Monday"    "Saturday"  "Sunday"    "Thursday"  "Tuesday"  
## [7] "Wednesday"
```

```r
levels(newData$weekdays) <- list(weekday = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), weekend = c("Saturday", "Sunday"))
levels(newData$weekdays)
```

```
## [1] "weekday" "weekend"
```

```r
table(newData$weekdays)
```

```
## 
## weekday weekend 
##   12960    4608
```
*Creating a plot of the 5-Minute interval and average number of steps taken, avergaed across weekdays or weekend days*

```r
avgSteps <- aggregate(newData$steps,list(interval = as.numeric(as.character(newData$interval)) , weekdays = newData$weekdays), FUN = "mean")
names(avgSteps)[3] <- "meanOfSteps"
library(lattice)
xyplot(avgSteps$meanOfSteps ~ avgSteps$interval | avgSteps$weekdays,layout = c(1, 2), type = "l", xlab = "Interval", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

