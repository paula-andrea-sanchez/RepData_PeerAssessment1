---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data



1. Load the data (i.e. `read.csv()`)


```r
file <- read.csv("activity.csv", header=TRUE, colClasses = c("numeric", "Date",     "numeric"))
```
2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
data <- file[!is.na(file$steps), ]
```
## What is mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day and 


```r
sum.total.days <- tapply(data$steps, data$date, sum)

hist(sum.total.days, main="Histogram total number of steps taken per day", xlab= "Total number of steps taken per day")
```

![](PA1_template_files/figure-html/sum-1.png)<!-- -->

2. Calculate and report the **mean** and **median** total number of steps taken per day


```r
mean(sum.total.days)
```

```
## [1] 10766.19
```

```r
median(sum.total.days)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
mean.intervals <- tapply(data$steps, data$interval, mean)

plot(unique(data$interval), mean.intervals, type = "l", main = "Average Daily Activity", xlab = "Intervals", ylab = "Average across all days")
```

![](PA1_template_files/figure-html/daily.activity-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
names(mean.intervals[mean.intervals == max(mean.intervals)])
```

```
## [1] "835"
```
## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NA`s)


```r
length(is.na(file$steps))
```

```
## [1] 17568
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
mean.intervals.df <- aggregate(list(data$steps), list(data$interval), mean)

colnames(mean.intervals.df) <- c("interval", "mean.total")

data.na <- file[is.na(file$steps), ]

for (i in unique(file[is.na(file$steps), ]$interval)){
  data.na[data.na$interval == i, 1] <- mean.intervals.df[mean.intervals.df$interval == i, 2]
  }
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
data.final <- rbind(data, data.na)
data.final <- data.final[order(data.final$date, data.final$interval), ]

summary(data.final)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 27.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0
```

```r
head(data.final, 20)
```

```
##        steps       date interval
## 1  1.7169811 2012-10-01        0
## 2  0.3396226 2012-10-01        5
## 3  0.1320755 2012-10-01       10
## 4  0.1509434 2012-10-01       15
## 5  0.0754717 2012-10-01       20
## 6  2.0943396 2012-10-01       25
## 7  0.5283019 2012-10-01       30
## 8  0.8679245 2012-10-01       35
## 9  0.0000000 2012-10-01       40
## 10 1.4716981 2012-10-01       45
## 11 0.3018868 2012-10-01       50
## 12 0.1320755 2012-10-01       55
## 13 0.3207547 2012-10-01      100
## 14 0.6792453 2012-10-01      105
## 15 0.1509434 2012-10-01      110
## 16 0.3396226 2012-10-01      115
## 17 0.0000000 2012-10-01      120
## 18 1.1132075 2012-10-01      125
## 19 1.8301887 2012-10-01      130
## 20 0.1698113 2012-10-01      135
```
4. Make a histogram of the total number of steps taken each day and Calculate and report the **mean** and **median** total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
total.days <- tapply(data.final$steps, data.final$date, sum)

hist(total.days, main="Histogram mean total number of steps taken per day", xlab= "Total number of steps taken per day")
```

![](PA1_template_files/figure-html/hist.missing.values-1.png)<!-- -->


```r
mean(total.days)
```

```
## [1] 10766.19
```

```r
median(total.days)
```

```
## [1] 10766.19
```

If you compare the mean and the median of both dataframes, with and without missing values, you can see that the mean is the same for both dataframes, while the median differs. Next, you can find the percentage error for the mean and median calculated for the dataframe with missing vallued



```r
(mean(total.days) - mean(sum.total.days))/mean(total.days)*100
```

```
## [1] 0
```

```r
(median(total.days) - median(sum.total.days))/median(total.days)*100
```

```
## [1] 0.01104085
```

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
data.final <- mutate(data.final, weekday = weekdays(date))
data.final[data.final$weekday == "Saturday" | data.final$weekday == "Sunday", "weekday.factor"] <- c("WE")
data.final[!(data.final$weekday == "Saturday" | data.final$weekday == "Sunday"), "weekday.factor"] <- c("WD")

str(data.final)
```

```
## 'data.frame':	17568 obs. of  5 variables:
##  $ steps         : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date          : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval      : num  0 5 10 15 20 25 30 35 40 45 ...
##  $ weekday       : chr  "Monday" "Monday" "Monday" "Monday" ...
##  $ weekday.factor: chr  "WD" "WD" "WD" "WD" ...
```

```r
head(data.final, 20)
```

```
##        steps       date interval weekday weekday.factor
## 1  1.7169811 2012-10-01        0  Monday             WD
## 2  0.3396226 2012-10-01        5  Monday             WD
## 3  0.1320755 2012-10-01       10  Monday             WD
## 4  0.1509434 2012-10-01       15  Monday             WD
## 5  0.0754717 2012-10-01       20  Monday             WD
## 6  2.0943396 2012-10-01       25  Monday             WD
## 7  0.5283019 2012-10-01       30  Monday             WD
## 8  0.8679245 2012-10-01       35  Monday             WD
## 9  0.0000000 2012-10-01       40  Monday             WD
## 10 1.4716981 2012-10-01       45  Monday             WD
## 11 0.3018868 2012-10-01       50  Monday             WD
## 12 0.1320755 2012-10-01       55  Monday             WD
## 13 0.3207547 2012-10-01      100  Monday             WD
## 14 0.6792453 2012-10-01      105  Monday             WD
## 15 0.1509434 2012-10-01      110  Monday             WD
## 16 0.3396226 2012-10-01      115  Monday             WD
## 17 0.0000000 2012-10-01      120  Monday             WD
## 18 1.1132075 2012-10-01      125  Monday             WD
## 19 1.8301887 2012-10-01      130  Monday             WD
## 20 0.1698113 2012-10-01      135  Monday             WD
```

2. Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using **simulated data**:


```r
weekdays <- subset(data.final, data.final$weekday.factor == "WD")
weekends <- subset(data.final, data.final$weekday.factor == "WE")

weekdays.mean <- tapply(weekdays$steps, weekdays$interval, mean)
weekends.mean <- tapply(weekends$steps, weekends$interval, mean)

par(mfrow=c(2, 1), bty = "l")
plot(unique(weekdays$interval), weekdays.mean, type = "l", main = "Average Daily Activity on Regular Weekdays", xlab = "Intervals", ylab = "Average across all days")
plot(unique(weekends$interval), weekends.mean, type = "l", main = "Average Daily Activity on Weekends", xlab = "Intervals", ylab = "Average across all days")
```

![](PA1_template_files/figure-html/plot-1.png)<!-- -->
