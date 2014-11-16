---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

First thing we need to do is to load the necessary data


```r
data <- read.csv("activity.csv")
```

While we are at it, let's have a look at the data


```r
summary(data)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```


```r
head(data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

There are some missing data (NA), but let's ignore these for now

## What is mean total number of steps taken per day?

### Histogram of total steps taken a day

First we need the total number of steps taken a day



```r
totalsteps_perday <- aggregate(data$steps,by=list(date=data$date), FUN=sum)
names(totalsteps_perday) <- c("date","totalsteps")
hist(totalsteps_perday$totalsteps)
```

![plot of chunk histogram 1](figure/histogram 1-1.png) 

### Next we want the mean and median

Now the mean and the median

```r
mean_steps <- mean(totalsteps_perday$totalsteps, na.rm= TRUE)
median_steps <- median(totalsteps_perday$totalsteps, na.rm= TRUE)
```

Making the mean = 1.0766189 &times; 10<sup>4</sup> and the median 10765

## What is the average daily activity pattern?

First we need the to calcuatle the average per interval


```r
averages <- aggregate(data$steps,by=list(interval=data$interval),FUN=mean, na.rm = TRUE)
names(averages) <- c("interval","steps")
```

Again, let's have a look at it


```r
head(averages,15)
```

```
##    interval     steps
## 1         0 1.7169811
## 2         5 0.3396226
## 3        10 0.1320755
## 4        15 0.1509434
## 5        20 0.0754717
## 6        25 2.0943396
## 7        30 0.5283019
## 8        35 0.8679245
## 9        40 0.0000000
## 10       45 1.4716981
## 11       50 0.3018868
## 12       55 0.1320755
## 13      100 0.3207547
## 14      105 0.6792453
## 15      110 0.1509434
```


And now for the plot

```r
 plot(averages$interval,averages$steps,type = "l")
```

![plot of chunk Average Daily Pattern](figure/Average Daily Pattern-1.png) 

The maximum clearly seems to be a bit prior to 10 o'clock, more precisely


```r
maxrow <- which.max(averages$steps)
averages[maxrow,]
```

```
##     interval    steps
## 104      835 206.1698
```

Making 8h35 the interval with the highest average number of steps

## Imputing missing values


### Na values present
How many NA values are present in the original data ste


```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

2304 intervals have are missing a value for steps

### Strategy for NA values

I know that the excercise proposed to use the average or mean for the missing values, however I believe it more accurate to chose 0 steps for the missing values, as most likely no steps were taken during the NA intervals (most likely the subject was sitting, laying or sleeping)

However, I do understand that for the last part of this excercise (difference in median and mean), no difference will be observed

### Replacing NA with 0 values


```r
data[is.na(data)] <- 0
head(data,30)
```

```
##    steps       date interval
## 1      0 2012-10-01        0
## 2      0 2012-10-01        5
## 3      0 2012-10-01       10
## 4      0 2012-10-01       15
## 5      0 2012-10-01       20
## 6      0 2012-10-01       25
## 7      0 2012-10-01       30
## 8      0 2012-10-01       35
## 9      0 2012-10-01       40
## 10     0 2012-10-01       45
## 11     0 2012-10-01       50
## 12     0 2012-10-01       55
## 13     0 2012-10-01      100
## 14     0 2012-10-01      105
## 15     0 2012-10-01      110
## 16     0 2012-10-01      115
## 17     0 2012-10-01      120
## 18     0 2012-10-01      125
## 19     0 2012-10-01      130
## 20     0 2012-10-01      135
## 21     0 2012-10-01      140
## 22     0 2012-10-01      145
## 23     0 2012-10-01      150
## 24     0 2012-10-01      155
## 25     0 2012-10-01      200
## 26     0 2012-10-01      205
## 27     0 2012-10-01      210
## 28     0 2012-10-01      215
## 29     0 2012-10-01      220
## 30     0 2012-10-01      225
```


### Difference in mean and median



```r
totalsteps_perday2 <- aggregate(data$steps,by=list(date=data$date), FUN=sum)
names(totalsteps_perday2) <- c("date","totalsteps")
hist(totalsteps_perday2$totalsteps)
```

![plot of chunk No Na Values Histogram](figure/No Na Values Histogram-1.png) 

We note a much higher part of 0 values

For the updated mean and the median

```r
mean_steps2 <- mean(totalsteps_perday2$totalsteps, na.rm= TRUE)
median_steps2 <- median(totalsteps_perday2$totalsteps, na.rm= TRUE)
```

Making the new mean = 1.0766189 &times; 10<sup>4</sup> and the new median 10765

Identical to what we observed earlier, meaning that when calcuating mean and median NA values will be counted as 0 values, so when calculating mean and averages a choice must be made to whether this is the desired way of calcuating the mean and median.

## Are there differences in activity patterns between weekdays and weekends?

We are continuing with the data set where the NA values are changed to 0 values.

We need a variable to tell us if a value is for a weekend day or not


```r
data$ndate <- as.Date(data$date)
data$weekday <- weekdays(data$ndate, abbreviate = TRUE)
data$weekend <- data$weekday %in% c("Sun","Sat")
```

Next we want to calcualte the average number of steps per interval based on if it is a weekend or not.


```r
par(mfrow=c(1,2))
averages2 <- aggregate(data$steps, by=list(interval = data$interval,weekend = data$weekend),FUN=mean)
names(averages2) <- c("Interval", "Weekend","Steps")
plot(averages2$Interval[averages2$Weekend == TRUE], averages2$Steps[averages2$Weekend == TRUE], type = "l",main="Weekend",xlab="Interval",ylab="Steps")
plot(averages2$Interval[averages2$Weekend == FALSE], averages2$Steps[averages2$Weekend == FALSE], type = "l",main="Weekdays",xlab="Interval",ylab="Steps")
```

![plot of chunk Weekend vs Weekdays plots](figure/Weekend vs Weekdays plots-1.png) 



