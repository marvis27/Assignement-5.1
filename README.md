---
title: "Activity analysis Markdown"
author: "Maria Visviki"
date: "6/5/2022"
output: html_document
---
Reproducible Research: Peer Assessment 1
========================================

## Loading & preprocessing the data

```{r}
dataRead <- read.csv("activity.csv")
```
- View the loaded data frame.

```{r}
names(dataRead)
 
str(dataRead)

head(dataRead, 10)
```
- Process/transform the data (if necessary) into a format suitable for analysis

### Subset data frame to values without na for next process

```{r}
data_Without_NA <- dataRead[complete.cases(dataRead),]
```
## What is mean total number of steps taken per day?

### Steps:
    1. Find out the total steps taken per day
    2. Put the descriptive variable names in data frame
    3. View new created data frame
    4. Plot histogram of the total steps per day
    
```{r}
totalSteps <- aggregate(steps ~ date, data_Without_NA, sum)

names(totalSteps)[2] <- "sum_steps"

head(totalSteps, 10)

hist(totalSteps$sum_steps,col = "orange",main = "Histogram of the Total Number of Steps Taken Each Day",xlab = "Total Number of Steps Taken Each Day",breaks = 30)
```
-Calculate and report the mean and median of the total number of steps taken per day.

```{r}
mean(totalSteps$sum_steps)

median(totalSteps$sum_steps)

```

## What is the average daily activity pattern?

-Make a time series plot (i.e. type = ālā) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

### the average number of steps taken, averaged across all days for each 5-minute:
  1. Find the "interval" dataframe.
  2. Put the descriptive variable names in data frame.
  3.View new created data frame.
  4. Format plot margins (bottom, left, top, right) for long text labels.
  5.Plot time series.
  
```{r}
interval_dataFrame <- aggregate(steps ~ interval, data_Without_NA, mean)

names(interval_dataFrame)[2] <- "mean_steps"

head(interval_dataFrame, 10)

par( mai = c(1.2, 1.5, 1,1) )

plot(
        x = interval_dataFrame$interval,
        y = interval_dataFrame$mean_steps,
        type = "l",
        main = "Time Series Plot of the 5-Minute Interval\n and the Average Number of Steps Taken, Averaged Across All Days",
        xlab = "5-Minute Interval",
        ylab = "Average Number of Steps Taken,\n Averaged Across All Days"
)
```
-Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

### Find out the maximum steps.

```{r}
interval_dataFrame[ interval_dataFrame$mean_steps == max(interval_dataFrame$mean_steps), ]
```

## Imputing missing values

-Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```{r}
missing <- is.na(dataRead$steps)
table(missing)
```

-Create a new dataset that is equal to the original dataset but with the missing data filled in.

  1. First, merge the original activity data frame with interval data frame.
  2.Then, merge NA values with averages rounding up for integers.
  3. Drop and reorder columns to match original activity data frame.

```{r}
NA_filled <- merge(dataRead, interval_dataFrame, by = 'interval', all.y = F)
NA_filled$steps[is.na(NA_filled$steps)] <- as.integer(round(NA_filled$mean_steps[is.na(NA_filled$steps)]))
keeps <- names(dataRead)
NA_filled <- NA_filled[keeps]
head(NA_filled, 10)
```

-Make a histogram of the total number of steps taken each day
    1. Find out the total number of steps taken per day with filled NA value.
    2.Put in the descriptive variable names in the newTotal data frame.
    3.Take a glance on this new data frame.
    
```{r}
newTotal<- aggregate(steps~date, NA_filled, sum)
names(newTotal)[2] <- 'sum_steps'
head(newTotal,10)

hist(newTotal$sum_steps, col="cyan", main="Histogram of the Total Number of Steps Taken Each Day\nwith the missing data filled in", xlab="Total Number of Steps Taken Each Day", breaks=30)
```

#### Mean of this new filled data frame.
```{r}
mean(newTotal$sum_steps)
```
#### Median of this new filled data frame.
```{r}
median(newTotal$sum_steps)
```

### Do these values differ from the estimates from the first part of the assignment?
#### It is a subtle difference in Mean calculation between two parts of the assignment;Mean = 10766.19 (original data frame) and 10765.64 (data frame filled with NA),But quite apparently difference in Median calculation between these two parts of the assignment.Median = 10765 (original data frame) and 10762 (data frame filled with NA),
### What is the impact of imputing missing data on the estimates of the total daily number of steps?
#### The impact is depend on the imputing level of the missing data.As shown in the experimental value, there was practically no much difference when using the average for a given interval as the averages is basically pulled towards to the inserted average value.

## Are there differences in activity patterns between weekdays and weekends?

-Create a new factor variable in the dataset with two levels - āweekdayā and āweekendā indicating whether a given date is a weekday or weekend day.

```{r}
newDataFrame <-NA_filled
weekend <- weekdays(as.Date(newDataFrame$date)) %in% c("Saturday", "Sunday")
newDataFrame$daytype <- 'weekday'
newDataFrame$daytype[weekend== TRUE]<- "weekend"
newDataFrame$daytype <- as.factor(newDataFrame$daytype)
str(newDataFrame)
head(newDataFrame,10)
```
## Make a panel plot containing a time series plot (i.e. type = ālā) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```{r}
new_Interval <- aggregate(steps~interval + daytype, newDataFrame, mean)
names(new_Interval)[3]<- 'mean_steps'
head(new_Interval, 10)

library(lattice)
xyplot(mean_steps ~ interval | daytype, new_Interval, type='l', layout = c(1,2), main = "Time Series Plot of the 5-Minute Interval\nand the Average Number of Steps Taken,\nAveraged Across All Weekday Days or Weekend Days", xlab= "5-Minute Interval", ylab="Average Number of Steps Taken")
```
