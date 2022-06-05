dataRead <- read.csv("activity.csv")
names(dataRead)
str(dataRead)
head(dataRead,10)

hist(totalSteps$sum_steps, col="orange", main="Histogram of the Total Number of Steps Taken Each Day", xlab= "Total Number of Steps Taken Per Day", breaks= 30)

mean(totalSteps$sum_steps)
median(totalSteps$sum_steps)

interval_dataFrame <- aggregate(steps~interval, data_without_NA,mean)
names(interval_dataFrame)[2] <- 'mean_steps'
head(interval_dataFrame,10)

par(mai=c(1.2, 1.5, 1, 1))
plot(x=interval_dataFrame$interval, y=interval_dataFrame$mean_steps, type="l", main="Time Series Plot of the 5-Minute Interval\ n and the Average Number of Steps Taken, Averaged Across All Days",
     xlab = "5-Minute Interval", ylab="Average Number of steps taken,\n Averaged Across All Days")
     
interval_dataFrame[interval_dataFrame$mean_steps == max(interval_dataFrame$mean_steps),]

missing <- is.na(dataRead$steps)
table(missing)
NA_filled <- merge(dataRead, interval_dataFrame, by = 'interval', all.y = F)
NA_filled$steps[is.na(NA_filled$steps)] <- as.integer(round(NA_filled$mean_steps[is.na(NA_filled$steps)]))
keeps <- names(dataRead)
NA_filled <- NA_filled[keeps]
head(NA_filled, 10)

newTotal<- aggregate(steps~date, NA_filled, sum)
names(newTotal)[2] <- 'sum_steps'
head(newTotal,10)

hist(newTotal$sum_steps, col="cyan", main="Histogram of the Total Number of Steps Taken Each Day\nwith the missing data filled in", xlab="Total Number of Steps Taken Each Day", breaks=30)

mean(newTotal$sum_steps)
median(newTotal$sum_steps)

newDataFrame <-NA_filled
weekend <- weekdays(as.Date(newDataFrame$date)) %in% c("Saturday", "Sunday")
newDataFrame$daytype <- 'weekday'
newDataFrame$daytype[weekend== TRUE]<- "weekend"
newDataFrame$daytype <- as.factor(newDataFrame$daytype)
str(newDataFrame)
head(newDataFrame,10)

new_Interval <- aggregate(steps~interval + daytype, newDataFrame, mean)
names(new_Interval)[3]<- 'mean_steps'
head(new_Interval, 10)

library(lattice)
xyplot(mean_steps ~ interval | daytype, new_Interval, type='l', layout = c(1,2), main = "Time Series Plot of the 5-Minute Interval\nand the Average Number of Steps Taken,\nAveraged Across All Weekday Days or Weekend Days", xlab= "5-Minute Interval", ylab="Average Number of Steps Taken")
