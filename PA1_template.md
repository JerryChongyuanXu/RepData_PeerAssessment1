# Reproducible Research: Peer Assessment 1

echo = TRUE

# Loading and preprocessing the data

unzip(zipfile = "activity.zip")

data <- read.csv("activity.csv")

# What is mean total number of steps taken per day?
##        Make a histogram of the total number of steps taken each day

library(ggplot2)

daysteps <- tapply(data$steps, data$date, FUN = sum, na.rm = TRUE)

qplot(daysteps, binwidth = 1000, xlab = "total number of steps taken each day")

##        Calculate and report the mean and median total number of steps taken per day

mean(daysteps, na.rm=TRUE)

median(daysteps, na.rm=TRUE)

# What is the average daily activity pattern?
##        Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

averagesteps <- aggregate(x = list(steps = data$steps), by = list(interval = data$interval), FUN = mean, na.rm = TRUE)

ggplot(data = averagesteps, aes(x = interval, y = steps)) + geom_line() + labs(title = "Time Series Plot of the 5-minute Interval", x = "5-minute interval", y = "average number of steps taken")

##        Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

names(averagesteps)[2] <- "meansteps"

averagesteps[averagesteps$meansteps == max(averagesteps$meansteps),  ]

# Imputing missing values
##        Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

sum(is.na(data))

##        Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
##                My strategy is to let all of the missing values be filled in with mean value for that 5-minute interval.

##        Create a new dataset that is equal to the original dataset but with the missing data filled in.

fullfilldata <- data

for (i in 1:nrow(fullfilldata)) {
  if (is.na(fullfilldata$steps[i])) {
    fullfilldata$steps[i] <- averagesteps[which(fullfilldata$interval[i] == averagesteps$interval), ]$meansteps
  }
}

head(fullfilldata)

sum(is.na(fullfilldata))

##        Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 

daysteps <- tapply(fullfilldata$steps, fullfilldata$date, FUN = sum, na.rm = TRUE)

qplot(daysteps, binwidth=1000, xlab="total number of steps taken each day")

mean(daysteps)

median(daysteps)

##        Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

##            Both values are larger than the previous values. The reason is that all the missing values are '0' in calculating the mean and median values in the first part, but after these replacement, these values are the exact mean value of that 5-minute interval, which is larger than 0. So the new mean and median values must be larger. 

# Are there differences in activity patterns between weekdays and weekends?

##        Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day. 

weekdayend <- function(date){
  dayweek <- weekdays(date)
  if (dayweek %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) {
    return("weekday")
  }
  else if (dayweek %in% c("Saturday", "Sunday")){
    return("weekend")
  }
  else {
    stop("invalid date")
  }
}

fullfilldata$date <- as.Date(fullfilldata$date)

fullfilldata$day <- sapply(fullfilldata$date, FUN = weekdayend)

##        Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

averagesteps <- aggregate(steps ~ interval + day, data = fullfilldata, mean)

ggplot(averagesteps, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) + xlab("5-minute interval") + ylab("Number of steps")