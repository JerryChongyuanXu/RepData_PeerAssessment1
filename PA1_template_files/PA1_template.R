# Loading and preprocessing the data

data <- read.csv("activity.csv")

# What is mean total number of steps taken per day?
# Make a histogram of the total number of steps taken each day

library(ggplot2)

daysteps <- tapply(data$steps, data$date, FUN = sum, na.rm = TRUE)
qplot(daysteps, binwidth = 800, xlab = "total number of steps taken each day")

# Calculate and report the mean and median total number of steps taken per day

mean(daysteps, na.rm=TRUE)
median(daysteps, na.rm=TRUE)

# What is the average daily activity pattern?
# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
# and the average number of steps taken, averaged across all days (y-axis)

averagesteps <- aggregate(x = list(steps = data$steps), by = list(interval = data$interval), FUN = mean, na.rm = TRUE)

ggplot(averagesteps, aes(x = interval, y = steps)) + geom_line()
        + xlab("5-minute interval") + ylab("average number of steps taken")

# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

names(averagesteps)[2] <- "meansteps"

averagesteps[averagesteps$meansteps == max(averagesteps$meansteps),  ]

# Imputing missing values
# Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

sum(is.na(data))

# Create a new dataset that is equal to the original dataset but with the missing data filled in.

fullfilldata <- data

for (i in 1 : nrow(fullfilldata)) {
  if (is.na(fullfilldata$steps[i])) {
    fullfilldata$steps[i] <- averagesteps[which(fullfilldata$interval[i] == averagesteps$interval), ]$meansteps
  }
}

head(fullfilldata)

sum(is.na(fullfilldata))

# Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total
# number of steps taken per day.

daysteps <- tapply(fullfilldata$steps, fullfilldata$date, FUN = sum, na.rm = TRUE)

qplot(daysteps, binwidth = 1000,
      xlab = "total number of steps taken each day")

mean(daysteps)

median(daysteps)

# Are there differences in activity patterns between weekdays and weekends?

# Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day. 

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

# Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute
# interval (x-axis) and the average number of steps taken, averaged across all weekday 
# days or weekend days (y-axis).

averagesteps <- aggregate(steps ~ interval + day, data = fullfilldata, mean)

ggplot(averagesteps, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) + xlab("5-minute interval") + ylab("Number of steps")