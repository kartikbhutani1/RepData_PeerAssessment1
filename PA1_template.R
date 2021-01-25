library(ggplot2)
library(Hmisc)

###1
if (!"data" %in% ls()) {
  unzip(zipfile="activity.zip")
  data <- read.csv("activity.csv")
}

###2
data_good <- subset(data, !is.na(data$steps))

###1
steps_per_day <- tapply(data_good$steps, data_good$date, FUN = sum)

###2
p <- qplot(steps_per_day, binwidth=500, ylab = "Frequency", xlab="Total steps per day")
#print(p)

###3

print(mean(steps_per_day))
print(median(steps_per_day))

###1

average_daily_pattern <- aggregate(x=list(steps=data$steps), 
                      by=list(interval = data$interval),
                      FUN = mean, na.rm=TRUE)
#print(head(average_daily_pattern))
p <- ggplot(average_daily_pattern, aes(x=interval, y=steps)) +
            geom_line() + labs(title = "Time series of average steps", 
                                x = "5 min intervals", y = "Average steps taken")
#print(p)

###2
max_steps_interval <- average_daily_pattern[which.max(averages$steps), "interval"]
print(max_steps_interval)

###1
print(length(which(is.na(data$steps))))

###2
# by average

###3
filled_data <- data
#print(length(which(is.na(filled_data$steps))))
fillValues <- function (step, interval) {
  filled_step <- step
  if (is.na(step)) {
    filled_step <- average_daily_pattern[average_daily_pattern$interval == interval, "steps"]
  }
  return (filled_step)
}
filled_data$steps <- mapply(fillValues, data$steps, data$interval)
#print(length(which(is.na(filled_data$steps))))

###4
steps_per_day_imputed <- tapply(filled_data$steps, filled_data$date, FUN = sum)

p <- qplot(steps_per_day_imputed, binwidth=500, ylab = "Frequency", xlab="Total steps per day (imputed)")
print(p)

print(mean(steps_per_day_imputed))
print(median(steps_per_day_imputed))

###1

filled_data$dayType <- ifelse(as.POSIXlt(filled_data$date)$wday 
                              %in% c(1,2,3,4,5), "weekday", "weekend")

average_daily_pattern_imputed <- aggregate(steps ~ interval + dayType,
                                           filled_data, FUN = mean)
p <- ggplot(average_daily_pattern_imputed, aes(interval, steps)) + 
  geom_line() + facet_grid(dayType~.) + labs(x = "Interval", 
                                             y = "Number of steps")
print(p)