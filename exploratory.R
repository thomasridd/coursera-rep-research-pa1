#Read in the data
data <- read.csv('activity.csv')
data <- data[!is.na(data$steps), ]

#Publish the data
require(plyr)
steps_perday <- ddply(data, ~date, summarise, total = sum(steps))
hist(steps_perday$total)
mean(steps_perday$total)
median(steps_perday$total)

data2 <- read.csv('activity.csv')
steps_perday <- ddply(data2, ~date, summarise, total = sum(steps))
hist(steps_perday$total)
mean(steps_perday$total)
median(steps_perday$total)

#Total missing values
sum(is.na(data2))

#Find the mean steps at each time period
steps_at_time <- ddply(data, ~interval, summarise, steps = mean(steps))

#Substitude if is.na
average_at_time <- sapply(data2$interval, function (x) steps_at_time$steps[steps_at_time$interval == x]) 
data2$steps[is.na(data2$steps)] <- average_at_time[is.na(data2$steps)]

sum(is.na(data2$steps))



data2$weekday <- weekdays(as.Date(data2$date, "%Y-%m-%d"))
weekend <- c('Saturday', 'Sunday')
data2$weekday <- ifelse(data2$weekday %in% weekend, 'weekend', 'weekday')
data2$weekday <- factor(data2$weekday)

#Calculate mean steps at each time per day type
steps_at_time_day <- ddply(data2, ~interval + weekday, summarise, steps = mean(steps))

require(ggplot2)
ggplot(steps_at_time_day, aes(x = interval, y = steps)) +
  geom_line() +
  facet_wrap( ~ weekday, ncol = 1 )