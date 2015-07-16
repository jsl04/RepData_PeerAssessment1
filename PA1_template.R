##unziping and reading the csv file
library(lubridate)
library(dplyr)
library(tidyr)
library(plyr)
unzip('repdata_data_activity.zip')
read.csv(activity.csv, header=TRUE, sep="")
##converting the date column from factors to dates
activity$date
dates_converted <- as.Date(dates,format="%Y-%m-%d")
activity$date <- dates_converted
grouped_by_dates <- group_by(activity, date)
steps_day <- ddply(grouped_by_dates,.(date),summarise,steps=sum(steps, na.rm=TRUE))
hist(steps_day$steps, xlab= "Steps Per Day", col="red")
sum <-summarise(steps_day, mean(steps),median(steps))
interval_average <- ddply(activity,~interval,summarise,interval_average=mean(steps, na.rm=TRUE))
plot(interval_average$interval,interval_average$interval_average,ylab= "Average Number of Steps", xlab= "interval", type="b", col="red")
?plot
