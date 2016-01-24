library(data.table)
library(ggplot2)

data <- fread('activity.csv')
data.sub <- na.omit(data)
data.sub.perday <- data.sub[,.(stepssum=sum(steps)),date]

ggplot(data.sub.perday, aes(stepssum)) + geom_histogram() + theme_bw()
mean(data.sub.perday$stepssum)
median(data.sub.perday$stepssum)

data.sub.perinterval <- data.sub[,.(stepsmean = mean(steps), stepsmedian = median(steps)),interval]
ggplot(data.sub.perinterval, aes(interval, stepsmean)) + geom_line() + theme_bw()
data.sub.perinterval[which.max(data.sub.perinterval$stepsmean)]

data.na <- data[which(is.na(data$steps))]
data.merged <- merge(data.na, data.sub.perinterval, by='interval')
data.merged[,steps:=floor(data.merged$stepsmedian)]
data.merged[,stepsmean:=NULL]
data.merged[,stepsmedian:=NULL]
data.imput <- rbind(data.sub, data.merged)

data.imput.perday <- data.imput[,.(stepssum=sum(steps)),date]
ggplot(data.imput.perday, aes(stepssum)) + geom_histogram() + theme_bw()
mean(data.imput.perday$stepssum)
median(data.imput.perday$stepssum)

Sys.setlocale("LC_TIME", "C")
data.imput$date <- as.Date(data.imput$date, "%Y-%m-%d")
weekdays(data.imput$date)
weekdays.names <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
data.imput[,dayofweek:=factor((weekdays(.SD$date) %in% weekdays.names), 
           levels=c(TRUE, FALSE), labels=c('weekend', 'weekday'))]

data.imput.perinterval.perdayofweek <- data.imput[,.(stepsmean = mean(steps)), .(interval, dayofweek)]
ggplot(data.imput.perinterval.perdayofweek, aes(interval, stepsmean)) + geom_line() + facet_grid(dayofweek~.) + theme_bw()
