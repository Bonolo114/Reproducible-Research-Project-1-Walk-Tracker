setwd("c:/Users/Molopyane/Documents/datasciencecoursera/Course 5 Reproducible Research/Week 2/Course Project 1/repdata_data_activity")
readings <- read.csv("activity.csv")                     
readings$date<- as.Date(readings$date, "%Y-%m-%d")
sapply(readings,class)



path <- getwd()
url <- "https://github.com/rdpeng/RepData_PeerAssessment1/blob/80edf39c3bb508fee88e3394542f967dd3fd3270/activity.zip"

download.file(url, destfile = paste(path,"activity.zip",sep = "/"))
unzip(zipfile = "activity.zip")
........................................... 

NAs <- as.data.frame(which(is.na(readings$steps)))
Zeros <- as.data.frame(which(readings$steps == 0))
Positives <- as.data.frame(which(readings$steps > 0))

Counts <- cbind( "NAs"= nrow(NAs), "Zeros"= nrow(Zeros), 
                 "Positive" = nrow(Positives))
Counts

.............................................

y<- aggregate(steps~date, readings ,sum)
means   <- aggregate(steps~date, readings, mean, na.rm = TRUE)
medians <- aggregate(steps~date, readings, median, na.rm = TRUE)

y[which(y[,"steps"]==max(y$steps)),]
means[which(means[,"steps"]==max(means$steps)),]
medians[which(medians[,"steps"]==max(medians$steps)),]

collection <- cbind(y, Means =means[,2],Medians = medians[,2])
collection

.............................
# Initial

par( mfrow = c(1,1))
hist(y$steps, xlab = "Daily Steps", main = "Sum of Daily Steps" )
hist(means$steps, xlab = "Mean Steps 5 minute per period", main = "Means of Daily Steps" )
......................

intervals_y <- aggregate(steps~interval, readings, sum)
par( mfrow = c(1,1))
barplot(height = intervals_y$steps, names.arg = intervals_y$interval)


intervals_y[which(intervals_y[,"steps"]==max(intervals_y$steps)),]


...........................................



readings$days<-weekdays(readings$date)

head(readings)
days<- c("Monday" ,"Tuesday", "Wednesday", "Thursday", "Friday")
weekends<- c("Saturday", "Sunday")

daysteps     <- readings[which(readings[,"days"] %in% days), ]
weekendsteps <- readings[which(readings[,"days"] %in% weekends),]

.................................. 

intervals_weekdays <- aggregate(steps~interval, daysteps , sum)
intervals_weekends <- aggregate(steps~interval, weekendsteps , sum)

.................................

library(lattice)

xyplot(steps~interval, data = intervals_weekdays, type = "l")

xyplot(steps~interval, data = intervals_weekends, type = "l")

xyplot(steps~interval, data = intervals_weekdays, type = "l", main = "Weekdays") 
abline(h= 2000)
xyplot(steps~interval, data = intervals_weekends, type = "l", main = "Weekends")
#PLOTS

#Steps Histograms
par(mfrow = c(3,2), oma = c(0,0,0,1))
# Initial
hist(y$steps, xlab = "Daily Steps"
     , main = "Sum of Daily Steps Original data NAs omitted" )
hist(means$steps, xlab = "Mean Steps 5 minute per period", main = "Means of Daily Steps" )
# Median
hist(median_y1$steps, xlab = "Daily Steps", 
     main = "Sum of Daily Steps Median Imputes" )
hist(means1$steps, xlab = "Mean Steps 5 minute per period", main = "Means of Daily Steps" )

#Mean
hist(median_y2$steps, xlab = "Daily Steps", 
     main = "Sum of Daily Steps Mean Imputes" )
hist(means2$steps, xlab = "Mean Steps 5 minute per period", main = "Means of Daily Steps" )

#Interval Histograms
par(mfrow = c(3,1), oma = c(0,0,0,1))
barplot(height = intervals_y$steps, names.arg = intervals_y$interval)
barplot(height = intervals_y1$steps, names.arg = intervals_y1$interval)
barplot(height = intervals_y2$steps, names.arg = intervals_y2$interval)

# Day Comparisons
par(mfrow = c(3,2))
#Initial

with(intervals_weekdays, plot(interval, steps, type = "l",
       main = "Cumulative Steps taken per Time Intervals on Weekdays"))
abline( h = 2000)
     
with(intervals_weekends,plot(interval, steps, type = "l",
       main = "Cumulative Steps taken per Time Intervals on Weekends"))
abline( h = 2000)

# Median
with(intervals_weekdays1, plot(interval, steps, type = "l",
                              main = "Cumulative Steps taken per Time Intervals
                              on Weekdays with Median imputations"))
abline( h = 2000)


with(intervals_weekends1,plot(interval, steps, type = "l",
                             main = "Cumulative Steps taken per Time Intervals 
                             on Weekends with Median imputations"))


#Mean
with(intervals_weekdays2, plot(interval, steps, type = "l",
                               main = "Cumulative Steps taken per Time Intervals
                              on Weekdays with Mean imputations"))
abline( h = 2000)
with(intervals_weekends2,plot(interval, steps, type = "l",
                              main = "Cumulative Steps taken per Time Intervals 
                             on Weekends with Mean imputations"))
abline( h = 2000)

The number of times the weekday steps exceed the weekend maximum

max(intervals_weekends$steps)
max(intervals_weekends1$steps)
max(intervals_weekends2$steps)
