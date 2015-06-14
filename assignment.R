Sys.setlocale("LC_ALL", "English")
library("ggplot2")
library("lattice")
setwd("C:/projetos/RepData_PeerAssessment1")
unzip("activity.zip")
activity <- read.csv("activity.csv")
activity$date <- as.Date(activity$date)
activityClean <- na.omit(activity) 

#What is mean total number of steps taken per day? #################################################
# Calculate the total number of steps taken per day ############################
stepsByDay <- aggregate(
  activityClean$steps, 
  by    = list(activityClean$date), 
  FUN   = sum)
names(stepsByDay) <- c("Date", "Steps")
hist(stepsByDay$Steps, breaks = 10, xlab="Total Number of Steps Taken per Day",col="green")

# Calculate and report the mean and median of the total ########################
# number of steps taken per day  ###############################################
meanByDay <- mean(stepsByDay$Steps)
meanByDay
medianByDay <- median(stepsByDay$Steps)
medianByDay

# What is the average daily activity pattern? ######################################################
# Make a time series plot (i.e. type = "l") of the 5-minute interval 
# (x-axis) and the average number of steps taken, averaged across 
# all days (y-axis)
stepsInterval <- aggregate(activity$steps, 
                                by = list(interval = activity$interval),
                                FUN=mean, na.rm=TRUE)

colnames(stepsInterval) <- c("interval", "steps")
plot(stepsInterval, type = "l", col="green")

# Which 5-minute interval, on average across all the days in the activity, 
# contains the maximum number of steps?
maxInterval <- stepsInterval[which.max(stepsInterval$steps),]
maxInterval

# Imputing missing values ##########################################################################
# Calculate and report the total number of missing values in the activity (i.e. the total number of rows with NAs)
totalNas <- sum(is.na(activity))
totalNas

# Devise a strategy for filling in all of the missing values in the activity. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
#replace the NAs with the median number of steps for that period

activityImputed <- NULL
activityMissing <- activity[is.na(activity$steps), ]

for(i in 1:nrow(activityMissing)) {
  row <- activityMissing[i,]
  row$steps <- stepsInterval[stepsInterval$interval == row$interval, ]$steps[1]
  activityImputed <<- rbind(row, activityImputed)
}

# Create a new activity that is equal to the original activity but with the missing data filled in.
activityImputed <- rbind(activityImputed, activity[!is.na(activity$steps), ])
dailySteps<-aggregate(activityImputed$steps,by=list(activityImputed$date),sum,na.rm=TRUE)
names(dailySteps)<-c("Day","Steps")


# Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
hist(dailySteps$Steps)


# Are there differences in activity patterns between weekdays and weekends?

# Create a new factor variable in the dataset with two levels - "weekday" 
# and "weekend" indicating whether a given date is a weekday or weekend day.
activityImputed$day <- weekdays(activityImputed$date)
activityImputed$dayType <- c("weekday")

# If day is Saturday or Sunday, make day_type as weekend
for (i in 1:nrow(activityImputed)){
  if (activityImputed$day[i] == "Saturday" || activityImputed$day[i] == "Sunday"){
    activityImputed$dayType[i] <- "weekend"
  }
}
activityImputed$dayType <- as.factor(activityImputed$dayType)

# Make a panel plot containing a time series plot (i.e. type = "l") 
# of the 5-minute interval (x-axis) and the average number of steps taken, 
# averaged across all weekday days or weekend days (y-axis). 
# See the README file in the GitHub repository to see an example of what 
# this plot should look like using simulated data.
dailySteps2 = aggregate(steps ~ interval + dayType, activityImputed, mean)
xyplot(steps ~ interval | factor(dayType), data = dailySteps2, aspect = 1/2, 
       type = "l")
