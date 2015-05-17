library(data.table)
library(ggplot2)
library(plyr)
library(dplyr)


##Loading and preprocessing the data
df <- read.csv("activity.csv", colClasses = c("numeric", "Date", "numeric"))

##What is mean total number of steps taken per day?

#1.  Calculate the total number of steps taken per day
stepSum <- aggregate(steps ~ date, data= df, sum)

#2.  Plot total via a Histogram
qplot(stepSum, 
      geom="histogram",
      main = "Histogram for Step Count Per Day",
      binwidth = 3000, 
      col=I("white")) 
#3. Calculate and report the mean and median of the total number of steps taken per day
mean(stepSum$steps)
median(stepSum$steps)

##What is the average daily activity pattern?

#1. Make a time series plot of the 5-minute interval and the average number of steps taken per day
meanInterval <- aggregate(steps ~ interval, data= df, mean)
ggplot(data=meanInterval, aes(x=interval, y=steps, group = 1)) +
  geom_line() 

#plot(tempFrame[, "data$interval"], tempFrame[, "data$steps"], type = "l")

#2. Which five-minute interval contains the maximum number of steps?
meanInterval[which.max(meanInterval$steps), ]
      
##Imputing missing values

#1. Calculate and report the total number of missing values in the dataset
sum(is.na(df))

#2. Devise strategy for filling in missing values
#Use the mean within a five minute interval

#3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
imputedData <- df

for (i in 1:nrow(imputedData)) {
  if (is.na(imputedData$steps[i])) {
    imputedData$steps[i] <- meanInterval$steps[meanInterval$interval == imputedData$interval[i]]
  }
}

#4. Make a histogram of the total number of steps taken each day 
##Calculate and report the mean and median total number of steps taken per day. 
##Do these values differ from the estimates from the first part of the assignment? 
##What is the impact of imputing missing data on the estimates of the total daily number of steps?

imputedSum<- aggregate(steps ~ date, data = imputedData, sum)
qplot(imputedSum$steps, 
      geom="histogram",
      main = "Total Steps Taken Per Day",
      xlab = "Total Steps Per Day",
      binwidth = 3000, 
      col=I("white"))

mean(imputedSum$steps)
median(imputedSum$steps)

#Create a column consisting of the day type (weekend/weekday)
imputedData$dayType <- ifelse(weekdays(imputedData$date) %in% c("Satuday", "Sunday"), "weekend", "weekday")


#Create new data frame with mean number of steps taken across all days
newDataInterval <- imputedData %>% group_by(interval, dayType) %>% summarise(meanSteps = mean(steps))

#plot using ggplot
plot <- ggplot(data = newDataInterval, mapping = aes(x = interval, y = meanSteps)) + 
  geom_line() + facet_grid(dayType ~ .) + scale_x_continuous("Day Interval", 
                                                                 breaks = seq(min(newDataInterval$interval), max(newDataInterval$interval), 100)) + 
  scale_y_continuous("Average Number of Steps") + ggtitle("Average Number of Steps Taken by Interval")
plot