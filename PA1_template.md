## Introduction

The goal of this assignment to practice skills needed for reproducible research. Specifically this assignment use R markdown to write a report that answers the questions detailed in the sections below. In the process, the single R markdown document will be processed by knitr and be transformed into an HTML file.

Start this assignment with fork/clone the GitHub repository created for this assignment. When finish, submit this assignment by pushing your completed files into your forked repository on GitHub. The assignment submission will consist of the URL to your GitHub repository and the SHA-1 commit ID for your repository state.

##Data

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day. The data for this assignment can be downloaded from the course web site: Dataset: Activity monitoring data [52K] The variables included in this dataset are:

* steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
* date: The date on which the measurement was taken in YYYY-MM-DD format
* interval: Identifier for the 5-minute interval in which measurement was taken The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

```{r global_options, include=FALSE}
knitr::opts_chunk$set(fig.path='instructions_fig/')
```


## Loading and preprocessing the data
```{r library, results="hide"}
#Load all libraries
library(ggplot2)
library(plyr)
library(dplyr)
library(knitr)
library(markdown)
```

```{r unzip}
unzip("activity.zip")
df <- read.csv("activity.csv", colClasses = c("numeric", "Date", "numeric"))
```

## What is mean total number of steps taken per day?
* Calculate the total number of steps taken per day
```{r stepSum}
stepSum <- aggregate(steps ~ date, data= df, sum)
head(stepSum)
```

* Plot total via a Histogram
```{r graph}
qplot(stepSum$steps, 
      geom="histogram",
      binwidth = 3000,
      main = "Histogram for Step Count Per Day",
      xlab = "Total Steps per Day", 
      col=I("white"))

```

![Plot 1](/instructions_fig/graph-1.png)
      
* Calculate and report the mean and median of the total number of steps taken per day
```{r mean_median}
mean(stepSum$steps)
median(stepSum$steps)
```

## What is the average daily activity pattern?
* Make a time series plot of the 5-minute interval and the average number of steps taken per day
```{r mean_plot}
meanInterval <- aggregate(steps ~ interval, data= df, mean)
ggplot(data=meanInterval, aes(x=interval, y=steps, group = 1)) +
  geom_line() 
```
![Plot 2](/instructions_fig/mean_plot-1.png)

* Which five-minute interval contains the maximum number of steps?
```{r mean_interval}
meanInterval[which.max(meanInterval$steps), ]
```

## Imputing missing values
* Calculate and report the total number of missing values in the dataset
```{r isNA}
sum(is.na(df))
```

## Devise strategy for filling in missing values

I will be using the mean within a five minute interval

* Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r impute}
imputedData <- df

for (i in 1:nrow(imputedData)) {
  if (is.na(imputedData$steps[i])) {
    imputedData$steps[i] <- meanInterval$steps[meanInterval$interval == imputedData$interval[i]]
  }
}
```


* Make a histogram of the total number of steps taken each day 
```{r impute_sum}
imputedSum<- aggregate(steps ~ date, data = imputedData, sum)
qplot(imputedSum$steps, 
      geom="histogram",
      main = "Total Steps Taken Per Day",
      xlab = "Total Steps Per Day",
      binwidth = 3000, 
      col=I("white"))
```
![Plot 3](/instructions_fig/impute_sum-1.png)
* Calculate and report the mean and median total number of steps taken per day.
```{r meanMedian}
mean(imputedSum$steps)
median(imputedSum$steps)
```

* Do these values differ from the estimates from the first part of the assignment? 
Not substantially

* What is the impact of imputing missing data on the estimates of the total daily number of steps?
Shape of the histogram stays the same, and frequency counts increase. 

## Are there differences in activity patterns between weekdays and weekends?
* Use the dataset with the filled-in missing values for this part. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day

```{r newType}
imputedData$dayType <- ifelse(weekdays(imputedData$date) %in% c("Satuday", "Sunday"), "weekend", "weekday")
#Create new data frame with mean number of steps taken across all days
newDataInterval <- imputedData %>% group_by(interval, dayType) %>% summarise(meanSteps = mean(steps))
```

* Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
```{r ggplotthat}
ggplot(data = newDataInterval, mapping = aes(x = interval, y = meanSteps)) + 
  geom_line() + facet_grid(dayType ~ .) + 
  scale_x_continuous("Day Interval", 
  breaks = seq(min(newDataInterval$interval), max(newDataInterval$interval), 100)) + 
  scale_y_continuous("Average Number of Steps") + ggtitle("Average Number of Steps Taken by Interval")

```
![Plot 4](/instructions_fig/ggplotthat-1.png)
