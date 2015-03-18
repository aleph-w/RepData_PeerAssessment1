---
title: "Reproducible Research - 1"
author: "aleph-w"
date: "Friday, March 13, 2015"
output: html_document
---

First, we need to load the csv file, but since it's in a zipped format, we need to crate a temporary file and save the .zip file to it, then unzip it and store it in memory as a data frame.

```{r echo=TRUE}
setwd("X:/Coursera/Rep_Research")
activity <- read.csv("./activity.csv")
activity$date <- as.Date(activity$date, format="%Y-%m-%d")
```

Now that the data is loaded, we can start doing some analysis. We are looking to calculate the numbers of steps per day, and plot them in a histogram.

```{r echo=TRUE}
daily_steps <- tapply(activity$steps, activity$date, sum, na.rm=T)
hist(daily_steps, main="Histogram of Number of steps per day",xlab="Number of Steps",ylab="Frequency")
```
Now we want to know the mean and median number of steps per day:
```{r echo=TRUE}
mean(activity$steps, na.rm=T)
median(activity$steps, na.rm=T)
```


Plotting the time series will tell us how average activity throughout the day changes.
```{r echo=TRUE}
interval_avg <- tapply(activity$steps, activity$interval, mean, na.rm=T)
interval_name <- tapply(activity$interval, activity$interval, mean, na.rm=T)
interval_avg <- data.frame(cbind(interval_name, interval_avg))
plot(interval_avg$interval_avg, type="l", main="Average Steps per Day",xlab="Time of Day",ylab="Number of Steps")
interval_avg[which(grepl(max(interval_avg$interval_avg),interval_avg$interval_avg)),]
```
We can see that at 8:35-8:40 in the morning, we see average activity hit its peak at slightly more than 206 steps.


However, it might also be important to know how much data is missing. Once we know that, we can devise a method for editing that missing value. In this case, we will simply use the daily average for that time period.

```{r}
sum(is.na(activity$steps))
activity2 <- activity
for (row in 1:nrow(activity))
{
  if (is.na(activity$steps[row]))
  {
    activity2$steps[row] <- interval_avg$interval_avg[which(grepl(activity$interval[row],interval_avg$interval_name))]
  }
}
```

We would like to see if this has made any difference in the histogram of the number of steps taken per day. Here is the histogram with the new values inputed:

```{r echo=TRUE}
daily_steps2 <- tapply(activity2$steps, activity2$date, sum, na.rm=T)
hist(daily_steps2, main="Histogram of Number of Steps per Day",sub="With Fixed Missing Values",xlab="Number of Steps",ylab="Frequency")
```

And here is the old graph for comparison:
```{r echo=TRUE}
hist(daily_steps, main="Histogram of Number of Steps per Day",xlab="Number of Steps",ylab="Frequency")
```
We can see that before fixing the data, there are more days with fewer steps taken. However, notice that the mean and median have not changed:
```{r echo=TRUE}
mean(activity2$steps)
median(activity2$steps)
```
Compared to earlier:
```{r echo=TRUE}
mean(activity$steps, na.rm=TRUE)
median(activity$steps, na.rm=TRUE)
```

Now let's see if this varies by whether or not the day is a weekday of on the weekend. First we'll create two variables to determine if it is a weekend or not
```{r include=FALSE}
for (row in 1:nrow(activity2))
{
  if (weekdays(activity$date[row]) == "Sunday" | weekdays(activity$date[row]) == "Saturday")
    {
      activity2$Date_Type[row] <- "weekend"
    }
  else
    {
      activity2$Date_Type[row] <- "weekday"
    }
}
```
Now let's graph the two series, one right on top of the other. My battery is at 9%, so I'm going to do it this way instead of creating a lattice plot, which doesn't give us as good of a picture of what's realy going on anyway. Hopefully, I can get this submitted in time before my laptop battery runs out!
```{r echo=TRUE}
weekdays <- subset(activity2, Date_Type == "weekday")
weekends <- subset(activity2, Date_Type == "weekend")
interval_avg_weekend <- tapply(weekends$steps, weekends$interval, mean)
interval_avg_weekday <- tapply(weekdays$steps, weekdays$interval, mean)
plot(interval_avg_weekday, type="l", main="Average Steps per Day",xlab="Time of Day",ylab="Number of Steps", col="red")
lines(interval_avg_weekend, type="l", col="blue")
legend("topright", c("Weekday", "Weekend"), lty=1, col=c("red","blue"))
```

Looks like there's more early morning activity on weekdays, but more later day activity on weekends.

Thanks!
