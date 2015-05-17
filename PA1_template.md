---
title: "Reproducible Research 1st Assignment"
subtitle: "Activity Monitoring Device Study"
author: "Rodrigo Hartmann"
date: "Sunday, May 17, 2015"
output: html_document
---

###Settings and necessary packages

```{r}
echo = TRUE
options(scipen = 1)
library(ggplot2)
library(dplyr)
```


## Loading and preprocessing the data:

```{r}
setwd("C://Users/Rodrigo/Desktop/Coursera/Reproducible/ass1/")
csvfile <- unzip("activity.zip")
activity <- read.csv(csvfile)
glimpse(activity)
```


## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day


```{r}
numberofsteps = activity%>%
        select(steps,date)%>%
        group_by(date)%>%
        summarise(sumofsteps = sum(steps))

m <- ggplot(numberofsteps, aes(x=sumofsteps))
m + geom_histogram(aes(fill = ..count..)) + ggtitle("Total Number of Steps Taken per Day")
```

![plot of fig1](figure/fig1.png) 

2. Calculate and report the mean and median of the total number of steps taken per day

### Mean:
```{r}
round(mean(numberofsteps$sumofsteps, na.rm = TRUE),0)
```

* Mean: 10766

#### Median:
```{r}
median(numberofsteps$sumofsteps, na.rm = TRUE)
```

* Median: 10765

## What is the average daily activity pattern?

1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r}
dailypattern <- 
        activity %>% 
        group_by(interval) %>% 
        summarise(meanSteps = mean(steps, na.rm = TRUE))

max = dailypattern[dailypattern$meanSteps == max(dailypattern$meanSteps), ]

qplot(x=interval, y=meanSteps, data = dailypattern,  geom = "line",   
      xlab="5-minute interval",
      ylab="Number of Steps",
      main="Average Daily Activity Pattern") + geom_vline(xintercept = max$interval,  colour="red", linetype = "longdash")
        
```

![plot of fig2](figure/fig2.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
round(max,2)
```

* 835

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset 

```{r}
sum(is.na(activity))
round((sum(is.na(activity))/nrow(activity)),2)
```

* 2304
* 13,11%

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
AND
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```{r}
data_no_na <- activity 
for (i in 1:nrow(data_no_na)) {
        if (is.na(data_no_na$steps[i])) {
                data_no_na$steps[i] <- dailypattern[which(data_no_na$interval[i] == dailypattern$interval), ]$meanSteps
        }
}

glimpse(data_no_na)
sum(is.na(data_no_na))
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 

```{r}
numberofsteps2 = data_no_na%>%
        select(steps,date)%>%
        group_by(date)%>%
        summarise(sumofsteps = sum(steps))

m2 <- ggplot(numberofsteps2, aes(x=sumofsteps))
m2 + geom_histogram(aes(fill = ..count..)) + ggtitle("Total Number of Steps Taken per Day")
```

![plot of fig3](figure/fig3.png) 

### Mean:
```{r}
round(mean(numberofsteps2$sumofsteps, na.rm = TRUE),0)
```
*Mean = 10766

#### Median:
```{r}
round(median(numberofsteps2$sumofsteps, na.rm = TRUE),0)
```
*Median = 10766

#### Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

The mean stayed the same, the median changed 1 point and looks like  the higher frequency bar just moved one break to the right


## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

* I'm Brazilian, my R is in Portuguese, please change "sábado" to "saturday" and "domingo" to "sunday"


```{r}
data_no_na$date = as.Date(data_no_na$date)

data_no_na$weekday <- ifelse(weekdays(data_no_na$date) %in% c("sábado", "domingo"), 
    "weekend", "weekday")

glimpse(numberofsteps2)

dailypattern2 <- 
        data_no_na %>% 
        group_by(weekday,interval) %>% 
        summarise(meanSteps = mean(steps, na.rm = TRUE))
```

* I'm Brazilian, my R is in Portugues, please change "sábado" to "saturday" and "domingo" to "sunday"


2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```{r}
library(lattice)
xyplot(dailypattern2$meanSteps ~ dailypattern2$interval | dailypattern2$weekday, 
       layout = c(1, 2), type = "l", 
       xlab = "Interval", ylab = "Number of steps")
```

![plot of fig4](figure/fig4.png) 







