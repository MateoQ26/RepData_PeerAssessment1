---
title: "Reproducible Research - Week 2"
subtitle: 'Peer Assessment 1'
author: "Andres Mateo Quevedo"
date: "17/8/2020"
output: html_document
---

## Assignment: Data Analysis

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

- *Dataset*: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

The variables in this dataset are:

- **steps**: Number of steps taking in a 5-minute interval (missing values are coded as **NA**)
- **date**: The date on which the measurement was taken in YYYY-MM-DD format.
- **interval**: Identifier for the 5-minute interval in which measurement was taken.



In the following, several questions are answered to obtain an exploratory basis for the given data set:


## Loading and preprocessing the data



```{r}
# Download file:

download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", 
              destfile = "activity.zip", 
              mode="wb")

# Unzip data and read: 

unzip("activity.zip")
Data <- read.csv("activity.csv", header = TRUE)
```

The data was saved in the **Data** variable, now they are displayed.

```{r}
head(Data,5)
str(Data)
```

Load the libraries that will be used in this report:

```{r}
library(ggplot2)
library(stats)
library(dplyr)
```

- Now the data is organized and cleaned (Delete the N/A)

```{r}
Data$date <- as.Date(as.character(Data$date))   
DataNA <- is.na(Data$steps)
cleanData <- Data[!DataNA,]
```

The missing NA values were deleted. Now the variable **cleanData** has the original data, but without the NA values.

```{r}
str(cleanData)
```

## What is mean total number of steps taken per day?

- Calculate the total number of steps taken per day

```{r cars}
total_steps <- tapply(cleanData$steps, cleanData$date, FUN=sum)
```

- Calculate and report the mean and median of the total number of steps taken per day:

```{r}
mean(total_steps)
median(total_steps)
```

- Make a histogram of the total number of steps taken each day:

```{r, echo=TRUE}
plot_1 <- qplot(total_steps, 
                binwidth=1000, 
                geom="histogram", 
                xlab="Total Daily Steps")
print(plot_1)
```


## What is the average daily activity pattern?

```{r}
pattern_average <- aggregate(x = list(steps = cleanData$steps), 
                             by = list(interval = cleanData$interval),
                             FUN = mean)
```

- Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r}
plot_2 <- ggplot(data = pattern_average, aes(x=interval, y=steps)) +
          geom_line(color = 'green') +
          xlab("5_minute interval") +
          ylab("Average number of steps")
print(plot_2)
```

- Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
pattern_average[which.max(pattern_average$steps),]
```

## Imputing missing values

The **Data** dataset has missing values **NA**, which we remove and create a new **cleanData** dataset; now a *strategy* is proposed to NOT remove this missing data but to *replace* these missing values to use the original data set in its entirety.

- Calculate and report the total number of missing values in the **Data**.

```{r}
missing_values <- sum(is.na(Data))
missing_values
```

- Devise a strategy for filling in all of the missing values in the **Data**.

*Strategy*:

```{r}
replace_mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
```

- Create a new dataset that is equal to the original dataset but with the missing data filled in.

*New data*:

```{r}
newData <- Data %>% 
           group_by(interval) %>% 
           mutate(steps = replace_mean(steps))
```

View **newData**:

```{r}
head(newData,10)
dim(newData)
```

It is checked that **newData** is the dataset resulting from **Data**, applying the strategy of filling in all the missing values.

```{r}
missing_values_newData <- sum(is.na(newData$steps))
missing_values_newData
```

- Make a histogram of the total number of steps taken each day in **newData**:

```{r}
total_steps_newData <- tapply(newData$steps, newData$date, FUN=sum)

plot_3 <- qplot(total_steps_newData, 
                binwidth=1000, 
                geom="histogram", 
                xlab="Total Daily Steps")
print(plot_3)
```

- Calculate and report the mean and median of the total number of steps taken per day in **newData**:

```{r}
mean(total_steps_newData)
median(total_steps_newData)
```

- Do these values differ from the estimates from the first part of the assignment?

```{r}
dif_mean <- mean(total_steps_newData) - mean(total_steps)
dif_mean


dif_median <- median(total_steps_newData) - median(total_steps)
dif_median
```

## Are there differences in activity patterns between weekdays and weekends?

For this part the **weekdays()** function may be of some help here. Use the dataset with the filled-in missing values for this part, e.i, **newData**.


- Create a new factor variable in the dataset with two levels – *“weekday”* and *“weekend”* indicating whether a given date is a weekday or weekend day.

```{r}
newData$date <- as.Date(newData$date)
newData$weekday <- weekdays(newData$date)
newData$weekend <- ifelse(newData$weekday == "sabado" | newData$weekday == "domingo", "Weekend", "Weekday" )

newData_weekend_weekday <- aggregate(newData$steps, by = list(newData$weekend, newData$interval), mean)

```

Assign the following names to **newData_weekend_weekday**

```{r}
names(newData_weekend_weekday) <- c("weekend", "interval", "steps")
```

- Make a panel plot containing a time series plot of the *5-minute interval (x-axis)* and the average number of steps taken, averaged across all *weekday days or weekend days (y-axis)*.

```{r}
plot_4 <- ggplot(newData_weekend_weekday, aes(x = interval, y = steps, color = weekend)) + geom_line() + facet_grid(weekend ~ .) + labs(title = "Mean of Steps by Interval", x = "interval", y = "steps")

print(plot_4)
```
