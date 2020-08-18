# Download file:

download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", 
              destfile = "activity.zip", 
              mode="wb")

# Unzip data and read: 

unzip("activity.zip")
Data <- read.csv("activity.csv", header = TRUE)

# Library:
library(ggplot2)
library(stats)
library(dplyr)

# View data:

head(Data)
summary(Data)
dim(Data)


## 1. Now the data is organized and cleaned (Delete the N/A)

Data$date <- as.Date(as.character(Data$date))   
DataNA <- is.na(Data$steps)
cleanData <- Data[!DataNA,]


## 2. What is mean total number of steps taken per day?

total_steps <- tapply(cleanData$steps, cleanData$date, FUN=sum)

# Calculate mean

mean(total_steps)

# Calculate median

median(total_steps)


## Make a histogram of the total number of steps taken each day:

plot_1 <- qplot(total_steps, 
                binwidth=1000, 
                geom="histogram", 
                xlab="Total Daily Steps")
print(plot_1)

dev.copy(png,"1_Total_Daily_Steps.png")
dev.off()

########################################

# 3. What is the average daily activity pattern?

pattern_average <- aggregate(x = list(steps = cleanData$steps), 
                             by = list(interval = cleanData$interval),
                             FUN = mean)


plot_2 <- ggplot(data = pattern_average, aes(x=interval, y=steps)) +
          geom_line(color = 'green') +
          xlab("5_minute interval") +
          ylab("Average number of steps")
print(plot_2)

dev.copy(png,"2_Total_Daily_Steps_NewData.png")
dev.off()


# Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?

pattern_average[which.max(pattern_average$steps),]

#########################################

# 4. Imputing missing values

## 1. Calculate and report the total number of missing values in the dataset.

missing_values <- sum(DataNA)
missing_values

## 2. Devise a strategy for filling in all of the missing values in the dataset.

# Strategy:
replace_mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))

## 3. Create a new dataset that is equal to the original dataset but with 
# the missing data filled in.

# New data:
newData <- Data %>% 
           group_by(interval) %>% 
           mutate(steps = replace_mean(steps))

# View new data:
head(newData)

# Check:

missing_values_newData <- sum(is.na(newData$steps))

## 4.1) Make a histogram of the total number of steps taken each day.

total_steps_newData <- tapply(newData$steps, newData$date, FUN=sum)

# Calculate mean newData

mean(total_steps_newData)

# Calculate median newData

median(total_steps_newData)


## Make a histogram:

plot_3 <- qplot(total_steps_newData, binwidth=1000, geom="histogram", xlab="Total Daily Steps")
print(plot_3)

dev.copy(png,"3_Total_Daily_Steps_NewData.png")
dev.off()

## 4.2) Do these values differ from the estimates from the first part of the assignment?

dif_mean <- mean(total_steps)-mean(total_steps_newData)
dif_mean


dif_median <- median(total_steps)-median(total_steps_newData)
dif_median


#####################################

# 5. Are there differences in activity patterns between weekdays and weekends?

## 5.1) Create a new factor variable in the dataset with two levels - "weekday" 
# and "weekend" indicating whether a given date is a weekday or weekend day.

newData$date <- as.Date(newData$date)
newData$weekday <- weekdays(newData$date)
newData$weekend <- ifelse(newData$weekday=="sabado" | newData$weekday=="domingo", "Weekend", "Weekday" )

newData_weekend_weekday <- aggregate(newData$steps, 
                                     by = list(newData$weekend, 
                                               newData$interval), mean)

names(newData_weekend_weekday) <- c("weekend", "interval", "steps")

## 5.2) Make a panel plot:

plot_4 <- ggplot(newData_weekend_weekday, aes(x = interval, y=steps, color=weekend)) +
          geom_line() +
          facet_grid(weekend ~ .) +
          labs(title = "Mean of Steps by Interval", x = "interval", y = "steps")

print(plot_4)

dev.copy(png,"4_Mean_of_Steps_by_Interval.png")
dev.off()
