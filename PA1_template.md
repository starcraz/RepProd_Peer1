---
title: "Peer Assignment 1"
author: "William Chan"
date: "Thursday, August 14, 2014"
output: html_document
---

I. During the data processing step, the dataset will be loaded into R and reformat date that is appropriate for further analysis

```{r process, echo=TRUE}
data <- read.csv("activity.csv")
data$date <- as.Date(data$date, format = "%Y-%m-%d")
```

II. Calcuate the mean total number of steps taken per day
     Create a histogram of the total number of stpes each day

```{r stephistogram, echo=TRUE}
dailysteps <- aggregate(steps ~ date, data = data, FUN = sum)
hist(dailysteps$steps, col = "purple", main = "Total Number of Steps per Day",
     xlab = "steps per Day")
```

     Calculate the mean and median total number of steps taken per day

```{r meanmedian, echo=TRUE}
meanStep <- format(mean(dailysteps$steps), digits=5, decimal.mark=",")
medianStep <- median(dailysteps$steps)
```

The mean of the total number of steps taken per day is **`r meanStep`** while the median is **`r medianStep`**.
III. Calculate the average daily activity pattern
     Creating a time series plot

```{r timeseries, echo=TRUE}
interval <- aggregate(steps ~ interval, data = data, FUN = mean)
plot(interval$interval, interval$steps, type = "l", col = "purple",
     main = "Average Daily Activity Pattern",
     xlab = "5-minute interval", ylab = "average number of steps taken")
```

     Determine the maximum numbers of steps with the 5-minute interval across all days

```{r maxstep, echo=TRUE}
maxSteps <- interval$interval[interval$steps == max(interval$steps)]
```

The maximum number of steps with the 5-minute interval across all days is **`r maxSteps`**.

IV. Determine missing values
     Calculate the total number of missing values in the dataset
     
```{r missingvalue, echo=TRUE}
na <- sum(is.na(data))
```
The total number of rows with missing values are **`r na`**.

     Devise a strategy for filling in all the missing values

The strategy used to impute the missing values within the steps variable was to use the VIM R package. The VIM package contains kNN function to impute values using Euclidean distance among the numerical variable. The dataset with imputed missing values is called imputeData. 

```{r strategy, echo=TRUE}
library(VIM)
imputeData <- kNN(data)
```

     Create a histogram of steps taken each day and calculate the mean and median total number of steps per day.
     
```{r imputehist, echo=TRUE}
imputesteps <- aggregate(steps ~ date, data = imputeData, FUN = sum)
hist(imputesteps$steps, col = "purple", main = "Total Number of Steps per Day After Imputation",
     xlab = "steps per Day")
```

```{r imputemeanmedian, echo=TRUE}
imputeMean <- mean(imputeData$steps)
imputeMedian <- median(imputeData$steps)
```

With the imputed dataset, the calculated mean of the total number of steps taken per day is **`r imputeMean`** while the median is **`r imputeMedian`**.

The imputed mean and median are extremely different than the mean and median from the original dataset. Imputed data dropped the estimates of the total daily number of steps.

V. Are there differences in activity patterns between weekdays and weekends?
     After creating the day variable, then we can create new weekday and weekend variables by using the sqldf package.
     
```{r weekdayend, echo=TRUE}
imputeData$day <- weekdays(imputeData$date)
library(sqldf)
weekend <- sqldf("select * from imputeData where day = 'Saturday' or 
                 day = 'Sunday'")
weekday <- sqldf("select * from imputeData where day != 'Saturday' or
                 day != 'Sunday'")
```

     The lattice package was used to create the panel plot of time series with 5-min interval and average steps


```{r panel, echo=TRUE}
library(lattice)
xyplot(steps ~ interval|which, make.groups(Weekday = weekday, Weekend = weekend),
       type = "l", layout = c(1,2), auto.key = T)
```