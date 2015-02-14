# Reproducible Research: Peer Assessment 1

## Load any external CRAN packages

```r
library(ggplot2)
```

## Loading and preprocessing the data

```r
activityData <- read.csv("activity.csv", stringsAsFactors=FALSE)
```


## What is mean total number of steps taken per day?

```r
# Summarize the data by date
activityDataByDate <- aggregate(steps ~ date, data=activityData, FUN=sum)
hist(activityDataByDate$steps, xlab="Number of steps in a day", main="Histogram of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
mean(activityDataByDate$steps, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(activityDataByDate$steps, na.rm=TRUE)
```

```
## [1] 10765
```


## What is the average daily activity pattern?


```r
activityDataByInterval <- aggregate(steps ~ interval, data=activityData, FUN=sum)
plot(activityDataByInterval, type="l", main="Number of steps taken by interval of the day", xlab="Interval", ylab="Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
activityDataByInterval[which.max(activityDataByInterval$steps),][,1]
```

```
## [1] 835
```

## Imputing missing values

Calculate the total number of records with any missing values in the dataset.

```r
# Get the total number of incomplete cases
sum(complete.cases(activityData)==FALSE)
```

```
## [1] 2304
```

With the relatively large number of missing values from the dataset - we can impute these by finding the median value of equivalent 5 minute time intervals from previous days to create better mean and median values.

```r
# Create the imputed dataset
imputedData <- activityData
imputedData$steps[is.na(imputedData$steps)] <- 
  with(imputedData, ave(steps, interval, FUN = function(x) median(x, na.rm=TRUE)))[is.na(imputedData$steps)]
```

Now we can see the histogram for the new imputed dataset along with the new mean and median values

```r
imputedDataByDate <- aggregate(steps ~ date, data=imputedData, FUN=sum)
hist(imputedDataByDate$steps, xlab="Number of steps in a day", main="Histogram of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

```r
mean(imputedDataByDate$steps, na.rm=TRUE)
```

```
## [1] 9503.869
```

```r
median(imputedDataByDate$steps, na.rm=TRUE)
```

```
## [1] 10395
```

As you can see from the new mean and median calculations - the figures for the imputed dataset are lower than the earlier dataset in which the NAs were ignored.  The reasoning behind this would be that the NAs more accurately represent no step reading for the interval - and corresponding intervals from different days are likely to have a low step count during that time period if it is early in the morning or late at night while the person sleeps and is not often active.  Imputing the NAs with these low corresponding values brings down the overall mean and median for the dataset.

## Are there differences in activity patterns between weekdays and weekends?

```r
imputedData$weekdaylabel<- weekdays(as.Date(imputedData$date)) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
aggregateIsWeekdayDataByDate <- aggregate(steps ~ interval + weekdaylabel, data=imputedData, FUN=mean)
aggregateIsWeekdayDataByDate$weekdaylabel[aggregateIsWeekdayDataByDate$weekdaylabel == "TRUE"] = "Weekday"
aggregateIsWeekdayDataByDate$weekdaylabel[aggregateIsWeekdayDataByDate$weekdaylabel == "FALSE"] = "Weekend"

# Plot using ggplot with 2 facets for the 2 locations
g <- ggplot(aggregateIsWeekdayDataByDate, aes(interval, steps)) 
p <- g + geom_line() + facet_grid(. ~ weekdaylabel)
print(p)
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

By using ggplot2 to plot the number of steps taken on a Weekday versus a weekend we can see a clear difference in behavior.  The behavior looks to be of a typical 9-5 office worker who, during the week, sees a peak just before 9am, then has a low number of steps throughout the workday and then another small peak after 5pm.  On the weekends, the steps are much more evenly distrubed throughout the day suggesting that the person does more walking no matter which interval of the day that it is.
