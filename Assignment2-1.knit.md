---
title: "Assignment2"
output: html_document
keep_md: true
---

##Reproducible Research Week 2 Project
#####Hugo G Schmidt

  

### Loading and preprocessing the data

  We upload the data, which is stored in comma separated value (csv) format and plae it into a data frame.  I have not found it necessarry to futher pre-process the data, as the data frame works well enough to run this kind of analysis.  However, due to my geographical location, I have had to download the zip file in its entirety and then both unzip it and load it.



```r
URL <- ""
my_zip <- "repdata%2Fdata%2Factivity.zip"
unzip(my_zip)
ActMonDat <- read.csv("activity.csv")
```

### What is the mean total number of steps taken per day?

To answer this question, we need to work out the total number of steps taken each day, and then take the mean, and the median.   We determine the total number of steps taken daily through the "aggregate" command.


```r
AMD<- na.omit(ActMonDat)

DayAgr <- aggregate(AMD$steps, by=list(Category = AMD$date), FUN = sum)
colnames(DayAgr) <- c("Day", "Steps")
hist(DayAgr$Steps, xlab = "Steps", main = "Histogram of Steps per Day")
```

<img src="Assignment2-1_files/figure-html/unnamed-chunk-1-1.png" width="672" />

```r
mean(DayAgr$Steps)
```

```
## [1] 10766.19
```

```r
median(DayAgr$Steps)
```

```
## [1] 10765
```

The mean number of steps taken is 1.0766189\times 10^{4}.

The median number of steps taken is 10765.

### What is the average daily activity pattern?

Here we need to know the mean number of steps per five minute interval.  As before, we use 'aggreagate' to create a new data.frame that tells us the mean number of steps in each five minute interval.  We then produce a line-plot of the number of steps per interval, and identify the interval in question.


```r
IntAgr <- aggregate(AMD$steps, by=list(Category = AMD$interval), FUN = mean)
colnames(IntAgr) <- c("Interval", "Steps")

plot(x = IntAgr$Interval, y=IntAgr$Steps, type="l", ylab = "Steps", xlab = "Interval", main = "Daily Activity Pattern")
```

<img src="Assignment2-1_files/figure-html/unnamed-chunk-2-1.png" width="672" />

```r
DayAgr2 <- aggregate(AMD$steps, by=list(Category = AMD$date), FUN = mean)
colnames(DayAgr2) <- c("Day", "Steps")
plot(DayAgr2$Steps, type="l", ylab = "Steps", xlab = "day")
```

<img src="Assignment2-1_files/figure-html/unnamed-chunk-2-2.png" width="672" />

```r
IntAgr$Interval[ which.max(IntAgr$Steps)]
```

```
## [1] 835
```

The interval with the most number of steps is the 104 interval.


### Imputing missing values

  We have two data frames here. 
  
  The _AM_ data frame that contains the raw values.  
  The _IntAgr_ data frame that contains the mean number of steps taken per interval.
  
  In the code below, we step through the _AM_ data frame and determine which intervals do not have a value, i.e. return _NA_.  We then retrieve the mean for that interval from _IntAgr_ and pass it to AM at that location.


```r
AM <- ActMonDat
sum(is.na(ActMonDat))
```

```
## [1] 2304
```

```r
i <- 0

for (i in 1:length(AM$steps)) 
  {
  if(is.na(AM$steps[i]))
    {
    AM$steps[i] <- as.numeric(IntAgr$Steps[ IntAgr$Interval == AM$interval[i] ] )
    }
  
}

AMagr <- aggregate(AM$steps, by=list(Category = AM$date), FUN = sum)
colnames(AMagr) <- c("Day", "steps")
hist(AMagr$steps)
```

<img src="Assignment2-1_files/figure-html/unnamed-chunk-3-1.png" width="672" />

```r
mean(AMagr$steps)
```

```
## [1] 10766.19
```

```r
median(AMagr$steps)
```

```
## [1] 10766.19
```



### Are there differences in activity patterns between weekdays and weekends?

To answer this question, we first determine whether or a given day is a weekday or a weekend.  We then add this information to a new variable in the _AM_ data frame, and, as before, aggregate the activity levels from each.


```r
AM$day <- weekdays(as.Date(AM$date))

wd <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
we <- c("Saturday", "Sunday")

for (i in 1:length(AM$steps))
  {
  if(AM$day[i] %in% wd )
  {
    AM$WeekPart[i] <- "Weekday"
  }
  
  if(AM$day[i] %in% we )
    {
    AM$WeekPart[i] <- "Weekend"
    }
}

agrw <- aggregate(AM$steps, by=list(AM$interval, AM$WeekPart), FUN = mean)
colnames(agrw) <- c("interval", "Weekpart", "steps")

ss1 <- agrw[agrw$Weekpart == "Weekday",]
ss2 <- agrw[agrw$Weekpart == "Weekend",]

plot(x = ss1$interval, y = ss1$steps, type = "l", col = "red", ylab = "Interval", xlab = "Mean Steps")
lines(x = ss2$interval, y = ss2$steps, type = "l", col = "blue")
```

<img src="Assignment2-1_files/figure-html/unnamed-chunk-4-1.png" width="672" />

We appear to have a more concentrated activity on the weekdays (red)  and a more spread out activity on the weekends (blue).  This corresponds to there being a lot of early activtiy to get to work on the weekdays followed by sedentary sitting at a desk.
