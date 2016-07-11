# Reproducible Research: Peer Assessment 1

# Reproducible research peer assginment 1
## Packages needed for the assignment


```r
library(ggplot2)
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
library(scales)
library(car)
library(gridExtra)
library(curl)
```

## Loading and processing the data

```r
 download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile="activity.zip")
 
 
 # unzip the file inside the zip file
 unzip("activity.zip")
 
 # read the data into R, with "|" seperating values
 code <- read.delim("activity.csv", sep = ",",colClasses = c("numeric","POSIXct","numeric"))
```

## Histogram of the total number of steps taken each day

### Step 1: summarize number of step per day

```r
daily_steps <- aggregate(code$steps,by = list(day=code$date),FUN=sum, na.rm=TRUE)
```

### Step 2: plot a histogram

```r
m<-ggplot(daily_steps, aes(x))
m + geom_histogram() + xlab("Total steps per day") + ggtitle("Total number of steps taken each day")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

## Mean and median number of steps taken each day

```r
mean_1 <- mean(daily_steps$x)
mean_1
```

```
## [1] 9354.23
```

```r
median_1 <- median(daily_steps$x)
median_1
```

```
## [1] 10395
```

## Time series plot of the average steps taken per 5 minutes interval


```r
interval_avg <- aggregate(x=code$steps,by=list(code$interval),FUN = mean, na.rm=TRUE)
ggplot(data=interval_avg, aes(x=Group.1, y=x))+ geom_line() + ylab("avg steps taken") + xlab("Minute") + ggtitle("Average number of steps taken every 5 minute interval")
```

![](PA1_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

## Identify the 5 minutes interval than on average contains the maximum number of steps


```r
interval_avg[which(interval_avg$x== max(interval_avg$x)),"Group.1"]
```

```
## [1] 835
```

## Code to describe and show the strategy to imput missing data


```r
number_of_na <- length(which(is.na(code$steps)))
code2 <- code

impute_vals<-tapply(code$steps,code$interval,FUN=mean,na.rm=TRUE)
vals<-names(impute_vals)
n<-length(vals)

for(i in 1:n){
  
 code2$steps[which(is.na(code$steps) & code$interval==vals[i])]<- round(impute_vals[i],digits=0) 
}
```

## Histogram of the dataframe with NA imputation

```r
daily_steps_imp <- aggregate(code2$steps,by = list(day=code$date),FUN=sum, na.rm=TRUE)


m<-ggplot(daily_steps_imp, aes(x))
m + geom_histogram() + xlab("Total steps per day") + ggtitle("Total number of steps taken each day -- impuateted data frame")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_files/figure-html/unnamed-chunk-9-1.png)<!-- -->
### Mean and median of new dataframe

```r
mean2 <- mean(daily_steps_imp$x)
mean2
```

```
## [1] 10765.64
```

```r
median2 <- median(daily_steps_imp$x)
median2
```

```
## [1] 10762
```
### Impact on mean an median

```r
var_tax_mean<-(mean2-mean_1)/mean_1
percent(var_tax_mean)
```

```
## [1] "15.1%"
```

```r
var_tax_median<-(median2-median_1)/median_1
percent(var_tax_median)
```

```
## [1] "3.53%"
```


## Weekdays and Weekends data

```r
Sys.info()["sysname"]->sysname
if(sysname=="Windows"){
Sys.setlocale("LC_TIME", "English")}
```

```
## [1] "English_United States.1252"
```

```r
code3 <- code2
code3$day<-weekdays(code2$date)
code3$day<-recode(code3$day,"c('Monday','Tuesday','Wednesday','Thursday','Friday')='weekday';c('Saturday','Sunday')='weekend'",as.factor.result = TRUE,levels=c("weekday","weekend"))

code3$interval<-factor(code3$interval)
steps_wday<-aggregate(code3$step,list(interval=code3$interval,day=code3$day),mean,na.rm=TRUE)
```
### weekdays dataframe and graph

```r
wd<-ggplot(steps_wday[which(steps_wday$day=='weekday'),], aes(x))
wd<-wd + geom_histogram() + xlab("Total steps per day") + ggtitle("Total number of steps taken each day - Weekday")
```
### weekend dataframe and graph

```r
we<-ggplot(steps_wday[which(steps_wday$day=='weekend'),], aes(x))
we<-we + geom_histogram() + xlab("Total steps per day") + ggtitle("Total number of steps taken each day - Weekend")
```
### Panel plot weekday and weekend data

```r
grid.arrange(wd,we,ncol=1)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_files/figure-html/unnamed-chunk-15-1.png)<!-- -->
