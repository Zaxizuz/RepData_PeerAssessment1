Set up the environment
----------------------

``` r
library(dplyr)
library(zoo)
library(lattice)
```

Load the data
-------------

``` r
rawdata <-read.csv("activity.csv")
```

What is mean total number of steps taken per day?
-------------------------------------------------

-   Calculate the total number of steps taken per day

``` r
data <-group_by(rawdata,date)
sumsteps <-summarise(data,sum(steps))
```

-   Make a histogram of the total number of steps taken each day

``` r
hist(sumsteps$`sum(steps)`,)
```

![](Course_Project_files/figure-markdown_github/histogram-1.png)

-   Calculate and report the mean and median of the total number of steps taken per day

``` r
s.mean<-mean(sumsteps$`sum(steps)`,na.rm=TRUE)
```

The mean is 1.076618910^{4}.

``` r
r.median<-median(sumsteps$`sum(steps)`,na.rm=TRUE)
```

The median is 10765.

Average daily activity pattern
------------------------------

``` r
pattern <-rawdata[,c(1,3)]
pattern$interval <-as.numeric(pattern$interval)
by_interval <-aggregate(steps ~interval, data=pattern,FUN=mean)
plot(by_interval,type="l")
```

![](Course_Project_files/figure-markdown_github/daily%20activity-1.png)

``` r
maxsteps <-summarise(by_interval,max(steps))
maxrow<-which(grepl(maxsteps,by_interval$steps))
```

In this data, the maximum of steps appears in the 835th 5-minute interval.

Imputing missing data
---------------------

-   The total number of raws including missing value

``` r
na.sum<-sum(is.na(pattern))
```

There are 2304 raws including missing value.

-   Filling in all of the missing values in the dataset

The strategy is to replace missing value with the average steps of the interval.

``` r
averagesteps<-aggregate(steps~interval,rawdata,mean)
newdata<-transform(rawdata,steps=ifelse(is.na(rawdata$steps),averagesteps$steps[match(rawdata$interval,averagesteps$interval)],rawdata$steps))
```

compare imputed data with original data
---------------------------------------

-   Calculate the average steps

``` r
totalsteps<-aggregate(steps~date,rawdata,sum)
totalsteps_new<-aggregate(steps~date,newdata,sum)
```

-   Visualising the comparison

``` r
hist(totalsteps_new$steps,col="red",xlab="Number of Steps",main="Steps taken each day")
hist(totalsteps$steps,col="black",add=T)
legend("topright",c("original","imputed"),col=c("black","red"),lwd=10)
```

![](Course_Project_files/figure-markdown_github/steps%20taken%20each%20day-1.png)

-   The difference between original and imputed data

``` r
meansteps<-mean(totalsteps$steps)
mediansteps<-median(totalsteps$steps)
meansteps_new<-mean(totalsteps_new$steps)
mediansteps_new<-median(totalsteps_new$steps)
mean_dif <-meansteps_new-meansteps
median_dif<-mediansteps_new-mediansteps
total_dif<-sum(totalsteps_new$steps)-sum(totalsteps_new$steps)
```

1.  The mean of original data is 1.076618910^{4}
2.  The mean of imputed data is 1.076618910^{4}
3.  The median of original data is 10765
4.  The median of imputed data is 1.076618910^{4}
5.  The mean difference is 0.
6.  The median difference is 1.1886792.
7.  The total steps difference is 0.

comparing the patterns between weekdays and weekend
---------------------------------------------------

``` r
newdata$date<-as.Date(newdata$date)
```

    ## Warning in strptime(xx, f <- "%Y-%m-%d", tz = "GMT"): unknown timezone
    ## 'zone/tz/2017c.1.0/zoneinfo/Asia/Hong_Kong'

``` r
newdata$interval<-as.numeric(newdata$interval)

weekdays1<-c("Monday","Tuesday","Wednesday","Thursday","Friday")
newdata$day<-factor((weekdays(newdata$date) %in% weekdays1),levels=c(FALSE,TRUE),labels = c("weekend","weekdays"))
pattern_weekday<-aggregate(steps~interval+day,newdata,mean)
xyplot(pattern_weekday$steps~pattern_weekday$interval|pattern_weekday$day,main="Comparison between weekdays and weekend",xlab="Interval",ylab="Steps",layout=c(1,2),type="l")
```

![](Course_Project_files/figure-markdown_github/pattern_weekdays-1.png)
