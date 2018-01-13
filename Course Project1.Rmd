---
title: "Course project1"
author: "Sen Lin"
date: "12/3/2017"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
## Set up the environment
```{r environment,results="hide",message=FALSE,warning=FALSE}
library(dplyr)
library(zoo)
library(lattice)
```
## Load the data

```{r load}
rawdata <-read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

- Calculate the total number of steps taken per day

```{r sum}

data <-group_by(rawdata,date)
sumsteps <-summarise(data,sum(steps))
```

- Make a histogram of the total number of steps taken each day

```{r histogram}
hist(sumsteps$`sum(steps)`,)
```

- Calculate and report the mean and median of the total number of steps taken per day

```{r mean,results="hide"}
s.mean<-mean(sumsteps$`sum(steps)`,na.rm=TRUE)

```
The mean is `r s.mean`.

```{r median}
r.median<-median(sumsteps$`sum(steps)`,na.rm=TRUE)
```
The median is `r r.median`.

## Average daily activity pattern

```{r daily activity}
pattern <-rawdata[,c(1,3)]
pattern$interval <-as.numeric(pattern$interval)
by_interval <-aggregate(steps ~interval, data=pattern,FUN=mean)
plot(by_interval,type="l")

```

```{r maximum,results="hide"}
maxsteps <-summarise(by_interval,max(steps))
maxrow<-which(grepl(maxsteps,by_interval$steps))

```
In this data, the maximum of steps appears in the `r by_interval[maxrow,1]`th 5-minute interval.

## Imputing missing data
- The total number of raws including missing value
```{r the number of NA,results="hide"}
na.sum<-sum(is.na(pattern))
```
There are `r na.sum` raws including missing value.

- Filling in all of the missing values in the dataset

The strategy is to replace missing value with the average steps of the interval.
```{r filling,error=FALSE}
averagesteps<-aggregate(steps~interval,rawdata,mean)
newdata<-transform(rawdata,steps=ifelse(is.na(rawdata$steps),averagesteps$steps[match(rawdata$interval,averagesteps$interval)],rawdata$steps))
```
##compare imputed data with original data

- Calculate the average steps
```{r comparing}
totalsteps<-aggregate(steps~date,rawdata,sum)
totalsteps_new<-aggregate(steps~date,newdata,sum)
```
- Visualising the comparison

```{r steps taken each day}

hist(totalsteps_new$steps,col="red",xlab="Number of Steps",main="Steps taken each day")
hist(totalsteps$steps,col="black",add=T)
legend("topright",c("original","imputed"),col=c("black","red"),lwd=10)

```

- The difference between original and imputed data

```{r differences}
meansteps<-mean(totalsteps$steps)
mediansteps<-median(totalsteps$steps)
meansteps_new<-mean(totalsteps_new$steps)
mediansteps_new<-median(totalsteps_new$steps)
mean_dif <-meansteps_new-meansteps
median_dif<-mediansteps_new-mediansteps
total_dif<-sum(totalsteps_new$steps)-sum(totalsteps_new$steps)


```
1. The mean of original data is `r meansteps`
2. The mean of imputed data is `r meansteps_new`
3. The median of original data is `r mediansteps`
4. The median of imputed data is `r mediansteps_new`
5. The mean difference is `r mean_dif`.
6. The median difference is `r median_dif`.
7. The total steps difference is `r total_dif`.

##comparing the patterns between weekdays and weekend

```{r pattern_weekdays}

newdata$date<-as.Date(newdata$date)
newdata$interval<-as.numeric(newdata$interval)

weekdays1<-c("Monday","Tuesday","Wednesday","Thursday","Friday")
newdata$day<-factor((weekdays(newdata$date) %in% weekdays1),levels=c(FALSE,TRUE),labels = c("weekend","weekdays"))
pattern_weekday<-aggregate(steps~interval+day,newdata,mean)
xyplot(pattern_weekday$steps~pattern_weekday$interval|pattern_weekday$day,main="Comparison between weekdays and weekend",xlab="Interval",ylab="Steps",layout=c(1,2),type="l")
```
