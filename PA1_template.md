# Reproducible Research: Peer Assessment 1

## Loading required packages
Install packages if necessary and load them into the workspace

```r
list.of.packages <- c("ggplot2","gridExtra")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(ggplot2)
par(mfrow=c(1,1))
```


## Loading and preprocessing the data
Let's load the activity csv-file and have a look at its structure:


```r
act_withna <- read.csv("activity.csv")
```

Now, let us sort out NA values, which are only present in the steps variable.


```r
act_wona <- act_withna[which(!is.na(act_withna$steps)),]
```

Let us save a set of activity values without date class (as it does not work on aggregate)

```r
act_wona$date <- as.character(act_wona$date)
act_withna$date <- as.character(act_withna$date)
```


## What is mean total number of steps taken per day?

Calculate the sum of steps per day


```r
attach(act_wona)
act_wona_agg_date <- aggregate(x=steps,by=list(date=date),FUN=sum)
detach(act_wona)
```

Show the histogram of total steps taken per day


```r
hist(act_wona_agg_date$x,main="Histogram - Total steps taken per day",xlab= "Steps taken per day", ylab="Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

Calculate the mean and median values of the total steps taken per day


```r
act_wona_agg_date_mean <- mean(act_wona_agg_date$x)
act_wona_agg_date_median <- median(act_wona_agg_date$x)
```

The mean value is 1.0766189\times 10^{4} and the median value is 10765

## What is the average daily activity pattern?

Show the average daily activity pattern by taking the mean of each interval across all days


```r
attach(act_wona)
act_wona_agg_int <-aggregate(x=steps,by=list(interval=interval),FUN=mean)
detach(act_wona)
ggplot(act_wona_agg_int) + geom_line(aes(x=act_wona_agg_int$interval,y=act_wona_agg_int$x)) + scale_x_continuous(limits=c(0,2355),breaks=c(0,0600,1200,1800,2355),labels=c("12 am","6 am","12 pm","6 pm","11.55 pm")) + labs(x="Average steps per interval",y="Intervals")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
act_wona_agg_int_maxintval <- max(act_wona_agg_int$x)
act_wona_agg_int_maxint <- act_wona_agg_int$interval[which.max(act_wona_agg_int$x)]
```

The interval with highest average value (206.1698113) is the interval at 835

## Imputing missing values

Before we start here. Let us enumerate the different versions of the dataset we are about to create. There will be three datasets :
- the initial datasets, containing NA values
- a dataset without NA values (deleted)
- a dataset with imputed NA values (NA values replaced by mean values per interval)

Calculate the number of missing values.


```r
missingvaluesnumber <- sum(!complete.cases(act_withna))
missingvaluespercentage <-round((sum(!complete.cases(act_withna))/dim(act_withna)[1])*100,2)
```

There are 2304 missing values in the data set, which make of 13.11% of the whole data set.

Before imputing mean values to replace the missing values in the "steps" columns, let us create the future "activity without NA" data frame.


```r
act_narep <- act_withna
```

Then, design a function which looks for the mean value for any given interval.


```r
imputemean <- function(giveninterval){
  return (act_wona_agg_int[which(act_wona_agg_int$interval==giveninterval),]$x)
}
```

Finally, go through all the rows containing missing values and replace the NA value by mean values of the given interval. 


```r
for (i in as.integer(row.names(act_withna[which(is.na(act_withna$steps)==TRUE),]))) {
  act_narep$steps[i] <- imputemean(act_narep$interval[i])
}
```

Calculate total number of steps per day by using the newly created dataset with imputed NA values (NA measurements replaced by mean values).


```r
attach(act_narep)
act_narep_agg_date <- aggregate(x=steps,by=list(date=date),FUN=sum)
act_narep_agg_int <-aggregate(x=steps,by=list(interval=interval),FUN=mean) #this is for further use
detach(act_narep)
hist(act_narep_agg_date$x, xlab = "total number of steps", main ="Total number of steps per day (imputed NA values)")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

Now let us have a look at the difference in mean and median values between the dataset with deleted NA values and the one with imputed NA values

First, calculate mean and median values of the dataset with imputed NA values.


```r
act_narep_agg_date_mean <- mean(act_narep_agg_date$x)
act_narep_agg_date_median <- median(act_narep_agg_date$x)
```

Then, let us compare those numbers with the ones of the dataset without NA values (deleted) :


```r
temp_datanames <- c("Imputed NA values","deleted NA values")
comparison_mean <- data.frame(cbind(temp_datanames,rbind(act_narep_agg_date_mean,act_wona_agg_date_mean)))
colnames(comparison_mean)<-c("dataset","value")
comparison_median <- data.frame(cbind(temp_datanames,rbind(act_narep_agg_date_median,act_wona_agg_date_median)))
colnames(comparison_median)<-c("dataset","value")

plot1 <- qplot(x=comparison_mean$dataset,y=comparison_mean$value,main="Difference between daily mean values",xlab="daily mean",ylab="dataset")+theme(panel.background = element_rect(fill = '#ccffff', colour = '#3399ff'))
plot2 <- qplot(x=comparison_median$dataset,y=comparison_median$value,main="Difference between daily median values",xlab="daily median",ylab="dataset")+theme(panel.background = element_rect(fill = '#ffffcc', colour = '#cc9900'))
library(gridExtra)
grid.arrange(plot1, plot2, nrow=2, ncol=1)
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

The two plots above show no significant difference in mean and median values.

However, the total number of steps is clearly higher in the dataset with imputed NA values (656737) than in the dataset without NA values (570608).

So, how can we explain the (almost) identical mean and median values between the datasets despite the big difference in total steps measured?

Is there an interval which "hides" the difference? Which interval could it be?


```r
comparison_int <- merge(act_narep_agg_int,act_wona_agg_int,by="interval",all = T)
comparison_int <- comparison_int[order(comparison_int$interval),]
names(comparison_int)<-c("interval","Replaced NAs","Initial Values")
comparison_int[which(is.na(comparison_int$`Initial Values`)),3]<-rep(0,dim(comparison_int[which(is.na(comparison_int$`Initial Values`)),])[1])
comparison_int$difference <- (comparison_int$`Replaced NAs` - comparison_int$`Initial Values`)
ggplot(comparison_int,aes(x=comparison_int$interval,y=comparison_int$difference))+geom_line(stat="identity",position="stack",colour="#66ccff")+labs(x = "Intervals", y = "Difference in mean steps per interval",title="Difference between initial dataset and the one with imputed NA values")+scale_x_continuous(limits = c(0,2355),breaks=c(0,0600,1200,1800,2355),labels =c("12 am","6 am","12 pm","6 pm","11.55 pm"))
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

The plot above clearly shows no difference in mean values per interval. Consequently, let us have a look at the differences in total steps per date between the datasets.


```r
comparison <- merge(act_narep_agg_date,act_wona_agg_date,by="date",all = T)
comparison <- comparison[order(comparison$date),]
names(comparison)<-c("date","Replaced NAs","Initial Values")
comparison[which(is.na(comparison$`Initial Values`)),3]<-rep(0,dim(comparison[which(is.na(comparison$`Initial Values`)),])[1])
comparison$difference <- (comparison$`Replaced NAs` - comparison$`Initial Values`)
ggplot(comparison,aes(x=comparison$date,y=comparison$difference))+geom_bar(stat="identity",position="stack",fill="#66ccff")+labs(x = "Dates", y = "Difference in total steps",title="Difference between initial dataset and the one with imputed NA values")+theme(axis.text.x=element_blank())
```

![](PA1_template_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

The plot above clearly indicates substantial differences between the datasets, but only at certain dates.

While having a closer look at the initial dataset (containing NA values), we can see that at certain dates only NA values have been measured. At these dates, not a single valid measurement was made.

Therefore, there is almost no difference between the average daily means and medians because all the values of these dates were replaced by mean values.

When calculating the mean value, the increase in total steps due to imputation is compensated by the higher number of days. Thus, no difference there.

## Are there differences in activity patterns between weekdays and weekends?

First of all, we need to create a weekday/weekend factor variable based on the date. 


```r
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday','Lundi', 'Mardi', 'Mercredi', 'Jeudi', 'Vendredi','lundi', 'mardi', 'mercredi', 'jeudi', 'vendredi')
act_narep$daytype <- factor(weekdays(strptime(as.character(act_narep$date),"%Y-%m-%d")) %in% weekdays1,levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))

#Let us prepare two datasets, one containing weekday data, the other weekend data 
act_narep_weekdays <- subset(act_narep,act_narep$daytype=="weekday")
act_narep_weekends <- subset(act_narep,act_narep$daytype=="weekend")

#Process weekday data
attach(act_narep_weekdays)
act_narep_weekdays_agg_int <-aggregate(x=steps,by=list(interval=interval),FUN=mean) #this is for further use
detach(act_narep_weekdays)

#Process weekend data
attach(act_narep_weekends)
act_narep_weekends_agg_int <-aggregate(x=steps,by=list(interval=interval),FUN=mean) #this is for further use
detach(act_narep_weekends)

#Produce the plots
plot1 <- ggplot(act_narep_weekdays_agg_int,aes(x=act_narep_weekdays_agg_int$interval,y=act_narep_weekdays_agg_int$x))+geom_line(stat="identity",position="stack",colour="#66ccff")+labs(x = "Intervals", y = "Mean steps per interval",title="Mean steps per interval on weekdays")+scale_x_continuous(limits = c(0,2355),breaks=c(0,0600,1200,1800,2355),labels =c("12 am","6 am","12 pm","6 pm","11.55 pm"))+ylim(c(min(act_narep_weekdays_agg_int$x),max(act_narep_weekdays_agg_int$x)))+geom_hline(yintercept = mean(act_narep_weekdays_agg_int$x),colour="blue")
plot2 <- ggplot(act_narep_weekends_agg_int,aes(x=act_narep_weekends_agg_int$interval,y=act_narep_weekends_agg_int$x))+geom_line(stat="identity",position="stack",colour="#66ccff")+labs(x = "Intervals", y = "Mean steps per interval",title="Mean steps per interval on weekends")+scale_x_continuous(limits = c(0,2355),breaks=c(0,0600,1200,1800,2355),labels =c("12 am","6 am","12 pm","6 pm","11.55 pm"))+ylim(c(min(act_narep_weekdays_agg_int$x),max(act_narep_weekdays_agg_int$x)))+geom_hline(yintercept = mean(act_narep_weekends_agg_int$x),colour="blue")

grid.arrange(plot1, plot2, nrow=2, ncol=1)
```

![](PA1_template_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

As shown by the two plots above, there are strong differences between the daily activity pattern, among which are the following:
- high activity in the early morning on weekdays
- more even pattern on weekends
- slightly higher activity during weekends
- activties start later on weekends.
