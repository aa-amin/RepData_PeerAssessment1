---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---




## Loading and preprocessing the data


```r
# check the file and unzip
if(!file.exists("activity.csv")){unzip("activity.zip")}

# load data 
activity <- read.csv("activity.csv", sep = ",")
activity <- activity %>% mutate(date = as.Date(date,format = "%Y-%m-%d"),
                    day = weekdays(date))
```

## What is mean total number of steps taken per day?

First we compute total number of steps taken per day and present that in a histogram.



```r
total_steps_per_day <- activity %>% group_by(date) %>% 
      summarise(total = sum(steps, na.rm = T)) %>% 
      filter(total > 0) %>% # remove days with o steps because of NAs
      ungroup()
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
ggplot(total_steps_per_day, aes(total)) +
      geom_histogram(bins = 5, fill = "blue")+
      labs(x="Steps", 
           y="Frequency",
           title = "Total Steps per Day")
```

![](PA1_template_files/figure-html/histogram_mean_median1-1.png)<!-- -->

```r
mean_med <- total_steps_per_day %>% 
      summarise(mean = round(mean(total, na.rm = T)),
                median = median(total, na.rm = T)) %>% ungroup()
kable(mean_med, caption = 'The mean and median of the total number of steps taken per day.', label = 'tbl1')
```



Table: The mean and median of the total number of steps taken per day.

|  mean| median|
|-----:|------:|
| 10766|  10765|

## What is the average daily activity pattern?

We compute the average number of steps taken, averaged across all days for each interval and then we make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken (y-axis).




```r
mean_steps_per_interval <- activity %>% group_by(interval) %>% 
      summarise(mean = mean(steps, na.rm = T)) %>% 
      ungroup()
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
ggplot(mean_steps_per_interval, aes(x=interval, y=mean)) +
      geom_line() +
      labs(y="Average number of steps", 
           x="5-minute Interval",
           title = "Average number of steps per 5-minute interval")
```

![](PA1_template_files/figure-html/mean_by_interval-1-1.png)<!-- -->

```r
max <- mean_steps_per_interval[mean_steps_per_interval$mean==max(mean_steps_per_interval$mean),1]
max
```

```
## # A tibble: 1 x 1
##   interval
##      <int>
## 1      835
```

The 5-minute interval, on average across all the days in the dataset, that contains the maximum number of steps is 835 interval. 

In addition, most of the average daily number of steps are between 25 and 75 for most of the 5-minute intervals frm 500 to 2000. 



## Imputing missing values

we can have a quick look at the number of missing values in each one of the three variables, respectively, as: 2304, 0, and 0. As the main variable here is the first one, `steps`, so the total number of missing values in the dataset is 2304. 




```r
activity_clean <- activity %>%
  group_by(interval)%>%
  mutate(mean_steps = mean(steps, na.rm = T)) %>%
  ungroup() %>% mutate(steps = ifelse(is.na(steps), mean_steps, steps ) )
```

We devise a strategy for filling in all of the missing values in the dataset by simply using the mean for the 5-minute interval.
Based on this strategy we create a new dataset that is equal to the original dataset but with the missing data filled in, and present the the total number of steps taken each day in the following histogram, followed by reported mean and median. 



```r
total_steps_fill_per_day <- activity_clean %>% group_by(date) %>% 
      summarise(total = sum(steps, na.rm = T)) %>% 
      filter(total > 0) %>% # remove days with o steps because of NAs
      ungroup()
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
ggplot(total_steps_fill_per_day, aes(total)) +
      geom_histogram(bins = 5, fill = "blue")+
      labs(x="Steps", 
           y="Frequency",
           title = "Total Steps per Day with imputed missing values")
```

![](PA1_template_files/figure-html/histogram_mean_median2-1.png)<!-- -->

```r
mean_med_fill <- total_steps_fill_per_day %>% 
      summarise(mean = round(mean(total, na.rm = T)),
                median = median(total, na.rm = T)) %>% ungroup()
kable(mean_med_fill, caption = 'The mean and median of the total number of steps taken per day with imputed missing values.', label = 'tbl2')
```



Table: The mean and median of the total number of steps taken per day with imputed missing values.

|  mean|   median|
|-----:|--------:|
| 10766| 10766.19|


we can observe that imputing the missing data with the proposed strategy has an impact on the total daily number of steps, where steps with moderate values have a higher frequency values (histogram is more centralized as shown below in the graph), howevere there is no impact on the central measures, i.e. mean and median.





![](PA1_template_files/figure-html/histogram_comparison-1.png)<!-- -->





## Are there differences in activity patterns between weekdays and weekends?






```r
activity_clean <- activity_clean %>% mutate(days_factor = day %in% c("Saturday", "Sunday" ), 
                          days_factor = ifelse(days_factor, "weekend", "weekday" ) )


mean_steps_per_interval_factor <- activity_clean %>% group_by(interval, days_factor) %>% 
      summarise(mean = mean(steps, na.rm = T)) %>% 
      ungroup()
```

```
## `summarise()` regrouping output by 'interval' (override with `.groups` argument)
```

```r
ggplot(mean_steps_per_interval_factor, aes(x=interval, y=mean)) +
      geom_line() +
      facet_wrap(~days_factor, nrow =2) +
      labs(y="Average number of steps", 
           x="5-minute Interval",
           title = "Average number of steps per 5-minute interval")
```

![](PA1_template_files/figure-html/mean_by_interval-2-1.png)<!-- -->

It is very clear from this time plot that in most of the 5-minute intervals the weekend days have higher average number of steps compared to weekday days.

