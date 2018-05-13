---
title: "Course 5: Project 1"
output: html_document
---


```r
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

This is an analysis of movement data - tracking the number of steps taken across time intervals in a day. 


```r
library(dplyr)
```

#### Average Steps Per Day

```r
data <- read.csv("activity.csv")

totals <- data %>% group_by(date) %>% 
  summarize(total = sum(steps, na.rm = T))
mean(totals$total) 
```

```
## [1] 9354.23
```

```r
median(totals$total)
```

```
## [1] 10395
```

```r
hist(x = totals$total, main = "Total Steps Per Day")
```

![plot of chunk one](figure/one-1.png)

#### Average and Maximum Steps Per Interval 

```r
intervals <- data %>% group_by(interval) %>% 
  summarize(avg.steps = mean(steps, na.rm = T))

with(intervals, plot(x = interval, y = avg.steps, type = "l"))
 title(main = "Average Steps Per Interval")
```

![plot of chunk two](figure/two-1.png)

```r
max.steps <- max(intervals$avg.steps)
```

#### Imputed Missing Values 

Using the interval average steps to substitute for missing values results in higher average steps.


```r
nrow(data %>% filter(is.na(steps) == T))
```

```
## [1] 2304
```

```r
merged <- merge(data, intervals, by = "interval")
merged$steps <- ifelse(is.na(merged$steps) == T, merged$avg.steps, merged$steps)

new_data <- merged %>% select(-avg.steps)

totals2 <- new_data %>% group_by(date) %>% 
  summarize(total = sum(steps, na.rm = T))
mean.total2 <- mean(totals2$total) 
median.total2 <- median(totals2$total)

hist(x = totals2$total, main = "Total Steps Per Day (Imputed Missing Values)")
```

![plot of chunk three](figure/three-1.png)


#### Weekdays vs Weekends


```r
data$date <- as.Date(data$date)
data_wk <- data %>% mutate(day.type = weekdays(data$date))
data_wk$day.type <- as.factor(ifelse(data_wk$day.type %in% c("Saturday", "Sunday"), 
                           "weekend", "weekday"))

data_wk_avg <- data_wk %>% group_by(interval, day.type) %>% 
  summarize(avg.steps = mean(steps, na.rm = T))

with(data_wk_avg %>% filter(day.type == "weekday"), 
     plot(x = interval, y = avg.steps, type = "l"))
 title(main = "Average Steps Per Interval on Weekdays")
```

![plot of chunk four](figure/four-1.png)

```r
with(data_wk_avg %>% filter(day.type == "weekend"), 
     plot(x = interval, y = avg.steps, type = "l"))
 title(main = "Average Steps Per Interval on the Weekend")
```

![plot of chunk four](figure/four-2.png)

