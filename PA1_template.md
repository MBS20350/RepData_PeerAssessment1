---
title: "Reproducible Research: Peer Assessment 1"
author:  Mark Sucato
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data

The raw data for this project is saved in CSV format.  A single transformation is used to transform
the 'date' variable from a factor to date format.


```r
library(tidyverse)
data <- read.csv("activity.csv")
data <- mutate(data, date = as.Date(date))
```

## What is the mean total number of steps taken per day?

A histogram of the total number of steps per day indicates a symmetric distribution with some outliers.


```r
data1 <- data %>%
	group_by (date) %>%
	summarise(total = sum(steps))
ggplot(data1, aes(x = total))+
	geom_histogram(binwidth = 2000, na.rm = TRUE, fill="firebrick")+
	theme_light() +
	labs(title = "Distribution of Days by Total Number of Steps") + 
	labs(x = "Total Number of Steps per Day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

Summary statistics verify the visually symmetric distribution with virtually identical
mean and median.


```r
st <- as.data.frame(unclass(summary(data1$total)))
kable(st, col.names = c("Value"))
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> Value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Min. </td>
   <td style="text-align:right;"> 41.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1st Qu. </td>
   <td style="text-align:right;"> 8841.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Median </td>
   <td style="text-align:right;"> 10765.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mean </td>
   <td style="text-align:right;"> 10766.19 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3rd Qu. </td>
   <td style="text-align:right;"> 13294.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Max. </td>
   <td style="text-align:right;"> 21194.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NA's </td>
   <td style="text-align:right;"> 8.00 </td>
  </tr>
</tbody>
</table>

## What is the average daily activity pattern?

Visual examination of the average daily pattern indicates lulls during common sleeping times and activity
during the day.  The period of maximum average steps occurs at 8:35 am with 206 steps..
  

```r
data2 <- data %>%
	filter(!is.na(steps)) %>%
	group_by(interval) %>%
	summarize(average = mean(steps))
ggplot(data2, aes(x=interval, y=average)) +
	geom_line() + 
	theme_light() + 
	labs(title = "Average Number of Steps by Time of Day") + 
	labs(y = "Average Number of Steps") +
	labs(x = "Time of Day (divided by five-minute interval)") +
	scale_x_continuous(breaks = c(300, 600, 900, 1200, 1500, 1800, 2100), 
		labels = c("3 am", "6 am", "9 am", "Noon", "3 pm", "6 pm", "9 pm"))
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

```r
max <- data2 %>% filter(average == max(average)) 
kable(max, col.names = c("Max Interval", "Average Steps"), digits = 1, align = c("c", "c"))
```

<table>
 <thead>
  <tr>
   <th style="text-align:center;"> Max Interval </th>
   <th style="text-align:center;"> Average Steps </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> 835 </td>
   <td style="text-align:center;"> 206.2 </td>
  </tr>
</tbody>
</table>

## Imputing missing values

Dplyr-based analysis of the raw data indicates 2,304 observations with missing step values.  There
are no missing dates or intervals.


```r
missing <- data %>% 
	filter(is.na(steps)) %>% 
	summarise(count = n())
kable(missing)
```

<table>
 <thead>
  <tr>
   <th style="text-align:right;"> count </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 2304 </td>
  </tr>
</tbody>
</table>
In this case, an imputing strategy of replacing NAs with the average for the specific interval
period in question across all days was chosen.  NAs were removed from the data to produce the time-series
plot shown immediately above.  'Left_join and 'coalesce' from the 'dplyr' package were used in 
conjunction with this revised data set to replace the NAs.  A histogram of the wrangled data
indicates inputing the average interval value retained the previous distribution symmetry but reduced the
 variation, indicated by a more vertical and "skinnier" distribution.    

```r
data3 <- data %>%
	left_join(data2)
```

```
## Joining, by = "interval"
```

```r
data3$steps <- coalesce(data3$steps, data3$average)
data3a <- data3 %>%
	group_by (date) %>%
	summarise(total = sum(steps))
ggplot(data3a, aes(x = total))+
	geom_histogram(binwidth = 2000, na.rm = TRUE, fill="steelblue")+
	theme_light() +
	labs(title = "Distribution of Days by Total Number of Steps") +
	labs(subtitle = "NA values replaced by imputing time-of-day average") + 
	labs(x = "Total Number of Steps per Day")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

```r
st2 <- as.data.frame(unclass(summary(data3a$total)))
kable(st, col.names = c("With NAs")) %>%
	kable_styling(full_width = FALSE, position = "float_left")
kable(st2, col.names = c("Without NAs")) %>%
	kable_styling(full_width = FALSE, position = "left")
```
<table class="table" style="width: auto !important; float: left; margin-right: 10px;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> With NAs </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Min. </td>
   <td style="text-align:right;"> 41.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1st Qu. </td>
   <td style="text-align:right;"> 8841.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Median </td>
   <td style="text-align:right;"> 10765.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mean </td>
   <td style="text-align:right;"> 10766.19 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3rd Qu. </td>
   <td style="text-align:right;"> 13294.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Max. </td>
   <td style="text-align:right;"> 21194.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NA's </td>
   <td style="text-align:right;"> 8.00 </td>
  </tr>
</tbody>
</table>

<table class="table" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> Without NAs </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Min. </td>
   <td style="text-align:right;"> 41.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1st Qu. </td>
   <td style="text-align:right;"> 9819.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Median </td>
   <td style="text-align:right;"> 10766.19 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mean </td>
   <td style="text-align:right;"> 10766.19 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3rd Qu. </td>
   <td style="text-align:right;"> 12811.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Max. </td>
   <td style="text-align:right;"> 21194.00 </td>
  </tr>
</tbody>
</table>
<br> 
Comparison of the summary statistics for the two data sets shows identical minimums, maximums, means and medians, but different
quartile values.  These results reinforce the noted reduced variation.  Minimum and maximum values
come from single observations present in the raw data set; inclusion of average values derived from
the same data set will not change these values.  Similarly, the mean and median values did not change
due to the use of averages as replacement values.  By contrast, the quartiles are indications of
variation and appreciably changed. 

## Are there differences in activity patterns between weekdays and weekends?

Creating a factor to denote weekdays and weekends, and then plotting the data set using weekday/weekend
 as a panel facet, reveals differences in the two facets. The weekday pattern looks very similar to
the aggregated dataset's pattern, but the weekend pattern shows differences.  For the
weekend plot, the large early morning spike is absent and activity throughout the day remains at a
higher level. This seems reasonable given many individuals start their weekends later in the morning
and spend the day doing non-sedentary activities.


```r
data4 <- data3 %>%
	mutate(WE = weekdays(date)) %>%
	mutate(WE = as.factor(ifelse(WE == "Saturday" | WE == "Sunday", "weekend", "weekday"))) %>%
	group_by(interval, WE) %>%
	summarize(average = mean(steps))
```

```
## `summarise()` has grouped output by 'interval'. You can override using the `.groups` argument.
```

```r
ggplot(data4, aes(x=interval, y=average)) +
	geom_line() + 
	theme_light() + 
	labs(title = "Average Number of Steps by Time of Day") + 
	labs(y = "Average Number of Steps") +
	labs(x = "Time of Day (divided by five-minute interval)") +
	scale_x_continuous(breaks = c(300, 600, 900, 1200, 1500, 1800, 2100), 
		labels = c("3 am", "6 am", "9 am", "Noon", "3 pm", "6 pm", "9 pm")) +
	facet_grid(rows = vars(WE))
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)
