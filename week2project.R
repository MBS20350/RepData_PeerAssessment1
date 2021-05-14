## Reproducible Research Week Two Project
## Mark Sucato

library(tidyverse)
library(knitr)
library(kableExtra)

data <- read.csv("activity.csv")
data <- mutate(data, date = as.Date(date))
	
## Task 1 - What is mean total number of steps taken per day?

data1 <- data %>%
	group_by (date) %>%
	summarise(total = sum(steps))
ggplot(data1, aes(x = total))+
	geom_histogram(binwidth = 2000, na.rm = TRUE, fill="firebrick")+
	theme_light() +
	labs(title = "Distribution of Days by Total Number of Steps") + 
	labs(x = "Total Number of Steps per Day")
st <- as.data.frame(unclass(summary(data1$total)))
kable(st, col.names = c("Value"))

## Task 2 - What is the average daily activity pattern?

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
max <- data2 %>% filter(average == max(average)) 
kable(max, col.names = c("Max Interval", "Average Steps"), digits = 1, align = c("c", "c"))

## Task 3 - Imputing missing values

missing <- data %>% 
	filter(is.na(steps)) %>% 
	summarise(count = n())
kable(missing)

data3 <- data %>%
	left_join(data2)
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
st2 <- as.data.frame(unclass(summary(data3a$total)))
kable(st, col.names = c("With NAs")) %>%
	kable_styling(full_width = FALSE, position = "float_left")
kable(st2, col.names = c("Without NAs")) %>%
	kable_styling(full_width = FALSE, position = "left")

## Task 4 - Are there differences in activity patterns between weekdays and weekends?

data4 <- data3 %>%
	mutate(WE = weekdays(date)) %>%
	mutate(WE = as.factor(ifelse(WE == "Saturday" | WE == "Sunday", "weekend", "weekday"))) %>%
	group_by(interval, WE) %>%
	summarize(average = mean(steps))
ggplot(data4, aes(x=interval, y=average)) +
	geom_line() + 
	theme_light() + 
	labs(title = "Average Number of Steps by Time of Day") + 
	labs(y = "Average Number of Steps") +
	labs(x = "Time of Day (divided by five-minute interval)") +
	scale_x_continuous(breaks = c(300, 600, 900, 1200, 1500, 1800, 2100), 
		labels = c("3 am", "6 am", "9 am", "Noon", "3 pm", "6 pm", "9 pm")) +
	facet_grid(rows = vars(WE))

# library(knitr)
# setwd(<working directory>)
# knit2html(“document.Rmd”)"
# browseURL(“document.html”)


