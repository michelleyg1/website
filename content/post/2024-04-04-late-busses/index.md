---
title: NJ Transit Bus Performance
author: R package build
date: '2024-04-04'
slug: late-busses
categories: ["R", "Transit"]
tags: ["Busses", "NJ Transit", "Public Transit"]
description: An exploratory analysis of the percent of NJ Transit busses that have arrived late from January 2009 to January 2024.
image: "images/bus.jpeg"
math: ~
license: Michelle Gulotta
hidden: no
comments: no
---

I finally decided to search for data outside of the public health and biostatistics realm and looked into the type of data that NJ Transit (the main public transit system in New Jersey) collected and published online to be viewed and analyzed by the public. I chose to analyze the system wide [bus system performance data](https://www.njtransit.com/performance-data-download).

First to load some libraries.

## Load Libraries

```r
suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(FSA)
  library(rcompanion)
})
```

## Import Data

I'm just going to do a simple import from my desktop.

```r
bus <- read_csv("/Users/michellegulotta/Desktop/BUS_OTP_DATA.csv")
```

```
## Rows: 182 Columns: 5
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (5): OTP_YEAR, OTP_MONTH, OTP, TOTAL_TRIPS, TOTAL_LATES
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

## Clean Data

Data found in the wild, collected by government agencies (in my experience) is generally pretty ugly and needs some cleaning up, this one wasn't really that bad though, just some changing the data types and removing the first row as it just was a line that seperated the variable names from the data on the csv file.

I also created some variables. The date variable which uses the lubridate package function make_date(), the season variable using the months of the year, pct_late which reflects the percent of busses in that particular year and month combination that were recorded to have arrived late to the bus stop.

```r
bus <- bus[-1, ]
names(bus) <- tolower(names(bus))
bus <- bus %>% 
  mutate(
    across(
    .cols = c(1:5),
    .fns = ~as.numeric(.x)
    ),
    across(
      .cols = c(1, 2),
      .fns = ~as.factor(.x),
      .names = "{col}_f"
    ),
    pct_late = (total_lates / total_trips) * 100,
    date = make_date(otp_year, otp_month),
    season = as.factor(case_when(
      otp_month < 4 ~ "Winter",
      otp_month < 7 ~ "Spring",
      otp_month < 10 ~ "Summer",
      otp_month >= 10 ~ "Fall"
    ))
  )
```

## Percent of Busses Arriving Late from 2009 to 2024

I'm going to use ggplot to look at how the pct_late variable has changed over the years that this data has been recorded.

```r
ggplot(data = bus, mapping = aes(x = date, y = pct_late)) +
  geom_smooth(fill = "#F4823C", color = "#04529C", alpha = 0.2) +
  scale_x_date(breaks = "year", date_labels = "%Y") +
  labs(title = "Percent of NJ Transit Busses that Arrived Late from 2009 to 2024",
       x = "Year",
       y = "Percent",
       caption = "Source: NJ Transit Performance Data") +
  theme_linedraw() +
  theme(plot.title = element_text(hjust=0.5))
```

```
## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-4-1.png" width="672" />

These differences look pretty drastic, but ggplot did scale down the y axis quite a bit, so we are looking at a tiny fraction of the variable (under 10%).

### Linear Regression

I'm going to use the factored year variable to treat the year as a category, and see if there were any years associated with increased or decreased percent of busses arriving late. Looking at the graph above there are certainly ups and downs when the y axis is scaled down.

```r
bus.fit <- lm(pct_late ~ otp_year_f, bus)
summary(bus.fit)
```

```
## 
## Call:
## lm(formula = pct_late ~ otp_year_f, data = bus)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.0887 -0.8516 -0.0588  0.8064  5.3810 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)     6.50673    0.44010  14.785  < 2e-16 ***
## otp_year_f2010 -0.02779    0.62240  -0.045 0.964444    
## otp_year_f2011  1.46707    0.62240   2.357 0.019591 *  
## otp_year_f2012  2.93640    0.62240   4.718 5.05e-06 ***
## otp_year_f2013  2.46509    0.62240   3.961 0.000111 ***
## otp_year_f2014  2.65628    0.62240   4.268 3.32e-05 ***
## otp_year_f2015  2.67063    0.62240   4.291 3.02e-05 ***
## otp_year_f2016  2.60015    0.62240   4.178 4.76e-05 ***
## otp_year_f2017  3.14401    0.62240   5.051 1.15e-06 ***
## otp_year_f2018  2.83756    0.62240   4.559 9.97e-06 ***
## otp_year_f2019  1.16573    0.62240   1.873 0.062844 .  
## otp_year_f2020 -4.06767    0.62240  -6.535 7.56e-10 ***
## otp_year_f2021 -2.45210    0.62240  -3.940 0.000120 ***
## otp_year_f2022 -1.29334    0.62240  -2.078 0.039259 *  
## otp_year_f2023  1.15551    0.62240   1.857 0.065160 .  
## otp_year_f2024  0.02389    1.58682   0.015 0.988008    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.525 on 165 degrees of freedom
## Multiple R-squared:  0.6778,	Adjusted R-squared:  0.6485 
## F-statistic: 23.14 on 15 and 165 DF,  p-value: < 2.2e-16
```

The coefficient that stood out the most to me was the one associated with the year 2020, with a very large decrease in pct_late it is interesting to see how the pandemic affected these metrics.
Another interesting observation was that during the years that Chris Christie was governor of New Jersey, there were statistically significant increases from the 2009 intercept, and the year that he left office there was still an increase but only about half the size of the other years.

### Diagnostics

```r
par(mfrow = c(2, 2))
plot(bus.fit)
```

```
## Warning: not plotting observations with leverage one:
##   181
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-6-1.png" width="672" />
Besides the normality of the data, it looks okay. 

## Season Associated with Late Busses

Next, I wanted to see if there were any differences between the four seasons and percent of busses arriving late, I created the season variable when cleaning the data, and it does not capture it totally accurately as it was only based off of the month number, and there is some overlap in every month there is a season change, but the day to day data is not something that NJ transit collects.

### Box Plot

I'm going to use ggplot again to make a box plot of the data which shows average percent of NJ Transit busses arriving late by season. 

```r
ggplot(data = bus, mapping = aes(x = season, y = pct_late)) +
  geom_boxplot(fill = c("#F4823C", "#BC228C", "#04529C", "#F4F2F4")) +
  labs(title = "Average Percent of NJ Transit Busses that Arrived Late 
       from 2009 to 2024 by Season",
       x = "Season",
       y = "Average Percent of Busses Arriving Late",
       caption = "Source: NJ Transit Performance Data") +
  theme_linedraw() +
  theme(plot.title = element_text(hjust=0.5))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-7-1.png" width="672" />

### Kruskal Wallace Test

Next, since the data is relatively non normal I'm going to use a non-parametric alternative to compare means. 

```r
kruskal.test(pct_late ~ season, bus)
```

```
## 
## 	Kruskal-Wallis rank sum test
## 
## data:  pct_late by season
## Kruskal-Wallis chi-squared = 18.292, df = 3, p-value = 0.0003828
```
So there are statistically significant differences between the means at a very low p value. 

### Post Hoc Comparisons

Something else that I learned when researching for this post was that there is a non-parametric post hoc comparison that can be done to see within which groups the differences lie, called the Dunn test.

Here, I utilize the functions in the FSA and rcompanion package to display the results of this test.

```r
bus.dunn <- dunnTest(pct_late ~ season, bus, method = "bh")
bus.dunn.res <- bus.dunn$res
cldList(comparison = bus.dunn.res$Comparison,
        p.value = bus.dunn.res$P.adj,
        threshold = 0.05)
```

```
##    Group Letter MonoLetter
## 1   Fall      a         a 
## 2 Spring     ab         ab
## 3 Summer     ab         ab
## 4 Winter      b          b
```

As I thought from looking at the graph, the differences lie in the fall and winter average percentage. 

### Average Total Rides by Season

Lastly, I wanted to take a quick look at the mean total trips as this may reflect a pattern in why busses tend to be later in the fall than in the winter, perhaps since there are less busses to be late in the winter?

```r
bus_mean <- bus %>% 
  group_by(season) %>% 
  summarise_at(vars(total_trips),
               list(mean_trips = mean))
print(bus_mean)
```

```
## # A tibble: 4 × 2
##   season mean_trips
##   <fct>       <dbl>
## 1 Fall       41623.
## 2 Spring     41963.
## 3 Summer     44010.
## 4 Winter     41564.
```

The season with the most rides is summer, and between winter there are almost the same amount of trips, so it is interesting that in the Fall they tend to be late more often than in the winter.

Perhaps since all of December was counted in the Fall category, and it is influenced by the holidays. There is really not enough information in this dataset to be able to draw conclusions, so just speculating why this is the case.

## Conclusion

It was nice to take a step back from health related statistics for this analysis, especially away from survey data as I have been using that very often. This also allowed me to practice more with ggplot as I have been using base R plot syntax with the survey package for a lot of my posts recently.

I'm interested to see how NJ transit collects this data and what is considered late for a bus, as this is an important issue as many New Jerseyans rely on public transit to get to work, appointments, and other important places. It will be interesting to repeat this analysis in a few years now that we are (for the most part) in a post pandemic, work from home world.
