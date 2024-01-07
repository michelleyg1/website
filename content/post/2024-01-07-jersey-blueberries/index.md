---
title: Jersey Blueberries
author: Michelle Gulotta
date: '2024-01-07'
slug: jersey-blueberries
categories: ["R", "Agriculture"]
tags: []
description: A data visualization of the condition of New Jersey blueberries in the 2021 growing season
image: "images/blueberry.jpeg"
math: ~
license: Michelle Gulotta
hidden: no
comments: no
---

I found this dataset by filtering through the various settings that they have on the National Agriculture Statistics Service [quick stats tool](https://quickstats.nass.usda.gov/) to see a survey of the condition of blueberries by year and week in the state of New Jersey. 

A lot of people think that New Jersey is only the city and the shore, but its not called the Garden State for nothing! For its small size, New Jersey punches above its weight class in producing and exporting various agricultural products.

One crop that New Jersey is known for is its blueberries.

## Load Packages

```r
suppressPackageStartupMessages({library(tidyverse)
library(janitor)})
```

## Import Dataset

```r
blueberries <- read_csv("/Users/michellegulotta/Desktop/my_first_project/blueberries/blueberry.csv") 
```

```
## Rows: 225 Columns: 21
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (9): Program, Period, Geo Level, State, watershed_code, Commodity, Data...
## dbl  (3): Year, State ANSI, Value
## lgl  (8): Ag District, Ag District Code, County, County ANSI, Zip Code, Regi...
## date (1): Week Ending
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
blueberries <- blueberries %>% 
  janitor::clean_names(., "snake") %>% 
  select(year, period, week_ending, data_item, value) %>% 
  rename(
    condition = data_item,
    percent = value,
    week_number = period
  ) %>% print()
```

```
## # A tibble: 225 × 5
##     year week_number week_ending condition                               percent
##    <dbl> <chr>       <date>      <chr>                                     <dbl>
##  1  2023 WEEK #26    2023-07-02  BLUEBERRIES, TAME - CONDITION, MEASURE…       0
##  2  2023 WEEK #26    2023-07-02  BLUEBERRIES, TAME - CONDITION, MEASURE…       0
##  3  2023 WEEK #26    2023-07-02  BLUEBERRIES, TAME - CONDITION, MEASURE…     100
##  4  2023 WEEK #26    2023-07-02  BLUEBERRIES, TAME - CONDITION, MEASURE…       0
##  5  2023 WEEK #26    2023-07-02  BLUEBERRIES, TAME - CONDITION, MEASURE…       0
##  6  2023 WEEK #27    2023-07-09  BLUEBERRIES, TAME - CONDITION, MEASURE…      18
##  7  2023 WEEK #27    2023-07-09  BLUEBERRIES, TAME - CONDITION, MEASURE…      19
##  8  2023 WEEK #27    2023-07-09  BLUEBERRIES, TAME - CONDITION, MEASURE…      44
##  9  2023 WEEK #27    2023-07-09  BLUEBERRIES, TAME - CONDITION, MEASURE…      19
## 10  2023 WEEK #27    2023-07-09  BLUEBERRIES, TAME - CONDITION, MEASURE…       0
## # ℹ 215 more rows
```

## Cleaning Data
### Clean up Condition and Week Number Column
I wanted to clean up the condition column as I noticed it was pretty redundant, and all we really needed was the condition. This would also make it eaiser as this is going to be the legend on our graph. Another thing that I wanted to do was to make the week number column more simple, as the name of the variable already tells us that this is the week number, so all we really need in the observation is the number.



```r
blueberries$condition <- str_replace_all(blueberries$condition, "BLUEBERRIES, TAME - CONDITION, MEASURED IN PCT", "")
blueberries$week_number <- str_replace_all(blueberries$week_number, "WEEK #", "")
```

Checking to make sure that the different conditions look good

```r
unique(blueberries$condition)
```

```
## [1] " EXCELLENT" " FAIR"      " GOOD"      " POOR"      " VERY POOR"
```

It looks like there's some random leading and trailing white space, so I'm going to clean that up using the trimws() function, as well as the str_to_title() function to make it more readable and look nicer on our graph


```r
blueberries$condition <- trimws(blueberries$condition)
blueberries$condition <- str_to_title(blueberries$condition)
```

Checking the conditions again

```r
unique(blueberries$condition)
```

```
## [1] "Excellent" "Fair"      "Good"      "Poor"      "Very Poor"
```
Looks good! Time to make a data visualization

## Data Visualization


```r
blueberries_2021 <- blueberries %>% 
  filter(year == 2021) %>% 
  mutate(
    condition_f = factor(condition)
  ) %>%
  print()
```

```
## # A tibble: 55 × 6
##     year week_number week_ending condition percent condition_f
##    <dbl> <chr>       <date>      <chr>       <dbl> <fct>      
##  1  2021 25          2021-06-27  Excellent      25 Excellent  
##  2  2021 25          2021-06-27  Fair           25 Fair       
##  3  2021 25          2021-06-27  Good           50 Good       
##  4  2021 25          2021-06-27  Poor            0 Poor       
##  5  2021 25          2021-06-27  Very Poor       0 Very Poor  
##  6  2021 26          2021-07-04  Excellent      84 Excellent  
##  7  2021 26          2021-07-04  Fair            8 Fair       
##  8  2021 26          2021-07-04  Good            8 Good       
##  9  2021 26          2021-07-04  Poor            0 Poor       
## 10  2021 26          2021-07-04  Very Poor       0 Very Poor  
## # ℹ 45 more rows
```

### Factor to Change Order of Legend
One of the problems that I came across while trying to make this graph was that the legend would appear in alphabetical order rather than in the order that made sense from excellent to very poor. After some trying and researching solutions, I realized that I could use the factor() function to change the order of the levels.


```r
blueberries_2021$condition_f <- factor(blueberries_2021$condition_f,
                                       levels = c("Excellent", "Good", "Fair", "Poor", "Very Poor"))
```

### Make Plot


```r
ggplot(blueberries_2021, aes(x = week_ending, 
                             y = percent,
                             fill = condition_f)) +
  geom_area(alpha = 0.5, position = "identity") +
  scale_fill_manual(values = c("#785EF0", "#009E73", "#FFB000", "#FE6100", "#DC267F")) +
  labs(title = "Condition of Blueberries Measured in Percent in New Jersey in 2021", 
       x = "Week", 
       y = "Percent",
       fill = "Condition",
       caption = "Source: USDA National Agrigultural Statistics Service") +
  theme_light()
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-9-1.png" width="672" />

## Conclusion 
Now it's the beginning of January but this has me looking forward to the beginning of July! 

Working on this specific visualization helped me work on my data cleaning skills. I also learned more about how factors work and how R automatically puts a character vector in alphabetical order, and that in order to get it to appear in the order you want on a legend, you have to change the order of the factor variable. I didn't think about that before I started but it definitely makes sense, R doesn't just know how humans rank the quality of blueberries. 

I also learned about geom_area(), I first tried to use geom_line() and was able to make that pretty easily, but when I tried to change it to geom_area() there was a lot that I had to change in order to get the effect that I wanted on my graph. 
