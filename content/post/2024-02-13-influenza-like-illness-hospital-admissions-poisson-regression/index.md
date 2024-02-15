---
title: Influenza Like Illness Hospital Admissions Poisson Regression
author: R package build
date: '2024-02-13'
slug: influenza-like-illness-hospital-admissions-poisson-regression
categories: ["R", "Public Health"]
tags: []
description: A poisson regression model for NYC Open Data's Hospital Data for ED visits and Adminssions for Influenza Like Illness in the most Populous Zip Code in New York City
image: "images/pois.jpeg"
math: ~
license: Michelle Gulotta
hidden: no
comments: no
---

For this analysis I was looking for data that captured count data so that I could practice using a poisson regression. I came across the NYC Open Data's [Emergency Department Visits and Admissions for Influenza-like Illness and/or Pneumonia](https://data.cityofnewyork.us/Health/Emergency-Department-Visits-and-Admissions-for-Inf/2nwg-uqyg/about_data) and decided to bring that into R to analyze. 

## Load Libraries

```r
suppressPackageStartupMessages({
  library(tidyverse)
  library(RSocrata)
  library(lubridate)
})
```

## Load Data

I chose 10,000 rows from the most populous zip code in New York City, 11368 which is in Corona, Queens. I chose this as I had trouble using the read.socrata() function with larger amounts of data.

```r
resp <- read.socrata("https://data.cityofnewyork.us/resource/2nwg-uqyg.json?mod_zcta=11368&$limit=10000")
```

Checking the different types of data that are stored in the different variables in the dataset.

```r
str(resp)
```

```
## 'data.frame':	10000 obs. of  6 variables:
##  $ extract_date      : POSIXct, format: "2021-09-27" "2021-07-08" ...
##  $ date              : POSIXct, format: "2021-01-28" "2021-02-20" ...
##  $ mod_zcta          : chr  "11368" "11368" "11368" "11368" ...
##  $ total_ed_visits   : chr  "122" "117" "96" "120" ...
##  $ ili_pne_visits    : chr  "10" "4" "3" "6" ...
##  $ ili_pne_admissions: chr  "2" "1" "1" "1" ...
```
Looks like the counts are stored as characters, and the dates are stored in POSIXct format. I'm going to need to convert the data types for the counts, and I'll also drop the extract_date variable as that is not very useful to my analysis, as well as the mod_zcta variable since all of the values in this particular dataset are from the same zip code.

```r
resp <- resp %>% 
  dplyr::select(-extract_date, -mod_zcta) %>% 
  mutate(
    total_ed_visits = as.numeric(total_ed_visits),
    ili_pne_visits = as.numeric(ili_pne_visits),
    ili_pne_admissions = as.numeric(ili_pne_admissions),
    month = factor(month(ymd(resp$date)))
  )
```

## Visualize Data

First I'm going to make a long version of the data so that I can group them by outcome and have a legend that shows which line is which and in different colors, I used pivot_longer() to achieve this. I also converted the date column from datetime to just date, so that I can later use the scale_x_date function in ggplot to scale my x axis.

```r
resp_long <- resp %>% 
  pivot_longer(cols = c(total_ed_visits, ili_pne_visits, ili_pne_admissions),
               names_to = "outcome",
               values_to = "count") %>% 
  mutate(
    date = as.Date(date)
  )
```

Then, I used ggplot to visualize the data.

```r
ggplot(data = resp_long, aes(x = date, y = count, color = outcome)) +
  geom_smooth(se = FALSE) +
  scale_color_discrete(name = "Outcome",
                       labels = c("Admissions for ILI*",
                                  "ED Visits for ILI*",
                                  "All ED Visits")) +
  labs(caption = "*Influenza Like Illness",
       x = "Date",
       y = "Count",
       title = "Count of Emergency Department Visits (All Cause and ILI*) 
       and Hospital Admissions for ILI*") +
  scale_x_date(date_breaks = "4 months") +
  theme_light() + 
  theme(plot.title = element_text(hjust=0.5))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-6-1.png" width="672" />

## Poisson Regression
### Fit Model

```r
pois.resp<- glm(ili_pne_admissions ~ month,
                 data = resp,
                 family = poisson)
summary(pois.resp)
```

```
## 
## Call:
## glm(formula = ili_pne_admissions ~ month, family = poisson, data = resp)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -5.0974  -1.2311  -0.3399   0.5260   7.9668  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  1.15670    0.02425  47.706  < 2e-16 ***
## month2      -0.05197    0.03821  -1.360    0.174    
## month3       1.40761    0.02553  55.145  < 2e-16 ***
## month4       1.31247    0.02572  51.025  < 2e-16 ***
## month5      -0.83506    0.03479 -24.005  < 2e-16 ***
## month6      -1.73316    0.04833 -35.860  < 2e-16 ***
## month7      -1.44372    0.04373 -33.014  < 2e-16 ***
## month8      -1.10669    0.04060 -27.259  < 2e-16 ***
## month9      -1.68666    0.05373 -31.393  < 2e-16 ***
## month10     -1.43405    0.04957 -28.932  < 2e-16 ***
## month11     -0.92730    0.04361 -21.263  < 2e-16 ***
## month12     -0.21770    0.03533  -6.161 7.21e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for poisson family taken to be 1)
## 
##     Null deviance: 93222  on 9999  degrees of freedom
## Residual deviance: 44040  on 9988  degrees of freedom
## AIC: 64530
## 
## Number of Fisher Scoring iterations: 5
```

### Visualize Coefficients

```r
coef <- c(coef(pois.resp))

coef_df <- data.frame(
  month = c(1:12),
  coefficient = coef
)

ggplot(data = coef_df, aes(x = month, y = coef)) +
  geom_line(color = "#619CFF", linewidth = 1) +
  geom_point(color = "black") +
  scale_x_continuous(n.breaks = 12) +
  labs(x = "Month",
       y = "Coefficient",
       title = "Coefficients by Month for Pois.Resp Model") +
  theme_light() +
  theme(plot.title = element_text(hjust=0.5))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-8-1.png" width="672" />

### Predict

I created a random small data frame of 3 different observations of month, and then used the predict() function to see what values for the response variable would be output. I also rounded it up to an interger as there can't be 0.6 admissions to the hospital.

```r
new_data <- data.frame(
  month = factor(c(1, 10, 4))
)

round(predict(pois.resp,
        new_data,
        type = "response"))
```

```
##  1  2  3 
##  3  1 12
```

## Conclusion

This was a short and sweet analysis on real life data that reinforced what I've been learning about poisson regression. I wanted to make my model multivariate but there was not much information in this dataset as it was only for one specific purpose. One thing that I could have done to make this more complex was to add more variables such as air quality information from the days that the data were collected, but I decided that that would involve more work than I wanted to put into this specific post.

I was thinking about putting general ED visits and Influenza like Illness ED visits as predictors in the model, but those are not really independent from the Influenza like Illness Admissions.

It would also be interesting to see this data in a "normal" year as COVID definitely had an impact on the data and which months were related to Influenza like Illness admissions.
