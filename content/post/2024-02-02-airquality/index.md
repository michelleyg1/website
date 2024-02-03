---
title: Air Quality Regression
author: R package build
date: '2024-02-02'
slug: airquality
categories: ["R", "Public Health", "Environment"]
tags: []
description: An analysis of R's built in airquality dataset
image: "images/smoke.jpeg"
math: ~
license: Michelle Gulotta
hidden: no
comments: no
---

Currently, I'm working through this great book [An Introduction to Statistical Learning with Applications in R](https://www.statlearning.com/), and I just finished the chapter on linear regression. One thing that I really like about this book is that there is a great balance of learning R and just learning statistics in a mathematical way which really shows the backbone of the R functions that I was just using without really knowing how they work.

For this analysis I used the built in dataset "airquality" in R which stores 153 observations of different air quality measures averaged out by day taken in New York City from May to September 1973. I used this to practice my skills in linear regression, both simple and multivariate.

## Load Libraries


```r
suppressPackageStartupMessages({
library(tidyverse)
library(broom)
library(car)
library(ggthemes)
  })
```

## Load Data

```r
data("airquality")
```

## Explore and Clean Data

```r
str(airquality)
```

```
## 'data.frame':	153 obs. of  6 variables:
##  $ Ozone  : int  41 36 12 18 NA 28 23 19 8 NA ...
##  $ Solar.R: int  190 118 149 313 NA NA 299 99 19 194 ...
##  $ Wind   : num  7.4 8 12.6 11.5 14.3 14.9 8.6 13.8 20.1 8.6 ...
##  $ Temp   : int  67 72 74 62 56 66 65 59 61 69 ...
##  $ Month  : int  5 5 5 5 5 5 5 5 5 5 ...
##  $ Day    : int  1 2 3 4 5 6 7 8 9 10 ...
```

### Summary Statistics
Creating a table of summary statistics that also doubles as a way to check for missing values, as since this dataset only has 153 observations so substantial amounts of missing data will affect the outcomes.

```r
airquality %>% 
  dplyr::select(Ozone, Solar.R, Wind, Temp) %>% 
  map_df(.f = ~broom::tidy(summary(.x)),
         .id = "variable")
```

```
## # A tibble: 4 × 8
##   variable minimum    q1 median   mean    q3 maximum    na
##   <chr>      <dbl> <dbl>  <dbl>  <dbl> <dbl>   <dbl> <dbl>
## 1 Ozone        1    18     31.5  42.1   63.2   168      37
## 2 Solar.R      7   116.   205   186.   259.    334       7
## 3 Wind         1.7   7.4    9.7   9.96  11.5    20.7    NA
## 4 Temp        56    72     79    77.9   85      97      NA
```
So it looks like there are a decent amount of missing values for the ozone variable, I'm going to use mean imputation to deal with this.

### Mean Imputation for Missing Values

```r
airquality_mi <- airquality %>% 
  mutate(
    ozone_mi = as.integer(if_else(is.na(Ozone), 
                            mean(Ozone, na.rm = TRUE), 
                            Ozone)),
    solar_mi = as.integer(if_else(is.na(Solar.R), 
                            mean(Solar.R, na.rm = TRUE), 
                            Solar.R))
  ) %>% 
  select(-1, -2)
```

### Clean Up and Print

```r
names(airquality_mi)[1:6] <- tolower(names(airquality_mi)[1:6])

airquality_mi <- airquality_mi %>%
  select(month, day, wind, temp, ozone_mi, solar_mi)

as_tibble(airquality_mi)
```

```
## # A tibble: 153 × 6
##    month   day  wind  temp ozone_mi solar_mi
##    <int> <int> <dbl> <int>    <int>    <int>
##  1     5     1   7.4    67       41      190
##  2     5     2   8      72       36      118
##  3     5     3  12.6    74       12      149
##  4     5     4  11.5    62       18      313
##  5     5     5  14.3    56       42      185
##  6     5     6  14.9    66       28      185
##  7     5     7   8.6    65       23      299
##  8     5     8  13.8    59       19       99
##  9     5     9  20.1    61        8       19
## 10     5    10   8.6    69       42      194
## # ℹ 143 more rows
```

## Simple Regression
### Visualize Variables of Interest

```r
ggplot(data = airquality_mi, aes(x = temp, y = ozone_mi)) +
  geom_jitter() +
  labs(
    x = "Temperature Measured in Fahrenheit",
    y = "Mean Ozone per Day in Parts Per Billion",
    title = "Ozone Concentration vs. Temperature in New York City
    from May to September 1973"
  ) +
  theme_stata() +
  theme(plot.title = element_text(hjust = 0.5))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-7-1.png" width="672" />

### Fit Model

```r
aq.simple <- lm(ozone_mi ~ temp, data = airquality_mi)
summary(aq.simple)
```

```
## 
## Call:
## lm(formula = ozone_mi ~ temp, data = airquality_mi)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -39.006 -15.851  -2.160   8.469 120.149 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -101.6222    15.3547  -6.618 5.96e-10 ***
## temp           1.8454     0.1957   9.428  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 22.84 on 151 degrees of freedom
## Multiple R-squared:  0.3706,	Adjusted R-squared:  0.3664 
## F-statistic:  88.9 on 1 and 151 DF,  p-value: < 2.2e-16
```

Looking at the summary of this model its already pretty evident that there are some issues, the residual standard error is pretty high and the R squared shows that a majority of variation cannot be explained by the model. I'm also going to run some diagnostic plots to see if there are other issues.

### Diagnostics

```r
par(mfrow = c(2, 2))
plot(aq.simple)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-9-1.png" width="672" />
These plots suggest that there may be a non linear relationship between the variables, so I'm going to apply a transformation and see if that helps.

### Polynomial Transformation

```r
aq.simple.trans <- lm(ozone_mi ~ temp + I(temp^2), data = airquality_mi)
summary(aq.simple.trans)
```

```
## 
## Call:
## lm(formula = ozone_mi ~ temp + I(temp^2), data = airquality_mi)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -41.463 -13.131  -2.735   9.869 124.916 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 296.43794   94.87393   3.125 0.002138 ** 
## temp         -8.78275    2.50999  -3.499 0.000615 ***
## I(temp^2)     0.06981    0.01644   4.246  3.8e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 21.65 on 150 degrees of freedom
## Multiple R-squared:  0.4381,	Adjusted R-squared:  0.4306 
## F-statistic: 58.47 on 2 and 150 DF,  p-value: < 2.2e-16
```
So that's a bit better in terms of R squared, I'm going to re-check the diagnostic plots.

```r
par(mfrow = c(2, 2))
plot(aq.simple.trans)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-11-1.png" width="672" />

This is not the best model for the data, but being able to recognize that is also important. I'm going to visualize the regression line on my previous plot for the sake of practice.

### Visualize with Least Squares Regression Line

```r
ggplot(data = airquality_mi, aes(x = temp, y = ozone_mi)) +
  geom_jitter() +
  stat_smooth(method = "lm",
              formula = y ~ poly(x, 2, raw = TRUE)) +
  labs(
    x = "Temperature Measured in Fahrenheit",
    y = "Mean Ozone per Day in Parts Per Billion",
    title = "Ozone Concentration vs. Temperature in New York City
    from May to September 1973"
  ) +
  theme_stata() +
  theme(plot.title = element_text(hjust = 0.5))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-12-1.png" width="672" />

## Multivariate Regression
I also want to do a multivariate regression with this data to see if I can come up with a better model and be able to practice making predictions using a regression model.

### Fit Model

```r
aq.mv <- lm(ozone_mi ~ temp + wind + solar_mi, data = airquality_mi)
summary(aq.mv)
```

```
## 
## Call:
## lm(formula = ozone_mi ~ temp + wind + solar_mi, data = airquality_mi)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -38.541 -14.590  -5.148  12.172 101.198 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -38.20875   18.88354  -2.023  0.04482 *  
## temp          1.24096    0.20907   5.935 1.97e-08 ***
## wind         -2.71862    0.54280  -5.009 1.53e-06 ***
## solar_mi      0.05772    0.02003   2.881  0.00454 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 20.9 on 149 degrees of freedom
## Multiple R-squared:   0.48,	Adjusted R-squared:  0.4696 
## F-statistic: 45.85 on 3 and 149 DF,  p-value: < 2.2e-16
```

### Diagnostics
The first thing I want to check since I have multiple predictor variables in my new model is colinearity. I'm going to use the vif function to calculate the variance inflation factor to see if there are any variables that are correlated too strongly. 

```r
vif(aq.mv)
```

```
##     temp     wind solar_mi 
## 1.363082 1.272795 1.080454
```

Looks good! Moving on to normal diagnostic plots

```r
par(mfrow = c(2, 2))
plot(aq.mv)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-15-1.png" width="672" />
This model could use some work to get it to fit the data and assumptions a bit better, I'm going to see if doing some transformations of the predictor helps.

### Transformation

First I'm going to visualize my individual predictor variables with the response variables just to see what I'm working with and which transformations might help me.

I already visualized ozone and temperature in the simple regression, so I'm going to visualize ozone and wind.

```r
ggplot(data = airquality_mi, aes(x = wind, y = ozone_mi)) +
  geom_jitter() +
  labs(
    x = "Average Wind Speed Measured in Miles Per Hour",
    y = "Mean Ozone per Day in Parts Per Billion",
    title = "Ozone Concentration vs. Wind in New York City
    from May to September 1973"
  ) +
  theme_stata() +
  theme(plot.title = element_text(hjust = 0.5))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-16-1.png" width="672" />

And ozone and solar.

```r
ggplot(data = airquality_mi, aes(x = solar_mi, y = ozone_mi)) +
  geom_jitter() +
  labs(
    x = "Solar Radiation Measured in Langleys",
    y = "Mean Ozone per Day in Parts Per Billion",
    title = "Ozone Concentration vs. Solar Radiation in New York City
    from May to September 1973"
  ) +
  theme_stata() +
  theme(plot.title = element_text(hjust = 0.5))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-17-1.png" width="672" />

I'm going to apply polynomial transformations to two out of the three predictor variables.

```r
aq.mv.trans <- lm(ozone_mi ~ temp + I(temp^2) + wind + I(wind^2) + solar_mi, data = airquality_mi)
summary(aq.mv.trans)
```

```
## 
## Call:
## lm(formula = ozone_mi ~ temp + I(temp^2) + wind + I(wind^2) + 
##     solar_mi, data = airquality_mi)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -36.293 -10.929  -3.289   8.684  89.513 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 321.25924   81.78038   3.928 0.000131 ***
## temp         -7.28999    2.18385  -3.338 0.001069 ** 
## I(temp^2)     0.05580    0.01431   3.898 0.000147 ***
## wind        -11.10077    1.94439  -5.709 6.07e-08 ***
## I(wind^2)     0.39616    0.08865   4.469 1.56e-05 ***
## solar_mi      0.06206    0.01772   3.503 0.000610 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 18.46 on 147 degrees of freedom
## Multiple R-squared:  0.5998,	Adjusted R-squared:  0.5862 
## F-statistic: 44.07 on 5 and 147 DF,  p-value: < 2.2e-16
```

This R2 and RSE is definitely better than the simple regression model, as now almost 60% of the variation in the data can be explained by the model rather than being closer to 40%, still not the best but definitely better. Now I'm going to re-check the diagnostics to see if this satisfies the assumptions a bit better.

```r
par(mfrow = c(2, 2))
plot(aq.mv.trans)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-19-1.png" width="672" />
This definitely helped deal with the pattern in the residuals vs. fitted plot and helps satisfy the linearity assumption of the model.

### Make Predictions
I'm going to use this model to make a couple predictions about ozone levels when given information about temperature, solar radiation, and wind speeds, along with confidence intervals.

```r
new_data_point <- data.frame(
  solar_mi = c(250, 120, 320),
  temp = c(63, 74, 82),
  wind = c(10.2, 9.5, 7.0)
)
 
predict(
  aq.mv.trans,
  new_data_point,
  interval = "confidence"
)
```

```
##        fit      lwr      upr
## 1 26.96636 19.31629 34.61643
## 2 25.10863 20.28017 29.93708
## 3 60.25022 54.08080 66.41964
```
## Conclusion
I've tried to perform linear regressions before, as it is very simple to do in R but I really didn't understand any of the outputs of the model or what constitutes as a good model, but the Linear Regression chapter of Intro to Statistical Learning along with running this analysis helped me understand those concepts better. 

I also, until then, didn't really understand the importance of linear regression and thought that it was just similar to correlation, but we can actually use linear regression in order to input new data to get a prediction for the response variable, in this case the amount of ozone in the atmosphere.

In working on this analysis I also learned about mean imputation when dealing with missing values, which has its advantages and disadvantages compared to just removing rows with missing values. 
