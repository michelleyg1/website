---
title: Air Quality Cross Validation
author: R package build
date: '2024-02-20'
slug: air-quality-cross-validation
categories: ["R", "Public Health", "Environment"]
tags: []
description: Practicing cross validation and new regression techniques on R's built in air quality dataset
image: "images/cross.jpeg"
math: ~
license: Michelle Gulotta
hidden: no
comments: no
---

For this analysis I'm going to revisit my [air quality analysis](https://www.mgulotta.com/p/airquality/) model in order to apply some new techniques that I've been learning. The main technique I wanted to practice was using cross validation and test mean square error in order to determine which model to select. I also learned about differnet types of model selection, such as subset selection methods, ridge regression, the lasso method, and dimension reduction methods.

## Load Libraries

```r
suppressPackageStartupMessages({
  library(tidyverse)
  library(leaps)
  library(glmnet)
})
```

## Load Data

```r
data("airquality")
```

## Clean Data
### Mean Imputation

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

names(airquality_mi)[1:6] <- tolower(names(airquality_mi)[1:6])
```

### Final Touches

```r
airquality_mi <- airquality_mi %>%
  dplyr::select(month, day, wind, temp, ozone_mi, solar_mi)

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

Now that I have my data to where it was when I began my model fitting on the last analysis, I'm going to play around with some techniques that I learned. As this model does not have many different variables, I'm going to go through best subset selection and using adjustments in order to select a model pretty quickly. 

I did learn about forward and backward subset selection, but I'm going to save those for a more high dimensional data analysis in the future.

## Best Subset Selection

First I'm going to fit the model using the regsubsets() function from the leaps library.

```r
best.subset <- regsubsets(ozone_mi ~ temp + wind + solar_mi, 
                          data = airquality_mi,
                          nvmax = 3)
```

And then use built in plots to visualize the R squared, adjusted R2, Cp, and BIC to see which variables make most sense to include in the model.

```r
plot(best.subset, scale = "r2")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-6-1.png" width="672" />

```r
plot(best.subset, scale = "adjr2")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-6-2.png" width="672" />

```r
plot(best.subset, scale = "Cp")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-6-3.png" width="672" />

```r
plot(best.subset, scale = "bic")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-6-4.png" width="672" />

## Test MSE of Original Model

Originally, I used R squared to test the fit of the model, however as I now know that only shows training error and tells us very little about how the model would work on data that it was not already fit on.

Here I'm going to use test and training data to compute the test MSE so that I can see if these new methods that I learned improve the model that I used in my other blog post.

```r
set.seed(123)
train <- sample(c(TRUE, FALSE),
                nrow(airquality_mi),
                replace = TRUE)
test <- (!train)
ozone.test <- airquality_mi$ozone_mi[test]

aq.mv <- lm(ozone_mi ~ temp + I(temp^2) + wind + I(wind^2) + solar_mi, 
                  data = airquality_mi,
                  subset = train)
aq.mv.pred <- predict(aq.mv,
                      airquality_mi[test, ])
mean((aq.mv.pred - ozone.test)^2)
```

```
## [1] 528.7101
```
So 528.7 is the number to beat for my next models.
Also, to fit on the whole dataset and check out the coefficients.

```r
aq.mv.whole <- lm(ozone_mi ~ temp + I(temp^2) + wind + I(wind^2) + solar_mi, 
                  data = airquality_mi)
summary(aq.mv.whole)
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

## Shrinkage Methods
### Ridge Regression

I'm going to use the glmnet package in order to fit these models, however I learned that the glmnet functions do not use normal model fitting syntax, but rather you have to create a model matrix first and then use the glmnet() formula.

```r
x <- model.matrix(ozone_mi ~ temp + wind + solar_mi, 
                  airquality_mi)[, -1]
y <- airquality_mi$ozone_mi
grid <- 10^seq(10, -2, length = 100)
ridge <- glmnet(x, y, alpha = 0, lambda = grid)
```

I'm going to use a test and training dataset to perform cross validation.

```r
set.seed(123)
train.mat <- sample(1:nrow(x), nrow(x) / 2)
test.mat <- (-train.mat)
y.test <- y[test.mat]
```

Next, I'm going to fit the model with the traiing data.

```r
ridge.train <- glmnet(x[train.mat, ],
                      y[train.mat],
                      alpha = 0,
                      lambda = grid,
                      thresh = 1e-12)
```

Here I use the cv.glmnet() function in order to select the best value for the tuning parameter lambda.

```r
cv.out <- cv.glmnet(x[train.mat, ],
                y[train.mat],
                alpha = 0)
lambda.ridge <- cv.out$lambda.min
```

Lastly, I go through the process of computing a set of predicted values and computing the test Mean Squared Error in order to assess how this model compares to the original.

```r
ridge.pred <- predict(ridge.train,
                      s = lambda.ridge,
                      newx = x[test.mat, ])
mean((ridge.pred - y.test)^2)
```

```
## [1] 363.6543
```
Alright, that's better than the original. Now to fit on the whole dataset and check out the predicted coefficients. 

```r
ridge.whole <- glmnet(x, y, alpha = 0)
predict(ridge.whole,
        type = "coefficients",
        s = lambda.ridge)
```

```
## 4 x 1 sparse Matrix of class "dgCMatrix"
##                       s1
## (Intercept) -29.47556219
## temp          1.10854966
## wind         -2.47425857
## solar_mi      0.05312106
```


### The Lasso
Yee haw lol. Okay, I'm going to repeat the proccess again, just using the alpha = 1 instead of alpha = 0.

```r
lasso.train <- glmnet(x[train.mat, ],
                y[train.mat],
                alpha = 1,
                lambda = grid)
cv.out <- cv.glmnet(x[train.mat, ],
                    y[train.mat],
                    alpha = 1)
lambda.lasso <- cv.out$lambda.min
```

And to compute the test MSE again.

```r
lasso.pred <- predict(lasso.train,
                      s = lambda.lasso,
                      newx = x[test.mat, ])
mean((lasso.pred - y.test)^2)
```

```
## [1] 364.4416
```

Not much different than the ridge, slightly worse actually, but that is to be expected really since there are not many variables.
Now to check out the predicted coefficients when I fit the model on the whole dataset

```r
lasso.whole <- glmnet(x, y, alpha = 1)
predict(lasso.whole,
        type = "coefficients",
        s = lambda.lasso)
```

```
## 4 x 1 sparse Matrix of class "dgCMatrix"
##                      s1
## (Intercept) -37.9143506
## temp          1.2367359
## wind         -2.6980120
## solar_mi      0.0567973
```
They're also pretty similar to each other in this case.

## Conclusion
This analysis helped me get a better grip on how to compute and use test mean squared error in order to use cross validation to put my model to the test. Originally, I did not know that there were so many different ways to do a regression but this textbook, [Intro to Statistical Learning with Applications in R](https://www.statlearning.com/) has helped me see beyond the basics and actually know whats going on within these statistical models instead of just taking shots in the dark. 

I also learned new and clever methods of seperating test and training data.

I'm definintely still a beginner but I'm already using my new skills to improve my prior analyses and learn from my mistakes.
