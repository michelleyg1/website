---
title: Doctor Visits Decision Trees
author: R package build
date: '2024-02-23'
slug: decision-tree-medicaid
categories: ["R", "Public Health"]
tags: []
description: Decision Tree Models to predict number of visits to ambulatory care for 1986 Medicaid Data
image: "images/tree.jpeg"
math: ~
license: ~
hidden: no
comments: no
---

I've been working through the chapter in [Intro to Statistical Learning with Applications in R](https://www.statlearning.com/) that discusses how to use decision trees to make predictions with both categorical and continuous response variables. I wanted to do a quick post working through my own models using the techniques that I learned in this chapter. For these models, I used the Data from the 1986 Medicaid Survey that can be found in the blapsr package.

## Load Libraries

```r
suppressPackageStartupMessages({
library(tidyverse)
library(blapsr)
library(tree)
library(randomForest)
library(gbm)
library(BART)
library(vtable)
})
```

## Load and Clean Data

```r
data(medicaid)
str(medicaid)
```

```
## 'data.frame':	485 obs. of  10 variables:
##  $ numvisits   : int  0 1 0 0 11 3 0 6 1 0 ...
##  $ exposure    : int  100 90 106 114 115 102 92 92 117 101 ...
##  $ children    : int  1 3 4 2 1 1 2 1 1 1 ...
##  $ age         : int  24 19 17 29 26 22 24 21 21 24 ...
##  $ income1000  : int  14500 6000 8377 6000 8500 6000 4000 6000 6000 6000 ...
##  $ access      : int  50 17 42 33 67 25 50 67 25 67 ...
##  $ pc1times1000: int  495 520 -1227 -1524 173 -905 -1202 656 -1227 -235 ...
##  $ maritalstat : int  0 0 0 0 0 0 0 1 0 1 ...
##  $ sex         : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ race        : int  1 1 1 1 1 0 1 1 1 1 ...
```
There are 10 different variables in this data frame, it looks like the factor variables maritalstat, sex, and race got read as integers rather than factors so I'm going to fix that below and also use case_when to show which dummy variable represents which level of the factor as they are not very intuitive. 


```r
medicaid <- medicaid %>% 
  mutate(race_f = as.factor(case_when(race == 0 ~ "Other",
                                    race == 1 ~ "White")),
         sex_f = as.factor(case_when(sex == 0 ~ "Male",
                                   sex == 1 ~ "Female")),
         maritalstat_f = as.factor(case_when(maritalstat == 0 ~ "Other",
                                           maritalstat == 1 ~ "Married"))
         ) %>% 
  dplyr::select(-maritalstat, -sex, -race)

medicaid_tibble <- as_tibble(medicaid)
medicaid_tibble
```

```
## # A tibble: 485 × 10
##    numvisits exposure children   age income1000 access pc1times1000 race_f sex_f
##        <int>    <int>    <int> <int>      <int>  <int>        <int> <fct>  <fct>
##  1         0      100        1    24      14500     50          495 White  Fema…
##  2         1       90        3    19       6000     17          520 White  Fema…
##  3         0      106        4    17       8377     42        -1227 White  Fema…
##  4         0      114        2    29       6000     33        -1524 White  Fema…
##  5        11      115        1    26       8500     67          173 White  Fema…
##  6         3      102        1    22       6000     25         -905 Other  Fema…
##  7         0       92        2    24       4000     50        -1202 White  Fema…
##  8         6       92        1    21       6000     67          656 White  Fema…
##  9         1      117        1    21       6000     25        -1227 White  Fema…
## 10         0      101        1    24       6000     67         -235 White  Fema…
## # ℹ 475 more rows
## # ℹ 1 more variable: maritalstat_f <fct>
```

## Summary Statistics


```r
sumtable(medicaid, out = "kable")
```

<table>
<caption><span id="tab:unnamed-chunk-4"></span>Table 1: Summary Statistics</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Variable </th>
   <th style="text-align:left;"> N </th>
   <th style="text-align:left;"> Mean </th>
   <th style="text-align:left;"> Std. Dev. </th>
   <th style="text-align:left;"> Min </th>
   <th style="text-align:left;"> Pctl. 25 </th>
   <th style="text-align:left;"> Pctl. 75 </th>
   <th style="text-align:left;"> Max </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> numvisits </td>
   <td style="text-align:left;"> 485 </td>
   <td style="text-align:left;"> 1.6 </td>
   <td style="text-align:left;"> 3.3 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 48 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> exposure </td>
   <td style="text-align:left;"> 485 </td>
   <td style="text-align:left;"> 104 </td>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 32 </td>
   <td style="text-align:left;"> 98 </td>
   <td style="text-align:left;"> 112 </td>
   <td style="text-align:left;"> 120 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> children </td>
   <td style="text-align:left;"> 485 </td>
   <td style="text-align:left;"> 2.3 </td>
   <td style="text-align:left;"> 1.3 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 9 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> age </td>
   <td style="text-align:left;"> 485 </td>
   <td style="text-align:left;"> 31 </td>
   <td style="text-align:left;"> 8.9 </td>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> 24 </td>
   <td style="text-align:left;"> 36 </td>
   <td style="text-align:left;"> 64 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> income1000 </td>
   <td style="text-align:left;"> 485 </td>
   <td style="text-align:left;"> 8098 </td>
   <td style="text-align:left;"> 3242 </td>
   <td style="text-align:left;"> 500 </td>
   <td style="text-align:left;"> 6000 </td>
   <td style="text-align:left;"> 8500 </td>
   <td style="text-align:left;"> 17500 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> access </td>
   <td style="text-align:left;"> 485 </td>
   <td style="text-align:left;"> 38 </td>
   <td style="text-align:left;"> 19 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 25 </td>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> 92 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> pc1times1000 </td>
   <td style="text-align:left;"> 485 </td>
   <td style="text-align:left;"> -0.041 </td>
   <td style="text-align:left;"> 1434 </td>
   <td style="text-align:left;"> -1524 </td>
   <td style="text-align:left;"> -1066 </td>
   <td style="text-align:left;"> 657 </td>
   <td style="text-align:left;"> 7217 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> race_f </td>
   <td style="text-align:left;"> 485 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ... Other </td>
   <td style="text-align:left;"> 159 </td>
   <td style="text-align:left;"> 33% </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ... White </td>
   <td style="text-align:left;"> 326 </td>
   <td style="text-align:left;"> 67% </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sex_f </td>
   <td style="text-align:left;"> 485 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ... Female </td>
   <td style="text-align:left;"> 469 </td>
   <td style="text-align:left;"> 97% </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ... Male </td>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> 3% </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> maritalstat_f </td>
   <td style="text-align:left;"> 485 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ... Married </td>
   <td style="text-align:left;"> 75 </td>
   <td style="text-align:left;"> 15% </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ... Other </td>
   <td style="text-align:left;"> 410 </td>
   <td style="text-align:left;"> 85% </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
</tbody>
</table>

## Regression Tree

This data did not require much cleaning, so I'm going to go straight into the model fitting. First, I'm going to separate training and test data so that I can put my models to be able to calculate an error rate and see if fitting different types of models improve my error rate.

```r
set.seed(123)
train <- sample(1:nrow(medicaid), nrow(medicaid) / 2)
test <- medicaid[-train, ]
y.test <- medicaid[-train, "numvisits"]
```

Now I'm going to fit my first model, just a simple regression tree using the tree() function in the tree library. 

```r
reg.tree <- tree(numvisits ~ .,
                 data = medicaid,
                 subset = train)
summary(reg.tree)
```

```
## 
## Regression tree:
## tree(formula = numvisits ~ ., data = medicaid, subset = train)
## Variables actually used in tree construction:
## [1] "access"       "pc1times1000" "age"         
## Number of terminal nodes:  4 
## Residual mean deviance:  12.85 = 3058 / 238 
## Distribution of residuals:
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## -10.8300  -1.3270  -0.9136   0.0000   0.6728  37.1700
```

Looks like a lot of variables were eliminated from the construction of the trees, I'm going to plot it.

```r
plot(reg.tree)
text(reg.tree, pretty = 0)
title("Regression Tree for Medicaid 1986 Data")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-7-1.png" width="672" />

### Prune Tree

This tree is already pretty minimal, but I'm going to go through the pruning process just to practice. First I'm going to plot the error as a function of size of the tree.

```r
cv.reg <- cv.tree(reg.tree)
plot(cv.reg$size, cv.reg$dev, type = "b")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-8-1.png" width="672" />

It looks like one is the best tree size for this particular dataset, but for the sake of being able to create a graph I'm going to set the best to two and see which variable was eliminated.

```r
prune.reg <- prune.tree(reg.tree, best = 2)
plot(prune.reg)
text(prune.reg, pretty = 0)
title("Pruned Regression Tree for Medicaid 1986 Data")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-9-1.png" width="672" />
So age was the one variable that was eliminated, leaving access and pc1times1000.

Now, I'm going to put this model to the test using mean square error for the test data.

```r
reg.pred <- predict(reg.tree, test)
prune.reg.pred <- predict(prune.reg, test)
# Unpruned Tree Test MSE
mean((reg.pred - y.test)^2)
```

```
## [1] 10.36599
```

```r
# Pruned Tree Test MSE
mean((prune.reg.pred - y.test)^2)
```

```
## [1] 10.20865
```
The pruning does improve the prediction but very slightly bringing the test MSE from 10.37 to 10.21.

## Bagging

The next model that I am going to fit is a bagging model, I'm going to set mtry to equal the number of predictors as that is the key difference between bagging and random forests.

```r
set.seed(123)
bag.tree <- randomForest(numvisits ~ .,
                         data = medicaid,
                         subset = train,
                         mtry = 9,
                         importance = TRUE)
```

Next, I'm going to make a set of predicted data and use it to calculate the test MSE.

```r
bag.pred <- predict(bag.tree, test)
mean((bag.pred - y.test)^2)
```

```
## [1] 9.255999
```
As expected, this yields a better result than just doing a regression tree with no bagging as it reduces variance, now I'm going to try a Random forest approach by altering the model slightly.

## Random Forest

Here, the only thing that was changed between Bagging and Random Forest is that mtry was set to sqrt(p) instead of p itself.

```r
set.seed(123)
rf.tree <- randomForest(numvisits ~ .,
                        data = medicaid,
                        subset = train,
                        mtry = 3,
                        importance = TRUE)
```

Now, to predict and calculate our test MSE.

```r
rf.pred <- predict(rf.tree, test)
mean((rf.pred - y.test)^2)
```

```
## [1] 7.044376
```
Nice! This brings the test MSE down to 7.14, a pretty big improvement from my first model.

### Importance Plot
I'm going to use a built in plot from the randomForest library to show the importance of the various predictors to take a look at which is the strongest variable in the model.

```r
varImpPlot(rf.tree)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-15-1.png" width="672" />

## Boosting

Next, I'm going to fit another model using decision trees, but this time I am going to use the boosting method that uses the gbm() function from the gbm library to fit the model.
Also, when a summary of this model is printed it prints a plot of relative influence of the predictors.

```r
set.seed(123)
boost.tree <- gbm(numvisits ~.,
                  data = medicaid,
                  distribution = "gaussian")
summary(boost.tree)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-16-1.png" width="672" />

```
##                         var   rel.inf
## pc1times1000   pc1times1000 67.629823
## access               access 15.745251
## exposure           exposure  5.430522
## children           children  4.713071
## income1000       income1000  3.591793
## age                     age  2.889541
## race_f               race_f  0.000000
## sex_f                 sex_f  0.000000
## maritalstat_f maritalstat_f  0.000000
```

This package also gives a built in plot of partial dependence. Here I integrate out all other variables and just take a look at the pc1times1000 variable. This variable represents the first principal component of three health status variables (functional limitations, acute conditions, and chronic conditions).

```r
plot(boost.tree, i = "pc1times1000")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-17-1.png" width="672" />

And to check the test MSE and see how the model compares to the rest.

```r
boost.pred <- predict(boost.tree, test)
```

```
## Using 100 trees...
```

```r
mean((boost.pred - y.test)^2)
```

```
## [1] 5.589588
```

### Changing Number of Trees

I want to see how the model is affected when I set n.trees equal to a thousand instead of the default of one hundred trees. Below, I added the n.trees specification to the model and calculated the MSE.

```r
set.seed(123)
boost.tree.2 <- gbm(numvisits ~.,
                  data = medicaid,
                  distribution = "gaussian",
                  n.trees = 1000)
boost.pred.2 <- predict(boost.tree.2, test, n.trees = 1000)
mean((boost.pred.2 - y.test)^2)
```

```
## [1] 4.80569
```
This is the best so far! More than half of the error of the first model that I fit.

## Bayesian Additive Regression Tree

Here I create the components of the model from my test and training data in order to use the gbart() function in the BART package.

```r
x <- medicaid[, 2:10]
y <- medicaid[, "numvisits"]
x.train <- x[train, ]
y.train <- y[train]
x.test <- x[-train, ]
y.test <- y[-train]
```

And again, I fit the model, create some predictions off of the test data, and then calculate the test MSE, lets see if this final method improves the MSE further.

```r
set.seed(123)
bart.tree <- gbart(x.train, y.train, x.test = x.test)
```

```
## *****Calling gbart: type=1
## *****Data:
## data:n,p,np: 242, 12, 243
## y1,yn: -0.595041, -1.595041
## x1,x[n*p]: 95.000000, 0.000000
## xp1,xp[np*p]: 100.000000, 1.000000
## *****Number of Trees: 200
## *****Number of Cut Points: 32 ... 1
## *****burn,nd,thin: 100,1000,1
## *****Prior:beta,alpha,tau,nu,lambda,offset: 2,0.95,0.848528,3,2.87615,1.59504
## *****sigma: 3.842563
## *****w (weights): 1.000000 ... 1.000000
## *****Dirichlet:sparse,theta,omega,a,b,rho,augment: 0,0,1,0.5,1,12,0
## *****printevery: 100
## 
## MCMC
## done 0 (out of 1100)
## done 100 (out of 1100)
## done 200 (out of 1100)
## done 300 (out of 1100)
## done 400 (out of 1100)
## done 500 (out of 1100)
## done 600 (out of 1100)
## done 700 (out of 1100)
## done 800 (out of 1100)
## done 900 (out of 1100)
## done 1000 (out of 1100)
## time: 6s
## trcnt,tecnt: 1000,1000
```

```r
yhat <- bart.tree$yhat.test.mean
mean((y.test - yhat)^2)
```

```
## [1] 7.379971
```
7.41, this is a step back in terms of accurasy on test data from the previous model.

## Conclusion

In this case boosting with 1000 trees gives the best mean square error of 4.8 which means the model is off generally by 2.2 visits when used on data outside of the training set. 

Decision trees are definitely a departure to what I've been learning the last couple of weeks, which has mostly been regression methods of predicting, but it is good to have a whole arsenal of options when trying to do statistical learning. 
