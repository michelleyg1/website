---
title: Alcohol Consumption and Bone Health
author: R package build
date: '2024-03-04'
slug: alcohol-bone-health
categories: ["R", "Public Health"]
tags: []
description: Is alcohol consumption good for your bones? An analysis of NHANES data from 2011 to 2016
image: "images/bone.jpeg"
math: ~
license: Michelle Gulotta
hidden: no
comments: no
---

For this analysis I wanted to explore the connection between bone health and alcohol consumption. From my non-medical professional prospective, I assumed that there would be decreased bone health measures as alcohol consumption increased, but [some papers](https://www.liebertpub.com/doi/abs/10.1089/jwh.1999.8.65) actually [show the opposite](https://link.springer.com/article/10.1007/s00198-006-0249-0) effect. I decided to see if the NHANES 2011 to 2016 data reflects this patter as well, and also to practice analyzing complex surveys and explore more that the survey package has to offer.
 
## Load Packages

```r
suppressPackageStartupMessages({
  library(tidyverse)
  library(nhanesA)
  library(survey)
  library(janitor)
  library(tableone)
})
```
 
 
## Load Data
 
I combined three cycles of NHANES data from the years 2011 to 2016, in doing this I saw in the documentation that new sample weights would have to be created to accommodate for this. Also, in reading the documentation I saw that the questionnaire that included the questions about alcohol consumption were administered in the mobile examination center, rather than in the in home interview questionnaire and therefore the exam weights would have to be used. 

I performed this calculation when I was manipulating the other columns using dplyr's mutate function, and for now I'm starting off with importing the relevant data using the nhanesA package.

```r
# Demographic Data
demo11 <- nhanes("DEMO_G") %>% 
  dplyr::select(SEQN, RIAGENDR, RIDAGEYR, RIDEXPRG, RIDRETH3, SDMVPSU, SDMVSTRA, WTMEC2YR)
demo13 <- nhanes("DEMO_H") %>% 
  dplyr::select(SEQN, RIAGENDR, RIDAGEYR, RIDEXPRG, RIDRETH3, SDMVPSU, SDMVSTRA, WTMEC2YR)
demo15 <- nhanes("DEMO_I") %>% 
  dplyr::select(SEQN, RIAGENDR, RIDAGEYR, RIDEXPRG, RIDRETH3, SDMVPSU, SDMVSTRA, WTMEC2YR)
# Alcohol Consumption Data
alc11 <- nhanes("ALQ_G") %>% 
  dplyr::select(SEQN, ALQ101, ALQ110, ALQ120Q, ALQ120U, ALQ130)
alc13 <- nhanes("ALQ_H") %>% 
  dplyr::select(SEQN, ALQ101, ALQ110, ALQ120Q, ALQ120U, ALQ130)
alc15 <- nhanes("ALQ_I") %>% 
  dplyr::select(SEQN, ALQ101, ALQ110, ALQ120Q, ALQ120U, ALQ130)
# Bone Density Data
bone11 <- nhanes("DXX_G") %>% 
  dplyr::select(SEQN, DXDTOBMC, DXDTOBMD)
bone13 <- nhanes("DXX_H") %>% 
  dplyr::select(SEQN, DXDTOBMC, DXDTOBMD)
bone15 <- nhanes("DXX_I") %>% 
  dplyr::select(SEQN, DXDTOBMC, DXDTOBMD)
```
 
## Merge Data

```r
df11 <- merge(demo11, alc11, by = c("SEQN"), all = TRUE) %>% merge(bone11, by = c("SEQN"), all = TRUE)
df13 <- merge(demo13, alc13, by = c("SEQN"), all = TRUE) %>% merge(bone13, by = c("SEQN"), all = TRUE)
df15 <- merge(demo15, alc15, by = c("SEQN"), all = TRUE) %>% merge(bone15, by = c("SEQN"), all = TRUE)

df <- rbind(df11, df13, df15)

df$SEQN <- NULL
```
 
## Data Cleaning

### Assess Variables

First I wanted to know the values of my selected variables so that I could make more concise categories and also assess for missing values and how they are coded. 

```r
df %>% tabyl(RIDRETH3)
```

```
##                             RIDRETH3    n    percent
##                     Mexican American 5006 0.16741355
##                       Other Hispanic 3344 0.11183198
##                   Non-Hispanic White 9713 0.32482777
##                   Non-Hispanic Black 7079 0.23674002
##                   Non-Hispanic Asian 3398 0.11363788
##  Other Race - Including Multi-Racial 1362 0.04554879
```

```r
df %>% tabyl(RIDEXPRG)
```

```
##                                                            RIDEXPRG     n
##  Yes, positive lab pregnancy test or self-reported pregnant at exam   192
##                            The participant was not pregnant at exam  3341
##             Cannot ascertain if the participant is pregnant at exam   272
##                                                                <NA> 26097
##      percent valid_percent
##  0.006420975    0.05045992
##  0.111731657    0.87805519
##  0.009096382    0.07148489
##  0.872750987            NA
```

```r
df %>% tabyl(ALQ101)
```

```
##      ALQ101     n      percent valid_percent
##         Yes 10844 0.3626513277  0.6948609509
##          No  4748 0.1587853655  0.3042419582
##  Don't know    14 0.0004681961  0.0008970909
##        <NA> 14296 0.4780951107            NA
```

```r
df %>% tabyl(ALQ110)
```

```
##      ALQ110     n      percent valid_percent
##         Yes  2047 6.845696e-02  0.4298614028
##          No  2700 9.029496e-02  0.5669886602
##  Don't know    14 4.681961e-04  0.0029399412
##     Refused     1 3.344258e-05  0.0002099958
##        <NA> 25140 8.407464e-01            NA
```

```r
df %>% tabyl(ALQ120Q)
```

```
##  ALQ120Q     n      percent valid_percent
##        0  2581 8.631530e-02  0.2002172058
##        1  3001 1.003612e-01  0.2327980762
##        2  2527 8.450940e-02  0.1960282368
##        3  1508 5.043141e-02  0.1169808393
##        4   855 2.859341e-02  0.0663253433
##        5   753 2.518226e-02  0.0584128462
##        6   427 1.427998e-02  0.0331238849
##        7   606 2.026620e-02  0.0470095415
##        8    77 2.575079e-03  0.0059731596
##        9     9 3.009832e-04  0.0006981615
##       10   192 6.420975e-03  0.0148941122
##       11     3 1.003277e-04  0.0002327205
##       12    69 2.307538e-03  0.0053525716
##       13     2 6.688516e-05  0.0001551470
##       14    13 4.347535e-04  0.0010084555
##       15    63 2.106882e-03  0.0048871306
##       16     4 1.337703e-04  0.0003102940
##       17     1 3.344258e-05  0.0000775735
##       18     3 1.003277e-04  0.0002327205
##       19     1 3.344258e-05  0.0000775735
##       20    53 1.772457e-03  0.0041113955
##       21     1 3.344258e-05  0.0000775735
##       22     1 3.344258e-05  0.0000775735
##       24     8 2.675406e-04  0.0006205880
##       25     8 2.675406e-04  0.0006205880
##       28     3 1.003277e-04  0.0002327205
##       29     1 3.344258e-05  0.0000775735
##       30    35 1.170490e-03  0.0027150725
##       35     2 6.688516e-05  0.0001551470
##       36     1 3.344258e-05  0.0000775735
##       40     3 1.003277e-04  0.0002327205
##       42     1 3.344258e-05  0.0000775735
##       45     2 6.688516e-05  0.0001551470
##       48     4 1.337703e-04  0.0003102940
##       50     2 6.688516e-05  0.0001551470
##       52     1 3.344258e-05  0.0000775735
##       60     6 2.006555e-04  0.0004654410
##       61     1 3.344258e-05  0.0000775735
##       64     1 3.344258e-05  0.0000775735
##       72     1 3.344258e-05  0.0000775735
##       80     2 6.688516e-05  0.0001551470
##       90     4 1.337703e-04  0.0003102940
##       96     1 3.344258e-05  0.0000775735
##       97     1 3.344258e-05  0.0000775735
##      100     5 1.672129e-04  0.0003878675
##      108     1 3.344258e-05  0.0000775735
##      112     1 3.344258e-05  0.0000775735
##      120     1 3.344258e-05  0.0000775735
##      138     1 3.344258e-05  0.0000775735
##      140     1 3.344258e-05  0.0000775735
##      150     2 6.688516e-05  0.0001551470
##      160     1 3.344258e-05  0.0000775735
##      168     1 3.344258e-05  0.0000775735
##      178     1 3.344258e-05  0.0000775735
##      180     2 6.688516e-05  0.0001551470
##      182     1 3.344258e-05  0.0000775735
##      200     4 1.337703e-04  0.0003102940
##      220     1 3.344258e-05  0.0000775735
##      245     1 3.344258e-05  0.0000775735
##      250     1 3.344258e-05  0.0000775735
##      300     1 3.344258e-05  0.0000775735
##      305     1 3.344258e-05  0.0000775735
##      340     1 3.344258e-05  0.0000775735
##      350     3 1.003277e-04  0.0002327205
##      365     8 2.675406e-04  0.0006205880
##      777     2 6.688516e-05  0.0001551470
##      999    12 4.013109e-04  0.0009308820
##       NA 17011 5.688917e-01            NA
```

```r
df %>% tabyl(ALQ120U)
```

```
##  ALQ120U     n    percent valid_percent
##     Week  4140 0.13845228     0.4020979
##    Month  2880 0.09631463     0.2797203
##     Year  3276 0.10955789     0.3181818
##     <NA> 19606 0.65567521            NA
```

```r
df %>% tabyl(ALQ130)
```

```
##  ALQ130     n      percent valid_percent
##       1  3601 1.204267e-01  3.493064e-01
##       2  2858 9.557889e-02  2.772335e-01
##       3  1498 5.009698e-02  1.453099e-01
##       4   777 2.598488e-02  7.537104e-02
##       5   467 1.561768e-02  4.530022e-02
##       6   525 1.755735e-02  5.092638e-02
##       7    89 2.976390e-03  8.633233e-03
##       8   139 4.648518e-03  1.348336e-02
##       9    20 6.688516e-04  1.940052e-03
##      10   103 3.444586e-03  9.991270e-03
##      11    10 3.344258e-04  9.700262e-04
##      12   128 4.280650e-03  1.241634e-02
##      13     7 2.340981e-04  6.790183e-04
##      14     9 3.009832e-04  8.730236e-04
##      15    33 1.103605e-03  3.201086e-03
##      16     7 2.340981e-04  6.790183e-04
##      17     1 3.344258e-05  9.700262e-05
##      18     5 1.672129e-04  4.850131e-04
##      20     8 2.675406e-04  7.760210e-04
##      21     2 6.688516e-05  1.940052e-04
##      23     1 3.344258e-05  9.700262e-05
##      24     4 1.337703e-04  3.880105e-04
##      25     1 3.344258e-05  9.700262e-05
##      30     2 6.688516e-05  1.940052e-04
##      64     1 3.344258e-05  9.700262e-05
##      82     1 3.344258e-05  9.700262e-05
##     777     2 6.688516e-05  1.940052e-04
##     999    10 3.344258e-04  9.700262e-04
##      NA 19593 6.552405e-01            NA
```

This was a useful part of data cleaning, as I was able to see that certain missing values were coded as 999 or 777 rather than NA, this would've seriously thrown off my calculations when I tried to create the variable to capture average drinks over last 12 months time period.

### Creating New Variables


```r
dff <- df %>% 
  mutate(weight = 1/3*df$WTMEC2YR,
         preg = as.factor(case_when(
                          RIDEXPRG == "Yes, positive lab pregnancy test or self-reported pregnant at exam" ~ "Yes",
                          RIDEXPRG == "The participant was not pregnant at exam" ~ "No",
                          TRUE ~ NA)),
         race = as.factor(case_when(
                          RIDRETH3 == "Mexican American" | RIDRETH3 == "Other Hispanic" ~ "Hispanic",
                          RIDRETH3 == "Non-Hispanic White" ~ "White",
                          RIDRETH3 == "Non-Hispanic Black" ~ "Black",
                          RIDRETH3 == "Non-Hispanic Asian" ~ "Asian",
                          RIDRETH3 == "Other Race - Including Multi-Racial" ~ "Other")),
         drinksyr = if_else(ALQ101 == "Don't know", NA, ALQ101),
         drinkslife = if_else(ALQ110 == "Don't know" | ALQ110 == "Refused", NA, ALQ110),
         drinksfreq = if_else(ALQ120Q == 777 | ALQ120Q == 999, NA, ALQ120Q),
         drinksamt = if_else(ALQ130 == 777 | ALQ130 == 999, NA, ALQ130),
         drinksfreqcalc = case_when(
                          ALQ120U == "Week" ~ drinksfreq * 52,
                          ALQ120U == "Month" ~ drinksfreq * 12,
                          ALQ120U == "Year" ~ drinksfreq,
                          ALQ120Q == 0 & ALQ120U == NA ~ 0,
                          ALQ120Q == NA & ALQ120U == NA ~ NA),
         drinkstotalyr = drinksfreqcalc * drinksamt,
         drinktype = as_factor(case_when(
                            RIAGENDR == "Female" & ALQ130 >= 4 ~ "Binge Drinker",
                            RIAGENDR == "Female" & ALQ130 < 4 ~ "Drinker",
                            RIAGENDR == "Male" & ALQ130 >= 5 ~ "Binge Drinker",
                            RIAGENDR == "Male" & ALQ130 < 5 ~ "Drinker",
                            ALQ110 == "No" ~ "Non Drinker"))
           ) %>% 
  rename(
    gender = RIAGENDR,
    age = RIDAGEYR,
    psu = SDMVPSU,
    strata = SDMVSTRA,
    bmc = DXDTOBMC,
    bmd = DXDTOBMD
  ) %>% 
  dplyr::select(-RIDRETH3, -RIDEXPRG, -ALQ101, -ALQ110, - ALQ120Q, - ALQ120U, -ALQ130, -WTMEC2YR)
```
 
Now I'm going to check out how my new columns look and make sure that there's nothing unexpected that I didn't cover through all of my case_when()s and if_else()s.

```r
str(dff)
```

```
## 'data.frame':	29902 obs. of  16 variables:
##  $ gender        : Factor w/ 2 levels "Male","Female": 1 2 1 2 2 1 1 1 1 1 ...
##  $ age           : num  22 3 14 44 14 9 0 6 21 15 ...
##  $ psu           : num  1 3 3 1 2 1 2 2 1 3 ...
##  $ strata        : num  91 92 90 94 90 91 92 103 92 91 ...
##  $ bmc           : num  2589 NA 1931 2039 1944 ...
##  $ bmd           : num  1.21 NA 1.02 1.07 1.06 ...
##  $ weight        : num  34746 5372 2623 42655 4461 ...
##  $ preg          : Factor w/ 2 levels "No","Yes": NA NA NA 1 NA NA NA NA NA NA ...
##  $ race          : Factor w/ 5 levels "Asian","Black",..: 5 3 1 5 2 5 1 4 1 4 ...
##  $ drinksyr      : Factor w/ 3 levels "Yes","No","Don't know": 2 NA NA NA NA NA NA NA 1 NA ...
##  $ drinkslife    : Factor w/ 4 levels "Yes","No","Don't know",..: 2 NA NA NA NA NA NA NA NA NA ...
##  $ drinksfreq    : num  NA NA NA NA NA NA NA NA 1 NA ...
##  $ drinksamt     : num  NA NA NA NA NA NA NA NA 2 NA ...
##  $ drinksfreqcalc: num  NA NA NA NA NA NA NA NA 12 NA ...
##  $ drinkstotalyr : num  NA NA NA NA NA NA NA NA 24 NA ...
##  $ drinktype     : Factor w/ 3 levels "Non Drinker",..: 1 NA NA NA NA NA NA NA 2 NA ...
```

It looks like factor levels that have no observations are still appearing as a valid level, I'm going to use the droplevels() function to clean this up.

```r
dff$drinksyr <- droplevels(dff$drinksyr)
dff$drinkslife <- droplevels(dff$drinkslife)
```
 
And now re-checking.

```r
str(dff)
```

```
## 'data.frame':	29902 obs. of  16 variables:
##  $ gender        : Factor w/ 2 levels "Male","Female": 1 2 1 2 2 1 1 1 1 1 ...
##  $ age           : num  22 3 14 44 14 9 0 6 21 15 ...
##  $ psu           : num  1 3 3 1 2 1 2 2 1 3 ...
##  $ strata        : num  91 92 90 94 90 91 92 103 92 91 ...
##  $ bmc           : num  2589 NA 1931 2039 1944 ...
##  $ bmd           : num  1.21 NA 1.02 1.07 1.06 ...
##  $ weight        : num  34746 5372 2623 42655 4461 ...
##  $ preg          : Factor w/ 2 levels "No","Yes": NA NA NA 1 NA NA NA NA NA NA ...
##  $ race          : Factor w/ 5 levels "Asian","Black",..: 5 3 1 5 2 5 1 4 1 4 ...
##  $ drinksyr      : Factor w/ 2 levels "Yes","No": 2 NA NA NA NA NA NA NA 1 NA ...
##  $ drinkslife    : Factor w/ 2 levels "Yes","No": 2 NA NA NA NA NA NA NA NA NA ...
##  $ drinksfreq    : num  NA NA NA NA NA NA NA NA 1 NA ...
##  $ drinksamt     : num  NA NA NA NA NA NA NA NA 2 NA ...
##  $ drinksfreqcalc: num  NA NA NA NA NA NA NA NA 12 NA ...
##  $ drinkstotalyr : num  NA NA NA NA NA NA NA NA 24 NA ...
##  $ drinktype     : Factor w/ 3 levels "Non Drinker",..: 1 NA NA NA NA NA NA NA 2 NA ...
```
 
### Variable Summary

My final dataset includes the following variables that I'm going to use to assess alcohol consumption
 - drinksyr: have you had at least 12 drinks in a year?
 - drinkslife: have you had at least 12 drinks in your life?
 - drinksamt: ALQ130 with missing values recoded as NA
 - drinksfreq: ALQ120Q with missing values recoded as NA
 - drinksfreqcalc: drinking days during a year period
 - drinkstotalyr: average total drinks a year created by multiplying average drinks/drinking day with average amount of drinking days in a year
 - drinktype: if an average drinking day for an individual is considered binge drinking (note that these amounts are different for men and women)

## Table

```r
init.table <- CreateTableOne(data = dff, 
                             includeNA = TRUE)
print(init.table,
      showAllLevels = TRUE)
```

```
##                             
##                              level         Overall            
##   n                                           29902           
##   gender (%)                 Male             14751 (49.3)    
##                              Female           15151 (50.7)    
##   age (mean (SD))                             31.60 (24.59)   
##   psu (mean (SD))                              1.54 (0.56)    
##   strata (mean (SD))                         111.13 (13.03)   
##   bmc (mean (SD))                           2080.01 (604.26)  
##   bmd (mean (SD))                              1.05 (0.15)    
##   weight (mean (SD))                       10414.87 (10842.12)
##   preg (%)                   No                3341 (11.2)    
##                              Yes                192 ( 0.6)    
##                              <NA>             26369 (88.2)    
##   race (%)                   Asian             3398 (11.4)    
##                              Black             7079 (23.7)    
##                              Hispanic          8350 (27.9)    
##                              Other             1362 ( 4.6)    
##                              White             9713 (32.5)    
##   drinksyr (%)               Yes              10844 (36.3)    
##                              No                4748 (15.9)    
##                              <NA>             14310 (47.9)    
##   drinkslife (%)             Yes               2047 ( 6.8)    
##                              No                2700 ( 9.0)    
##                              <NA>             25155 (84.1)    
##   drinksfreq (mean (SD))                       3.49 (14.26)   
##   drinksamt (mean (SD))                        2.77 (2.64)    
##   drinksfreqcalc (mean (SD))                  72.23 (96.89)   
##   drinkstotalyr (mean (SD))                  227.84 (465.05)  
##   drinktype (%)              Non Drinker       2700 ( 9.0)    
##                              Drinker           8482 (28.4)    
##                              Binge Drinker     1827 ( 6.1)    
##                              <NA>             16893 (56.5)
```
 
## Survey Object
 
Here I used the survey package to create a survey design object with my clean dataset.

```r
alc.svy <- svydesign(id = ~psu,
                     strata = ~strata,
                     weights = ~weight,
                     nest = TRUE,
                     survey.lonely.psu = "adjust",
                     data = dff)
```
 
### Exclusion Criteria

As survey data is only supposed to be filtered through the subset() method of an already existing survey object, I used this method to apply my exclusion criteria of age being over 21 and the participant not being pregnant. I also filtered out observations where the first question about alcohol consumption behaviors was missing, and missing information about bone mineral density and bone mineral content. 

```r
alc.subset <- subset(alc.svy,
                     age >= 21 
                     & preg == "No" | is.na(preg) 
                     & !is.na(drinksyr) 
                     & !is.na(bmc) 
                     & !is.na(bmd))
```
 

## Graphics
### Box Plot of Bone Health Metrics by Type of Drinker

Here I used the built in graphics in the survey package to create a box plot.

```r
par(mfrow = c(1, 2),
    xpd = NA)
svyboxplot(bmd ~ drinktype, 
           alc.subset,
           ylab = "Bone Mineral Density",
           col = c("#98bad5", "#305674", "#c6d3e3"),
           show.names = FALSE)
svyboxplot(bmc ~ drinktype, 
           alc.subset,
           ylab = "Bone Mineral Content",
           col = c("#98bad5", "#305674", "#c6d3e3"),
           show.names = FALSE,
           bty = "L")
legend("bottomleft", 
       legend = c("Non Drinker", "Normal Drinker", "Binge Drinker"),
       fill = c("#98bad5", "#305674", "#c6d3e3"),
       inset = c(-1.25, -0.3),
       cex = 0.9,
       horiz = TRUE)
mtext("Bone Mineral Content and Density by Type of Drinker", 
      side = 3, 
      line = -2.5,
      cex = 1.2,
      outer = TRUE)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-12-1.png" width="672" />

### Bone Health Metrics by Average Amount of Alcoholic Drinks Consumed in One Year

Here I use the built in graphics to take a look at the relationship between the two different bone health measurements and total average alcohol consumption over the course of a year.

```r
par(mfrow = c(1, 2))
      
svyplot(bmd ~ drinkstotalyr, 
        alc.subset,
        xlim = c(0, 5000),
        xlab = "",
        ylab = "Bone Mineral Density (g/cm^2)",
        style = c("transparent"))
lines(svysmooth(bmd ~ drinkstotalyr, 
                alc.subset, 
                bandwidth = 200),
       col = "#2C54A7",
       lwd = 2)

svyplot(bmc ~ drinkstotalyr, 
        alc.subset,
        xlim = c(0, 5000),
        ylab = "Bone Mineral Content (g)",
        xlab = "",
        style = c("transparent"))
lines(svysmooth(bmc ~ drinkstotalyr, 
                alc.subset, 
                bandwidth = 200),
       col = "#2C54A7",
       lwd = 2)
mtext("Bone Mineral Density(g/cm^2) and Bone Mineral Content by
      Average Alcoholic Drinks in One Year", 
      side = 3, 
      line = -2.5,
      cex = 1.2,
      outer = TRUE)
mtext("Average Alcoholic Drinks in One Year", 
      side = 1, 
      line = -2.5,
      outer = TRUE)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-13-1.png" width="672" />

## Average Total Drinks in One Year Regression

Now I'm going to fit some models to assess the relationship between my selected responses and covariates to see if there are any relationships between alcohol consumption and bone health measures when adjusting for other variables. 
I also plot the diagnostic plots to assess these models.

### Bone Density

```r
bmd.glm <- svyglm(bmd ~ drinkstotalyr + gender + age + race, 
               alc.subset)
par(mfrow = c(2, 2))
plot(bmd.glm)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-14-1.png" width="672" />
Diagnostics look good, the data is relatively normal and there are no obvious patterns in the other values that would throw off any assumptions. Now I'm going to take a look at a summary of the model to get the coefficients. 

```r
summary(bmd.glm)
```

```
## 
## Call:
## svyglm(formula = bmd ~ drinkstotalyr + gender + age + race, design = alc.subset)
## 
## Survey design:
## subset(alc.svy, age >= 21 & preg == "No" | is.na(preg) & !is.na(drinksyr) & 
##     !is.na(bmc) & !is.na(bmd))
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    1.124e+00  7.061e-03 159.139  < 2e-16 ***
## drinkstotalyr -3.275e-06  3.813e-06  -0.859  0.39562    
## genderFemale  -6.502e-02  3.100e-03 -20.974  < 2e-16 ***
## age           -4.602e-04  1.337e-04  -3.441  0.00137 ** 
## raceBlack      1.052e-01  5.846e-03  17.991  < 2e-16 ***
## raceHispanic   2.298e-02  4.843e-03   4.744 2.66e-05 ***
## raceOther      4.947e-02  9.054e-03   5.463 2.68e-06 ***
## raceWhite      3.928e-02  5.236e-03   7.502 3.79e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.01040399)
## 
## Number of Fisher Scoring iterations: 2
```
So it looks like drinkstotalyr is not statistically significant. 

### Bone Mineral Content

Now to check out the same things for the bone mineral content variable instead of the bone mineral density.

```r
bmc.glm <- svyglm(bmc ~ drinkstotalyr + gender + age + race, 
               alc.subset)
par(mfrow = c(2, 2))
plot(bmc.glm)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-16-1.png" width="672" />
This looks okay, I'm going to look at the summary.

```r
summary(bmc.glm)
```

```
## 
## Call:
## svyglm(formula = bmc ~ drinkstotalyr + gender + age + race, design = alc.subset)
## 
## Survey design:
## subset(alc.svy, age >= 21 & preg == "No" | is.na(preg) & !is.na(drinksyr) & 
##     !is.na(bmc) & !is.na(bmd))
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   2397.58331   19.63872 122.085  < 2e-16 ***
## drinkstotalyr   -0.01464    0.01079  -1.357   0.1824    
## genderFemale  -501.15816    9.84552 -50.902  < 2e-16 ***
## age             -0.76125    0.37994  -2.004   0.0519 .  
## raceBlack      467.51804   19.12565  24.445  < 2e-16 ***
## raceHispanic   139.32230   17.15159   8.123 5.39e-10 ***
## raceOther      278.44977   38.19013   7.291 7.40e-09 ***
## raceWhite      264.39021   17.33046  15.256  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 128137.6)
## 
## Number of Fisher Scoring iterations: 2
```
This is not statistically significant either.

## Type of Drinker Regression

I'm now going to look at my created drinktype variable to see if there is any signifigance within a model that has that variable instead of total drinks consumed in a year on average.

### Bone Mineral Density

```r
bmd.glm2 <- svyglm(bmd ~ drinktype + gender + age + race, 
                   alc.subset)
summary(bmd.glm2)
```

```
## 
## Call:
## svyglm(formula = bmd ~ drinktype + gender + age + race, design = alc.subset)
## 
## Survey design:
## subset(alc.svy, age >= 21 & preg == "No" | is.na(preg) & !is.na(drinksyr) & 
##     !is.na(bmc) & !is.na(bmd))
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             1.1014758  0.0066615 165.350  < 2e-16 ***
## drinktypeDrinker        0.0188781  0.0041735   4.523 5.56e-05 ***
## drinktypeBinge Drinker  0.0144244  0.0046685   3.090 0.003686 ** 
## genderFemale           -0.0655285  0.0030256 -21.658  < 2e-16 ***
## age                    -0.0004467  0.0001254  -3.561 0.000992 ***
## raceBlack               0.1079738  0.0054775  19.712  < 2e-16 ***
## raceHispanic            0.0251814  0.0036067   6.982 2.27e-08 ***
## raceOther               0.0545921  0.0081870   6.668 6.14e-08 ***
## raceWhite               0.0428369  0.0047636   8.992 4.74e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.00985392)
## 
## Number of Fisher Scoring iterations: 2
```

### Bone Mineral Content

```r
bmc.glm2 <- svyglm(bmc ~ drinktype + gender + age + race, 
                   alc.subset)
summary(bmc.glm2)
```

```
## 
## Call:
## svyglm(formula = bmc ~ drinktype + gender + age + race, design = alc.subset)
## 
## Survey design:
## subset(alc.svy, age >= 21 & preg == "No" | is.na(preg) & !is.na(drinksyr) & 
##     !is.na(bmc) & !is.na(bmd))
## 
## Coefficients:
##                         Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            2321.5997    17.1247 135.571  < 2e-16 ***
## drinktypeDrinker         65.2218    17.4097   3.746  0.00058 ***
## drinktypeBinge Drinker   61.9370    17.8532   3.469  0.00129 ** 
## genderFemale           -500.5703     9.3263 -53.673  < 2e-16 ***
## age                      -0.6723     0.3902  -1.723  0.09281 .  
## raceBlack               464.5450    18.3295  25.344  < 2e-16 ***
## raceHispanic            138.4712    14.0689   9.842 4.01e-12 ***
## raceOther               291.6212    37.1853   7.842 1.54e-09 ***
## raceWhite               270.4601    15.9841  16.921  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 121702.2)
## 
## Number of Fisher Scoring iterations: 2
```

So these models do show a positive increase of bone mineral content and density when the individual is classified as a drinker and also, to a lesser extent, as a binge drinker, which reinforces the findings from previous papers about this topic.

## Conclusion

Things that are generally considered bad for you aren't always bad for everything, sometimes they can also be neutral when you're looking at very specific measures of someone's health rather than the big picture.

I'm definitely used to using ggplot2 for graphics, but using R's default graphics syntax was a bit challenging and definitely took me a bit to find out how to do certain things, like putting the legend on the box plot for example. I was originally hesitant to use these built in graphics due to that, but I decided to go ahead and use them as it is meant to be used with survey data and the content was correct, it just took me longer to customize the plots.
