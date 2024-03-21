---
title: SAMHSA National Survey on Drug Use and Health Analysis
author: R package build
date: '2024-03-21'
slug: cigarettes-samhsa
categories: ["R", "Public Health"]
tags: ["Tobacco", "Health Behaviors", "Survey"]
description: An analysis of SAMHSAs National Survey on Drug Use and Health to explore differences in lifetime cigarette use
image: "images/purp.jpeg"
math: ~
license: Michelle Gulotta
hidden: no
comments: no
---

For this analysis I decided to try analyzing a different survey, the 2021 National Survey on Drug Use and Health conducted by the Substance Abuse and Mental Health Services Administration, also known as SAMHSA.

## Load Libraries

```r
library(tidyverse)
```

```
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.1.4     ✔ readr     2.1.4
## ✔ forcats   1.0.0     ✔ stringr   1.5.1
## ✔ ggplot2   3.5.0     ✔ tibble    3.2.1
## ✔ lubridate 1.9.3     ✔ tidyr     1.3.0
## ✔ purrr     1.0.2     
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

```r
library(haven)
library(survey)
```

```
## Loading required package: grid
## Loading required package: Matrix
## 
## Attaching package: 'Matrix'
## 
## The following objects are masked from 'package:tidyr':
## 
##     expand, pack, unpack
## 
## Loading required package: survival
## 
## Attaching package: 'survey'
## 
## The following object is masked from 'package:graphics':
## 
##     dotchart
```

## Import Data

I came across this [useful guide](https://asdfree.com/national-survey-on-drug-use-and-health-nsduh.html) on how to import the data from the 2021 SAMHSA NSDUH survey, as I did not want to download all of the data locally since the files for these complex surveys tend to be very big, using the method below I was able to import it directly into R. 

```r
zip_tf <- tempfile()
zip_url <- paste0(
  "https://www.datafiles.samhsa.gov/sites/default/files/field-uploads-protected/",
  "studies/NSDUH-2021/NSDUH-2021-datasets/NSDUH-2021-DS0001/",
  "NSDUH-2021-DS0001-bundles-with-study-info/NSDUH-2021-DS0001-bndl-data-r_v3.zip")
download.file(zip_url, zip_tf, mode = 'wb')
nsduh_rdata <- unzip(zip_tf, exdir = tempdir())
nsduh_rdata_contents <- load(nsduh_rdata)
nsduh_df_name <- grep('PUF' , nsduh_rdata_contents , value = TRUE)
nsduh_df <- get(nsduh_df_name)
names( nsduh_df ) <- tolower(names(nsduh_df))
nsduh_df[ , "one"] <- 1
```

## Select Variables of Interest

After importing the data I realized that there were 2989 different variables that represented the various answers to all of the survey questions. As I was only interested in some demographic variables and two variables that represented questions about cigarette use, I used dplyr's select() to dramatically pare down this data. I also included the id, strata, and weight variables as these are very important aspects of analyzing survey data and will be used later in my survey design object.

```r
cig <- nsduh_df %>% 
  dplyr::select(cigever, # Have you ever smoked a cigarette?
                cigtry, # Age when first smoked a cigarette
                catag6, # Age category (6 Levels)
                sexident, # Sexual identity
                speakengl, # How well do you speak English?
                irsex, # Gender
                eduhighcat, # Level of Education
                newrace2, # Race/Hispanic Origin
                wrkstatwk2, # What was your work situation in the past week?
                irpinc3, # Income Level
                govtprog, # Participated in Government Assistance
                verep, # ID
                vestr_c, # Strata
                analwt_c) # Weights
```

Now to take a look at the variables of the new and smaller data set.

```r
str(cig)
```

```
## 'data.frame':	58034 obs. of  14 variables:
##  $ cigever   : int  1 1 2 1 2 1 1 2 2 2 ...
##  $ cigtry    : int  19 15 991 9 991 15 23 991 991 991 ...
##  $ catag6    : int  3 6 2 4 2 5 2 4 1 2 ...
##  $ sexident  : int  3 1 1 1 1 1 1 1 99 1 ...
##  $ speakengl : int  1 1 1 1 2 3 1 1 1 1 ...
##  $ irsex     : int  2 1 2 1 1 1 1 2 2 2 ...
##  $ eduhighcat: int  3 4 4 1 2 1 4 4 5 2 ...
##  $ newrace2  : int  1 1 1 1 7 7 5 1 2 7 ...
##  $ wrkstatwk2: int  5 8 1 5 9 9 1 1 99 2 ...
##  $ irpinc3   : int  2 7 1 1 1 1 3 5 1 1 ...
##  $ govtprog  : int  1 2 2 2 2 2 2 2 1 2 ...
##  $ verep     : int  1 2 2 2 1 2 2 1 2 2 ...
##  $ vestr_c   : int  40047 40037 40037 40045 40006 40041 40021 40008 40007 40022 ...
##  $ analwt_c  : num  675 12436 647 11275 351 ...
##  - attr(*, "var.labels")= chr [1:2988] "RESPONDENT IDENTIFICATION" "CREATION DATE OF THE DATA FILE" "EVER SMOKED A CIGARETTE" "IF BEST FRIEND OFFERED, WOULD YOU SMOKE CIG" ...
```

## Data Cleaning

I'm going to re-code some variables as I realized while reading through the documentation that binary variables were coded as 1 and 2 rather than 0 and 1, and missing values were coded in various numeric ways and would throw off my calculations if I were to not replace them with NAs. As I needed to do this across multiple columns, to avoid repetitive code I used the across() function.

```r
cig2 <- cig %>% 
  mutate(
    across(
      .cols = c(cigtry,
                sexident,
                speakengl,
                wrkstatwk2),
      .fns = ~if_else(.x == 985 | 
                      .x == 991 | 
                      .x == 994 | 
                      .x == 997 |
                      .x == 85 |
                      .x == 94 |
                      .x == 97 |
                      .x == 98 |
                      .x == 99,
                      NA,
                      .x)
    ),
    across(
      .cols = c(cigever, govtprog, irsex),
      .fns = ~if_else(.x == 2, 0, 1)
  )
)
```

After re-coding the variables, I wanted to factor them and give them labels so that my graphics and model outputs would be more interpretable without having to keep going back to the NSDUH survey documentation.

```r
cig3 <- cig2 %>% 
  mutate(
    cigever = factor(cigever, 
                     levels = c(0, 1), 
                     labels = c("No", "Yes")),
    catag6 = factor(catag6, 
                    levels = c(1:6),
                    labels = c("12-17 years old",
                               "18-25 years old",
                               "26-34 years old",
                               "35-49 years old",
                               "50-64 years old",
                               "65+ years old")),
    sexident = factor(sexident,
                      levels = c(1:3),
                      labels = c("Straight",
                                 "Lesbian or Gay",
                                 "Bisexual")),
    speakengl = factor(speakengl,
                       levels = c(1:4),
                       labels = c("Very Well",
                                  "Well",
                                  "Not Well",
                                  "Not at All")),
    irsex = factor(irsex, 
                   levels = c(0, 1),
                   labels = c("Female", "Male")),
    eduhighcat = factor(eduhighcat,
                        levels = c(1:5),
                        labels = c("Less than High School",
                                   "High School Graduate",
                                   "Some College / Associates Degree",
                                   "College Graduate",
                                   "12-17 Year Old")),
    newrace2 = factor(newrace2,
                      levels = c(1:7),
                      labels = c("White",
                                 "Black",
                                 "Native American / Alaska Native",
                                 "Native Hawaiian / Pacific Islander",
                                 "Asian",
                                 "More Than One Race",
                                 "Hispanic")),
    wrkstatwk2 = factor(wrkstatwk2,
                        levels = c(1:9),
                        labels = c("Full Time Job",
                                   "Part Time Job",
                                   "Has Job / Volunteer Work but Did Not Work Last Week",
                                   "Unemployed / Laid Off Looking for Work",
                                   "Disabled",
                                   "Homemaker",
                                   "In School / Training",
                                   "Retired",
                                   "Does Not Have a Job, Other Reason")),
    irpinc3 = factor(irpinc3,
                     levels = c(1:7),
                     labels = c("Less than $10,000",
                                "Between $10,000 and $19,000",
                                "Between $20,000 and $29,000",
                                "Between $30,000 and $39,000",
                                "Between $40,000 and $49,000",
                                "Between $50,000 and $74,000",
                                "$75,000 or More")),
    govtprog = factor(govtprog,
                      levels = c(0, 1),
                      labels = c("No", "Yes"))
  )
```


## Survey Design Object

Below, I create my survey design object using the id, strata, and weights provided by the NSDUH data set using the svydesign() function.

```r
cig.svy <- svydesign(
  id = ~verep, 
  strata = ~vestr_c, 
  weights = ~analwt_c,
  data = cig3,
  nest = TRUE
)
```

## Participation in Government Assistance Program(s) and Cigarette Use

The first predictor variable I wanted to look at was participation in one or more government programs and differences in age when the participant first tried a cigarette, and if they ever tried a cigarette at all.

### Mean Age First Tried a Cigarette and Government Assistance Program Participant

Here, I used the svyby() function to check out the means for the two different groups.

```r
svyby(~cigtry, ~govtprog, cig.svy, svymean, na.rm = TRUE)
```

```
##     govtprog   cigtry         se
## No        No 16.35160 0.05977798
## Yes      Yes 15.79158 0.12337171
```
And to graph this data.

```r
svyboxplot(cigtry ~ govtprog, 
           cig.svy,
           ylim = c(0, 30),
           xlab = "Participated in One or More Government Assistance Programs",
           ylab = "Age",
           col = c("#A6A9C8", "#554D74"))
title(main = "Average Age Participant First Tried a Cigarette
      by Participation in Government Assistance Programs",
      adj = 0.5)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-9-1.png" width="672" />

It seems like the mean age where a cigarette was first tried for participants who have used one or more government assistance programs is slightly lower on average, lets see if this difference is statistically significant.

First, I'm going to look at how the weighted cigtry variable is distributed.

```r
svyhist(~cigtry, 
        cig.svy, 
        breaks = 30,
        col = c("#A6A9C8"),
        main = "Distribution of Age Participant First Tried a Cigarette")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-10-1.png" width="672" />

It seems that there are some outliers who first tried a cigarette at an older age, but the distribution is mostly bell curve shaped. I'm going to run a weighted t-test using the svyttest() function.

```r
svyttest(cigtry ~ govtprog, cig.svy)
```

```
## 
## 	Design-based t-test
## 
## data:  cigtry ~ govtprog
## t = -4.2839, df = 49, p-value = 8.546e-05
## alternative hypothesis: true difference in mean is not equal to 0
## 95 percent confidence interval:
##  -0.8227208 -0.2973152
## sample estimates:
## difference in mean 
##          -0.560018
```
It looks like this difference between the two groups is statistically significant, and those who have utilized one or more government assistance programs tend to first try a cigarette at a younger age.

### Has the Participant Ever Tried a Cigarette and Government Assistance Program Participation

In order to assess this I'm going to use the svychisq() function to determine if there is a difference between the proportions within the two different binary variables. I'm going to set the Ntotal setting to true as the weighted aspect of the survey package results in very large numbers, making it a bit less readable from just glancing at it.

```r
svytable(~cigever + govtprog, cig.svy, Ntotal = TRUE)
```

```
##        govtprog
## cigever         No        Yes
##     No  0.38847802 0.09974387
##     Yes 0.39291777 0.11886035
```

```r
svychisq(~cigever + govtprog, cig.svy, statistic = "Chisq")
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  svychisq(~cigever + govtprog, cig.svy, statistic = "Chisq")
## X-squared = 66.313, df = 1, p-value = 6.518e-06
```
It looks participants who have utilized one or more government assistance programs more often have tried a cigarette in their lives.

### Ever Smoked a Cigarette Logistic Regression

Lastly, I am going to take several predictor variables of interest and fit a multivariable weighted logistic regression model. I would like to see which coefficients have a statistically significant impact on whether a participant has ever smoked a cigarette in their lives.


```r
cig.fit <- svyglm(cigever ~ sexident + 
                     speakengl + 
                     irsex + 
                     eduhighcat + 
                     newrace2 + 
                     wrkstatwk2 + 
                     irpinc3 + 
                     govtprog,
                   cig.svy,
                   family = quasibinomial)
summary(cig.fit)
```

```
## 
## Call:
## svyglm(formula = cigever ~ sexident + speakengl + irsex + eduhighcat + 
##     newrace2 + wrkstatwk2 + irpinc3 + govtprog, design = cig.svy, 
##     family = quasibinomial)
## 
## Survey design:
## svydesign(id = ~verep, strata = ~vestr_c, weights = ~analwt_c, 
##     data = cig3, nest = TRUE)
## 
## Coefficients:
##                                                               Estimate
## (Intercept)                                                    0.10862
## sexidentLesbian or Gay                                         0.24135
## sexidentBisexual                                               0.31938
## speakenglWell                                                 -0.31160
## speakenglNot Well                                             -0.81491
## speakenglNot at All                                           -0.88363
## irsexMale                                                      0.37537
## eduhighcatHigh School Graduate                                -0.18217
## eduhighcatSome College / Associates Degree                    -0.09616
## eduhighcatCollege Graduate                                    -0.41272
## newrace2Black                                                 -0.94815
## newrace2Native American / Alaska Native                       -0.14689
## newrace2Native Hawaiian / Pacific Islander                    -0.86070
## newrace2Asian                                                 -1.04402
## newrace2More Than One Race                                    -0.09915
## newrace2Hispanic                                              -0.64055
## wrkstatwk2Part Time Job                                       -0.15217
## wrkstatwk2Has Job / Volunteer Work but Did Not Work Last Week -0.04618
## wrkstatwk2Unemployed / Laid Off Looking for Work               0.25849
## wrkstatwk2Disabled                                             0.54665
## wrkstatwk2Homemaker                                            0.21088
## wrkstatwk2In School / Training                                -1.07989
## wrkstatwk2Retired                                             -0.01107
## wrkstatwk2Does Not Have a Job, Other Reason                    0.11649
## irpinc3Between $10,000 and $19,000                             0.36938
## irpinc3Between $20,000 and $29,000                             0.48541
## irpinc3Between $30,000 and $39,000                             0.48611
## irpinc3Between $40,000 and $49,000                             0.53580
## irpinc3Between $50,000 and $74,000                             0.52905
## irpinc3$75,000 or More                                         0.54041
## govtprogYes                                                    0.44118
##                                                               Std. Error
## (Intercept)                                                      0.09094
## sexidentLesbian or Gay                                           0.11183
## sexidentBisexual                                                 0.06567
## speakenglWell                                                    0.07126
## speakenglNot Well                                                0.18639
## speakenglNot at All                                              0.22132
## irsexMale                                                        0.03388
## eduhighcatHigh School Graduate                                   0.07062
## eduhighcatSome College / Associates Degree                       0.08069
## eduhighcatCollege Graduate                                       0.08046
## newrace2Black                                                    0.05909
## newrace2Native American / Alaska Native                          0.22734
## newrace2Native Hawaiian / Pacific Islander                       0.27240
## newrace2Asian                                                    0.09878
## newrace2More Than One Race                                       0.12547
## newrace2Hispanic                                                 0.05912
## wrkstatwk2Part Time Job                                          0.06677
## wrkstatwk2Has Job / Volunteer Work but Did Not Work Last Week    0.07789
## wrkstatwk2Unemployed / Laid Off Looking for Work                 0.09772
## wrkstatwk2Disabled                                               0.11551
## wrkstatwk2Homemaker                                              0.09539
## wrkstatwk2In School / Training                                   0.12776
## wrkstatwk2Retired                                                0.06071
## wrkstatwk2Does Not Have a Job, Other Reason                      0.07895
## irpinc3Between $10,000 and $19,000                               0.06627
## irpinc3Between $20,000 and $29,000                               0.07007
## irpinc3Between $30,000 and $39,000                               0.07732
## irpinc3Between $40,000 and $49,000                               0.07806
## irpinc3Between $50,000 and $74,000                               0.08146
## irpinc3$75,000 or More                                           0.07447
## govtprogYes                                                      0.04725
##                                                               t value Pr(>|t|)
## (Intercept)                                                     1.194 0.246323
## sexidentLesbian or Gay                                          2.158 0.043245
## sexidentBisexual                                                4.863 9.41e-05
## speakenglWell                                                  -4.373 0.000294
## speakenglNot Well                                              -4.372 0.000295
## speakenglNot at All                                            -3.992 0.000716
## irsexMale                                                      11.078 5.51e-10
## eduhighcatHigh School Graduate                                 -2.579 0.017907
## eduhighcatSome College / Associates Degree                     -1.192 0.247336
## eduhighcatCollege Graduate                                     -5.129 5.11e-05
## newrace2Black                                                 -16.047 6.89e-13
## newrace2Native American / Alaska Native                        -0.646 0.525550
## newrace2Native Hawaiian / Pacific Islander                     -3.160 0.004929
## newrace2Asian                                                 -10.569 1.24e-09
## newrace2More Than One Race                                     -0.790 0.438653
## newrace2Hispanic                                              -10.835 8.08e-10
## wrkstatwk2Part Time Job                                        -2.279 0.033780
## wrkstatwk2Has Job / Volunteer Work but Did Not Work Last Week  -0.593 0.559934
## wrkstatwk2Unemployed / Laid Off Looking for Work                2.645 0.015528
## wrkstatwk2Disabled                                              4.732 0.000127
## wrkstatwk2Homemaker                                             2.211 0.038876
## wrkstatwk2In School / Training                                 -8.453 4.93e-08
## wrkstatwk2Retired                                              -0.182 0.857172
## wrkstatwk2Does Not Have a Job, Other Reason                     1.475 0.155657
## irpinc3Between $10,000 and $19,000                              5.574 1.86e-05
## irpinc3Between $20,000 and $29,000                              6.927 9.99e-07
## irpinc3Between $30,000 and $39,000                              6.287 3.88e-06
## irpinc3Between $40,000 and $49,000                              6.864 1.14e-06
## irpinc3Between $50,000 and $74,000                              6.495 2.48e-06
## irpinc3$75,000 or More                                          7.257 5.08e-07
## govtprogYes                                                     9.337 9.90e-09
##                                                                  
## (Intercept)                                                      
## sexidentLesbian or Gay                                        *  
## sexidentBisexual                                              ***
## speakenglWell                                                 ***
## speakenglNot Well                                             ***
## speakenglNot at All                                           ***
## irsexMale                                                     ***
## eduhighcatHigh School Graduate                                *  
## eduhighcatSome College / Associates Degree                       
## eduhighcatCollege Graduate                                    ***
## newrace2Black                                                 ***
## newrace2Native American / Alaska Native                          
## newrace2Native Hawaiian / Pacific Islander                    ** 
## newrace2Asian                                                 ***
## newrace2More Than One Race                                       
## newrace2Hispanic                                              ***
## wrkstatwk2Part Time Job                                       *  
## wrkstatwk2Has Job / Volunteer Work but Did Not Work Last Week    
## wrkstatwk2Unemployed / Laid Off Looking for Work              *  
## wrkstatwk2Disabled                                            ***
## wrkstatwk2Homemaker                                           *  
## wrkstatwk2In School / Training                                ***
## wrkstatwk2Retired                                                
## wrkstatwk2Does Not Have a Job, Other Reason                      
## irpinc3Between $10,000 and $19,000                            ***
## irpinc3Between $20,000 and $29,000                            ***
## irpinc3Between $30,000 and $39,000                            ***
## irpinc3Between $40,000 and $49,000                            ***
## irpinc3Between $50,000 and $74,000                            ***
## irpinc3$75,000 or More                                        ***
## govtprogYes                                                   ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasibinomial family taken to be 1.12446)
## 
## Number of Fisher Scoring iterations: 4
```

From looking at this model, there are many variables that have an impact on whether the participant reports that they have tried a cigarette in their lives. Some of these have a negative coefficient meaning it makes them more likely to have not tried a cigarette, while others have a positive coefficient meaning the opposite.

Some comparatively large significant negative coefficients that I noticed were that Asian race and in school/training. The positive coefficients were not as dramatic, but some to point out are work status being disabled and income in the range of $75,000 plus.

## Conclusion

It was interesting to analyze another complex survey besides NHANES, as I got to discover a new method of importing a large amount of data into R without having to save it locally. Publicly available survey data is a truly valuable resource, it enables me to learn data analysis skills for free and learn more about the health trends of the country I live in.

I will definitely be coming back to this particular survey, and others that are provided in the guide to practice my skills, and find out more about health behaviors of the American public and the factors that influence them.
