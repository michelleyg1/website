---
title: Total Cholesterol CDC NHANES Analysis
author: R package build
date: '2024-01-12'
slug: total-cholesterol-cdc-nhanes-analysis
categories: ["R", "Public Health"]
tags: []
description: An analysis of Total Cholesterol and Race/Hispanic Origin based off of the CDC's NHANES Data from 2017 to Pre Pandemic 2020
image: "images/purple.jpeg"
math: ~
license: ~
hidden: no
comments: no
---

For this analysis I decided to use the CDC NHANES data (2017 - pre pandemic 2020). I found [this guide](https://ehsanx.github.io/SPPH504007SurveyData/docs/importing-nhanes-to-r.html#nhanes-dataset) on importing and cleaning NHANES data using the nhanesA package, and found it very useful in this project. 

NHANES is a very large survey conducted by the CDC that is a crucial resource for public health data analysis, and is all availabe for free public use. As this was my first time using this dataset, I decided to keep it simple, but I will definitely be coming back to it as there is just so much information collected that can be used for all types of public health analysis.

## Load Packages

```r
suppressPackageStartupMessages({
library(tidyverse)
library(nhanesA)
library(tableone)
library(arsenal)
library(agricolae)
})
```

## Grab Datasets that I Need

Here I used the nhanesA package to get the datasets (or Data Files as they are called on the NHANES site), the two that I needed for this analysis were the demographics dataset

```r
demo <- nhanes("P_DEMO")
demo_translate <- nhanesTranslate("P_DEMO", names(demo), data = demo)
```

```
## Translated columns: RIDSTATR RIAGENDR RIDRETH1 RIDRETH3 RIDEXMON DMDBORN4 DMDYRUSZ DMDEDUC2 DMDMARTZ RIDEXPRG SIALANG SIAPROXY SIAINTRP FIALANG FIAPROXY FIAINTRP MIALANG MIAPROXY MIAINTRP AIALANGA
```

and the total cholesterol one

```r
exam <- nhanes("P_TCHOL")
exam_translate <- nhanesTranslate("P_TCHOL", names(exam), data = exam)
```

```
## Warning in nhanesTranslate("P_TCHOL", names(exam), data = exam): No columns
## were translated
```

## Retain Useful Variables

Here I am only retaining the variables that I want to bring into my final dataset.

From the demographics file, I decided on SEQN which is the sequence number that will help us merge the two datasets, RIDEXPRG which indicates if the respondent is pregnant, RIAGENDR which stores the participants gender, RIDAGEYR which stores the participant's age in years, and lastly RIDRETH3 which categorizes the participant's race.

From the exam file I only extracted the sequence number, and lab value for total cholesterol.

```r
demo_select <- demo_translate %>% 
  select(SEQN, RIDEXPRG, RIAGENDR, RIDAGEYR, RIDRETH3)

exam_select <- exam_translate %>% 
  select(SEQN, LBXTC)
```

## Merge the Data Using SEQN to Match Values

The structure of the NHANES database makes it easy to match which lab values belong to which patient in including the SEQN column on the seperate datasets. I also went ahead and dropped the SEQN column when I was done merging as I only needed it for that task.


```r
merged_data <- merge(demo_select, exam_select,
                     by = c("SEQN"), all = TRUE)
merged_data$SEQN <- NULL
```

## Initial Investigaton of the Data

Here I want to see the different categories and how much we have of each so that I can apply the eligibility criteria and recode the values properly.

```r
initial_table <- CreateTableOne(data = merged_data,
                               includeNA = TRUE)
print(initial_table,
      showAllLevels = TRUE)
```

```
##                       
##                        level                                                             
##   n                                                                                      
##   RIDEXPRG (%)         Yes, positive lab pregnancy test or self-reported pregnant at exam
##                        The participant was not pregnant at exam                          
##                        Cannot ascertain if the participant is pregnant at exam           
##                        <NA>                                                              
##   RIAGENDR (%)         Male                                                              
##                        Female                                                            
##   RIDAGEYR (mean (SD))                                                                   
##   RIDRETH3 (%)         Mexican American                                                  
##                        Other Hispanic                                                    
##                        Non-Hispanic White                                                
##                        Non-Hispanic Black                                                
##                        Non-Hispanic Asian                                                
##                        Other Race - Including Multi-Racial                               
##   LBXTC (mean (SD))                                                                      
##                       
##                        Overall       
##   n                     15560        
##   RIDEXPRG (%)             87 ( 0.6) 
##                          1604 (10.3) 
##                           183 ( 1.2) 
##                         13686 (88.0) 
##   RIAGENDR (%)           7721 (49.6) 
##                          7839 (50.4) 
##   RIDAGEYR (mean (SD))  33.74 (25.32)
##   RIDRETH3 (%)           1990 (12.8) 
##                          1544 ( 9.9) 
##                          5271 (33.9) 
##                          4098 (26.3) 
##                          1638 (10.5) 
##                          1019 ( 6.5) 
##   LBXTC (mean (SD))    177.46 (40.36)
```

## Apply Eligibility Criteria

I decided for the eligibility criteria to exclude pregnant women, as well as those under the age of 20, I did that using dplyr's filter() function, I also filtered out the columns where there was no total cholesterol lab value as that is not useful to my analysis.


```r
analytic_data <- merged_data %>% 
  filter(!is.na(LBXTC),
         RIDAGEYR >= 20,
         RIDEXPRG != "Yes, positive lab pregnancy test or self-reported pregnant at exam" | is.na(RIDEXPRG))
```

## Recode and Make Categories

In this section, I created several new variables using dplyr's mutate() function. I created the age_cat variable that groups the participants by age, the total_cholesterol_cat variable that groups the lab values by their normal, borderline and high ranges, and I also simplified the race data into more concise and broad categories.

I also renamed the remaining variables to match the naming conventions in the other newly created variables, also to give some more sense to them as NHANES is great but the variables all look like keyboard smashes to me. Their [variable search tool](https://wwwn.cdc.gov/nchs/nhanes/search/default.aspx) helps with that.

```r
category_data <- analytic_data %>% 
  mutate(
    age_cat = cut(analytic_data$RIDAGEYR,
                  c(20, 40, 60, Inf),
                  right = FALSE),
    total_cholesterol_cat = cut(analytic_data$LBXTC,
                                c(0, 200, 240, Inf),
                                labels = c("Normal", "Borderline", "High"),
                                right = FALSE),
    race = car::recode(analytic_data$RIDRETH3,
                       "c('Mexican American',
                       'Other Hispanic') = 'Hispanic';
                       'Non-Hispanic White' = 'White';
                       'Non-Hispanic Black' = 'Black';
                       'Non-Hispanic Asian' = 'Asian';
                       'Other Race - Including Multi-Racial' = 'Other';
                       else = NA") 
  ) %>% 
  rename(
    pregnancy_status = RIDEXPRG,
    gender = RIAGENDR,
    age = RIDAGEYR,
    total_cholesterol = LBXTC
  ) %>% 
  select(-RIDRETH3, -pregnancy_status)
```


```r
cholesterol <- as_tibble(category_data)
cholesterol
```

```
## # A tibble: 7,845 × 6
##    gender   age total_cholesterol age_cat  total_cholesterol_cat race    
##    <fct>  <dbl>             <dbl> <fct>    <fct>                 <fct>   
##  1 Female    29               195 [20,40)  Normal                Asian   
##  2 Male      49               147 [40,60)  Normal                White   
##  3 Male      36               164 [20,40)  Normal                White   
##  4 Male      68               105 [60,Inf) Normal                Other   
##  5 Male      76               233 [60,Inf) Borderline            White   
##  6 Female    44               212 [40,60)  Borderline            Hispanic
##  7 Female    68               165 [60,Inf) Normal                Black   
##  8 Female    42               229 [40,60)  Borderline            Asian   
##  9 Male      58               172 [40,60)  Normal                Hispanic
## 10 Male      44               189 [40,60)  Normal                White   
## # ℹ 7,835 more rows
```

## Table of Total Cholesterol Categories

Here I used the arsenal package to make a table displaying how the different total cholesterol categories look within our study sample.

```r
labels <- list(age_cat = "Age Range", gender = "Gender", race = "Race")
tab <- arsenal::tableby(total_cholesterol_cat ~ age_cat + gender + race, data = cholesterol, test = FALSE)
summary(tab, labelTranslations = labels, text=TRUE)
```

```
## 
## 
## |            | Normal (N=5236) | Borderline (N=1873) | High (N=736) | Total (N=7845) |
## |:-----------|:---------------:|:-------------------:|:------------:|:--------------:|
## |Age Range   |                 |                     |              |                |
## |-  [20,40)  |  1789 (34.2%)   |     412 (22.0%)     | 117 (15.9%)  |  2318 (29.5%)  |
## |-  [40,60)  |  1499 (28.6%)   |     789 (42.1%)     | 338 (45.9%)  |  2626 (33.5%)  |
## |-  [60,Inf) |  1948 (37.2%)   |     672 (35.9%)     | 281 (38.2%)  |  2901 (37.0%)  |
## |Gender      |                 |                     |              |                |
## |-  Male     |  2659 (50.8%)   |     850 (45.4%)     | 323 (43.9%)  |  3832 (48.8%)  |
## |-  Female   |  2577 (49.2%)   |    1023 (54.6%)     | 413 (56.1%)  |  4013 (51.2%)  |
## |Race        |                 |                     |              |                |
## |-  Asian    |   559 (10.7%)   |     271 (14.5%)     | 108 (14.7%)  |  938 (12.0%)   |
## |-  Black    |  1430 (27.3%)   |     400 (21.4%)     | 162 (22.0%)  |  1992 (25.4%)  |
## |-  Hispanic |  1169 (22.3%)   |     432 (23.1%)     | 164 (22.3%)  |  1765 (22.5%)  |
## |-  Other    |   245 (4.7%)    |      96 (5.1%)      |  36 (4.9%)   |   377 (4.8%)   |
## |-  White    |  1833 (35.0%)   |     674 (36.0%)     | 266 (36.1%)  |  2773 (35.3%)  |
```

## Check Out the Distribution of Total Cholesterol

Next before I look and see if there are any statistically significant differences between the groups of my choice, I have to look and see the distribution of our data to see what kind of assumptions we can make when running the stats.

```r
ggplot(cholesterol, aes(x = total_cholesterol)) +
  geom_histogram(binwidth = 15, color = "black", fill = "#bbbbff") +
  labs(x = "Total Cholesterol (mg/dL)",
       y = "Count",
       title = "Total Cholesterol (mg/dL) Distribution",
       caption = "Source: CDC NHANES 2017 - Pre Pandemic 2020") +
  theme(plot.title = element_text(hjust = 0.5))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-11-1.png" width="672" />


This follows a generally normal distribution, but I'm going to take a closer look as I do see outliers towards the end of our bell curve.

```r
qqnorm(cholesterol$total_cholesterol)
qqline(cholesterol$total_cholesterol, col = "#bbbbff", lwd = 3)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-12-1.png" width="672" />
It does seem to deviate from the normal distribution slightly at the beginning and end of the data, but the sample size is very large, so I'm going to stick with an ANOVA.

## Statistical Analysis

### Visualize Data with a Boxplot

```r
ggplot(cholesterol, aes(x = race, y = total_cholesterol, fill = race)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("#E6FFFD", "#AEE2FF", "#ACBCFF", "#B799FF", "#AA77FF")) +
  labs(
    x = "Race/Hispanic Origin",
    y = "Total Cholesterol (mg/dL)",
    title = "Total Cholesterol (mg/dL) by Race/Hispanic Origin of Participant",
    caption = "Source: CDC NHANES 2017 - Pre Pandemic 2020"
  ) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-13-1.png" width="672" />

It does look like there are some differences among the different means from what I can see on the box plot, but I'm going to run our ANOVA to see if these differences are statistically significant. 

### One Way ANOVA

```r
cholesterol_model <- lm(total_cholesterol ~ race, data = cholesterol)
anova(cholesterol_model)
```

```
## Analysis of Variance Table
## 
## Response: total_cholesterol
##             Df   Sum Sq Mean Sq F value    Pr(>F)    
## race         4    89298 22324.6  13.516 5.561e-11 ***
## Residuals 7840 12949382  1651.7                      
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

With our p-value being very small we can see there is at least one group whose mean is statistically significantly different from the others. Lets see which groups are causing that difference.

### Tukey's HSD Test


```r
cholesterol_aov <- aov(cholesterol_model)
HSD.test(cholesterol_aov, "race", console = TRUE)
```

```
## 
## Study: cholesterol_aov ~ "race"
## 
## HSD Test for total_cholesterol 
## 
## Mean Square Error:  1651.707 
## 
## race,  means
## 
##          total_cholesterol      std    r        se Min Max Q25 Q50 Q75
## Asian             192.3465 40.51686  938 1.3269821  79 384 164 190 217
## Black             180.9347 40.58260 1992 0.9105878  78 428 152 178 206
## Hispanic          186.9637 39.77251 1765 0.9673734  77 381 159 184 211
## Other             185.4881 39.64343  377 2.0931281  76 335 159 183 208
## White             185.3440 41.39873 2773 0.7717765  71 446 155 182 211
## 
## Alpha: 0.05 ; DF Error: 7840 
## Critical Value of Studentized Range: 3.858554 
## 
## Groups according to probability of means differences and alpha level( 0.05 )
## 
## Treatments with the same letter are not significantly different.
## 
##          total_cholesterol groups
## Asian             192.3465      a
## Hispanic          186.9637      b
## Other             185.4881     bc
## White             185.3440     bc
## Black             180.9347      c
```

So there are several statistically differences between Asian and all other groups as well as between Hispanic and Black participants. This is an interesting difference, that can definitely inform health decisions by patients and recommendations by healthcare providers.

## Conclusion
I was always hesitant to use the CDC's NHANES dataset as it is so big and there are so many different variables but I'm glad that I found the nhanesA package as that made it so easy to do this analysis, and I'll definitely be using it again to avoid having to dig for datasets and also having to download gigantic ones to my poor old computer lol. 

I also learned some other things during this analysis, in terms of R skills I learned about the arsenal package that builds tables to display the various categories that were pre-existing in the data, as well as the categories that I created using the cut() function based on the typical ranges used in medicine for total cholesterol. 

In terms of statistics, I still have some confusion about the central limit theorem and normal distributions and if you can actually use parametric tests on non normal distributions, as there seems to be a lot of heated debate over this on the various statistics forums that I visited in hope of getting an answer to this question. Now I'm mainly trying to practice my R skills, and have pretty rudimentary statistics knowledge so I just went with a parametric ANOVA test. In researching for this post I also learned about the Kruskal Wallis test, along with the paired Wilcoxon test. Kind of funny to me coming from a more medical/biology background being used to Latin or Greek based medical terminology that 90% of everything in statistics is named after dudes.
