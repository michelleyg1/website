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

NHANES is a very large survey conducted by the CDC that is a crucial resource for public health data analysis, and is all available for free public use. As this was my first time using this dataset, I decided to keep it simple, but I will definitely be coming back to it as there is just so much information collected that can be used for all types of public health analysis.

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

## Checking Assumptions
### Assumption of Normal Distribution
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
cholesterol_model <- lm(total_cholesterol ~ race, data = cholesterol)
plot(cholesterol_model, which = 2)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-12-1.png" width="672" />
It seems like there is a systematic deviation from the expected relationship if the data were to be normally distributed. The data is not normally distributed.

### Assumption of Constant Variance

```r
plot(cholesterol_model, which = 3)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-13-1.png" width="672" />
There is constant variance throughout the data, however since our assumption of normal distribution was violated I am going to check one more assumption that would be needed to run a non parametric test assessing the differences between the central tendency of our chosen groups. 

### Assumption of Similar Skewness for Each Category

```r
ggplot(cholesterol, aes(x = total_cholesterol, fill = race)) +
  geom_histogram(binwidth = 15, color = "black") +
  labs(x = "Total Cholesterol (mg/dL)",
       y = "Count",
       title = "Total Cholesterol (mg/dL) Distribution by Race/Hispanic Origin",
       caption = "Source: CDC NHANES 2017 - Pre Pandemic 2020") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~race) +
  scale_fill_manual(values = c("#E6FFFD", "#AEE2FF", "#ACBCFF", "#B799FF", "#AA77FF"))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-14-1.png" width="672" />
Looks like each category of Race/Hispanic origin does have a similar skewness.

## Statistical Testing

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

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-15-1.png" width="672" />

### Kruskal Wallis Test
Since I checked various assumptions, and the assumption of normality was violated, I decided to go with a non parametric Kruskal Wallis Test to test my hypothesis that there is a different mean total cholesterol value between the 5 different racial/hispanic origin categories.

```r
options(scipen = 999)
kruskal.test(total_cholesterol ~ race, data = cholesterol)
```

```
## 
## 	Kruskal-Wallis rank sum test
## 
## data:  total_cholesterol by race
## Kruskal-Wallis chi-squared = 60.582, df = 4, p-value =
## 0.000000000002188
```
The p value is very very small, meaning there is one or more categories that have a mean total cholesterol that differs from the other. To check which groups are causing there to be a statisticially signifigant difference, I'll run a pairwise comparison.

### Pairwise Comparison Test

```r
pairwise.wilcox.test(cholesterol$total_cholesterol, cholesterol$race,
                 p.adjust.method = "BH")
```

```
## 
## 	Pairwise comparisons using Wilcoxon rank sum test with continuity correction 
## 
## data:  cholesterol$total_cholesterol and cholesterol$race 
## 
##          Asian           Black           Hispanic Other  
## Black    0.0000000000011 -               -        -      
## Hispanic 0.00096         0.0000031636325 -        -      
## Other    0.01722         0.01722         0.77812  -      
## White    0.0000031636325 0.00069         0.12485  0.61744
## 
## P value adjustment method: BH
```
Looks like there are multiple groups in which a statistically significant difference can be observed, in fact it is present in all except Other and Hispanic, White and Hispanic, and Other and White.

## Conclusion
I was always hesitant to use the CDC's NHANES dataset as it is so big and there are so many different variables but I'm glad that I found the nhanesA package as that made it so easy to do this analysis, and I'll definitely be using it again to avoid having to dig for datasets and also having to download gigantic ones to my poor old computer lol. 

I also learned some other things during this analysis, in terms of R skills I learned about the arsenal package that builds tables to display the various categories that were pre-existing in the data, as well as the categories that I created using the cut() function based on the typical ranges used in medicine for total cholesterol. 

In terms of statistics, I still have some confusion about the central limit theorem and normal distributions and if you can actually use parametric tests on non normal distributions, as there seems to be a lot of heated debate over this on the various statistics forums that I visited in hope of getting an answer to this question. I decided to play it safe and use a non parametric test, but I do want to learn more about how and when you can violate assumptions if you should at all.
