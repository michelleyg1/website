---
title: Asthma Incidents Analysis
author: R package build
date: '2024-01-03'
slug: asthma-incidents-analysis
categories: ["R", "Public Health"]
tags: []
description: An exploratory analysis of asthma incidents in the United States from 2010 to 2020
image: "images/featured.jpeg"
math: ~
license: Michelle Gulotta
hidden: no
comments: no
---

I found this [interesting dataset](https://www.kaggle.com/datasets/adampq/reported-asthma-incidents-by-state-2010-2020) on Kaggle and wanted to do an exploratory analysis as I also have had asthma since I was a kid and wanted to see if I could find any interesting patterns within the data.

## Load Packages

```r
suppressPackageStartupMessages({
    library(tidyverse)
    library(janitor)
})
```


## Import Dataset

```r
asthma <- read.csv("/Users/michellegulotta/Desktop/my_first_project/asthma/CDIAsthmaByStateTransposed2010-2020.csv")
asthma <- asthma %>% 
  janitor::clean_names(., "snake")
```

Taking a look at the first few rows of the data to see what kind of information this dataset is providing


```r
head(asthma)
```

```
##                                          geo_loc year      state  pop_est
## 1   POINT (-86.63186076199969 32.84057112200048) 2010    Alabama  4785514
## 2  POINT (-147.72205903599973 64.84507995700051) 2010     Alaska   713982
## 3 POINT (-111.76381127699972 34.865970280000454) 2010    Arizona  6407342
## 4   POINT (-92.27449074299966 34.74865012400045) 2010   Arkansas  2921998
## 5  POINT (-120.99999953799971 37.63864012300047) 2010 California 37319550
## 6 POINT (-106.13361092099967 38.843840757000464) 2010   Colorado  5047539
##   f_fatal m_fatal o_fatal   f_er  m_er   o_er f_hosp m_hosp o_hosp
## 1      44      17      61     NA    NA     NA     NA     NA     NA
## 2      NA      NA      NA     NA    NA     NA     NA     NA     NA
## 3      31      27      58  22280 18335  40650   4717   3352   8073
## 4      29       8      37     NA    NA     NA   1876    962   2841
## 5     251     152     403 116571 96999 217637  19399  12958  34583
## 6      33      15      48     NA    NA     NA   2385   1942   4336
```

## Asthma Fatalities by Gender in New Jersey Data Visualization
### Clean Up Data

Since its my home state, I decided to narrow the data that I wanted to look at down a bit to just New Jersey. I also only was interested in looking at fatalities for this particular graph as it is the most severe type of asthma incident recorded in this data.

The first thing that I needed to do was to pivot the data so that gender was its own observation, I originally missed out on this step when I was trying to make the graph and had a hard time coming up with the ggplot code as I was just repeating adding a different shape to my graph for each column. After a bit of trial and error I realized there was probably a way to do it where I wouldn’t have to add the columns individually as their own geoms.


```r
nj_asthma_fatalities <- asthma %>% 
  filter(state == "New Jersey") %>% 
  select(year, ends_with("_fatal")) %>% 
  pivot_longer(
    cols = ends_with("_fatal"),
    names_to = "gender",
    values_to = "fatalities",
    names_pattern = "(.*)_fatal"
  ) %>% 
  mutate(
    gender = factor(gender, 
                    c("f", "m", "o"), 
                    c("Female", "Male", "Overall"))
  ) %>% 
  print()
```

```
## # A tibble: 33 × 3
##     year gender  fatalities
##    <int> <fct>        <dbl>
##  1  2010 Female          49
##  2  2010 Male            31
##  3  2010 Overall         80
##  4  2011 Female          62
##  5  2011 Male            32
##  6  2011 Overall         94
##  7  2012 Female          63
##  8  2012 Male            40
##  9  2012 Overall        103
## 10  2013 Female          73
## # ℹ 23 more rows
```

### Create a Graph

Now to take my newly pivoted data and create a graph using the ggplot2 package:


```r
ggplot(data = nj_asthma_fatalities) +
  geom_smooth(mapping = aes(
    x = year,
    y = fatalities, 
    group = gender, 
    color = gender), 
  se = FALSE) +
  scale_color_manual(values = c("pink", "blue", "black")) +
  scale_x_continuous(n.breaks = 10) +
  labs(title = "Asthma Fatalities in New Jersey by Gender from 2010 to 2020",
       x = "Years", 
       y = "Number of Fatalities") +
  theme(plot.title = element_text(hjust=0.5)) +
  theme_bw()
```

```
## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-5-1.png" width="672" />

This graph shows an interesting trend that at least in the state of New Jersey over the 2010s decade asthma fatality incidents increased and then decreased, just to increase once again around 2017. Also, another trend that caught my eye was that that females made up the majority of overall fatalities that were recorded.

As these are not proportional to the population, they do not tell us the whole story. I’m interested in seeing if the mortality rates show the same pattern as anyone living in New Jersey can tell you that the population has increased over this past decade just from the traffic alone, does the population change account for the increase in asthma fatality incidents?

### Calculate Mortality Rate per 100,000 People Column

I calculated the cause specific mortality rate per 100,000 people in the whole population of the state.


```r
nj_asthma_mortality_rate <- asthma %>% 
  filter(state == "New Jersey") %>% 
  select(year, pop_est, ends_with("_fatal")) %>% 
  pivot_longer(
    cols = ends_with("_fatal"),
    names_to = "gender",
    values_to = "fatalities",
    names_pattern = "(.*)_fatal"
  ) %>% 
  mutate(
    gender = factor(gender, 
                    c("f", "m", "o"), 
                    c("Female", "Male", "Overall")),
    mortality_rate = round(((fatalities / pop_est) * 100000), 2)
  ) %>% 
  print()
```

```
## # A tibble: 33 × 5
##     year pop_est gender  fatalities mortality_rate
##    <int>   <int> <fct>        <dbl>          <dbl>
##  1  2010 8799451 Female          49           0.56
##  2  2010 8799451 Male            31           0.35
##  3  2010 8799451 Overall         80           0.91
##  4  2011 8828552 Female          62           0.7 
##  5  2011 8828552 Male            32           0.36
##  6  2011 8828552 Overall         94           1.06
##  7  2012 8845671 Female          63           0.71
##  8  2012 8845671 Male            40           0.45
##  9  2012 8845671 Overall        103           1.16
## 10  2013 8857821 Female          73           0.82
## # ℹ 23 more rows
```

### Create a Graph of Asthma Mortality Rate per 100,000 People and Group By Gender

Then I used ggplot2 to graph this new column to compare the mortality rate per 100,000 people over the decade of 2010 to 2020 to see if the same trend emerges:


```r
ggplot(data = nj_asthma_mortality_rate) +
  geom_smooth(mapping = aes(x = year, 
                            y = mortality_rate, 
                            group = gender, 
                            color = gender), 
            se = FALSE) +
  scale_color_manual(values = c("pink", "blue", "black")) +
  scale_x_continuous(n.breaks = 10) +
  labs(title = "Asthma Mortality Rate in New Jersey by Gender from 2010 to 2020", 
       x = "Years", 
       y = "Number of Fatalities") +
  theme(plot.title = element_text(hjust=0.5)) +
  theme_bw()
```

```
## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-7-1.png" width="672" />

Interesting, the graphs look pretty much the same to me so the population increase is not the reason for the increase in asthma fatality incidents as the mortality rate from asthma follows the same pattern as the fatality incidents over time, as well as the gender disparity.

## Nationwide Analysis of Gender Differences In Asthma Mortality Rate
### Clean Up Data


```r
asthma_mortality <- asthma %>% 
  select(year, state, pop_est, f_fatal, m_fatal) %>% 
  pivot_longer(
    cols = ends_with("_fatal"),
    names_to = "gender",
    values_to = "fatalities",
    names_pattern = "(.*)_fatal"
  ) %>% 
  mutate(
    gender = factor(gender, 
                    c("f", "m", "o"), 
                    c("Female", "Male", "Overall")),
    mortality_rate = round(((fatalities / pop_est) * 100000), 2)
      ) %>%
  filter(gender != "Overall") %>% 
  print()
```

```
## # A tibble: 1,122 × 6
##     year state       pop_est gender fatalities mortality_rate
##    <int> <chr>         <int> <fct>       <dbl>          <dbl>
##  1  2010 Alabama     4785514 Female         44           0.92
##  2  2010 Alabama     4785514 Male           17           0.36
##  3  2010 Alaska       713982 Female         NA          NA   
##  4  2010 Alaska       713982 Male           NA          NA   
##  5  2010 Arizona     6407342 Female         31           0.48
##  6  2010 Arizona     6407342 Male           27           0.42
##  7  2010 Arkansas    2921998 Female         29           0.99
##  8  2010 Arkansas    2921998 Male            8           0.27
##  9  2010 California 37319550 Female        251           0.67
## 10  2010 California 37319550 Male          152           0.41
## # ℹ 1,112 more rows
```

I noticed there were a decent amount of missing values just by glancing at this lets see how many exactly


```r
sum(is.na(asthma_mortality$mortality_rate))
```

```
## [1] 264
```

Hm, 264/1122 that is not that bad, also it does look like they're states with limited populations, so that might be the reason is that there just weren't any fatal asthma incidents in that particular year.

I'm going to remove the missing values for our next step in this analysis


```r
asthma_mortality_no_miss <- asthma_mortality %>% 
  drop_na(mortality_rate) %>% 
  print()
```

```
## # A tibble: 858 × 6
##     year state       pop_est gender fatalities mortality_rate
##    <int> <chr>         <int> <fct>       <dbl>          <dbl>
##  1  2010 Alabama     4785514 Female         44           0.92
##  2  2010 Alabama     4785514 Male           17           0.36
##  3  2010 Arizona     6407342 Female         31           0.48
##  4  2010 Arizona     6407342 Male           27           0.42
##  5  2010 Arkansas    2921998 Female         29           0.99
##  6  2010 Arkansas    2921998 Male            8           0.27
##  7  2010 California 37319550 Female        251           0.67
##  8  2010 California 37319550 Male          152           0.41
##  9  2010 Colorado    5047539 Female         33           0.65
## 10  2010 Colorado    5047539 Male           15           0.3 
## # ℹ 848 more rows
```


### Create a Histogram of Fatal Asthma Incidents for Each Gender

I then decided to create a histogram to look at how these asthma fatalities are distributed.

I'm going to use ggplot2 to make a histogram comparing the distribution of mortality rates between the two populations


```r
ggplot(data = asthma_mortality_no_miss, aes(x = mortality_rate, fill = gender)) +
  geom_histogram(color = "black") +
  scale_fill_manual(values=c("pink", "blue")) +
  labs(title = "Asthma Mortality Rate by Gender in the USA from 2010 to 2020", 
       x = "Mortality Rate",
       y= "Number of Observations")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-11-1.png" width="672" />

It looks like there are some outliers, let me take a look at the data sorted by mortality rate descending


```r
asthma_mortality_no_miss[order(asthma_mortality_no_miss$mortality_rate, decreasing = TRUE),] 
```

```
## # A tibble: 858 × 6
##     year state        pop_est gender fatalities mortality_rate
##    <int> <chr>          <int> <fct>       <dbl>          <dbl>
##  1  2020 New Mexico    211839 Female         26          12.3 
##  2  2020 Pennsylvania 1299444 Female         97           7.46
##  3  2020 Illinois     1278658 Male           89           6.96
##  4  2020 Illinois     1278658 Female         88           6.88
##  5  2020 New Mexico    211839 Male           13           6.14
##  6  2020 Pennsylvania 1299444 Male           69           5.31
##  7  2014 Mississippi  2991892 Female         39           1.3 
##  8  2016 Mississippi  2990595 Female         38           1.27
##  9  2014 Oregon       3965447 Female         50           1.26
## 10  2014 Iowa         3110643 Female         38           1.22
## # ℹ 848 more rows
```

Wow, it looks like in 2020 there was a huge increase due to the pandemic most likely. I'm going to take a look at the data from 2010-2019 to get a closer look at the distribution.


```r
asthma_mortality_drop_2020 <- asthma_mortality_no_miss %>% 
  filter(year != 2020) %>% 
  print()
```

```
## # A tibble: 780 × 6
##     year state       pop_est gender fatalities mortality_rate
##    <int> <chr>         <int> <fct>       <dbl>          <dbl>
##  1  2010 Alabama     4785514 Female         44           0.92
##  2  2010 Alabama     4785514 Male           17           0.36
##  3  2010 Arizona     6407342 Female         31           0.48
##  4  2010 Arizona     6407342 Male           27           0.42
##  5  2010 Arkansas    2921998 Female         29           0.99
##  6  2010 Arkansas    2921998 Male            8           0.27
##  7  2010 California 37319550 Female        251           0.67
##  8  2010 California 37319550 Male          152           0.41
##  9  2010 Colorado    5047539 Female         33           0.65
## 10  2010 Colorado    5047539 Male           15           0.3 
## # ℹ 770 more rows
```

And repeat the histogram process with the 2010 to 2019 data

```r
ggplot(data = asthma_mortality_drop_2020, aes(x = mortality_rate, fill = gender)) +
  geom_histogram(color = "black") +
  scale_fill_manual(values=c("pink", "blue")) +
  labs(title = "Asthma Mortality Rate by Gender in the USA from 2010 to 2019", 
       x = "Mortality Rate",
       y= "Number of Observations")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-14-1.png" width="672" />

I want to compare the means of these populations to see if there is a statistically significant difference in fatal asthma incidence between the two genders throughout the whole USA.

### Statistical Testing

From the histogram, these don't really seem to fit a normal distribution, but rather a skewed distribution, but let's run the Shapiro-Wilk test to be sure instead of just eyeballing it and assuming.


```r
options(scipen = 999)
shapiro.test(asthma_mortality_no_miss$mortality_rate)
```

```
## 
## 	Shapiro-Wilk normality test
## 
## data:  asthma_mortality_no_miss$mortality_rate
## W = 0.27179, p-value < 0.00000000000000022
```

Okay, since the p value is not greater than 0.05, I can't use a t-test as that assumes normal distribution.

I'm going to go for a non-parametric test since we're not assuming any particular distribution here to test my hypothesis that when it comes to asthma fatalities females have a higher mortality rate than males.
Since I have two unpaired samples and I want to test how their values compare, I'm going to use the Mann-Whitney test.


```r
wilcox.test(asthma_mortality_no_miss$mortality_rate)
```

```
## 
## 	Wilcoxon signed rank test with continuity correction
## 
## data:  asthma_mortality_no_miss$mortality_rate
## V = 368511, p-value < 0.00000000000000022
## alternative hypothesis: true location is not equal to 0
```

With these results we can reject the null hypothesis, and say within this data there is a statistically significant difference between the two populations.

## Conclusion

Before this analysis I had no idea of the gender differences that arise in asthma incidents, upon looking into this further after completing this analysis I came across a [paper](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5629917/) that discusses the gender differences in asthma prevalence and severity on a biological level. 

[According to the Asthma and Allergy Network](https://allergyasthmanetwork.org/news/the-asthma-gender-gap/), In people under 18, it is more common for boys to have asthma than girls, but this prevalence switches when analyzing adult populations. The fact that women have a higher risk of death from asthma when compared to men is also confirmed by this source.
