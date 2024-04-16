---
title: HIV and Antiretroviral Therapy Across the Globe
author: R package build
date: '2024-04-16'
slug: gho-hiv
categories: ["R", "Public Health"]
tags: ["HIV", "Global Health"]
description: An exploration of the World Health Organization's Global Health Observatory Data
image: "images/red.jpeg"
math: ~
license: Michelle Gulotta
hidden: no
comments: no
---

For this post I wanted to check out other publicly available large data sets that have a package associated with it so that I wouldn't have to download it locally as I generally try to conserve space on my desktop and keep everything organized. I saw that the WHO Global Health Observatory had exactly what I was looking for and decided to explore some of the data that they had available.

## Load Libraries

```r
suppressPackageStartupMessages({
  library(tidyverse)
  library(rgho)
})
```

## View and Import Data

With this package there are vignettes available to search the different dimensions/data sets that can be pulled using the rgho package. I was able to access it by running this code.

```r
# Look at Dimensions
vignette("b-dimensions", "rgho")
# Look at GHO values
vignette("c-values-gho", "rgho")
```

As there are several data sets that need to be imported, I wrote a simple function to grab the data that I want, select the variables that I want, and filter for columns where region was present that way that I can see the totals per region, rather than per country.

```r
gho <- function(data, code = "CODE") {
  data <- get_gho_data(code = code) %>% 
    dplyr::select(REGION, YEAR, NumericValue) %>% 
    filter(!is.na(REGION))
}
```

And then I used this function to import the data.

```r
hiv <- gho(hiv, code = "HIV_0000000001") %>% 
  rename(hiv = NumericValue)
art <- gho(art, code = "HIV_0000000009") %>% 
  rename(art = NumericValue)
art_pct <- gho(art_pct, code = "HIV_ARTCOVERAGE") %>% 
  rename(art_pct = NumericValue)
region <- get_gho_values(dimension = "REGION") %>% 
  rename(REGION = Code,
         region = Title)
```

And then to merge the data sets.

```r
dfs <- list(hiv, art, art_pct)
df <- dfs %>% 
  reduce(full_join, by = c("REGION", "YEAR"))

dff <- full_join(df, region, by = "REGION")
```

As I made some variables lower case already, I use the tolower() function to standardize the case of all of the variables.

```r
dff <- dff %>% 
  dplyr::select(-REGION)
names(dff) <- tolower(names(dff))
```

The variables in the dataset represent the following information:
- hiv: number of people of all ages living with HIV
- art: number of people receiving ART
- art_pct: art therapy coverage among people living with HIV

## Visualization
### People Living With HIV


```r
ggplot(data = dff) +
  geom_smooth(mapping = aes(x = year, 
                            y = hiv,
                            group = region,
                            color = region)) +
  labs(title = "People Living with HIV by Region from 1990 to 2022",
       y = "Percent",
       x = "Year",
       color = "Region",
       caption = "Source: WHO Global Health Observatory") +
    theme(plot.title = element_text(hjust = 0.5))
```

```
## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
```

```
## Warning: Removed 34 rows containing non-finite outside the scale range
## (`stat_smooth()`).
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-7-1.png" width="672" />

### ART Coverage

```r
ggplot(data = dff) +
  geom_smooth(mapping = aes(x = year, 
                            y = art_pct,
                            group = region,
                            color = region)) +
  labs(title = "Antiretroviral Therapy Coverage Among People Living with HIV 
       by Region from 1990 to 2022",
       y = "Percent",
       x = "Year",
       color = "Region",
       caption = "Source: WHO Global Health Observatory") +
    theme(plot.title = element_text(hjust = 0.5))
```

```
## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-8-1.png" width="672" />

## ART Coverage in 2022

```r
dff_filtered <- dff %>% 
  filter(year == 2022)

ggplot(data = dff_filtered, aes(x = region, y = art_pct)) +
  geom_col(aes(fill = region)) +
  labs(title = "Antiretroviral Therapy Coverage Among People Living with HIV in 2022",
       y = "Percent",
       x = "Region",
       caption = "Source: WHO Global Health Observatory") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-9-1.png" width="672" />

## Conclusion

This was just a quick exploration of a small fraction of the data that is available through the rgho package, and it was useful to see how this works and how many different variables the Global Health Observatory tracks. 

In taking a look at this data and creating visualizations, I was able to see how far we have come in providing antiretroviral therapy to those who have received an HIV diagnosis, but also that there is still a lot of work to be done in making sure that everyone is covered by the life saving treatment. There is also a large disparity between the rest of the world and the eastern Mediterranean region (which generally covers the Middle East and North Africa), so there should be work done to break down barriers to ART access focused in this region.