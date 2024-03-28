---
title: Concentration of Heavy Metals in Blood
author: R package build
date: '2024-03-28'
slug: heavy-metal
categories: ["R", "Public Health"]
tags: ["Survey", "Heavy Metals"]
description: An analysis of 2017 to pre pandemic 2020 NHANES data to explore data related to demographics and blood concentration of heavy metals in blood
image: "images/metal.jpeg"
math: ~
license: Michelle Gulotta
hidden: no
comments: no
---

I revisited the NHANES 2017 to pre pandemic 2020 data, as there are so many variables to analyse. For this post, I wanted to analyze the labs that were performed on participants of the Mobile Examination Center portion of the exam, particularly the heavy metal labs. According to the article [Heavy Metals Toxicity and the Environment](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4144270/), "Because of their high degree of toxicity, arsenic, cadmium, chromium, lead, and mercury rank among the priority metals that are of public health significance. These metallic elements are considered systemic toxicants that are known to induce multiple organ damage, even at lower levels of exposure."

I wanted to take a look at not only the proportion of people who are at or above the detection limit, but if there are certain demographic variables that are correlated with higher amounts of heavy metals detected in the blood of participants. 

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

## Import Data

```r
demo <- nhanes("P_DEMO") %>% 
  dplyr::select(SEQN, RIAGENDR, RIDAGEYR, RIDEXPRG, RIDRETH3, SDMVPSU, SDMVSTRA, WTMECPRP)
lab <- nhanes("P_PBCD") %>% 
  dplyr::select(SEQN, LBXBPB, LBDBPBLC, LBXBCD, LBDBCDLC, LBXTHG, LBDTHGLC)
```

## Merge Data

```r
df <- merge(demo, 
            lab,
            by = c("SEQN"), 
            all.y = TRUE)
df$SEQN <- NULL
```

## Investigate and Clean Data

```r
init.table <- CreateTableOne(data = df, includeNA = TRUE)
print(init.table, showAllLevels = TRUE)
```

```
##                       
##                        level                                                             
##   n                                                                                      
##   RIAGENDR (%)         Male                                                              
##                        Female                                                            
##   RIDAGEYR (mean (SD))                                                                   
##   RIDEXPRG (%)         Yes, positive lab pregnancy test or self-reported pregnant at exam
##                        The participant was not pregnant at exam                          
##                        Cannot ascertain if the participant is pregnant at exam           
##                        <NA>                                                              
##   RIDRETH3 (%)         Mexican American                                                  
##                        Other Hispanic                                                    
##                        Non-Hispanic White                                                
##                        Non-Hispanic Black                                                
##                        Non-Hispanic Asian                                                
##                        Other Race - Including Multi-Racial                               
##   SDMVPSU (mean (SD))                                                                    
##   SDMVSTRA (mean (SD))                                                                   
##   WTMECPRP (mean (SD))                                                                   
##   LBXBPB (mean (SD))                                                                     
##   LBDBPBLC (%)         At or above the detection limit                                   
##                        Below lower detection limit                                       
##                        <NA>                                                              
##   LBXBCD (mean (SD))                                                                     
##   LBDBCDLC (%)         At or above the detection limit                                   
##                        Below lower detection limit                                       
##                        <NA>                                                              
##   LBXTHG (mean (SD))                                                                     
##   LBDTHGLC (%)         At or above the detection limit                                   
##                        Below lower detection limit                                       
##                        <NA>                                                              
##                       
##                        Overall            
##   n                       13772           
##   RIAGENDR (%)             6818 (49.5)    
##                            6954 (50.5)    
##   RIDAGEYR (mean (SD))    35.16 (24.73)   
##   RIDEXPRG (%)               87 ( 0.6)    
##                            1604 (11.6)    
##                              59 ( 0.4)    
##                           12022 (87.3)    
##   RIDRETH3 (%)             1763 (12.8)    
##                            1373 (10.0)    
##                            4565 (33.1)    
##                            3688 (26.8)    
##                            1483 (10.8)    
##                             900 ( 6.5)    
##   SDMVPSU (mean (SD))      1.54 (0.54)    
##   SDMVSTRA (mean (SD))   160.36 (6.95)    
##   WTMECPRP (mean (SD)) 23148.77 (27878.70)
##   LBXBPB (mean (SD))       1.03 (1.16)    
##   LBDBPBLC (%)            11103 (80.6)    
##                               4 ( 0.0)    
##                            2665 (19.4)    
##   LBXBCD (mean (SD))       0.36 (0.48)    
##   LBDBCDLC (%)             9720 (70.6)    
##                            2382 (17.3)    
##                            1670 (12.1)    
##   LBXTHG (mean (SD))       1.07 (2.06)    
##   LBDTHGLC (%)             8296 (60.2)    
##                            3806 (27.6)    
##                            1670 (12.1)
```

Here, I'm going to rename some variables to make the names more intuitive, as well as make the names of the categories more simple so that it is more readable when the output of the multivariate regression is printed.

```r
dff <- df %>% 
  mutate(
         preg = as.factor(case_when(
                          RIDEXPRG == "Yes, positive lab pregnancy test or self-reported pregnant at exam" ~ "Yes",
                          RIDEXPRG == "The participant was not pregnant at exam" ~ "No",
                          TRUE ~ NA)),
         race = as.factor(case_when(
                          RIDRETH3 == "Mexican American" | RIDRETH3 == "Other Hispanic" ~ "Hispanic",
                          RIDRETH3 == "Non-Hispanic White" ~ "White",
                          RIDRETH3 == "Non-Hispanic Black" ~"Black",
                          RIDRETH3 == "Non-Hispanic Asian" ~ "Asian",
                          RIDRETH3 == "Other Race - Including Multi-Racial" ~ "Other")),
         gender = as.factor(RIAGENDR)
  ) %>% 
  rename(
    age = RIDAGEYR,
    psu = SDMVPSU,
    strata = SDMVSTRA,
    weight = WTMECPRP,
    lead = LBXBPB,
    lead_lim =  LBDBPBLC,
    cadmium = LBXBCD,
    cadmium_lim = LBDBCDLC, 
    mercury = LBXTHG,
    mercury_lim = LBDTHGLC
  ) %>% 
  dplyr::select(-RIDEXPRG, -RIDRETH3, -RIAGENDR)
```

Checking again, making sure that I didn't miss anything.

```r
clean.table <- CreateTableOne(data = dff, includeNA = TRUE)
print(clean.table, showAllLevels = TRUE)
```

```
##                      
##                       level                           Overall            
##   n                                                      13772           
##   age (mean (SD))                                        35.16 (24.73)   
##   psu (mean (SD))                                         1.54 (0.54)    
##   strata (mean (SD))                                    160.36 (6.95)    
##   weight (mean (SD))                                  23148.77 (27878.70)
##   lead (mean (SD))                                        1.03 (1.16)    
##   lead_lim (%)        At or above the detection limit    11103 (80.6)    
##                       Below lower detection limit            4 ( 0.0)    
##                       <NA>                                2665 (19.4)    
##   cadmium (mean (SD))                                     0.36 (0.48)    
##   cadmium_lim (%)     At or above the detection limit     9720 (70.6)    
##                       Below lower detection limit         2382 (17.3)    
##                       <NA>                                1670 (12.1)    
##   mercury (mean (SD))                                     1.07 (2.06)    
##   mercury_lim (%)     At or above the detection limit     8296 (60.2)    
##                       Below lower detection limit         3806 (27.6)    
##                       <NA>                                1670 (12.1)    
##   preg (%)            No                                  1604 (11.6)    
##                       Yes                                   87 ( 0.6)    
##                       <NA>                               12081 (87.7)    
##   race (%)            Asian                               1483 (10.8)    
##                       Black                               3688 (26.8)    
##                       Hispanic                            3136 (22.8)    
##                       Other                                900 ( 6.5)    
##                       White                               4565 (33.1)    
##   gender (%)          Male                                6818 (49.5)    
##                       Female                              6954 (50.5)
```

## Survey Design

Creating the survey design object here.

```r
metal.svy <- svydesign(id = ~psu,
                       strata = ~strata,
                       weights = ~weight,
                       nest = TRUE,
                       survey.lonely.psu = "adjust",
                       data = dff)
```

## Proportions

I want to use the variables that show if the participant was at or above the detection limit to take a look at the weighted proportions of the survey participants blood heavy metal concentrations, which reflects the population as a whole.

```r
svymean(~lead_lim, metal.svy, na.rm = TRUE)
```

```
##                                               mean    SE
## lead_limAt or above the detection limit 0.99970726 2e-04
## lead_limBelow lower detection limit     0.00029274 2e-04
```

```r
svymean(~mercury_lim, metal.svy, na.rm = TRUE)
```

```
##                                               mean    SE
## mercury_limAt or above the detection limit 0.72948 0.014
## mercury_limBelow lower detection limit     0.27052 0.014
```

```r
svymean(~cadmium_lim, metal.svy, na.rm = TRUE)
```

```
##                                              mean     SE
## cadmium_limAt or above the detection limit 0.8448 0.0071
## cadmium_limBelow lower detection limit     0.1552 0.0071
```
Wow, almost everyone is at or above the detection limit for lead, and a large majority for the other heavy metals.

## Fit Multivariate Regressions

First I want to take a subset of the survey design object to remove missing values for lab values for any of the heavy metals.

```r
metal.subset <- subset(metal.svy,
                       !is.na(lead)
                       & !is.na(cadmium)
                       & !is.na(mercury))
```

Then, to fit the model using svyglm() with the subset of the survey design object.

```r
lead.fit <- svyglm(lead ~ gender + age + race,
                   metal.subset)
mercury.fit <- svyglm(mercury ~ gender + age + race, 
                      metal.subset)
cadmium.fit <- svyglm(cadmium ~ gender + age + race,
                      metal.subset)
```

### Diagnostics

I'm going to take a quick look at the diagnostic plots for each model, to make sure that there is a linear relationship. I'm going to start with the lead model.

```r
par(mfrow = c(2, 2))
plot(lead.fit)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-11-1.png" width="672" />

And then mercury.

```r
par(mfrow = c(2, 2))
plot(mercury.fit)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-12-1.png" width="672" />

And lastly, cadmium. 

```r
par(mfrow = c(2, 2))
plot(cadmium.fit)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-13-1.png" width="672" />

Most of the plots look good, there are departures from the normal distribution, but overall the relationship is linear. 
Now to take a look at the coefficients of the models.

### Signifigant Coefficients 

For lead,

```r
summary(lead.fit)
```

```
## 
## Call:
## svyglm(formula = lead ~ gender + age + race, design = metal.subset)
## 
## Survey design:
## subset(metal.svy, !is.na(lead) & !is.na(cadmium) & !is.na(mercury))
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   0.750177   0.058333  12.860 7.98e-11 ***
## genderFemale -0.268034   0.025504 -10.509 2.35e-09 ***
## age           0.016640   0.000466  35.705  < 2e-16 ***
## raceBlack    -0.242977   0.054023  -4.498 0.000246 ***
## raceHispanic -0.308710   0.067818  -4.552 0.000218 ***
## raceOther    -0.327197   0.062801  -5.210 4.98e-05 ***
## raceWhite    -0.395893   0.058607  -6.755 1.88e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.9845273)
## 
## Number of Fisher Scoring iterations: 2
```
All significant statistically, some that stand out to me are that Non-Hispanic white is associated with the largest decrease when compared to the intercept which is Asian. Another is that the only positive coefficient is age although it is quite small.
Female is also lower than male, which is interesting as you would think they would be the same from household exposures as men and women live together, perhaps there is another place that men were more frequently exposed to lead.

And mercury,

```r
summary(mercury.fit)
```

```
## 
## Call:
## svyglm(formula = mercury ~ gender + age + race, design = metal.subset)
## 
## Survey design:
## subset(metal.svy, !is.na(lead) & !is.na(cadmium) & !is.na(mercury))
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   2.127359   0.300128   7.088 9.63e-07 ***
## genderFemale -0.172979   0.047452  -3.645  0.00172 ** 
## age           0.015134   0.001399  10.819 1.46e-09 ***
## raceBlack    -1.532638   0.298384  -5.136 5.87e-05 ***
## raceHispanic -1.582814   0.293034  -5.401 3.27e-05 ***
## raceOther    -1.639871   0.297172  -5.518 2.53e-05 ***
## raceWhite    -1.597285   0.287334  -5.559 2.32e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 3.284387)
## 
## Number of Fisher Scoring iterations: 2
```
it seems to follow the same patterns as lead.

And cadmium,

```r
summary(cadmium.fit)
```

```
## 
## Call:
## svyglm(formula = cadmium ~ gender + age + race, design = metal.subset)
## 
## Survey design:
## subset(metal.svy, !is.na(lead) & !is.na(cadmium) & !is.na(mercury))
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   0.2393745  0.0270553   8.848 3.64e-08 ***
## genderFemale  0.0748863  0.0116488   6.429 3.66e-06 ***
## age           0.0044088  0.0003573  12.338 1.62e-10 ***
## raceBlack    -0.0203956  0.0282558  -0.722  0.47920    
## raceHispanic -0.1487407  0.0241972  -6.147 6.58e-06 ***
## raceOther     0.0074420  0.0360248   0.207  0.83854    
## raceWhite    -0.0868630  0.0281021  -3.091  0.00601 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.2304773)
## 
## Number of Fisher Scoring iterations: 2
```
it definitely deviates from the pattern seen in the other two model's coefficients: Black and Other Race are not statistically significant coefficients, and surprisingly in this model Female has a positive coefficient instead of negative. I want to dig deeper into these gender differences. 

## Gender and Heavy Metal Concentration in Blood

First I want to visualize the mean concentration for each type of heavy metal, in the two different gender groups.

```r
par(mfrow = c(1, 3),
    xpd = NA)
svyboxplot(lead ~ gender, 
           metal.subset,
           ylim = c(0, 3),
           xaxt = "n",
           ylab = "Blood Lead (ug/dL)",
           col = c("#235347", "#8EB69B"))
svyboxplot(mercury ~ gender, 
           metal.subset,
           ylim = c(0, 4),
           xaxt = "n",
           ylab = "Blood Mercury (ug/L)",
           col = c("#235347", "#8EB69B"))
svyboxplot(cadmium ~ gender, 
           metal.subset,
           ylim = c(0, 2),
           xaxt = "n",
           ylab = "Blood Cadmium (ug/L)",
           col = c("#235347", "#8EB69B"),
           bty = "L")
legend("bottomleft", 
       legend = c("Male", "Female"),
       fill = c("#235347", "#8EB69B"),
       horiz = TRUE,
       inset = c(-1.5, -0.15),
       cex = 1.2)
mtext("Heavy Metal Concentration in Blood by Gender", 
      side = 3, 
      line = -2.5,
      outer = TRUE)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-17-1.png" width="672" />

So there are some visible differences in the lead and cadmium group, and not as much within the mercury group. However, statistical significance can't be determined just by looking at the graph.

### Wilcoxon Signed Rank Test

I'm going to use the Wilcoxon Signed Rank test rather than the parametric t-test as the data showed a deviation from the normal distribution in the q-q plot.

```r
svyranktest(lead ~ gender, metal.subset, test = c("wilcoxon"))
```

```
## 
## 	Design-based KruskalWallis test
## 
## data:  lead ~ gender
## t = -10.78, df = 24, p-value = 1.111e-10
## alternative hypothesis: true difference in mean rank score is not equal to 0
## sample estimates:
## difference in mean rank score 
##                   -0.08826645
```

```r
svyranktest(cadmium ~ gender, metal.subset, test = c("wilcoxon"))
```

```
## 
## 	Design-based KruskalWallis test
## 
## data:  cadmium ~ gender
## t = 12.499, df = 24, p-value = 5.349e-12
## alternative hypothesis: true difference in mean rank score is not equal to 0
## sample estimates:
## difference in mean rank score 
##                     0.1000802
```

```r
svyranktest(mercury ~ gender, metal.subset, test = c("wilcoxon"))
```

```
## 
## 	Design-based KruskalWallis test
## 
## data:  mercury ~ gender
## t = -0.91282, df = 24, p-value = 0.3704
## alternative hypothesis: true difference in mean rank score is not equal to 0
## sample estimates:
## difference in mean rank score 
##                  -0.006822827
```

This test confirms my suspicions from the graph, that lead and cadmium do have statistically significant differences in the means between men and women.

## Conclusion

Looking into this aspect of the NHANES survey was an interesting exploratory analysis that raised some questions for me regarding gender disparities in heavy metal exposure. 

Exposure to these heavy metals can be linked to adverse health outcomes and it is concerning that such a large marjority of the population has detectable exposure in their blood.
