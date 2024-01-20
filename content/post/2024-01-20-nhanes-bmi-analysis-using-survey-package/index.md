---
title: NHANES BMI Analysis Using Survey Package
author: R package build
date: '2024-01-20'
slug: nhanes-bmi-analysis-using-survey-package
categories: ["R", "Public Health"]
tags: []
description: An analysis of BMI using the CDC NHANES Data in conjunction with the Survey package to account for complex survey design
image: "images/green.jpeg"
math: ~
license: Michelle Gulotta
hidden: no
comments: no
---

One thing that I’ve learned since I’ve been looking into working with large complex survey data like the CDC’s NHANES is about survey weights and how to use them in your analysis. In this analysis I tried to use the survey package in R to use the survey design to analyze the data, as there are certain demographics that were oversampled and undersampled as a part of the survey design.

## Load Packages

``` r
suppressPackageStartupMessages({
library(tidyverse)
library(nhanesA)
library(janitor)
library(survey)
library(gtsummary)
})
```

## Load Data

``` r
demo <- nhanes("P_DEMO")
demo <- nhanesTranslate("P_DEMO", names(demo), data = demo)
```

    ## Translated columns: RIDSTATR RIAGENDR RIDRETH1 RIDRETH3 RIDEXMON DMDBORN4 DMDYRUSZ DMDEDUC2 DMDMARTZ RIDEXPRG SIALANG SIAPROXY SIAINTRP FIALANG FIAPROXY FIAINTRP MIALANG MIAPROXY MIAINTRP AIALANGA

``` r
exam <- nhanes("P_BMX")
exam <- nhanesTranslate("P_BMX", names(exam), data = exam)
```

    ## Translated columns: BMDSTATS BMIWT BMIHT BMDBMIC

## Retain Useful Variables

``` r
demo_select <- demo %>% 
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH3, SDMVPSU, SDMVSTRA, WTMECPRP)

exam_select <- exam %>% 
  select(SEQN, BMXBMI)
```

## Merge Data

``` r
merged_data <- merge(demo_select, exam_select,
                     by = c("SEQN"), all = TRUE)
merged_data$SEQN <- NULL
```

## Clean Dataset to Make Analysis Easier

First I wanted to see which race categories there are so that I can capture all of them when I recode them into more simple categories.

``` r
merged_data %>% tabyl(RIDRETH3)
```

    ##                             RIDRETH3    n    percent
    ##                     Mexican American 1990 0.12789203
    ##                       Other Hispanic 1544 0.09922879
    ##                   Non-Hispanic White 5271 0.33875321
    ##                   Non-Hispanic Black 4098 0.26336761
    ##                   Non-Hispanic Asian 1638 0.10526992
    ##  Other Race - Including Multi-Racial 1019 0.06548843

Now, I’m going to rename some variables, and use case_when() to make the race_cat and bmi_cat columns, as well as factoring these columns so that they can be used in my analysis and are not just recognized as character strings by R.

I also added at the end to output a tibble of the raw NHANES data without the weights to print out how my clean data looks and make sure that I didn’t miss any columns in the renaming process.

``` r
nhanes <- merged_data %>% 
  rename(
    gender = RIAGENDR,
    age = RIDAGEYR,
    bmi = BMXBMI,
    psu = SDMVPSU,
    strata = SDMVSTRA,
    weight = WTMECPRP) %>% 
  mutate(
    race_cat = case_when(RIDRETH3 == "Mexican American" | RIDRETH3 == "Other Hispanic" ~ "Hispanic",
                         RIDRETH3 == "Non-Hispanic White" ~ "White",
                         RIDRETH3 == "Non-Hispanic Black" ~ "Black",
                         RIDRETH3 == "Non-Hispanic Asian" ~ "Asian",
                         RIDRETH3 == "Other Race - Including Multi-Racial" ~ "Other"),
    bmi_cat = case_when(bmi < 18.5 ~ "Underweight",
                        bmi < 25 ~ "Normal",
                        bmi < 30 ~ "Overweight",
                        bmi >= 30 ~ "Obese")
  ) %>% 
  filter(!is.na(bmi)) %>% 
  mutate_if(., is.character, as.factor) %>% 
  select(-RIDRETH3)

# Unweighted 
nhanes_tibble <- as_tibble(nhanes) %>% 
  filter(age >= 18) %>%
  select(-psu, -strata, -weight) %>% 
  print()
```

    ## # A tibble: 8,790 × 5
    ##    gender   age   bmi race_cat bmi_cat   
    ##    <fct>  <dbl> <dbl> <fct>    <fct>     
    ##  1 Female    29  37.8 Asian    Obese     
    ##  2 Male      49  29.7 White    Overweight
    ##  3 Male      36  21.9 White    Normal    
    ##  4 Male      68  30.2 Other    Obese     
    ##  5 Male      76  26.6 White    Overweight
    ##  6 Female    44  39.1 Hispanic Obese     
    ##  7 Female    33  28.9 Asian    Overweight
    ##  8 Female    68  28.1 Black    Overweight
    ##  9 Female    42  31.3 Asian    Obese     
    ## 10 Male      58  30.5 Hispanic Obese     
    ## # ℹ 8,780 more rows

### Refactor Order of BMI Categories

R shows factored variables autmatically in alphabetical order, but this did not make much sense for my chart as BMI categories make more sense from underweight to obese, so I used the factor() function to remedy this before I make my table.

``` r
nhanes$bmi_cat <- factor(nhanes$bmi_cat, 
                         levels = c("Underweight", "Normal", "Overweight", "Obese"))
```

## Survey Design

I learned that in order to properly analyze large survey data, and to make it representative of the population at large, you have to create a survey design object and use that in your analysis.

``` r
nhanes_design <- svydesign(id = ~psu,
                           strata = ~strata,
                           weights = ~weight,
                           nest = TRUE,
                           data = nhanes)
```

### Apply Eligibility Criteria

It is also important that the data is subset in this manner when working with survey data.

``` r
nhanes_adult <- subset(nhanes_design, age >= 18)
```

## Summary Statistics

### BMI Categories by Race/Hispanic Origin, Gender, and Average Age Weighted

``` r
tbl_svysummary(nhanes_adult,
               by = bmi_cat,
               include = c(gender, age, race_cat),
               label = list(gender ~ "Gender",
                            age ~ "Age",
                            race_cat ~ "Race/Hispanic Origin"),
               statistic = list(age ~ "{mean} ({sd})")
               )
```

<div id="vbpxxhnths" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#vbpxxhnths table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#vbpxxhnths thead, #vbpxxhnths tbody, #vbpxxhnths tfoot, #vbpxxhnths tr, #vbpxxhnths td, #vbpxxhnths th {
  border-style: none;
}
&#10;#vbpxxhnths p {
  margin: 0;
  padding: 0;
}
&#10;#vbpxxhnths .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#vbpxxhnths .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#vbpxxhnths .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#vbpxxhnths .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#vbpxxhnths .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#vbpxxhnths .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#vbpxxhnths .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#vbpxxhnths .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#vbpxxhnths .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#vbpxxhnths .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#vbpxxhnths .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#vbpxxhnths .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#vbpxxhnths .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#vbpxxhnths .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#vbpxxhnths .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#vbpxxhnths .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#vbpxxhnths .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#vbpxxhnths .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#vbpxxhnths .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#vbpxxhnths .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#vbpxxhnths .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#vbpxxhnths .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#vbpxxhnths .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#vbpxxhnths .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#vbpxxhnths .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#vbpxxhnths .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#vbpxxhnths .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#vbpxxhnths .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#vbpxxhnths .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#vbpxxhnths .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#vbpxxhnths .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#vbpxxhnths .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#vbpxxhnths .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#vbpxxhnths .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#vbpxxhnths .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#vbpxxhnths .gt_left {
  text-align: left;
}
&#10;#vbpxxhnths .gt_center {
  text-align: center;
}
&#10;#vbpxxhnths .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#vbpxxhnths .gt_font_normal {
  font-weight: normal;
}
&#10;#vbpxxhnths .gt_font_bold {
  font-weight: bold;
}
&#10;#vbpxxhnths .gt_font_italic {
  font-style: italic;
}
&#10;#vbpxxhnths .gt_super {
  font-size: 65%;
}
&#10;#vbpxxhnths .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#vbpxxhnths .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#vbpxxhnths .gt_indent_1 {
  text-indent: 5px;
}
&#10;#vbpxxhnths .gt_indent_2 {
  text-indent: 10px;
}
&#10;#vbpxxhnths .gt_indent_3 {
  text-indent: 15px;
}
&#10;#vbpxxhnths .gt_indent_4 {
  text-indent: 20px;
}
&#10;#vbpxxhnths .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Underweight&lt;/strong&gt;, N = 3,797,884&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>Underweight</strong>, N = 3,797,884<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Normal&lt;/strong&gt;, N = 62,452,871&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>Normal</strong>, N = 62,452,871<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Overweight&lt;/strong&gt;, N = 76,984,839&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>Overweight</strong>, N = 76,984,839<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Obese&lt;/strong&gt;, N = 101,342,006&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>Obese</strong>, N = 101,342,006<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Gender</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Male</td>
<td headers="stat_1" class="gt_row gt_center">1,148,289 (30%)</td>
<td headers="stat_2" class="gt_row gt_center">27,234,973 (44%)</td>
<td headers="stat_3" class="gt_row gt_center">41,256,750 (54%)</td>
<td headers="stat_4" class="gt_row gt_center">48,289,482 (48%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Female</td>
<td headers="stat_1" class="gt_row gt_center">2,649,595 (70%)</td>
<td headers="stat_2" class="gt_row gt_center">35,217,899 (56%)</td>
<td headers="stat_3" class="gt_row gt_center">35,728,089 (46%)</td>
<td headers="stat_4" class="gt_row gt_center">53,052,524 (52%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Age</td>
<td headers="stat_1" class="gt_row gt_center">37 (17)</td>
<td headers="stat_2" class="gt_row gt_center">44 (19)</td>
<td headers="stat_3" class="gt_row gt_center">50 (17)</td>
<td headers="stat_4" class="gt_row gt_center">48 (17)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Race/Hispanic Origin</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Asian</td>
<td headers="stat_1" class="gt_row gt_center">385,874 (10%)</td>
<td headers="stat_2" class="gt_row gt_center">6,348,964 (10%)</td>
<td headers="stat_3" class="gt_row gt_center">5,473,194 (7.1%)</td>
<td headers="stat_4" class="gt_row gt_center">2,321,160 (2.3%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Black</td>
<td headers="stat_1" class="gt_row gt_center">613,071 (16%)</td>
<td headers="stat_2" class="gt_row gt_center">6,036,949 (9.7%)</td>
<td headers="stat_3" class="gt_row gt_center">7,381,673 (9.6%)</td>
<td headers="stat_4" class="gt_row gt_center">13,643,866 (13%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Hispanic</td>
<td headers="stat_1" class="gt_row gt_center">440,910 (12%)</td>
<td headers="stat_2" class="gt_row gt_center">7,245,902 (12%)</td>
<td headers="stat_3" class="gt_row gt_center">14,181,024 (18%)</td>
<td headers="stat_4" class="gt_row gt_center">17,625,000 (17%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Other</td>
<td headers="stat_1" class="gt_row gt_center">106,290 (2.8%)</td>
<td headers="stat_2" class="gt_row gt_center">2,276,951 (3.6%)</td>
<td headers="stat_3" class="gt_row gt_center">2,692,021 (3.5%)</td>
<td headers="stat_4" class="gt_row gt_center">4,801,387 (4.7%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    White</td>
<td headers="stat_1" class="gt_row gt_center">2,251,739 (59%)</td>
<td headers="stat_2" class="gt_row gt_center">40,544,106 (65%)</td>
<td headers="stat_3" class="gt_row gt_center">47,256,927 (61%)</td>
<td headers="stat_4" class="gt_row gt_center">62,950,593 (62%)</td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="5"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> n (%); Mean (SD)</td>
    </tr>
  </tfoot>
</table>
</div>

### BMI Categories by Race/Hispanic Origin, Gender, and Average Age Unweighted

``` r
tbl_summary(nhanes_tibble,
               by = bmi_cat,
               include = c(gender, age, race_cat),
               label = list(gender ~ "Gender",
                            age ~ "Age",
                            race_cat ~ "Race/Hispanic Origin"),
               statistic = list(age ~ "{mean} ({sd})")
               )
```

<div id="wbodqqisyf" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#wbodqqisyf table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#wbodqqisyf thead, #wbodqqisyf tbody, #wbodqqisyf tfoot, #wbodqqisyf tr, #wbodqqisyf td, #wbodqqisyf th {
  border-style: none;
}
&#10;#wbodqqisyf p {
  margin: 0;
  padding: 0;
}
&#10;#wbodqqisyf .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#wbodqqisyf .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#wbodqqisyf .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#wbodqqisyf .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#wbodqqisyf .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#wbodqqisyf .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#wbodqqisyf .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#wbodqqisyf .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#wbodqqisyf .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#wbodqqisyf .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#wbodqqisyf .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#wbodqqisyf .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#wbodqqisyf .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#wbodqqisyf .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#wbodqqisyf .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#wbodqqisyf .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#wbodqqisyf .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#wbodqqisyf .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#wbodqqisyf .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#wbodqqisyf .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#wbodqqisyf .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#wbodqqisyf .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#wbodqqisyf .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#wbodqqisyf .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#wbodqqisyf .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#wbodqqisyf .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#wbodqqisyf .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#wbodqqisyf .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#wbodqqisyf .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#wbodqqisyf .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#wbodqqisyf .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#wbodqqisyf .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#wbodqqisyf .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#wbodqqisyf .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#wbodqqisyf .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#wbodqqisyf .gt_left {
  text-align: left;
}
&#10;#wbodqqisyf .gt_center {
  text-align: center;
}
&#10;#wbodqqisyf .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#wbodqqisyf .gt_font_normal {
  font-weight: normal;
}
&#10;#wbodqqisyf .gt_font_bold {
  font-weight: bold;
}
&#10;#wbodqqisyf .gt_font_italic {
  font-style: italic;
}
&#10;#wbodqqisyf .gt_super {
  font-size: 65%;
}
&#10;#wbodqqisyf .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#wbodqqisyf .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#wbodqqisyf .gt_indent_1 {
  text-indent: 5px;
}
&#10;#wbodqqisyf .gt_indent_2 {
  text-indent: 10px;
}
&#10;#wbodqqisyf .gt_indent_3 {
  text-indent: 15px;
}
&#10;#wbodqqisyf .gt_indent_4 {
  text-indent: 20px;
}
&#10;#wbodqqisyf .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Normal&lt;/strong&gt;, N = 2,185&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>Normal</strong>, N = 2,185<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Obese&lt;/strong&gt;, N = 3,688&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>Obese</strong>, N = 3,688<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Overweight&lt;/strong&gt;, N = 2,767&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>Overweight</strong>, N = 2,767<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Underweight&lt;/strong&gt;, N = 150&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>Underweight</strong>, N = 150<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Gender</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Male</td>
<td headers="stat_1" class="gt_row gt_center">1,044 (48%)</td>
<td headers="stat_2" class="gt_row gt_center">1,644 (45%)</td>
<td headers="stat_3" class="gt_row gt_center">1,521 (55%)</td>
<td headers="stat_4" class="gt_row gt_center">62 (41%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Female</td>
<td headers="stat_1" class="gt_row gt_center">1,141 (52%)</td>
<td headers="stat_2" class="gt_row gt_center">2,044 (55%)</td>
<td headers="stat_3" class="gt_row gt_center">1,246 (45%)</td>
<td headers="stat_4" class="gt_row gt_center">88 (59%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Age</td>
<td headers="stat_1" class="gt_row gt_center">46 (20)</td>
<td headers="stat_2" class="gt_row gt_center">50 (17)</td>
<td headers="stat_3" class="gt_row gt_center">52 (18)</td>
<td headers="stat_4" class="gt_row gt_center">39 (20)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Race/Hispanic Origin</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Asian</td>
<td headers="stat_1" class="gt_row gt_center">478 (22%)</td>
<td headers="stat_2" class="gt_row gt_center">163 (4.4%)</td>
<td headers="stat_3" class="gt_row gt_center">403 (15%)</td>
<td headers="stat_4" class="gt_row gt_center">27 (18%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Black</td>
<td headers="stat_1" class="gt_row gt_center">494 (23%)</td>
<td headers="stat_2" class="gt_row gt_center">1,175 (32%)</td>
<td headers="stat_3" class="gt_row gt_center">609 (22%)</td>
<td headers="stat_4" class="gt_row gt_center">48 (32%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Hispanic</td>
<td headers="stat_1" class="gt_row gt_center">341 (16%)</td>
<td headers="stat_2" class="gt_row gt_center">879 (24%)</td>
<td headers="stat_3" class="gt_row gt_center">701 (25%)</td>
<td headers="stat_4" class="gt_row gt_center">19 (13%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Other</td>
<td headers="stat_1" class="gt_row gt_center">110 (5.0%)</td>
<td headers="stat_2" class="gt_row gt_center">198 (5.4%)</td>
<td headers="stat_3" class="gt_row gt_center">113 (4.1%)</td>
<td headers="stat_4" class="gt_row gt_center">6 (4.0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    White</td>
<td headers="stat_1" class="gt_row gt_center">762 (35%)</td>
<td headers="stat_2" class="gt_row gt_center">1,273 (35%)</td>
<td headers="stat_3" class="gt_row gt_center">941 (34%)</td>
<td headers="stat_4" class="gt_row gt_center">50 (33%)</td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="5"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> n (%); Mean (SD)</td>
    </tr>
  </tfoot>
</table>
</div>

## Gender vs BMI T-Test

### Normality Assumption

``` r
svyqqmath(~bmi, design = nhanes_adult)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-12-1.png" width="672" />

This deviates from the normal distribution, so I applied a log transformation to the BMI variable to remedy this deviation from the normal distribution at the tail end of the data.

``` r
svyqqmath(~log10(bmi), design = nhanes_adult)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-13-1.png" width="672" />
That looks a lot better, now time to visualize the data and run the analysis.

### Boxplot Visualization

``` r
svyboxplot(log10(bmi) ~ gender, 
           design = nhanes_adult,
           xlab = "Gender",
           main = "Weighted Boxplot of Mean Log Transformed BMI by Gender",
           ylab = "Log Transformed BMI")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-14-1.png" width="672" />

### T-Test

``` r
svyttest(log10(bmi) ~ gender, nhanes_adult)
```

    ## 
    ## 	Design-based t-test
    ## 
    ## data:  log10(bmi) ~ gender
    ## t = 0.46451, df = 24, p-value = 0.6465
    ## alternative hypothesis: true difference in mean is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.005848037  0.009244940
    ## sample estimates:
    ## difference in mean 
    ##        0.001698451

## Race/Hispanic Origin vs. BMI ANOVA

``` r
svyboxplot(log10(bmi) ~ race_cat, 
           design = nhanes_adult,
           xlab = "Race/Hispanic Origin",
           ylab = "Log Transformed BMI",
           main = "Weighted Boxplot of Mean Log Transformed BMI by 
           Race/Hispanic Origin")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-16-1.png" width="672" />
In visually inspecting the boxplot, it looks like there is a difference between the various race/hispanic origin groups and BMI, but lets see.

### Fit Model

``` r
race.bmi.glm <- svyglm(log10(bmi) ~ race_cat, nhanes_adult)
```

### ANOVA

A barrier that I ran into was that the survey package in R does not seen to have an ANOVA function, and when you run the survey GLM through the regular ANOVA function it does not work as I thought it would. I did some researching around and I found that this is a way to assess this for complex surveys using a Wald test.

``` r
options(scipen = 999)
regTermTest(race.bmi.glm, ~race_cat)
```

    ## Wald test for race_cat
    ##  in svyglm(formula = log10(bmi) ~ race_cat, design = nhanes_adult)
    ## F =  184.1178  on  4  and  21  df: p= 0.00000000000000050064

So it looks like there is a statistically significant difference between the various Race/Hispanic Origin categories and BMI, as hypothesized by the boxplot.

## Conclusion

While the NHANES dataset is still very useful, I did have to dig a lot deeper and do more research to find out how I can properly use this data.

As I am also working through another book on statistics, I found out that there is not just parametric statistics if your data is normal, and non parmetric statistics if it isn’t, but that you’re allowed to transform your data to help fit better into the normal distribution so that you can use parametric statistics like T-Tests with more confidence.
