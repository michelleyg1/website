---
title: NYC Open Data Motor Vehicle Collision Injury Outcome Classification
author: R package build
date: '2024-02-12'
slug: accident-classification
categories: ["R", "Public Health"]
tags: []
description: Using a portion of NYC Open Data's Motor Vehicle Collision data to create a model to predict if any person involved in an accident will sustain injuries
image: "images/bridge.jpeg"
math: ~
license: Michelle Gulotta
hidden: no
comments: no
---

## Load Libraries


```r
suppressPackageStartupMessages({
library(tidyverse)
library(janitor)
library(lubridate)
library(e1071)
})
```


## Load and Explore Data

I found this huge and interesting dataset on [NYC's Open Data portal](https://data.cityofnewyork.us/Public-Safety/Motor-Vehicle-Collisions-Crashes/h9gi-nx95/about_data) and wanted to use it to practice with the new knowledge and skills that I learned recently. My goal was to create a classifier model that can predict if motor vehicle collisions would result in injuries or not to anyone involved in the accident based on several predictors. 

I saw there was a way to query the API directly and get the data into R without downloading it as a csv, and I did figure out how to do that for other open datasets that were smaller, but I had trouble with this large dataset. I ended up using the NYC Open Data's query tool to filter the dataset beforehand and downloaded it from there. I chose the borough of Queens to work with as there are many more highways and potential for accidents resulting in injuries there, rather than Manhattan where vehicles move very slowly through the grid shaped streets. I also filtered it for the calendar year of 2023 to further reduce the number of observations.

```r
motor_raw <- read.csv("/Users/michellegulotta/Desktop/motor_raw.csv")
motor_raw <- motor_raw %>% 
  janitor::clean_names(., "snake")
```

I wanted to also minimize missing values to deal with, so for the sake of simplicity I only focused on the data in the first columns for contributing factor and vehicle type.

```r
motor_cat <- motor_raw %>% 
  dplyr::select(crash_date, crash_time, collision_id, number_of_persons_injured, number_of_persons_killed, contributing_factor_vehicle_1, vehicle_type_code_1)
```

Now to check out the variables remaining along with the data types associated with each column.

```r
str(motor_cat)
```

```
## 'data.frame':	17822 obs. of  7 variables:
##  $ crash_date                   : chr  "11/16/2023" "11/17/2023" "11/17/2023" "11/12/2023" ...
##  $ crash_time                   : chr  "15:45" "6:57" "14:30" "10:58" ...
##  $ collision_id                 : int  4679634 4679868 4679994 4679991 4663636 4663437 4629913 4631543 4631801 4632069 ...
##  $ number_of_persons_injured    : int  1 0 0 0 0 0 0 4 0 1 ...
##  $ number_of_persons_killed     : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ contributing_factor_vehicle_1: chr  "Driver Inexperience" "Other Vehicular" "Unspecified" "Unspecified" ...
##  $ vehicle_type_code_1          : chr  "Motorcycle" "Sedan" "Station Wagon/Sport Utility Vehicle" "Station Wagon/Sport Utility Vehicle" ...
```

## Clean and Manipulate Data

Time is stored as character and is in the 24 hour format, I just want the hour as I am going to use that to create a time of day category to use as a predictor. I also made a month category using the lubridate package, but I did not end up using this as a predictor, it was interesting to see how the lubridate package can help me in future projects.

```r
motor_cat <- motor_cat %>% 
  mutate(hour = substr(motor_cat$crash_time, 
                       start = 1, 
                       stop = 2),
         across(
           .cols = c(contributing_factor_vehicle_1, vehicle_type_code_1),
           .fns = ~if_else(.x == "", NA, .x)
         ),
         month = as.Date(crash_date, format = "%m/%d/%Y")) %>% 
  rename(type = vehicle_type_code_1,
         contrib = contributing_factor_vehicle_1)


motor_cat$hour <- str_replace_all(motor_cat$hour, ":", "")
motor_cat$hour <- as.integer(motor_cat$hour)
motor_cat$month <- month(ymd(motor_cat$month))
```

I'm going to come back to this later, I want to do all of the category creating at once so I'm going to work on the contributing factor and vehicle type variables. In the previous code I renamed these variables to make the name shorter as I'm going to be renaming a lot of stuff later as you'll see.

```r
motor_cat %>% tabyl(contrib)
```

```
##                                                contrib    n      percent
##                                  Accelerator Defective   13 7.294355e-04
##                           Aggressive Driving/Road Rage  145 8.136012e-03
##                                    Alcohol Involvement  408 2.289305e-02
##                                         Animals Action   14 7.855460e-04
##                                       Backing Unsafely  691 3.877230e-02
##                                       Brakes Defective   69 3.871619e-03
##                                 Cell Phone (hand-Held)   11 6.172147e-04
##                                Cell Phone (hands-free)    1 5.611043e-05
##                         Driver Inattention/Distraction 4204 2.358882e-01
##                                    Driver Inexperience  372 2.087308e-02
##                             Driverless/Runaway Vehicle   24 1.346650e-03
##                                        Drugs (illegal)   11 6.172147e-04
##                                     Eating or Drinking    3 1.683313e-04
##                                  Failure to Keep Right   35 1.963865e-03
##                          Failure to Yield Right-of-Way 1830 1.026821e-01
##                                        Fatigued/Drowsy   27 1.514981e-03
##                                            Fell Asleep  117 6.564920e-03
##                                  Following Too Closely  988 5.543710e-02
##                                                  Glare   20 1.122209e-03
##                                   Headlights Defective    1 5.611043e-05
##                                                 Illnes   36 2.019975e-03
##                       Lane Marking Improper/Inadequate    7 3.927730e-04
##                                     Lost Consciousness   50 2.805521e-03
##                                     Obstruction/Debris   27 1.514981e-03
##                                Other Electronic Device    3 1.683313e-04
##                                 Other Lighting Defects    1 5.611043e-05
##                                        Other Vehicular  362 2.031197e-02
##                                Outside Car Distraction   47 2.637190e-03
##                                      Oversized Vehicle   33 1.851644e-03
##                                  Passenger Distraction   34 1.907754e-03
##                                    Passing Too Closely  656 3.680844e-02
##                         Passing or Lane Usage Improper  959 5.380990e-02
##                                     Pavement Defective   14 7.855460e-04
##                                      Pavement Slippery   60 3.366626e-03
##  Pedestrian/Bicyclist/Other Pedestrian Error/Confusion  129 7.238245e-03
##                                    Physical Disability    6 3.366626e-04
##                                Prescription Medication    2 1.122209e-04
##                         Reaction to Uninvolved Vehicle  125 7.013803e-03
##                           Shoulders Defective/Improper    1 5.611043e-05
##                                       Steering Failure   53 2.973853e-03
##                                         Tinted Windows    4 2.244417e-04
##                                Tire Failure/Inadequate   30 1.683313e-03
##                                    Tow Hitch Defective    2 1.122209e-04
##            Traffic Control Device Improper/Non-Working    6 3.366626e-04
##                            Traffic Control Disregarded  742 4.163394e-02
##                                     Turning Improperly  468 2.625968e-02
##                                   Unsafe Lane Changing  193 1.082931e-02
##                                           Unsafe Speed  770 4.320503e-02
##                                            Unspecified 3754 2.106385e-01
##                       Using On Board Navigation Device    1 5.611043e-05
##                                      Vehicle Vandalism    5 2.805521e-04
##                                View Obstructed/Limited  150 8.416564e-03
##                                                   <NA>  108 6.059926e-03
##  valid_percent
##   7.338828e-04
##   8.185616e-03
##   2.303263e-02
##   7.903353e-04
##   3.900869e-02
##   3.895224e-03
##   6.209778e-04
##   5.645252e-05
##   2.373264e-01
##   2.100034e-02
##   1.354861e-03
##   6.209778e-04
##   1.693576e-04
##   1.975838e-03
##   1.033081e-01
##   1.524218e-03
##   6.604945e-03
##   5.577509e-02
##   1.129050e-03
##   5.645252e-05
##   2.032291e-03
##   3.951677e-04
##   2.822626e-03
##   1.524218e-03
##   1.693576e-04
##   5.645252e-05
##   2.043581e-02
##   2.653269e-03
##   1.862933e-03
##   1.919386e-03
##   3.703286e-02
##   5.413797e-02
##   7.903353e-04
##   3.387151e-03
##   7.282376e-03
##   3.387151e-04
##   1.129050e-04
##   7.056565e-03
##   5.645252e-05
##   2.991984e-03
##   2.258101e-04
##   1.693576e-03
##   1.129050e-04
##   3.387151e-04
##   4.188777e-02
##   2.641978e-02
##   1.089534e-02
##   4.346844e-02
##   2.119228e-01
##   5.645252e-05
##   2.822626e-04
##   8.467879e-03
##             NA
```
I'm going to categorize these into much fewer categories to make it easier to create a model, how about now with the type of car as the primary vehicle involved in the accident.

```r
motor_cat %>% tabyl(type)
```

```
##                                 type    n      percent valid_percent
##                           2 dr sedan    1 5.611043e-05  5.703205e-05
##                               3-Door    2 1.122209e-04  1.140641e-04
##                           4 dr sedan   11 6.172147e-04  6.273526e-04
##                                 AMBU    2 1.122209e-04  1.140641e-04
##                            AMBULANCE   15 8.416564e-04  8.554808e-04
##                            AMBULENCE    2 1.122209e-04  1.140641e-04
##                            Ambulance   78 4.376613e-03  4.448500e-03
##                        Armored Truck    4 2.244417e-04  2.281282e-04
##                           BOOM MOPED    1 5.611043e-05  5.703205e-05
##                       Beverage Truck    2 1.122209e-04  1.140641e-04
##                                 Bike  205 1.150264e-02  1.169157e-02
##                            Box Truck  176 9.875435e-03  1.003764e-02
##                     Bulk Agriculture    3 1.683313e-04  1.710962e-04
##                                  Bus  247 1.385928e-02  1.408692e-02
##                            Carry All    7 3.927730e-04  3.992244e-04
##                          Chassis Cab    6 3.366626e-04  3.421923e-04
##                           Commercial    1 5.611043e-05  5.703205e-05
##                       Concrete Mixer    3 1.683313e-04  1.710962e-04
##                          Convertible   27 1.514981e-03  1.539865e-03
##                              Courier    1 5.611043e-05  5.703205e-05
##                           DELIVERY T    1 5.611043e-05  5.703205e-05
##                                 DUMP    1 5.611043e-05  5.703205e-05
##                           DUMP TRUCK    1 5.611043e-05  5.703205e-05
##                                 Dump   36 2.019975e-03  2.053154e-03
##                               E-Bike  154 8.641005e-03  8.782936e-03
##                            E-Scooter   75 4.208282e-03  4.277404e-03
##                           Electric m    1 5.611043e-05  5.703205e-05
##                                 FDNY    1 5.611043e-05  5.703205e-05
##                           FDNY AMBUL    2 1.122209e-04  1.140641e-04
##                            FDNY FIRE    1 5.611043e-05  5.703205e-05
##                           FDNY truck    1 5.611043e-05  5.703205e-05
##                           FIRE ENGIN    1 5.611043e-05  5.703205e-05
##                           FIRE TRUCK    3 1.683313e-04  1.710962e-04
##                            FIRETRUCK    3 1.683313e-04  1.710962e-04
##                           FLYWING MO    1 5.611043e-05  5.703205e-05
##                            FOOD CART    1 5.611043e-05  5.703205e-05
##                            FORK LIFT    1 5.611043e-05  5.703205e-05
##                           FREIGHT VA    1 5.611043e-05  5.703205e-05
##                           Fire Truck    1 5.611043e-05  5.703205e-05
##                           Fire engin    1 5.611043e-05  5.703205e-05
##                           Fire truck    1 5.611043e-05  5.703205e-05
##                            Firetruck    3 1.683313e-04  1.710962e-04
##                             Flat Bed   26 1.458871e-03  1.482833e-03
##                            Flat Rack    7 3.927730e-04  3.992244e-04
##                              Ford FF    1 5.611043e-05  5.703205e-05
##                             Forklift    1 5.611043e-05  5.703205e-05
##                           GARBAGE TR    2 1.122209e-04  1.140641e-04
##                            GAS MOPED    1 5.611043e-05  5.703205e-05
##                             GOLFCART    1 5.611043e-05  5.703205e-05
##                    Garbage or Refuse   24 1.346650e-03  1.368769e-03
##                           Government    1 5.611043e-05  5.703205e-05
##                                 LIMO    3 1.683313e-04  1.710962e-04
##                               Ladder    1 5.611043e-05  5.703205e-05
##                           Ladder tru    1 5.611043e-05  5.703205e-05
##                            Lift Boom    1 5.611043e-05  5.703205e-05
##                                   MC    1 5.611043e-05  5.703205e-05
##                           METRO TRAN    1 5.611043e-05  5.703205e-05
##                                MOPED    5 2.805521e-04  2.851603e-04
##                           MOTOR SCOO    1 5.611043e-05  5.703205e-05
##                           MOTORIZEDS    1 5.611043e-05  5.703205e-05
##                            Minicycle    1 5.611043e-05  5.703205e-05
##                                Moped   94 5.274380e-03  5.361013e-03
##                            Motorbike   11 6.172147e-04  6.273526e-04
##                           Motorcycle  188 1.054876e-02  1.072203e-02
##                         Motorscooter   21 1.178319e-03  1.197673e-03
##                              Mta bus    1 5.611043e-05  5.703205e-05
##                Multi-Wheeled Vehicle    1 5.611043e-05  5.703205e-05
##                           NYC FIRE T    1 5.611043e-05  5.703205e-05
##                                  OMS    1 5.611043e-05  5.703205e-05
##                              Omnibus    1 5.611043e-05  5.703205e-05
##                            Open Body    1 5.611043e-05  5.703205e-05
##                                  PAS    2 1.122209e-04  1.140641e-04
##                                   PK   34 1.907754e-03  1.939090e-03
##                        Pick-up Truck  428 2.401526e-02  2.440972e-02
##                                   RV    2 1.122209e-04  1.140641e-04
##                            Red moped    1 5.611043e-05  5.703205e-05
##                     Refrigerated Van    1 5.611043e-05  5.703205e-05
##                           Road sweep    1 5.611043e-05  5.703205e-05
##                           SCHOOL BUS    1 5.611043e-05  5.703205e-05
##                            SCHOOLBUS    1 5.611043e-05  5.703205e-05
##                              SCOOTER    1 5.611043e-05  5.703205e-05
##                           SELF INSUR    1 5.611043e-05  5.703205e-05
##                                  SPC    2 1.122209e-04  1.140641e-04
##                           Sanitation    1 5.611043e-05  5.703205e-05
##                           School Bus    2 1.122209e-04  1.140641e-04
##                                Sedan 8491 4.764336e-01  4.842592e-01
##                           Sprinter v    1 5.611043e-05  5.703205e-05
##                        Stake or Rack    1 5.611043e-05  5.703205e-05
##  Station Wagon/Sport Utility Vehicle 6625 3.717316e-01  3.778373e-01
##                            TOW TRUCK    1 5.611043e-05  5.703205e-05
##                              TRAILER    2 1.122209e-04  1.140641e-04
##                                TRUCK    1 5.611043e-05  5.703205e-05
##                               Tanker    8 4.488834e-04  4.562564e-04
##                                 Taxi  251 1.408372e-02  1.431505e-02
##                            Tow Truck    2 1.122209e-04  1.140641e-04
##                  Tow Truck / Wrecker   17 9.538772e-04  9.695449e-04
##                            Tow truck    1 5.611043e-05  5.703205e-05
##                           Tractor Tr    1 5.611043e-05  5.703205e-05
##                 Tractor Truck Diesel   64 3.591067e-03  3.650051e-03
##               Tractor Truck Gasoline   13 7.294355e-04  7.414167e-04
##                              Trailer    2 1.122209e-04  1.140641e-04
##                                Truck    3 1.683313e-04  1.710962e-04
##                                UHAUL    1 5.611043e-05  5.703205e-05
##                                 USPS    1 5.611043e-05  5.703205e-05
##                           USPS VEHIC    1 5.611043e-05  5.703205e-05
##                              UTILITY    1 5.611043e-05  5.703205e-05
##                                  Van   74 4.152171e-03  4.220372e-03
##                           Van Camper    1 5.611043e-05  5.703205e-05
##                           Waste truc    1 5.611043e-05  5.703205e-05
##                            ambulance    1 5.611043e-05  5.703205e-05
##                           commerical    1 5.611043e-05  5.703205e-05
##                                 delv    1 5.611043e-05  5.703205e-05
##                             forklift    1 5.611043e-05  5.703205e-05
##                           pick up tr    1 5.611043e-05  5.703205e-05
##                                  van    2 1.122209e-04  1.140641e-04
##                                 <NA>  288 1.615980e-02            NA
```

### Creating Categories

This dataset is a mess, I'm going to clean it up here.

```r
motor_cat <- motor_cat %>% 
  mutate(contrib_cat = as.factor(case_when(
    contrib == "Accelerator Defective" | contrib == "Brakes Defective" | contrib == "Steering Failure" | contrib == "Headlights Defective" | contrib == "Other Lighting Defects" | contrib == "Tinted Windows" | contrib == "Tire Failure/Inadequate" | contrib == "Tow Hitch Defective" ~ "Car Defects",
    contrib == "Backing Unsafely" | contrib == "Driver Inexperience" | contrib == "Failure to Keep Right" | contrib == "Failure to Yield Right-of-Way" | contrib == "Following Too Closely" | contrib == "Passing Too Closely" | contrib == "Passing or Lane Usage Improper" | contrib == "Turning Improperly" ~ "Driver Error",
    contrib == "Alcohol Involvement" | contrib == "Drugs (illegal)" | contrib == "Prescription Medication" ~ "Substances",
    contrib == "Animals Action" | contrib == "Driverless/Runaway Vehicle" | contrib == "Glare" | contrib == "Lane Marking Improper/Inadequate" | contrib == "Obstruction/Debris" | contrib == "Pavement Defective" | contrib == "Pavement Slippery" | contrib == "Pedestrian/Bicyclist/Other Pedestrian Error/Confusion" | contrib == "Reaction to Uninvolved Vehicle" | contrib == "Shoulders Defective/Improper" | contrib == "Traffic Control Device Improper/Non-Working" | contrib == "View Obstructed/Limited" | contrib == "Vehicle Vandalism" ~ "Road Conditions",
    contrib == "Cell Phone (hand-Held)" | contrib == "Cell Phone (hands-free)" | contrib == "Driver Inattention/Distraction" | contrib == "Eating or Drinking" | contrib == "Other Electronic Device" | contrib == "Outside Car Distraction" | contrib == "Using Onboard Navigation Device" ~ "Distracted Driver",
    contrib == "Fatigued/Drowsy" | contrib == "Fell Asleep" | contrib == "Illnes" | contrib == "Lost Consciousness" | contrib == "Physical Disability" ~ "Condition of Driver",
    contrib == "Aggressive Driving/Road Rage" | contrib == "Traffic Control Disregarded" | contrib == "Unsafe Lane Change" | contrib == "Unsafe Speed" ~ "Dangerous Driving",
    contrib == "Unspecified" | is.na(contrib) | contrib == "Other Vehicular" ~ NA
  )),
  type_cat = as.factor(case_when(
    type == "2 dr sedan" | type == "3-Door" | type == "4 dr sedan" | type == "Sedan" ~ "Sedan",
    type == "Taxi" ~ "Taxi",
    type == "Refrigerated Van" | type == "Van" | type == "Van Camper" | type == "van" ~ "Van",
    type == "Station Wagon/Sport Utility Vehicle" ~ "SUV",
    type == "AMBU" | type == "AMBULANCE" | type == "AMBULENCE" | type == "Ambulance" | type == "FDNY AMBUL" | type == "ambulance" ~ "Ambulance",
    type == "Armored Truck" | type == "Beverage Truck" | type == "Box Truck" | type == "Concrete Mixer" | type == "DELIVERY T" | type == "DUMP" | type == "RV" | type == "DUMP TRUCK" | type == "Dump" | type == "Flat Bed" | type == "Flat Rack" | type == "GARBAGE TR" | type == "Garbage or Refuse" | type == "Road sweep" | type == "Sanitation" | type == "TOW TRUCK" | type == "TRAILER" | type == "TRUCK" | type == "Tanker" | type == "Tow Truck" | type == "Tow Truck / Wrecker" | type == "Tow truck" | type == "Tractor Tr" | type == "Tractor Truck Diesel" | type == "Tractor Truck Gasoline" | type == "Trailer" | type == "Truck" | type == "UHAUL" | type == "USPS" | type == "USPS VEHIC" | type == "UTILITY" | type == "Waste truc" ~ "Truck",
    type == "Chassis Cab" | type == "Pick-up Truck" | type == "Pick up tr" ~ "Pick Up Truck",
    type == "BOOM MOPED" | type == "GAS MOPED" | type == "MOPED" | type == "Moped"| type == "Motorbike" | type == "Motorcycle" | type == "Red moped" ~ "Motorcycle/Moped",
    type == "Bike" | type == "E-Bike" | type == "Electric m" | type == "FLYWING MO" ~ "Bike/E-Bike",
    type == "E-Scooter" | type == "MOTOR SCOO" | type == "MOTORIZEDS" | type == "Motorscooter" | type == "SCOOTER" ~ "Scooter/E-Scooter",
    type == "Bus" | type == "Mta bus" | type == "Omnibus" | type == "SCHOOL BUS" | type == "SCHOOLBUS" | type == "School Bus" ~ "Bus",
    type == "FDNY" | type == "FDNY FIRE" | type == "FDNY truck" | type == "FIRE ENGIN" | type == "FIRE TRUCK" | type == "FIRETRUCK" | type == "Fire Truck" | type == "Fire engin" | type == "Fire truck" | type == "Firetruck" | type == "Ladder" | type == "Ladder tru"| type == "NYC Fire T" ~ "Fire Truck",
    is.na(type) ~ NA,
    TRUE ~ "Other"
  )),
  time_of_day = as.factor(case_when(
    hour < 6 | hour > 20 ~ "Night",
    hour < 12 ~ "Morning",
    hour < 18 ~ "Afternoon",
    hour <= 20 ~ "Evening"
  )),
  fatal_cat = as.factor(case_when(
    number_of_persons_killed > 0 ~ "Fatal",
    TRUE ~ "Non Fatal")
  ),
  injured_cat = as.factor(case_when(
    number_of_persons_injured > 0 ~ "Injuries",
    TRUE ~ "No Injuries")
  ))
```

Phew, that was a lot of work. Now I'm going to check to make sure that I didn't leave anything out and everything was either categorized or caught by the catch all at the end of the new type of vehicle category variable.

```r
motor_cat %>% tabyl(contrib_cat)
```

```
##          contrib_cat    n     percent valid_percent
##          Car Defects  173 0.009707104    0.01297143
##  Condition of Driver  236 0.013242060    0.01769513
##    Dangerous Driving 1657 0.092974975    0.12424083
##    Distracted Driver 4269 0.239535406    0.32008698
##         Driver Error 5999 0.336606441    0.44980130
##      Road Conditions  582 0.032656268    0.04363800
##           Substances  421 0.023622489    0.03156632
##                 <NA> 4485 0.251655258            NA
```
Looks good, now the type of vehicle category.

```r
motor_cat %>% tabyl(type_cat)
```

```
##           type_cat    n     percent valid_percent
##          Ambulance  100 0.005611043   0.005703205
##        Bike/E-Bike  361 0.020255864   0.020588571
##                Bus  253 0.014195938   0.014429109
##         Fire Truck   18 0.001009988   0.001026577
##   Motorcycle/Moped  301 0.016889238   0.017166648
##              Other  102 0.005723263   0.005817269
##      Pick Up Truck  434 0.024351925   0.024751911
##  Scooter/E-Scooter   99 0.005554932   0.005646173
##              Sedan 8505 0.477219167   0.485057602
##                SUV 6625 0.371731568   0.377837345
##               Taxi  251 0.014083717   0.014315045
##              Truck  407 0.022836943   0.023212045
##                Van   78 0.004376613   0.004448500
##               <NA>  288 0.016159802            NA
```

Here's the final dataset, I generally display it in a tibble format as the blogdown format seems to display only the first few rows of tibbles which I like as it does not take up too much space on my blog post.

```r
motor_cat_df <- motor_cat %>% 
  filter(!is.na(contrib_cat) & !is.na(type_cat)) %>% 
  dplyr::select(collision_id, month, hour, contrib_cat, type_cat, time_of_day, injured_cat)

motor_cat_final <- as_tibble(motor_cat_df)
motor_cat_final
```

```
## # A tibble: 13,150 × 7
##    collision_id month  hour contrib_cat       type_cat   time_of_day injured_cat
##           <int> <dbl> <int> <fct>             <fct>      <fct>       <fct>      
##  1      4679634    11    15 Driver Error      Motorcycl… Afternoon   Injuries   
##  2      4663636     9     7 Distracted Driver Sedan      Morning     No Injuries
##  3      4663437     9     0 Driver Error      Sedan      Night       No Injuries
##  4      4629913     5    15 Driver Error      Sedan      Afternoon   No Injuries
##  5      4631543     5    16 Driver Error      SUV        Afternoon   Injuries   
##  6      4631801     5    14 Driver Error      SUV        Afternoon   No Injuries
##  7      4632066     5    15 Distracted Driver Pick Up T… Afternoon   No Injuries
##  8      4631365     5     9 Distracted Driver Truck      Morning     No Injuries
##  9      4680340    11    18 Dangerous Driving Motorcycl… Evening     Injuries   
## 10      4631597     5    15 Driver Error      SUV        Afternoon   No Injuries
## # ℹ 13,140 more rows
```


## Data Visualization

I wanted to visualize the predictors that I am going to use in the model just to see how everything is distributed and if there are any obvious differences that the model might pick up on.

### Contributing Factor

```r
ggplot(data = motor_cat_final, aes(y = contrib_cat, fill = injured_cat)) +
  geom_bar(color = "dimgray", alpha = 0.8) +
  labs(title = "Count of Accidents and Their Outcomes by Contributing Factor",
       y = "Primary Contributing Factor of Accident",
       x = "Count",
       caption = "Source: NYC Open Data Motor Vehicle Collisions- Crashes (January-December 2023)",
       fill = "Injury Outcome") +
  theme(plot.title = element_text(hjust=0.5)) +
  theme_minimal()
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-12-1.png" width="672" />
The most common primary cause of the accident is distracted driving and driver error, this especially makes sense as distracted driving is made so easy these days by all the electronic devices that we have available. 

### Type of Vehicle

```r
ggplot(data = motor_cat_final, aes(y = type_cat, fill = injured_cat)) +
  geom_bar(color = "dimgray", alpha = 0.8) +
  labs(title = "Count of Accidents and Their Outcomes by Type of Primary Vehicle",
       y = "Primary Vehicle Involved in of Accident",
       x = "Count",
       caption = "Source: NYC Open Data Motor Vehicle Collisions- Crashes (January-December 2023)",
       fill = "Injury Outcome") +
  theme(plot.title = element_text(hjust=0.5)) +
  theme_minimal()
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-13-1.png" width="672" />
One thing that stuck out to me, which is not very surprising now that I think about it, is the proportion of accidents that resulted in injuries when a bike/e-bike or motorcycle/moped is involved. This really emphasizes the importance of being safe and fully attentive when on the road not just for other drivers, but for bikes and motorcycles too, as well as the importance of bike infrastructure that would keep bikes seperate from other vehicles that can hurt them.

### Time of Day

```r
ggplot(data = motor_cat_final, aes(x = time_of_day, fill = injured_cat)) +
  geom_bar(color = "dimgray", alpha = 0.8) +
  labs(title = "Count of Accidents and Their Outcomes by Time of Day",
       x = "Time of Day that Accident Occurred",
       y = "Count",
       caption = "Source: NYC Open Data Motor Vehicle Collisions- Crashes (January-December 2023)",
       fill = "Injury Outcome") +
  theme(plot.title = element_text(hjust=0.5)) +
  theme_minimal()
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-14-1.png" width="672" />
Just inspecting this visually it does not look like there is that big of a difference between the different times of day and the proportion of accidents that result in injuries. 

## Classification Models to Predict Injury Outcome of Motor Vehicle Collision

### Create Training and Test Data

In order to create and test my model that classifies if an accident results in injuries or not for anyone involved, I'm going to seperate my training and test data by taking an arbitrary amount of observations to be considered test and training using the sample_frac() and anti_join() functions.

```r
set.seed(123)
training <- motor_cat_df %>% 
  sample_frac(.70)
  
test <- motor_cat_df %>% anti_join(training, by = "collision_id")

training_injury_outcome <- training$injured_cat
test_injury_outcome <- test$injured_cat
```

### Logistic Regression

The first model that I wanted to fit was a logistic regression model, as there are only two different outcomes in the data (injuries vs. no injuries). Here I fitted that model with my predictors: contributing factor, type of vehicle, and time of day, along with my response variable which was if the accident resulted in injuries or not. 

```r
glm.inj <- glm(injured_cat ~ contrib_cat + type_cat + time_of_day,
               data = training,
               family = binomial)
summary(glm.inj)
```

```
## 
## Call:
## glm(formula = injured_cat ~ contrib_cat + type_cat + time_of_day, 
##     family = binomial, data = training)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.0102  -1.2497   0.8528   1.0539   2.3111  
## 
## Coefficients:
##                                Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                     1.74107    0.48089   3.621 0.000294 ***
## contrib_catCondition of Driver -0.53689    0.24739  -2.170 0.029991 *  
## contrib_catDangerous Driving   -0.38954    0.19907  -1.957 0.050375 .  
## contrib_catDistracted Driver    0.09706    0.19357   0.501 0.616072    
## contrib_catDriver Error        -0.03027    0.19242  -0.157 0.874993    
## contrib_catRoad Conditions     -0.53907    0.21675  -2.487 0.012880 *  
## contrib_catSubstances           0.24526    0.22726   1.079 0.280488    
## type_catBike/E-Bike            -3.93770    0.49501  -7.955 1.79e-15 ***
## type_catBus                    -1.76695    0.47378  -3.729 0.000192 ***
## type_catFire Truck             -0.91710    0.94812  -0.967 0.333402    
## type_catMotorcycle/Moped       -3.50744    0.49026  -7.154 8.42e-13 ***
## type_catOther                  -1.65302    0.50726  -3.259 0.001119 ** 
## type_catPick Up Truck          -1.30525    0.46299  -2.819 0.004814 ** 
## type_catScooter/E-Scooter      -3.41098    0.54718  -6.234 4.55e-10 ***
## type_catSedan                  -1.54048    0.44326  -3.475 0.000510 ***
## type_catSUV                    -1.66967    0.44347  -3.765 0.000167 ***
## type_catTaxi                   -2.37742    0.47385  -5.017 5.24e-07 ***
## type_catTruck                  -1.11660    0.46499  -2.401 0.016336 *  
## type_catVan                    -1.29899    0.56568  -2.296 0.021658 *  
## time_of_dayEvening             -0.01271    0.06711  -0.189 0.849830    
## time_of_dayMorning              0.27367    0.05781   4.734 2.20e-06 ***
## time_of_dayNight                0.52662    0.05700   9.238  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 12644  on 9204  degrees of freedom
## Residual deviance: 12059  on 9183  degrees of freedom
## AIC: 12103
## 
## Number of Fisher Scoring iterations: 4
```

Then I used the predict() function to predict the outcome using my test data, and compared the real data along with the predicted data using a confusion matrix.

```r
inj.probs <- predict(glm.inj,
                     test,
                     type = "response")
inj.pred <- rep("No Injuries", length(inj.probs))
inj.pred[inj.probs > 0.5] <- "Injuries"

table(inj.pred, test_injury_outcome)
```

```
##              test_injury_outcome
## inj.pred      Injuries No Injuries
##   Injuries        1304        1980
##   No Injuries      422         239
```

```r
mean(inj.pred == test_injury_outcome)
```

```
## [1] 0.391128
```
Yikes, that's worse than just random guessing. I'm going to use a different model now and see how that compares to the logistic regression.

### Naive Bayes
I chose to use the naive bayes model as it does not assume any particular distribution in the data. I fit the model using the naiveBayes() function from the e1071 package and displayed the output of the model object below.

```r
nb.inj <- naiveBayes(injured_cat ~ contrib_cat + type_cat + time_of_day,
                 data = training)
nb.inj
```

```
## 
## Naive Bayes Classifier for Discrete Predictors
## 
## Call:
## naiveBayes.default(x = X, y = Y, laplace = laplace)
## 
## A-priori probabilities:
## Y
##    Injuries No Injuries 
##   0.4437806   0.5562194 
## 
## Conditional probabilities:
##              contrib_cat
## Y             Car Defects Condition of Driver Dangerous Driving
##   Injuries     0.01126071          0.02105263        0.14369645
##   No Injuries  0.01445313          0.01484375        0.10761719
##              contrib_cat
## Y             Distracted Driver Driver Error Road Conditions Substances
##   Injuries           0.29987760   0.44504284      0.05483476 0.02423501
##   No Injuries        0.34003906   0.45058594      0.03339844 0.03906250
## 
##              type_cat
## Y                Ambulance  Bike/E-Bike          Bus   Fire Truck
##   Injuries    0.0014687882 0.0438188494 0.0166462668 0.0004895961
##   No Injuries 0.0074218750 0.0044921875 0.0142578125 0.0009765625
##              type_cat
## Y             Motorcycle/Moped        Other Pick Up Truck Scooter/E-Scooter
##   Injuries        0.0337821297 0.0073439412  0.0205630355      0.0127294982
##   No Injuries     0.0052734375 0.0072265625  0.0292968750      0.0023437500
##              type_cat
## Y                    Sedan          SUV         Taxi        Truck          Van
##   Injuries    0.4411260710 0.3801713586 0.0212974296 0.0173806610 0.0031823745
##   No Injuries 0.5011718750 0.3804687500 0.0117187500 0.0310546875 0.0042968750
## 
##              time_of_day
## Y             Afternoon   Evening   Morning     Night
##   Injuries    0.3618115 0.1703794 0.2296206 0.2381885
##   No Injuries 0.2933594 0.1333984 0.2527344 0.3205078
```

Now to check the model to see how it compares to my worse-than-random-guessing logistic regression model.

```r
nb.inj.pred <-  predict(nb.inj, test)
table(nb.inj.pred, test_injury_outcome)
```

```
##              test_injury_outcome
## nb.inj.pred   Injuries No Injuries
##   Injuries         461         263
##   No Injuries     1265        1956
```

```r
mean(nb.inj.pred == test_injury_outcome)
```

```
## [1] 0.6126743
```
Well that's not great, but it's definitely an improvement from the previous logistic regression model.

## Improve Logistic Regression Model
It looks like the primary contributing factor does not really have much statistical significance if there are injuries in a motor vehicle accident or not, I'm going to remove that variable from each model and see how that changes things.

```r
glm.inj.new <- glm(injured_cat ~ type_cat + time_of_day,
               data = training,
               family = binomial)
summary(glm.inj.new)
```

```
## 
## Call:
## glm(formula = injured_cat ~ type_cat + time_of_day, family = binomial, 
##     data = training)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.1353  -1.2387   0.9115   1.0588   2.1639  
## 
## Coefficients:
##                            Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                1.651322   0.441470   3.741 0.000184 ***
## type_catBike/E-Bike       -3.888045   0.493421  -7.880 3.28e-15 ***
## type_catBus               -1.732093   0.472146  -3.669 0.000244 ***
## type_catFire Truck        -0.806790   0.946818  -0.852 0.394155    
## type_catMotorcycle/Moped  -3.482104   0.488651  -7.126 1.03e-12 ***
## type_catOther             -1.608261   0.505064  -3.184 0.001451 ** 
## type_catPick Up Truck     -1.283938   0.461426  -2.783 0.005393 ** 
## type_catScooter/E-Scooter -3.353955   0.545475  -6.149 7.81e-10 ***
## type_catSedan             -1.508263   0.441687  -3.415 0.000638 ***
## type_catSUV               -1.633338   0.441928  -3.696 0.000219 ***
## type_catTaxi              -2.305245   0.471983  -4.884 1.04e-06 ***
## type_catTruck             -1.076075   0.463503  -2.322 0.020254 *  
## type_catVan               -1.271352   0.563454  -2.256 0.024048 *  
## time_of_dayEvening        -0.003336   0.066715  -0.050 0.960122    
## time_of_dayMorning         0.267486   0.057502   4.652 3.29e-06 ***
## time_of_dayNight           0.520432   0.055919   9.307  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 12644  on 9204  degrees of freedom
## Residual deviance: 12142  on 9189  degrees of freedom
## AIC: 12174
## 
## Number of Fisher Scoring iterations: 4
```

Now checking again

```r
inj.probs.new <- predict(glm.inj.new,
                     test,
                     type = "response")
inj.pred.new <- rep("No Injuries", length(inj.probs.new))
inj.pred.new[inj.probs.new > 0.5] <- "Injuries"

table(inj.pred.new, test_injury_outcome)
```

```
##              test_injury_outcome
## inj.pred.new  Injuries No Injuries
##   Injuries        1524        2156
##   No Injuries      202          63
```

```r
mean(inj.pred.new == test_injury_outcome)
```

```
## [1] 0.4022814
```
Still worse than guessing even though this does result in a slight improvement, but the Naive Bayes model still remains on top.

## Conclusion
I learned so many new things trying to practice the skills that I've learned using classification models. I'm starting to become more familiar with the code on how to make predictions and check a classification model. Also, I learned a different way on how to seperate test and training data using the sample_frac() function in conjunction with the anti_join() function to create two different data frames that have a random fraction of observations in one data frame, and then the ones that were not selected in the other data frame.

I also see how messy working with real life data rather than data from an R package or a textbook is, this dataset is so huge and I only used a very pared down version of it. The original has millions of observations! There are also a lot of instances of human error and just differences in recording observations in the contributing factor and type of vehicle columns.
