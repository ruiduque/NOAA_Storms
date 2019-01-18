---
title: "Coursera Reproducible Research - Project 2"
author: "Rui"
date: "15/01/2019"
output: 
  html_document: 
    fig_caption: yes
    keep_md: yes
    number_sections: yes
    toc: yes
params: 
        data_file: "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
---



# Severe Weather Event Impact Analysis


## Synopsis
This project involves exploring the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. This database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage. The two key questions addressed in this project are:
1. Which types of weather events are most harmful with respect to population health in the US and
2. Which types of weather events have the greatest economic consequences in the US.
The questions will be addressed through a combination of exploratory data analysis of the NOAA dataset and ensuring any analysis steps could be fully reproducible by independent researchers.

# Data Processing
This section demonstrates the steps used by the researcher to load the NOAA dataset and prepare it for analysis. The code and associated comments illustrate the process carried out to ingest and transform the initial dataset up to the point where it is ready to support the analysis stage.

## List of external libraries 

```r
library(readr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(naniar, warn.conflicts = FALSE)
```

## Dataset download
This section checks if the file containing the NOAA dataset exists in the project folder. If not, it attempts to download a fresh version from the URL provided in the global parameters. 
The cache in this code chunk is turned on to improve the file processing times.

```r
if(!file.exists("repdata_data_StormData.csv.bz2")){
        download.file(params$data_file, "repdata_data_StormData.csv.bz2")
}

NOAA <- read_csv("repdata_data_StormData.csv.bz2", col_names = TRUE)
```

## Dataset structure inspection and missing values

This section provides a glimpse of the dataset structure in terms of variables and total observations. It also shows which variables have missing data so this could be taken into consideration during analysis.


```r
# Check dataset structure
glimpse(NOAA)
```

```
## Observations: 902,297
## Variables: 37
## $ STATE__    <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...
## $ BGN_DATE   <chr> "4/18/1950 0:00:00", "4/18/1950 0:00:00", "2/20/195...
## $ BGN_TIME   <chr> "0130", "0145", "1600", "0900", "1500", "2000", "01...
## $ TIME_ZONE  <chr> "CST", "CST", "CST", "CST", "CST", "CST", "CST", "C...
## $ COUNTY     <dbl> 97, 3, 57, 89, 43, 77, 9, 123, 125, 57, 43, 9, 73, ...
## $ COUNTYNAME <chr> "MOBILE", "BALDWIN", "FAYETTE", "MADISON", "CULLMAN...
## $ STATE      <chr> "AL", "AL", "AL", "AL", "AL", "AL", "AL", "AL", "AL...
## $ EVTYPE     <chr> "TORNADO", "TORNADO", "TORNADO", "TORNADO", "TORNAD...
## $ BGN_RANGE  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
## $ BGN_AZI    <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ BGN_LOCATI <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ END_DATE   <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ END_TIME   <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ COUNTY_END <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
## $ COUNTYENDN <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ END_RANGE  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
## $ END_AZI    <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ END_LOCATI <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ LENGTH     <dbl> 14.0, 2.0, 0.1, 0.0, 0.0, 1.5, 1.5, 0.0, 3.3, 2.3, ...
## $ WIDTH      <dbl> 100, 150, 123, 100, 150, 177, 33, 33, 100, 100, 400...
## $ F          <dbl> 3, 2, 2, 2, 2, 2, 2, 1, 3, 3, 1, 1, 3, 3, 3, 4, 1, ...
## $ MAG        <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
## $ FATALITIES <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 4, 0, ...
## $ INJURIES   <dbl> 15, 0, 2, 2, 2, 6, 1, 0, 14, 0, 3, 3, 26, 12, 6, 50...
## $ PROPDMG    <dbl> 25.0, 2.5, 25.0, 2.5, 2.5, 2.5, 2.5, 2.5, 25.0, 25....
## $ PROPDMGEXP <chr> "K", "K", "K", "K", "K", "K", "K", "K", "K", "K", "...
## $ CROPDMG    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
## $ CROPDMGEXP <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ WFO        <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ STATEOFFIC <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ ZONENAMES  <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ LATITUDE   <dbl> 3040, 3042, 3340, 3458, 3412, 3450, 3405, 3255, 333...
## $ LONGITUDE  <dbl> 8812, 8755, 8742, 8626, 8642, 8748, 8631, 8558, 874...
## $ LATITUDE_E <dbl> 3051, 0, 0, 0, 0, 0, 0, 0, 3336, 3337, 3402, 3404, ...
## $ LONGITUDE_ <dbl> 8806, 0, 0, 0, 0, 0, 0, 0, 8738, 8737, 8644, 8640, ...
## $ REMARKS    <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ REFNUM     <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, ...
```

```r
# Check entire dataset for missing data that could cause deviations during analysis
miss_dt <- miss_var_summary(NOAA)
vis_miss(NOAA, warn_large_data = FALSE)
```

![](NOAA_Analysis_files/figure-html/structure&nas-1.png)<!-- -->

## Dataset tidying and reduction

From a close inspection of the data structure, we can verify that this dataset has 902297 observations and 37 variables. In addition, around 36.3% of records are missing (NAs). Giving that we will only require part of the dataset to complete this analysis, we will retain only the required variables and delete the original dataset to free computational resources using the following code:


```r
rNOAA <- NOAA %>%
        select(STATE_ID = STATE__, BGN_DATE:EVTYPE, FATALITIES, INJURIES, PROPDMG:CROPDMGEXP)
#rm(NOAA)
```

# Analysis
The analysis document must have at least one figure containing a plot.
Your analysis must have no more than three figures. Figures may have multiple plots in them (i.e. panel plots), but there cannot be more than three figures total.

1. Across the United States, which types of events are most harmful with respect to population health?
2. Across the United States, which types of events have the greatest economic consequences?


### Top events with impact on population health across US
The dataset provides two variables for the impact on population health by severe weather events, they are: number of fatalities and number of injuries. For this analysis, we will look at the top 10 events in terms of each variable.

For fatalities, the top 10 events ordered in descending order are:


```r
# Overall top10 events by number of fatalities
top10_fatalities <- rNOAA %>%
        select(EVTYPE, FATALITIES) %>%
        group_by(EVTYPE) %>%
        summarise(FATALITIES = sum(FATALITIES)) %>%
        top_n(10, FATALITIES) %>%
        arrange(desc(FATALITIES)) %>%
        print
```

```
## # A tibble: 10 x 2
##    EVTYPE         FATALITIES
##    <chr>               <dbl>
##  1 TORNADO              5633
##  2 EXCESSIVE HEAT       1903
##  3 FLASH FLOOD           978
##  4 HEAT                  937
##  5 LIGHTNING             816
##  6 TSTM WIND             504
##  7 FLOOD                 470
##  8 RIP CURRENT           368
##  9 HIGH WIND             248
## 10 AVALANCHE             224
```

For injuries, we have the following:


```r
# Overall top10 events by number of injuries
top10_injuries <- rNOAA %>%
        select(EVTYPE, INJURIES) %>%
        group_by(EVTYPE) %>%
        summarise(INJURIES = sum(INJURIES)) %>%
        top_n(10, INJURIES) %>%
        arrange(desc(INJURIES)) %>%
        print
```

```
## # A tibble: 10 x 2
##    EVTYPE            INJURIES
##    <chr>                <dbl>
##  1 TORNADO              91346
##  2 TSTM WIND             6957
##  3 FLOOD                 6789
##  4 EXCESSIVE HEAT        6525
##  5 LIGHTNING             5230
##  6 HEAT                  2100
##  7 ICE STORM             1975
##  8 FLASH FLOOD           1777
##  9 THUNDERSTORM WIND     1488
## 10 HAIL                  1361
```

### Top events with most economic consequences across US
In this second question, we will look at the economic impact of severe weather events across the US. For this, the dataset provides two numeric variables with the economic impact of severe weather, these are:  

1. Property damage (PROPDMG)  
2. Crops damage (CROPDMG)

With each of the numeric variables, there is a one character variable indicating the exponent of the base variable:  

1. Property damage exponent (PROPDMEXP)  
-- Values: K, M, NA, B, m, +, 0, 5, 6, ?, 4, 2, 3, h, 7, H, -, 1, 8  
2. Crop damage exponent (CROPDMGEXP)  
-- Values: NA, FALSE

**Parsing the EXP values**
The following links provide further information on how to parse the EXP values.
[https://www.coursera.org/learn/reproducible-research/discussions/weeks/4/threads/38y35MMiEeiERhLphT2-QA](Coursera Instructions)
[https://rstudio-pubs-static.s3.amazonaws.com/58957_37b6723ee52b455990e149edde45e5b6.html](How To Handle Exponent Value of PROPDMGEXP and CROPDMGEXP)


```r
top10_property <- rNOAA %>%
        select(EVTYPE, PROPDMG, PROPDMGEXP) %>%
        filter(PROPDMGEXP %in% c(letters, LETTERS, "0":"9")) %>%
        mutate(Prop_Exp = case_when(
                PROPDMGEXP == "K" | PROPDMGEXP == "k" ~ 10^3,
                PROPDMGEXP == "M" | PROPDMGEXP == "m" ~ 10^6,
                PROPDMGEXP == "B" | PROPDMGEXP == "b" ~ 10^9,
                PROPDMGEXP == "H" | PROPDMGEXP == "h" ~ 10^2,
                TRUE ~ 10^as.numeric(PROPDMGEXP))) %>%
        mutate(Dollars = PROPDMG * Prop_Exp) %>%
        group_by(EVTYPE) %>%
        summarise(Dollars = sum(Dollars)) %>%
        top_n(10, Dollars) %>%
        arrange(desc(Dollars)) %>%
        print
```

```
## # A tibble: 10 x 2
##    EVTYPE                  Dollars
##    <chr>                     <dbl>
##  1 FLOOD             144657709800 
##  2 HURRICANE/TYPHOON  69305840000 
##  3 TORNADO            56947380614.
##  4 STORM SURGE        43323536000 
##  5 FLASH FLOOD        16822723772.
##  6 HAIL               15735267456.
##  7 HURRICANE          11868319010 
##  8 TROPICAL STORM      7703890550 
##  9 WINTER STORM        6688497251 
## 10 HIGH WIND           5270046260
```


```r
unique(rNOAA$CROPDMGEXP)
```

```
## [1]    NA FALSE
```

```r
top10_crop <- rNOAA %>%
        select(EVTYPE, CROPDMG, CROPDMGEXP) %>%
        group_by(EVTYPE) %>%
        summarise(Dollars = sum(CROPDMG)) %>%
        top_n(10, Dollars) %>%
        arrange(desc(Dollars)) %>%
        print
```

```
## # A tibble: 10 x 2
##    EVTYPE             Dollars
##    <chr>                <dbl>
##  1 HAIL               579596.
##  2 FLASH FLOOD        179200.
##  3 FLOOD              168038.
##  4 TSTM WIND          109203.
##  5 TORNADO            100019.
##  6 THUNDERSTORM WIND   66791.
##  7 DROUGHT             33899.
##  8 THUNDERSTORM WINDS  18685.
##  9 HIGH WIND           17283.
## 10 HEAVY RAIN          11123.
```




# Results

There should be a section titled Results in which your results are presented.



