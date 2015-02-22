# Reproducible Research: Peer Assessment 2
## Analysis of U.S. National Oceanic and Atmospheric Administration's (NOAA) storm data and their impact on population health and economy

##Synopsis
In this report we are exploring and analysing the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm data collected between the year 1950 and 2011. The objective of this report is to identify the major storms and weather events in the United States that have the most impact on population health in terms of fatalities and injuries and on economy in terms of property damage. Based on the Anlysis performed, the **excessive heat** and **tornado** are most harmful with respect to population health, while **flood**, **drought**, **and hurricane/typhoon** have the greatest economic consequences.
 

## Data Processing
- First we need to download the data from this URL and save it in our working directory


```r
working_directory <- getwd()
# Download file from URL if doesn't exist
if (!file.exists(paste(working_directory,"repdata-data-StormData.csv.bz2", sep="/"))) 
  {
    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", working_directory)
}
```

- Here we will load the data from the CSV file using the **read.table()** and **bzfile** functions 



```r
# Read the data from downloaded file
if (file.exists(paste(working_directory,"repdata-data-StormData.csv.bz2", sep="/"))) {
  storm_data <- read.table(bzfile("repdata-data-StormData.csv.bz2"),sep=",",stringsAsFactors=FALSE,header=TRUE)  
  
}
```



```r
#head(storm_data)
```

- Limit the dataset to needed columns only to analyze the impact on population health and economy


```r
storm_data <- storm_data [,c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]
head(storm_data)
```

```
##    EVTYPE FATALITIES INJURIES PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP
## 1 TORNADO          0       15    25.0          K       0           
## 2 TORNADO          0        0     2.5          K       0           
## 3 TORNADO          0        2    25.0          K       0           
## 4 TORNADO          0        2     2.5          K       0           
## 5 TORNADO          0        2     2.5          K       0           
## 6 TORNADO          0        6     2.5          K       0
```



- Prepare economy data by converting the exp characters into number and create a new variable for cash

```r
# get list of unique values in PROPDMGEXP
unique(storm_data$PROPDMGEXP)
```

```
##  [1] "K" "M" ""  "B" "m" "+" "0" "5" "6" "?" "4" "2" "3" "h" "7" "H" "-"
## [18] "1" "8"
```

```r
# replace Null/ special characters with 1  (- ? + )

storm_data$PROPDMGEXP[storm_data$PROPDMGEXP == "" | storm_data$PROPDMGEXP == "-"| storm_data$PROPDMGEXP == "?" | storm_data$PROPDMGEXP == "+"] <- "1"
# replace characters with the corresponding exp number  ( B h H K m M)
storm_data$PROPDMGEXP[storm_data$PROPDMGEXP == "h" | storm_data$PROPDMGEXP == "H"] <- "100"
storm_data$PROPDMGEXP[storm_data$PROPDMGEXP == "k" | storm_data$PROPDMGEXP == "K"] <- "1000"
storm_data$PROPDMGEXP[storm_data$PROPDMGEXP == "m" | storm_data$PROPDMGEXP == "M"] <- "1000000"
storm_data$PROPDMGEXP[storm_data$PROPDMGEXP == "b" | storm_data$PROPDMGEXP == "B"] <- "1000000000"
storm_data$PROPDMGEXP <- as.numeric(storm_data$PROPDMGEXP)
```


```r
# get list of unique values in CROPDMGEXP
unique(storm_data$CROPDMGEXP)
```

```
## [1] ""  "M" "K" "m" "B" "?" "0" "k" "2"
```

```r
# replace Null/ special characters with 1  (- ? + )
storm_data$CROPDMGEXP[storm_data$CROPDMGEXP == "" | storm_data$CROPDMGEXP == "-"| storm_data$CROPDMGEXP == "?" | storm_data$CROPDMGEXP == "+"] <- "1"
# replace characters with the corresponding exp number  ( B h H K m M)
storm_data$CROPDMGEXP[storm_data$CROPDMGEXP == "h" | storm_data$CROPDMGEXP == "H"] <- "100"
storm_data$CROPDMGEXP[storm_data$CROPDMGEXP == "k" | storm_data$CROPDMGEXP == "K"] <- "1000"
storm_data$CROPDMGEXP[storm_data$CROPDMGEXP == "m" | storm_data$CROPDMGEXP == "M"] <- "1000000"
storm_data$CROPDMGEXP[storm_data$CROPDMGEXP == "b" | storm_data$CROPDMGEXP == "B"] <- "1000000000"
storm_data$CROPDMGEXP <- as.numeric(storm_data$CROPDMGEXP)
```

- Calculate impact on economy in cash and insert the values in new variables

```r
storm_data$PROPDMGCash <- storm_data$PROPDMGEXP*storm_data$PROPDMG
storm_data$CROPDMGCash <- storm_data$CROPDMGEXP*storm_data$CROPDMG
```

- Aggregate the values per event type



## Results