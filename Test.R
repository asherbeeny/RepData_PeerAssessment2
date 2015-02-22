setwd("Z:/Sherbeeny/GITHUB/RR-W3-Peer/RepData_PeerAssessment2")
working_directory <- getwd()

if (file.exists(paste(working_directory,"repdata-data-StormData.csv.bz2", sep="/"))) 
  
  {
  storm_data <- read.table(bzfile("repdata-data-StormData.csv.bz2"),sep=",",stringsAsFactors=FALSE,header=TRUE)  
  }

  storm_data <- storm_data [,c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]
  head(storm_data)

  



# get list of unique values in PROPDMGEXP
unique(storm_data$PROPDMGEXP)
# replace Null/ special characters with 1  (- ? + )

storm_data$PROPDMGEXP[storm_data$PROPDMGEXP == "" | storm_data$PROPDMGEXP == "-"| storm_data$PROPDMGEXP == "?" | storm_data$PROPDMGEXP == "+"] <- "1"
# replace characters with the corresponding exp number  ( B h H K m M)
storm_data$PROPDMGEXP[storm_data$PROPDMGEXP == "h" | storm_data$PROPDMGEXP == "H"] <- "100"
storm_data$PROPDMGEXP[storm_data$PROPDMGEXP == "k" | storm_data$PROPDMGEXP == "K"] <- "1000"
storm_data$PROPDMGEXP[storm_data$PROPDMGEXP == "m" | storm_data$PROPDMGEXP == "M"] <- "1000000"
storm_data$PROPDMGEXP[storm_data$PROPDMGEXP == "b" | storm_data$PROPDMGEXP == "B"] <- "1000000000"
storm_data$PROPDMGEXP <- as.numeric(storm_data$PROPDMGEXP)



# get list of unique values in CROPDMGEXP
unique(storm_data$CROPDMGEXP)

# replace Null/ special characters with 1  (- ? + )
storm_data$CROPDMGEXP[storm_data$CROPDMGEXP == "" | storm_data$CROPDMGEXP == "-"| storm_data$CROPDMGEXP == "?" | storm_data$CROPDMGEXP == "+"] <- "1"
# replace characters with the corresponding exp number  ( B h H K m M)
storm_data$CROPDMGEXP[storm_data$CROPDMGEXP == "h" | storm_data$CROPDMGEXP == "H"] <- "100"
storm_data$CROPDMGEXP[storm_data$CROPDMGEXP == "k" | storm_data$CROPDMGEXP == "K"] <- "1000"
storm_data$CROPDMGEXP[storm_data$CROPDMGEXP == "m" | storm_data$CROPDMGEXP == "M"] <- "1000000"
storm_data$CROPDMGEXP[storm_data$CROPDMGEXP == "b" | storm_data$CROPDMGEXP == "B"] <- "1000000000"
storm_data$CROPDMGEXP <- as.numeric(storm_data$CROPDMGEXP)





storm_data$PROPDMGCash <- storm_data$PROPDMGEXP*storm_data$PROPDMG
storm_data$CROPDMGCash <- storm_data$CROPDMGEXP*storm_data$CROPDMG



fatalbyevent <- aggregate(FATALITIES ~ EVTYPE, storm_data, sum,na.rm = T)
injurbyevent <- aggregate(INJURIES ~ EVTYPE, storm_data, sum,na.rm = T)


plot (fatalbyevent$EVTYPE)
plot (injurbyevent$INJURIES, injurbyevent$EVTYPE)
