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

# order event type by 5 most harmful events which caused fatalities
fatalbyevent <- fatalbyevent[order(fatalbyevent$FATALITIES, decreasing = TRUE), ]
fatalities_top5 <- fatalbyevent[1:5, ]
print(fatalities_top5)

# order event type by 5 most harmful events which caused injuries

injurbyevent <- injurbyevent[order(injurbyevent$INJURIES, decreasing = TRUE), ]
# 5 most harmful causes of injuries
injurbyevent_top5 <- injurbyevent[1:5, ]
print(injurbyevent_top5)






library(ggplot2)
library(plyr)

plot_fatalities <- qplot(EVTYPE, data = fatalities_top5, weight = FATALITIES, geom = "bar", binwidth = 1) + 
  scale_y_continuous("Fatalities Count") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("Event Type") + 
  ggtitle("Fatalities by Event Type\n in the U.S.\n from 1995 - 2011")

plot_injuries <- qplot(EVTYPE, data = injurbyevent_top5, weight = INJURIES, geom = "bar", binwidth = 1) + scale_y_continuous("Injuries Count") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("Event Type") + 
  ggtitle("Injuries by Weather event Type\n in the U.S.\n from 1995 - 2011")

plot_fatalities
plot_injuries

c<-ddply(storm_data,.(EVTYPE),summarize,PROPDMGCash=sum(PROPDMGCash),CROPDMGCash=sum(CROPDMGCash))

economy_damage<-mutate(c,economy_damage=PROPDMGCash+CROPDMGCash,economy_damage_m=economy_damage/1000000)

# order event type by 10 most harmful events which affected ecconomy
economy_damage_ordered<-arrange(economy_damage,desc(economy_damage_m))
economy_damage_top10 <- economy_damage_ordered[1:10,]

plot_economy_damage<- qplot(EVTYPE, data = economy_damage_top10, weight = economy_damage_m, geom = "bar", binwidth = 1) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_y_continuous("Economy Damage (M$)") + 
  xlab("Severe Weather Type") + ggtitle("Top 10 Weather events impacting economy in\n the U.S. from 1995 - 2011")

plot_economy_damage