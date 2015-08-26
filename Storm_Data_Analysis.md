# Injury, Mortality and Economic Analysis of different weather events in the USA based on data within the NOAA Storm Database

By Anthony V. Rosati
Date: August 23, 2015

## Reproducible Research: Peer Assessment 2

### 1. Purpose

The basic goal of this assignment is to answer some basic questions about the impact of severe weather events within the United States, using a publically-accessible data store, the NOAA Storm Database. We are to explore the data within this database to answer several questions, and provide the supporting R code for the analysis in "reproducible" manner. 

### 2. Abstract

The National Oceanic and Atmospheric Administration (NOAA) publishes and maintains a public repository of national storm events. The data encompasses storm event types, locations, dates, property damage estimates, as well as the human injuries and fatalities. In this report we investigate which type of events are physically and financially the most harmful to the U.S. population.

What was discovered is that, at least with respect to the NOAA Storm Data repository data, there is no direct 
correlation between human casualties (either injuries or fatalities) and the ecomomic damage caused by severe weather events. We did determine that tornado's are by far, the highest cause for human injuries, and the second for human fatalities. In addition,  heat & drought are obly fourth leading cause for human injuries, but are the leading cause for human fatalities. Other severe storm factors causing both human injuries and fatalities include (within the list of Top 5 causes) Thunderstorms (second with respect to injuries and fifth with respect to fatalities), Flooding (third leading cause for both) as well as Snow & Ice (fifth with resect to human injuries and fourth with respect to human fatalities).

When economic impact was analysed, it was discovered that property damage was the overwhelming factor in reported total damage, with the exception of Heat & Drought, which accounted for significant crop damage that totalled over 90% of total damage reported. In comparison against human casualities, Flooding & High Surf (the leading cause of economic impact) is accountable for more than 80% of all economic costs of severe weather, while Wind & Storm (the second leading cause of economic impact) isn't even in the Top 5 list of causes of human casualties.


### 3. Data Processing


##### _3a. Loading the Necessary Libraries_
The following are what we determined were the necessary libraries to perform loading, computation, transformation and plotting of data:

```r
library(RCurl) # using getBinaryURL for downloading the NOAA dataset
library(R.utils) # using bunzip2 to unwrap data package downloaded
library(plyr) # using counting & aggregation methods
library(reshape2) # using melt 
library(ggplot2) # for plotting
library(grid) # for grid functions
library(gridExtra) # for some of the advanced plots generated
library(scales) # for scaling of the plots generated
```


##### _3b. Loading the source file and extracting the dataset from it_
To make processing and plotting easier, we decided to store our results using a RData file. When we want to skip ahead to do the plotting, the data can be loaded from the RData file, when it is available. If one needs to rerun the processing, one simply deletes the RData file and executes the code.


We start by downloading the specified source file from URL and extyracting it locally

```r
dataProcess <- TRUE # set a flag for indicating when one needs to process the data....
# check if the RData file already exists...
if(file.exists("./data/StormData.RData")){
  load("./data/StormData.RData")
  dataProcess <- FALSE # set proessinf flag to false to skip cranking out the statistical analysis...
}

# so, if we need to generate statistical results....
if(dataProcess){
  # first, create a data dir if it doesn't exist
  if(!file.exists("./data")){
      dir.create("./data")
  }
  # second, load the compressed data file from the source URL into the data dir....
  if(!file.exists("./data/StormData.csv.bz2")){
    fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
    destPath <- "./data/StormData.csv.bz2"
    binData <- getBinaryURL(fileUrl, ssl.verifypeer=0L, followlocation=1L)
    destFileHandle <- file(destPath, open="wb")
    writeBin(binData,destFileHandle)
    close(destFileHandle)
  }
  # third, unzip the compressed file to extract the data file in CSV format...
  if(!file.exists("./data/StormData.csv")){
    filePath <- "./data/StormData.csv.bz2"
    destPath <- "./data/StormData.csv"
    bunzip2(filePath,destPath,overwrite=TRUE, remove=FALSE)
  }
}
```


##### _3c. Reading in and loading the data into R_
Read the source .csv file

```r
if(dataProcess){
  csvStormData <- read.csv("./data/StormData.csv")
}
```

##### _3d. Remove everything extraneous (not used for this analysis)_
Just keep the following data columns needed for our analysis: BGN_DATE, EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP

```r
if(dataProcess){
  neededColumns <- c("BGN_DATE", "EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")
  minStormData <- csvStormData[, neededColumns]
  minStormData <- rename(minStormData, c("FATALITIES"="fatalities", "INJURIES"="injuries"))
}
```

##### _3e. Refactoring BGN_DATE, determining the offset year to use, and reducing the dataset further_
Because earlier years hold inconsistent amounts of data, the data set could very well be skewed towards later years, since they correspondingly hold more data readings. Setting the cutoff point at 75% (an arbitrary assignment) we can still use the majority of the data set. 

```r
if(dataProcess){
  # First, we find out how many observations we have...
  totalNumberOfObservations <- nrow(minStormData)
  cutOffPercentage = 0.75
  cutOffObservations = round(totalNumberOfObservations * cutOffPercentage)
  
  # Second, we add columns for reformatted date calculations...
  minStormData$year = as.numeric(format(as.Date(minStormData$BGN_DATE, format = "%m/%d/%Y"), "%Y"))
  
  # Third, we create a dataset with counts per year, then reverse the recordset & create a running total (cumulative sum) ...
  yearRecords <- count(minStormData, "year")
  yearRecords <- yearRecords[order(yearRecords$year, decreasing=TRUE), ]
  yearRecords$runningTotal = cumsum(yearRecords$freq)
  cutOffYear <- min(yearRecords[yearRecords$runningTotal < cutOffObservations, 1])
  
  # Fourth, we reduce our dataset even further...
  minStormData <- minStormData[minStormData$year >= cutOffYear, ]
  endYear <- max(minStormData$year)
  
  # Fin ally, we clean up our final, reduced dataset...
  minStormData$BGN_DATE <- NULL
  rownames(minStormData) <- NULL
}
```

##### _3f. Refactoring Event Type (EVTYPE) into high-level categories_
The Event TYpe variable (EVTYPE) contains around 985 unique source events. With some effort, these could be organized into a high-level set of event categories, focused around their similarities. The result was a cqtegory listing of 11 types, covering the majority of the useful final data observations.

```r
if(dataProcess){
  # let's build the new 11 categories...
  minStormData$damageSource <- NA
  
  minStormData[grepl("precipitation|rain|hail|drizzle|wet|percip|burst|depression|fog|wall cloud", 
                         minStormData$EVTYPE, ignore.case = TRUE), "damageSource"] <- "Precipitation & Fog"
  minStormData[grepl("wind|storm|wnd|hurricane|typhoon", 
                         minStormData$EVTYPE, ignore.case = TRUE), "damageSource"] <- "Wind & Storm"
  minStormData[grepl("slide|erosion|slump", 
                         minStormData$EVTYPE, ignore.case = TRUE), "damageSource"] <- "Landslide & Erosion"
  minStormData[grepl("warmth|warm|heat|dry|hot|drought|thermia|temperature record|record temperature|record high", 
                         minStormData$EVTYPE, ignore.case = TRUE), "damageSource"] <- "Heat & Drought"
  minStormData[grepl("cold|cool|ice|icy|frost|freeze|snow|winter|wintry|wintery|blizzard|chill|freezing|avalanche|glaze|sleet", 
                         minStormData$EVTYPE, ignore.case = TRUE), "damageSource"] <- "Snow & Ice"
  minStormData[grepl("flood|surf|blow-out|swells|fld|dam break", 
                         minStormData$EVTYPE, ignore.case = TRUE), "damageSource"] <- "Flooding & High Surf"
  minStormData[grepl("seas|high water|tide|tsunami|wave|current|marine|drowning", 
                         minStormData$EVTYPE, ignore.case = TRUE), "damageSource"] <- "High seas"
  minStormData[grepl("dust|saharan", 
                         minStormData$EVTYPE, ignore.case = TRUE), "damageSource"] <- "Dust & Saharan winds"  
  minStormData[grepl("tstm|thunderstorm|lightning", 
                         minStormData$EVTYPE, ignore.case = TRUE), "damageSource"] <- "Thunderstorm & Lightning"
  minStormData[grepl("tornado|spout|funnel|whirlwind", 
                         minStormData$EVTYPE, ignore.case = TRUE), "damageSource"] <- "Tornado"
  minStormData[grepl("fire|smoke|volcanic", 
                         minStormData$EVTYPE, ignore.case = TRUE), "damageSource"] <- "Fire & Volcanic activity"
  
  # OK, so let's remove the NA's from our dataset....
  minStormData <- minStormData[complete.cases(minStormData[, "damageSource"]), ]
  minStormData$damageSource <- as.factor(minStormData$damageSource)
  
  # Now, let's clean up our dataset by removing the old event type categories...
  minStormData$EVTYPE <- NULL
}
```

##### _3g. Refactoring Property Damage (PROPDMG, CROPDMG, PROPDMGEXP & CROPDMGEXP) to absolute damage values_
We felt it would be good to reformat the DMG and DMGEXP fields into ones with absolute values. Of course, we removed all undefined EXP properties (e.g., +, ?) by setting the record they are in as an NA and subsequently removing them.

```r
if(dataProcess){
  # here we have a function that converts a symbol to a power of 10 (PROPDMGEXP & CROPDMGEXP)
  toTenPower <- function(x){
    if(is.numeric(x)) {
      x <- x
    }
    else if(grepl("h", x, ignore.case=TRUE)) {
      x <- 2
    }
    else if(grepl("k", x, ignore.case=TRUE)) {
      x <- 3
    }
    else if(grepl("m", x, ignore.case=TRUE)) {
      x <- 6
    }
    else if(grepl("b", x, ignore.case=TRUE)) {
      x <- 9
    }
    else if(x == "" || x == " "){
      x <- 0
    }
    else{
      x <- NA
    }
    x
  }
   
  # Here is a function to take two parameters, num and exp, and convert them to one absolute value. Any non-integers are set to 0...
  calculateAmount <- function(num, exp){
    pow <- toTenPower(exp)
    if(is.numeric(num)){
      num <- num * (10 ^ pow)
    }
    
    if(!is.numeric(num)){
      num <- 0
    }
    
    num
  }
  
  # Here we create 2 new fields for calculating propDamage & cropDamage and aggregating them to one damageTotal field...
  minStormData$propDamage <- mapply(calculateAmount, minStormData$PROPDMG, minStormData$PROPDMGEXP)
  minStormData$cropDamage <- mapply(calculateAmount, minStormData$CROPDMG, minStormData$CROPDMGEXP)
  minStormData$damageTotal = minStormData$propDamage + minStormData$cropDamage
  
  # Now, let's clean our dataset again by removing all the old damage value columns...
  minStormData$PROPDMG <- NULL
  minStormData$PROPDMGEXP <- NULL
  minStormData$CROPDMG <- NULL
  minStormData$CROPDMGEXP <- NULL
}
```

##### _3h. Creating aggregated datasets and variables for plots_
Now that we have set everything up, our final data frames must be recast to be used when plotting.

```r
if(dataProcess){
  # First, aggregate economic damage per damageSource...
  sumEconomicDamage <- aggregate(formula=cbind(propDamage, cropDamage, damageTotal) ~ damageSource, data=minStormData, FUN=sum, na.rm=TRUE)
  sumEconomicDamage <- sumEconomicDamage[order(sumEconomicDamage$damageTotal, decreasing=TRUE),]
  rownames(sumEconomicDamage) <- NULL
  sumEconomicDamage$damageSource <- factor(sumEconomicDamage$damageSource, levels=rev(sumEconomicDamage$damageSource))
  
  # Second, melt the sumEconomicDamage into the data frame so that we can use it in a bar chart...
  meltSumEconomicDamage <- melt(sumEconomicDamage, id.vars=c("damageSource"), measure.vars=c("propDamage","cropDamage"), variable.name="damageType", value.name="damage")
  levels(meltSumEconomicDamage$damageType)[levels(meltSumEconomicDamage$damageType)=="propDamage"] <- "property"
   levels(meltSumEconomicDamage$damageType)[levels(meltSumEconomicDamage$damageType)=="cropDamage"] <- "crops"
  
  # Third, aggregate human casualties per damageSource...
  sumHumanDamage <-aggregate(formula=cbind(injuries, fatalities) ~ damageSource, data=minStormData, FUN=sum, na.rm=TRUE) 
  sumHumanDamage <- sumHumanDamage[order(sumHumanDamage$injuries, decreasing=TRUE),]
  rownames(sumHumanDamage) <- NULL
  sumHumanDamage$damageSource <- factor(sumHumanDamage$damageSource, levels=rev(sumHumanDamage$damageSource))
  
  # Fourth, define max values for the bar chart scale...
  maxInjuries <- max(sumHumanDamage$injuries)
  maxInjuries <- maxInjuries + round(maxInjuries * 0.25)
 
  maxFatalities <- max(sumHumanDamage$fatalities)
  maxFatalities <- maxFatalities + round(maxFatalities * 0.25)  
}
```

##### _3i. Save the final dataset and other info to the RData file_
Now, we save all the processed data to an RData file (see 3b.)

```r
if(dataProcess){
  save(minStormData, 
       sumHumanDamage, 
       meltSumEconomicDamage,
       sumEconomicDamage, 
       maxInjuries, 
       maxFatalities,
       cutOffYear,
       endYear,
       file="./data/StormData.RData")
}
```


### 4. Results
##### _4a. Show the first & last 5 lines of the new, processed data set_
Let's show a few records of our cleaned, reformatted Storm Data dataset to be used for analysis...

```r
head(minStormData, n=5L)
```

```
##   fatalities injuries year             damageSource propDamage cropDamage
## 1          0        0 1996               Snow & Ice     380000      38000
## 2          0        0 1996                  Tornado     100000          0
## 3          0        0 1996 Thunderstorm & Lightning       3000          0
## 4          0        0 1996 Thunderstorm & Lightning       5000          0
## 5          0        0 1996 Thunderstorm & Lightning       2000          0
##   damageTotal
## 1      418000
## 2      100000
## 3        3000
## 4        5000
## 5        2000
```

```r
tail(minStormData, n=5L)
```

```
##        fatalities injuries year damageSource propDamage cropDamage
## 653526          0        0 2011 Wind & Storm          0          0
## 653527          0        0 2011 Wind & Storm          0          0
## 653528          0        0 2011 Wind & Storm          0          0
## 653529          0        0 2011   Snow & Ice          0          0
## 653530          0        0 2011   Snow & Ice          0          0
##        damageTotal
## 653526           0
## 653527           0
## 653528           0
## 653529           0
## 653530           0
```

##### _4b. Human Casualties - Injuries vs. Fatalities_
We compared, side-by-side, injuries and fatalaties sustained for each major weather event, orderd by the number of injuries. One can see that the top five causes for both contain the same events. However, Heat & Drought cause more fatalities than Tornados, which is overwhlemingly the leading cause of all human injuries. 


```r
# To do our comparison plt, first we add the middle column with just the damageSource labels...
g.mid <- ggplot(data=sumHumanDamage, aes(x=1,y=damageSource)) +
            geom_text(aes(label=damageSource), size=4) +
            ggtitle("") +
            ylab(NULL) +
            scale_x_continuous(expand=c(0,0),limits=c(0.94,1.065)) +
            theme(axis.title=element_blank(),
                  panel.grid=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks.y=element_blank(),
                  panel.background=element_blank(),
                  axis.text.x=element_text(color=NA),
                  axis.ticks.x=element_line(color=NA),
                  plot.margin = unit(c(1,-1,1,-1), "mm"))

# Next, we add the left chart with the human injuries...
g.injuries <- ggplot(data=sumHumanDamage, aes(x=damageSource, y=injuries)) +
            geom_bar(stat = "identity") + 
            geom_text(aes(label=injuries), size=3, vjust=0.5, hjust=2.0) +
            ggtitle("Injuries") +
            scale_y_reverse(expand=c(0, 0), limits=c(maxInjuries,0)) + 
            coord_flip() +
            theme(axis.title.x = element_blank(), 
                  axis.title.y = element_blank(), 
                  axis.text.y = element_blank(), 
                  axis.ticks.y = element_blank(), 
                  plot.margin = unit(c(1,-1,1,0), "mm")) 

# Next, we add the right chart with the humjan fatalities...
g.fatalities <- ggplot(data=sumHumanDamage, aes(x=damageSource, y=fatalities)) +
            geom_bar(stat = "identity") + 
            geom_text(aes(label=fatalities), size=3, vjust=0.5, hjust=-1.0) +
            ggtitle("Fatalities") +
            scale_y_continuous(expand=c(0, 0), limits=c(0,maxFatalities)) + 
            coord_flip() +
            theme(axis.title.x = element_blank(), 
                  axis.title.y = element_blank(), 
                  axis.text.y = element_blank(), 
                  axis.ticks.y = element_blank(), 
                  plot.margin = unit(c(1,0,1,-1), "mm")) 

# Set up chart title...
titleString <- paste("Aggregated Human Casualties [Injuries & Fatalities] for weather events from ",cutOffYear," to ",endYear, sep="")

# Finally, combine both charts into one graphic...
gg.injuries <- ggplot_gtable(ggplot_build(g.injuries))
gg.fatalities <- ggplot_gtable(ggplot_build(g.fatalities))
gg.mid <- ggplot_gtable(ggplot_build(g.mid))

grid.arrange(gg.injuries,gg.mid,gg.fatalities, ncol=3, widths=c(4/10,2/10,4/10), top=titleString)
```

![](TomLous_stormDataAnalysis_files/figure-html/unnamed-chunk-12-1.png) 

The underlying data

```r
sumHumanDamage
```

```
##                damageSource injuries fatalities
## 1                   Tornado    20670       1514
## 2  Thunderstorm & Lightning     9305       1049
## 3      Flooding & High Surf     8760       1483
## 4            Heat & Drought     7644       2047
## 5                Snow & Ice     3873       1150
## 6              Wind & Storm     3190        556
## 7       Precipitation & Fog     1812        170
## 8  Fire & Volcanic activity     1458         87
## 9                 High seas      763        618
## 10     Dust & Saharan winds      415         13
## 11      Landslide & Erosion       55         43
```

##### _4c. Economic Impact_
From the big picture perspective, we find that crop damage, overall, is hardly a factor in the total economic impact of all the weather types, with one exception. For Heat & Drought, crop damage accounts for over 90% of the economic impact. Another interesting result is that Wind & Storm and Flooding & High Surf, when factored together, account for more than 80% of all economic damage over all the years. However, Tornados, Thunderstorms and Snow & Ice, while having an overwhelming impact in the cost of human lives affected, only impact in a minor way with respect economic cost.

```r
ggplot(meltSumEconomicDamage, aes(x=damageSource, y=damage/1000000)) + 
  geom_bar(stat = "identity", aes(fill=damageType)) +
  xlab("Event Type") +
  theme(axis.text.x = element_text(angle = 45, size=8, hjust = 1, vjust = 1)) +
  ylab("Total Damage (millions of USD)") +
  ggtitle(paste("Aggregated Economic Impact - Property and Crop Damage - for weather events from ",cutOffYear," to ",endYear, sep=""))
```

![](TomLous_stormDataAnalysis_files/figure-html/unnamed-chunk-14-1.png) 

The underlying data

```r
sumEconomicDamage
```

```
##                damageSource   propDamage  cropDamage  damageTotal
## 1      Flooding & High Surf 159875724170  6349563200 166225287370
## 2              Wind & Storm 137984324660  6726848600 144711173260
## 3                   Tornado  24622829010   283425010  24906254020
## 4       Precipitation & Fog  15203426360  3225242250  18428668610
## 5            Heat & Drought   1057077300 13860159500  14917236800
## 6  Thunderstorm & Lightning   8662530360  1023891040   9686421400
## 7                Snow & Ice   6467844450  2816170100   9284014550
## 8  Fire & Volcanic activity   7761049500   402255130   8163304630
## 9                 High seas   4798122340    41022500   4839144840
## 10      Landslide & Erosion    327494100    20017000    347511100
## 11     Dust & Saharan winds      6157630     3100000      9257630
```
