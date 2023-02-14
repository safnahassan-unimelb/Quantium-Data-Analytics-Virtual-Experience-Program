#### Load required libraries and datasets
library(data.table)
library(ggplot2)
library(tidyr)
library(dplyr)

#### Point the filePath to where you have downloaded the datasets to and
#### assign the data files to data.tables
filePath <- "D:/UniMelb/Quantium/Task 2/"

data <- fread(paste0(filePath,"QVI_data.csv"))
#### Set themes for plots
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))
### Selected store numbers 77, 86 and 88 as trial stores
### Selected Metrics:
### - Monthly overall sales revenue
### - Monthly number of customers
### - Monthly number of transactions per customer
### Calculate these measures over time for each store

#### Add a new month ID column in the data with the format yyyymm.
data$YEARMONTH <- format(data$DATE, "%Y%m")
#### Total Sales for each store and month
#### number of customers for each store and month
#### transactions per customer for each store and month
#### chips per customer for each store and month
#### average price per unit for each store and month
measureOverTime <- data %>% group_by(STORE_NBR,YEARMONTH) %>% summarise(
  totSales = sum(TOT_SALES),
  nCustomers = uniqueN(LYLTY_CARD_NBR),
  nTxnPerCust = uniqueN(TXN_ID)/uniqueN(LYLTY_CARD_NBR), 
  nChipsPerTxn = sum(PROD_QTY)/uniqueN(TXN_ID), 
  avgPricePerUnit = (sum(TOT_SALES)/sum(PROD_QTY)))

#### Filter to the pre-trial period and stores with full observation periods
storesWithFullObs <- as.data.table(table(measureOverTime$STORE_NBR))

storesWithFullObs <- storesWithFullObs %>% filter(N==12)

storesWithFullObs<-setNames(storesWithFullObs,c("STORE_NBR","N"))

preTrialMeasures <- measureOverTime %>% filter(YEARMONTH < 201902,STORE_NBR %in% storesWithFullObs$STORE_NBR)


#### Function to calculate correlation for a measure, looping through each control store.
calculateCorrelation <- function(preTrialMeasures,trialStore_sales,trialStoreN){
  
  calTable = data.table(Store1 = numeric(), Store2 = numeric(), corr_measure = numeric())
  
  stN <- preTrialMeasures %>% select(STORE_NBR)
  
  for(i in stN$STORE_NBR){
    
    contSt <- preTrialMeasures %>% filter(STORE_NBR==i)
    contSt <- contSt %>% select(nCustomers)
    
    calMeasure = data.table("Store1" = trialStoreN, "Store2" = i, "corr_measure" = cor(trialStore_sales$nCustomers,contSt$nCustomers))
    
    calTable <- rbind(calTable, calMeasure) }
  return(calTable)
}



#### Create a function to calculate a standardised magnitude distance for a measure

calculateMagnitudeDistance <- function(preTrialMeasures,trialStore_sales,trial_storeN){
  calTable = data.table(Store1 = numeric(), Store2 = numeric(), YEARMONTH = numeric(),mag_measure = numeric())
  stN <- preTrialMeasures %>% select(STORE_NBR)
  for(i in stN$STORE_NBR){
    contSt <- preTrialMeasures %>% filter(STORE_NBR==i)
    contSt <- contSt %>% select(totSales)
    calMeasure = data.table("Store1" = trial_storeN, "Store2" = i, "YEARMONTH" = preTrialMeasures$YEARMONTH ,"mag_measure" = abs(trialStore_sales$totSales - contSt$totSales))
    
    calTable <- rbind(calTable,calMeasure) 
    calTable <- unique(calTable)
  }
  return(calTable)
}
###Standardize
standMag <- function(magnitude_nSales) {
  minMaxDist <- magnitude_nSales[, .(minDist = min( magnitude_nSales$mag_measure), maxDist = max(magnitude_nSales$mag_measure)), by = c("Store1", "YEARMONTH")]
  distTable <- merge(magnitude_nSales, minMaxDist, by = c("Store1", "YEARMONTH"))
  distTable[, magnitudeMeasure := 1 - (mag_measure - minDist)/(maxDist - minDist)]
  finalDistTable <- distTable[, .(magN_measure = mean(magnitudeMeasure)), by = .(Store1, Store2)]
  return(finalDistTable)
}

#### Use the function you created to calculate correlations against store 77 using total sales and number of customers.
trial_store <- 77
trialStore_sales <- preTrialMeasures %>% filter(STORE_NBR ==77)
trialStore_sales <- trialStore_sales %>% select(STORE_NBR,YEARMONTH,totSales,nCustomers)
corr_nSales <- calculateCorrelation(preTrialMeasures,trialStore_sales,trial_store)

corr_nSales <- unique(corr_nSales)

corr_nCustomers <- calculateCorrelation(preTrialMeasures, trialStore_sales, trial_store )

corr_nCustomers <- unique(corr_nCustomers)
#### Use the functions for calculating magnitude
magnitude_nSales <- calculateMagnitudeDistance(preTrialMeasures, trialStore_sales, trial_store)
magnitude_nSales <- standMag1(magnitude_nSales)
magnitude_nCustomers <- calculateMagnitudeDistance(preTrialMeasures,trialStore_sales, trial_store)
magnitude_nCustomers <- standMag(magnitude_nCustomers)

#### Create a combined score composed of correlation and magnitude, by first merging the correlations table with the magnitude table.
corr_weight <- 0.5

score_nSales <- merge(corr_nSales,magnitude_nSales, by = c("Store1", "Store2"))

score_nSales <- score_nSales %>% mutate(scoreNSales = (score_nSales$corr_measure * corr_weight)+(score_nSales$magN_measure * (1 - corr_weight)))

score_nCustomers <- merge(corr_nCustomers,magnitude_nCustomers, by = c("Store1", "Store2"))

score_nCustomers <- score_nCustomers %>% mutate(scoreNCust = (score_nCustomers$corr_measure * corr_weight)+(score_nCustomers$magN_measure * (1 - corr_weight)))

score_Control <- merge(score_nSales,score_nCustomers, by = c("Store1", "Store2"))

score_Control <- score_Control %>% mutate(finalControlScore = (scoreNSales * 0.5) + (scoreNCust * 0.5))

#### Select control stores based on the highest matching store (closest to 1 but
#### not the store itself, i.e. the second ranked highest store)

control_store <- score_Control[order(-finalControlScore),]

control_store <- control_store$Store2

control_store <- control_store[2]

#### Visual checks on trends based on the drivers
measureOverTimeSales <- as.data.table(measureOverTime)
data$YEARMONTH <- as.numeric(as.character(data$YEARMONTH))

pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == trial_store, "Trial",ifelse(STORE_NBR == control_store,"Control", "Other stores"))][, totSales := mean(totSales), by = c("YEARMONTH","Store_type")][, TransactionMonth := as.Date(paste(as.numeric(YEARMONTH) %/%100, as.numeric(YEARMONTH) %% 100, 1, sep = "‐"), "%Y‐%m‐%d")][as.numeric(YEARMONTH) < 201903 , ]

##Visualize
ggplot(pastSales, aes(TransactionMonth, totSales, color = Store_type)) + geom_line() + labs(x = "Month of Operation", y = "Total Sales", title = "Total Sales by Month")

### Number of Customers
measureOverTimeCusts <- as.data.table(measureOverTime)
pastCustomers <- measureOverTimeCusts[, Store_type := ifelse(STORE_NBR == trial_store, "Trial",ifelse(STORE_NBR == control_store,"Control", "Other stores"))][, numberCustomers := mean(nCustomers), by = c("YEARMONTH","Store_type")][, TransactionMonth := as.Date(paste(YEARMONTH %/%100, YEARMONTH %% 100, 1, sep = "‐"), "%Y‐%m‐%d")][YEARMONTH < 201903 ]
###Visualize
ggplot(pastCustomers, 
       aes(TransactionMonth, numberCustomers, color = Store_type)) + 
  geom_line() + 
  labs(x = "Month of Operation", y = "Total Number of Customers", title = "Total Number of Customers by Month")

## Assessment of trial
preTrialMeasures <- as.data.table(preTrialMeasures)

scalingFactorForControlSales <- preTrialMeasures[STORE_NBR == trial_store & YEARMONTH < 201902, sum(totSales)]/preTrialMeasures[STORE_NBR == control_store & YEARMONTH < 201902, sum(totSales)]

##Applying the Scaling Factor
measureOverTimeSales <- as.data.table(measureOverTime)

scaledControlSales <- measureOverTimeSales[STORE_NBR == control_store, ][ , controlSales := totSales * scalingFactorForControlSales ]

measureOverTime <- as.data.table(measureOverTime)

percentageDiff <- merge(scaledControlSales[, c("YEARMONTH", "controlSales")], measureOverTime[STORE_NBR == trial_store, c("totSales", "YEARMONTH")], by = "YEARMONTH")[ , percentageDiff := abs(controlSales - totSales)/controlSales]
#### As our null hypothesis is that the trial period is the same as the pre-trial period, let's take the standard deviation based on the scaled percentage difference in the pre-trial period 

stdDev <- sd(percentageDiff[YEARMONTH < 201902, percentageDiff])

#### Note that there are 8 months in the pre-trial period
#### hence 8 - 1 = 7 degrees of freedom
degreesOfFreedom <- 7 

#### We will test with a null hypothesis of there being 0 difference between trial and control stores.
#### Calculate the t-values for the trial months. 
percentageDiff[ , tvalue := (percentageDiff - 0)/stdDev][ , TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"),"%Y-%m-%d")][YEARMONTH < 201905 & YEARMONTH > 201901, .(TransactionMonth, tvalue)]

#### Also,find the 95th percentile of the t distribution with the appropriate degrees of freedom to check whether the hypothesis is statistically significant.

qt(0.95, df = degreesOfFreedom)

#measureOverTimeSales <- as.data.table(measureOverTime)
pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR ==trial_store, "Trial", ifelse(STORE_NBR == control_store,"Control", "Other stores"))][, totSales := mean(totSales), by = c("YEARMONTH","Store_type")][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "‐"), "%Y‐%m‐%d")][Store_type %in% c("Trial", "Control"), ]

#pastSales <- as.data.table(pastSales)

### Control Store 95th percentile
pastSales_Controls95 <- pastSales[ Store_type == "Control" , ][, totSales := totSales * (1 + stdDev * 2)][, Store_type := "Control 95th % confidence"]

### Control Store 5th percentile
pastSales_Controls5 <- pastSales[Store_type == "Control" , ][, totSales := totSales * (1 - stdDev * 2)][, Store_type := "Control 5th % confidence"]

trialAssessment <- rbind(pastSales, pastSales_Controls95, pastSales_Controls5)


### Visualize
ggplot(trialAssessment, aes(TransactionMonth, totSales, color = Store_type)) + geom_rect(data = trialAssessment[ YEARMONTH < 201905 & YEARMONTH > 201901 ,], aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth), ymin = 0 , ymax = Inf, color = NULL), show.legend = FALSE) + geom_line() + labs(x = "Month of operation", y = "Total sales", title = "Total sales by month")

preTrialMeasures <- as.data.table(preTrialMeasures)

scalingFactorForControlCusts <- preTrialMeasures[STORE_NBR == trial_store & YEARMONTH < 201902, sum(nCustomers)]/preTrialMeasures[STORE_NBR == control_store & YEARMONTH < 201902, sum(nCustomers)]

measureOverTimeCusts <- as.data.table(measureOverTime)

scaledControlCustomers <- measureOverTimeCusts[STORE_NBR == control_store, ][, controlCustomers := nCustomers * scalingFactorForControlCusts][,Store_type := ifelse(STORE_NBR == trial_store, "trial", ifelse(STORE_NBR == control_store,"control","Other Store"))]

###Calculate the % difference between scaled control sales and trial sales
percentageDiff <- merge(scaledControlCustomers[, c("YEARMONTH", "controlCustomers")], measureOverTimeCusts[STORE_NBR == trial_store, c("nCustomers", "YEARMONTH")], by = "YEARMONTH")[ , percentageDiff := abs(controlCustomers - nCustomers)/controlCustomers]
#### As our null hypothesis is that the trial period is the same as the pre-trial period, let's take the standard deviation based on the scaled percentage difference in the pre-trial period 
stdDev <- sd(percentageDiff[YEARMONTH < 201902 , percentageDiff]) 

degreesOfFreedom <- 7

#### Trial and control store number of customers
measureOverTimeCusts <- as.data.table(measureOverTime)
pastCustomers <- measureOverTimeCusts[, Store_type := ifelse(STORE_NBR == trial_store, "Trial", ifelse(STORE_NBR == control_store, "Control", "Other stores"))][, nCusts := mean(nCustomers), by = c("YEARMONTH","Store_type")][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "‐"), "%Y‐%m‐%d")
][Store_type %in% c("Trial", "Control"), ]

###Control 95th percentile
pastCustomers_Control95 <- pastCustomers[Store_type == "Control",][, nCusts := nCusts * (1 + stdDev * 2)][, Store_type := "Control 95th % confidence interval"]

###Control 5th percentile
pastCustomers_Control5 <- pastCustomers[Store_type == "Control",][, nCusts := nCusts * (1 + stdDev * 2)][, Store_type := "Control 5th % confidence interval"]

trialAssessment <- rbind(pastCustomers,pastCustomers_Control95,pastCustomers_Control5)

###Visualize
ggplot(trialAssessment, aes(TransactionMonth, nCusts, color = Store_type)) + geom_rect(data = trialAssessment[YEARMONTH < 201905 & YEARMONTH > 201901 , ], aes(xmin = min(TransactionMonth), xmax =  max(TransactionMonth), ymin = 0, ymax = Inf, coor = NULL), show.legend = F) + geom_line(aes(linetype = Store_type)) + labs(x = "Month Of Operation", y = "Total Number of Customers", title = "Total Number of Customers by Month")

##Trial Store 86

data <- as.data.table(data)
measureOverTime <- data[, .(totSales = sum(TOT_SALES), nCustomers = uniqueN(LYLTY_CARD_NBR), nTxnPerCust = uniqueN(TXN_ID)/uniqueN(LYLTY_CARD_NBR), nChipsPerTxn = sum(PROD_QTY)/uniqueN(TXN_ID), avgPricePerUnit = sum(TOT_SALES)/sum(PROD_QTY)), by = c("STORE_NBR", "YEARMONTH")][order(STORE_NBR,YEARMONTH)]

### USe the fucntions for calculating correlation
trial_store <- 86

trialStore_sales <- preTrialMeasures %>% filter(STORE_NBR ==86)
trialStore_sales <- trialStore_sales %>% select(STORE_NBR,YEARMONTH,totSales,nCustomers)

corr_nSales <- calculateCorrelation(preTrialMeasures,trialStore_sales,trial_store)
corr_nSales <- unique(corr_nSales)

corr_nCustomers <- calculateCorrelation(preTrialMeasures, trialStore_sales, trial_store )
corr_nCustomers <- unique(corr_nCustomers)

#### Use the functions for calculating magnitude
magnitude_nSales <- calculateMagnitudeDistance(preTrialMeasures, trialStore_sales, trial_store)
magnitude_nSales <- standMag(magnitude_nSales)
magnitude_nCustomers <- calculateMagnitudeDistance(preTrialMeasures,trialStore_sales, trial_store)
magnitude_nCustomers <- standMag(magnitude_nCustomers)

#### Now, create a combined score composed of correlation and magnitude

corr_weight <- 0.5

score_nSales <- merge(corr_nSales,magnitude_nSales, by = c("Store1", "Store2"))
score_nSales <- score_nSales %>% mutate(scoreNSales = (score_nSales$corr_measure * corr_weight)+(score_nSales$magN_measure * (1 - corr_weight)))

score_nCustomers <- merge(corr_nCustomers,magnitude_nCustomers, by = c("Store1", "Store2"))
score_nCustomers <- score_nCustomers %>% mutate(scoreNCust = (score_nCustomers$corr_measure * corr_weight)+(score_nCustomers$magN_measure * (1 - corr_weight)))


#### Finally, combine scores across the drivers using a simple average.

score_Control <- merge(score_nSales,score_nCustomers, by = c("Store1", "Store2"))
score_Control <- score_Control %>% mutate(finalControlScore = (scoreNSales * 0.5) + (scoreNCust * 0.5))


#### Select control stores based on the highest matching store
#### (closest to 1 but not the store itself, i.e. the second ranked highest store)

control_store <- score_Control[order(-finalControlScore),]
control_store <- control_store$Store2
control_store <- control_store[2]

#### 155 - control store for trial store 86.
measureOverTimeSales <- as.data.table(measureOverTime)

pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == trial_store, "Trial", ifelse(STORE_NBR == control_store, "Control", "Other stores"))][, totSales := mean(totSales), by = c("YEARMONTH","Store_type")][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "‐"), "%Y‐%m‐%d")][YEARMONTH < 201903 , ]


###Visualize
ggplot(pastSales, aes(TransactionMonth, totSales, color = Store_type)) +
  geom_line(aes(linetype = Store_type)) +
  labs(x = "Month of operation", y = "Total sales", title = "Total sales by month")

measureOverTimeCusts <- as.data.table(measureOverTime)

pastCustomers <- measureOverTimeCusts[, Store_type := ifelse(STORE_NBR == trial_store, "Trial", ifelse(STORE_NBR == control_store, "Control", "Other stores"))][, nCusts := mean(nCustomers), by = c("YEARMONTH","Store_type")][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "‐"), "%Y‐%m‐%d")][YEARMONTH < 201903 ,] 

###Visualize
ggplot(pastCustomers, aes(TransactionMonth, nCusts, color = Store_type)) +
  geom_line() + labs(x = "Month of operation", y = "Total number of customers", title = "Total number of customers by month")
