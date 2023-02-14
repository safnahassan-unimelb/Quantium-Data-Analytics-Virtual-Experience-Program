#### Example code to install packages
install.packages("data.table")
install.packages("ggplot2")
install.packages("ggmosaic")
install.packages("readr")
install.packages("tidyverse")
install.packages("arules")
#### Load required libraries
library(data.table)
library(ggplot2)
library(ggmosaic)
library(readr)
library(arules)
#### Point the filePath to where you have downloaded the datasets to and
#### assign the data files to data.tables

filePath <- "D:/UniMelb/Quantium/Task 1/"
transactionData <- fread(paste0(filePath,"QVI_transaction_data.csv"))
customerData <- fread(paste0(filePath,"QVI_purchase_behaviour.csv"))

#### Examining the data
dim(customerData)
str(customerData)
head(customerData)
View(customerData)

dim(transactionData)
str(transactionData)
head(transactionData)
View(transactionData)

#### Convert DATE column to a date format
transactionData$DATE <- as.Date(transactionData$DATE, origin = "1899-12-30")

#### Examine PROD_NAME
summary(transactionData$PROD_NAME)
productWords <- data.table(unlist(strsplit(unique(transactionData[, PROD_NAME]), "
")))
setnames(productWords, 'words')
#### Removing digits
nonNumericProductWords <- data.table(unlist(gsub("[0-9]","", productWords$words)))
setnames(nonNumericProductWords,'words')
#### Removing special characters
filteredProductWords <- data.table(unlist(gsub("[[:punct:]]","", nonNumericProductWords$words)))
setnames(filteredProductWords,'words')
#### Remove salsa products
transactionData[, SALSA := grepl("salsa", tolower(PROD_NAME))]
transactionData <- transactionData[SALSA == FALSE, ][, SALSA := NULL]
#### Summary of data to check nulls and outliers
summary(transactionData)
#### Product quantity appears to have an outlier which we should investigate further. 
#### Investigate the case where 200 packets of chips are bought in one transaction.
outlier <- transactionData[PROD_QTY == 200]
#### 2 transactions in outliers, both have Loyalty Card num = 226000
#### Let's see if the customer has had other transactions
outlierCustomerTransactions <- transactionData[LYLTY_CARD_NBR == 226000]
#### remove outlier
transactionData <- transactionData[LYLTY_CARD_NBR != 226000]
#### Count the number of transactions by date
countByDate <- transactionData[, .N, by=DATE]
#### countByDate has 364 observations

#### Create a sequence of dates and join this the count of transactions by date
library(tidyverse)
transactions_by_day <- countByDate %>% complete(DATE = seq.Date(as.Date("2018-07-01"), as.Date("2019-06-30"), by="day"))
setnames(transactions_by_day,c("DATE","N"))

#### Setting plot themes to format graphs
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))
#### Plot transactions over time
ggplot(transactions_by_day, aes(x = DATE, y = N)) +
  geom_line() +
  labs(x = "Day", y = "Number of transactions", title = "Transactions over time") +
  scale_x_date(breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

#### Zoom in december transactions
decemberTransactions <- filter(transactions_by_day, transactions_by_day$DATE %between% c("2018-12-01","2019-01-01"))
ggplot(decemberTransactions, aes(x = DATE, y = N)) +
  geom_line() +
  labs(x = "Day", y = "Number of transactions", title = "Transactions in December") +
  scale_x_date(breaks = "1 day") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

#### Pack size
#### We can work this out by taking the digits that are in PROD_NAME
transactionData[, PACK_SIZE := parse_number(PROD_NAME)]
#### Always check your output
#### Let's check if the pack sizes look sensible
packSize <- transactionData[, .N, PACK_SIZE][order(PACK_SIZE)]

#### Let's plot a histogram of PACK_SIZE since we know that it is a categorical variable and not a continuous variable even though it is numeric.
#### Histogram showing the number of transactions by pack size.
hist(packSize$PACK_SIZE)

#### Brands
transactionData$BRAND <- word(transactionData$PROD_NAME,1)
#### Clean brand names
transactionData[BRAND == "RED", BRAND := "RRD"]

#### CUSTOMER DATA
#### Examining customer data
summary(customerData)
#### Merge transaction data to customer data
data <- merge(transactionData, customerData, all.x = TRUE)

#### Code to save dataset as a csv
fwrite(data, paste0(filePath,"QVI_data.csv"))

## Data analysis on customer segments

#### Total sales by LIFESTAGE and PREMIUM_CUSTOMER
totalSalesPerSegment <- data %>% group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>% summarise(sum(TOT_SALES))
setnames(totalSalesPerSegment,c("LIFESTAGE","PREMIUM_CUSTOMER","TOTAL_SALES"))
### Plot of Total sales per segment
ggplot(totalSalesPerSegment, aes(x = LIFESTAGE, y = TOTAL_SALES)) +
  geom_bar(stat = "identity", width = 0.5, position = "dodge") +
  facet_grid(.~ PREMIUM_CUSTOMER) +
  theme(axis.text.x = element_text(angle = 90))


#### Number of customers by LIFESTAGE and PREMIUM_CUSTOMER
numberOfCustomersInSegments = data %>% group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>% summarise(NUM_OF_CUSTOMERS = n_distinct(LYLTY_CARD_NBR))
### Plot for Number of customers by LIFESTAGE and PREMIUM_CUSTOMER
ggplot(numberOfCustomersInSegments, aes(x = LIFESTAGE, y = NUM_OF_CUSTOMERS)) +
  geom_bar(stat = "identity", width = 0.5, position = "dodge") +
  facet_grid(.~ PREMIUM_CUSTOMER) +
  theme(axis.text.x = element_text(angle = 90))


#### Average number of units per customer by LIFESTAGE and PREMIUM_CUSTOMER
averageUnitsPerCustomer = data %>% group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>% summarise(AVERAGE_UNITS = mean(PROD_QTY*PACK_SIZE))
### Plot of average units purchased in each segment
ggplot(averageUnitsPerCustomer, aes(x = LIFESTAGE, y = AVERAGE_UNITS)) +
  geom_bar(stat = "identity", width = 0.5, position = "dodge") +
  facet_grid(.~ PREMIUM_CUSTOMER) +
  theme(axis.text.x = element_text(angle = 90))

#### Average price per unit by LIFESTAGE and PREMIUM_CUSTOMER
averagePricePerUnit = data %>% group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>% summarise(AVERAGE_PRICE_PER_UNIT = mean(TOT_SALES/(PROD_QTY*PACK_SIZE)))
### Plot of average units purchased in each segment
ggplot(averagePricePerUnit, aes(x = LIFESTAGE, y = AVERAGE_PRICE_PER_UNIT)) +
  geom_bar(stat = "identity", width = 0.5, position = "dodge") +
  facet_grid(.~ PREMIUM_CUSTOMER) +
  theme(axis.text.x = element_text(angle = 90))

#### T-Test between mainstream vs premium and budget
#### midage and young singles and couples
tTestData <- filter(averagePricePerUnit, LIFESTAGE %in% c("MIDAGE SINGLES/COUPLES","YOUNG SINGLES/COUPLES"))
tTestData$PREMIUM_CUSTOMER[tTestData$PREMIUM_CUSTOMER != "Mainstream"] <- "NOT_MAINSTREAM"
t.test(AVERAGE_PRICE_PER_UNIT ~ PREMIUM_CUSTOMER, data=tTestData)

#### t = 5.9662, df = 2.2393, p-value = 0.02051
#### alternative hypothesis: true difference in means between group Mainstream and group NOT_MAINSTREAM is not equal to 0
#### 95 percent confidence interval:
#### 0.000547574 0.002600530
#### sample estimates:
#### mean in group Mainstream mean in group NOT_MAINSTREAM 
#### 0.02404217                   0.02246812 

### Deep dive into Mainstream, young singles/couples
mainstreamYoungSC <- filter(data, LIFESTAGE %in% c("YOUNG SINGLES/COUPLES") & PREMIUM_CUSTOMER %in% c("Mainstream"))
rules <- apriori(mainstreamYoungSC, parameter = list(minlen=2, maxlen=10,supp=.7, conf=.5))
inspect(rules)
# lhs                                   rhs                                 support confidence  coverage lift count
# [1] {PROD_QTY=[2,5]}                   => {PREMIUM_CUSTOMER=Mainstream}     0.8472165  1.0000000 0.8472165    1 16558
# [2] {PREMIUM_CUSTOMER=Mainstream}      => {PROD_QTY=[2,5]}                  0.8472165  0.8472165 1.0000000    1 16558
# [3] {PROD_QTY=[2,5]}                   => {LIFESTAGE=YOUNG SINGLES/COUPLES} 0.8472165  1.0000000 0.8472165    1 16558
# [4] {LIFESTAGE=YOUNG SINGLES/COUPLES}  => {PROD_QTY=[2,5]}                  0.8472165  0.8472165 1.0000000    1 16558
# [5] {PREMIUM_CUSTOMER=Mainstream}      => {LIFESTAGE=YOUNG SINGLES/COUPLES} 1.0000000  1.0000000 1.0000000    1 19544
# [6] {LIFESTAGE=YOUNG SINGLES/COUPLES}  => {PREMIUM_CUSTOMER=Mainstream}     1.0000000  1.0000000 1.0000000    1 19544
# [7] {PROD_QTY=[2,5],                                                                                                 
# PREMIUM_CUSTOMER=Mainstream}      => {LIFESTAGE=YOUNG SINGLES/COUPLES} 0.8472165  1.0000000 0.8472165    1 16558
# [8] {PROD_QTY=[2,5],                                                                                                 
# LIFESTAGE=YOUNG SINGLES/COUPLES}  => {PREMIUM_CUSTOMER=Mainstream}     0.8472165  1.0000000 0.8472165    1 16558
# [9] {LIFESTAGE=YOUNG SINGLES/COUPLES,                                                                                
# PREMIUM_CUSTOMER=Mainstream}      => {PROD_QTY=[2,5]}                  0.8472165  0.8472165 1.0000000    1 16558

