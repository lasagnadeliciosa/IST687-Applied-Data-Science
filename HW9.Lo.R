#IST687.M006
#Jen Yeu Lo
#Homework 9
#Due date: 11/07/2018, Date Submitted: 11/06/2018

#Part A: Explore the Data Set
#1)
setwd("/Users/jenyeulo/Documents/Syracuse_University/IST687/csv_files/")#set my working directory
library(RJSONIO)#load RJSONIO library, this specific json library is required in order for the below codes to work.

hoteljson <- fromJSON("hotelSurveyBarriot.json", simplify = TRUE, nullValue = NA) #read the json file

#2)
hotelSurvey <- data.frame(hoteljson)

#Part B: Explore the Data Set
#1)
str(hotelSurvey)
#View(hotelSurvey)
#2)
bucketfunction <- function(vec){
  vBuckets <- replicate(length(vec), "Average")
  vBuckets[vec > 7] <- "High"
  vBuckets[vec < 7] <- "Low"
  return(vBuckets)
}

bucketfunction2 <- function(vec){
  q <- quantile(vec, c(0.4, 0.6))
  vBuckets <- replicate(length(vec), "Average")
  vBuckets[vec <= q[1]] <- "Low"
  vBuckets[vec > q[2]] <- "High"
  return(vBuckets)
}
#mapping numeric attribute to categories:
ocs <- hotelSurvey$overallCustSat <- bucketfunction(hotelSurvey$overallCustSat)
checkin<- hotelSurvey$checkInSat <- bucketfunction(hotelSurvey$checkInSat)
clean <- hotelSurvey$hotelClean <- bucketfunction(hotelSurvey$hotelClean)
friendly <- hotelSurvey$hotelFriendly <- bucketfunction(hotelSurvey$hotelFriendly)
size <- hotelSurvey$hotelSize <- bucketfunction2(hotelSurvey$hotelSize)
age <- hotelSurvey$guestAge <- bucketfunction2(hotelSurvey$guestAge)
length <- hotelSurvey$lengthOfStay <- bucketfunction2(hotelSurvey$lengthOfStay)
when <- hotelSurvey$whenBookedTrip <- bucketfunction2(hotelSurvey$whenBookedTrip)

#3)
tableAge <- table(hotelSurvey$guestAge) #show the count for guest age
tableAge
tableFriendly <- table(hotelSurvey$hotelFriendly)
tableFriendly

#4)
#convert the results in question 3 into percentage.
prop.table(tableAge) #make it into %
prop.table(tableFriendly)

#5)
prop.table(table(hotelSurvey$overallCustSat, hotelSurvey$guestAge, dnn=c("CustSat","guestAge"))) #y-axis is satisfaction, x-axis is age.

#Guests with low age are likely to give average satisfaction followed by high and low.
#Guests with average age are likely to give high satisfaction followed by average.
#Guests with high age are more likely to give average satisfaction rating.

#Part C: Coerce the data frame into transactions
#6)
#install.packages("arules")
#install.packages("arulesViz")
library(arules)
library(arulesViz)

#7)
dataframe <- data.frame(ocs, checkin, clean, friendly, size, age, length) #create new dataframe from question #2.
hotelSurveyX <- as(dataframe,"transactions")

#8)
inspect(head(hotelSurveyX)) #used head function due to too many outputs in the console.
itemFrequency(hotelSurveyX)
itemFrequencyPlot(hotelSurveyX)

#Part D: Use arules to discover patterns
#9)
hotelrules <- apriori(hotelSurveyX, parameter=list(support=0.2, confidence=0.8), appearance = list(default="lhs", rhs=c("ocs=High")))

#10)
inspect(hotelrules)
rules_confidence <- sort(hotelrules, by="confidence", decreasing = TRUE) #sort confidence by highest values first.
inspect(rules_confidence)

#11)
#It would be Rule 1 and 2 from rules_confidence.
#Rule 1 hav high checkin satisfaction, clean hotel, and high guest age with high confidence, support and lift values.
#Followed by Rule 2 that also have high checkin satisfaction, clean hotel, with a difference in average friendliness.
#In conclusion the hotel management should definitely focus on having high check in satisfaction along with clean hotel in order to maintain a high overall customer satisfaction.
