#IST687.M006
#Jen Yeu Lo
#Homework 5
#Due date: 10/03/2018, Date Submitted: 10/03/2018


#Step A: Load the data
#1)
library(jsonlite)
library(dplyr)
accidentsURL <- "http://data.maryland.gov/api/views/pdvh-tf2u/rows.json?accessType=DOWNLOAD" #grab the data from the web
json_lists <- fromJSON(accidentsURL) #load in the data but they are in lists.

dfAccidents <- json_lists[['data']] #extract 'data' within the list.
dfAccidents <- as.data.frame(dfAccidents) #converts the list into dataframe

#Step B: Clean the data
#2)
dfAccidents <- dfAccidents[,-1:-8] #removes the first 8 columns.

#3)
colnames(dfAccidents) <- c("CASE_NUMBER","BARRACK","ACC_DATE","ACC_TIME","ACC_TIME_CODE","DAY_OF_WEEK","ROAD","INTERSECT_ROAD","DIST_FROM_INTERSECT","DIST_DIRECTION","CITY_NAME","COUNTY_CODE","COUNTY_NAME","VEHICLE_COUNT","PROP_DEST","INJURY","COLLISION_WITH_1","COLLISION_WITH_2")
View(dfAccidents)

#Step C: Explore the data - using the dataframe you created.

#4)
countInjury <- length(which(dfAccidents$INJURY == "YES")) #count the number of cases with injuries.
countInjury

#5)
dfAccidents$DAY_OF_WEEK <- gsub("\\s", "", dfAccidents$DAY_OF_WEEK) #removes the white spaces after days of week.
sundayInjury <- length(which(dfAccidents$DAY_OF_WEEK == "SUNDAY")) #SUNDAY has three spaces after it.
sundayInjury

sundaytable <- table(dfAccidents$DAY_OF_WEEK) #another method to get the sunday values.
sundaytable[4] #shows the Sunday column with its value.

#6)
injuryDays <- table(dfAccidents$DAY_OF_WEEK, dfAccidents$INJURY) #creates a table that tabulates injuries in days of week.
injuryDays[,2] #displays only the second column, which is "YES".

#Step D: Explore the data - using dplyr
#7)
dfGroupByInjuries <- group_by(dfAccidents, INJURY)
a <- summarise(dfGroupByInjuries, Injuries = sum(INJURY == "YES", na.rm = TRUE))
a #total number of accidents is 6433

#Alternative method to get the same result.
dd <- filter(dfAccidents, INJURY == "YES") 
nrow(dd) #count the number of rows where the INJURY == "YES"

#8)
dfGroupBySundays <- group_by(dfAccidents, DAY_OF_WEEK)
b <- summarise(dfGroupBySundays, SundayAccidents = sum(DAY_OF_WEEK == "SUNDAY"))
b #accidents that happened on sunday is 2373.

#9)
dfGroupByInjuries <- group_by(dfAccidents, DAY_OF_WEEK)
c <- summarise(dfGroupByInjuries, Injuries = sum(INJURY == "YES", na.rm = TRUE))
c #displays total number of injuries by days of week.

#10)
#I think dpyl is easier because it follows a set syntax that allows you to sort through data in many ways,
#all you needed to make sure of is to plug the correct variables and parameters.

#Step D: Explore the distribution of the number of vehicles in accidents
#11)
position = which(dfAccidents$DAY_OF_WEEK == "FRIDAY") #find the indexes for fridays
totalFriday = dfAccidents$VEHICLE_COUNT[position] #mark the number of cars that occured in each accidents on friday.
#totalFriday
totalFriday = as.numeric(totalFriday) #convert it to numeric value
hist(totalFriday,
     main = "Number of Vehicles involved in Friday accidents",
     xlab = "Number of Vehicles involved",
     ylab = "Number of incidents",
     breaks = 30)
quantile(totalFriday, c(0.05,0.95), na.rm = TRUE)

#Alternative method to create the same histogram
fridayAccidents <- subset(dfAccidents, dfAccidents$DAY_OF_WEEK == "FRIDAY") #Select accidents that occured only on Friday.
hist(as.numeric(fridayAccidents$VEHICLE_COUNT)) #create histogram of number of vehicles that involved in accidents on Friday.


#12)
position = which(dfAccidents$DAY_OF_WEEK == "SUNDAY") #find the indexes for sunday.
totalSunday = dfAccidents$VEHICLE_COUNT[position] #mark the number of cars that occured in each accidents on sunday.
#totalSunday
totalSunday = as.numeric(totalSunday) #convert it to numeric value
hist(totalSunday,
     main = "Number of Vehicles involved in Sunday accidents",
     xlab = "Number of Vehicles involved",
     ylab = "Number of incidents",
     breaks = 30)

#On Friday the majority of the accidents mostly two vehicles, while on Sunday it was mostly one.

#Alternative method to create the same histogram
sundayAccidents <- subset(dfAccidents, dfAccidents$DAY_OF_WEEK == "SUNDAY") #Select accidents that occured only on Friday.
hist(as.numeric(sundayAccidents$VEHICLE_COUNT)) #create histogram of number of vehicles that involved in accidents on Friday.
