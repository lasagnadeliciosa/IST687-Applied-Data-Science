#IST687.M006
#Jen Yeu Lo
#Homework 3
#Due date: 09/20/2018, Date Submitted: 09/18/2018

#Step A: Use read.csv( ) and url( ) to read a CSV file form the web into a data frame
#1)
dfStates <- read.csv(file = "~/Documents/Syracuse_University/IST687/csv_files/states.csv", header = T, stringsAsFactors = FALSE)
#used the CSV file instead due to the url being unstable at times.

#Step B: Clean the dataframe
#2)
View(dfStates) #Getting a view of what the df looks like as a whole.
head(dfStates[5]) #Examining the first 5 rows to find unneeded value/s.
tail(dfStates[5]) #Examining the last 5 rows to find unneeded value/s.

#3)
dfStates <- dfStates[,-1:-4] #removes the first 4 columns.

#4 and 5)
dfStates <- dfStates[2:52,] #we keep the states while removing the unneeded row on the top and bottom of the df, including Puerto Rico.

#6)
columnNames <- colnames(dfStates) #changing the names of the columns.
columnNames[1] <- "stateName"
columnNames[2] <- "population"
columnNames[3] <- "popOver18"
columnNames[4] <- "percentOver18"
colnames(dfStates) <- columnNames #replace the old values with the new.

#Step C: Create a function
#7)
stateFunction <- function(){
  dfStates <- read.csv(file = "~/Documents/Syracuse_University/IST687/csv_files/states.csv", header = T, stringsAsFactors = FALSE)
  dfStates <- dfStates[,-1:-4] #removes the first 4 columns.
  dfStates <- dfStates[2:52,] #we keep the states while removing the unneeded row on the top and bottom of the df, including Puerto Rico.
  columnNames <- colnames(dfStates) #changing the names of the columns.
  columnNames[1] <- "stateName"
  columnNames[2] <- "population"
  columnNames[3] <- "popOver18"
  columnNames[4] <- "percentOver18"
  colnames(dfStates) <- columnNames #replace the old values with the new.
  return(dfStates)
}

#Step D: Explore the dataframe
#8)
mean(dfStates$population)
#6386651

#9)
highestPop <- which.max(dfStates$population)
dfStates$stateName[highestPop]
#California

#10)
hist(dfStates$population, breaks = 14)
#There are 15 states with population less than 2 million. One state with a population approaching 40 million.
#41 states has a population of under 10 million. Only 9 states has a population that exceeded 10 million.

#11)
sortedDataFrame <- dfStates[order(dfStates$population),]
sortedDataFrame #df sorted from the lowest population to the highest.

#12)
head(sortedDataFrame,10) #Display the 10 states with the lowest population.

#13)
barplot(sortedDataFrame$population) #barplot displaying states with lowerst population to highest.
#The 4th largest state had a 7 million population difference with the 5th largest state.
#The largest 4 states accouts for 33.3% of all the population in the US.

sum4Largest <- sum(sortedDataFrame$population[48:51]) #these are my own codes that analyze the data for the biggest 4 states.
sum4Largest/(sum(sortedDataFrame$population))

