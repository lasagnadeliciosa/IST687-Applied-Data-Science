#IST687.M006
#Jen Yeu Lo
#Homework 4
#Due date: 09/26/2018, Date Submitted: 09/25/2018

#Part A: Write a function to reveal the distribution of a vector of numeric values
#1 & 2 & 4)
printVecInfo <- function(n){
  mean <- mean(n)
  cat("Mean: ",mean,"\n")
  median <- median(n)
  cat("Median: ",median,"\n")
  min <- min(n)
  cat("Min: ",min,"\n")
  max <- max(n)
  cat("Max: ",max,"\n")
  sd <- sd(n)
  cat("St.Dev: ",sd,"\n")
  quantile <- quantile(n, c(0.05, 0.95))
  cat("Quantile: ",quantile,"\n")
} #created the printVecInfo function with all the required functions in it. Also wrote in the cat function so that labels in Exercise 4 would show up properly.

#3)
testVector <- 1:10 #creating the test vector from 1:10.
printVecInfo(testVector) #testing the function with testVector.

#Part B: Read the census dataset
#5)
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
}#this was the same function that was created in HM3
dfStates <- stateFunction() #storing the function output into a variable.

#6)
firstSample <- sample(dfStates$population, size = 20, replace = TRUE) #get the first 20 sample.
printVecInfo(firstSample) #plug the sample into the function
hist(firstSample) #create the histogram.

#7)
secondSample <- sample(dfStates$population, size = 20, replace = TRUE) #second iteration.
printVecInfo(secondSample)
hist(secondSample)

thirdSample <- sample(dfStates$population, size = 20, replace = TRUE) #third iteration.
printVecInfo(thirdSample)
hist(thirdSample)

#8)
#Because the sample size is so small (only 20), the results on the histogram can be erratic and all over the place due to the large range of population.
#However it can kind of infer that the distribution is right skewed.

#Part D: Replicate the sampling
#9)
bigSample1 <- replicate(2000, sample(dfStates$population, size = 20, replace = TRUE), simplify = TRUE)
printVecInfo(bigSample1)
hist(bigSample1)

#10)
bigSample2 <- replicate(2000, sample(dfStates$population, size = 20, replace = TRUE), simplify = TRUE) #second iteration
printVecInfo(bigSample2)
hist(bigSample2)

bigSample3 <- replicate(2000, sample(dfStates$population, size = 20, replace = TRUE), simplify = TRUE) #third iteration
printVecInfo(bigSample3)
hist(bigSample3)

#11)
#Here the distribution are much more consistent, with a very clear right skew. The reason was because of the Law of Large Numbers.
#The Law of Large numbers states that f you run a statistical process a large number of times, it will converge on a stable result.
