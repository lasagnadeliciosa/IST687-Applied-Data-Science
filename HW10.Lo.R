#IST687.M006
#Jen Yeu Lo
#Homework 10
#Due date: 11/14/2018, Date Submitted: 11/14/2018

#Part A: Load and condition the data
#1)
install.packages("kernlab")
library(kernlab)

setwd("/Users/jenyeulo/Documents/Syracuse_University/IST687/csv_files/")#set my working directory
library(RJSONIO)#load RJSONIO library, this specific json library is required in order for the below codes to work.
hoteljson <- fromJSON("hotelSurveyBarriot.json", simplify = TRUE, nullValue = NA) #read the json file
hotelSurvey <- data.frame(hoteljson)

#Part B: Create a happy customer variable
#2)
hotelSurvey$hotelState <- NULL #remove the hotelState variable/column due to irrelevance.
hotelSurvey$freeText <- NULL #remove the freeText variable/column due to irrelavance.

hotelSurvey$isSatOver8 <- factor(ifelse(hotelSurvey$overallCustSat>=8,"happy","notHappy")) #create a new column where overallCustSas is 8 or higher
hotelSurvey$overallCustSat <- NULL #remove overallCustSat because it will heavily influence the results of the prediction, but only after creating the new column.
str(hotelSurvey) #to make sure that isSatOver8 is converted to factor.
#View(hotelSurvey) #view the df to make sure columns were removed/added properly.

#Part C: Create training and test data sets
#3)
#divide the hotelSurvey data set into 2/3 training and 1/3 test set:
randIndex <- sample(1:dim(hotelSurvey)[1])

cutPoint2_3 <- floor(2*dim(hotelSurvey)[1]/3) #calculates the 2/3 cut point based on the number of rows in hotelSurvey.
#dim(hotelSurvey)[1] gives the number of rows in the spam data set.
cutPoint2_3 #what is the index # of the cutting point.

#build the training set.
trainHotel <- hotelSurvey[randIndex[1:cutPoint2_3],]

#build the test set.
testHotel <- hotelSurvey[randIndex[(cutPoint2_3+1):dim(hotelSurvey)[1]],]

#4)
dim(trainHotel) #number of rows for the train case
dim(testHotel) #number of rows for the test case

#5)
ksvm(isSatOver8~checkInSat+hotelClean+hotelFriendly, #using the three variables to predict happy/unhappy customer in isSatOver8 variable.
     data=trainHotel, #use which dataframe
     kernel="rbfdot", #radial basis function. In simple terms: maximize separation distance between spam and nonspam cases.
     kpar="automatic", #refers to a variety of parameters that can be used to control operation of the rbf kernel.
     C=5, #cost of contraints
     cross=3, #3-fold cross validation
     prob.model=TRUE)

#6)
#The C argument refers to cost of constraints. With a small C value we can get a more generalized model with large margin of separation but that will include some mistakes.
#A large C value will give you fewer mistakes but it will make the margin of separation smaller.
#Cross refers to the cross validation model the algorithm uses. Cross validation is important for avoiding the problems of overfitting.
#If we push the algorithm too hard, it will become too specialized to the training data and we won't be able to use it anywhere else.
#By using k-fold (3-fold) cross validation, we can create an appropriate model that is more likely to generalize to other data.


#7)
svmOutputHotel <- ksvm(isSatOver8~checkInSat+hotelClean+hotelFriendly, #using the three variables to predict happy/unhappy customer in isSatOver8 variable.
                  data=trainHotel, #use which dataframe
                  kernel="rbfdot", #radial basis function. In simple terms: maximize separation distance between spam and nonspam cases.
                  kpar="automatic", #refers to a variety of parameters that can be used to control operation of the rbf kernel.
                  C=5, #cost of contraints
                  cross=3, #3-fold cross validation
                  prob.model=TRUE)
svmOutputHotel

#Part D: Predict Values in the Test Data and Create a Confusion Matrix
#8)
svmPredHotel <- predict(svmOutputHotel, testHotel, type="votes") #using predict() function to validate the model against test data.

#9)
#str(svmPredHotel)
#head(svmPredHotel)

#10)
compTableHotel <- data.frame(testHotel$isSatOver8, svmPredHotel[1,])
#compTableHotel
table(compTableHotel) #create the confusion matrix

#11)
x <- (330+336)/(330+336+1217+1451) #calculate the error rate.
x #whis is arund 20%.

#Part F: Find a good prediction
#12)
svmOutputHotel2 <- ksvm(isSatOver8~., #using the three variables to predict happy/unhappy customer in isSatOver8 variable.
                       data=trainHotel, #use which dataframe
                       kernel="rbfdot", #radial basis function. In simple terms: maximize separation distance between spam and nonspam cases.
                       kpar="automatic", #refers to a variety of parameters that can be used to control operation of the rbf kernel.
                       C=5, #cost of contraints
                       cross=3, #3-fold cross validation
                       prob.model=TRUE)
svmOutputHotel2

svmPredHotel2 <- predict(svmOutputHotel2, testHotel, type="votes")

compTableHotel2 <- data.frame(testHotel$isSatOver8, svmPredHotel2[1,])
#compTableHotel2
table(compTableHotel2)

y <- (167+169)/(167+169+1618+1380)
y #10% error rate.

z <- table(compTableHotel2)
(z[1]+z[4])/(z[1]+z[2]+z[3]+z[4])

#13)
#Because the most ideal situation is to have the training model to not know anything about the testing model.
#If it does it would heavily influence the prediction, making it too specialized. Thus we don't know if the model is general enough to predict new sets of data.

