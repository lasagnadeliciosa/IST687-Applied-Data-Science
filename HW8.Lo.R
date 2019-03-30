#IST687.M006
#Jen Yeu Lo
#Homework 8
#Due date: 10/24/2018, Date Submitted: 10/23/2018

#Part A: Load and condition the data
#1)
setwd("/Users/jenyeulo/Documents/Syracuse_University/IST687/csv_files/")#set my working directory
library(RJSONIO)#load RJSONIO library, this specific json library is required in order for the below codes to work.

hoteljson <- fromJSON("hotelSurveySherison.json", simplify = TRUE, nullValue = NA) #read the json file
hoteldf <- as.data.frame(hoteljson) #convert it into dataframe

#2) use str command to inspect the df
str(hoteldf)

#Part B: Explore the data  
#1)
library(ggplot2)
plot1 <- ggplot(data=hoteldf, aes(x=hotelSize, y=overallCustSat)) + geom_point() + geom_jitter(alpha=0.5) + ggtitle("overallCustSat vs. hotelSize")
plot1

plot2 <- ggplot(data=hoteldf, aes(x=checkInSat, y=overallCustSat)) + geom_point() + geom_jitter(alpha=0.5) + ggtitle("overallCustSat vs. checkInSat")
plot2

plot3 <- ggplot(data=hoteldf, aes(x=hotelState, y=overallCustSat)) + geom_point() + geom_jitter(alpha=0.5) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("overallCustSat vs. hotelState")
plot3

plot4 <- ggplot(data=hoteldf, aes(x=hotelClean, y=overallCustSat)) + geom_point() + geom_jitter(alpha=0.5) + ggtitle("overallCustSat vs. hotelClean")
plot4

plot5 <- ggplot(data=hoteldf, aes(x=hotelFriendly, y=overallCustSat)) + geom_point() + geom_jitter(alpha=0.5) + ggtitle("overallCustSat vs. hotelFriendly")
plot5

plot6 <- ggplot(data=hoteldf, aes(x=gender, y=overallCustSat)) + geom_point() + geom_jitter(alpha=0.5) + ggtitle("overallCustSat vs. gender")
plot6

plot7 <- ggplot(data=hoteldf, aes(x=guestAge, y=overallCustSat)) + geom_point() + geom_jitter(alpha=0.5) + ggtitle("overallCustSat vs. guestAge")
plot7

plot8 <- ggplot(data=hoteldf, aes(x=lengthOfStay, y=overallCustSat)) + geom_point() + geom_jitter(alpha=0.5) + ggtitle("overallCustSat vs. lengthOfStay")
plot8

plot9 <- ggplot(data=hoteldf, aes(x=whenBookedTrip, y=overallCustSat)) + geom_point() + geom_jitter(alpha=0.5) + ggtitle("overallCustSat vs. whenBookedTrip")
plot9

#2)
#most of the graphs makes no visual sense other than checkInSat, hotelClean, and hotelFriendly. The higher these attributes are, the higher the customer satisfaction.

#Part B: Generate a linear model  
#3)
model1 <- lm(formula = overallCustSat~hotelSize+hotelState+checkInSat+hotelClean+hotelFriendly+gender+guestAge+lengthOfStay+whenBookedTrip, data=hoteldf)
summary(model1) #using hotelState is not meaningful in this model.

model2 <- lm(formula = overallCustSat~hotelSize+checkInSat+hotelClean+hotelFriendly+gender+guestAge+lengthOfStay+whenBookedTrip, data=hoteldf)
summary(model2) #it is clear that hotelState is not only statistically not significant, but also creates too much clutter in the ourput by actually printing all the state names. 
#therefore removing it will visually and statistically improve the model overall.

#4)
#R-squared for this linear model is 0.6702
#dependent variables that are significant: checkInSat, hotelClean, hotelFriendly, guestAge, lengthOfStay, whenBookedTrip.
#checkInSat: -2.385e-01
#hotelClean: 3.996e-02
#hotelFriendly: 1.122e+00
#guestAge: -1.203e-01
#lengthOfStay: -3.280e-01
#whenBookedTrip: 6.357e-03

#5)
#It is abundantly clear that the variable hotelState have little significance in this linear model due to their very high p-value.
#hotelSize, and gender of the customers also have little significance in the linear model.
#the independent variables that best explains the dependent variable overallCustSat are checkInStat, hotelFriendly, guestAge, lengthOfStay, and whenBookedTrip.

#Part C: Generate a different linear model  
#6)
model3 <- lm(formula = overallCustSat~hotelFriendly, data = hoteldf)
summary(model3)

#7)
#The first model is better because it has a higher R-squared of 0.6702 compared to the second model's 0.3785.
#Having multiple variables in a linear model is much better for making predictions, compared to only 1 model, even if it has a very low p-value.
#I chose hotelFriendly because out of all the variables that are significant it has the highest R-squared, meaning that this variable explains the model better than the other variables.




