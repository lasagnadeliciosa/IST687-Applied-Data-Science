#IST687.M006
#Jen Yeu Lo
#Homework 1
#Due date: 9/5/18, Date Submitted:8/31/18

#Step A: Create a Vector
#1)
grades <- c(4.0, 3.3, 3.7)
#2)
courseName <- c("Bio,", "Math", "History")
#3)
BetterThanB <- 3

#Step B: Step B: Calculating statistics using R
#4)
mean(grades)
#5)
total.length <- length(grades)
#6)
total.length
#7)
total <- sum(grades)
#8)
total/total.length

#Step C: Using the max/min functions in R
#9)
maxG <- max(grades)
#10)
minG <- min(grades)

#Step D: Vector Math
#11)
betterGrades <- grades + 0.3
#12)
mean(betterGrades)

#Step E: Using Conditional if statements
#13)
if(maxG>3.5){print("yes")} else {print("no")}
#14)
if(minG>BetterThanB){print("yes")} else {print("no")}

#Step F: Accessing an element in a vector
#15)
courseName[2]
