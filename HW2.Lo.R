#IST687.M006
#Jen Yeu Lo
#Homework 2
#Due date: 09/12/2018, Date Submitted: 09/09/2018

#Step A: Initialize an ‘arrests’ dataframe
#1)
arrests <- data.frame(USArrests)
arrests

#Step B: Explore the assault rate
#2)
#A lower assault rate would be the best for humanity in general.

#3)
best_assault_rate <- rownames(arrests[order(arrests$Assault),])
head(best_assault_rate,1)
#North Dakota

#Step C: Explore the murder rate
#4)
max_murder <- rownames(arrests[order(-arrests$Murder),])
head(max_murder,1)
#Georgia

#5)
sorted_arrests <- arrests[order(-arrests$Murder),]
sorted_arrests

#6)
head(sorted_arrests, 10)

#7)
sorted_arrests[20,3]
#50

#Step D: Which state is the least safe? Explain your logic
#8)

normalize <- function(x){
  return((x - min(x)) / (max(x) - min(x)))
}

murderNorm <- as.data.frame(lapply(sorted_arrests, normalize))
murderNorm
least_safe_state <- which.max(rowSums(murderNorm))
least_safe_state #It will return a value of 3, which corresponds to Florida in sorted_arrests df.

#9)
#After normalizing the data in every column add them together row by row, I determined that Florida is the least safe state overall because it has the highest normalized value.