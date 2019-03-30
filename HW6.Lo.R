#IST687.M006
#Jen Yeu Lo
#Homework 6
#Due date: 10/10/2018, Date Submitted: 10/09/2018

#Step A: Load and Merge datasets
#1)Read in the census dataset (using the function created in HW 3)
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
UScensus <- stateFunction()

#2)Copy the USArrests dataset into a local variable (similar to HW 2)
arrests <- data.frame(USArrests)
#rownames <- row.names(arrests) #grabs all the row names from arrests
arrests$stateName <- rownames(arrests) #create new column and add the row names in
#arrests

#3)
mergedf <- merge(UScensus, arrests, by.x = "stateName") #merge the two dataframes by "stateName"
mergedf

#Step B: Explore the Data – Understanding distributions
#4)
histpop <- ggplot(mergedf, aes(x=population))
histpop <- histpop + geom_histogram(binwidth=500000,color="black",fill="orange")
histpop <- histpop + ggtitle("States Population")
histpop #histogram for population

histmurder <- ggplot(mergedf, aes(x=Murder))
histmurder <- histmurder + geom_histogram(binwidth=0.5,color="black",fill="orange")
histmurder <- histmurder + ggtitle("States Murder")
histmurder #histogram for murder

#5)
boxpop <- ggplot(mergedf, aes(x=factor(0),population))
boxpop <- boxpop + geom_boxplot()
boxpop <- boxpop + ggtitle("US Population")
boxpop #boxplot for population of the US

boxmurder <- ggplot(mergedf, aes(x=factor(0),Murder))
boxmurder <- boxmurder + geom_boxplot()
boxmurder <- boxmurder + ggtitle("US Murder Rate")
boxmurder #boxplot for murder rate

#6)
#For me the box plot was much more clearer because I can clearly see where the distribution generally falls,
#the location of the median, the general location for Q2 and Q3.

#Step C: Which State had the Most Murders – bar charts
#7)
mergedf$murderperstate <- mergedf$population*mergedf$Murder/100000
murderperstate <- cbind(mergedf$stateName, mergedf$murderperstate)
murderperstate #display the number of murder per state.

#8)
barmurder <- ggplot(mergedf, aes(x=stateName, y = murderperstate, group = 1))
barmurder <- barmurder + geom_col()
barmurder <- barmurder + ggtitle("Murders per state")
barmurder

#9)
barmurder1 <- ggplot(mergedf, aes(x=stateName, y = murderperstate, group = 1))
barmurder1 <- barmurder1 + geom_col()
barmurder1 <- barmurder1 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
barmurder1 <- barmurder1 + ggtitle("Murders per state")
barmurder1

#10)
barmurder2 <- ggplot(mergedf, aes(x=reorder(stateName, Murder), y = murderperstate, group = 1))
barmurder2 <- barmurder2 + geom_col()
barmurder2 <- barmurder2 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
barmurder2 <- barmurder2 + ggtitle("Murders per state sorted by murder rate")
barmurder2

#11)
barmurder3 <- ggplot(mergedf, aes(x=reorder(stateName, Murder), y = murderperstate, group = 1, fill = percentOver18))
barmurder3 <- barmurder3 + geom_col()
barmurder3 <- barmurder3 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
barmurder3 <- barmurder3 + ggtitle("Murders per state sorted by murder rate showing percentOver18")
barmurder3

#12)
scattermurder <- ggplot(mergedf, aes(x=population, y=percentOver18))
scattermurder <- scattermurder + geom_point(aes(size=Murder, color = Murder))
#scattermurder <- scattermurder + geom_text(aes(label = stateName), size = 4, hjust = 1, vjust = -1)
scattermurder <- scattermurder + ggtitle("Murders per state")
scattermurder
