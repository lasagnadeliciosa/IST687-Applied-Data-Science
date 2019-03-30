#IST687.M006
#Jen Yeu Lo
#Homework 7
#Due date: 10/17/2018, Date Submitted: 10/16/2018

#Step A: Load and Merge datasets
#1)
options(stringsAsFactors = FALSE)

stateFunction <- function(){
  dfStates <- read.csv(file = "~/Documents/Syracuse University/IST687/csv_files/states.csv", header = T, stringsAsFactors = FALSE)
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
arrests <- data.frame(USArrests)
arrests$stateName <- rownames(arrests) #create new column and add the row names in
mergedf <- merge(UScensus, arrests, by.x = "stateName") #merge the two dataframes by "stateName"
str(mergedf)

#2)
center <- state.center
area <- state.area
stateName <- state.name
tempdf <- data.frame(center, area, stateName, stringsAsFactors = FALSE) #create the columns and put it into a mini df.

mergedf$stateName <- tolower(mergedf$stateName) #lowercase all the statenames

tempdf$stateName <- tolower(tempdf$stateName) #lowercase all the statenames

mergedf <- merge(mergedf,tempdf, by="stateName") #merge the two dfs.
str(mergedf)

#Step B: Generate a color coded map
#3) Create a color coded map, based on the area of the state
us <- map_data("state")
areamap <- ggplot(mergedf, aes(map_id=stateName))
areamap <- areamap + geom_map(map=us, aes(fill=area))#fill by area
areamap <- areamap + expand_limits(x=us$long, y=us$lat)#expand the limits based on long and lat of US
areamap <- areamap + coord_map() + ggtitle("US map by state area")#coord_map keeps map from being distorted or stretched.
areamap

#Step C: Create a color shaded map of the U.S. based on the Murder rate for each state 
#4)
murdermap <- ggplot(mergedf, aes(map_id=stateName))
murdermap <- murdermap + geom_map(map=us, aes(fill=Murder))#fill by murder
murdermap <- murdermap + expand_limits(x=us$long, y=us$lat)#expand the limits based on long and lat of US
murdermap <- murdermap + coord_map() + ggtitle("US map murder rate by state")#coord_map keeps map from being distorted or stretched.
murdermap

#5)
popmap <- ggplot(mergedf, aes(map_id=stateName))
popmap <- popmap + geom_map(map=us, aes(fill=population)) #make the fill by population, just to make is more obvious.
popmap <- popmap + geom_point(aes(size=population, x=x, y=y), color="red")
popmap <- popmap + expand_limits(x=us$long, y=us$lat)#expand the limits based on long and lat of US
popmap <- popmap + coord_map() + ggtitle("US population by state")#coord_map keeps map from being distorted or stretched.
popmap

#Step D: Zoom the map
#6 IMG0132
murdermap2 <- ggplot(mergedf, aes(map_id=stateName))
murdermap2 <- murdermap2 + geom_map(map=us, aes(fill=Murder))#fill by murder
murdermap2 <- murdermap2 + expand_limits(x=us$long, y=us$lat)#expand the limits based on long and lat of US
murdermap2 <- murdermap2 + xlim(-84.00597,-64.00597) + ylim(30.71427,50.71427) #zoom in on the map based on the x and y coordinate of NY+-10.
murdermap2 <- murdermap2 + coord_map() + ggtitle("US map murder rate by state")#coord_map keeps map from being distorted or stretched.
murdermap2

latlon <- geocode(source="dsk", "NYC, NY")
latlon

popmap2 <- ggplot(mergedf, aes(map_id=stateName))
popmap2 <- popmap2 + geom_map(map=us, aes(fill=population)) #make the fill by population, just to make is more obvious.
popmap2 <- popmap2 + geom_point(aes(size=population, x=x, y=y), color="red")
popmap2 <- popmap2 + expand_limits(x=us$long, y=us$lat)#expand the limits based on long and lat of US
popmap2 <- popmap2 + xlim(latlon$lon-10,latlon$lon+10) + ylim(latlon$lat-10,latlon$lat+10) #zoom in on the map based on the x and y coordinate of NY+-10.
popmap2 <- popmap2 + coord_map() + ggtitle("US population by state")#coord_map keeps map from being distorted or stretched.
popmap2

