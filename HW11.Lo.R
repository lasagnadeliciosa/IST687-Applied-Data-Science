#IST687.M006
#Jen Yeu Lo
#Homework 11
#Due date: 11/28/2018, Date Submitted: 11/27/2018

#Part A: Load and condition the text file that contains the speech  
#1-2)
setwd("/Users/jenyeulo/Documents/Syracuse_University/IST687/csv_files/")#set my working directory
library(RJSONIO)#load RJSONIO library, this specific json library is required in order for the below codes to work.
hoteljson <- fromJSON("hotelSurveyBarriot.json", simplify = TRUE, nullValue = NA) #read the json file
hotelSurvey <- data.frame(hoteljson)
#View(hotelSurvey)
#Part B: Create a list of word counts from the free text
#3)
library(tm)
library(wordcloud)

words.vec <- VectorSource(hotelSurvey$freeText)
words.corpus <- Corpus(words.vec) #coerce the text file vector (hotelSurvey$freeText) into a custom class called a corpus.
words.corpus

#data munging to clean up the text entries
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english")) #remove english stopwords.

#create term-document matrix
tdm <- TermDocumentMatrix(words.corpus)
tdm
inspect(tdm)

m <- as.matrix(tdm) #coerce text data back into a plain data matrix so that we can sort it by frequency.
wordCounts <- rowSums(m) #sum the frequency of the words.
wordCounts <- sort(wordCounts, decreasing=TRUE) #sorted with the most frequent terms appearing first.
head(wordCounts)
str(wordCounts)

#Determining positive and negative word matches
pos <- "positive-words.txt"
neg <- "negative-words.txt"

#seperate each word
p <- scan(pos,character(0), sep = "\n")
n <- scan(neg,character(0), sep = "\n")

#remove the headers
p <- p[-1:-34]
n <- n[-1:-34]

#count the total number of words in our text
totalWords <- sum(wordCounts)
totalWords #901 total words with duplicates
#have a vector that just has all the words
words <- names(wordCounts)
words #506 unique words

#match the positve words, output are index numbers
matched <- match(words, p, nomatch = 0)
#count all the words that did match
mCounts <- wordCounts[which(matched !=0)]
length(mCounts) #there are 64 unique positive words in the speech.
#View(mCounts)
mWords <- names(mCounts)
nPos <- sum(mCounts)
nPos #there are 156 positive words total including duplicates.

#do the same for negative words.
matched <- match(words, n, nomatch = 0)
nCounts <- wordCounts[which(matched !=0)]
nNeg <- sum(nCounts)
nNeg #40 total negative words including duplicates
nWords <- names(nCounts)
length(nCounts) #28 unique negative words

#4)
#calculate percentage of positive and negative words for this survey:
ratioPos <- nPos/totalWords
ratioPos #~17% positive words
ratioNeg <- nNeg/totalWords
ratioNeg #~4% negative words

#5)
#Overall there is a higher number of positive words compared to negative words amongst all the unique words that appeared in the word pool for both happy and unhappy customers.

#Part D: Visualize the results
#6)
cloudFrame <- data.frame(word=names(wordCounts), freq=wordCounts)
wordcloud(cloudFrame$word, cloudFrame$freq) #creates a wordcloud with the most frequent term the largest.

#7)
barplot(mCounts, main = "Positve words", ylab = "Frequency", las=2) #barplot for positive words
barplot(nCounts, main = "Negative words", ylab = "Frequency", las=2) #barplot for negative words

#8)
#The word cloud is composed by a jumble of words for the word bank, that includes the positive, negative, and neutral words.
#The most frequent words for the positive barplot is "friendly", "clean", "nice".
#The most frequent words for the negative barplot is "bad", "expensive", "worse"

#9)
#For me the most informative one is definitely the barplot, as it clearly tells you what are the most common words that appeared, and at what frequency.
#For this particular wordcloud: it's not clear becuse it included a mix of positive, negative and neutral words, with newtral words being the most frequent, 
#making it difficult to determine the overall feelings for the customer satisfaction experience.

#Part E: Evaluate Happy and Unhappy customer responses
#10)
happyCustomer <- subset(hotelSurvey, hotelSurvey$overallCustSat >=8) #create subset for happyCustomer, 4751 entries.
#View(happyCustomer)
str(happyCustomer)
unhappyCustomer <- subset(hotelSurvey, hotelSurvey$overallCustSat <8) #create subset for unhappyCustomer, 5249 entries.
#View(unhappyCustomer)
str(unhappyCustomer)

#11)

#For happyCustomer subset
#Part B: Create a list of word counts from the free text
happy.words.vec <- VectorSource(happyCustomer$freeText)
happy.words.corpus <- Corpus(happy.words.vec) #coerce the text file vector (happyCustomer$freeText) into a custom class called a corpus.
happy.words.corpus

#data munging to clean up the text entries
happy.words.corpus <- tm_map(happy.words.corpus, content_transformer(tolower))
happy.words.corpus <- tm_map(happy.words.corpus, removePunctuation)
happy.words.corpus <- tm_map(happy.words.corpus, removeNumbers)
happy.words.corpus <- tm_map(happy.words.corpus, removeWords, stopwords("english")) #remove english stopwords.

#create term-document matrix
happy.tdm <- TermDocumentMatrix(happy.words.corpus)
happy.tdm
inspect(happy.tdm)

m <- as.matrix(happy.tdm) #coerce text data back into a plain data matrix so that we can sort it by frequency.
happy.wordCounts <- rowSums(m) #sum the frequency of the words.
happy.wordCounts <- sort(happy.wordCounts, decreasing=TRUE) #sorted with the most frequent terms appearing first.
head(happy.wordCounts)
#count the total number of words in our text
happy.totalWords <- sum(happy.wordCounts)
happy.totalWords #472 words in happyCustomer subset with duplicates.
#have a vector that just has all the unique words
happy.words <- names(happy.wordCounts)
str(happy.words) #286 total unique words

#match the positve words, output are index numbers
happy.matched <- match(happy.words, p, nomatch = 0)
#count all the words that did match
happyPosCounts <- happy.wordCounts[which(happy.matched !=0)]
length(happyPosCounts) #there are 54 unique positive words in the survey.
#View(mCounts)
mWords <- names(happyPosCounts)
happyPos <- sum(happyPosCounts)
happyPos #there are 130 positive words total including duplicates.

#do the same for negative words.
happy.matched <- match(happy.words, n, nomatch = 0)
happyNegCounts <- happy.wordCounts[which(happy.matched !=0)]
length(happyNegCounts) #5 unique negative words
nNeg <- names(happyNegCounts)
happyNeg <- sum(happyNegCounts)
happyNeg #5 total negative words including duplicates

#calculate percentage of positive and negative words for this survey:
ratioPos <- happyPos/happy.totalWords
ratioPos #~28% positive words
ratioNeg <- happyNeg/happy.totalWords
ratioNeg #~1% negative words

#There is a much higher proportion of positive words compared to negative words amongst all the unique words that appeared
#in the word pool for both happy and unhappy customers in the happyCustomer subset.

#Part D: Visualize the results
happy.cloudFrame <- data.frame(word=names(happy.wordCounts), freq=happy.wordCounts)
wordcloud(happy.cloudFrame$word, happy.cloudFrame$freq) #creates a wordcloud with the most frequent term the largest.

barplot(mCounts, main = "Positve words in happyCustomer subset", ylab = "Frequency", las=2) #barplot for positive words
barplot(nCounts, main = "Negative words in happyCustomer subset", ylab = "Frequency", las=2) #barplot for negative words

#The most frequent words for the positive barplot is "friendly", "great", "clean".
#The most frequent words for the negative barplot is "issues", "blind", "dark"

#The barplot still clearly tells you what are the most common words that appeared, and at what frequency.
#However, the wordcloud for the happyCustomer subset also does a great job giving you a general feeling of the customer at first glance.

#For unhappyCustomer subset
#Part B: Create a list of word counts from the free text
unhappy.words.vec <- VectorSource(unhappyCustomer$freeText)
unhappy.words.corpus <- Corpus(unhappy.words.vec) #coerce the text file vector (unhappyCustomer$freeText) into a custom class called a corpus.
unhappy.words.corpus

#data munging to clean up the text entries
unhappy.words.corpus <- tm_map(unhappy.words.corpus, content_transformer(tolower))
unhappy.words.corpus <- tm_map(unhappy.words.corpus, removePunctuation)
unhappy.words.corpus <- tm_map(unhappy.words.corpus, removeNumbers)
unhappy.words.corpus <- tm_map(unhappy.words.corpus, removeWords, stopwords("english")) #remove english stopwords.

#create term-document matrix
unhappy.tdm <- TermDocumentMatrix(unhappy.words.corpus)
unhappy.tdm
inspect(unhappy.tdm)

f <- as.matrix(unhappy.tdm) #coerce text data back into a plain data matrix so that we can sort it by frequency.
unhappy.wordCounts <- rowSums(f) #sum the frequency of the words.
unhappy.wordCounts <- sort(unhappy.wordCounts, decreasing=TRUE) #sorted with the most frequent terms appearing first.
head(unhappy.wordCounts)
#count the total number of words in our text
unhappy.totalWords <- sum(unhappy.wordCounts)
unhappy.totalWords #429 words in unhappyCustomer subset with duplicates.
#have a vector that just has all the words
unhappy.words <- names(unhappy.wordCounts)
str(unhappy.words) #284 unique words in this subset

#match the positve words, output are index numbers
unhappy.matched <- match(unhappy.words, p, nomatch = 0)
#count all the words that did match
unhappyPosCounts <- unhappy.wordCounts[which(unhappy.matched !=0)]
length(unhappyPosCounts) #there are 19 unique positive words in the survey.
#View(mCounts)
mWords <- names(unhappyPosCounts)
unhappyPos <- sum(unhappyPosCounts)
unhappyPos #there are 26 positive words total including duplicates.

#do the same for negative words.
unhappy.matched <- match(unhappy.words, n, nomatch = 0)
unhappyNegCounts <- unhappy.wordCounts[which(unhappy.matched !=0)]
length(unhappyNegCounts) #23 unique negative words
nNeg <- names(unhappyNegCounts)
unhappyNeg <- sum(unhappyNegCounts)
unhappyNeg #35 total negative words including duplicates

#calculate percentage of positive and negative words for this survey:
ratioPos <- unhappyPos/unhappy.totalWords
ratioPos #~6% positive words
ratioNeg <- unhappyNeg/unhappy.totalWords
ratioNeg #~8% negative words

#There is a much higher proportion of positive words compared to negative words amongst all the unique words that appeared
#in the word pool for both happy and unhappy customers in the happyCustomer subset.

#Part D: Visualize the results
unhappy.cloudFrame <- data.frame(word=names(unhappy.wordCounts), freq=unhappy.wordCounts)
wordcloud(unhappy.cloudFrame$word, unhappy.cloudFrame$freq) #creates a wordcloud with the most frequent term the largest.

barplot(mCounts, main = "Positve words in unhappyCustomer subset", ylab = "Frequency", las=2) #barplot for positive words
barplot(nCounts, main = "Negative words in unhappyCustomer subset", ylab = "Frequency", las=2) #barplot for negative words

#The most frequent words for the positive barplot is "work", "good", "recommend".
#The most frequent words for the negative barplot is "bad", "expensive", "worse".

#The barplot still clearly tells you what are the most common words that appeared, and at what frequency.
#However, the wordcloud for the unhappyCustomer subset did an sub-par job this time because it displayed a great deal of neutral words,
#making it difficult to discern the overall feelings of the customers.

#12)
#The ratio for the happyCustomer subset: ~28% positive words, ~1% negative words.
#The ratio for the unhappyCustomer subset: ~6% positive words, ~8% negative words.