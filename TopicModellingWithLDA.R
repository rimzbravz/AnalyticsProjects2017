#https://eight2late.wordpress.com/2015/09/29/a-gentle-introduction-to-topic-modeling-using-r/

.libPaths("//au.qbe.pri/Home/NSW_Home/200583/Profile/Documents/R/R-3.3.1/library")

#for topic modelling using LDA

library(tm)

setwd("G:\\GSSC\\Analytics\\AO GSSC\\Rimar\\internal\\LDA\\TopicModelling\\data")

#list .txt files in the directory

filenames <- list.files(getwd(), pattern = "*.txt")

#read files into a chracter vector
files <- lapply(filenames, readLines)
#at this point each document(file) is placed in a vector
#for example [1:80] means 1 by 80 lines, i.e 80 lines/elements but in 1 vector

docs <- Corpus(VectorSource(files))
#A vector source interprets each element of the vector x as a document.
#now docs is a collection of documents(i.e Corpus) containing 1 file per document

#inspect a particular document in corpus
writeLines(as.character(docs[[1]]))



#Preprocessing
#always transform to lower case
docs <- tm_map(docs,content_transformer(tolower))

?tm_map

#Text Cleaning
#potentially problematic symbols
#don't run this yet
#toSpace = content_transformer(function(x,pattern){return(gsub(pattern, " ", x))})
#gsub substitutes a pattern with " "
docs <- tm_map(docs, content_transformer(gsub), pattern = "-", replacement = "")
docs <- tm_map(docs, content_transformer(gsub), pattern = "`", replacement = "")
docs <- tm_map(docs, content_transformer(gsub), pattern = "'", replacement = "")
docs <- tm_map(docs, content_transformer(gsub), pattern = "*", replacement = "")
docs <- tm_map(docs, content_transformer(gsub), pattern = "''", replacement = "")
docs <- tm_map(docs, content_transformer(gsub), pattern = "``", replacement = "")

#get bak to this if this is right!

#Remove Punctuation
docs <- tm_map(docs, removePunctuation)

#strip digits
docs <- tm_map(docs, removeNumbers)

#remove stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))

#remove whitespace
docs <- tm_map(docs, stripWhitespace)


writeLines(as.character(docs[[30]]))

#Document Stemming: transforming into root word
docs <- tm_map(docs, stemDocument)

#fix up 1) differences between us and aussie english 2) general errors

docs <- tm_map(docs, content_transformer(gsub),pattern = "organiz", replacement = "organ")
docs <- tm_map(docs, content_transformer(gsub),pattern = "organis", replacement = "organ")
docs <- tm_map(docs, content_transformer(gsub),pattern = "andgovern", replacement = "govern")
docs <- tm_map(docs, content_transformer(gsub),pattern = "inenterpris", replacement = "enterpris")
docs <- tm_map(docs, content_transformer(gsub),pattern = "team-", replacement = "team")

#define and eliminate all custom stopwords

myStopwords <- c("can", "say","one","way","use",
                  "also","howev","tell","will",
                  "much","need","take","tend","even",
                  "like","particular","rather","said",
                  "get","well","make","ask","come","end",
                  "first","two","help","often","may",
                  "might","see","someth","thing","point",
                  "post","look","right","now","think","'ve",
                  "'re","anoth","put","set","new","good",
                  "want","sure","kind","larg","yes,","day","etc",
                  "quit","sinc","attempt","lack","seen","awar",
                  "littl","ever","moreov","though","found","abl",
                  "enough","far","earli","away","achiev","draw",
                  "last","never","brief","bit","entir","brief",
                  "great","lot")

#remove stopwords
docs <- tm_map(docs, removeWords, myStopwords)

#inspect
writeLines(as.character(docs[[30]]))


str(docs)

#Create Term-Document Matrix
dtm <- DocumentTermMatrix(docs)
#documents on each row
#Terms on columns

rownames(dtm) <- filenames


#collapse matrix, sum over columns
#frequency of words
freq <- colSums(as.matrix(dtm))

length(freq) #number of terms
summary(freq) #just a check of max term freq

#create freq in descending order
ord <- order(freq, decreasing = TRUE) #ord is just an index
freq[ord]

write.csv(freq[ord],"word_freq.csv")

#--------------- This is the LDA Part -----------#
#---the document term Matrix is the one
#---we will feed into the LDA model

library(topicmodels)

#Set parameters for Gibbs Sampling

burnin <- 4000
iter <- 2000
thin <- 500
seed <- list(2003, 5, 63, 100001, 765) #define 5 seed integers
nstart <- 5
best <- TRUE


#Number of topics
k <- 5

#Run LDA using Gibbs Sampling
ldaOut <- LDA(dtm, k, method = "Gibbs", control = list(nstart = nstart, seed = seed, best = best,
burnin = burnin, iter = iter, thin = thin))


#Write the results
#docs-topics: assigned topics to each documents
ldaOut.topics <- as.matrix(topics(ldaOut))
write.csv(ldaOut.topics, file = paste("LDAGibbs", k, "DOcsToTopics.csv"))

#top terms in each topics
ldaOut.terms <- as.matrix(terms(ldaOut, 6))
write.csv(ldaOut.terms, file = paste("LDAGibbs", k, "TopicToTerms.csv"))


#Probabilities associated with each topic assignment
#Document topic probability
topicProbabilities <- as.data.frame(ldaOut@gamma)
write.csv(topicProbabilities, file = paste("LDAGibbs", k, "TopicProbabilities.csv"))


#Find relative importance of top 2 topics
Topic1ToTopic2 <- lapply()




