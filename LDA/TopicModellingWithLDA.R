#for topic modelling using LDA

library(tm)

setwd("H:\\rimar\\Analytics\\TextMining\\LDA")

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
?writeLines
