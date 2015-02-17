# daviddalisay
# NGramEval finds all ngrams from 1 to n.
# @TODO: Remove punctuation with regex
# @TODO: Make sure each sentence has >= n # of words for specified n 

library(qdap) # load qdap with ngram package
library(gdata) # load gdata package

mydata = read.csv(file.choose(),header=FALSE,sep=",")


pushList <- function(el, ls) {
	ls[length(ls)+1] <- el
	return(ls)
}

# Remove all not non-words and non-chars using regex
cleanStr <- function(sentence) {
	words <- unlist(strsplit(sentence," "))
	wordcount <- length(words)
	cleanWords <- list()
	for (word in words){
		word <- gsub("[^a-zA-Z0-9]"," ",word)
		if (is.na(word)){
			next
		}
		cleanWords <- pushList(word,cleanWords)
	}
	clnStr <- paste(cleanWords,collapse=" ") 
	return(c(clnStr,wordcount))	
}

maxN = as.numeric(readline("input n:")) # n number of ngram strings. 
allNGrams <- list()
for (row in mydata){	
	rowStr <- as.character(row)
	cleanRow <- cleanStr(rowStr)
	grams <- unlist(ngrams(text.var=cleanRow,grouping.var=NULL,n=maxN)[1])	
	for (gram in grams){
		allNGrams <- pushList(gram,ngramArr)
	}	
}

nGramFrequencies <- list()
for (ngram in ngramArr){
	if (!grepl("NA",ngram) && !(ngram=="") && !(ngram==" ")){
		if (is.null(nGramFrequencies[[ngram]])){
			nGramFrequencies[[ngram]] <- 1
		}
		else{
			nGramFrequencies[[ngram]] <- nGramFrequencies[[ngram]] + 1
		}
	}
}

# output frequencies to frequencies.csv
nGramNameVector <- names(nGramFrequencies)
nGramFreqVector <- vector()
for (freq in nGramFrequencies){
	nGramFreqVector <- c(nGramFreqVector,freq)
}
nGramDataFrame <- data.frame(nGramNameVector,nGramFreqVector)
print(nGramDataFrame)
write.table(nGramDataFrame, file = "frequencies.csv", sep = ",", col.names = NA)


