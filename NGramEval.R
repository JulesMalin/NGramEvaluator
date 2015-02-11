# daviddalisay
# NGramEval finds all ngrams from 1 to n.
# @TODO: Remove punctuation with regex
# @TODO: Make sure each sentence has >= n # of words for specified n 

library(ngram) # load ngram package
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
		word <- gsub("[^a-zA-Z[0-9]]","",word)
		cleanWords <- pushList(word,cleanWords)
	}
	clnStr <- paste(cleanWords,collapse=" ") 
	return(c(clnStr,wordcount))	
}

for (row in mydata){	
	rowStr <- as.character(row)
	cleanRow <- cleanStr(rowStr)
	print(cleanRow)
}

# Calculate number of words per line

# Calculate ngram frequencies
maxN = 2 # n number of ngram strings. 
for (n in 1:maxN) {
	for (row in mydata){
		cat("ngrams for current n = ",n,"\n\n")
		rowInfo <- cleanStr(as.character(row))
		rowStr <- rowInfo[1]
		rowWordCount <- rowInfo[2]
		if (maxN > rowWordCount){
			print("Not enough words for ngram. Finding next ngram.\n")
			next
		}
		ng <- ngram(rowStr,n)
		print(ng,full=TRUE)
	}
}