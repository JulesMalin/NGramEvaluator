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

# Store raw ngram output to output.txt
maxN = as.numeric(readline("input n:")) # n number of ngram strings. 
ngramArr <- list()
for (n in 1:maxN) {
	sink("output.txt",append=TRUE,split=FALSE) # direct stdout to output.txt	
	print(paste0("##### ngrams for n=",n," #####"))
	sink() # return to stdout
	for (row in mydata){
		rowInfo <- cleanStr(as.character(row))
		rowStr <- rowInfo[1]
		rowWordCount <- rowInfo[2]
		if (maxN > rowWordCount){
			print("Not enough words for ngram. Finding next ngram.")
			next
		}
		ng <- ngram(rowStr,n)
		grams <- get.ngrams(ng)
		for (gram in grams){
			if (!isTRUE(ngramArr$gram)){
				ngramArr[[gram]] <- 0
			}
			ngramArr[[gram]] <- ngramArr[[gram]] + 1
		}
		sink("output.txt",append=TRUE,split=FALSE)
		print(ng,full=TRUE)
		sink()
	}
}

# Calculate ngram frequencies
sink("frequencies.txt",append=TRUE,split=FALSE)
print("####### NGRAM FREQUENCIES ######")
ngramNames <- names(ngramArr)
ngramCount <- 0
for (gram in ngramArr){
	print(paste0(ngramNames[ngramCount],":",gram))
	ngramCount <- ngramCount + 1
}
sink()
