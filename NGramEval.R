# @daviddalisay
# NGramEval finds all ngrams from 1 to n.

x = "David Dalisay is the best. David Dalisay is probably the best of them all."
library(ngram)
ng <- ngram(x,2)
print(ng,full=TRUE)