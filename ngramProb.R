##BUILD PROBABILITY DATA.TABLE FROM NGRAM FREQUENCIES

##SET PARAMETERS
freqFileNm <- "fourgramFreq40k.rds"
probFileNm <- "fourgramProb40k.rds"
n <- 4 #number of words in ngram

library(data.table)
library(dplyr)
library(ngram)

setwd("C:/Users/Kenneth/Documents/DS10 - Data Science Capstone/Project/")
ngramFreq <- readRDS(freqFileNm)

#remove rows with number of words not equal to n
isNwords <- function(x) { wordcount(x) == n }
ngramFreq <- ngramFreq[sapply(ngramFreq$ngrams, isNwords),]

#split ngram into pre-n and nth portion
ngramFreq <- mutate(ngramFreq, a=ngrams, b=ngrams)
partA <- function(x) { paste(strsplit(x, " ")[[1]][1:(n-1)], collapse=" ") }
partB <- function(x) { strsplit(x, " ")[[1]][n] }
ngramFreq$a <- sapply(ngramFreq$a, partA)
ngramFreq$b <- sapply(ngramFreq$b, partB)

#calculate probabilities from frequency data
ngramFreq2 <- ngramFreq %>% group_by(a) %>% summarize(freqA=sum(freq))
ngramFreq <- ngramFreq %>% inner_join(ngramFreq2, by="a")
ngramFreq <- mutate(ngramFreq, p=freq/freqA)

#extract relevant columns into data.table and add key
ngramProb <- as.data.table(ngramFreq[,c("a","b","p")])
setkey(ngramProb, a)

#save data.frame object
saveRDS(ngramProb, file=probFileNm)
