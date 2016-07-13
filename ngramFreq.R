##GET NGRAM FREQUENCIES

##SET PARAMETERS
fileNm <- "fourgramFreq40k.rds" #filename
m <- 40000 #sample size
n <- 4 #ngram size
s <- 10000 #summarize after s iterations

library(dplyr)
library(ngram)
library(qdap)

setwd("C:/Users/Kenneth/Documents/DS10 - Data Science Capstone/Project/")

files <- c("../final/en_US/en_US.blogs.txt",
           "../final/en_US/en_US.news.txt",
           "../final/en_US/en_US.twitter.txt")

freqAgg <- NULL
print(Sys.time())

for (j in 1:length(files)) {
    txtFile <- readLines(files[j])
    set.seed(n+j)
    rowSample <- sample(1:length(txtFile), m)
    
    phraseFreq <- NULL
    gc()
    startTime <- proc.time()
    
    for (i in 1:length(rowSample)) {
        sentences <- sent_detect_nlp(txtFile[rowSample[i]])
        
        for (k in 1:length(sentences)) {
            str <- preprocess(sentences[k], case="lower", remove.punct=TRUE, remove.numbers=TRUE, fix.spacing=TRUE)
            
            if(wordcount(str) >= n) {
                df <- get.phrasetable(ngram(str, n))
                phraseFreq <- rbind(phraseFreq, df[,1:2])
            }
        }
        
        if(i %% s == 0) {
            phraseFreq <- phraseFreq %>% group_by(ngrams) %>% summarize(freq=sum(freq))
        }
    }
    
    endTime <- proc.time()
    print(paste(files[j], m))
    print(endTime-startTime)
    
    freqAgg <- rbind(freqAgg, phraseFreq)
}

print(Sys.time())
freqAgg <- freqAgg %>% group_by(ngrams) %>% summarize(freq=sum(freq))
freqAgg <- as.data.frame(freqAgg)
saveRDS(freqAgg, file=fileNm)
print(Sys.time())
