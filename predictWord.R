library(data.table)
library(hunspell)
library(ngram)

setwd("C:/Users/Kenneth/Documents/DS10 - Data Science Capstone/Project/")
biProb <- readRDS("bigramProb40k.rds")
triProb <- readRDS("trigramProb40k.rds")
fourProb <- readRDS("fourgramProb40k.rds")

predictWord <- function(x) {
    if (nchar(trimws(x)) > 0) {
        endSpace <- (substr(x, nchar(x), nchar(x)) == " ")
        str <- preprocess(x, case="lower", remove.punct=TRUE, remove.numbers=TRUE, fix.spacing=TRUE)
        words <- strsplit(str, " ")[[1]]
        l <- length(words)
        result <- as.data.table(NULL)
        
        for (i in 1:2) {
            if ((i==2) & (nrow(result)<3)) { #do spell check
                if (l >= 3) {
                    if (!hunspell_check(words[l-2])) { words[l-2] <- hunspell_suggest(words[l-2])[[1]][1] }
                }
                if (l >= 2) {
                    if (!hunspell_check(words[l-1])) { words[l-1] <- hunspell_suggest(words[l-1])[[1]][1] }
                }
                if (endSpace) {
                    if (!hunspell_check(words[l])) { words[l] <- hunspell_suggest(words[l])[[1]][1] }
                }
            }
            
            if (nrow(result)<3) {
                if (endSpace) { #predict next word
                    if ((l>=3) & (nrow(result)<3)) {
                        result <- rbind(result, 
                                        head(fourProb[a==paste(words[(l-2):l],collapse=" ")][order(-p)],3))
                    }
                    if ((l>=2) & (nrow(result)<3)) {
                        result <- rbind(result, 
                                        head(triProb[a==paste(words[(l-1):l],collapse=" ")][order(-p)],3))
                    }
                    if (nrow(result)<3) {
                        result <- rbind(result, head(biProb[a==words[l]][order(-p)],3))
                    }
                }
                else { #predict last word
                    if ((l>=4) & (nrow(result)<3)) {
                        result <- rbind(result,
                                        head(subset(fourProb[a==paste(words[(l-3):(l-1)],collapse=" ")],
                                                    grepl(paste0("^",words[l]), b))[order(-p)],3))
                    }
                    if ((l>=3) & (nrow(result)<3)) {
                        result <- rbind(result,
                                        head(subset(triProb[a==paste(words[(l-2):(l-1)],collapse=" ")],
                                                    grepl(paste0("^",words[l]), b))[order(-p)],3))
                    }
                    if ((l>=2) & (nrow(result)<3)) {
                        result <- rbind(result,
                                        head(subset(biProb[a==paste(words[l-1])],
                                                    grepl(paste0("^",words[l]), b))[order(-p)],3))
                    }
                    if ((i==2) & (nrow(result)<3)) {
                        result <- rbind(result,
                                        data.frame("a"=c("","",""),
                                                   "b"=hunspell_suggest(words[l])[[1]][1:3],
                                                   "p"=c(0,0,0)))
                    }
                }
            }
        }
    }
    #result
    #head(result$b, 3)
    as.character(result[1:3]$b)
}

# triProb[a=="this is" & grepl("^the", b)] #starts with
# subset(triProb[a=="this is"], grepl("^the", b)) #starts with (faster)
# triProb[a=="this is" & b %like% "the"] #like
