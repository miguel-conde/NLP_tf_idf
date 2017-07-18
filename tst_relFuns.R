# GET CORDIS DATA
# library(readxl)
# cordis_h2020projects <- read_excel("./DATA/cordis-h2020projects.xlsx")

myURL <- "http://cordis.europa.eu/data/cordis-h2020projects.csv"

library(data.table)
cordis_h2020projects <- fread(myURL)

## Health topics
library(dplyr)
health_projects <- filter(cordis_h2020projects, 
                          topics == "SMEInst-06-2016-2017")
dim(health_projects)

health_projects_SME1 <- filter(health_projects, fundingScheme == "SME-1" )
health_projects_SME2 <- filter(health_projects, fundingScheme == "SME-2" )

# BUILD CORPUS
library(tm)
# vProy <- c(health_projects$title, health_projects$objective)
vProy <- paste0(health_projects$title, ".", health_projects$objective)
targetProy <- paste0("A smart ecosystem for the comprehensive clinical process of managing diabetes",
                     ". ",
                     "DIASFERA aims to be the first smart ecosystem to empower physicians' and patients’ ability to optimise type I diabetes
treatments improving their efficiency and lowering the associated economic and social burden.
Practitioners who treat people with diabetes feel that the efficiency of their treatments would greatly increase if they could be
there all the time to monitor their effects and consequently adjust them, while encouraging their patients’ adherence to
healthy lifestyle habits and to follow all the clinical practice guidelines.
Meanwhile patients say they often find themselves alone and helpless in facing the day-to-day diabetes and miss the
continued support of professional caregivers.
Therefore, we need to amplify the physician's ability to attend patients continuously between follow up visits eliminating
subjectivity as much as possible and focusing on the comprehensive clinical process of managing diabetes.
Based on our own technology, díaSfera focuses on the comprehensive clinical practice process and is built upon cutting
edge statistical learning and clinical data processing techniques.")
vProy <- c(vProy, targetProy)
idxTgtProy <- length(vProy)

health_proy_corp <- VCorpus(VectorSource(vProy),
                            readerControl = list(language = "en"))

health_proy_corp <- tm_map(health_proy_corp, removePunctuation)
f <- content_transformer(function(x, pattern, s) gsub(pattern, s, x))
health_proy_corp <- tm_map(health_proy_corp, f, "['’’\"]", " ")
health_proy_corp <- tm_map(health_proy_corp, f, "[[:punct:]]+", " ")
health_proy_corp <- tm_map(health_proy_corp, content_transformer(tolower))
health_proy_corp <- tm_map(health_proy_corp, removeWords, stopwords("english"))
health_proy_corp <- tm_map(health_proy_corp, f, " s ", " ")
health_proy_corp <- tm_map(health_proy_corp, stripWhitespace)
health_proy_corp <- tm_map(health_proy_corp, stemDocument, language = "english") 

# BUILD TDM
health_proy_tdm <- TermDocumentMatrix(health_proy_corp)

findFreqTerms(health_proy_tdm, 20)
findAssocs(health_proy_tdm, "diabet", 0.4)
inspect(removeSparseTerms(health_proy_tdm, 0.87))

# BUILD TDM WITH TfIdf
health_proy_tdm_TfIdf <- TermDocumentMatrix(health_proy_corp,
                                            control = list(weighting = weightTfIdf))
findFreqTerms(health_proy_tdm_TfIdf, 1.0)
findAssocs(health_proy_tdm_TfIdf, "diabet", 0.3)
inspect(removeSparseTerms(health_proy_tdm_TfIdf, 0.87))
# health_proy_tdm_TfIdf <- removeSparseTerms(health_proy_tdm_TfIdf, 0.87)


# LSA ---------------------------------------------------------------------

## PLAYING TO LEARN
library(lsa)
A <- as.matrix(health_proy_tdm_TfIdf)
lsaSpace <- lsa(A)  # create LSA space

dist.mat.lsa <- dist(t(as.textmatrix(lsaSpace)))  # compute distance matrix
dist.mat.lsa  # check distance mantrix

words <- lsaSpace$tk %*% diag(lsaSpace$sk)
docs  <- diag(lsaSpace$sk) %*% t(lsaSpace$dk)
tm_index("diabet")
# q <- (words["diabet", ] + words["data", ]) / 2
q <- words["diabet", ]
query_rank <- apply(docs, 2, function(x) {
  x %*% q / sqrt(sum(x^2)*sum(q^2))
})

query_rank <- query_rank[order(query_rank, decreasing = TRUE)]
head(query_rank)

vProy[as.integer(names(query_rank)[1])]

# TEST FUNCTIONS

source("lsaRelFuns.R")

head(relDocs4Query(health_proy_tdm_TfIdf, c("diabet")))
head(relDocs4Query(health_proy_tdm_TfIdf, c("diabet", "data")))

head(relWordsInDocs(health_proy_tdm_TfIdf, c(idxTgtProy)))
head(relWordsInDocs(health_proy_tdm_TfIdf, c(idxTgtProy, 24, 42, 6, 34, 12)))

head(relDocs2Docs(health_proy_tdm_TfIdf, c(idxTgtProy)))
vProy[[24]]
vProy[[42]]
head(relDocs2Docs(health_proy_tdm_TfIdf, c(47, 24, 42, 6, 34, 12)))


## NOW WITH BIGRAMS
BigramTokenizer <- function(x) {
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), 
           use.names = FALSE)
}

tdmCtrl <- list(tokenize = BigramTokenizer, 
                weighting = weightTfIdf)
health_proy_tdm_TfIdf2 <- TermDocumentMatrix(health_proy_corp,
                                            control = tdmCtrl)
inspect(removeSparseTerms(health_proy_tdm_TfIdf2[, 1:10], 0.9))
# health_proy_tdm_TfIdf2 <- removeSparseTerms(health_proy_tdm_TfIdf2, 0.99)

##
grep("diabet", health_proy_tdm_TfIdf2$dimnames$Terms, value = TRUE)
head(relDocs4Query(health_proy_tdm_TfIdf2, c("diabet care")))
vProy[34]
vProy[24]
vProy[3]

head(relDocs4Query(health_proy_tdm_TfIdf2, c("diabet care", "blood biomark")))
head(relDocs4Query(health_proy_tdm_TfIdf2, c("diabet care", "analysi data",
                                            "big data", "data analysi", 
                                            "data analyt")))
head(relDocs4Query(health_proy_tdm_TfIdf2, c("diabet care", "blood biomark")))


head(relWordsInDocs(health_proy_tdm_TfIdf2, c(idxTgtProy)))
head(relWordsInDocs(health_proy_tdm_TfIdf2, c(34, 3, 6, 42, 10, 24)))

# Probatina 1-grams + 2-grams
multi_tdm <- rbind(as.matrix(health_proy_tdm_TfIdf),
                   as.matrix(health_proy_tdm_TfIdf2))
head(relDocs4Query(multi_tdm, c("diabet", "analysi data",
                                "big data", "data analysi",
                                "data analyt")))
vProy[50]

intTerms <- grep("diabet | data", health_proy_tdm_TfIdf2$dimnames$Terms)
head(relDocs4Query(health_proy_tdm_TfIdf2, 
                   health_proy_tdm_TfIdf2$dimnames$Terms[intTerms]))
vProy[70]

intTerms <- grep("diabet", health_proy_tdm_TfIdf2$dimnames$Terms)
head(relDocs4Query(health_proy_tdm_TfIdf2, 
                   health_proy_tdm_TfIdf2$dimnames$Terms[intTerms]))
vProy[70]

## lsa facilities
myLSAspace <- lsa(as.matrix(health_proy_tdm_TfIdf))
myNewMatrix = as.textmatrix(myLSAspace) 
myNewMatrix # should look be different!

# compare two terms with the cosine measure
cosine(myNewMatrix["diabet",], myNewMatrix["clinic",])

# compare two documents with the cosine measure
cosine(myNewMatrix[,1], myNewMatrix[,2])
relDocs2Docs(health_proy_tdm_TfIdf, c(1))["2"]

# compare two documents with pearson
cor(myNewMatrix[,1], myNewMatrix[,2], method="pearson")
