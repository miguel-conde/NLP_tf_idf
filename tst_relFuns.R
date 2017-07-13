# GET CORDIS DATA
library(readxl)
cordis_h2020projects <- read_excel("./DATA/cordis-h2020projects.xlsx")

## Health topics
library(dplyr)
health_projects <- filter(cordis_h2020projects, 
                          topics == "SMEInst-06-2016-2017")

health_projects_SME1 <- filter(health_projects, fundingScheme == "SME-1" )
health_projects_SME2 <- filter(health_projects, fundingScheme == "SME-2" )

# BUILD CORPUS
library(tm)
vProy <- c(health_projects$title, health_projects$objective)
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
findFreqTerms(health_proy_tdm_TfIdf, 20)
findAssocs(health_proy_tdm_TfIdf, "diabet", 0.3)
inspect(removeSparseTerms(health_proy_tdm_TfIdf, 0.87))


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

head(relWordsInDocs(health_proy_tdm_TfIdf, c(88)))
head(relWordsInDocs(health_proy_tdm_TfIdf, c(80, 34, 88, 70, 12)))


## NOW WITH BIGRAMS
BigramTokenizer <- function(x) {
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), 
           use.names = FALSE)
}

tdmCtrl <- list(tokenize = BigramTokenizer, 
                weighting = weightTfIdf)
health_proy_tdm_TfIdf <- TermDocumentMatrix(health_proy_corp,
                                            control = tdmCtrl)
inspect(removeSparseTerms(health_proy_tdm_TfIdf[, 1:10], 0.9))

##
grep("diabet", health_proy_tdm_TfIdf$dimnames$Terms, value = TRUE)
head(relDocs4Query(health_proy_tdm_TfIdf, c("diabet care")))
vProy[34]
vProy[70]

head(relDocs4Query(health_proy_tdm_TfIdf, c("diabet care", "ambulatori set")))
head(relDocs4Query(health_proy_tdm_TfIdf, c("diabet care", "analysi data",
                                            "big data", "data analysi", 
                                            "data analyt")))
head(relDocs4Query(health_proy_tdm_TfIdf, c("diabet care", "ambulatori set")))


head(relWordsInDocs(health_proy_tdm_TfIdf, c(88)))
head(relWordsInDocs(health_proy_tdm_TfIdf, c(80, 34, 88, 70, 12)))
