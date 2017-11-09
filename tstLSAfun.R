### LSAfun package
# https://link.springer.com/article/10.3758/s13428-014-0529-0


# Getting an LSA space ----------------------------------------------------

### GET CORDIS DATA

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

### BUILD TERM-BY-DOCUMENT MATRIX (TDM)

## BUILD CORPUS
library(tm)
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

## PROCESS CORPUS
health_proy_corp <- tm_map(health_proy_corp, removePunctuation)
f <- content_transformer(function(x, pattern, s) gsub(pattern, s, x))
health_proy_corp <- tm_map(health_proy_corp, f, "['’’\"]", " ")
health_proy_corp <- tm_map(health_proy_corp, f, "[[:punct:]]+", " ")
health_proy_corp <- tm_map(health_proy_corp, content_transformer(tolower))
health_proy_corp <- tm_map(health_proy_corp, removeWords, stopwords("english"))
health_proy_corp <- tm_map(health_proy_corp, f, " s ", " ")
health_proy_corp <- tm_map(health_proy_corp, stripWhitespace)
health_proy_corp <- tm_map(health_proy_corp, stemDocument, language = "english") 

## BUILD TDM (WITH TfIdf)
health_proy_tdm_TfIdf <- TermDocumentMatrix(health_proy_corp,
                                            control = list(weighting = weightTfIdf))
findFreqTerms(health_proy_tdm_TfIdf, 1.0)
findAssocs(health_proy_tdm_TfIdf, "diabet", 0.3)
inspect(removeSparseTerms(health_proy_tdm_TfIdf, 0.87))
# health_proy_tdm_TfIdf <- removeSparseTerms(health_proy_tdm_TfIdf, 0.87)


### Conducting the SVD and performing a dimensionality reduction with lsa()

library(lsa)
A <- as.matrix(health_proy_tdm_TfIdf)
lsaSpace <- lsa(A)  # create LSA space

words <- lsaSpace$tk %*% diag(lsaSpace$sk)
docs  <- diag(lsaSpace$sk) %*% t(lsaSpace$dk)
colnames(docs) <- paste0("d", colnames(docs))


# Computing similarities --------------------------------------------------
library(LSAfun)
## Cosine() computes the cosine similarity between two single words:
Cosine("diabet", "data", tvectors = words)

# Or documents:
Cosine("d23", "d60", tvectors = t(docs))

# multicos() computes all pairwise word similarities for two lists of words
multicos("diabet data", "diasfera practic clinic econom", 
         tvectors = words)

# Or documents:
multicos(c("d23", "d40"), "d12 d1 d60", tvectors = t(docs))

# costring() computes the similarity between two documents consisting of 
# multiple words
costring(vProy[[12]], vProy[[24]], tvectors = words)

# multicostring() computes the similarities between a document and a list of 
# single words
multicostring(vProy[[12]], "diasfera practic clinic econom", tvectors = words)

# Neighborhood computations -----------------------------------------------

# takes single words or documents as input and computes their nearest neighbors,
# i.e. the words with the largest LSA cosine to this input.
neighbors("diabet", n = 10, tvectors = words)

neighbors("d12", n = 10, tvectors = t(docs))

# A more generalized version of the neighbors() function is the choose.target() 
# function. This provides the possibility to not just compute the nearest words 
# to a given input, but to randomly sample words within any given range of 
# similarity to the input. 
choose.target("diabet", lower = .7, upper = .9, n = 10, tvectors = words)


# Plots and multidimensional scaling --------------------------------------
plot_neighbors("clinic", n = 10, tvectors = words, method = "PCA",
               dims = 3, connect.lines = "all", alpha = "shade")
plot_neighbors("clinic", n = 10, tvectors = words, method = "MDS",
               dims = 2)

vwords <- c("clinic", "practic", "least", "insur", "growth", "guidelin",
           "clear", "rollout", "treatment", "physician")
plot_wordlist(vwords, method = "MDS", dims = 2, tvectors = words)
