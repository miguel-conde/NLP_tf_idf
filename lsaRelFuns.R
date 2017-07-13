library(lsa)

# RELevant DOCS FOR a QUERY
relDocs4Query <- function(tdm, qWords) {
  A <- as.matrix(tdm)
  lsaSpace <- lsa(A)  # create LSA space
  
  dist.mat.lsa <- dist(t(as.textmatrix(lsaSpace)))  # compute distance matrix
  
  words <- lsaSpace$tk %*% diag(lsaSpace$sk)
  docs  <- diag(lsaSpace$sk) %*% t(lsaSpace$dk)
  
  if (length(qWords) == 1) {
    q <- words[qWords, ]
  } else {
    q <- apply(words[qWords,], 2, mean) # Calc query vector
  }
  
  query_rank <- apply(docs, 2, function(x) {
    x %*% q / sqrt(sum(x^2)*sum(q^2))
  })
  
  query_rank <- query_rank[order(query_rank, decreasing = TRUE)]
  query_rank
}

# RELevant WORDS IN a set of DOCumentS
relWordsInDocs <- function(tdm, qDocs) {
  A <- as.matrix(tdm)
  lsaSpace <- lsa(A)  # create LSA space
  
  dist.mat.lsa <- dist(t(as.textmatrix(lsaSpace)))  # compute distance matrix
  
  words <- lsaSpace$tk %*% diag(lsaSpace$sk)
  docs  <- diag(lsaSpace$sk) %*% t(lsaSpace$dk)
  
  if (length(qDocs) == 1) {
    q <- docs[, qDocs]
  } else {
    q <- apply(docs[, qDocs], 1, mean) # Calc query vector
  }
  
  query_rank <- apply(words, 1, function(x) {
    x %*% q / sqrt(sum(x^2)*sum(q^2))
  })
  
  query_rank <- query_rank[order(query_rank, decreasing = TRUE)]
  query_rank
}