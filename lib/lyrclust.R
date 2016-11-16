load("C:/Users/LIU/Desktop/ADS/P4/lyr.RData")
lyr <- lyr[,-c(2,3,6:30)]
library(tm)
f_words<-colSums(lyr[,-1])
stop_words <- stopwords("English")
del <- colnames(lyr[,-1]) %in% stop_words | f_words < 5
term.table <- f_words[!del]
vocab_m <- names(term.table)
index <- match(vocab_m,colnames(lyr))

lyr_cleaned<-lyr[,index]
trackid <- lyr[,1]
rownames(lyr_cleaned) <- trackid

# now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <-which(x != 0)
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}

documents <- apply(lyr_cleaned,1, get.terms)

##
D <- length(documents)  # number of documents 
W <- length(vocab_m)  # number of terms in the vocab 
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document 
N <- sum(doc.length)  # total number of tokens in the data 
term.frequency <- as.integer(term.table)  # frequencies of terms in the corpus 

##
K <- 18
G <- 1000
alpha <- 0.01
eta <- 0.01

# Fit the model:
library(lda)
set.seed(3154)
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab_m, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

ass <- theta

assignments <- apply(ass,2,which.max)

tracks <- names(documents)
lyrclust <- c()
for(i in 1:length(assignments)){
  n = length(assignments[[i]])
  for(j in 1:n){
    m <- cbind(tracks[i],assignments[[i]][j])
    lyrclust <- rbind(lyrclust,m)
  }
}


clustRank <- phi
for(i in 1:10)clustRank[i,] <- rank(clustRank[i,])

save(lyrclust,file="C:/Users/LIU/Desktop/ADS/P4/lyrclust.RData")
save(clustRank,file="C:/Users/LIU/Desktop/ADS/P4/clustRank.RData")

