setwd("C:/Users/LIU/Desktop/ADS/P4/data")
files <- dir(recursive=TRUE, full.names=TRUE)

#source("http://bioconductor.org/biocLite.R")
#biocLite("rhdf5")
library(rhdf5)
ptm <- proc.time()
analysis <- c()
for(file in files){
  dfanal <- h5read(file, "/analysis")
  temp <- c()
  for(i in 1:length(names(dfanal))){
    temp[i] <- mean(dfanal[[i]])
  }
  analysis <- rbind(analysis,temp)
}
colnames(analysis) <- names(dfanal)
proc.time() - ptm
songs <- cbind(files,analysis)
save(test_dat,file="C:/Users/LIU/Desktop/ADS/P4/songs.RData")
