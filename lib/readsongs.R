setwd("C:/Users/LIU/Desktop/ADS/P4/data")
files <- dir(recursive=TRUE, full.names=TRUE)

#source("http://bioconductor.org/biocLite.R")
#biocLite("rhdf5")
library(rhdf5)
ptm <- proc.time()
a_songs <- c()
for(file in files){
  dfanal <- h5read(file, "/analysis")
  a_song <- dfanal$songs
  a_songs <- rbind(a_songs,a_song)
}
proc.time() - ptm
songs <- cbind(files,a_songs)
save(songs,file="C:/Users/LIU/Desktop/ADS/P4/songs.RData")