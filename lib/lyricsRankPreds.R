load("C:/Users/LIU/Desktop/ADS/P4/songs.RData")
load("C:/Users/LIU/Desktop/ADS/P4/lyrclust.RData")
load("C:/Users/LIU/Desktop/ADS/P4/clustRank.RData")

label <- lyrclust[,2]
dat <- cbind(songs,label)

train_dat <- dat[,-1]
rownames(train_dat) <- c(1:2350)
train_dat <- as.data.frame(train_dat)
train_dat <- train_dat[-c(715,950,991,1112,1325,1375,1658,1705,2284,2300),] #remove non-numeric rows
#####################load your test data here############################
load("C:/Users/LIU/Desktop/ADS/P4/test_dat.RData")
rownames(test_dat) <- c(1:100)
test_dat <- as.data.frame(test_dat)
#########################################################################

library(class)

###########################cv training process###########################
cross_validation=function(features,labels,k,p){
  #dat=cbind(features,labels)
  accuracy=vector()
  n=2340/k
  index=list()
  for (i in 1:k){
    index[[i]]=seq(i,2340,k)
  }
  for (i in 1:k){
    ind=index[[i]]
    train_dat=features[-ind,]
    train_label=labels[-ind]
    test_dat=features[ind,]
    test_label=labels[ind]
    pred=knn(train_dat,test_dat,train_label,k=p)
    
    accuracy[i]=mean(pred==test_label)
  }
  return(mean(accuracy))
}
gen_knn=function(features,labels){
  k=5
  p=c(1:10)
  acc=vector()
  j=1
  for (i in p){
    acc[j]=cross_validation(features,labels,k,i)
    j=j+1
  }
  ind=which.max(acc)
  print(acc[ind])
  final.p=p[ind]
  return(final.p)
}
cv_k <- gen_knn(train_dat[,-16],train_dat$label)
#######################################################################

######################predict rank in test data########################
preds <- knn(train_dat[,-16],test_dat[,-1],train_dat$label,k=cv_k)
lyrRank <- data.frame()
predRank <- data.frame()
for(i in 1:length(test_dat$files)){
  j <- preds[i]
  r <- matrix(clustRank[j,],ncol=length(clustRank[j,]))
  lyrRank <- cbind(test_dat$files[i],r)
  predRank <- rbind(predRank,lyrRank)
}
names(predRank) <- c('files',colnames(clustRank))

write.csv(predRank,"C:/Users/LIU/Desktop/ADS/P4/predRank.csv")
