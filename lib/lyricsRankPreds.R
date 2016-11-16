load("C:/Users/LIU/Desktop/ADS/P4/songs.RData")
load("C:/Users/LIU/Desktop/ADS/P4/lyrclust.RData")
load("C:/Users/LIU/Desktop/ADS/P4/clustRank.RData")

label <- lyrclust[,2]
dat <- cbind(songs,label)

ind <- c("duration","end_of_fade_in","key","key_confidence","loudness","mode",
         "mode_confidence","start_of_fade_out","tempo","time_signature","time_signature_confidence","track_id","label")
train <- dat[ind]
model_train <- subset(train,select = -track_id)

#####################load your test data here############################
test_dat <- train[1:100,-13] 
#########################################################################

library(class)

###########################cv training process###########################
cross_validation=function(features,labels,k,p){
  dat=cbind(features,labels)
  accuracy=vector()
  n=2350/k
  index=list()
  for (i in 1:k){
    index[[i]]=seq(i,2350,k)
  }
  for (i in 1:k){
    ind=index[[i]]
    train_dat=dat[-ind,]
    train_label=labels[-ind]
    test_dat=dat[ind,]
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
cv_k <- gen_knn(model_train[,-12],model_train$label)
#######################################################################

######################predict rank in test data########################
preds <- knn(model_train[,-12],test_dat[,-12],model_train$label,k=cv_k)
lyrRank <- data.frame()
predRank <- data.frame()
for(i in 1:length(test_dat$track_id)){
  j <- preds[i]
  r <- matrix(clustRank[j,],ncol=length(clustRank[j,]))
  lyrRank <- cbind(test_dat$track_id[j],r)
  predRank <- rbind(predRank,lyrRank)
}
names(predRank) <- c('id',colnames(clustRank))