
rm(list=ls())
data=read.csv("Concrete_Data.csv")
data<-data[sample(nrow(data)),]
colnames(data)<-c("Cement","Blast Slag","Fly Ash","Water","Superplasticizer","Coarse Aggregate","Fine Aggregate","Age","Strength")
row.names(data) <- 1:nrow(data)
cross_no=5
#lm fit
len=floor(nrow(data)/cross_no)
set.seed(143)

set1=data[1:len,]
set2=data[(len+1):(2*len),]
row.names(set2) <- 1:nrow(set2)
set3=data[(2*len+1):(3*len),]
row.names(set3) <- 1:nrow(set3)
set4=data[(3*len+1):(4*len),]
row.names(set4) <- 1:nrow(set4)
set5=data[(4*len+1):(5*len),]
row.names(set5) <- 1:nrow(set5)


sets<-list("set1","set2","set3","set4","set5")
count=1
lm_acc<-c()
glm_acc<-c()
lda_acc<-c()
qda_acc<-c()
knn_acc<-c()
 for (i in sets)
{
  
  test=get(as.character(sets[count]))
  train_set=sets[-count]
  
  train=rbind(get(as.character(train_set[1])),get(as.character(train_set[2])),get(as.character(train_set[3])),get(as.character(train_set[4])))
  
  #lm fit
  
  lm.fit<-lm(Strength~.,data=train)
  pred=predict.lm(lm.fit,test[,-9])
  #prediction<-sapply(pred, FUN=function(x) if (x>0.5) 1 else 0)
  #lm_acc[count]<- sum(test$Strength==prediction)/length(prediction)
  sum=0
  for (i in 1:length(pred))
  {
    sum=sum+((pred[i]-test$Strength[i])^2)
  }
  #MSE
  lm_acc[count]=sum/length(pred)
  
  
  
  #glm fit
  t<-sapply(train$Strength, FUN=function(x) if (x>10) 1 else 0)
  t1<-sapply(test$Strength, FUN=function(x) if (x>10) 1 else 0)
  train1=train
  train1$Strength<-t
  test1=test
  test1$Strength<-t1
  
  mylogit <- glm(Strength~.,data =train , family = "gaussian")
  summary(mylogit)
  p<-predict(mylogit, newdata=test[,-9], type="response")
  #prediction1<-sapply(p, FUN=function(x) if (x>0.5) 1 else 0)
  #glm_acc[count]<- sum(test1$Strength==prediction1)/length(prediction1)
  
  sum=0
  for (i in 1:length(p))
  {
    sum=sum+((p[i]-test$Strength[i])^2)
  }
  #MSE
  glm_acc[count]=sum/length(p)
  
  
  #lda fit
  
  ldafit=lda(Strength~.,data=train)
  pred1=predict(ldafit,test[,-9])
  #ldaStrength<-pred1$class
  #table(ldaStrength,test$Strength)
  #lda_acc[count]<-mean(ldaStrength==test$Strength)
  sum=0
  for (i in 1:length(pred1$class))
  {
    sum=sum+((as.numeric(as.character(pred1$class[i]))-test$Strength[i])^2)
    cat(sum," ")
  }
  #MSE
  lda_acc[count]=sum/length(pred1)
  
  #qda fit
  
  
  qdafit=qda(Strength~.,data=train1)
  pred2=predict(qdafit,test[,-9])
  table(pred2$class,test1$Strength)
  qda_acc[count]<-mean(pred2$class==test1$Strength)
  
  #knn fit
  
  knnfit=knn(train1,test1,train1$Strength,k=5)
  table(knnfit,test1$Strength)
  knn_acc[count]<-mean(knnfit==test1$Strength)
  
  count=count+1
}
acc_lm<-mean(lm_acc)
acc_glm<-mean(glm_acc)
acc_lda<-mean(lda_acc)
acc_qda<-mean(qda_acc)
acc_knn<-mean(knn_acc)


