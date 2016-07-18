
rm(list=ls())
data=read.csv("data_banknote_authentication.csv")
data<-data[sample(nrow(data)),]
colnames(data)<-c("Variance","Skewness","Curtosis","Entropy","Class")
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
set5=data[(4*len+1):(5*len+1),]
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

lm.fit<-lm(Class~.,data=train)
pred=predict.lm(lm.fit,test[,-5])
prediction<-sapply(pred, FUN=function(x) if (x>0.5) 1 else 0)
lm_acc[count]<- sum(test$Class==prediction)/length(prediction)



#glm fit


mylogit <- glm(Class~.,data =train , family = "binomial")
summary(mylogit)
p<-predict(mylogit, newdata=test[,-5], type="response")
prediction1<-sapply(p, FUN=function(x) if (x>0.5) 1 else 0)
glm_acc[count]<- sum(test$Class==prediction1)/length(prediction1)


#lda fit

ldafit=lda(Class~.,data=train)
pred1=predict(ldafit,test[,-5])
ldaclass<-pred1$class

table(ldaclass,test$Class)
lda_acc[count]<-mean(ldaclass==test$Class)

#qda fit

qdafit=qda(Class~.,data=train)
pred2=predict(qdafit,test[,-5])
table(pred2$class,test$Class)
qda_acc[count]<-mean(pred2$class==test$Class)

#knn fit

knnfit=knn(train,test,train$Class,k=3)
table(knnfit,test$Class)
knn_acc[count]<-mean(knnfit==test$Class)

count=count+1
}
acc_lm<-mean(lm_acc)
acc_glm<-mean(glm_acc)
acc_lda<-mean(lda_acc)
acc_qda<-mean(qda_acc)
acc_knn<-mean(knn_acc)


