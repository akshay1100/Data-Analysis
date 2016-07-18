
a<-read.csv("lowbwt.csv")

#removing the btw
a<-a[,-11]
k<-sample(1:nrow(a))
limit<-floor((0.75)*nrow(a))
train_<-sort(k[0:(limit)])
test_<-sort(k[(limit+1):nrow(a)])
train<-a[train_,,]
test<-a[test_,,]
row.names(train) <- 1:nrow(train)
row.names(test) <- 1:nrow(test)

mylogit <- glm(LOW~.,data = a, family = "binomial")
summary(mylogit)
p<-predict(mylogit, newdata=test[,-2], type="response")

threshold=0.5
prediction<-sapply(p, FUN=function(x) if (x>threshold) 1 else 0)
actual<-test$LOW
accuracy <- sum(actual==prediction)/length(actual)

names(train)


l_model<-lm(LOW~.,data=train)
summary(l_model)
a<-a[,c(1,2,7)]


k<-sample(1:nrow(a))
limit<-floor((0.75)*nrow(a))
train_<-sort(k[0:(limit)])
test_<-sort(k[(limit+1):nrow(a)])
train<-a[train_,,]
test<-a[test_,,]
row.names(train) <- 1:nrow(train)
row.names(test) <- 1:nrow(test)

mylogit <- glm(LOW~.,data = a, family = "binomial")
summary(mylogit)
p<-predict(mylogit, newdata=test[,-2], type="response")

threshold=0.5
prediction<-sapply(p, FUN=function(x) if (x>threshold) 1 else 0)
actual<-test$LOW
accuracy1 <- sum(actual==prediction)/length(actual)