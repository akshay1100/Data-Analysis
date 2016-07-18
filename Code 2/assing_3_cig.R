

rm(list=ls())
options(warn = -1)
a<-read.csv("cigarettes-data.csv")

names(a)
model<-lm(CO~Brand,data=a)

summary(model)

#Brand is not a predictor
par(mfrow=c(2,1))
model<-lm(CO~.-Brand,data=a)

summary(model)


#sampling data in sets of 75% for train and 25% for train.
set.seed(123)
a<-a[,-1]
k<-sample(1:nrow(a))
limit<-floor((0.75)*nrow(a))
train_<-sort(k[0:(limit)])
test_<-sort(k[(limit+1):nrow(a)])
train<-a[train_,,]
test<-a[test_,,]
row.names(train) <- 1:nrow(train)
row.names(test) <- 1:nrow(test)

model<-lm(CO~.,data=train)
plot1<-resid(model)
plot(train$CO,plot1,xlab = "C0", ylab = "Residuals",
     main = "Cigar Residual plot")
abline(0,0)

l<-c()
l<-as.numeric(predict(model,test))
l1<-test$CO
sum=0
for (i in length(l))
{
  sum=+((l[i]-l1[i])^2)
  
}
#MSE
MSE=sum/length(l)


model<-lm(CO~.,data=train)
summary(model)

names(a)
model1<-lm(CO~.-Weight,data=train)
summary(model1)

model2<-lm(CO~.-Nicotine,data=train)
summary(model2)

train1<-train[,-3]
test1<-test[,-3]

model1<-lm(CO~.,data=train1)
summary(model1)
outlierTest(model1)

qqPlot(model1,main='Studentized Residual')
plot2<-resid(model1)
plot(train$CO,plot2,xlab = "co", ylab = "Residuals",
     main = "Cigar 1 Residual plot")
abline(0,0)

#collinerality
modelk<-lm(Tar~Nicotine,data=train)
summary(modelk)
#thus v can reomvoe nicotine

train1<-train1[,-2]
test1<-test[,-2]

modelk1<-lm(CO~.,data=train1)
summary(modelk1)

outlierTest(modelk)
