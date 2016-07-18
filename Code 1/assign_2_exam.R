
#exams data set
rm(list=ls())
options(warn = -1)
b<-read.csv("Practice Test Effectiveness - Test.csv")
a<-read.csv("Practice Test Effectiveness - Train.csv")

summary(a)
k1<-as.numeric(as.character(a[,1]))
k<-na.omit(as.numeric(as.character(a[,1])))
m=1
l<-1:length(k)
for(u in 1:length(k1))
{ if(is.na(k1[u]))
{ next}
  if (k[m]==k1[u])
{l[m]=u
m=m+1}}
a1<-a[l,]
row.names(a1) <- 1:nrow(a1)
e<-as.numeric(as.character(a1$PracticeExam))
c<-mapply(function(x,index) {if (x>30){ return(index)} else {NA}},e,seq_along(e))
c<-na.omit(c)
train<-a1[c,]
row.names(train) <- 1:nrow(train)

#l<-lapply(k,function(y) {i=1; if(y==w[i]){ return(i)}    }})
summary(a)
k1<-as.numeric(as.character(b[,1]))
k<-na.omit(as.numeric(as.character(b[,1])))
m=1
l<-1:length(k)
for(u in 1:length(k1))
{ if(is.na(k1[u]))
{ next}
  if (k[m]==k1[u])
  {l[m]=u
  m=m+1}}
b1<-b[l,]
row.names(b1) <- 1:nrow(b1)
e<-as.numeric(as.character(b1$PracticeExam))
c<-mapply(function(x,index) {if (x>30){ return(index)} else {NA}},e,seq_along(e))
c<-na.omit(c)
test<-b1[c,]
row.names(test) <- 1:nrow(test)
#----------------------------------

train$PracticeExam<-as.numeric(levels(train$PracticeExam)[train$PracticeExam])
test$PracticeExam<-as.numeric(levels(test$PracticeExam)[test$PracticeExam])

#---------linear
model<-lm(RealExam~PracticeExam,data=train)
summary(model)
list2<-c()
list2<-as.numeric(predict(model,test))

l1<-test$RealExam

sum=0
for (i in length(list2))
{
  sum = sum + ((list2[i]-l1[i])^2)
  
}
#MSE
MSE=sum/length(list2)


plot1<-resid(model)
plot(train$RealExam,plot1)
abline(0,0)

#---------------Cubic
model1<-lm(RealExam~I(PracticeExam^3),data=train)
summary(model1)
list2<-c()
list2<-as.numeric(predict(model1,test))

l1<-test$RealExam

sum=0
for (i in length(list2))
{
  sum = sum + ((list2[i]-l1[i])^2)
  
}
#MSE
MSE_cubic=sum/length(list2)


plot1<-resid(model1)
plot(train$RealExam,plot1,xlab='RealExam',ylab='Residuals',main='Exam Residual Plot')
abline(0,0)

summary(model)
summary(model1)
cat("MSE value for linear Model:\n",MSE,"\nMSE value for Cubic Model:\n",MSE_cubic)



# Cat("Mse value is:",MSE)
# model1<-lm(RealExam~I(PracticeExam^3),data=train)
# summary(model1)
# plot1<-resid(model1)
# plot(train$RealExam,plot1)
# abline(0,0)
