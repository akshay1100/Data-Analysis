
rm(list=ls())
options(warn = -1)
b<-read.csv("Car Worth - Test.csv")
a<-read.csv("Car Worth - Train.csv")
model<-lm(Price~.,data=a)
summary(model)
l1<-b$Price
#predicting before removing any redundent terms
l<-as.numeric(predict(model,b))
sum=0
for (i in 1:length(l))
{
  sum=+((l[i]-l1[i])^2)
  
}
#MSE
mse_pre=sum/length(l)


#Removing extra terms:
model3<-lm(Price~.-Cruise-Doors-Cylinder,data=a)
summary(model3)
cat("The attribute Cylinder can be dropped from table as it provides no real improvement")

#Removing extra terms:
model6<-lm(Price~.-Cruise-Doors-Cylinder-Type,data=a)
summary(model6)
cat("The attribute Type can be dropped from table as it provides no real improvement")

#Removing extra terms:
model7<-lm(Price~.-Cruise-Doors-Cylinder-Type-Liter,data=a)
summary(model7)
cat("The attribute Liter can be dropped from table as it provides no real improvement")

#Removing extra terms:
model7<-lm(Price~.-Cruise-Doors-Cylinder-Type-Liter-Make,data=a)
summary(model7)
cat("The attribute Make can be dropped from table as it provides no real improvement")

#Creating a new datamodel with by dropping the extra atrributes
a_sound<-a[,-c(3,6,7,8,9,10)]
model_s<-lm(Price~.,data=a_sound)
summary(model_s)
l<-c()
l<-as.numeric(predict(model_s,b))
sum=0
for (i in length(l))
{
  sum=+((l[i]-l1[i])^2)
  
}
#MSE
MSE=sum/length(l)


#Plotting the residual plot.
plot1<-resid(model_s)
plot(a_sound$Price,plot1,xlab = "Price", ylab = "Residuals",
     main = "Car Residual plot")
abline(0,0)
summary(a)
summary(a_sound)
cat("The following attributes have been removed\n1.Cylinder\n2.Liter\n3.Cruise\n4.Type\n5.Doors\n6.Make\nThe New Car Worth data consists of the following attributes:\n1.Price\n2.Mileage\n3.Model\n4.Trim\n5.Sound\n6.Leather\nMSE value: (12 attributes)\n",mse_pre,"\nMSE value (after eleminating attributes):\n",MSE)



outlierTest(model_s)
#finding outliers

qqPlot(model_s,main='Car Studenetized Residuals')
#finding the outliers through plots

new_data<-a_sound[-c(359,105,446,644),]
model_new=lm(Price~.,data=new_data)
qqPlot(model_new)
summary(model_s)
summary(model_new)
plot1<-resid(model_new)
plot(new_data$Price,plot1,xlab = "Price", ylab = "Residuals",
     main = "Car Residual plot")
abline(0,0)



outlierTest(model_new)
#finding outliers
which.max(hatvalues(model_s))
#The hat matrix provides a measure of leverage.
qqPlot(model_new,main='sdf')

#r^2=0.9923 , rss=825.8
#predicting mse for new_data

l<-c()
l<-as.numeric(predict(model_new,b))
sum=0
for (i in length(l))
{
  sum=+((l[i]-l1[i])^2)
}
#MSE
MSE1=sum/length(l)


names(a_sound$Model)
dist<-unique(a_sound$Trim)
dist_full<-(a_sound$Model)
num<-1:32
list12<-c()
num
nrow(a_sound)

a_sound[,3]=as.numeric(a_sound[,3])
a_sound[,4]=as.numeric(a_sound$Trim)


#checking for correlations
cor(a_sound$Mileage,a_sound$Model)
cor(a_sound$Model,a_sound$Trim)
