
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
for (i in length(l))
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
# 
# a_nosound<-a[,-c(3,6,7,8,9,10,11)]
# model_ns<-lm(Price~.,data=a_nosound)
# summary(model_ns)
# 
# 
# l<-c()
# l<-as.numeric(predict(model_ns,b))
# sum=0
# for (i in length(l))
# {
#   sum=+((l[i]-l1[i])^2)
#   
# }
# #MSE
# mse_ns=sum/length(l)


#Plotting the residual plot.
plot1<-resid(model_s)
plot(a_sound$Price,plot1,xlab = "Price", ylab = "Residuals",
     main = "Car Residual plot")
abline(0,0)
summary(a)
summary(a_sound)
cat("The following attributes have been removed\n1.Cylinder\n2.Liter\n3.Cruise\n4.Type\n5.Doors\n6.Make\nThe New Car Worth data consists of the following attributes:\n1.Price\n2.Mileage\n3.Model\n4.Trim\n5.Sound\n6.Leather\nMSE value: (12 attributes)\n",mse_pre,"\nMSE value (after eleminating attributes):\n",MSE)
