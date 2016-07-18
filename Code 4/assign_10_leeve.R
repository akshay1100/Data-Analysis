
require(e1071)
a =read.table("mmr_levee.dat")

#linear
 
svmfit =svm(as.factor(V1)~., data=a , kernel="linear", cost =10,scale =FALSE )
 
 
#k=data.frame(a)
#plot(svmfit , a

summary(svmfit)

#tune
tune.out=tune(svm,as.factor(V1)~.,data=a ,kernel ="linear",ranges =list(cost=c(0.001 , 0.01, 0.1, 1,5,10,100) ))

#bestmodel
bestmod =tune.out$best.model
summary(bestmod)



#accuracy
smp_size <- floor(0.75 * nrow(a))
train_ind <- sample(seq_len(nrow(a)), size = smp_size)
train <- a[train_ind, ]
test <- a[-train_ind, ]
 
svmfit1 =svm(as.factor(V1)~., data=train , kernel="linear", cost =0.1,scale =FALSE )
ypred=predict(svmfit1 ,test )
table(predict =ypred , truth= test$V1 )

#accuracy = 10/18 *100 ==55%


#Nonlienar
#tune
tune.out=tune(svm , V1~., data=train, kernel ="radial",ranges =list(cost=c(0.1 ,1 ,10 ,100 ,1000),gamma=c(0.5,1,2,3,4) ))

#accuracy

svmfit1 =svm(as.factor(V1)~., data=train , kernel="radial",gamma=2, cost =10,scale =FALSE )
ypred=predict(svmfit1 ,test )
table(predict =ypred , truth= test$V1 )
