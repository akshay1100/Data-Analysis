

require(e1071)

#remove the first line from the dataset and then load the data


  a<-read.csv("bloodDonorData.txt")
  
  colnames(a)<-c("x1","x2","x3","x4","x5")
  
  #Sampling
  smp_size <- floor(0.75 * nrow(a))
  train_ind <- sample(seq_len(nrow(a)), size = smp_size)
  train <- a[train_ind, ]
  test <- a[-train_ind, ]

#Linear model
  svmfit =svm(as.factor(x5)~., data=a , kernel="linear", cost =10,scale =FALSE )
  
  summary(svmfit)

#tune  
tune.out=tune(svm,as.factor(x5)~.,data=train ,kernel ="linear",ranges =list(cost=c(0.001 , 0.01, 0.1, 1,5,10,100) ))
  
#bestmodel
bestmod =tune.out$best.model
summary(bestmod)




#refitting the data
  #svmfit1 =svm(as.factor(V1)~., data=train , #kernel="linear", cost =0.001,scale =FALSE )

#Accuracy

ypred=predict(bestmod ,test )
table(predict =ypred , truth= test$x5 )

#accurcy is 76%


#-----------------------
#Radial model (non linear)

tune.out=tune(svm , as.factor(x5)~., data=train, kernel ="radial",ranges =list(cost=c(0.1 ,1 ,10 ,100 ,1000),gamma=c(0.5,1,2,3,4) ))

#bestmodel
bestmod =tune.out$best.model
summary(bestmod)


#Accuracy

ypred=predict(bestmod ,test )
table(predict =ypred , truth= test$x5 )

#accurcy is 75.4%
