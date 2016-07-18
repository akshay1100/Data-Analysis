
#loading data
a1 =read.csv("water-treatment.txt.data")

#cleaning data
a1=a1[,-1]
count=1
rm<-c()
for(i in 1:nrow(a1))
{
 for(j in 1:ncol(a1))
 {
   if(a1[i,j]=='?')
   {# cat(i," ")
     rm[count]<-i
     count=count+1
     break
   }}}
a<-a1[-rm,]

#finding mean
apply(a,2,mean)
 
####Clustering - 2 Types


#hierarchical clustering

hc.out<-hclust(dist(a))

hc.clusters =cutree (hc.out ,4)

hc.out

summary(hc.out)

summary(hc.clusters)

#kmeans

km.out

km.out=kmeans(a,4,nstart = 10)

km.clusters =km.out$cluster

summary(km.out)

summary(km.clusters)

table(km.clusters ,hc.clusters )

#plots

plot(hclust (dist(a),method ="complete"),main="CompleteLinkage ", xlab ="", sub ="", ylab ="")

plot(hclust (dist(a),method ="average"),main="averageLinkage ", xlab ="", sub ="", ylab ="")
