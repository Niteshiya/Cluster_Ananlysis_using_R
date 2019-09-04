#Loading the Data set
data <- iris
str(data)
#Scatter plot
plot(data$Petal.Length~data$Petal.Width,col=data$Species)
with(data,text(Petal.Length~Petal.Width,labels=Species,pos=2,cex=0.1))

#Normalization
z <- data[,-5]
head(z)
m <- apply(z,2,mean)
s <- apply(z,2,sd)
z <- scale(z,m,s)
head(z)

#Calculate Euclidean distance
distance <- dist(z)
distance
print(distance,digits=2)

#Cluster Dendrogram with compeate linkage
hc.cl <- hclust(distance)
hc.cl
attributes(hc.cl)
plot(hc.cl,labels=data$Species)
#If we draw a line on height=5 we get 2 cluster /2.5 we get 3 cluster

#Cluster Dendrogram with Average Linkage
hc.a <- hclust(distance,method="average")
plot(hc.a,labels=data$Species)


#Cluster membership
member.c <- cutree(hc.cl,3)
member.a <- cutree(hc.a,3)
table(member.c,member.a)

#Cluster Means
aggregate(z,list(member.c),mean)


#Silhouette Plot
library(cluster)
plot(silhouette(cutree(hc.cl,3),distance))

#K-means Clustering
kc  <- kmeans(z,3)
kc
plot(Petal.Length~Petal.Width,data,col=kc$cluster)
