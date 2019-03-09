
#Unsupervised Learning

#Principle Components Analysis

states=row.names(USArrests)
states
names(USArrests)
apply(USArrests,2,mean)
apply(USArrests,2,var)
#due to the large differences in mean and variance, we need to scale the variables before PCA
#PCA
pr.out=prcomp(USArrests,scale=TRUE)
names(pr.out)
#center is mean and scale is stdev
pr.out$center
pr.out$scale
#rotation matrix contains principal component loadings
#each column of pr.out$rotatoin cotnains the corresponding principal comonent loading vector
pr.out$rotation
#principal component loading vectors * data = principal component score vectors
#prcomp() automatically calculates these in matrix x
dim(pr.out$x)
#plot first two principal components
biplot(pr.out,scale=0)
#scale=0 biplot ensures that the arrows are scaled to represent the loadings
#other values for scale give slightly different biplots with different interpretatins

#sdev of each component
pr.out$sdev
pr.var=pr.out$sdev^2
pr.var
#proportion of variance explained by each principal component
pve=pr.var/sum(pr.var)
pve
plot(pve,xlab="Principal Component",ylab="Proportion of Variance Explained",ylim=c(0,1),type='b')
plot(cumsum(pve),xlab="Principal Component",ylab="Cumulative Proportion of Var Explained",ylim=c(0,1),type='b')



#Clustering

set.seed(2)
x=matrix(rnorm(50*2),ncol=2)
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4

km.out=kmeans(x,2,nstart=20)
km.out$cluster
plot(x,col=(km.out$cluster+1),main="K-Means Clustering Results with K=2",xlab="",ylab="",pch=20,cex=2)

#now with k=3
set.seed(4)
km.out=kmeans(x,3,nstart=20)
km.out
#nstart sets the nmber of multile random assignments in step 1

#let's compare to nstart=1
set.seed(3)
km.out=kmeans(x,3,nstart=1)
km.out$tot.withinss
km.out=kmeans(x,3,nstart=20)
km.out$withinss

#hierarchical clustering

#with varying linkages
hc.complete=hclust(dist(x),method="complete")
hc.average=hclust(dist(x),method="average")
hc.single=hclust(dist(x),method="single")
par(mfrow=c(1,3))
plot(hc.complete,main="Complete LInkage",xlab="",sub="",cex=.9)
plot(hc.average,main="Average Linkage",xlab="",sub="",cex=.9)
plot(hc.single,main="Average Linkage",xlab="",sub="",cex=.9)

#cutree() dislays cluster labels for each obs
cutree(hc.complete,2)
cutree(hc.average,2)
cutree(hc.single,2)

#to scale vars before performing hierarchical clustering
xsc=scale(x)
plot(hclust(dist(xsc),method="complete"),main="Hierarchical CLustering with Scaled Features")

#we can use correlation-based distance with as.dist() 
#need at least 3 dimensions to do this
x=matrix(rnorm(30*3),ncol=3)
dd=as.dist(1-cor(t(x)))
plot(hclust(dd,method="complete"),main="Complete Linkage with Correlation-based Distance",xlab="",sub="")


#NCI60 Example - Genomic Data
library(ISLR)
nci.labs=NCI60$labs
nci.data=NCI60$data
dim(nci.data)
nci.labs[1:4]
table(nci.labs)

#PCA on NCI60 data
pr.out=prcomp(nci.data,scale=TRUE)

#to visualize, assign a distinct color to each cancer type
Cols=function(vec){
  cols=rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}

par(mfrow=c(1,2))
plot(pr.out$x[,1:2],col=Cols(nci.labs),pch=19,xlab="Z1",ylab="Z2")
plot(pr.out$x[,c(1,3)],col=Cols(nci.labs),pch=19,xlab="Z1",ylab="Z3")
#it looks like cell lines from the same cancer type tend to have pretty similar gene expression levels

#Proportion Variance Explained
summary(pr.out)
plot(pr.out)
#scree plot
pve=100*pr.out$sdev^2/sum(pr.out$sdev^2)
par(mfrow=c(1,2))
plot(pve,type="o",ylab="PVE",xlab="Principal Component",col="blue")
plot(cumsum(pve),type="o",ylab="Cumulative PVE",xlab="Principal Component",col="brown3")


#clustering NCI60 data

#scale
sd.data=scale(nci.data)
#hierarchical clustering
par(mfrow=c(1,3))
data.dist=dist(sd.data)
plot(hclust(data.dist),lables=nci.labs,main="Complete Linkage",xlab="",sub="",ylab="")
plot(hclust(data.dist, method="average"),lables=nci.labs,main="Average Linkage",xlab="",sub="",ylab="")
plot(hclust(data.dist, method="single"),lables=nci.labs,main="Single Linkage",xlab="",sub="",ylab="")

#cut the dendrogram at 4 clusters
hc.out=hclust(dist(sd.data))
hc.clusters=cutree(hc.out,4)
table(hc.clusters,nci.labs)

#plot the dendrogram that produced these 4 clusters
par(mfrow=c(1,1))
plot(hc.out,lables=nci.labs)
abline(h=139,col="red")

hc.out

#kmeans
set.seed(2)
km.out=kmeans(sd.data,4,nstart=20)
km.clusters=km.out$cluster
table(km.clusters,hc.clusters)
#clusters are different between kmeans and hierarchical

#perform clustering on the first few principal components
hc.out=hclust(dist(pr.out$x[,1:5]))
plot(hc.out,labels=nci.labs,main="Hier. Clust. on First Five Score Vectors")
table(cutree(hc.out,4),nci.labs)
