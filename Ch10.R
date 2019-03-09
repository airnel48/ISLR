
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


