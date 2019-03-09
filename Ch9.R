
#Support Vector Machines

#e1071 contains implementations for a number of stat learning methods
#svm() can be used to fit support vector classifier when kernel="linear" is used

#Support Vector Classifier
set.seed(1)
x=matrix(rnorm(20*2),ncol=2)
y=c(rep(-1,10),rep(1,10))
x[y==1,]=x[y==1,]+1
plot(x,col=(3-y))

dat=data.frame(x=x,y=as.factor(y))
library(e1071)
svmfit=svm(y~.,data=dat,kernel="linear",cost=10,scale=FALSE)
#argument scale=FALSE tells svm() not to scale each feature to have mean 0 or stdv 1

plot(svmfit,dat)
#decision boundary is linear since we used kernel="linear"
#support vectors are plotted as crosses and remaining observations are circles

#identify support vectors
svmfit$index

summary(svmfit)
#we have 7 support vectors. 4 on one side and 3 on the other. Cost is 10

#try a smaller value on cost parameter
svmfit=svm(y~.,data=dat,kernel="linear",cost=0.1,scale=FALSE)
plot(svmfit,dat)
svmfit$index
#number of support vectors increases bc margin is wider

#cross validation
# e1071 library has tune() fxn for cv
set.seed(1)
tune.out=tune(svm,y~.,data=dat,kernel="linear",ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)))
summary(tune.out)
#best cost is 0.1

bestmod=tune.out$best.model
summary(bestmod)

#generate a test data set
xtest=matrix(rnorm(20*2),ncol=2)
ytest=sample(c(-1,1),20,rep=TRUE)
xtest[ytest==1,]=xtest[ytest==1,]+1
testdat=data.frame(x=xtest,y=as.factor(ytest))

#now predict class labels
ypred=predict(bestmod,testdat)
table(predict=ypred,truth=testdat$y)
#17 correct predictions

#what if we try cost =0.01
svmfit=svm(y~.,data=dat,kernel="linear",cost=.01,scale=FALSE)
ypred=predict(svmfit,testdat)
table(predict=ypred,truth=testdat$y)

#if two classes arelinearly separable we can find a separating hyperplane
#first need to further separate the two classes in our simulated data
x[y==1,]=x[y==1,]+0.5
plot(x,col=(y+5)/2,pch=19)
#now we fit svc and plot hyperplane using a large value of cost so no obs are misclassified
dat=data.frame(x=x,y=as.factor(y))
svmfit=svm(y~.,data=dat,kernel="linear",cost=1e5)
summary(svmfit)
plot(svmfit,dat)
#margin is very narrow so model is probably overfit
#now try a smaller value of cost
svmfit=svm(y~.,data=dat,kernel="linear",cost=1)
summary(svmfit)
plot(svmfit,dat)
#we now mislcassify one obs, but we obtain a wider margin and make use of 7 support vectors instead of 3


#support vector machine

set.seed(1)
x=matrix(rnorm(200*2),ncol=2)
x[1:100,]=x[1:100,]+2
x[101:150,]=x[101:150,]-2
y=c(rep(1,150),rep(2,50))
dat=data.frame(x=x,y=as.factor(y))
#plotting data makes it clear class boundary is nonliner
plot(x,col=y)

train=sample(200,100)
svmfit=svm(y~.,data=dat[train,],kernel="radial",gamma=1,cost=1)
plot(svmfit,dat[train,])
summary(svmfit)

#we can reduce training errors by increasing cost, but this comes at risk of overfitting
svfit=svm(y~.,data=dat[train,],kernel="radial",gamma=1,cost=1e5)
plot(svmfit,dat[train,])

#cross validation
set.seed(1)
tune.out=tune(svm,y~.,data=dat[train,],kernel="radial",ranges=list(cost=c(0.1,1,10,100,1000),
                                                                   gamma=c(0.5,1,2,3,4)))
summary(tune.out)
#best choice is cost=1 and gamma=2
table(true=dat[-train,"y"],pred=predict(tune.out$best.model,newdata=dat[-train,]))
#10% misclassification rate

#ROC curves
install.packages("ROCR")
library(ROCR)
rocplot=function(pred,truth,...){
  predob=prediction(pred,truth)
  perf=performance(predob,"tpr","fpr")
  plot(perf,...)}
#to obtain fitted values for SVM we use decision.values=TRUE
svmfit.opt=svm(y~.,data=dat[train,],kernel="radial",gamma=2,cost=1,decision.values=T)
fitted=attributes(predict(svmfit.opt,dat[train,],decision.values=TRUE))$decision.values
par(mfrow=c(1,2))
rocplot(fitted,dat[train,"y"],main="training data")
#we can produce a more flexible fit by increasing gamma
svmfit.flex=svm(y~.,data=dat[train,],kernel="radial",gamma=50,cost=1,decision.values=T)
fitted=attributes(predict(svmfit.flex,dat[train,],decision.values=TRUE))$decision.values
rocplot(fitted,dat[train,"y"],main="training data")

#note - these ROC curves are all on the training data
#usting test data, gamma=2 produces better results
fitted=attributes(predict(svmfit.opt,dat[-train,],decision.values=TRUE))$decision.values
rocplot(fitted,dat[-train,"y"],main="test data")
fitted=attributes(predict(svmfit.flex,dat[-train,],decision.values=TRUE))$decision.values
rocplot(fitted,dat[-train,"y"],add=T,col="red")

#SVM with multiple cases
set.seed(1)
x=rbind(x,matrix(rnorm(50*2),ncol=2))
y=c(y,rep(0,50))
x[y==0,2]=x[y==0,2]+2
dat=data.frame(x=x,y=as.factor(y))
par(mfrow=c(1,1))
plot(x,col=(y+1))
svmfit=svm(y~.,data=dat,kernel="radial",cost=10,gamma=1)
plot(svmfit,dat)

#application to gene expression data
library(ISLR)
names(Khan)
dim(Khan$xtrain)
dim(Khan$xtest)
length(Khan$ytrain)
length(Khan$ytest)
table(Khan$ytrain)
table(Khan$ytest)

#we will use support vector to predict cancer subtype
#there are a large number of features relative to observations
#this suggests we hsould use a linear kernel bc additional flexibility from polynomial or radial isn't needed

data=data.frame(x=Khan$xtrain,y=as.factor(Khan$ytrain))
out=svm(y~.,data=dat,kernel="linear",cost=10)
summary(out)
table(out$fitted,dat$y)
# there are NO training errors
#this isn't surprising as large number of vars relative to obs implied it is easy to find hyperplanes
#that fully separate classes

#now on test samples
dat.te=data.frame(x=Khan$xtest,y=as.factor(Khan$ytest))
pred.te=(predict(out,newdata=dat.te))
table(pred.te,dat.te$y)
