
#Decision Trees

#Classification trees
install.packages("tree")
library(tree)
library(ISLR)
attach(Carseats)
High=ifelse(Sales<=8,"No","Yes")
Carseats=data.frame(Carseats,High)

#build tree
tree.carseats=tree(High~.-Sales,Carseats)
summary(tree.carseats)
#visualize tree
plot(tree.carseats)
text(tree.carseats,pretty=0)
#print output corresponding to each branch
#split criterion, # of observations, deviance, prediction, and fraction of obs that are yes or no
tree.carseats

#split train and test
set.seed(2)
train=sample(1:nrow(Carseats),200)
Carseats.test=Carseats[-train,]
High.test=High[-train]
tree.carseats=tree(High~.-Sales,Carseats,subset=train)
tree.pred=predict(tree.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(86+57)/200
#correct for 71.5% of predictions

#prune tree -> cv.tree() performs cross-validation to determine optimal tree complexity
set.seed(3)
#FUN=prune.misclass indicates we want classification error rate to guide cross-validation and pruning process
#as opposed to default which is deviance
cv.carseats=cv.tree(tree.carseats,FUN=prune.misclass)
names(cv.carseats)
cv.carseats
#tree with 9 terminal nodes results in lowest error rate (50 cv errors)
#k is value of cost-complexity parameter

#plot error rate as a fxn of size and k
par(mfrow=c(1,2))
plot(cv.carseats$size,cv.carseats$dev,type="b")
plot(cv.carseats$k,cv.carseats$dev,type="b")

#apply prune.misclass() to prude to 9-node tree
prune.carseats=prune.misclass(tree.carseats,best=9)
plot(prune.carseats)
text(prune.carseats,pretty=0)
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(94+60)/200
#up to 77%


#fitting regression trees
library(MASS)
set.seed(1)
train=sample(1:nrow(Boston),nrow(Boston)/2)
tree.boston=tree(medv~.,Boston,subset=train)
summary(tree.boston)
plot(tree.boston)
text(tree.boston,pretty=0)
#prune
cv.boston=cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev,type='b')
#most complex tree is chosen by cross-validation so no need to prune

yhat=predict(tree.boston,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
plot(yhat,boston.test)
abline(0,1)
mean((yhat-boston.test)^2)
#test set MSE is 25.05. root of the MSE is around 5 indicating this model leads to test predictions
#within around $5k of true median value for the suburb

#bagging and random forests
install.packages("randomForest")
library(randomForest)
set.seed(1)
#bagging is just a special case of RF where all variables (13) are tested at each branch
#RF randomly selects n vars at each branch which reduces variance and improves performance
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,importance=TRUE)
bag.boston
yhat.bag=predict(bag.boston,newdata=Boston[-train,])
plot(yhat.bag,boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)
#much better MSE of 13.5

#try changing the number of trees
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,ntree=25)
yhat.bag=predict(bag.boston,newdata=Boston[-train,])
mean((yhat.bag-boston.test)^2)

#now build a RF. By default, randomForest() uses p/3 vars when building forest sqrt(p) when building
#classification trees
#let's try mtry=6
set.seed(1)
rf.boston=randomForest(medv~.,data=Boston,subset=train,mtry=6,importance=TRUE)
yhat.rf=predict(rf.boston,newdata=Boston[-train,])
mean((yhat.rf-boston.test)^2)
#Mse down to 11.7

#variable importance
importance(rf.boston)
varImpPlot(rf.boston)

#Boosting
install.packages("gbm")
library(gbm)
set.seed(1)
boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4)
#var importance
summary(boost.boston)
#partial dependence plots
par(mfrow=c(1,2))
plot(boost.boston,i="rm")
plot(boost.boston,i="lstat")
#predict medv on test set
yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-boston.test)^2)
#MSE 10.8 better than RF

#let's try a different shrinkage parameter. Default is 0.001, but let's try 0.01
boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth = 4,shrinkage=0.01,verbose=F)
yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-boston.test)^2)
#MSE down to 10.45

