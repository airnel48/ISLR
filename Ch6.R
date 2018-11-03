#Linear model selection and regularization

#Best subset selection
library(ISLR)
fix(Hitters)
?fix
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))
Hitters=na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))
library(leaps)
regfit.full=regsubsets(Salary~.,Hitters)
summary(regfit.full)
regfit.full=regsubsets(Salary~.,data=Hitters,nvmax=19)
reg.summary=summary(regfit.full)
names(reg.summary)
reg.summary$rsq
par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Vars", ylab="RSS",type="1")
plot(reg.summary$adjr2,xlab="Number of Vars", ylab="Adj Rsq",type="1")
which.max(reg.summary$adjr2)
points(11,reg.summary$adjr2[11],col="red",cex=2,pch=20)
plot(reg.summary$cp,xlab="Number of Vars",ylab="Cp",type='1')
which.min(reg.summary$cp)
points(10,reg.summary$cp[10],col="red",cex=2,pch=20)
which.min(reg.summary$bic)
plot(reg.summary$bic[6],col="red",cex=2,pch=20)

plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")
coef(regfit.full,6)

#fwd and bckwd stepwise selection

regfit.fwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
summary(regfit.fwd)
regfit.bwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="backward")
summary(regfit.bwd)

coef(regfit.full,7)
coef(regfit.fwd,7)
coef(regfit.bwd,7)

#choosing mmodel using validation set and cross-validation
set.seed(1)
train=sample(c(TRUE,FALSE),nrow(Hitters),rep=TRUE)
?sample
test=(!train)
#best subset selection
regfit.best=regsubsets(Salary~.,data=Hitters[train,],nvmax=19)
?regsubsets
test.mat=model.matrix(Salary~.,data=Hitters[test,])
#model.matrix() builds an 'X' matrix from data. 
head(test.mat)
#run a loop 
#for each size i, extract coeffs from regfit.best for best model of that size
#multiply them into appropriate clumns of test model matrix to form predictions
#compute test MSE
val.errors=rep(NA,19)
?rep()
for(i in 1:19){
  coefi=coef(regfit.best,id=i)
  #multiply x values by model coefficients to obtain prediction
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((Hitters$Salary[test]-pred)^2)
}


