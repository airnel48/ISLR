
#Non-linear modeling

library(ISLR)
attach(Wage)

#polynomial regression and step functions
fit=lm(wage~poly(age,4),data=Wage)
coef(summary(fit))
#obtain age, age^2, age^3, age^4 directly usign raw = T
fit2=lm(wage~poly(age,4,raw=T),data=Wage)
coef(summary(fit2))
#another  way
fit2a=lm(wage~age+I(age^2)+I(age^3)+I(age^4),data=Wage)
coef(fit2a)
fit2b=lm(wage~cbind(age,age^2,age^3,age^4))
#create a grid of values for age at which we want predictions
agelims=range(age)
age.grid=seq(from=agelims[1],to=agelims[2])
preds=predict(fit,newdata=list(age=age.grid),se=TRUE)
se.bands=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)
#plot data and add fit from 4 degree polynomial
par(mfrow=c(1,2),mar=c(4.5,4.5,1,1),oma=c(0,0,4,0))
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Degree -4 Polynomial",outer=T)
lines(age.grid,preds$fit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)

#in polynomial, must decide degree of polynomial to use
#can do this with hypothesis testing
#will use ANOVA F-test

fit.1=lm(wage~age, data=Wage)
fit.2=lm(wage~poly(age,2),data=Wage)
fit.3=lm(wage~poly(age,3),data=Wage)
fit.4=lm(wage~poly(age,4),data=Wage)
fit.5=lm(wage~poly(age,5),data=Wage)
anova(fit.1,fit.2,fit.3,fit.4,fit.5)
#cubic or quartic works best

#now, predict whether an individual earns more than $250K per year
fit=glm(I(wage>250)~poly(age,4),data=Wage,family=binomial)
#I() creates a binary response where wage>250 is evaluated to be True or False. 

preds=predict(fit,newdata=list(age=age.grid),se=T)
#transform logit output from logit predictions to confidence intervals
pfit=exp(preds$fit)/(1+exp(preds$fit))
se.bands.logit=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)
se.bands=exp(se.bands.logit)/(1+exp(se.bands.logit))

plot(age,I(wage>250),xlim=agelims,type="n",ylim=c(0,.2))
points(jitter(age),I((wage>250)/5),cex=.5,pch="|",col="darkgrey")
lines(age.grid,pfit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)

#fit the step function. Use cut() to cut data in 4 intervals
table(cut(age,4))
fit=lm(wage~cut(age,4),data=Wage)
coef(summary(fit))

#Splines
#bs() generates the entire matrix of basis fxn for splines with specified set of knots. 
#default is cubic splines

library(splines)
fit = lm(wage~bs(age,knots=c(25,40,60)),data=Wage)
pred=predict(fit,newdata=list(age=age.grid),se=T)
plot(age,wage,col="gray")
lines(age.grid,pred$fit,lwd=2)
lines(age.grid,pred$fit+2*pred$se,lty="dashed")
lines(age.grid,pred$fit-2*pred$se,lty="dashed")
#can use df to produce a spline with knots at uniform quantiles of the data
dim(bs(age,knots=c(25,40,60)))
dim(bs(age,df=6))
attr(bs(age,df=6),"knots")

#fit a natural spline with 4 degrees of freedom
fit2=lm(wage~ns(age,df=4),data=Wage)
pred2=predict(fit2,newdata=list(age=age.grid),se=T)
lines(age.grid,pred2$fit,col="red",lwd=2)

#we can use smooth.spline() to produce a smooth spline
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Smoothing spline")
fit=smooth.spline(age,wage,df=16)
fit2=smooth.spline(age,wage,cv=TRUE)
fit2$df
lines(fit,col="red",lwd=2)
lines(fit2,col="blue",lwd=2)
legend("topright",legend=c("16 DF","6.8 DF"),col=c("red","blue"),lty=1,lwd=2,cex=.8)

#perform local regression
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Local Regression")
fit=loess(wage~age,span=.2,data=Wage)
fit2=loess(wage~age,span=.5,data=Wage)
lines(age.grid,predict(fit,data.frame(age=age.grid)),col="red",lwd=2)
lines(age.grid,predict(fit2,data.frame(age=age.grid)),col="blue",lwd=2)
legend("topright",legend=c("Span=0.2","Span=0.5"),col=c("red","blue"),lty=1,lwd=2,cex=.8)

#GAMS
#predict wage using natural spline functions of year and age, treating education as qualitative predictor
#this is a large linear regression model using basis fxns
gam1=lm(wage~ns(year,4)+ns(age,5)+education,data=Wage)

#now using gam library
#s() indicates we would like to use a smoothing spline
install.packages("gam")
library(gam)
gam.m3=gam(wage~s(year,4)+s(age,5)+education,data=Wage)
par(mfrow=c(1,3))
plot(gam.m3,se=TRUE,col="blue")
plot.gam(gam1,se=TRUE,col="red")

#apply to anova to determine which model is best:
#1 a GAM that excludes year
#2 a GAM that uses a linear fxn of year
#3 a GAM that uses a spline fxn of year

gam.m1=gam(wage~s(age,5)+education,data=Wage)
gam.m2=gam(wage~year+s(age,5)+education,data=Wage)
anova(gam.m1,gam.m2,gam.m3,test="F")

summary(gam.m2)

preds=predict(gam.m2,newdata=Wage)

#use local regression fits as buildign blocks in a GAM using the lo() fxn
gam.lo=gam(wage~s(year,df=4)+lo(age,span=0.7)+education,data=Wage)
plot.gam(gam.lo,se=TRUE,col="green")

#use lo() to create intreactions before calling gam() fxn
#here the first term is an interaction between year and age
gam.lo.i=gam(wage~lo(year,age,span=0.5)+education,data=Wage)
install.packages("akima")
library(akima)
plot(gam.lo.i)

#we can fit a logisitc regression GAM with I() fxn to create binary response variable
gam.lr=gam(I(wage>250)~year+s(age,df=5)+education,family=binomial,data=Wage)
par(mfrow=c(1,3))
plot(gam.lr,se=T,col="green")
table(education,I(wage>250))

#since no high earners in <HS category, we fit logistic GAM using all but this category
gam.lr.s=gam(I(wage>250)~year+s(age,df=5)+education,family=binomial,data=Wage,subset=(education!="1. < HS Grad"))
plot(gam.lr.s,se=T,col="green")
#this produces more sensible results