
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
