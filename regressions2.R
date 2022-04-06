#Project test codes

rm(list=ls())
library(MASS)
library(car)
#1
Estate <- read.table(file.choose())     
View(Estate)
names(Estate) <- c("Size", "Value", "Price")
attach(Estate)

nsize<-length(Size)

one20<-rep(1,20)
ez71<-as.matrix(cbind(one20,Estate[,1:2]))
View(ez71)
ey71<-Estate[,3]
b71<-solve(t(ez71)%*%ez71)%*%t(ez71)%*%ey71
b71

#finding rsquare
y71hat<-ez71%*%b71
resid71<-ey71-y71hat
resid71

y71mean<-mean(ey71)
SST71<-sum((ey71-y71mean)^2)
SSR71<-sum((y71hat-y71mean)^2)
SSE71<-sum(resid71^2)
Rsquare71<-SSR71/SST71
Rsquare71

#adjusted R-square
#r is the number of predictor varibles
r71<-ncol(ez71)-1
n71<-nrow(ez71)

#adjusted r-square
arsquare71<-1-(1-Rsquare71)*(n71-1)/(n71-r71-1)
arsquare71


#Beta coefficients
#load package QuantPsyc
library(QuantPsyc)
regsize<-lm(Price~Size+Value)
summary(regsize)
plot(regsize)
Estatemat<-as.matrix(Estate)
summary(Estatemat)

#mean of residuals is 0
mean(regsize$residuals)

sd(Estatemat[,2])
sd(Estatemat[,3])
#find beta coefficient
bvalue<-sd(Estatemat[,2])/sd(Estatemat[,3])*.04518
bsize<-sd(Estatemat[,1])/sd(Estatemat[,3])*2.6344
bvalue
bsize
#beta coffficient with code
lm.beta(regsize)



#estimate prediction and confidence intervals
predict(regsize, newdata=data.frame(Size=75, Value=90), int="pred") #prediction with the error, result 7.8 pg 379
predict(regsize, newdata=data.frame(Size=75, Value=90), int="confidence")

#predction intervals are wider than confidence intervals because of the additional
#uncertainty of prediction
#interpretation: 95% of the time, the actual value of y0 given the predicted values will 
#fall within this range. 5% of the time, the actual value of y (with these parameters)
#will fall outside of the range

#finding standard error (Var B)

S71 <- solve(t(ez71)%*%ez71)
S71
sigsq71<-t(resid71)%*%resid71/(n71-r71-1)
sigsq71

varb71<- 12.0583*S71
varb71

stderr71<-sqrt(varb71)
stderr71

summary(regsize)

#getting the t-value
t71_1<-b71[1]/stderr71[1,1]
t71_2<-b71[2]/stderr71[2,2]
t71_3<-b71[3]/stderr71[3,3]
t71_1
t71_2
t71_3

#multiple by 2 for both tails
2*pt(t71_1,n71-r71-1, lower.tail=F)
2*pt(t71_2,n71-r71-1, lower.tail=F)
2*pt(t71_3,n71-r71-1, lower.tail=F)

#finding rsquare
y71hat<-ez71%*%b71
resid71<-ey71-y71hat
resid71

y71mean<-mean(ey71)
SST71<-sum((ey71-y71mean)^2)
SSR71<-sum((y71hat-y71mean)^2)
SSE71<-sum(resid71^2)
Rsquare71<-SSR71/SST71
Rsquare71

#confidence interval
t_int71mat<-matrix(rep(0,6),3,2)
View(t_int71mat)
n71<-nrow(Estate)
r71<-ncol(Estate)-1 #number of independent variables
n71
r71
tstat71<-qt(.025,n71-r71-1, lower.tail=F)
tstat71
t_int71mat[1,1]<-b71[1]-tstat71*stderr71[1,1]
t_int71mat[1,2]<-b71[1]+tstat71*stderr71[1,1]
t_int71mat[2,1]<-b71[2]-tstat71*stderr71[2,2]
t_int71mat[2,2]<-b71[2]+tstat71*stderr71[2,2]
t_int71mat[3,1]<-b71[3]-tstat71*stderr71[3,3]
t_int71mat[3,2]<-b71[3]+tstat71*stderr71[3,3]
t_int71mat

confint(regsize)

#Residual Standard Error
library(psych)#psych library has describe function
describe(Price)
serrorresid1<-sqrt(SSE71/(n71-3))
serrorresid1

y71hat<-ez71%*%b71
resid71<-ey71-y71hat
resid71

y71mean<-mean(ey71)
SST71<-sum((ey71-y71mean)^2)
SSR71<-sum((y71hat-y71mean)^2)
SSE71<-sum(resid71^2)
Rsquare71<-SSR71/SST71
Rsquare71

summary(regsize)


#--prediction
predict(regsize, newdata=data.frame(Size=15, Value=75), int="pred") #prediction with the error, result 7.8
predict(regsize, newdata=data.frame(Size=15, Value=75), int="confidence") 



#F-statistic
Fstat71<-(Rsquare71/r71)/((1-Rsquare71)/(nsize-r71-1))
Fstat71
fprob71<-1-pf(Fstat71,r71,nsize-r71-1)
fprob71

#---leverage
hat71<-ez71%*%solve(t(ez71)%*%ez71)%*%t(ez71)
View(hat71)
diaghat<-rep(0,nrow(hat71))

for (i in 1:nrow(hat71)){
    diaghat[i]<-hat71[i,i]
}
diaghat
plot(diaghat)
plot(regsize)

#-Stepwise linear regression
ozone.pollution <- read.table(file.choose(), header=T) #ozone.data.txt
View(ozone.pollution)
attach(ozone.pollution)
names(ozone.pollution)

pairs(ozone.pollution, panel=panel.smooth)

model1<-lm(ozone~temp*wind*rad+I(rad^2)+I(temp^2)+I(wind^2)) #the I() means treat "as is"
summary(model1)

model1bad<-lm(ozone~temp*wind*rad+rad^2+temp^2+wind^2)
summary(model1bad)

#---------

## Interaction term is not significant, we drop it:

model2 <- update(model1,~.-temp:wind:rad)
summary(model2)

## Another interaction term is not significant, we drop it:

model3 <- update(model2,~.-temp:rad)
summary(model3)

## Another interaction term is not significant at .05, we drop it:

model4 <- update(model3,~.-temp:wind)
summary(model4)

## Now the last interaction term is not significant and we drop it

model5 <- update(model4,~.-wind:rad)
summary(model5)

## One of the quadratic terms is not significant either

model6 <- update(model5,~.-I(rad^2))
summary(model6)

## All terms are significant, let's check residuals
plot(model6)



## Variance increases with fitted values and the normality test is not very good. We 
## transform the response variable with the logarithm

model7 <- lm(log(ozone)~temp+wind+rad+I(temp^2)+I(wind^2))
summary(model7)
## Another quadratic term is not significant, we drop it
model8 <- update(model7,~.-I(temp^2))
summary(model8)

## All coefficients are significant, we do another residual test:

par(mfrow=c(2,2))     ## show four plots at once
plot(model8)

## We have an observation with high leverage so we take it out:

model9 <- lm(log(ozone)~temp+wind+rad+I(wind^2), subset=(1:length(ozone)!=17))
summary(model9)
## Check residuals now
plot(model9)

##We now compare with the stepwise regression procedure using the Akaike information criterion (AIC):
#with AIC smaller is better. AIC balances size of residual squares with # of parameters  (pg 386 of text book)
model10 <- step(model1, direction="backward")
summary(model10)
AIC(model10)

summary(model8)
AIC(model8, k=2)

sse1 <- sum(resid(model8) ^2)
sse1

ozone.n<-nrow(ozone.pollution)

#note: The textbook ignores leading terms and use
aic8<-ozone.n + ozone.n*log(2*pi) + ozone.n * log(sse1 / ozone.n) + 2 * (5+1)
aic8
## We get a rather generous model that needs further pruning.

#other multivariate tests in R
#heplots package can display them

library(heplots)
etasq(model8)
summary(model8)
#multivariate regression
library(stats)
z1 <- 0:4
y1 <- c(1, 4, 3, 8, 9)
y2 <- c(-1, -1, 2, 3, 2)
fit <- lm(cbind(y1, y2)~z1)
summary(fit)



#
Comp <- read.table(file.choose()) 
View(Comp)
names(Comp) <- c("Orders", "ADitems", "CPUt") #
attach(Comp)

## Compute linear model with command lm()
comp.model <- lm(CPUt~Orders + ADitems) #orders and aditems are the dependent variables, CPUt is the independent variable.
summary(comp.model)
confint(comp.model)

#how to interpret? 1 unit increase of orders 1.07 unit increase of CPUt. Translation of data

## Use predict command to estimate confidence intervals 
predict(comp.model, newdata=data.frame(Orders=130, ADitems=7.5), int="pred") #prediction with the error, result 7.8
predict(comp.model, newdata=data.frame(Orders=130, ADitems=7.5), int="confidence") #result 7.7

## Compute Cp for model reduction
drop1(comp.model, scale=1.44)
