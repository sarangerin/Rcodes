#Regression I (base codes)

rm(list=ls())
library(MASS)
library(car)
#1
#------------
a<-matrix(c(0,1,2,3,4,1,4,3,8,9),2,5,byrow=T)
a 

ta<-t(a)
ta
one<-rep(1,5)
one
z<-cbind(one,a[1,])
z
y<-a[2,]
y

b<-solve(t(z)%*%z)%*%t(z)%*%y
b

yhat<-z%*%b
yhat
resid<-y-yhat
resid
t(resid)%*%resid


#-----------
#2
ymean<-mean(y)
SST<-sum((y-ymean)^2)
SST

SSR<-sum((yhat-ymean)^2)
SSR
SSE<-sum(resid^2)
SSE
Rsquare<-SSR/SST
Rsquare

#adjusted rsquare
n73<-nrow(t(a))
n73
r73<-1 # this is the number of variables
Arsquare<-1-(1-Rsquare)*(n73-1)/(n73-r73-1)
Arsquare


#3

z1<-a[1,]
z1
reg73<-lm(y~z1)
summary(reg73)
plot(reg73)
confint(reg73)

#3-2
table71<-read.table(file.choose())
one<-rep(1,20)
one
z71<-as.matrix(cbind(one,table71[,1:2]))
View(z71)
y71<-table71[,3]
b71<-solve(t(z71)%*%z71)%*%t(z71)%*%y71
b71



b<-solve(t(z)%*%z)%*%t(z)%*%y

S73 <- solve(t(z)%*%z)
S73
SSE
sigsq<-SSE/(n73-r73-1)
sigsq
varb73<- sigsq*S73 
varb73
sterror73<-sqrt(varb73)
sterror73

#not lower tail is false becasue we want the "ends" of the distribution. 
qf(2/.4472, r73+1, n73-r73-1,lower.tail=F)
contint73<-c(0,0)

contint73[1]<-b[1]-sqrt(varb73[1,1])*sqrt((r73+1)*qf(.05, r73+1, n73-r73-1,lower.tail=F))
contint73[2]<-b[1]+sqrt(varb73[1,1])*sqrt((r73+1)*qf(.05, r73+1, n73-r73-1,lower.tail=F))
contint73
#practice with lowertail=T
contint73p<-c(0,0)
contint73p[1]<-b[1]-sqrt(varb73[1,1])*sqrt((r73+1)*qf(.05, r73+1, n73-r73-1,lower.tail=T))
contint73p[2]<-b[1]+sqrt(varb73[1,1])*sqrt((r73+1)*qf(.05, r73+1, n73-r73-1,lower.tail=T))
contint73p

#


contint73b<-c(0,0)

contint73b[1]<-b[2]-sqrt(varb73[2,2])*sqrt((r73+1)*qf(.05, r73+1, n73-r73-1,lower.tail=F))
contint73b[2]<-b[2]+sqrt(varb73[2,2])*sqrt((r73+1)*qf(.05, r73+1, n73-r73-1,lower.tail=F))

contint73b

#confidence interval with t-statistic 
#dividing alpha by .05 because we want 0.025 in each tail

contint73t<-c(0,0)
contint73t[1]<-b[1]-sqrt(varb73[1,1])*qt(.05/2, n73-r73-1,lower.tail=F)
contint73t[2]<-b[1]+sqrt(varb73[1,1])*qt(.05/2, n73-r73-1,lower.tail=F)
contint73t

contint73tb<-c(0,0)
contint73tb[1]<-b[2]-sqrt(varb73[2,2])*qt(.05/2, n73-r73-1,lower.tail=F)
contint73tb[2]<-b[2]+sqrt(varb73[2,2])*qt(.05/2, n73-r73-1,lower.tail=F)
contint73tb

