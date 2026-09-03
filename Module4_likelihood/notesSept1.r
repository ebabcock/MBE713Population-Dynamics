library(tidyverse)
n<-20
heads<-13

binomialNLL<-function(p,k,n) {
  negLL<- -dbinom(k,n,p,log=TRUE)
  return(negLL)
}
binomialNLL(p=0.5,k=heads,n=n)

fit<-optim(par=c(p=0.5),
           fn=binomialNLL,
           k=heads,
           n=n,
           method="Brent",
           lower=0,
           upper=1)
fit
binomialNLL(p=0.65,k=heads,n=n)

## mpg MLE

RegressionNLL<-function(par,x,y,s) {
  a<-par["a"]
  b<-par["b"]
  s<-par["s"]
  y_hat<-a+b*x
  LL<-sum(dnorm(y,mean=y_hat,sd=s))
  return(-LL)
}
fit<-optim(par=c(a=0.5,b=1,s=1),
           fn=RegressionNLL,
           x=mpg$hwy,
           y=mpg$cty)
fit

# With fixed sigma


RegressionNLL<-function(par,x,y) {
  a<-par["a"]
  b<-par["b"]
  s<-1
  y_hat<-a+b*x
  LL<-sum(dnorm(y,mean=y_hat,sd=s))
  return(-LL)
}
fit<-optim(par=c(a=0.5,b=1),
           fn=RegressionNLL,
           x=mpg$hwy,
           y=mpg$cty)
fit

## swordfish example

rm<-0.15
K<-1000
N0<-100

Population<-data.frame(t=0:50,N=NA)
Population$N[1]<-N0
for(i in 2:nrow(Population)){
  Population$N[i]<-Population$N[i-1]+
    rm*Population$N[i-1]*(1-Population$N[i-1]/K)
}
ggplot(Population,aes(x=t,y=N))+geom_line()


# swordfish
swordfish<-read.csv("swordfish.csv")



