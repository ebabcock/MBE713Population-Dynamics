library(tidyverse)
n<-20
heads<-13

binomialNLL<-function(p,k,n){
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

RegressionNLL<-function(par,x,y,s) {
  a<-par["a"]
  b<-par["b"]
  s<-par["s"]
  y_hat<-a+b*x
  LL<-sum(dnorm(y,mean=y_kat,sd=s))
  return(-LL)
}
fit<-optim(par=c(a=0.5,b=1,s=1),
           fn=RegressionLL,
           x=mpg$hwy,
           y=mpg$cty)




##swordfish example

rm<-0.15
K<-1000
N0<-100

Population<-data.frame(t=0:50,N=NA)
Population$N[1]<-N0
for(i in 2:nrow(Population)){
  Populatioj$N[i]<-Population$N[i-1]+
    rm*Population$N[i-1]*(1-Population)
}

