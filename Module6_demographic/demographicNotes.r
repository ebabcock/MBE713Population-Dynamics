library(tidyverse)
squirrel<-read_csv("squirrel.csv")
view(squirrel)

squirrel <- squirrel |>
  mutate(lxmx=lx*mx)

R0<-with(squirrel,sum(lxmx))
R0

MGT<-with(squirrel,sum(x*lxmx)/sum(lxmx))
MGT

# plot values of rm
EulerEquation<-function(rm,data) {
  (sum(exp(-rm*data$x)*data$lx*data$mx)-1)^2
}
rvals<-data.frame(r=seq(-0.1,0.2,by=0.01))
for(i in 1:length(rvals$r)) {
  rvals$Euler[i]<-EulerEquation(rvals$r[i],data=list(x=squirrel$x,
                                                     lx=squirrel$lx,
                                                     mx=squirrel$mx))
}
ggplot(rvals,aes(x=r,y=Euler))+geom_line()

fit<-optim(par=c(rm=0.1),
           fn=EulerEquation,
           data=squirrel,
           method="L-BFGS-B")
fit
rm<-fit$par
lambda<-exp(fit$par)
lambda
rm

## leslie matrix
Pvals<-squirrel$lx[2:6]/squirrel$lx[1:5]
Pvals
Fvals<-squirrel$mx[2:6]*Pvals
Fvals
Leslie<-matrix(0,5,5)

Leslie[1,]<-Fvals
for(i in 1:4) Leslie[i+1,i]<-Pvals[i]
round(Leslie,3)


# Matrix multiplication
Nt<-matrix(1,5,1)
Nt
Nt1<- Leslie %*% Nt
Nt1

# many years
nyear<-100
N<-matrix(NA,5,nyear)
N[,1]<-rep(1,5)
N[,1:5]
for(i in 2:nyear) {
  N[,i]<-Leslie %*% N[,i-1]
}
Ntotal<-colSums(N)
Ntotal
lambda<-Ntotal[100]/Ntotal[99]
lambda
# age structure
N[,100]/sum(N[,100])

## Eigen values and vectors
eigen(Leslie)

round(Re(eigen(Leslie)$vectors[,1]/
           sum(eigen(Leslie)$vectors[,1])),3)


# Elasticity calculations

left_eigen<-function(A) {
  Re(eigen(t(A))$vectors)
}
w<-Re(eigen(Leslie)$vectors)[,1]
v<-left_eigen(Leslie)[,1]
w/sum(w)
v/v[1]
w
v

elasticityFunc<-function(A) {
  w<-Re(eigen(A)$vectors)[,1]
  v<-left_eigen(A)[,1]
  w<-w/sum(w)
  v<-v/v[1]
  lambda<-Re(eigen(A)$values)[1]
  sensitivity<-outer(v,w)/sum(v*w)
  elasticity<-(A/lambda)*sensitivity
  elasticity
}
elasticityFunc(Leslie)
