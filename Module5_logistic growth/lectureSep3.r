library(tidyverse)

swordfish<-read.csv("swordfish.csv")
view(swordfish)


# B[t+1]=B[t]+B[t]*rm*(1-B[t]/K)-c[t]
K<-1000
rm<-0.5
YieldCurve<-data.frame(B=seq(0,K,10))
YieldCurve$Surplus_Production<-YieldCurve$B*rm*(1-YieldCurve$B/K)
ggplot(YieldCurve,aes(x=B,y=Surplus_Production))+
  geom_line()

# Fit with swordfish

B0<-10000
r<-0.7
K<-1000000
swordfish$B<-NA
swordfish$B[1]<-B0
for(t in 2:nrow(swordfish)) {
  swordfish$B[t]=swordfish$B[t-1]+
    swordfish$B[t-1]*r*(1-swordfish$B[t-1]/K)-
    swordfish$Catch[t-1]
}

theta<-c(logK=log(1000000),
         r=0.7,
         sigma=1,
         q=0.0001)

getNegLogLike<-function(theta,data) {
  B<-NULL
  LL<-NULL
  logpredI<-NULL
  K<-exp(theta["logK"])
  B[1]<-K  #B0=K
  for(y in 2:nrow(data)) {
    B[y]<-max(c(B[y-1]+theta["r"]*B[y-1]*(1-B[y-1]/K)-
                  data$Catch[y-1],0.0001))
  }
  for(y in 1:nrow(data)) {
    logpredI[y]<-log(B[y]*theta["q"])
    LL[y]<-dlnorm(data$CPUE[y],logpredI[y],theta["sigma"],log=TRUE)
  }
  -sum(LL,na.rm=TRUE)
}
getNegLogLike(theta=c(r=0.8,
                      logK=log(70000),
                      q=0.0001,
                      sigma=0.3),
              data=swordfish)


modFit<-optim(par=c(r=0.8,logK=log(90000),q=0.001,sigma=0.3),
              fn=getNegLogLike,
              data=swordfish,
              lower=c(r=0.01,K=10,q=0.0000001,sigma=0.0001),
              upper=c(r=4,K=1000000,q=1,sigma=10),
              method="L-BFGS-B",
              control=list(trace=TRUE))
modFit

# cohort live table
cohort<-read_csv("cohortLifeTable.csv")
view(cohort)

cohort$S_M[1:9]-cohort$S_M[2:10]
                         
cohort$D_M<-cohort$S_M-c(cohort$S_M[2:10],0)
cohort$D_F<-cohort$S_F-c(cohort$S_F[2:10],0)
cohort
cohort<-cohort |> mutate(q_M=D_M/S_M,
                        q_F=D_F/S_F)

ggplot(cohort,aes(x=x)) +
  geom_line(aes(y=q_M,color="M"))+
  geom_line(aes(y=q_F,color="F"))+
  labs(x="Age",y="Mortality rate")
