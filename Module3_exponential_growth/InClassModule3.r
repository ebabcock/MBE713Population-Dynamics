library(tidyverse)
theme_set(theme_bw())
Rm<-0.25
lambda<-1+Rm
N0<-100
Population<-data.frame(t=0:9,N=NA)
Population
Population$N[1]<-N0
for(i in 2:10) {
  Population$N[i]<-Population$N[i-1]*lambda
}
View(Population)
# Same answer in one step
Population$N<-N0*lambda^Population$t
ggplot(Population, aes(x=t,y=N))+
  geom_line()
# Different values of lambda
Rm<-c(-0.25,0,0.1,0.25)
lambda<-1+Rm
lambda

Population<-expand.grid(t=0:9,
                        lambda=lambda)
Population$N<-N0*Population$lambda^Population$t

view(Population)
ggplot(Population,aes(x=t,
                      y=N,
                      color=factor(lambda)))+
  geom_line()

### 

ggplot(Population,aes(x=t,
                      y=log(N),
                      color=factor(lambda)))+
  geom_line()

# Exponential vs. continuous

Rm<-0.1
lambda<-1+Rm
lambda
# instantaneous rate rm
rm<-log(lambda)
rm
N0
Population<-data.frame(t=0:9)
Population$N<-N0*exp(rm*Population$t)
View(Population)

Rm<-exp(rm)-1

#Plot of continuous vs. discrete

N0<-100
lambda<-1.8
rm<-log(lambda)
Population<-data.frame(t=0:9)
Population$N<-N0*lambda^(Population$t)
ggplot(Population,
       aes(x=t,y=N))+
  geom_line()+
  geom_function(fun = function(x) N0*exp(rm*x), colour = "red")
# Units of rm

rm_per_day<-0.001 # per day
rm_per_year<-rm_per_day*365
rm_per_year


