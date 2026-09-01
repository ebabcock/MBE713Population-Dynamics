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

## Humans example. 
library(tidyverse)
humans<-read_csv("HumanPopulation.csv") %>%
  filter(Year <1900)   
ggplot(humans,aes(x=Year,y=Population))+
  geom_point()

ggplot(humans,aes(x=Year,y=log(Population)))+
  geom_point()+
  stat_smooth(method="lm")

# do linear regression
humans.lm<-lm(log(Population)~Year,data=humans)
summary(humans.lm)
coef(humans.lm)
#y = intercept + slope* x
#y=log(N)
#x=t
#slope=rm
#log(N0)=intercept
#No=exp(intercept)
rm<-coef(humans.lm)[2]
rm
N0<-exp(coef(humans.lm)[1])
N0
# plot
ggplot(humans,aes(x=Year))+
  geom_point(aes(y=Population))+
  geom_function(fun=~N0*exp(rm*.x)) 
# lambda
lambda=exp(rm)
lambda

# Linear regression estimation
ggplot(mpg,aes(x=hwy,y=cty))+
  geom_point()+
  stat_smooth(method="lm")+
  labs(x="Highway miles per gallon",y="City miles per gallon",
       title="Fuel efficiency regression")
cars.lm<-lm(cty~hwy,data=mpg)
coef(cars.lm)

## least squares function

RegressionSS<-function(par,x,y) {
  a<-par["a"]
  b<-par["b"]
  y_hat<-a+b*x
  SS<-sum((y-y_hat)^2)
  return(SS)
}
RegressionSS(par=c(a=0.84,b=0.68),
             x=mpg$cty,
             y=mpg$hwy)
# find minimum
fit<-optim(par=c(a=0,b=1),
           fn=RegressionSS,
           x=mpg$hwy,
           y=mpg$cty)
fit

##




