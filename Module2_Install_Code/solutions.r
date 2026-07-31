library(tidyverse)
bruv<-read_csv("bruv.csv")
summary(bruv)
ggplot(bruv,aes(x=Zone,fill=Sharks))+
  geom_bar()
