library(tidyverse)
bruv<-read.csv("bruv.csv")
theme_set(theme_minimal())
ggplot(bruv,aes(x=Zone,fill=Sharks))+
  geom_bar()+
  scale_fill_manual(values=c("blue","pink"))+
  labs(x="",y="Number of BRUV samples",fill="Sharks present?")+
  theme(legend.position="top")
# I edited this on the website 
# The revised data show sharks were present more often at not fished sites. 
