library(tidyverse)
bruv<-read.csv("bruv.csv")
theme_set(theme_minimal())
ggplot(bruv,aes(x=Zone,fill=Sharks))+
  geom_bar()+
  scale_fill_manual(values=c("slateblue","red"))+
  labs(x="",y="Number of BRUV samples",fill="Sharks present?")+
  theme(legend.position="top")
# I edited this on the website DRESSER

# THis is Beth's edit on August 21. I'm not sure which repository to use so I did both. 