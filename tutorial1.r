library(tidyverse)
summary(mpg)
getwd()
write.csv(mpg,"mpg.csv")
mpg2<-read.csv("mpg.csv")
mpg2
ggplot(mpg,aes(y=manufacturer))+geom_bar()
