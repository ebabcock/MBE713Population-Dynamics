
#Use the following simulated data.
set.seed(2)
meanVals<-rlnorm(8,log(10),0.5)
habArea<-trunc(rlnorm(8,log(200),0.5))
area<-data.frame(strata=1:8) %>%
  mutate(habArea=habArea)%>%
  uncount(habArea)%>%
  mutate(N=rpois(sum(habArea),meanVals[strata]))
ggplot(area,aes(x=strata))+
  geom_bar()

#1. Take a sample of size 50 and calculate the total numbers of animals, it's standard error, and 95% confidence interval using simple random sampling
sum(area$N)
n<-50
dataSimple<-sample(1:nrow(area),size=n,replace=FALSE)
simple<-area[dataSimple,] %>% 
  summarize(meanCount=mean(N),
            seCount=sd(N)/sqrt(n()),
            n=n(),
            N=meanCount*nrow(area),
            seN=seCount*nrow(area),
            lci=N-2*seN,
            uci=N+2*seN)
simple %>% mutate(across(where(is.numeric),round,2))


#Also take a stratified sample of the same sample size and cacluate the total numbers, se and CI. Is simple random or stratified more precies?

SamplesPerStrata<-round(n*meanVals/sum(meanVals))  #variance=mean for Poisson
SamplesPerStrata
strataSample<-NULL
for(i in 1:8) strataSample<-
  c(strataSample,sample(which(area$strata==i),
                        size=SamplesPerStrata[i],
                        replace=FALSE))
table(area$strata[strataSample])

stratified<- area[strataSample,] %>% 
  group_by(strata)%>%
  summarize(meanCount=mean(N),
            seCount=sd(N)/sqrt(n()),
            n=n()) %>%
  mutate(area=habArea,
         N=meanCount*area,
         seN=seCount*area) %>%
  ungroup()

stratified %>% mutate(across(where(is.numeric),round,2))
stratTotal<-stratified %>% summarize(N=sum(N),
                                     seN=sqrt(sum(seN^2)),
                                     lci=N-2*seN,
                                     uci=N+2*seN)
stratTotal %>% mutate(across(where(is.numeric),round,2))

bind_rows(list(simple=simple,
               stratified=stratTotal),
          .id="Method")%>%
  ggplot(aes(x=Method,y=N,ymin=lci,ymax=uci))+
  geom_point()+
  geom_errorbar(width=0.1)+
  geom_hline(yintercept=sum(area$N),color="red")



#Using the data from the shark BLL survey, calculate the mean abundance in each year 
#SPHYRNA	MOKARRAN, assuming a simple random sample

#Using the impala data with distance sampling, use AIC to find the best functional 
#form form between distance and 
https://www.multibugs.org/examples/latest/Impala.html

