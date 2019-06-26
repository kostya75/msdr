library(ggplot2)
library(dplyr)

geom_timeline(xmin, xmax,color,size alpha)

geom_stat<-
  data_df2%>%
  filter(between(YEAR, 2000, 2015),COUNTRY %in% c("USA","CHINA"))%>%
  select(datetime,YEAR,EQ_PRIMARY,DEATHS,COUNTRY)%>%
  mutate(DEATHS=convert_na_n(DEATHS,0),c2=ifelse(COUNTRY=="USA",30,60))



ggplot(geom_stat)+geom_segment(aes(x=min(geom_stat$datetime),xend=max(geom_stat$datetime),y=30,yend=30))+
  geom_point(aes(y=30,x=datetime,size=EQ_PRIMARY,color=DEATHS),alpha=1/5)+scale_size_area(max_size=10)


ggplot(geom_stat)+geom_hline(aes(yintercept=30))+geom_hline(aes(yintercept=60))+
  geom_point(aes(y=c2,x=datetime,size=EQ_PRIMARY,color=DEATHS),alpha=1/5)+scale_size_area(max_size=2)+ylim(c(0,90))
