#Challenge 1

library(tidyverse)
library(randomcoloR)
covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")
View(covid_data_tbl)
ylab=c(0,2.5,5,7.5,10,12.5,15,17.5)
xlab=month.abb[1:12]
continents=unique(covid_data_tbl$continentExp)

#Group data
grouped1 <- covid_data_tbl %>%
  select(month, cases, continentExp) %>%
  
  group_by(month,continentExp) %>%
  summarize(
    cases = sum(cases),
  ) %>%
  ungroup() 
colnames(grouped1)=c("month","continents","cases")

#Add missing values
grouped2=data.frame(continents=rep(continents,12),stringsAsFactors = F)
grouped2=grouped2[order(grouped2$continents),]
grouped2=as.data.frame(cbind(grouped2,rep(1:12,length(continents))))
colnames(grouped2)=c("continents","month")
grouped2=merge.data.frame(grouped2,grouped1,all=T)
grouped2[is.na(grouped2)]=0

#Calculate Cumulative Cases
grouped3<- grouped2%>%
  select(month, cases, continents) %>%
  
  group_by(continents) %>%
  summarize(
    cumulativeCases = cumsum(cases),
  ) %>%
  select(cumulativeCases, continents) %>%
  ungroup()
#Add month column
grouped3=as.data.frame(cbind(grouped3,rep(1:12,length(continents))))
colnames(grouped3)=c("cumulativeCases","continents","month")
View(grouped3)

#Canvas
ggplot(data=grouped3,
       aes(x=month,y=cumulativeCases,col=continents),group=continents)+
  geom_line()+
  scale_x_continuous(name="Year 2020",breaks = 1:12,labels=xlab)+
  theme(axis.text.x = element_text(angle=45,hjust=1),axis.text.x.bottom =element_text(xlab))+
  scale_y_continuous(expand=c(0,0),name="Cumulative Cases",labels=paste0(ylab,"M"),breaks=10^6*ylab,limits = c(0,17.5e6))+
  scale_color_manual(values=randomColor(length(continents)))+
  labs(x="Year 2020",
       y="Cumulative Cases",
       title="COVID-19 confirmed cases worldwide\nAs of 11/02/2020, Europe had more cases than USA")+
  theme(plot.title = element_text(face="bold",hjust=0,vjust=2.12,size=7),plot.caption = element_text(face="bold.italic"))