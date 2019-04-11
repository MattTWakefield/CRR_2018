library(tidyverse)
library(reshape2)
library(ggplot2)
library(rmarkdown)
library(knitr)
library(waffle)
library(extrafont)
#for formatting percents
library(scales)

#Define colors, note that these colors are also defined in the .rmd file. 
myred<-'#D22630'
myblue<-'#002D72'
mygrey<-'#75787B'

#cv.rds and names.RDS is generated via datapull.R
#cv<-readRDS('./Data/cv.RDS')


names<-readRDS('./Data/names.RDS')

#Shapes were copied from H Drive. 
TN_SHAPE_DIR<-"C:/GIS/CRR/CRR_2018/Graphics/Tennessee3.png"
TN_PROP_DIR<-"C:/GIS/CRR/CRR_2018/Graphics/ProportionImage.png"


source('./WaffleProp.R')

RISKDEMO<-readRDS('./data/RISKDEMOFATSF.RDS')
RISKDEMO.st<-RISKDEMO%>%filter(FDID == 'STATE')
RISKDEMO<-RISKDEMO%>%filter(FDID != 'STATE')
st.prop<-RiskBChart(RISKDEMO.st)
t1<-readRDS('./data/t1.RDS')
t2<-readRDS('./data/t2.RDS')
t3<-readRDS('./data/t3.RDS')
sfv<-readRDS('./data/sfv.RDS')
sf.top5.count<-readRDS('./data/sf.top5.count.RDS')
sf.top5.count.state<-readRDS('./data/sf.top5.count.state.RDS')
sf.top5.loss<-readRDS('./data/sf.top5.loss.RDS')
sf.top5.loss.state<-readRDS('./data/sf.top5.loss.state.RDS')


p_years<-c(2014:2018)
p_fdid<-c('19532')

for(i in p_fdid){

####Call Volume Information####

t1.f<-t1%>%filter(FDID == i)%>%select(-FDID)
t2.f<-t2%>%filter(FDID == i)%>%select(-FDID)
t3.f<-t3%>%filter(FDID == i)%>%select(-FDID)

colnames(t1.f)<-c("Incident Type", rep(c("Count","Perc"),5))
colnames(t2.f)<-c("Incident Type", rep(c("Count","Perc"),5))
colnames(t3.f)<-c("Incident Type", rep(c("Count","Perc"),5))


####Waffle Charts & Proportion Bar

fdrisk<-RISKDEMO%>%filter(FDID == i)
fdrisk.prop<-RiskBChart(fdrisk)
fdrisk.waffle<-RiskWaffle(fdrisk)

#test - remove for final
FD_NAME<-names%>%filter(FDID == i)%>%select(FDNAME)%>%as.vector()
FD_NAME.file<-str_replace_all(FD_NAME,' ','_')

rmarkdown::render('./report/report.rmd','pdf_document',paste0(i,'-',FD_NAME.file,'2018.pdf'), './Output')

}


