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
cv<-readRDS('./Data/cv.RDS')


names<-readRDS('./Data/names.RDS')

#Shapes were copied from H Drive. 
TN_SHAPE_DIR<-"C:/GIS/CRR/CRR_2018/Graphics/Tennessee3.png"
TN_PROP_DIR<-"C:/GIS/CRR/CRR_2018/Graphics/ProportionImage.png"


source('./WaffleProp.R')
RISKDEMO<-readRDS('./data/fdstats.RDS')
RISKDEMO.st<-RISKDEMO%>%summarise(HighRisk=sum(HighRisk), MedRisk = sum(MedRisk), LowRisk = sum(LowRisk))%>%
  mutate(Total = sum(HighRisk+MedRisk+LowRisk))%>%
  mutate(HighRisk.perc = HighRisk/Total,
         MedRisk.perc = MedRisk/Total,
         LowRisk.perc = LowRisk/Total)

st.prop<-RiskBChart(RISKDEMO.st)

p_years<-c(2012:2017)
p_fdid<-c('79553','19532')

for(i in p_fdid){

####Call Volume Information####
###***Note that Call Volume is being reworked into FireRates.R***###
cv_f<-cv%>%
  filter(year %in% p_years & 
           FDID == i &
           GSM_FLAG == 0
           )

cv_t1<-table(cv_f$IN_TYP_DESC, cv_f$year)%>%
  as.data.frame.matrix()%>%
  rownames_to_column('Incident Category')%>% 
  mutate(Total = rowSums(.[,-1]))%>%
  mutate(Percent = percent(Total/sum(Total)))


cv_t2<-table(cv_f$IN_TYP_DESC2, cv_f$year)%>%
  as.data.frame.matrix()%>%
  rownames_to_column('Incident Category')%>%
  mutate(Total = rowSums(.[,-1]))%>%
  mutate(Percent = percent(Total/sum(Total)))

####Waffle Charts & Proportion Bar

fdrisk<-RISKDEMO%>%filter(FDID == i)
fdrisk.prop<-RiskBChart(fdrisk)
fdrisk.waffle<-RiskWaffle(fdrisk)





#test - remove for final
FD_NAME<-names%>%filter(FDID == i)%>%select(FDNAME)%>%as.vector()
FD_NAME.file<-str_replace_all(FD_NAME,' ','_')

rmarkdown::render('./report/report.rmd','pdf_document',paste0(i,'-',FD_NAME.file,'2018.pdf'), './Output')
}



data("mtcars")
t<-ggplot(data = mtcars, aes(x=mpg, y = cyl))+geom_point()
rmarkdown::render('./report/report.rmd','pdf_document',paste0(i,'-',FD_NAME.file,'2018.pdf'), './Output')

