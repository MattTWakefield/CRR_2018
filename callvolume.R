library(tidyverse)
library(reshape2)
library(ggplot2)
library(rmarkdown)
library(knitr)



cv<-readRDS('./Data/cv.RDS')
names<-readRDS('./Data/names.RDS')

p_years<-c(2012:2017)
p_fdid<-c('79553','19532')

for(i in p_fdid){
cv_f<-cv%>%
  filter(year %in% p_years & 
           FDID == i &
           GSM_FLAG == 0
           )

cv_t1<-table(cv_f$IN_TYP_DESC, cv_f$year)

cv_t2<-table(cv_f$IN_TYP_DESC2, cv_f$year)

#test - remove for final
FD_NAME<-names%>%filter(FDID == i)%>%select(FDNAME)%>%as.vector()

#
rmarkdown::render('./report/report.rmd','pdf_document',paste0(i,' - ',FD_NAME,'2018.pdf'), './Output')
}













