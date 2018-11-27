library(tidyverse)
library(reshape2)
library(ggplot2)
library(rmarkdown)
library(knitr)



cv<-readRDS('./Data/cv.RDS')

p_years<-c(2012:2017)
p_fdid<-c('79553')

cv_f<-cv%>%
  filter(year %in% p_years & 
           FDID %in% p_fdid &
           GSM_FLAG == 0
           )

cv_t1<-table(cv$IN_TYP_DESC, cv$year)

cv_t2<-table(cv$IN_TYP_DESC2, cv$year)

rmarkdown::render('./report/report.rmd','pdf_document','test.pdf', './Output')













