library(tidyverse)

cv<-readRDS('./Data/cv.RDS')

cv$ResponseTime<-cv$Arrival_Date___Time - cv$Alarm_Date___Time

cv$ResponseTime<-as.integer(cv$ResponseTime)

cv<-cv%>%mutate("Sub5"=case_when(
  ResponseTime<=320~1,
  TRUE~0
),
"Sub9"=case_when(
  ResponseTime<=540~1,
  TRUE~0
),
"Sub10"=case_when(
  ResponseTime<=600~1,
  TRUE~0
),
"Sub14"=case_when(
  ResponseTime<=840~1,
  TRUE~0
)
)




rt<-cv%>%filter(MUTFLAG==0, IN_TYP_DESC == 'Fires', !is.na(Arrival_Date___Time) | !is.na(Alarm_Date___Time),ResponseTime > 0)%>%
  group_by(FDID)%>%summarise(Fires=n(),
                                         "Average Response Time"=round(mean((ResponseTime)/60, na.rm = TRUE),2),
                                         "14 Minutes or Less"=round((sum(Sub14)/n())*100,2),
                                         "10 Minutes or Less"=round((sum(Sub10)/n())*100,2),
                                         "9 Minutes or Less"=round((sum(Sub9)/n())*100,2),
                                         #changed from 4 minutes
                                         "5 Min 20 sec or Less"=round((sum(Sub5)/n())*100,2))

saveRDS(rt,'./Data/ResponseTimes.RDS')