####Package Load and Establish Connection####
library(odbc)
library(DBI)
library(tidyverse)
library(RODBC)
library(lubridate)
library(reshape2)
library(stringr)
library(getPass)

pwd=getPass::getPass()

dbhandle <- DBI::dbConnect(odbc::odbc(), dsn = "STS GIS", uid = "FIRE_MARSHALL_ADMIN", 
                           pwd = pwd)


####Call Volume (limited # rows pulled)####

start<-Sys.time()

cv<-dbGetQuery(dbhandle, 
                  "SELECT  FDID,
                            Exposure,
                            Incident_Type_Code__National_,
                            Arrival_Date___Time,
                            Incident_Type_Description,
                            IN_TYP_DESC,
                            IN_TYP_DESC2,
                            MUTFLAG,
                            GSM_FLAG,
                            Alarm_Date___Time,
                            Controlled_Date___Time,
                            Property_Use_Code__National_
                   FROM GA_NFIRS
                    ")
end<-Sys.time()

end-start
#took about 1.3 hrs over vpn. 
cv$Alarm_Date___Time<-ymd_hms(cv$Alarm_Date___Time)
cv$Controlled_Date___Time<-ymd_hms(cv$Controlled_Date___Time)
cv$Arrival_Date___Time<-ymd_hms(cv$Arrival_Date___Time)
cv$year<-year(cv$Alarm_Date___Time)

cv$FDID<-str_pad(cv$FDID, width = 5, pad = "0")

saveRDS(cv,'./Data/cv.RDS')

####FD Names####

start<-Sys.time()

names<-dbGetQuery(dbhandle, 
               "SELECT FDID, FDNAME
                  FROM FR_SuppData
               ")
end<-Sys.time()

end-start

saveRDS(names,'./Data/names.RDS')

cv<-left_join(cv, names, by = c('FDID' = 'FDID'))


