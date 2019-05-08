library(tidyverse)
library(lubridate)
library(RODBC)
library(stringr)
library(reshape2)

##Date Range of Fatalities, check output. 
YearRange<-c(2009,2018)

#State Fatality Rate. 
STATE.f<-13.5

#Combine spreadsheets from NFIRS indicating incidents with Fatalities.
#This will allow us to run 10 year statistics because FFDB only goes back to ~2010
#o.f. = Old Fatalities
o.f.files<-list.files('C:/GIS/CRR/CRR_2018/Data/old.fatalities',full.names = T)

o.f.list<-list()

for(i in o.f.files){
  index<-match(i,o.f.files)
  o.f.list[[index]]<-read_csv(i, col_types = cols_only(
                                        `Alarm Date / Time` = "c",
                                        `Incident Number` = "c",
                                        `Fire Department ID`  = "c",
                                        Exposure = "n",
                                        `Alarm Date - Year` = "n",
                                        `Aid Given or Received Description` = "c",
                                        `Non Fire Service Fatalities` = "n",
                                        `Property Use Code (National)` = "c",
                                        `Incident Type Code (National)` = "c",
                                        `New Cause Description` = "c"))
}


#Binds the lists together.  
o.f.df<-bind_rows(o.f.list)

#Remove duplicate rows that arise due to persons involved. 
o.f.df<-o.f.df%>%unique()


o.f.df$`Alarm Date / Time`<-ymd_hms(o.f.df$`Alarm Date / Time`)

#Removes the cause code (i.e. 13) and leaves just the description. 
o.f.df$`New Cause Description`<-substr(o.f.df$`New Cause Description`,7,nchar(o.f.df$`New Cause Description`))


#For right now, we are only using the year 2009. 
o.f.df<-o.f.df%>%filter(!`Aid Given or Received Description` %in% c("Automatic aid given",
                                                           "Mutual aid given",
                                                           "Other aid given"),
                        `Incident Type Code (National)` >=111,
                        `Incident Type Code (National)` <=123,
                        `Alarm Date - Year` == 2009,
                        !`New Cause Description` %in% c('Intentional','Investigation with Arson Mod.'))


#Connect to access database containing new fire fatalities. 
channel<-odbcConnectAccess2007("C:/GIS/CRR/CRR_2018/Data/Fire Fatality.accdb")


#n.f = new fatalities
#Uses date range established by YearRange variable at top of sheet. 
n.f.df <- sqlQuery( channel , paste ("SELECT *
FROM Fires LEFT JOIN Causes ON Fires.Cause = Causes.ID 
WHERE (year(FireDate) > ",YearRange[1]," AND year(FireDate) <= ",YearRange[2],")
AND Causes.Cause <> \'Intentional\'"))

odbcClose(channel)

#tidy up columns from access db for unioning 
n.f.df$FDID<-as.character(n.f.df$FDID)
n.f.df$FDID<-str_pad(n.f.df$FDID,5,pad = "0")
n.f.df$Year<-year(n.f.df$FireDate)
n.f.df$Cause.1<-as.character(n.f.df$Cause.1)

#ss = subset
n.f.df.ss<-n.f.df%>%select(FireDate, FDID, Fatalities, "Cause" = Cause.1, Year)
o.f.df.ss<-o.f.df%>%select(`Alarm Date / Time`, `Fire Department ID`, `Non Fire Service Fatalities`, `New Cause Description`, `Alarm Date - Year`)

#use column names of n.f and apply to o.f
names(o.f.df.ss)<-names(n.f.df.ss)

#unioning it all together.
f.df<-rbind(o.f.df.ss,n.f.df.ss)


#fdstats.RDS originates from readcensus.R
RISKDEMO<-readRDS('C:/GIS/CRR/CRR_2018/Data/fdstats.RDS')

#Total fatalities by FDID
f.summary<-f.df%>%group_by(FDID)%>%summarise(Total_Fatalities=sum(Fatalities))

#Combine Demographic Info with Fatality Information. 
RISKDEMO<-left_join(RISKDEMO, f.summary, by=c('FDID'='FDID'))
#Replace NAs with 0s
RISKDEMO[is.na(RISKDEMO$Total_Fatalities),"Total_Fatalities"]<-0
#Determine Deaths per MM per Year (multiplying by 100000 instead of 1MM because we are using 10 years.)
RISKDEMO$FDRATE.f<-(RISKDEMO$Total_Fatalities/RISKDEMO$pop2016)*1e+05

#State fatality rate (STATE.f) is hardcoded at top of this file.

#Converts fatality rate to the portion of a year one can expect to pass for one fatality to occure. 

RISKDEMO<-RISKDEMO%>%mutate(YR_PORTION.ST = 1/((STATE.f/1e+06)*pop2016))%>%
  
#Converts the portion into a string indicating years and months
  mutate(TNDeathRate = case_when(
  floor(YR_PORTION.ST)==0 ~ 
    paste(round((YR_PORTION.ST*12)%%12,0), "month(s)"),
  TRUE~
    paste(floor(YR_PORTION.ST), "year(s)",round((YR_PORTION.ST*12)%%12,0), "month(s)")
)
)


#Same as above, but instead of frequency based on state fatality rate, it's frequency based on fire department fatality rate.
RISKDEMO<-RISKDEMO%>%mutate(YR_PORTION.FD = 1/((FDRATE.f/1e+06)*pop2016))%>%
  mutate(FDDeathRate = case_when(FDRATE.f == 0 ~ '',
                                 floor(YR_PORTION.FD)==0 ~ paste(round((YR_PORTION.FD*12)%%12,0), "month(s)"),
                                 TRUE~ paste(floor(YR_PORTION.FD), "year(s)",round((YR_PORTION.FD*12)%%12,0), "month(s)")
  )
)


f.melt<-melt(f.df,id.vars = c("Year","FDID"), measure.vars = "Fatalities")

f.cast<-dcast(f.melt, FDID ~ Year, sum)

f.max<-f.df%>%arrange(desc(FireDate))%>%
  group_by(FDID)%>%
  mutate(rank = row_number())%>%
  filter(rank == 1)%>%
  mutate(FireDate = as.Date(FireDate))%>%
  select(FDID, FireDate, Cause, Fatalities)

RISKDEMO<-left_join(RISKDEMO, f.cast, by = c('FDID' = 'FDID'))%>%
  left_join(.,f.max, by = c("FDID" = "FDID"))

saveRDS(RISKDEMO, './Data/RISKDEMOFAT.RDS')














