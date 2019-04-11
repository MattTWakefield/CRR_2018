library(tidyverse)
library(scales)
library(reshape2)


cv<-readRDS('./Data/cv.RDS')
cv<-cv%>%filter(GSM_FLAG == 0, IN_TYP_DESC != 'OTHER')

names<-readRDS('./Data/names.RDS')

#Shapes were copied from H Drive. 
TN_SHAPE_DIR<-"C:/GIS/CRR/CRR_2018/Graphics/Tennessee3.png"
TN_PROP_DIR<-"C:/GIS/CRR/CRR_2018/Graphics/ProportionImage.png"

#Gather unique FDIDs and incident types and cross join them to serve as left join later on.
inyear<-c(2014:2018)
FDID<-unique(cv$FDID)
IN_TYP_DESC<-unique(cv$IN_TYP_DESC)
IN_TYP_DESC<-crossing(IN_TYP_DESC, FDID, inyear)
IN_TYP_DESC2<-unique(cv$IN_TYP_DESC2)
IN_TYP_DESC2<-crossing(IN_TYP_DESC2, FDID, inyear)


####Call Volume Categories####
t1<-cv%>%filter(year %in% inyear)%>%
  group_by(FDID, year, IN_TYP_DESC)%>%
  summarise(Incident_Count = n())%>%
  mutate(percent = percent(Incident_Count/sum(Incident_Count)))%>%
  left_join(IN_TYP_DESC, ., by = c("FDID" = "FDID","IN_TYP_DESC" = "IN_TYP_DESC", "inyear" = "year"))%>%
  group_by(FDID, IN_TYP_DESC, inyear)%>%
  mutate(Incident_Count = replace_na(Incident_Count, 0), percent=replace_na(percent,"No Incidents"))

t1.melt<-melt(t1, id = c("IN_TYP_DESC","FDID","inyear"))
t1.cast<-dcast(t1.melt, IN_TYP_DESC + FDID ~ inyear + variable, value.var = "value")

###Fire Categories###

t2<-cv%>%filter(year %in% inyear, IN_TYP_DESC2 %in% c('StructureFires','VegetationFires','VehicleFires'))%>%
  group_by(FDID, year, IN_TYP_DESC2)%>%
  summarise(Incident_Count = n())%>%
  mutate(percent = percent(Incident_Count/sum(Incident_Count)))%>%
  left_join(IN_TYP_DESC2[IN_TYP_DESC2$IN_TYP_DESC2 %in% c('StructureFires','VegetationFires','VehicleFires'),], ., by = c("FDID" = "FDID","IN_TYP_DESC2" = "IN_TYP_DESC2", "inyear" = "year"))%>%
  group_by(FDID, IN_TYP_DESC2, inyear)%>%
  mutate(Incident_Count = replace_na(Incident_Count, 0), percent=replace_na(percent,"No Incidents"))


t2.melt<-melt(t2, id = c("IN_TYP_DESC2","FDID","inyear"))
t2.cast<-dcast(t2.melt, IN_TYP_DESC2 + FDID ~ inyear + variable, value.var = "value")

####The rest####



t3<-cv%>%filter(year %in% inyear, !IN_TYP_DESC2 %in% c('StructureFires','VegetationFires','VehicleFires','OTHER'))%>%
  group_by(FDID, year, IN_TYP_DESC2)%>%
  summarise(Incident_Count = n())%>%
  mutate(percent = percent(Incident_Count/sum(Incident_Count)))%>%
  left_join(IN_TYP_DESC2[!IN_TYP_DESC2$IN_TYP_DESC2 %in% c('StructureFires','VegetationFires','VehicleFires','OTHER'),], ., by = c("FDID" = "FDID","IN_TYP_DESC2" = "IN_TYP_DESC2", "inyear" = "year"))%>%
  group_by(FDID, IN_TYP_DESC2, inyear)%>%
  mutate(Incident_Count = replace_na(Incident_Count, 0), percent=replace_na(percent,"No Incidents"))


t3.melt<-melt(t3, id = c("IN_TYP_DESC2","FDID","inyear"))
t3.cast<-dcast(t3.melt, IN_TYP_DESC2 + FDID ~ inyear + variable, value.var = "value")


saveRDS(t1.cast, './data/t1.RDS')
saveRDS(t2.cast, './data/t2.RDS')
saveRDS(t3.cast, './data/t3.RDS')


####STructure Fires####

sfv<-cv%>%
  filter(IN_TYP_DESC2 == "StructureFires", !New_Cause_Description %in% c('Intentional','Investigation with Arson Mod.'), MUTFLAG == 0, GSM_FLAG == 0)%>%
  group_by(FDID, New_Cause_Description, year)%>%
  summarise(Incident_Count = n(), Total_Loss=sum(Total_Loss, na.rm = TRUE))


#Loss and Count by year

Total.sf<-sfv%>%
  group_by(FDID)%>%
  summarise(Total_SF_Yr = sum(Incident_Count)/5, Total_Loss_SF_Yr = sum(Total_Loss,rm.na = T)/5,
            Unknown_perc = percent(sum(Incident_Count[New_Cause_Description == "Unknown"])/sum(Incident_Count)))

Total.sf.state<-sfv%>%
  ungroup()%>%
  summarise(Total_SF_Yr = sum(Incident_Count)/5, Total_Loss_SF_Yr = sum(Total_Loss,rm.na = T)/5,
            Unknown_perc = percent(sum(Incident_Count[New_Cause_Description == "Unknown"])/sum(Incident_Count)))


sfv<-sfv%>%filter(New_Cause_Description!="Unknown")

####Top 5####

sf.top5.count<-sfv%>%
  group_by(FDID, New_Cause_Description)%>%
  summarise(Incident_Count = sum(Incident_Count))%>%
  arrange(desc(Incident_Count))%>%
  mutate(rn = row_number())%>%
  filter(rn <=5)%>%
  select(-rn)

#Adding State
sf.top5.count.state<-sfv%>%group_by(New_Cause_Description)%>%
  summarise(Incident_Count = sum(Incident_Count))%>%
  arrange(desc(Incident_Count))%>%
  mutate(rn = row_number())%>%
  filter(rn <= 5)%>%
  select(-rn)

#Loss by Cuase
sf.top5.loss<-sfv%>%
  group_by(FDID, New_Cause_Description)%>%
  summarise(Total_Loss = sum(Total_Loss))%>%
  arrange(desc(Total_Loss))%>%
  mutate(rn = row_number())%>%
  filter(rn <=5)%>%
  select(-rn)

#Adding State
sf.top5.loss.state<-sfv%>%group_by(New_Cause_Description)%>%
  summarise(Total_Loss = sum(Total_Loss))%>%
  arrange(desc(Total_Loss))%>%
  mutate(rn = row_number())%>%
  filter(rn <= 5)%>%
  select(-rn)

#saving data

saveRDS(sfv, './data/sfv.RDS')
saveRDS(sf.top5.count, './data/sf.top5.count.RDS')
saveRDS(sf.top5.count.state, './data/sf.top5.count.state.RDS')
saveRDS(sf.top5.loss, './data/sf.top5.loss.RDS')
saveRDS(sf.top5.loss.state, './data/sf.top5.loss.state.RDS')


rdf<-readRDS('./data/RISKDEMOFAT.RDS')

rdf<-left_join(rdf, Total.sf, by = c("FDID"="FDID"))

rdf<-rdf%>%
  mutate(Structure_Fires_Per_1000=(Total_SF_Yr/pop2016)*1000,Structure_Fire_Loss_Per_1000=(Total_Loss_SF_Yr/pop2016)*1000)



####combine state data. ####
#Much of this logic is replicated from readcensus.R
state.demo<-rdf%>%summarise(pop2016=sum(pop2016),
                homes2016 = sum(homes2016),
                over25_2016 = sum(over25_2016),
                occ_homes_2016 = sum(occ_homes_2016),
                ag_over65_2016 = sum(ag_over65_2016),
                hv_under125k_2016 = sum(hv_under125k_2016),
                hi_under45k_2016 = sum(hi_under45k_2016),
                belowbs_2016 = sum(belowbs_2016),
                ha_older1980 = sum(ha_older1980),
                HighRisk = sum(HighRisk),
                MedRisk = sum(MedRisk),
                LowRisk = sum(LowRisk))%>%
mutate(over25_2016.perc=over25_2016/pop2016,
occ_homes_2016.perc = occ_homes_2016/homes2016,
ag_over65_2016.perc = ag_over65_2016/pop2016,
hv_under125k_2016.perc = hv_under125k_2016/occ_homes_2016,
hi_under45k_2016.perc = hi_under45k_2016/homes2016,
belowbs_2016.perc = belowbs_2016/over25_2016,
HighRisk.perc = HighRisk/pop2016,
MedRisk.perc = MedRisk/pop2016,
LowRisk.perc = LowRisk/pop2016
)

state.fatality<-rdf%>%summarise(Total_Fatalities=sum(Total_Fatalities), 
                FDRATE.f = NA,
                YR_PORTION.ST = NA,
                TNDeathRate = NA,
                YR_PORTION.FD = NA,
                FDDeathRate = NA)


state.fatality.yoy<-rdf[,29:38]%>%apply(.,2,sum, na.rm = T)%>%as.data.frame%>%t(.)

state.demo.fat<-cbind(state.demo,state.fatality, state.fatality.yoy)

state.sf1<-cbind(data.frame(FireDate = NA, Cause = NA, Fatalities = NA),Total.sf.state)

state.demo.fat.sf<-cbind(state.demo.fat, state.sf1)


state.demo.fat.sf<-state.demo.fat.sf%>%
             mutate(Structure_Fires_Per_1000 = (Total_SF_Yr/ pop2016)*1000,
             Structure_Fire_Loss_Per_1000 = (Total_Loss_SF_Yr / pop2016)*1000)

state.summary<-cbind(data.frame(FDID = "STATE"), state.demo.fat.sf)

rdf<-rbind(rdf, state.summary)

rt<-readRDS('./Data/ResponseTimes.RDS')

rdf<-left_join(rdf, rt, by = c("FDID"="FDID"))

saveRDS(rdf,'./data/RISKDEMOFATSF.RDS') 
             
             



#Structure Fire Rate
#Top Five Causes & Quantity of fires. 
#Top Five Causes & Cost of fires. 

#Structure Fires Per 1000
############TESTING2#############




