library(tidyverse)
library(reshape2)
library(ggplot2)
library(rmarkdown)
library(knitr)
library(waffle)
library(extrafont)
library(rowr)
#for formatting percents
library(scales)

#Define colors, note that these colors are also defined in the .rmd file. 
myred<-'#D22630'
myblue<-'#002D72'
mygrey<-'#75787B'

#cv.rds and names.RDS is generated via datapull.R
#cv<-readRDS('./Data/cv.RDS')


#Shapes were copied from H Drive. 
TN_SHAPE_DIR<-"C:/GIS/CRR/CRR_2018/Graphics/Tennessee3.png"
TN_PROP_DIR<-"C:/GIS/CRR/CRR_2018/Graphics/ProportionImage.png"


source('./WaffleProp.R')

#Read the data in
names<-readRDS('./Data/names.RDS')
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
CRR_CAUSES<-read.csv("./data/CRR_CAUSES.csv", stringsAsFactors = F)
alarmtime.melt<-readRDS("./data/alarmtime.melt")
alarmtime.month.melt<-readRDS("./data/alarmtime.month.melt")


p_years<-c(2014:2018)
p_fdid<-c('01133','19532','79553')

for(i in p_fdid){

####Call Volume Information####

t1.f<-t1%>%filter(FDID == i)%>%dplyr::select(-FDID)
t1.f$IN_TYP_DESC<-c("Explosions", "False Alarm", "Fires","Good Intent","Hazards","Rescues & EMS","Service Calls","Severe Weather","Special Incident")

t2.f<-t2%>%filter(FDID == i)%>%dplyr::select(-FDID)
t2.f$IN_TYP_DESC2<-c("Structure Fires","Vegitation Fires","Vehicle Fires")


t3.f<-t3%>%filter(FDID == i)%>%dplyr::select(-FDID)
t3.f$IN_TYP_DESC2<-c("EMS ALS","EMS BLS","Motor Vehicle","Motor Vehicle Extraction","Rescues")



colnames(t1.f)<-c("Incident Type", rep(c("Count","Perc"),5))
colnames(t2.f)<-c("Incident Type", rep(c("Count","Perc"),5))
colnames(t3.f)<-c("Incident Type", rep(c("Count","Perc"),5))


####Waffle Charts & Proportion Bar####

fdrisk<-RISKDEMO%>%filter(FDID == i)
fdrisk.prop<-RiskBChart(fdrisk)
fdrisk.waffle<-RiskWaffle(fdrisk)
ggsave("./Graphics/tempWafflep.png",fdrisk.waffle, width = 10, height = 2, dpi = 1000)

####Comparison Chart####
comp.rownames<-c("Percentage at High Risk", "Structure Fires Per 1K","Death Rate")

State<-RISKDEMO.st%>%dplyr::select(HighRisk.perc, Structure_Fires_Per_1000)%>%
  mutate(HighRisk.perc = percent(HighRisk.perc),
         Structure_Fires_Per_1000 = round(Structure_Fires_Per_1000,2))%>%t()%>%as.vector()%>%append(fdrisk$TNDeathRate)
`Fire Department`<-fdrisk%>%dplyr::select(HighRisk.perc, Structure_Fires_Per_1000, FDDeathRate)%>%
  mutate(HighRisk.perc = percent(HighRisk.perc),
         Structure_Fires_Per_1000 = round(Structure_Fires_Per_1000,2))%>%t()%>%as.vector()
comp.df<-cbind(`Fire Department`, State)%>%as.data.frame()
rownames(comp.df)<-comp.rownames


####Top 5####

top5.fd<-sf.top5.count%>%filter(FDID == i)%>%ungroup()%>%dplyr::select(-FDID)

top5.df<-cbind.fill(1:5,top5.fd, 1:5,sf.top5.count.state, fill = '')%>%as.data.frame()

colnames(top5.df)<-c('Rank(FD)','Cause(FD)','Fiers(FD)','Rank(State)','State Cause','Fires(State)')

####Demographics####

dem1<-fdrisk%>%dplyr::select(ag_over65_2016.perc, belowbs_2016.perc, hi_under45k_2016.perc)%>%
  melt()%>%
  mutate(perc_formatted = percent(value))

dem1.st<-RISKDEMO.st%>%dplyr::select(ag_over65_2016.perc, belowbs_2016.perc, hi_under45k_2016.perc)%>%
  melt()%>%
  mutate(perc_formatted = percent(value))

dem1.df<-cbind(dem1, dem1.st)

colnames(dem1.df)<-c('var1','Fire Department Unformatted','Fire Department','var2','State Unformatted','State')
dem1.df<-dem1.df%>%dplyr::select(-var1, -var2)
dem1.df$Demographics<-c("Percentage of population > than 65"
                                              ,"Percentage of population over 25 without a Bachelor's degree or higher"
                                              ,"Percentage of households with income < than $45,000")

####Housing Demographics####
dem2<-fdrisk%>%dplyr::select(ha_older1980.perc,hv_under125k_2016.perc)%>%
  melt()%>%
  mutate(perc_formatted = percent(value))

dem2.st<-RISKDEMO.st%>%dplyr::select(ha_older1980.perc, hv_under125k_2016.perc)%>%
  melt()%>%
  mutate(perc_formatted = percent(value))

dem2.df<-cbind(dem2, dem2.st)

colnames(dem2.df)<-c('var1','Fire Department Unformatted','Fire Department','var2','State Unformatted','State')
dem2.df<-dem2.df%>%dplyr::select(-var1, -var2)
dem2.df$Demographics<-c("Percentage of homes built before 1980"
                        ,"Percentage of homes values at less than $125,000")



####Mitigation Strategies####

top5.fd.ms<-left_join(top5.fd, CRR_CAUSES, by = c("New_Cause_Description" = "Cause"))


####Fatalities####

FD_Fatalities<-melt(fdrisk[,30:39], variable.name = "Year",value.name = "Fatalities")

if(is.na(fdrisk$FireDate)==TRUE){
  Max_Fatal="Your department has not had a fire fatality in the past five years"
}else{
  Max_Fatal=paste("Your most recent fatal fire was on "
                  ,format(fdrisk$FireDate,"%m/%d/%Y"), "where ",fdrisk$Fatalities," fatalitie(s) occured. The cause for this fire was listed as ", fdrisk$Cause,".")
}

####Response Time####

RTS<-fdrisk[50:53]%>%
  melt(variable.name = "Metric", value.name = "Percentage")%>%
  mutate(Percentage = percent(Percentage/100))

RTS.avg<-paste0("Average response time: ",fdrisk$`Average Response Time`," minutes")

####Bubble Charts####


fdsf<-sfv%>%filter(FDID == '19532')
stsf<-sfv%>%group_by(year, New_Cause_Description)%>%
  summarise(Incident_Count = sum(Incident_Count),Total_Loss = sum(Total_Loss))


fdchart<-ggplot(data = fdsf, aes(x = year, y = Incident_Count))+
  geom_point(aes(size = Total_Loss, color = New_Cause_Description), alpha = .5)+
  scale_size_continuous(range=c(1, 15), labels = comma)+
  scale_color_discrete(drop = FALSE)+
  theme_pander()+
  labs(color = "Cause", size = "Total Loss", x = "Year", y = "Incident Count")+
  guides(colour = guide_legend(override.aes = list(size=5)))

stchart<-ggplot(data = stsf, aes(x = year, y = Incident_Count))+
  geom_point(aes(size = Total_Loss, color = New_Cause_Description), alpha = .5)+
  scale_size_continuous(range=c(1, 15), labels = comma)+
  scale_color_discrete(drop = FALSE)+
  theme_pander()+
  labs(color = "Cause", size = "Total Loss", x = "Year", y = "Incident Count")+
  guides(colour = guide_legend(override.aes = list(size=5)))

####Fire Charts####
####Area of Origin####
aoe<-cv%>%
  filter(IN_TYP_DESC2 == "StructureFires", !New_Cause_Description %in% c('Intentional','Investigation with Arson Mod.',''), Area_of_Origin_Description!='', MUTFLAG == 0, GSM_FLAG == 0)%>%
  group_by(FDID, Area_of_Origin_Description)%>%
  summarise(Incident_Count = n(), Total_Loss=sum(Total_Loss, na.rm = TRUE))

aoe.fdid<-aoe%>%filter(FDID == i)
aoe.fdid.cnt<-aoe.fdid%>%arrange(desc(Incident_Count))%>%filter(row_number()<=10)

aoe.fdid.loss<-aoe.fdid%>%arrange(desc(Total_Loss))%>%filter(row_number()<=10)

aoe.cnt.chrt<-ggplot(aoe.fdid.cnt, aes(x = reorder(Area_of_Origin_Description,Incident_Count), y = Incident_Count))+
  geom_bar(stat = "identity", fill = myred)+coord_flip()+labs(x = "Area of Origin",y = "Incident Count")+
  theme_pander()+
  theme(axis.text=element_text(size=8))

aoe.loss.chrt<-ggplot(aoe.fdid.loss, aes(x = reorder(Area_of_Origin_Description,Total_Loss), y = Total_Loss))+
  geom_bar(stat = "identity", fill = myred)+coord_flip()+labs(x = "Area of Origin",y = "Total Loss")+
  scale_y_continuous(label = comma)+
  theme_pander()+
  theme(axis.text=element_text(size=8))

####Heat Source Description####

hs.fdid<-hs%>%filter(FDID == i)
hs.fdid.cnt<-hs.fdid%>%arrange(desc(Incident_Count))%>%filter(row_number()<=10)

hs.fdid.loss<-hs.fdid%>%arrange(desc(Total_Loss))%>%filter(row_number()<=10)

hs.cnt.chrt<-ggplot(hs.fdid.cnt, aes(x = reorder(Heat_Source_Description,Incident_Count), y = Incident_Count))+
  geom_bar(stat = "identity", fill = myblue)+coord_flip()+labs(x = "Heat Source",y = "Incident Count")+
  theme_pander()+
  theme(axis.text=element_text(size=8))

hs.loss.chrt<-ggplot(hs.fdid.loss, aes(x = reorder(Heat_Source_Description,Total_Loss), y = Total_Loss))+
  geom_bar(stat = "identity", fill = myblue)+coord_flip()+labs(x = "Heat Source",y = "Total Loss")+
  scale_y_continuous(label = comma)+
  theme_pander()+
  theme(axis.text=element_text(size=8))


####Item First Ignited####

ig.fdid<-ig%>%filter(FDID == i)
ig.fdid.cnt<-ig.fdid%>%arrange(desc(Incident_Count))%>%filter(row_number()<=10)

ig.fdid.loss<-ig.fdid%>%arrange(desc(Total_Loss))%>%filter(row_number()<=10)

ig.cnt.chrt<-ggplot(ig.fdid.cnt, aes(x = reorder(Item_First_Ignited_Description,Incident_Count), y = Incident_Count))+
  geom_bar(stat = "identity", fill = mygrey)+coord_flip()+labs(x = "Item First Ignited",y = "Incident Count")+
  theme_pander()+
  theme(axis.text=element_text(size=8))

ig.loss.chrt<-ggplot(ig.fdid.loss, aes(x = reorder(Item_First_Ignited_Description,Total_Loss), y = Total_Loss))+
  geom_bar(stat = "identity", fill = mygrey)+coord_flip()+labs(x = "Item First Ignited",y = "Total Loss")+
  scale_y_continuous(label = comma)+
  theme_pander()+
  theme(axis.text=element_text(size=8))

FrChrts<-ggarrange(aoe.cnt.chrt, aoe.loss.chrt, hs.cnt.chrt, hs.loss.chrt, ig.cnt.chrt, ig.loss.chrt, ncol = 2, nrow = 3)

####Heat Map####
fd.at<-alarmtime.melt%>%filter(FDID == i)


fd.at.plot<-ggplot(fd.at, aes(x = value, y = weekday, fill = Calls))+
  geom_tile()+theme(axis.title.y=element_blank()) + labs(x = "Hour")+
  guides(fill=FALSE)+theme_pander()+ylab(NULL)

fd.mo.at<-alarmtime.month.melt%>%filter(FDID == i)


fd.mo.at.plot<-ggplot(fd.mo.at, aes(x = value, y = fct_rev(Month), fill = Calls))+
  geom_tile()+theme(axis.title.y=element_blank()) + labs(x = "Day of the Month")+
  guides(fill=FALSE)+
  theme_pander()+
  ylab(NULL)+
  scale_fill_gradient2(low = "darkred", high = "white", mid = "yellow", midpoint = median(fd.mo.at$Calls))



#####################


#test - remove for final
FD_NAME<-names%>%filter(FDID == i)%>%dplyr::select(FDNAME)%>%as.vector()
FD_NAME.file<-str_replace_all(FD_NAME,' ','_')

FD_RISK_VAL<-percent(fdrisk$HighRisk.perc)

rmarkdown::render('./report/report.rmd','pdf_document',paste0(i,'-',FD_NAME.file,'2018.pdf'), './Output')

}





