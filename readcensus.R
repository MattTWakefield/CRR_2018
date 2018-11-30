library(rgdal)
library(sf)
library(tidyverse)
library(stringr)
blocks <- readOGR(dsn = "./Data/blocksf", layer = "tabblock2010_47_pophu")
blockdf<-blocks@data

factochar<-function(x){
  i <- sapply(x, is.factor)
  x[i] <- lapply(x[i], as.character)
  return(x)
}
blockdf$BG_ID<-substr(blockdf$BLOCKID10,1,12)



bgpoly<-readOGR(dsn = './data/bg.gdb', layer = "ACS_2016_5YR_BG_47_TENNESSEE")

####population & age####
pop<-sf::st_read(dsn = './data/bg.gdb', layer = "X01_AGE_AND_SEX")
pop<-factochar(pop)%>%select(GEOID, 
                             'pop'=B01001e1,
                             B01001e20,
                             B01001e21,
                             B01001e22,
                             B01001e23,
                             B01001e24,
                             B01001e25,
                             B01001e44,
                             B01001e45,
                             B01001e46,
                             B01001e47,
                             B01001e48,
                             B01001e49)
pop$BG_ID<-substr(pop$GEOID,8,nchar(pop$GEOID))
pop$ag_over65<-rowSums(pop[,3:14])
pop<-pop[,-c(3:14)]


####housing####
housing<-sf::st_read(dsn = './data/bg.gdb', layer = "X25_HOUSING_CHARACTERISTICS")
housing<-factochar(housing)%>%select(GEOID, 
                             'homes'=B25001e1,
                             B25075e2,
                             B25075e3,
                             B25075e4,
                             B25075e5,
                             B25075e6,
                             B25075e7,
                             B25075e8,
                             B25075e9,
                             B25075e10,
                             B25075e11,
                             B25075e12,
                             B25075e13,
                             B25075e14)

housing$BG_ID<-substr(housing$GEOID,8,nchar(housing$GEOID))
housing$hv_under125k<-rowSums(housing[,3:15])
housing<-housing[,-c(3:15)]

####household income####
"X19_INCOME"

income<-sf::st_read(dsn = './data/bg.gdb', layer = "X19_INCOME")
#estimate of total not used becuase total homes is contained in B250001e1 ('homes')
income<-factochar(income)%>%select(GEOID, 
                                   B19001e2,
                                   B19001e4,
                                   B19001e5,
                                   B19001e6,
                                   B19001e7,
                                   B19001e8,
                                   B19001e9)

income$BG_ID<-substr(income$GEOID,8,nchar(income$GEOID))
income$hi_under45k<-rowSums(income[,2:8])
income<-income[,-c(2:8)]

####education####

"X15_EDUCATIONAL_ATTAINMENT"

education<-sf::st_read(dsn = './data/bg.gdb', layer = "X15_EDUCATIONAL_ATTAINMENT")
education<-factochar(education)%>%select(GEOID,
                                         'popover25' = B15003e1,
                                         B15003e22,
                                         B15003e23,
                                         B15003e24,
                                         B15003e25
                                         )

education$BG_ID<-substr(education$GEOID,8,nchar(education$GEOID))
education$belowbs<-rowSums(education[,3:6])
education<-education[,-c(3:6)]


####joining####

pop<-left_join(pop, income, by = c('BG_ID' = 'BG_ID'))
pop<-left_join(pop, housing, by = c('BG_ID' = 'BG_ID'))
pop<-left_join(pop, education, by = c('BG_ID' = 'BG_ID'))

pop<-pop%>%select(-c(GEOID.x, GEOID.y, GEOID.x.x, GEOID.y.y))


####Calculate Block Group Percentages####
pop<-pop%>%mutate('perc_ag_over' = ag_over65/pop,
             'perc_hi_under45k' = hi_under45k/homes,
             'perc_hv_under125k' = hv_under125k/homes,
             'perc_belowbs' = belowbs/popover25,
             'perc_over25' = popover25/pop)


####blocks calculations####

blockdf<-blockdf%>%group_by(BG_ID)%>%
  mutate( perc_pop = POP10/sum(POP10),
          perc_house = HOUSING10/sum(HOUSING10))

####combine BG and blocks####

blockdf<-left_join(blockdf, pop, by = c('BG_ID' = 'BG_ID'))

blockdf<-blockdf%>%mutate(pop2016 = pop * perc_pop,
                 homes2016 = homes * perc_house)

blockdf<-blockdf%>%mutate(over25_2016 = perc_over25 * pop2016,
                          ag_over65_2016 = perc_ag_over * pop2016,
                          hi_under45k_2016 = perc_hi_under45k * homes2016,
                          hv_under125k_2016 = perc_hv_under125k * homes2016)%>%
  mutate(belowbs_2016 = perc_belowbs * over25_2016)






####Join Blocks to FD####
#script uses arcpy to assign an fdid to each block. 
library(reticulate)
use_python("C:/Users/ce29109/AppData/Local/ESRI/conda/envs/arcgispro-py3-clone/python.exe", 
           required = TRUE)

source_python("ArcGIS.py") 

#read in dataframe

fdblocks<-readOGR(dsn = './data/FD_Blocks.gdb', layer = "FD_Blocks")
fdbdf<-fdblocks@data

#convert to character
fdbdf<-factochar(fdbdf)

#pad FDID with leading 0
fdbdf$FDID<-str_pad(fdbdf$FDID, 5, pad = 0)
fdbdf<-fdbdf%>%select(BLOCKID10, FDID)

t<-left_join(blockdf, fdbdf, by = c('BLOCKID10' = 'BLOCKID10'))








#keep at the bottom


ogrListLayers('./Data/FD Boundaries.gdb')


fdpoly<-readOGR(dsn = './data/FD Boundaries.gdb', layer = "FD_BoundariesDec2017_V2")




