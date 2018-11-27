library(rgdal)
library(sf)
blocks <- readOGR(dsn = "./Data/blocksf", layer = "tabblock2010_47_pophu")
blockdf<-blocks@data
factochar<-function(x){
  i <- sapply(x, is.factor)
  x[i] <- lapply(x[i], as.character)
  return(x)
}
blockdf$BG_ID<-substr(blockdf$BLOCKID10,1,12)


ogrListLayers('./Data/bg.gdb')
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
pop$Over65<-rowSums(pop[,3:14])
pop<-pop[,-c(3:14)]


####housing####
housing<-sf::st_read(dsn = './data/bg.gdb', layer = "X25_HOUSING_CHARACTERISTICS")
housing<-factochar(housing)%>%select(GEOID, 
                             'homes'=B25001e1,
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

mdata<-sf::st_read(dsn = './data/bg.gdb', layer = "BG_METADATA_2016")

#B01001e1 - population
