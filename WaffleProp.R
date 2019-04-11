

RiskBChart<-function(j){

sj<-stack(j[,c('LowRisk.perc','MedRisk.perc','HighRisk.perc')])

colnames(sj)<-c('Percentage','Category')

bar_p<-ggplot(sj, aes(x = 1,y = Percentage, fill = Category,label=paste0(round(Percentage*100),"%")))+
  geom_bar(stat = 'identity',position=position_stack())+coord_flip()+labs(fill="")+
  theme(legend.position='bottom',panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(), 
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank())+
  xlab("")+ylab("")+
  geom_text(size = 6, color="white",fontface="bold",position = position_stack(vjust = 0.5))+
  scale_fill_manual(values=rev(c("HighRisk.perc"=myred,"MedRisk.perc"=mygrey,"LowRisk.perc"=myblue)),guide=guide_legend(reverse=T)
                    ,labels = c("Low Risk", "Medium Risk", "High Risk")
  )
return(bar_p)

}



RiskWaffle<-function(j){
HighPart<-round(j$HighRisk.perc,1)*10
LowPart<-10-HighPart
parts<-c("High Risk"=HighPart, "At or Below Average Risk"=LowPart)
waffle_p<-waffle(parts, rows = 1, colors=c(myred,myblue), use_glyph="male", glyph_size=18
       ,legend_pos = "bottom", size=1
       ,xlab="One person represents 10 percent of the fire department's protected population rounded to the nearest 10 percent.")
return(waffle_p)

}



