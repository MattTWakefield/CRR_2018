library(pdftools)
library(stringr)
library(png)


####File conversion Template
#Set the working directory.
setwd("C:/GIS/CRR/CRR_2018/graphics/sf")

FileList<-list.files(pattern="pdf$", full.names = T)
NewName<-paste0("./",substr(FileList,str_locate(FileList,"_")+1,str_locate(FileList,".pdf")-1)%>%str_pad(.,5,"left","0"),".pdf")

for(i in FileList){
  
  file.rename(i,NewName[i])
  
}



  tfile<-NewName[i]
  bitmap <- pdf_render_page(tfile, page = 1, dpi=1000)
  writePNG(bitmap, str_replace(tfile,".pdf",".png"), dpi=500)
  
}

# #Convert files. 
# for (i in FileList){
#   
#   tempfile<-paste0(substr(i,1,nchar(i)-4),".png")
#   
#   bitmap <- pdf_render_page(i, page = 1, dpi=1000)
#   writePNG(bitmap, tempfile, dpi=500)
#   
# }
# 
# paste0(substr(FileList,str_locate(FileList,"_")+1,str_locate(FileList,".pdf")-1)%>%str_pad(.,5,"left","0"),".pdf")


library(pdftools)
library(stringr)
library(png)


####File conversion Template
#Set the working directory.
WD<-"./graphics/risk"

setwd(WD)

FileList<-list.files(pattern="pdf$")


for(i in FileList){
  
  file.rename(i,gsub("_","",i))
  
  
}
FileList<-list.files(pattern="pdf$")
#Convert files. 
for (i in FileList){
  
  tempfile<-paste0(substr(i,1,nchar(i)-4),".png")
  
  bitmap <- pdf_render_page(i, page = 1, dpi=1000)
  writePNG(bitmap, tempfile, dpi=500)
  
}

       