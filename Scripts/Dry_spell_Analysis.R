library(ncdf4)
library(dplyr)
library(rio)
rm(list = ls())

setwd("C:/Users/oyaco/OneDrive/Bureau/Test/")
Africa<-readOGR("gadm41_KEN_shp/gadm41_KEN_1.shp")

Data_Source="CHIRPS"
MinLon=33
MaxLon=42
MinLat=-
source("C:/Users/oyaco/OneDrive/Bureau/test/Dry_spell_3.R")


k=1981
A=c(4,5,6)
for (k in 1981:2022) {
  print(k)
  if(k==1981){
    Data<-rio::import(paste("CHIRPS/",k,".csv",sep=""))
    

    
    Data$Year<-format(Data$T,"%Y")
    Data$Month=as.numeric(format(Data$T,"%m"))
    Data$prcp[is.na(Data$prcp)]=0
    Data= filter(Data,Month %in% A)
    D1=Data
    Null= (sum(is.na(Data$prcp))/nrow(Data))*100
    
    Data_Dry_Speel<-Data%>%
      group_by(X,Y,Year)%>%
      summarise(Dry_Spell=CDD_function(prcp,1,Nb=5))
    
    
    Data<-merge(Data,Data_Dry_Speel,by=c("X","Y","Year"))   
    
  
  }else{
    
    Data1<-rio::import(paste("CHIRPS/",k,".csv",sep=""))
   
    Data1$Year<-format(Data1$T,"%Y")
    Data1$Month=as.numeric(format(Data1$T,"%m"))
    Data1$prcp[is.na(Data1$prcp)]=0
    Data1= filter(Data1,Month %in% A)
    D1=rbind(D1,Data1)
    Data_Dry_Speel1<-Data1%>%
      group_by(X,Y,Year)%>%
      summarise(Dry_Spell=CDD_function(prcp,1,Nb=10))
    
  
   
    Data_Dry_Speel<-rbind(Data_Dry_Speel,Data_Dry_Speel1)
  }
}


Clim= D1%>%
  group_by(X,Y)%>%
  summarise(clim=mean(prcp))
Clim$binary <- ifelse(Clim$clim >= 1, 1, 0)

deep=filter(Clim,binary>0)

rio::export(Data_Dry_Speel,"Data/Dry_Spell_year.csv")

DrySppel<-Data_Dry_Speel%>%
   group_by(X,Y)%>%
   summarise(Mean=mean(Dry_Spell,na.rm=T))

rio::export(DrySppel,"Data/DrySppel.csv")



Africa<-readOGR("gadm41_KEN_shp/gadm41_KEN_1.shp")

Data_Source="CHIRPS"
MinLon=33
MaxLon=42
MinLat=-5
MaxLat=5
##############################Legend############################################
###Read Data########################
Data<-rio::import(paste("Data/DrySppel.csv",sep=""))

Raster_file<-rasterFromXYZ(Data[c("X","Y","Mean")])

Raster_file_1=disaggregate(Raster_file,30,method='bilinear')

rr = raster::mask(Raster_file_1 ,Africa)

Data <- as.data.frame(rasterToPoints(rr ))

mybreaks <- c(0,0.5,1,1.5,2,3,4,Inf)

#Function to return the desired number of colors

mycolors<- function(x) {
  colors<-colorRampPalette(c("#f0ff00","#ffce00","#ff9a00","#ff5a00","#ff0000","darkred"))(7)
  colors[1:x]
}

#Function to create labels for legend

breaklabel <- function(x){
  labels<-as.character(c(0,0.5,1,1.5,2,3,4))
  #labels<- as.character(seq(1,5))
  labels[1:x]
}
################################################################################

Title<-toupper(paste("June CDD average yearly over Kenya","\nRef: 1983-2022","\nData Source: ",Data_Source,sep=""))

#Im<-grid::rasterGrob(png::readPNG("Logos/Acmad_logo_1.png"), interpolate = TRUE)

l<-ggplot()+geom_contour_filled(data=Data, aes(x,y,z =Mean),breaks= mybreaks, show.legend = TRUE) +
  scale_fill_manual(palette=mycolors, values=breaklabel(7), name="", drop=FALSE, guide = guide_legend(reverse = T))+theme_bw()

last<-l+geom_polygon(data = Africa, aes(x = long,y = lat, group = group), fill = NA,color = "black",size = 1.1)+ theme(legend.position = c(.04, .04),legend.justification = c("left", "bottom"),legend.box.just = "right",legend.margin = margin(6, 6, 6, 6),legend.text = element_text(size=20),plot.title = element_text(hjust = 0.5,size=25,face = "bold"),axis.text.x = element_text(size=15,face = "bold"),axis.text.y = element_text(size=15,face = "bold"))

#last<-last+  annotation_custom(Im, xmin = MaxLon-5, xmax = MaxLon, ymin =MaxLat-5, ymax = MaxLat) +coord_cartesian(clip = "off")

last<-last+metR::scale_x_longitude(limits = c(MinLon,MaxLon),breaks =seq(MinLon,MaxLon,0.5))+scale_y_latitude(limits = c(MinLat,MaxLat),breaks =seq(MinLat,MaxLat,0.5))

last<-last+labs(title = Title,x="",y="")
dir.create(paste("Products/",sep=""),recursive = T,showWarnings = F)

jpeg(filename = paste("Products/CDD_Map_","_",Data_Source,".jpeg",sep=""),
     width = 12,
     height =14,
     units = "in",
     res=300)
print(last)
dev.off()

