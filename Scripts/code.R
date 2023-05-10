

library(raster)
library(rio)
library(dplyr)
library(RColorBrewer)
library(metR)
library(rgdal)
library(ggplot2)
library(tidyverse)
library(raster)
library(sp)
library(ggdark)
#library(showtext)
library(ggrepel)
require("fields")
library(zoo)
library(tidync)
library(Hmisc)
library(ncdf4)
rm(list = ls())

options(download.file.extra = '--no-check-certificate')
options(timeout=600)
options(warn=-1)
rm(list=ls())
#https://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.UNIFIED_PRCP/.GAUGE_BASED/.GLOBAL/.v1p0/.Monthly/.extREALTIME/.rain/T/(Jan%202021)/(Sep%202021)/RANGEEDGES/Y/-40/0.5/40/GRID/X/55/0.5/-25/GRID/T/9/runningAverage/270/mul/ngridtable+table-+skipanyNaN+3+-table+.html
#############################################################################################################################################################################
setwd("C:/Users/oyaco/OneDrive/Bureau/Test/")
Africa<-readOGR("gadm41_KEN_shp/gadm41_KEN_1.shp") 

Data =rio::import("Africa_1997-2023_Mar24.xlsx")
Data=filter(Data,COUNTRY=='Kenya')
Data_Cum1<-Data%>%
  group_by(YEAR,ADMIN1)%>%
  summarise(Cum=sum(FATALITIES,na.rm=T))

LZ_names=paste0("LZ",sprintf("%02d", 1:length(Africa$NAME_1)))


X=as.data.frame(matrix(ncol=6, nrow=1047))
colnames(X)=c("fatalities","CDD","Perc95", "Total_rainfall", "Trend","Risk")
Data_Cum1=filter(Data_Cum1,YEAR!=2023)
colnames(Data_Cum1)[colnames(Data_Cum1)=="YEAR"]<-"year"
colnames(Data_Cum1)[colnames(Data_Cum1)=="ADMIN1"]<-"Region"

da1=filter(da,year>1996)
DD=merge(Data_Cum1,da1,by=c("Region","year"))

#X$fatalities = Data_Cum1$Cum





breakpoints=c(-Inf,seq(0,300,100),Inf)
cols=colorRampPalette(c("lightgreen", "yellow","orange","red", "darkred"))(length(breakpoints)-1)

xx=cut(c(t(X$fatalities)),breaks= breakpoints) ; levels(xx)=cols 


Africa[["fatalities"]]=as.vector(xx)
png(paste0("Fatalities01.png"), height=800, width=1200, type="cairo")

plot(Africa, col=Africa$fatalities,main="Average of Number of conflict fatalities",cex=3,cex.main=3)

legend("toprigh",text.col=cols,legend =c("[0-1]","[1-2]","[2-3]","[3-4]","[4-5]"))
dev.off()



X$ID= 1:47
library(zoo)
Trend1<-DD%>%
  group_by(Region)%>%
  summarise(TrendValue=summary(lm(coredata(Cum)~index(prcp)))$coefficients[2])

#Trend$TrendValue<-Trend$TrendValue*10

Pvalue<-DD%>%
  group_by(Region)%>%
  summarise(pvalue=summary(lm(coredata(Cum)~index(prcp)))$coefficients[8]) 


X=as.data.frame(matrix(ncol=6, nrow=47))
colnames(X)=c("fatalities","CDD","Perc95", "Total_rainfall", "Trend","Risk")
Trend1= rbind(Trend,c("Z",0));Trend1= rbind(Trend1,c("Z",0))
X$Trend1=Trend1$TrendValue
breakpoints=c(-Inf,seq(-4,4,1),Inf)
cols=colorRampPalette(c("lightgreen", "yellow","orange","red", "darkred"))(length(breakpoints)-1)
#cols=colorRampPalette(c("darkred","red","orange","yellow","lightgreen"))(length(breakpoints)-1)

xx=cut(c(t(as.numeric(X$Trend1))),breaks= breakpoints) ; levels(xx)=cols 


Africa[["Trend1"]]=as.vector(xx)
png(paste0("Correlation.png"), height=800, width=1200, type="cairo")

plot(Africa, col=Africa$Trend1,main="Correlation between conflict fatalities and climate",cex=3,cex.main=3)

legend("toprigh",text.col=cols,legend =c("[-4-3]","[-3-2]","[-2-1]","[-1-0]","[0-1]","[1-2]","[2-3]","[3-4]"))
dev.off()
