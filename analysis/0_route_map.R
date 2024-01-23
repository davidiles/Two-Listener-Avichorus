rm(list=ls())
library(ggplot2)
library(tidyverse)
#library(bbsAssistant)
library(dplyr)
library(sf)
library(rnaturalearth)
#library(rgdal)

setwd("C:/Users/IlesD/OneDrive - EC-EC/Iles/Projects/Landbirds/Two-Listener-Avichorus/analysis")

#load the avichorus BBS routes
avi_routes<-read.csv("data/shapefiles/avichorus_routes.csv")

#load BCRs
BCR_data<- read_sf("data/shapefiles/BCR_boundaries/BCR_Terrestrial_master.shp") %>%
  filter(COUNTRY=="CANADA")


#create color palette
colfunc<-colorRampPalette(c("#263238","#78909C","#BDBDBD","#FFECF6"))

#map of CA coloured by BCR
ca_map<-ggplot()+
  geom_sf(data=ca_bcr,colour="grey15",aes(fill=as.factor(BCR)))+
  scale_fill_manual(breaks=c(unique(ca_bcr$BCR)),values=colfunc(12),name="BCR")+#theme(legend.position="none")+
  geom_rect(aes(xmin=-94.7,xmax=-85.8,ymin=48.5,ymax=52),color="darkviolet",fill=NA,linewidth=0.7)
 
ca_map

#map of points coloured by BCR
avi_map<-ggplot()+
  geom_sf(data=ca_bcr,colour="grey15",aes(fill=as.factor(BCR)))+
  geom_point(data=avi_routes,aes(y=Latitude,x=Longitude),colour="white",
             fill="darkviolet",size=4,pch=21,position="jitter")+
  scale_fill_manual(breaks=c(unique(ca_bcr$BCR)),values=colfunc(12))+theme(legend.position="none")+
  coord_sf(xlim=c(-94.7,-85.8),ylim=c(48.5,52))
  
avi_map

#map of CA and avi points
inset_map<-
  cowplot::ggdraw()+
  coord_equal(xlim = c(0, 8), ylim = c(0, 10), expand = FALSE) +
  annotation_custom(ggplotGrob(ca_map), xmin = 0, xmax = 8, ymin = 5.5, ymax = 10) +
  annotation_custom(ggplotGrob(avi_map), xmin = 0, xmax = 8, ymin = 0, ymax = 5) +
  geom_segment(aes(x = 4.23, xend = 7.92, y = 6.75, yend = 4.9), color = "darkviolet", linewidth = 0.8) +
  geom_segment(aes(x = 3.85, xend = 0.8, y = 6.75, yend = 4.9), color = "darkviolet", linewidth = 0.8)

inset_map  
pdf("C:\\Users\\DalyL\\OneDrive - EC-EC\\Documents\\Avi_route_map.pdf",width = 7,height=9)
inset_map
dev.off()
