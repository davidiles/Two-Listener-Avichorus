
library(tidyverse)
library(dplyr)
library(sf)
library(ggspatial)

rm(list=ls())

sf_use_s2(FALSE)


setwd("C:/Users/IlesD/OneDrive - EC-EC/Iles/Projects/Landbirds/Two-Listener-Avichorus/analysis")

# Albers equal area projection
proj <- "+proj=aea +lat_0=60 +lat_1=50 +lat_2=70 +lon_0=-100 +x_0=0 +y_0=0 +datum=NAD83 +units=km +no_defs"

#load the avichorus BBS routes
avi_routes<-read.csv("data/shapefiles/avichorus_routes.csv") %>%
  st_as_sf(coords = c("Longitude", "Latitude"),crs=4326, remove = FALSE) %>%
  st_transform(proj)

# bounding box around survey area
bbox <- st_bbox(avi_routes) %>% st_as_sfc()

xlim <- range(as.data.frame(st_coordinates(bbox))$X)
xlim[1] <- xlim[1] - 500
xlim[2] <- xlim[2] + 500
ylim <- range(as.data.frame(st_coordinates(bbox))$Y)
ylim[1] <- ylim[1] - 500
ylim[2] <- ylim[2] + 500



#load BCR boundaries
all_BCRs <- read_sf("data/shapefiles/BCR_boundaries/BCR_Terrestrial_master.shp") %>%
  filter(COUNTRY %in% c("CANADA","USA")) %>%
  st_transform(crs = proj)

# Country border

CAN <- read_sf("data/shapefiles/BCR_boundaries/BCR_Terrestrial_master.shp") %>%
  filter(COUNTRY=="CANADA") %>%
  st_transform(crs = proj)  %>%
  st_make_valid() %>%
  st_union() %>%
  st_buffer(1)

USA <- read_sf("data/shapefiles/BCR_boundaries/BCR_Terrestrial_master.shp") %>%
  filter(COUNTRY=="USA") %>%
  st_transform(crs = proj) %>%
  st_make_valid() %>%
  st_union() %>%
  st_buffer(1)


border <- st_intersection(CAN,USA)

#create color palette
colfunc<-colorRampPalette(c("#263238","#78909C","#BDBDBD","#FFECF6"))

survey_map <- ggplot()+
  
  geom_sf(data=all_BCRs,colour="grey15",aes(fill=as.factor(BCR)))+
  geom_sf(data=border,colour="black",fill = "transparent", linewidth = 1)+
  
  geom_sf(data=avi_routes,colour="white",fill="darkviolet",size=3,pch=21)+
  
  scale_fill_manual(breaks=c(unique(all_BCRs$BCR)),
                    values=colfunc(length(unique(all_BCRs$BCR))),name="BCR")+
  
  coord_sf(xlim = xlim, ylim = ylim, crs = proj)+
  
  annotation_scale(style = "ticks",
                   text_face = "bold")+
  
  annotation_north_arrow(which_north = "true",
                         location = "tr",
                         pad_x = unit(0.25, "cm"), pad_y = unit(0.25, "cm"),
                         height = unit(1, "cm"),
                         width = unit(1, "cm"),
                         style = north_arrow_fancy_orienteering(text_col = 'black',
                                                                line_col = 'gray20',
                                                                text_face = "bold",
                                                                fill = 'gray80'))+
  
  theme_bw()+
  
  theme(legend.position="none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

png("output/figures/Fig_1_map.png",units="in", 
    res = 600, width = 6, height=4)
print(survey_map)
dev.off()
