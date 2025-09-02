
library(maps)
library(soilDB)
library(sf)
library(tidyverse)



###
#This script downloads soils data from KSSL database. 
#It's also possible to download from ssurgo
#After downloading the data I calculate Hydraulic parameters using the Van Genuchten equation from Rossetta
#These are used for making site file(.sit)for SHAW
###
setwd("C:/Users/Brian.Morra/Box/Shaw Model/examplecodeGithub")
soil_summary<-read.csv("GetSoils.csv")
s<-fetchKSSL( pedon_key = soil_summary$Pedon_Key)
s<-fetchKSSL( pedon_key = soil_summary$Pedon_Key[1])
Soils<-data.frame(s@horizons)
Soils<-Soils[,c(1,3,4,5,8:10,51,68)]




#Create a map to visualize where these points are
library(maps)
library(sf)

#open the shapefile containing the seed zones in the GB
setwd("C:/Users/Brian.Morra/Box/Shaw Model/")
seed_zones<-read_sf(dsn = "C:/Users/Brian.Morra/Box/Shaw Model/SeedZoneMap/", layer = "OverlayedFeatures")
seed_zones<-st_transform(seed_zones, crs ="WGS84")
st_crs(seed_zones)
seed_zones <- st_make_valid(seed_zones)

#get soil points the same CRS as the seed zones
points_sf <- st_as_sf(soil_summary, coords = c("x", "y"), crs = st_crs(seed_zones))
st_crs(points_sf)


#now perform a spatial join to be able to relate points to the polygons the fall within
points_within_zones <- na.omit(st_join(points_sf, seed_zones, join = st_within))

## Using the maps package for US state boundaries
bbox_seed_zones <- st_bbox(seed_zones)

us_states <- st_as_sf(maps::map("state", fill = TRUE, plot = FALSE))  


ggplot() +
  geom_sf(data = us_states, fill = "gray90", color = "black") +  # US states outline as background
  geom_sf(data = subset(seed_zones,AHM_class=="6 - 12"),alpha=0.7, aes(fill = seed_zone), color = "black") +  # Seed zones as polygons
  
  geom_sf(data = points_within_zones, size = 2,shape=21,fill="black") +  # Points on top
  
  coord_sf(xlim = c(bbox_seed_zones["xmin"], bbox_seed_zones["xmax"]),
           ylim = c(bbox_seed_zones["ymin"], bbox_seed_zones["ymax"]),expand = FALSE)+
  scale_fill_manual(values = c("dodgerblue","deeppink", "tomato", "gold", "chartreuse", "orchid", 
                               "sienna", "firebrick", "white", "mediumseagreen", "coral", 
                               "mediumpurple", "khaki", "black"))+
  theme_minimal() +
  
  labs(title = "Soil Sample Points within 6-12 Seed Zones", color = "seed zone")  # Customize title and legend




