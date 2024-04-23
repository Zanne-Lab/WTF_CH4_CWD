library(ggplot2)
library(tidyverse)
library(readxl)
library(raster)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggpubr)

# read in data file 'mound_CH4_emissions.csv'#

mound <-read_csv(file.choose(""))

length(unique(mound$study)) # number of studies
length(unique(mound$location)) # number of locations
length(unique(mound$site)) # number of sites

# extract latitude and longitude coordinates for sites
Coords.m<-mound%>%
  filter(!is.na(latitude))%>%
  filter(!is.na(longitude))%>%
  filter(!(scale == "hectare"))%>%
  dplyr::select(c("study", "location", "species", "longitude", "latitude", "scale"))%>%
  group_by(location)%>%
  summarise(long = longitude[1],
            lat = latitude[1],
            n = length(unique(study)),
            n2 = length(unique(species)),
            scale = scale[1])%>%
  rowid_to_column("ID")

# read in data file 'individual_CH4_emissions.csv' #

termite<-read_csv(file.choose(""))
names(termite)
length(unique(termite$reference)) # number of studies
length(unique(termite$location))

Coords.t<-termite%>%
  filter(!is.na(latitude))%>%
  filter(!is.na(longitude))%>%
  mutate(scale = "termite")%>%
  dplyr::select(c("species", "longitude", "latitude", "location", "reference", "scale"))%>%
  rename(study = reference)%>%
  group_by(location)%>%
  summarise(long = longitude[1],
            lat = latitude[1],
            n = length(unique(study)),
            n2 = length(unique(species)),
            scale = scale[1])%>%
  rowid_to_column("ID")

Coords.t

Coords3<-rbind(Coords.m, Coords.t)

# load map of earth

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

# plot termite map by number of studies in each site/location
termite.plot2<-ggplot(data = world) +  
  geom_sf(fill = "white")+
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE)+
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Estimates of flux from individual species")+
  theme_bw()+
  geom_jitter(data = Coords.t, mapping = aes(x = long, y = lat, size = n), 
             width = 0.2, alpha = 0.8, pch = 21, colour = "black", fill = "#E69F00")+
  labs(size="Number of studies")+
  scale_size(range = c(3, 7), breaks = c(1, 3, 5))

termite.plot2

# plot mound map by number of studies in each site/location
mound.plot2<-ggplot(data = world) +  
  geom_sf(fill = "white")+
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE)+
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Estimates of flux from mounds")+
  theme_bw()+
  geom_jitter(data = Coords.m, mapping = aes(x = long, y = lat, size = n), 
             width = 0.2, alpha = 0.8, pch = 21, colour = "black", fill = "#0072B2")+
  labs(size="Number of studies")+
  scale_size(range = c(3, 7), breaks = c(1, 2, 3))

mound.plot2

Fig.3<-ggarrange(termite.plot2, mound.plot2,
          ncol = 1, nrow = 2)

png("Fig.3.png",
    width=200, height = 200, units = "mm", res =800)
Fig.3
dev.off()

