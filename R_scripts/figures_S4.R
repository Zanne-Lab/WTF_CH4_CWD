library(ggplot2)
library(tidyverse)
library(readxl)
library(ggpubr)

######################
# READ IN DATA FILES #
######################

### read in data file 'mound_CH4_emissions.csv' ###

mound <-read_csv(file.choose(""))

## read in data file 'individual_CH4_emissions.csv' #

termite<-read_csv(file.choose(""))

## download and read in dataset 'termite_methane_production_rate' from Zhou et al. (2023) 
## available at https://doi.org/10.5061/dryad.vt4b8gtvk 

termite_rates<-read_excel(file.choose(""), sheet = 2)

# combine datasets on individual termite methane emissions #
termite_rates%>%
  rename(study = reference)%>%
  dplyr::select(-c(location, species))->termite_rates # remove columns present in termite file

termite_all<-left_join(termite, termite_rates, by = "ID")

#####################
# STANDARDISE UNITS #
#####################

## convert all units in mound dataframe to standard units for comparison ##
unique(mound$unit)

## by area all units changed to ug CH4 m-2 h-2 #

byarea<-mound%>%
  filter(!(nest_type == "subterranean"))%>% # remove estimates for subterranean nests
  mutate(food = ifelse(nest_type == "arboreal", "wood arboreal nest", food))%>%
  filter(scale == "area")%>%
  mutate(unit.st = case_when(unit == "ug CH4-C m-2 h-1" ~ (methane_emission*1.33),
                             unit == "ng CH4 m-2 s-1"~ (methane_emission/1000)*3600,
                             unit == "umol CH4 m-2 h-1" ~ methane_emission*16.04,
                             unit == "ug CH4 m-2 d-1" ~ methane_emission/24,
                             unit == "mg CH4-C m-2 h-1" ~ (methane_emission*1.33)*1000,
                             unit == "kg C02-e m-2 y-1" ~ ((methane_emission/25)*1e9)/8760))%>%
  dplyr::select(c("study", "scale", "nest_type", "vegetation", "family", "sub_family", "species", "food", "methane_emission", "unit.st"))

byarea

# largest emission estimate
largest<-byarea%>%
  filter(unit.st>18000)
largest

# smallest emission estimate
smallest<-byarea%>%
  filter(unit.st<600)
smallest

## by mound all units changed to mound-1 h-1
bymound<-mound%>%
  filter(!(nest_type == "subterranean"))%>% # remove estimates for subterranean nests
  mutate(food = ifelse(nest_type == "arboreal", "wood arboreal nest", food))%>%
  filter(scale %in% c("mound", "nest"))%>%
  filter(!(unit == "ug kg-1mound h-1"))%>%
  mutate(scale = if_else(scale == "nest", "mound", scale))%>%
  mutate(unit.st = case_when(unit == "ng CH4 mound-1 s-1" ~ (methane_emission/1000)*3600,
                             unit == "umol CH4 mound-1 h-1"~ methane_emission*16.04,
                             unit == "ug CH4 mound-1 s-1" ~ methane_emission*3600,
                             unit == "mg CH4 mound-1 h-1" ~ methane_emission*1000))%>%
  filter(!is.na(unit.st))%>%
  dplyr::select(c("study", "scale", "nest_type", "vegetation", "family", "sub_family", "species", "food", "methane_emission", "unit.st"))

# largest emission estimate
largest<-bymound%>%
  filter(unit.st>10000)
largest

# smallest emission estimate
smallest<-bymound%>%
  filter(unit.st<0.1)
smallest

#################
# JOIN DATASETS #
#################

# combine datasets from mounds and termites to produce figures

bytermite<-termite_all%>%
  mutate(unit.st = as.numeric(production_rate), # create a numeric column for methane emissions
         scale = "termite",
         nest_type = NA)%>%
  filter(!is.na(unit.st))%>% # remove any NA rows
  dplyr::select(c("study", "scale", "nest_type", "ecosystem", "family", 
                  "new_subfamily_names", "species", "new_feeding_groups", "production_rate", "unit.st"))%>%
  rename(vegetation =  ecosystem,
         sub_family = new_subfamily_names,
         food = new_feeding_groups,
         methane_emission = production_rate)%>%
  mutate(food = case_when(food == "Wood feeder" ~ "wood",
                          food == "Soil feeder" ~ "soil",
                          food == "Grass feeder" ~ "grass",
                          food == "Wood/soil feeder" ~ "wood/soil",
                          food == "Fungus grower" ~ "fungus",
                          food == "Lichen/moss feeder" ~ "lichen/moss",
                          food == "Debris feeder"  ~ "debris"))

# combine all emission data (on mounds - per mound and per area and by termites)
all<-rbind(byarea, bymound, bytermite)
unique(all$food)

# correct typos in columns #
all<-all%>%
  mutate(food = str_replace(food, "wood / soil", "wood/soil"))%>%
  mutate(food = str_replace(food, "wood /soil", "wood/soil"))%>%
  filter(!is.na(unit.st))%>%
  filter(!is.na(food))%>%
  mutate(family = ifelse(family == "Termitiidae", "Termitidae", family))


all$food<-factor(all$food, levels=c("wood arboreal nest", "wood", "debris", "grass", "lichen/moss", "fungus", "wood/soil", "soil"))

##################
# SUMMARISE DATA #
##################

# number of estimates, number of species, number of studies
all%>%
  group_by(scale)%>%
  summarise(est = length(unit.st),
            sp = length(unique(species)),
            st = length(unique(study)))

# number of named species in mound studies
md_sp<-all%>%
  filter(!(scale == "termite"))
unique(md_sp$species)

# number of named species in individual termites
tm_sp<-all%>%
  filter((scale == "termite"))
unique(tm_sp$species)

# proportion of estimates within subfamily and feeding group
all%>%
  filter(!is.na(unit.st))%>%
  filter(scale == "termite")%>%
  group_by(sub_family)%>%
  summarise(n = length(unit.st))

all%>%
  filter(!is.na(unit.st))%>%
  filter(scale == "termite")%>%
  group_by(food)%>%
  summarise(n = length(unit.st))

all%>%
  filter(!is.na(unit.st))%>%
  filter(!(scale == "termite"))%>%
  group_by(sub_family)%>%
  summarise(n = length(unit.st))

all%>%
  filter(!is.na(unit.st))%>%
  filter(!(scale == "termite"))%>%
  group_by(food)%>%
  summarise(n = length(unit.st))

##################
## PLOT FIGURES ##
##################

# LABELS FOR PLOTS #

my_labeller = as_labeller(
  c(
    area = 'mu~g~CH[4]~m^{-2}~h^{-1}', 
    mound = 'mu~g~CH[4]~mound^{-1}~h^{-1}',
    termite = 'mu~g~CH[4]~g[termite]^{-1}~h^{-1}'
  ), 
  default = label_parsed)

dat_text <- data.frame(
  label = c("Flux by mound area (n = 21)", "Flux by mound (n = 38)", "Flux by termite (n = 233)"),
  scale   = c("area", "mound", "termite")
)


## Figure S4.1: By feeding group ##
mean_food <- all%>%
  group_by(food, scale) %>% 
  summarise(mean_CH4 = mean(unit.st), 
            n=n(),
            sd = sd(unit.st),
            se = sd / sqrt(n),
            upperCI = mean_CH4 + (1.96*se),
            lowerCI = mean_CH4 - (1.96*se))

mean_food

Fig.S4.1<-all%>%
  ggplot(aes(x=food, y=unit.st)) + 
  geom_boxplot()+
  facet_wrap(~scale, 
             scales = "free_y",
             labeller = my_labeller,
             strip.position = "left")+
  geom_text(data = mean_food,
            aes(x= food, y= 0, label = n), colour = "black", size = 3,
            vjust = 1.5, position=position_dodge(.9))+
  labs(x = "Feeding group",
       y = "")+
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  theme_bw(base_size=16)+
  theme(strip.background = element_blank(),
        strip.placement = "outside")+
  geom_text(
    data    = dat_text,
    mapping = aes(x = 1, y = Inf, label = label),
    hjust   = 0.1,
    vjust   = 1,
    size=6
  )

Fig.S4.1

png("Fig.S4.1",
    width=350, height = 200, units = "mm", res =400)
Fig.S4.1
dev.off()

# Figure.S4.2: by family #

mean_f <- all%>%
  group_by(family, scale) %>% 
  summarise(mean_CH4 = mean(unit.st), 
            n=n(),
            sd = sd(unit.st),
            se = sd / sqrt(n),
            upperCI = mean_CH4 + (1.96*se),
            lowerCI = mean_CH4 - (1.96*se))

mean_f

Fig.S4.2<-all%>%
  ggplot(aes(x=family, y=unit.st)) + 
  geom_boxplot()+
  facet_wrap(~scale, 
             scales = "free_y",
             labeller = my_labeller,
             strip.position = "left")+
  geom_text(data = mean_f,
            aes(x= family, y= 0, label = n), colour = "black", size = 3,
            vjust = 1.5, position=position_dodge(.9))+
  labs(x = "Family",
       y = "")+
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  theme_bw(base_size=16)+
  theme(strip.background = element_blank(),
        strip.placement = "outside")+
  geom_text(
    data    = dat_text,
    mapping = aes(x = 1, y = Inf, label = label),
    hjust   = 0.1,
    vjust   = 1,
    size=6
  )

Fig.S4.2

png("Fig.S4.2.png",
    width=350, height = 200, units = "mm", res =400)
Fig.S4.2
dev.off()

# Figure S4.3: by subfamily #

mean_subf <- all%>%
  group_by(sub_family, scale) %>% 
  summarise(mean_CH4 = mean(unit.st), 
            n=n(),
            sd = sd(unit.st),
            se = sd / sqrt(n),
            upperCI = mean_CH4 + (1.96*se),
            lowerCI = mean_CH4 - (1.96*se))

mean_subf

Fig.S4.3<-all%>%
  ggplot(aes(x=sub_family, y=unit.st)) + 
  geom_boxplot()+
  facet_wrap(~scale, 
             scales = "free_y",
             labeller = my_labeller,
             strip.position = "left")+
  geom_text(data = mean_subf,
            aes(x= sub_family, y= 0, label = n), colour = "black", size = 3,
            vjust = 1.5, position=position_dodge(.9))+
  labs(x = "Subfamily",
       y = "")+
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  theme_bw(base_size=16)+
  theme(strip.background = element_blank(),
        strip.placement = "outside")+
  geom_text(
    data    = dat_text,
    mapping = aes(x = 1, y = Inf, label = label),
    hjust   = 0.1,
    vjust   = 1
  )

Fig.S4.3

png("Fig.S4.3.png",
    width=350, height = 200, units = "mm", res =400)
Fig.S4.3
dev.off()

#############################
# Ecosystem scale estimates #
#############################

##  select only hectare scale measurements ###

eco.md<-mound%>%
  filter(scale == "hectare")

unique(eco.md$vegetation)
unique(eco.md$location)
unique(eco.md$study)
unique(eco.md$site)
nrow(eco.md)

eco.md<-eco.md%>%
  mutate(vegetation = case_when(vegetation == "savanna" ~ "Savanna",
                                vegetation %in% c("Rainforest", "Rainforest primary", "Rainforest secondary old", "Rainforest secondary young") ~ "Rainforest",
                                vegetation %in% c("Pre montane forest", "Pre montane forest secondary", "Pre montane forest primary") ~ "Premontane forest",
                                vegetation == "ephemeral wetland" ~ "Ephemeral wetland"))%>%
  mutate(continent = case_when(location %in% c("Darwin Australia", "Queensland Australia") ~ "Australia",
                               location %in% c("Burkina Faso", "Cameroon") ~ "Africa",
                               location == ("Central Amazonia Brazil") ~ "South America",
                               location == c("Sabah Malaysia")~ "Asia"))



# Figure S4.4a: by continent #
samplesize <- eco.md %>% 
  group_by(continent) %>% 
  tally()

Fig.S4.4a<-eco.md%>%
  ggplot(aes(x=continent, y= methane_emission)) + 
  geom_boxplot()+
  geom_text(data = samplesize,
            aes(continent, y=0, label = n), vjust = 1)+
  geom_text(data = samplesize,
            aes(x= 3, y=3, label = "Unpub. data"), vjust = 1)+
  labs(x = "Continent",
       y = expression(atop("Methane production rate", 
                           paste(~kg~CH[4]~ha^-1~y^-1))),
       title = "(i)")+
  theme_bw(base_size =16)

Fig.S4.4a

# Figure.S4.4b: by vegetation #

samplesize <- eco.md %>% 
  group_by(vegetation) %>% 
  tally()

Fig.S4.4b<-eco.md%>%
  ggplot(aes(x=vegetation, y= methane_emission)) + 
  geom_boxplot()+
  geom_text(data = samplesize,
            aes(vegetation, y=-0.1, label = n), vjust = 1)+
  geom_text(data = samplesize,
            aes(x= 4, y=3, label = "Unpub. data"), vjust = 1)+
  geom_point(aes(x = 4, y = 0.016), shape =8, size = 2)+
  geom_point(aes(x = 3, y = 0.011), shape = 8, size = 2)+
  labs(x = "Vegetation",
       y = "",
       title = "(ii)")+
  theme_bw(base_size =16)

Fig.S4.4b


Fig.S4.4<-ggarrange(Fig.S4.4a, Fig.S4.4b,
                        ncol = 2, nrow = 1)

png("Fig.S4.4.png",
    width=350, height = 200, units = "mm", res =400)
Fig.S4.4
dev.off()



