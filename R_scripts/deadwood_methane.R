library(ggplot2)
library(tidyverse)
library(gridExtra)
library(ggpubr)
library(parameters)

#################################
# DOWNLOAD RAW DATA AND PROCESS #
#################################

# DATA ON GAS EMISSIONS FROM DEADWOOD #

#download the data 'wood_respiration_rates.csv' from Duan et al., 2023  

data<-read.csv("https://raw.githubusercontent.com/Zanne-Lab/WTF-Climate-Flux/main/weather_flux/data/processed/wood_respiration/wood_respiration_rates.csv")
as.data.frame(data)
str(data)

data<-data%>%
  mutate(termite_present = replace_na(termite_present, 0))%>%
  mutate(CH4_resp_rate_hr = CH4_resp_rate * 3600)%>% # change methane production rate from per sec to per hour
  mutate(biome = case_when(site %in% c("DRO", "MLRF") ~ "Rainforest",
                           site %in% c("PNW", "STCK") ~ "Savanna"))%>%# add in biomes for sites
  mutate(experiment = if_else(Species.Code == "PIRA", "Pine", "Native"))%>% # add in type of wood used (pine or native)
  filter(!(is.na(CH4_resp_rate_hr))) # remove any NA values for CH4 production

### read in data file 'woodblock_termite_presence'

termites<-read.csv(file.choose(""), header =T, sep = ",")

# extract columns with SampleID and ter_in_block  
termites_cut<-termites%>%
  mutate(SampleID = as.character(SampleID))%>%
  select(c(SampleID, ter_in_block))

# join data on gas emissions with presence of termites in wood blocks

data_all<-left_join(data, termites_cut)%>%
  mutate(ter_in_block = replace_na(ter_in_block, 0))%>%
  mutate(site = if_else(site %in% c("OLD_RESCUE", "YOUNG_RESCUE"), "RF_RESCUE", site))# join old_rescue and young_rescue into the same site

data_cut<-data_all%>% # remove samples with a negative methane production (possible measurement error, removed 262 blocks so n = 773)
  filter(CH4_resp_rate_hr >0)%>%
  filter(!(is.na(CH4_resp_rate_hr)))

# DATA ON DEADWOOD SURVEY #

### read in data file 'dead_wood_survey.csv ###

deadwood <-read.csv(file.choose(""))
str(deadwood)

################################################################
# SUMMARISE DATA AND CALCULATE MEAN GAS EMISSIONS FROM DEADWOOD#
################################################################

# how many blocks had termites present during gas measurements
perc<-data_all%>%
  group_by(experiment, ter_in_block)%>%
  summarise(presence = length(ter_in_block))

perc

# how many blocks gave a positive methane signal
perc2<-data_all%>%
  mutate(methane = if_else(CH4_resp_rate <0, 0, 1))%>% #binary assignment, 0 if no methane signal, 1 if methane signal produced
  group_by(experiment, ter_in_block, methane)%>%
  summarise(methane2 = length(methane))

perc2

# summary of gas emissions from blocks with termites present
TB<-data_all%>%
  filter(ter_in_block == 1)%>%
  select(SampleID, site, experiment, CO2_resp_rate, CH4_resp_rate)

TB

# mean emissions across all samples if termites are present or absent

mean_exp <- data_cut%>%
  group_by(as.factor(ter_in_block)) %>% 
  summarise(mean_CH4 = mean(CH4_resp_rate_hr), 
            n=n(),
            sd = sd(CH4_resp_rate_hr),
            se = sd / sqrt(n),
            upperCI = mean_CH4 + (1.96*se),
            lowerCI = mean_CH4 - (1.96*se))

mean_exp

#################
# PLOT FIGURE 2 #
#################

# mean by biome (i.e. using native and pine in rain forest (RF) (MLRF and DRO sites) and savanna (SV) (PNW and STCK sites))

mean_biome <- data_cut%>%
  filter(!(is.na(biome)))%>%
  group_by(biome, ter_in_block) %>% 
  summarise(mean_CH4 = mean(CH4_resp_rate_hr), 
            n=n(),
            sd = sd(CH4_resp_rate_hr),
            se = sd / sqrt(n),
            upperCI = mean_CH4 + (1.96*se),
            lowerCI = mean_CH4 - (1.96*se))

mean_biome

Fig.2a<-mean_biome%>%
  mutate(ter_in_block = as.factor(ter_in_block))%>%
  ggplot(aes(x=ter_in_block, y=mean_CH4)) + 
  geom_bar(position=position_dodge(), stat="identity", colour='black', fill ="grey")+
  geom_errorbar(aes(ymin=mean_CH4 - se, ymax=mean_CH4 + se), width=.2,position=position_dodge(.9))+
  geom_text(aes(label=n,y = -0.0005), position=position_dodge(.9), size =5)+
  facet_wrap(~biome)+
  labs(x = "Termites present in block",
       y = expression(atop("Methane production rate", 
                           paste(~mu~g~CH[4]~g^-1~(wood)~h^-1))))+
  scale_x_discrete(labels=c("0" = "No", "1" = "Yes"))+
  ylim(-0.0005, 0.009)+
  ggtitle("(a) Comparing biomes")+
  theme_bw(base_size=18) 

Fig.2a

# mean emission plus se by gradient (experiment) but only using sites DRO/PNW

mean_exp <- data_cut%>%
  filter(site %in% c("DRO", "PNW"))%>%
  group_by(experiment, ter_in_block) %>% 
  summarise(mean_CH4 = mean(CH4_resp_rate_hr), 
            n=n(),
            sd = sd(CH4_resp_rate_hr),
            se = sd / sqrt(n),
            upperCI = mean_CH4 + (1.96*se),
            lowerCI = mean_CH4 - (1.96*se))

mean_exp

Fig.2b<-mean_exp%>%
  mutate(ter_in_block = as.factor(ter_in_block))%>%
  ggplot(aes(x=ter_in_block, y=mean_CH4)) + 
  geom_bar(position=position_dodge(), stat="identity", colour='black', fill = "grey")+
  geom_errorbar(aes(ymin=mean_CH4 - se, ymax=mean_CH4 + se), width=.2,position=position_dodge(.9))+
  geom_text(aes(label=n,y = -0.0005), position=position_dodge(.9), size = 5)+
  facet_wrap(~experiment)+
  labs(x = "Termites present in block",
       y = expression(atop("Methane production rate", 
                           paste(~mu~g~CH[4]~g^-1~(wood)~h^-1))))+
  scale_x_discrete(labels=c("0" = "No", "1" = "Yes"))+
  ylim(-0.0005, 0.009)+
  ggtitle("(b) Comparing wood type")+
  theme_bw(base_size=18) 

Fig.2b

Fig2<-ggarrange(Fig.2a, Fig.2b,
                          ncol = 2, nrow = 1)

Fig2

png("Fig2.png",
    width=350, height=200, units= "mm", res = 400)
Fig2
dev.off()

##################
# PLOT FIGURE S2 #
##################

# mean emission plus se by experiment and by site

mean_ex_site <- data_cut%>%
  group_by(experiment, site, ter_in_block) %>% 
  summarise(mean_CH4 = mean(CH4_resp_rate_hr), 
            n=n(),
            sd = sd(CH4_resp_rate_hr),
            se = sd / sqrt(n),
            upperCI = mean_CH4 + (1.96*se),
            lowerCI = mean_CH4 - (1.96*se))%>%
  mutate(site2 = case_when(site == "DRO" ~ "Rainforest: DRO",
                           site == "PNW" ~ "Savanna: PNW",
                           site == "DRO_drought" ~ "Rainforest drought",
                           site == "MLRF" ~ "Rainforest: MLRF",
                           site == "MLES" ~ "Wet sclerophyll",
                           site == "STCK" ~ "Savanna: STCK",
                           site == "RF_RESCUE" ~ "Rainforest: RESCUE"))%>%
  ungroup()%>%
  add_row(experiment = "Pine", site = "DRO", site2 = "Rainforest: DRO", ter_in_block = 1, mean_CH4 = 0, n = 0)%>%
  add_row(experiment = "Pine", site = "MLRF", site2 = "Rainforest: MLRF", ter_in_block = 1, mean_CH4 = 0, n = 0)


mean_ex_site

Fig.S2a<-mean_ex_site%>%
  filter(experiment == "Native")%>%
  mutate(ter_in_block = as.factor(ter_in_block))%>%
  ggplot(aes(x=ter_in_block, y=mean_CH4)) + 
  geom_bar(position=position_dodge(), stat="identity", colour='black', fill = "grey" )+
  geom_errorbar(aes(ymin=mean_CH4 - se, ymax=mean_CH4 + se), width=.2,position=position_dodge(.9))+
  geom_text(aes(label=n, y = -0.0002), position=position_dodge(.9), size =5)+
  facet_wrap(~site2)+
  labs(title = "Native",
       x = "Termites present in block",
       y = expression(atop("Methane production rate", 
                           paste(~mu~g~CH[4]~g^-1~(wood)~h^-1))))+
  scale_x_discrete(labels=c("0" = "No", "1" = "Yes"))+
  ggtitle("(a) Emissions across sites for native deadwood")+
  theme_bw(base_size=18) 

Fig.S2a

Fig.S2b<-mean_ex_site%>%
  filter(experiment == "Pine")%>%
  mutate(ter_in_block = as.factor(ter_in_block))%>%
  mutate(mean_CH4 = if_else(mean_CH4 <0 , 0, mean_CH4))%>%
  ggplot(aes(x=ter_in_block, y=mean_CH4)) + 
  geom_bar(position=position_dodge(), stat="identity", colour='black', fill = "grey")+
  geom_errorbar(aes(ymin=mean_CH4 - se, ymax=mean_CH4 + se), width=.2,position=position_dodge(.9))+
  geom_text(aes(label=n), hjust = 1.5, vjust = 0, position=position_dodge(.9), size =5)+
  facet_wrap(~site2, scales = "free")+
  labs(title = "Pine gradient",
       x = "Termites present in block",
       y = expression(atop("Methane production rate", 
                           paste(~mu~g~CH[4]~g^-1~(wood)~h^-1))))+
  scale_x_discrete(labels=c("0" = "No", "1" = "Yes"))+
  ggtitle("(b) Emissions across sites for pine deadwood")+
  theme_bw(base_size=18) 

Fig.S2b

Fig.S2<-ggarrange(Fig.S2a, Fig.S2b,
                         ncol = 2, nrow = 1)

Fig.S2

png("FigS2.png",
    width = 500, height=300, units ="mm", res = 400)
Fig.S2
dev.off()


##############################################################

#### UPSCALE ESTIMATES ####

# (1) Estimate Biomass of deadwood in each site (DRO and PNW) using survey from this study and from other published surveys on deadwood

# data from this study (dead_wood_survey.csv) calculate volume of each piece using truncated cone /fustrum =  1/3 * pi * length * (R2 + Rr + r2)
deadwood_est<-deadwood%>%
  mutate(radius1 = diameter_1/2,
         radius2 = diameter_2/2)%>%
  mutate(R2 = radius1^2,
         Rr = radius1*radius2,
         r2 = radius2^2)%>%
  mutate(volume = (pi * length * (R2 + Rr + r2))/3)%>%
  mutate(vol_m3 = volume/1000000)%>%# convert volume from cm3 to m3
  select(diameter_1, diameter_2, length, R2, Rr, r2, volume, vol_m3, site)

# get total volume of deadwood at each site #
deadwood_est%>%
  filter(!is.na(vol_m3))%>%
  group_by(site)%>%
  summarise(total_vol = sum(vol_m3), # in a 50m x 50m plot
            vol_ha = total_vol*4) # scale up to hectare

#### create data.frame with deadwood volume estimates from all three studies (this study, Cheesman et al., 2018 (https://doi.org/10.1111/aec.12561) and Clement et al., 2021 (https://doi.org/10.3389/fevo.2021.657444))
site<-c(rep("DRO",3 ), rep("PNW", 3))
est<-rep(c("Che.2018", "Cle.2021", "This.study"),2)
vol_est<-c(23.5, 192.39, 39.3,
           19.6, 1.47, 12.5)
wd<-c(rep(0.527,3), rep(0.715,3)) # mean wood density for native wood in DRO and PNW taken from Law et al., 2023
BM_est<-as.data.frame(cbind.data.frame(site, est, vol_est, wd))%>%
  mutate(BM = vol_est * wd) # estimate biomass by vol * wood density, units are Mg ha-1 #
BM_est

mean_bm<-BM_est%>%
  group_by(site)%>%
  summarise(mean_BM = mean(BM))

mean_bm # calculate mean biomass from 3 studies

# (2) Upscale using estimates from Native deadwood only (in rain forest site DRO, and savanna site PNW) #

# Add in estimates of deadwood mass in each site (DRO and PNW) and proportion of deadwood with termites to data frame #

data.3<-data_cut%>%
  mutate(CH4_g_y = (CH4_resp_rate_hr) * 8760)%>% # convert g per h to g per y
  mutate(biome = case_when(site %in% c("DRO", "MLRF") ~ "Rainforest",
                           site %in% c("PNW", "STCK") ~ "Savanna"))%>% # add biomes
  mutate(mass_DW_Mg_Ch = case_when(biome == "Rainforest" ~ 12.38,
                                   biome == "Savanna" ~  14.01))%>% #add in est of DW Mg per ha from Cheesman et al., 2018 
  mutate(mass_DW_Mg_Hb = case_when(biome == "Rainforest" ~ 20.71,
                                biome == "Savanna" ~  8.94))%>% #add in est of DW Mg per ha from this study  
  mutate(mass_DW_Mg_Cl = case_when(biome == "Rainforest" ~ 100.39,
                                   biome == "Savanna" ~  1.05))%>% #add in est of DW Mg per ha from Clement et al. 2021
  mutate(mass_DW_Mg_M = case_when(biome == "Rainforest" ~ 44.8,
                                   biome == "Savanna" ~  8.00))%>% #add in est of DW Mg per ha mean of above three
  mutate(biome_area = case_when(biome == "Rainforest" ~ 2267674, # total biome area in Australia taken from www.agriculture.gov.au 
                                biome == "Savanna" ~ 190000000))%>% # total biome area in Australia taken from Jamali et al., 2011 (https://doi.org/10.1007/S10021-01). 
  mutate(prop_Cl = case_when(site == "PNW" ~ 0.21,
                             site == "STCK" ~ 0.24,
                             site == "MLES" ~ 0.02,
                             site == "MLRF" ~ 0.01,
                             site == "DRO" ~ 0.03))# add in proportion of DW with termites from Clement et al., 2021


# LOWER ESTIMATE using mean estimate of deadwood in each site and proportion of deadwood with termites at 1% and 3% in RF and SV

lower_nat <- data.3%>%
  group_by(site, experiment, ter_in_block) %>% 
  summarise(mean_CH4 = mean(CH4_g_y), # micrograms of CH4 per gram of deadwood per year
            n=n(),
            sd = sd(CH4_g_y),
            se = sd / sqrt(n),
            upperCI = mean_CH4 + (1.96*se),
            lowerCI = mean_CH4 - (1.96*se),
            biome = biome[1], 
            biome_area = biome_area[1],
            mass_DW_Mg_M = mass_DW_Mg_M[1],
            prop_Cl = prop_Cl[1])%>%
  mutate(freq = n / sum(n))%>%
  filter(ter_in_block == 1,
         site %in% c("PNW", "DRO"),
         experiment == "Native")%>%
  mutate(DW_ter = (mass_DW_Mg_M*1000000)*freq)%>% # mass of DW with termites in g ha-1
  summarise(kg_CH4_ha_y = (mean_CH4*DW_ter)/1e9,
            kg_CO2e_ha_y = kg_CH4_ha_y*25,
            kg_CH4_y_biome = kg_CH4_ha_y*biome_area,
            Tg_CH4_y_biome = kg_CH4_y_biome / 1e9,
            kg_CO2e_y_biome = kg_CH4_y_biome*25,
            Tg_CO2e_y_biome = Tg_CH4_y_biome*25,
            DW_ter = DW_ter[1])
  

lower_nat

# UPPER ESTIMATE using proportion of deadwood with termites of 3% in RF and 21% in SV (from Clement et al., 2021)

upper_nat <- data.3%>%
  group_by(site, experiment, ter_in_block) %>% 
  summarise(mean_CH4 = mean(CH4_g_y), 
            n=n(),
            sd = sd(CH4_g_y),
            se = sd / sqrt(n),
            upperCI = mean_CH4 + (1.96*se),
            lowerCI = mean_CH4 - (1.96*se),
            biome = biome[1], 
            biome_area = biome_area[1],
            mass_DW_Mg_Hb = mass_DW_Mg_Hb[1],
            mass_DW_Mg_Ch = mass_DW_Mg_Ch[1],
            mass_DW_Mg_Cl = mass_DW_Mg_Cl[1],
            mass_DW_Mg_M = mass_DW_Mg_M[1],
            prop_Cl = prop_Cl[1])%>%
  mutate(freq = n / sum(n))%>%
  filter(ter_in_block == 1,
         site %in% c("PNW", "DRO"),
         experiment == "Native")%>%
  mutate(DW_ter = (mass_DW_Mg_M*1000000)*prop_Cl)%>%
  summarise(kg_CH4_ha_y = (mean_CH4*DW_ter)/1e9,
            kg_CO2e_ha_y = kg_CH4_ha_y*25,
            kg_CH4_y_biome = kg_CH4_ha_y*biome_area,
            Tg_CH4_y_biome = kg_CH4_y_biome / 1e9,
            kg_CO2e_y_biome = kg_CH4_y_biome*25,
            Tg_CO2e_y_biome = Tg_CH4_y_biome*25)

upper_nat


# (2) Upscale using estimates of CH4 from Native and pine deadwood in RF (DRO and MLRF sites) and SV (PNW and STCK sites) #

# LOWER ESTIMATE # 

lower_biome <- data.3%>%
  group_by(biome, ter_in_block) %>% 
  summarise(mean_CH4 = mean(CH4_g_y), 
            n=n(),
            sd = sd(CH4_g_y),
            se = sd / sqrt(n),
            upperCI = mean_CH4 + (1.96*se),
            lowerCI = mean_CH4 - (1.96*se),
            biome = biome[1], 
            biome_area = biome_area[1],
            prop_Cl = prop_Cl[1])%>%
  mutate(freq = n / sum(n))%>%
  filter(!is.na(biome))%>%
  filter(ter_in_block == 1)%>%
  mutate(mass_DW_Mg = if_else(biome == "Rainforest", 58.39, 2.29))%>% # deadwood mass estimated from mean deadwood volume in sv and rf sites in Clement et al. 2021 and multiplied by density
  mutate(DW_ter = (mass_DW_Mg*1000000)*freq)%>% # using proportion of deadwood with termites from this study: 1% in Rf and 6% in SV
  summarise(kg_CH4_ha_y = (mean_CH4*DW_ter)/1e9,
            kg_CO2e_ha_y = kg_CH4_ha_y*25,
            kg_CH4_y_biome = kg_CH4_ha_y*biome_area,
            Tg_CH4_y_biome = kg_CH4_y_biome / 1e9,
            kg_CO2e_y_biome = kg_CH4_y_biome*25,
            Tg_CO2e_y_biome = Tg_CH4_y_biome*25,
            freq = freq[1])

lower_biome

# UPPER estimate using Clement proportion

upper_biome <- data.3%>%
  group_by(biome, ter_in_block) %>% 
  summarise(mean_CH4 = mean(CH4_g_y), 
            n=n(),
            sd = sd(CH4_g_y),
            se = sd / sqrt(n),
            upperCI = mean_CH4 + (1.96*se),
            lowerCI = mean_CH4 - (1.96*se),
            biome = biome[1], 
            biome_area = biome_area[1])%>%
  mutate(freq = n / sum(n))%>%
  mutate(prop_Cl = case_when(biome == "Rainforest" ~ 0.02,
                             biome == "Savanna" ~ 0.225))%>% # proportion of deadwood with termites taken from Clement et al., (2021)
  mutate(mass_DW_Mg = if_else(biome == "Rainforest", 58.39, 2.29))%>% 
  filter(!is.na(biome))%>%
  filter(ter_in_block == 1)%>%
  mutate(DW_ter = (mass_DW_Mg*1000000)*prop_Cl)%>%
  summarise(kg_CH4_ha_y = (mean_CH4*DW_ter)/1e9,
            kg_CO2e_ha_y = kg_CH4_ha_y*25,
            kg_CH4_y_biome = kg_CH4_ha_y*biome_area,
            Tg_CH4_y_biome = kg_CH4_y_biome / 1e9,
            kg_CO2e_y_biome = kg_CH4_y_biome*25,
            Tg_CO2e_y_biome = Tg_CH4_y_biome*25)

upper_biome

############################
# LINEAR REGRESSION MODELS #
############################

# log transform methane emissions for normality

data_cut<-data_cut%>%
  mutate(CH4_tr = log(CH4_resp_rate_hr))

# Does site or presence of termites predict methane production?
mod1<-lm(CH4_tr~ as.factor(site) + as.factor(ter_in_block), data = data_cut)
summary(mod1)
model_parameters(mod1)
model_parameters(anova(mod1))
plot(resid(mod1))
qqnorm(resid(mod1))

# Does biome (rf or sv) or presence of termites predict methane production?
mod2<-lm(CH4_tr~as.factor(biome) + as.factor(ter_in_block), data = filter(data_cut, biome %in% c("Savanna", "Rainforest")))
summary(mod2)
model_parameters(mod2)
model_parameters(anova(mod2))
plot(resid(mod2))
qqnorm(resid(mod2))

# Does type of wood (native or pine) or presence of termites predict methane production?
mod3<-lm(CH4_tr~as.factor(experiment) + as.factor(ter_in_block), data = filter(data_cut, site %in% c("DRO", "PNW")))
summary(mod3)
model_parameters(mod3)
model_parameters(anova(mod3))
plot(resid(mod3))
qqnorm(resid(mod3))

