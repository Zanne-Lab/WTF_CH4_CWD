library(tidyverse)
library(broom)
library(lubridate)
library(readxl)
library(viridis)

# read in file 'mesocosm_gas_data_raw.csv'

termite_resp <- read_csv(file.choose(""))
termite_resp$site <- as.character(termite_resp$site)

# Dealing with NAs in data 
# Following have NA as site_id
# 2019-06-11 has IRGA data but no chamber data
# 2019-10-29 has both IRGA and chamber data but no matching time
# 4732-09-07 / 2061-02-09 two random dates, single rows only

# Deal with NA values and cut date frame

ii <- is.na(termite_resp$Etime)
termite_resp$Etime[ii] <- -9999

termite_resp %>%
  filter(!is.na(site) & Etime >=0) %>%
  filter(Etime >=0 & Etime<=170)%>%# Filter out one minute calibration time and last 10 seconds in case of chamber/IRGA time offset
  mutate(Time_diff = Time - lag(Time)) -> cut_termite_resp

# Get estimates of CO2 and CH4 production rate from slope

#make a flag for when measurement are happening (S)
cut_termite_resp$flag <- ifelse(cut_termite_resp$Time_diff > 60 | is.na(cut_termite_resp$Time_diff), yes = "S", no = "M")

#column for row id
cut_termite_resp$flag_id <- rownames(cut_termite_resp)

#which rows have measurements
s_num <- which(cut_termite_resp$flag=="S",arr.ind = TRUE) 

#generate vector which length is equal to where measurements (S) start in cut_termite_resp data 
long <- c(1:length(which(cut_termite_resp$flag=="S")))

site_length <- length(long)
s <- sapply(1:site_length, function(i){ s_num <- c(s_num, nrow(cut_termite_resp)+1) ; r <- s_num[i+1]-s_num[i]},simplify = TRUE)
sum(s,na.rm = TRUE)
s_out <- rep(long,s)

cut_termite_resp$lvl <-s_out

# visualise slopes for each mound on each date

# CO2 production
cut_termite_resp%>%
  mutate(date = lubridate::date(cut_termite_resp$Time))%>%
  ggplot(aes(x= Etime,y = CO2d_ppm, group = date, colour = factor(date)))+
  geom_line(aes(x= Etime,y = CO2d_ppm, group =date, colour = factor(date)))+
  facet_wrap(~c(tag))

# CH4 production
cut_termite_resp%>%
  mutate(date = lubridate::date(cut_termite_resp$Time))%>%
  ggplot(aes(x= Etime,y = CH4d_ppm, group = date, colour = factor(date)))+
  geom_line(aes(x= Etime,y = CH4d_ppm, group =date, colour = factor(date)))+
  facet_wrap(~c(tag))


# CO2 estimates

cut_termite_resp %>%
  nest(data=c(-site, -tag, -lvl))%>%
  mutate(fit = map(data, ~ lm(CO2d_ppm ~ Etime, data = .)), 
         results = map(fit, glance)) %>%
  unnest(results) %>%
  select(site, tag,lvl, r.squared )-> lm_out

cut_termite_resp %>%
  nest(data=c(-site, -tag, -lvl)) %>%
  mutate(fit = map(data, ~ lm(CO2d_ppm ~ Etime, data = .)), 
         results = map(fit, tidy)) %>%
  unnest(results) %>%
  filter(term=="Etime")%>%
  select(site, tag,lvl, estimate)-> slope

left_join(lm_out, slope)  -> delta_resp

# CH4 estimates

cut_termite_resp %>%
  nest(data=c(-site, -tag, -lvl))%>%
  mutate(fit = map(data, ~ lm(CH4d_ppm ~ Etime, data = .)), 
         results = map(fit, glance)) %>%
  unnest(results) %>%
  select(site, tag,lvl, r.squared )-> lm_out

cut_termite_resp %>%
  nest(data= c(-site, -tag, -lvl)) %>%
  mutate(fit = map(data, ~ lm(CH4d_ppm ~ Etime, data = .)), 
         results = map(fit, tidy)) %>%
  unnest(results) %>%
  filter(term=="Etime")%>%
  select(site, tag, lvl, estimate)-> slope


left_join(lm_out, slope)  -> delta_methane

# join gas estimates with temperature of chamber
cut_termite_resp %>%
  mutate(site = gsub(pattern = "_NA",replacement = "",x = site), 
         date = lubridate::date(cut_termite_resp$Time),
         GasP_kpa=GasP_torr*0.133322368)%>%
  group_by(site, tag, lvl, date) %>%
  summarise(Tcham=mean(Tcham, na.rm = TRUE), Cell_pressure = mean(GasP_kpa, na.rm = TRUE,)) -> t_cham #torr to kpa for pressure in the optical path

# resp estimates
resp <- left_join(delta_resp, t_cham, by = c("site","tag", "lvl"))
resp$date <- lubridate::as_date(resp$date)


# methane estimates
methane <- left_join(delta_methane, t_cham, by = c("site","tag", "lvl"))
methane$date <- lubridate::as_date(methane$date)

#join CO2 and methane estimates to one data frame
resp%>%
  rename(CO2.rsq = r.squared,
         CO2.est = estimate)->resp

methane%>%
  rename(CH4.rsq = r.squared,
         CH4.est = estimate)->methane

both_gases<-left_join(resp, methane)

# read in file 'mesocosm_weights.csv' to get weight and volume data of mounds

wt_dry<-read.csv(file.choose(""))
wt_dry%>%
  mutate(measurement_date = as.POSIXct(measurement_date, format = "%d/%m/%Y"))->wt_dry

#calculate volume to weight relationship assuming a paraboloid from 10 weight measurements at start 10 existing weight measurements have

wt_dry %>%
  mutate(mound_id = tolower(mound_id),
         volume_cm3=(1/2*pi*(1/2*maximum_diameter)^2*total_ht))->wt_dry

summary(lm(mound_wt~volume_cm3, wt_dry))

lm_eqn <- function(wt_dry){
  m <- lm(mound_wt ~ volume_cm3, wt_dry);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

Fig.S3.1<-ggplot(wt_dry, aes(x= volume_cm3, y =mound_wt ))+
  geom_point()+
  geom_smooth(method = "lm")+
  xlab(expression("Mound volume ("*cm^-3*")"))+
  ylab("Mound weight (kg)")+
  geom_text(x = 1000, y = 5, label = lm_eqn(wt_dry), parse = TRUE)+
  theme_bw(base_size=16)

Fig.S3.1

png("Fig.S3.1.png",
    width=250, height=150, units = "mm", res = 300)
Fig.S3.1
dev.off()

# calculate an estimated weight for all volume measurements using parameters from model
wt_dry %>%
  mutate(mound_id = tolower(mound_id),
         volume_cm3=(1/2*pi*(1/2*maximum_diameter)^2*total_ht),
         wt_calculated_g= (0.4717212+0.0008413*volume_cm3)*1000) ->wt_dry

#cm3 to m3

wt_dry %>%
  mutate(volume_m3 = volume_cm3/1000000, # convert volume from cm3 to m3
         measurement_date = lubridate::as_date(measurement_date),
         wt_calculated_kg =(wt_calculated_g/1000))  %>% # convert weight from g to kg
  select(mound_id, measurement_date,sand_wt, wt_calculated_kg,volume_m3)-> wt_out

wt_out$total_wt <- apply(wt_out[,c("sand_wt", "wt_calculated_kg")], 1, sum, na.rm =TRUE)

both_gases <- left_join(both_gases, wt_out,by = c("tag"="mound_id", "date" = "measurement_date"))

ii <- both_gases$tag %in% c("nc1","nc2")
both_gases$volume_m3[ii] <- 0.0000

########## Calculate flux rates based on formula by Dossa et al. 2015 (https://doi.org/10.1111/2041-210X.12451) ##########

# read in file 'mesocosm.offsets.csv'

offset <- read.csv(file.choose(""))

offset<-offset%>%
  mutate(tag = tolower(mound_id))%>%
  select(c(tag, offset))

# calculating total volume (chamber and mesocosm) in cm^3 and then converted to m^3
#this the volume of the chamber from cm3 to m3
Vchamber <- 4076.2/1000000
#bucket volume in m3
Vbucket <- 0.41*(pi*(0.14^2))

# Include specific offset for each mesocosm ##
both_gases<-left_join(both_gases, offset)%>%
  mutate(offset= replace_na(offset, 20))%>%
  mutate(offset.tr = (offset/100)*(pi*(0.14^2)))%>% 
  mutate(Vtotal = (Vbucket-offset.tr)+Vchamber)

# Temp conversion from degrees celsius to kelvin: T = Tc + Ti; Ti = 273.15
# Pi (standard air pressure in kPa) = 101.325
# Vi (standard molar volume of air in L mol-1) = 22.41
# MCO2 (molar mass CO2 in g mol-1) = 44.01
# MCH4 (molar mass CH4 in g mol-1) = 16.04
# R Universal gas constant in J K-1 mol -1

MCO2 = 44.01
MCH4 = 16.04
Pi  <- 101.325
Vi  <- 22.41
Ti <- 273.15
R <- 8.314 

# calculate respiration rate and rate of methane production

respiration_rate<-function(deltaCO2,P,Tc,Vc,Vs,Ws){ 
  MCO2<-44.01# molar mass of CO2 
  Ti<-273.15# absolute temperature in Kelvin
  Pi<-101.325# atmospheric pressure at standard conditions
  Vi<-22.41# air molar volume at standard conditions
  
  resp <-deltaCO2*MCO2*(P*(Pi^-1))*((Vc-Vs)*(Vi^-1))*(Ti*((Ti+Tc)^-1))*(Ws^-1)
  return(resp)
}

methane_rate<-function(deltaCH4,P,Tc,Vc,Vs,Ws){ 
  MCH4<-16.04# molar mass of CH4 
  Ti<-273.15# absolute temperature in Kelvin
  Pi<-101.325# atmospheric pressure at mbar
  Vi<-22.41# air molar volume at standard conditions
  
  methane <-deltaCH4*MCH4*(P*(Pi^-1))*((Vc-Vs)*(Vi^-1))*(Ti*((Ti+Tc)^-1))*Ws^-1
  return(methane)
}

both_gases %>%
  mutate(resp = respiration_rate(deltaCO2 = CO2.est, P = Cell_pressure,Tc =Tcham ,Vc =Vtotal ,Vs = volume_m3,Ws =total_wt ),
         date_since_start =as.numeric(date-lubridate::as_date("2019/04/30"))) %>%
  mutate(methane = methane_rate(deltaCH4 = CH4.est,P = Cell_pressure,Tc =Tcham ,Vc =Vtotal ,Vs = volume_m3,Ws =total_wt ),
         date_since_start =as.numeric(date-lubridate::as_date("2019/04/30")))->gases_out

gases_out[gases_out$tag=="tm2",c("tag", "CO2.est", "CH4.est", "resp", "methane")]
gases_out$date_since_start[gases_out$date_since_start==101] <- 106

gases_out %>%
  group_by(tag) %>%
  mutate(avg_wt = mean(wt_calculated_kg, na.rm = TRUE)) %>%
  filter(!is.na(CO2.est))%>%
  filter(!is.na(CH4.est))-> gases_out

# read in wood trait data from file 'mesocosm_wood_traits.csv'.

wood_data <- read.csv(file.choose(""))

wood_data%>%
  rename_with(~ gsub(paste(c("[(]", "[%]", "[)]"), collapse = "|"), "", .x))%>%
  select(1:11)%>%
  rename("mean_C" = "mean_C_.",
         "mean_N" = "mean_N_.")-> wood_data

gases_wood<-left_join(gases_out, wood_data, by = c("tag"="mound_id"))

# add column of CH4:CO2 ratio and season

mesocosm_processed<-gases_wood%>%
  mutate(season = case_when(
    month(date) %in% c(1:5, 12)   ~ "WET",
    month(date) %in% c(6:11) ~ "DRY"))%>%
  select(!("lvl"))%>%
  distinct()

names(mesocosm_processed)
mesocosm_processed%>%
  select(site, tag, date, Tcham, wt_calculated_kg, volume_m3, resp, date_since_start, methane, avg_wt,
         species_code, initial_weight, final_weight, wood_density, mean_C, mean_N, mean_S.G, mean_pH, mean_C.N, season)->mesocosm_processed

write.csv(x = mesocosm_processed, file = "mesocosm_processed.csv", row.names=F)





