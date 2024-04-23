library(tidyverse)
library(broom)
library(lubridate)
library(googledrive)
library(readxl)
library(viridis)
library(ggpubr)

#read in data file 'mesocosm_Q10_raw.csv'

termite_resp <- read_csv(file.choose(""))
termite_resp$date_id <- as.character(termite_resp$date_id)

# Deal with NA values and cut date frame

ii <- is.na(termite_resp$Etime)
termite_resp$Etime[ii] <- -9999

termite_resp %>%
  filter(!is.na(date_id))%>%
  filter(Etime >=0 & Etime<=170)%>%
  mutate(Time_diff = Time - lag(Time)) -> cut_termite_resp

# Get estimates of CH4 production rate from slope

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

# CH4 estimates

cut_termite_resp %>%
  nest(data= c(-date_id, -temp_id, -lvl))%>%
  mutate(fit = map(data, ~ lm(CH4d_ppm ~ Etime, data = .)), 
         results = map(fit, glance)) %>%
  unnest(results) %>%
  select(date_id, temp_id,lvl, r.squared )-> lm_out

cut_termite_resp %>%
  nest(data= c(-date_id, -temp_id, -lvl)) %>%
  mutate(fit = map(data, ~ lm(CH4d_ppm ~ Etime, data = .)), 
         results = map(fit, tidy)) %>%
  unnest(results) %>%
  filter(term=="Etime")%>%
  select(date_id, temp_id, lvl, estimate)-> slope

left_join(lm_out, slope)  -> delta_methane

delta_methane%>%
  filter(!(temp_id == "17.5" & lvl == 6))%>%
  filter(!(is.na(estimate)))%>%
  print(n=Inf)->delta_methane

# CO2 estimates

cut_termite_resp %>%
  nest(data= c(-date_id, -temp_id,  -lvl))%>%
  mutate(fit = map(data, ~ lm(CO2d_ppm ~ Etime, data = .)), 
         results = map(fit, glance)) %>%
  unnest(results) %>%
  select(date_id, temp_id,lvl, r.squared )-> lm_out

cut_termite_resp %>%
  nest(data=c(-date_id, -temp_id, -lvl))%>%
  mutate(fit = map(data, ~ lm(CO2d_ppm ~ Etime, data = .)), 
         results = map(fit, tidy)) %>%
  unnest(results) %>%
  filter(term=="Etime")%>%
  select(date_id, temp_id,lvl, estimate)-> slope

left_join(lm_out, slope)  -> delta_resp


delta_resp%>%
  filter(!(temp_id == "17.5" & lvl == 6))%>%
  filter(!(is.na(estimate)))%>%
  print(n=Inf)->delta_resp

# join gas estimates with temperature of chamber
cut_termite_resp %>%
  mutate(site = gsub(pattern = "_NA",replacement = "",x = date_id), 
         date = lubridate::date(cut_termite_resp$Time),
         GasP_kpa=GasP_torr*0.133322368)%>%
  group_by(date_id, temp_id, lvl, date) %>%
  summarise(Tcham=mean(Tcham, na.rm = TRUE), Cell_pressure = mean(GasP_kpa, na.rm = TRUE,)) -> t_cham #torr to kpa for pressure in the optical path

t_cham%>%
  filter(!(temp_id == "17.5" & lvl == 6))->t_cham

# methane estimates
methane <- left_join(delta_methane, t_cham, by = c("date_id","temp_id", "lvl"))
methane$date <- lubridate::as_date(methane$date)

# resp estimates
resp <- left_join(delta_resp, t_cham, by = c("date_id","temp_id", "lvl"))
resp$date <- lubridate::as_date(resp$date)


#join CO2 and methane estimates to one data frame
resp%>%
  rename(CO2.rsq = r.squared,
         CO2.est = estimate)->resp

methane%>%
  rename(CH4.rsq = r.squared,
         CH4.est = estimate)->methane

both_gases<-left_join(resp, methane)

######### Calculate flux rates based on formula by Dossa et al. 2015? ##########

# Temp conversion from degrees celsius to kelvin: T = Tc + Ti; Ti = 273.15
# Pi (standard air pressure in KPa) = 101.325
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

# calculating total volume (chamber and mesocosm) in cm^3 and then converted to m^3

Offset <- 0.20*(pi*(0.14^2))# 
#this the volume of the chamber from cm3 to m3
Vchamber <- 4076.2/1000000
#bucket volume in m3
Vbucket <- 0.41*(pi*(0.14^2))
Vtotal <- (Vbucket-Offset)+Vchamber

# calculate rate of respiration

respiration_rate<-function(deltaCO2,P,Tc,Vc,Vs,Ws){ 
  MCO2<-44.01# molar mass of CO2 
  Ti<-273.15# absolute temperature in Kelvin
  Pi<-101.325# atmospheric pressure at standard conditions
  Vi<-22.41# air molar volume at standard conditions
  
  resp <-deltaCO2*MCO2*P*(Pi^-1)*(Vc-Vs)*(Vi^-1)*Ti*((Ti+Tc)^-1)*Ws^-1
  return(resp)
}


methane_rate<-function(deltaCH4,P,Tc,Vc,Vs,Ws){ 
  MCH4<-16.04# molar mass of CH4 
  Ti<-273.15# absolute temperature in Kelvin
  Pi<-101.325# atmospheric pressure at standard conditions
  Vi<-22.41# air molar volume at standard conditions
  
  methane <-deltaCH4*MCH4*P*(Pi^-1)*(Vc-Vs)*(Vi^-1)*Ti*((Ti+Tc)^-1)*Ws^-1
  return(methane)
}

both_gases %>%
  mutate(resp = respiration_rate(deltaCO2 = CO2.est, P = Cell_pressure,Tc =Tcham ,Vc =Vtotal ,Vs = 0.0024, Ws = 2.495),
         date_since_start =as.numeric(date-lubridate::as_date("2019/04/30"))) %>%
  mutate(methane = methane_rate(deltaCH4 = CH4.est,P = Cell_pressure,Tc =Tcham ,Vc =Vtotal ,Vs = 0.0024, Ws = 2.495 ),
         date_since_start =as.numeric(date-lubridate::as_date("2019/04/30"))) ->gases_out

gases_out$date_since_start[gases_out$date_since_start==101] <- 106

gases_out %>%
  group_by(temp_id) %>%
  filter(!is.na(CO2.est))%>%
  filter(!is.na(CH4.est))%>%
  print(Inf)-> gases_out

gases_out%>%
  select(date, temp_id, Tcham, resp, methane)->gases_out

write_csv(x = gases_out, file = "mesocosm_Q10_processed.csv")

