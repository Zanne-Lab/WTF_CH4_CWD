library(ggplot2)  
library(png) 
library(readxl)
library(tidyverse)
library(viridis)
library(ggrepel)
library(lme4)
library(lmerTest)
library(ggeffects)
library(MuMIn)
library(parameters)
library(MuMIn)
library(corrplot)
library(car)
library(ggpubr)

#############
# LOAD DATA #
#############

# read in file 'mesocosm_processed.csv'

resp_out<- read_csv(file.choose(""))

# add an ID for each mound that includes wood species fed to each mound
# scale up rates to estimates g-1(mound) h-1  / mound-1 h-1 to compare with literature estimates

resp_out%>%
  mutate(meth_hr = methane*3600, # g-1(mound) h-1
         resp_hr = resp*3600, # g-1(mound) h-1
         CH4_md = meth_hr*(wt_calculated_kg*1000), # mound-1 h-1
         CO2_md = resp_hr*(wt_calculated_kg*1000), # mound-1 h-1
         wood_eaten = initial_weight - final_weight)%>% # total g of wood consumed 
  mutate(ID = case_when(tag == "tm2"  ~ "ALSC.1",
                        tag == "tm4"  ~ "ALSC.2", 
                        tag == "tm3"  ~ "CAAU.1",
                        tag == "tm9"  ~ "CAAU.2",
                        tag == "tm1"  ~  "CASU.1",
                        tag == "tm5"~  "CASU.2",
                        tag == "tm6" ~  "NONO.1",
                        tag == "tm8"  ~ "NONO.2",
                        tag == "tm7"  ~  "SYSA.1",
                        tag == "tm10"  ~ "SYSA.2",
                        TRUE ~ as.character(tag)))%>%
  mutate(month = lubridate::month(date))->resp_out # add in month

resp_out$ID <- factor(resp_out$ID, levels = c("ALSC.1", "ALSC.2", "CAAU.1","CAAU.2", "CASU.1", 
                                              "CASU.2", "NONO.1", "NONO.2", "SYSA.1", "SYSA.2"))

####################
# GROWTH OF MOUNDS #
####################

# what is the size distribution of mounds?

Fig.S3.2 <- ggplot(resp_out, 
                      aes(x= ID, y =wt_calculated_kg,group = interaction(ID, species_code), fill= (species_code)))+
  geom_boxplot()+
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  ylab("Mound weight (kg)")+
  xlab("Termite mounds")+
  labs(fill = "Wood species")+
  ggtitle("")+
  theme_bw(base_size=16)

Fig.S3.2

png("Fig.S3.2.png",
    width=250, height=150, units = "mm", res = 300)
Fig.S3.2
dev.off()


# does mound growth depend on wood type fed?

size.mod<-lmer(wt_calculated_kg~as.factor(species_code)*date_since_start + (1|tag), data = resp_out)
summary(size.mod)
anova(size.mod, type = 2)
plot(resid(size.mod))
qqnorm(resid(size.mod))
model_parameters(anova(size.mod), type = 2)
r.squaredGLMM(size.mod)

Fig.4a <- ggplot(resp_out,
                      aes(x=date_since_start , y =wt_calculated_kg, colour=ID))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  scale_color_viridis(discrete = TRUE, guide = "none")+
  ylab("Mound weight (kg)")+
  xlab("Days of growth")+
  ggtitle("(a) Mound growth")+
  facet_wrap(~species_code)+
  theme_bw(base_size=16)

Fig.4a

########################
# GAS FLUX FROM MOUNDS #
########################

# check to see if resp and methane productions rates vary per month for each mound

Fig.S3.3a<-ggplot(resp_out,aes(x=date_since_start, y =(resp*1000)))+
  geom_point()+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  ylab(expression("Respiration rate ("*mu*"g "*CO[2]*" "*kg^-1*hr^-1*")"))+
  xlab("Days since start")+
  ggtitle(expression("(a) "*CO[2]*" Flux"))+
  facet_wrap(~ID, scales = "free_x")+
  theme_bw(base_size = 16)

print(Fig.S3.3a)

Fig.S3.3b<-ggplot(resp_out,aes(x=date_since_start , y =(meth_hr*1000)))+
  geom_point()+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  ylab(expression("Methane production ("*mu*"g "*CH[4]*" "*kg^-1*hr^-1*")"))+
  xlab("Days since start")+
  ggtitle(expression("(b) "*CH[4]*" Flux"))+
  facet_wrap(~ID, scales = "free_x")+
  theme_bw(base_size =16)

Fig.S3.3b

Fig.S3.3<-ggarrange(Fig.S3.3a, Fig.S3.3b,
                     ncol = 2, nrow = 1)

Fig.S3.3

png("Fig.S3.3.png",
    width=400, height=250, units = "mm", res = 300)
Fig.S3.3
dev.off()

# remove ALSC.2, NONO.2 and CASU.1, smallest mounds with lowest estimates

resp_out%>%
  filter(!(ID %in% c("ALSC.2", "NONO.2", "CASU.1")))->resp_cut # (n=34 down from n =49)

#########

# Calculate mean rates to compare with other literature

# by g-1(mound)h-1
mean(resp_cut$resp_hr)
sd(resp_cut$resp_hr)/sqrt(length(resp_cut$resp_hr))

mean(resp_cut$meth_hr)
sd(resp_cut$meth_hr)/sqrt(length(resp_cut$meth_hr))

# by mound-1 h-1
mean(resp_cut$CO2_md)
sd(resp_cut$CO2_md)/sqrt(length(resp_cut$CO2_md))

mean(resp_cut$CH4_md)
sd(resp_cut$CH4_md)/sqrt(length(resp_cut$CH4_md))

#########

# Q1. does weight of mound, species of wood or temp of chamber influence methane production rate?

# using all mounds in models

all.mod<-lmer(meth_hr~wt_calculated_kg + as.factor(species_code) + Tcham + date_since_start +
                (1|tag), data = resp_out)
summary(all.mod)
anova(all.mod, type = 2)
r.squaredGLMM(all.mod)
model_parameters(anova(all.mod))
plot(resid(all.mod))
qqnorm(resid(all.mod))

# removing smallest mounds

all.mod<-lmer(meth_hr~wt_calculated_kg + as.factor(species_code) + Tcham + date_since_start + (1|tag), data = resp_cut)
summary(all.mod)
anova(all.mod)
r.squaredGLMM(all.mod)
plot(resid(all.mod))
qqnorm(resid(all.mod))
model_parameters(anova(all.mod))

# plot of flux by wood type

Fig.4b <- ggplot(resp_cut, aes(x= species_code, y =(meth_hr*1000),group = interaction(ID, avg_wt), fill= (avg_wt)))+
  geom_boxplot(na.rm = TRUE, width =0.2)+
  ylab(expression("Methane production ("*mu*"g "*CH[4]*" "*kg^-1*hr^-1*")"))+
  xlab("Wood species fed to termite mound")+
  ggtitle("(b) Methane Flux")+
  scale_fill_viridis_c()+
  guides(fill = guide_colorbar(title="Average \nmound weight (kg)"))+
  theme_bw(base_size =16)+
  theme(legend.position =c(0.3,0.85),
        legend.background = element_rect(colour = "transparent", fill = "transparent"))

Fig.4b

# Q2. does methane production vary according to wood traits?
# check which traits correlate wood density and pH
vars1 <- c("wood_density", "mean_C", "mean_N", "mean_S.G", "mean_pH")
corrplot(cor(resp_out[vars1]),order = 'AOE',diag = F,type = 'upper',method = 'number') #pH and wood_density strongly correlated

# with all mound data
all.mod<-lmer(meth_hr~wood_density + mean_C + mean_N + mean_S.G + (1|tag), data = resp_out)
summary(all.mod)
vif(all.mod)
r.squaredGLMM(all.mod)
model_parameters(all.mod)

# without smallest mounds
all.mod<-lmer(meth_hr~wood_density + mean_C + mean_N + mean_S.G + (1|tag), data = resp_cut)
summary(all.mod)
vif(all.mod)
r.squaredGLMM(all.mod)
model_parameters(all.mod)

Fig.S3.4a <- ggplot(resp_cut, 
                       aes(x= wood_density, y =(meth_hr*1000), colour= (species_code)))+
  geom_point(size = 3)+
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  ylab(expression("Methane production ("*mu*"g "*CH[4]*" "*kg^-1*hr^-1*")"))+
  xlab(expression("Wood Density ("*g~cm^-3*")"))+
  ggtitle("(a)")+
  theme_bw(base_size =16)+
  labs(colour = "Wood Species")+
  theme(legend.position = c(0.2, 0.75),
        legend.background = element_rect(colour = "transparent", fill = "transparent"))

Fig.S3.4a

Fig.S3.4b <- ggplot(resp_cut, 
                  aes(x= mean_S.G, y =(meth_hr*1000),colour= (species_code)))+
  geom_point(size = 3)+
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  ylab(expression("Methane production ("*mu*"g "*CH[4]*" "*kg^-1*hr^-1*")"))+
  xlab("S:G ratio")+
  ggtitle("(b)")+
  theme_bw(base_size =16)+
  labs(fill = "Wood Species")+
  theme(legend.position = "none")

Fig.S3.4b

Fig.S3.4c <- ggplot(resp_cut, 
                 aes(x= mean_N, y =(meth_hr*1000),colour = (species_code)))+
  geom_point(size = 3)+
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  ylab(expression("Methane production ("*mu*"g "*CH[4]*" "*kg^-1*hr^-1*")"))+
  xlab("% N")+
  ggtitle("(c)")+
  theme_bw(base_size =16)+
  labs(fill = "Wood Species")+
  theme(legend.position = "none")

Fig.S3.4c

Fig.S3.4d<- ggplot(resp_cut, 
                 aes(x= mean_C, y =(meth_hr*1000),colour = (species_code)))+
  geom_point(size = 3)+
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE, alpha = 0.5) +
  ylab(expression("Methane production ("*mu*"g "*CH[4]*" "*kg^-1*hr^-1*")"))+
  xlab("% C")+
  ggtitle("(d)")+
  theme_bw(base_size =16)+
  labs(fill = "Wood Species")+
  theme(legend.position = "none")

Fig.S3.4d


Fig.S3.4<-ggarrange(Fig.S3.4a, Fig.S3.4b, Fig.S3.4c, Fig.S3.4d,
                        ncol = 2, nrow = 2)

Fig.S3.4

png("Fig.S3.4.png",
    width=300, height=300, units = "mm", res = 300)
Fig.S3.4
dev.off()

######################
# PCA of wood traits #
######################

pca_values <- 
  prcomp(resp_out[vars1], center = TRUE, scale = TRUE)
summary(pca_values)

pca_points <- as_tibble(pca_values$x) %>% 
  bind_cols(resp_out)%>%
  select(tag, PC1, PC2)

resp_pca<- left_join(resp_out,pca_points)

pca_load <- 
  as_tibble(pca_values$rotation, rownames = 'variable') %>% 
  mutate(variable = dplyr::recode(variable,
                                  'wood_density' = 'Density',
                                  'mean_C' = '% C',
                                  'mean_N' = '% N',
                                  'mean_S.G' = 'S:G ratio',
                                  'mean_pH' = 'pH'))

pca_load2<-pca_load%>%
  select(c(variable, PC1, PC2))%>%
  gather(PC1, PC2, key = PC, value = loadings)

Fig.4c <- 
  ggplot(pca_points, aes(x = PC1, y = PC2)) +
  geom_segment(data = pca_load, 
               aes(x = 0, y = 0, 
                   xend = PC1*3.7,
                   yend = PC2*3.7),
               arrow = arrow(length = unit(1/2, 'picas')))+
  annotate('text', x = (pca_load$PC1*4.4), y = (pca_load$PC2*4.1),
           label = pca_load$variable,
           size = 5)+
  geom_text(aes(label = species_code, colour = "red"), nudge_x = -0.3, show.legend = F, data=resp_pca)+
  scale_x_continuous("PC1 (76.0%)", limits=c(-4, 4), breaks=seq(-4, 4, 2))+
  scale_y_continuous("PC2 (13.8%)", limits=c(-3, 3), breaks=seq(-2, 2, 2))+
  labs(colour = "Site", shape = "Site")+
  theme_bw(base_size=16)+
  ggtitle("(c) Wood traits")

Fig.4c


loadings_plot <- pca_load2%>%
  ggplot(aes(x = variable, y = loadings, colour=variable, fill = variable)) +
  geom_bar(stat="identity")+
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE, alpha = 0.5) +
  xlab("Wood traits")+
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  ylab("Factor loadings")+
  facet_wrap(~PC)+
  theme_bw()+
  theme(plot.title = element_text(hjust=0, size=18),
        axis.text.y=element_text(size=16,color="black"),
        axis.text.x=element_text(size=16,color="black"),
        axis.title.y=element_text(size=18),
        axis.title.x=element_text(size=18),
        strip.text = element_text(size=18),
        legend.text = element_text(size=14),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  ggtitle("(b)")

loadings_plot


Fig4<-ggarrange(Fig.4a, Fig.4b, Fig.4c,
                       ncol = 3, nrow = 1)

Fig4

png("Fig4.png",
    width=400, height=200, units = "mm", res = 300)
Fig4
dev.off()







