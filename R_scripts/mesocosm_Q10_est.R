library(rTPC)
library(nls.multstart)
library(broom)
library(tidyverse)
library(ggrepel)
library(readxl)
library(ggplot2)
library(ggpubr)

# read in file 'mesocsom_Q10_processed.csv' #
gases_out<- read_csv(file.choose(""))

gases_out%>%
  filter(!(temp_id %in% c("amb", "cold-14")))%>%
  mutate(meth_hr = methane *3600)%>% # ug CH4 per g of md per hr
  mutate(resp_hr = resp*3600)%>% #ug CO2 per g of md per hr
  mutate(air_temp = as.numeric(temp_id))->gases_out

names(gases_out)
str(gases_out)
range(gases_out$meth_hr)
range(gases_out$resp_hr)
head(gases_out)

gases_out%>%
  rename("temp" = "air_temp",
         "rate" = "methane")%>%
  select(c("temp", "rate"))->gases_out2


## THERMAL RESPONSE FOR METHANE PRODUCTION ###

# compare models (choose models that use optimum temp in the function)
d_fits <- nest(gases_out2, data = c(temp, rate)) %>%
  mutate(joehnk = map(data, ~nls_multstart(rate~joehnk_2008(temp = temp, rmax, topt, a, b, c),
                                           data = .x,
                                           iter = 500,
                                           start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'joehnk_2008'),
                                           start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'joehnk_2008'),
                                           lower = get_lower_lims(.x$temp, .x$rate, model_name = 'joehnk_2008'),
                                           upper = get_upper_lims(.x$temp, .x$rate, model_name = 'joehnk_2008'),
                                           convergence_count = FALSE)),
         modifiedgaussian = map(data, ~nls_multstart(rate~modifiedgaussian_2006(temp = temp, rmax, topt, a, b),
                                                     data = .x,
                                                     iter = 500,
                                                     start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'modifiedgaussian_2006'),
                                                     start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'modifiedgaussian_2006'),
                                                     lower = get_lower_lims(.x$temp, .x$rate, model_name = 'modifiedgaussian_2006'),
                                                     upper = get_upper_lims(.x$temp, .x$rate, model_name = 'modifiedgaussian_2006'),
                                                     convergence_count = FALSE)),
         oneill = map(data, ~nls_multstart(rate~oneill_1972(temp = temp, rmax, ctmax, topt, q10),
                                           data = .x,
                                           iter = 500,
                                           start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'oneill_1972'),
                                           start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'oneill_1972'),
                                           lower = get_lower_lims(.x$temp, .x$rate, model_name = 'oneill_1972'),
                                           upper = get_upper_lims(.x$temp, .x$rate, model_name = 'oneill_1972'),
                                           convergence_count = FALSE)),
         thomas1 = map(data, ~nls_multstart(rate~thomas_2012(temp = temp, a,b,c,tref),
                                            data = .x,
                                            iter = 500,
                                            start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'thomas_2012'),
                                            start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'thomas_2012'),
                                            lower = get_lower_lims(.x$temp, .x$rate, model_name = 'thomas_2012'),
                                            upper = get_upper_lims(.x$temp, .x$rate, model_name = 'thomas_2012'),
                                            convergence_count = FALSE)),
         thomas2 = map(data, ~nls_multstart(rate~thomas_2017(temp = temp, a,b,c,d,e),
                                            data = .x,
                                            iter = 500,
                                            start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'thomas_2017'),
                                            start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'thomas_2017'),
                                            lower = get_lower_lims(.x$temp, .x$rate, model_name = 'thomas_2017'),
                                            upper = get_upper_lims(.x$temp, .x$rate, model_name = 'thomas_2017'),
                                            convergence_count = FALSE)))

#stack models
d_stack <- select(d_fits, -data) %>%
  pivot_longer(., names_to = 'model_name', values_to = 'fit', joehnk:thomas2)

# get parameters using tidy
params <- d_stack %>%
  mutate(., est = map(fit, tidy)) %>%
  select(-fit) %>%
  unnest(est)

# get predictions using augment
newdata <- tibble(temp = seq(min(gases_out2$temp), max(gases_out2$temp), length.out = 100))

d_preds <- d_stack %>%
  mutate(., preds = map(fit, augment, newdata = newdata)) %>%
  select(-fit) %>%
  unnest(preds)


# plot
label_facets_num <- function(string){
  len <- length(string)
  string = paste('(', 1:len, ') ', string, sep = '')
  return(string)
}

ggplot(d_preds, aes(temp, rate)) +
  geom_point(aes(temp, rate), gases_out2) +
  geom_line(aes(temp, .fitted), col = 'blue') +
  facet_wrap(~model_name, labeller = labeller(model_name = label_facets_num), scales = 'free', ncol = 5) +
  theme_bw(base_size = 12) +
  theme(legend.position = 'none',
        strip.text = element_text(hjust = 0),
        strip.background = element_blank()) +
  labs(x = 'Temperature (ºC)',
       y = 'Metabolic rate',
       title = 'Fits of every model available in rTPC') +
  geom_hline(aes(yintercept = 0), linetype = 2)

d_ic <- d_stack %>%
  mutate(., info = map(fit, glance),
         AICc =  map_dbl(fit, MuMIn::AICc),
         q10 = map(fit, get_q10)) %>%
  select(-fit) %>%
  unnest(info) %>%
  unnest(q10)%>%
  select(model_name, sigma, AIC, AICc, BIC, df.residual, q10)

d_ic

# filter for best model
best_model = filter(d_ic, AICc == min(AICc)) %>% pull(model_name)
best_model

# get colour code
col_best_mod = RColorBrewer::brewer.pal(n = 6, name = "Dark2")[6]

# plot
Fig.5a<-ggplot(d_preds, aes(temp, .fitted)) +
  geom_line(aes(group = model_name), col = 'grey20', alpha = 0.6, linetype = "longdash")+
  geom_line(data = filter(d_preds, model_name == best_model), 
            col = col_best_mod, linewidth = 0.8) +
  geom_point(aes(temp, rate), gases_out2) +
  annotate(geom="label", x=18, y=10e-06, label="Q10 = 2.09",
           color="black", size =5)+
  theme_bw(base_size = 18) +
  theme(legend.position = 'none') +
  ylab(expression("Methane production ("*mu*"g "*CH[4]*" "*g[mound]^-1*" "*s^-1*")"))+
  xlab(expression("Temperature ("*degree*"C)"))+
  ggtitle('(a)')

Fig.5a

## THERMAL RESPONSE FOR RESPIRATION ###

gases_out%>%
  rename("temp" = "air_temp",
         "rate" = "resp")%>%
  select(c("temp", "rate"))->gases_out3

# compare models (choose models that use optimum temp in the function)
d_fits <- nest(gases_out3, data = c(temp, rate)) %>%
  mutate(joehnk = map(data, ~nls_multstart(rate~joehnk_2008(temp = temp, rmax, topt, a, b, c),
                                           data = .x,
                                           iter = 500,
                                           start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'joehnk_2008'),
                                           start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'joehnk_2008'),
                                           convergence_count = FALSE)),
         modifiedgaussian = map(data, ~nls_multstart(rate~modifiedgaussian_2006(temp = temp, rmax, topt, a, b),
                                                     data = .x,
                                                     iter = 500,
                                                     start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'modifiedgaussian_2006'),
                                                     start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'modifiedgaussian_2006'),
                                                     convergence_count = FALSE)),
         oneill = map(data, ~nls_multstart(rate~oneill_1972(temp = temp, rmax, ctmax, topt, q10),
                                           data = .x,
                                           iter = 500,
                                           start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'oneill_1972'),
                                           start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'oneill_1972'),
                                           lower = get_lower_lims(.x$temp, .x$rate, model_name = 'oneill_1972'),
                                           upper = get_upper_lims(.x$temp, .x$rate, model_name = 'oneill_1972'),
                                           convergence_count = FALSE)),
         thomas1 = map(data, ~nls_multstart(rate~thomas_2012(temp = temp, a,b,c,tref),
                                            data = .x,
                                            iter = 500,
                                            start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'thomas_2012'),
                                            start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'thomas_2012'),
                                            lower = get_lower_lims(.x$temp, .x$rate, model_name = 'thomas_2012'),
                                            upper = get_upper_lims(.x$temp, .x$rate, model_name = 'thomas_2012'),
                                            convergence_count = FALSE)),
         thomas2 = map(data, ~nls_multstart(rate~thomas_2017(temp = temp, a,b,c,d,e),
                                            data = .x,
                                            iter = 500,
                                            start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'thomas_2017'),
                                            start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'thomas_2017'),
                                            convergence_count = FALSE)))
#stack models
d_stack <- select(d_fits, -data) %>%
  pivot_longer(., names_to = 'model_name', values_to = 'fit', joehnk:thomas2)

# get parameters using tidy
params <- d_stack %>%
  mutate(., est = map(fit, tidy)) %>%
  select(-fit) %>%
  unnest(est)

# get predictions using augment
newdata <- tibble(temp = seq(min(gases_out3$temp), max(gases_out3$temp), length.out = 100))

d_preds <- d_stack %>%
  mutate(., preds = map(fit, augment, newdata = newdata)) %>%
  select(-fit) %>%
  unnest(preds)


# plot
ggplot(d_preds, aes(temp, rate)) +
  geom_point(aes(temp, rate), gases_out3) +
  geom_line(aes(temp, .fitted), col = 'blue') +
  facet_wrap(~model_name, labeller = labeller(model_name = label_facets_num), scales = 'free', ncol = 5) +
  theme_bw(base_size = 12) +
  theme(legend.position = 'none',
        strip.text = element_text(hjust = 0),
        strip.background = element_blank()) +
  labs(x = 'Temperature (ºC)',
       y = 'Metabolic rate',
       title = 'Fits of every model available in rTPC') +
  geom_hline(aes(yintercept = 0), linetype = 2)

d_ic <- d_stack %>%
  mutate(., info = map(fit, glance),
         AICc =  map_dbl(fit, MuMIn::AICc),
         q10 = map(fit, get_q10))%>%
  select(-fit) %>%
  unnest(info) %>%
  unnest(q10)%>%
  select(model_name, sigma, AIC, AICc, BIC, df.residual, q10)

d_ic

# filter for best model
best_model = filter(d_ic, model_name == "thomas1")%>% pull(model_name)
best_model

# get colour code
col_best_mod = RColorBrewer::brewer.pal(n = 6, name = "Dark2")[6]

# plot
Fig.5b<-ggplot(d_preds, aes(temp, .fitted)) +
  geom_line(aes(group = model_name), col = "grey20", alpha = 0.6, linetype = "longdash") +
  geom_line(data = filter(d_preds, model_name == best_model), col = col_best_mod, linewidth = 0.8) +
  geom_point(aes(temp, rate), gases_out3) +
  annotate(geom="label", x=18, y=0.0077, label="Q10 = 1.83",
           color="black", size = 5)+
  theme_bw(base_size = 18) +
  theme(legend.position = 'none') +
  ylab(expression("Respiration rate ("*mu*"g "*CO[2]*" "*g[mound]^-1*" "*s^-1*")"))+
  xlab(expression("Temperature ("*degree*"C)"))+
  ggtitle("(b)")

Fig.5b

Fig.5<-ggarrange(Fig.5a, Fig.5b,
                     ncol = 2, nrow = 1)

Fig.5

png("Fig.5.png",
    width=400, height=200, units = "mm", res = 300)
Fig.5
dev.off()
