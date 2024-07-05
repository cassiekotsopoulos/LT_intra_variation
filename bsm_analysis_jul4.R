
##Main analysis for lake trout intraspecific variation manuscript
##By: Cassandra Kotsopoulos
##Date: July 4/ 2024


#set wd
setwd("~/Desktop/BSM")

#load packages 
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(car)

#read in the data
reg_df <- read.csv ("bsm_analysis_dat.csv")

##run regression models for trophic position range, littoral coupling range, SEAc and SDNND

#Trophic position range (dY range)
tpr_mlr1 <- lm(log_dY_range ~ log_area_ha + summer_air_temp, data = reg_df)

summary(tpr_mlr1)


#make partial regression plots for TP range 
#model with just area
model_area_tp<- lm(log_dY_range ~ log_area_ha, data = reg_df)
reg_df$tp_area_residuals <- residuals(model_area_tp) 

#model with just temp
model_temp_tp <- lm(log_dY_range ~ summer_air_temp, data = reg_df)
reg_df$temp_residuals_tp <- residuals(model_temp_tp)

#temp against area
model_temp_against_area <- lm(summer_air_temp ~ log_area_ha, data = reg_df)
reg_df$temp_against_area_residuals <- residuals(model_temp_against_area) #

#model area against temp
model_area_against_temp <- lm(log_area_ha ~ summer_air_temp, data = reg_df)
reg_df$area_against_temp_residuals <- residuals(model_area_against_temp) 

##Area vs tp plot
TP_area_plot <- ggplot(reg_df, aes(x = area_against_temp_residuals, y = temp_residuals_tp)) +
  geom_point() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme_classic() +
  theme(axis.text = element_text(size = 16))+
  xlab("Lake Area (log(ha))") +
  ylab("Trophic Position Range (log)")
TP_area_plot

#temp vs tp plot
TP_temp_plot <- ggplot(reg_df, aes(x = temp_against_area_residuals, y = tp_area_residuals)) +
  geom_point() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme_classic() +
  theme(axis.text = element_text(size = 16))+
  xlab("Summer Air Temperature (C)") +
  ylab("Trophic Posiiton Range (log)")
TP_temp_plot



#Littoral coupling range (dX range) 
lc_mlr1 <- lm(log_dX_range ~ log_area_ha + summer_air_temp, data = reg_df)

summary(lc_mlr1)


#Partial regression plots

#model with just area
model_area_lc <- lm(log_dX_range ~ log_area_ha, data = reg_df)
reg_df$lc_area_residuals <- residuals(model_area_lc) 

#model with just temp
model_temp <- lm(log_dX_range ~ summer_air_temp, data = reg_df)
reg_df$temp_residuals <- residuals(model_temp)

#temp against area
model_temp_against_area <- lm(summer_air_temp ~ log_area_ha, data = reg_df)
reg_df$temp_against_area_residuals <- residuals(model_temp_against_area) #

#model area against temp
model_area_against_temp <- lm(log_area_ha ~ summer_air_temp, data = reg_df)
reg_df$area_against_temp_residuals <- residuals(model_area_against_temp) 


##LC~area
LC_area_plot2 <- ggplot(reg_df, aes(x = area_against_temp_residuals, y = temp_residuals)) +
  geom_point() + geom_smooth(method = 'lm', color = '#e76254') +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme_classic() +
  theme(axis.text = element_text(size = 16))+
  xlab("Lake Area (log(ha))") +
  ylab("Littoral Coupling Range (log)")
LC_area_plot2


#LC~temp
LC_temp_plot2 <- ggplot(reg_df, aes(x = temp_against_area_residuals, y = lc_area_residuals)) +
  geom_point() + geom_smooth(method = 'lm', color = '#e76254') +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme_classic() +
  theme(axis.text = element_text(size = 16))+
  xlab("Summer Air Temperature (C)") +
  ylab("Littoral Coupling Range (log)")
LC_temp_plot2





#SEAc - standard ellipse area
seac_mlr1 <- lm(log_SEAc ~ log_area_ha + summer_air_temp, data = reg_df)

summary(seac_mlr1)

###partial regression plots 
#model with just area
model_area_seac <- lm(log_SEAc ~ log_area_ha, data = reg_df)
reg_df$seac_area_residuals <- residuals(model_area_seac) 

#model with just temp
model_temp_seac <- lm(log_SEAc ~ summer_air_temp, data = reg_df)
reg_df$seac_temp_residuals <- residuals(model_temp_seac)

#temp against area
model_temp_against_area <- lm(summer_air_temp ~ log_area_ha, data = reg_df)
reg_df$temp_against_area_residuals <- residuals(model_temp_against_area) #

#model area against temp
model_area_against_temp <- lm(log_area_ha ~ summer_air_temp, data = reg_df)
reg_df$area_against_temp_residuals <- residuals(model_area_against_temp) 

#area vs seac
seac_area_plot <- ggplot(reg_df, aes(x = area_against_temp_residuals, y = seac_temp_residuals)) +
  geom_point() + geom_smooth(method = 'lm', color = '#e76254')  +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme_classic() +
  theme(axis.text = element_text(size = 16))+
  xlab("Lake Area (log(ha))") +
  ylab("SEAc (log)")
seac_area_plot


#seac vs temp plot
seac_temp_plot <- ggplot(reg_df, aes(x = temp_against_area_residuals, y = seac_area_residuals)) +
  geom_point() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme_classic() +
  theme(axis.text = element_text(size = 16))+
  xlab(" Summer Air Temp (C)") +
  ylab("SEAc (log)")


seac_temp_plot


#SDNND - standard deviation of nearest neighbor distance
sdnnd_mlr1 <- lm(log_SDNND ~ log_area_ha + summer_air_temp, data = reg_df)

summary(sdnnd_mlr1)


###partial plots for SDNND 
#model with just area
model_area_sdnnd <- lm(log_SDNND ~ log_area_ha, data = reg_df)
reg_df$sdnnd_area_residuals <- residuals(model_area_sdnnd) 

#model with just temp
model_temp_sdnnd <- lm(log_SDNND ~ summer_air_temp, data = reg_df)
reg_df$sdnnd_temp_residuals <- residuals(model_temp_sdnnd)

#temp against area
model_temp_against_area <- lm(summer_air_temp ~ log_area_ha, data = reg_df)
reg_df$temp_against_area_residuals <- residuals(model_temp_against_area) #

#model area against temp
model_area_against_temp <- lm(log_area_ha ~ summer_air_temp, data = reg_df)
reg_df$area_against_temp_residuals <- residuals(model_area_against_temp) 

#area vs sdnnd
sdnnd_area_plot <- ggplot(reg_df, aes(x = area_against_temp_residuals, y = sdnnd_temp_residuals)) +
  geom_point() + geom_smooth(method = 'lm', color = '#e76254') +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme_classic() +
  theme(axis.text = element_text(size = 16))+
  xlab("Lake Area (log(ha))") +
  ylab("SDNND(log)")
sdnnd_area_plot


#sdnnd vs temp plot
sdnnd_temp_plot <- ggplot(reg_df, aes(x = temp_against_area_residuals, y = sdnnd_area_residuals)) +
  geom_point() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme_classic() +
  theme(axis.text = element_text(size = 16))+
  xlab(" Summer Air Temp (C)") +
  ylab("SDNND (log)")


sdnnd_temp_plot


