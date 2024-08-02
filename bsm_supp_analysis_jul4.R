
##Supplementary analysis for lake trout intraspecific variation manuscript
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



#SUPPLEMENTARY ANALYSIS #1
##AICc model selection
#code for tables S.6 - S.9

# load packages
library(tidyverse)
library(MuMIn)
library(olsrr)


# create a variable standardise functions - same as z score
standardise = function(x) (x - mean(x))/sd(x)

# set for aicc dredging
options(na.action = "na.fail")

# read in filter data
d1 = read.csv("bsm_analysis_dat.csv") %>%
  select(Lake_Name, Area_ha, summer_air_temp, SEAc, dY_range, dX_range, SDNND, lt_CUE = LaTro_CUE, total_pred_CUE) %>%
  drop_na() %>%
  mutate(
    log_SEAc = log10(SEAc),
    log_dY_range = log10(dY_range),
    log_dX_range = log10(dX_range),
    log_SDNND = log10(SDNND),
    area_ha_s = standardise(Area_ha),
    summer_air_temp_s = standardise(summer_air_temp),
    total_pred_cue_s = standardise(total_pred_CUE),
    lt_cue_s = standardise(lt_CUE)
  )


# SEAc analysis

# saturated model
mod_seac = lm(log_SEAc ~ area_ha_s + summer_air_temp_s + total_pred_cue_s + lt_cue_s, data = d1)

# aic model selection
dl_seac = dredge(mod_seac, extra = "adjR^2", rank = "AICc")
dl_seac


##put together df for table on lake info for supp.
table_df <- dl_seac



# summary of the model averaged model - note use the conditional averages parameter estimates
summary(model.avg(dl_seac, subset = delta < 2)) #using <delta aicc
summary(model.avg(dl_seac, subset = cumsum(weight) <= .95)) #using 95% weight


##adding in 95% CI
confint(model.avg(dl_seac, subset = cumsum(weight) <= .95))

summary(model.avg(dl_seac, subset = cumsum(weight) <= .95))


# dX analysis

# saturated model
mod_dX = lm(log_dX_range ~ area_ha_s + summer_air_temp_s + total_pred_cue_s + lt_cue_s, data = d1)

# aic model selection
dl_dX = dredge(mod_dX, extra = "adjR^2", rank = "AICc")
dl_dX

confint(model.avg(dl_dX, subset = cumsum(weight) <= .95))

dx_tab_df <- dl_dX
# summary of the model averaged model - note use the conditional averages parameter estimates
summary(model.avg(dl_dX, subset = delta < 2))
summary(model.avg(dl_dX, subset = cumsum(weight) <= .95)) 

##adding in 95% CI
confint(model.avg(dl_dX, subset = cumsum(weight) <= .95))

summary(model.avg(dl_dX, subset = cumsum(weight) <= .95))


# dY analysis

# saturated model
mod_dY = lm(log_dY_range ~ area_ha_s + summer_air_temp_s + total_pred_cue_s + lt_cue_s, data = d1)

# aic model selection
dl_dY = dredge(mod_dY, extra = "adjR^2", rank = "AICc")
dl_dY

confint(model.avg(dl_dY, subset = cumsum(weight) <= .95))

dy_table <- dl_dY

# summary of the model averaged model - note use the conditional averages parameter estimates
summary(model.avg(dl_dY, subset = delta < 2))
summary(model.avg(dl_dY, subset = cumsum(weight) <= .95)) 
# very weak and null model was ranked top, no support


##adding in 95% CI
confint(model.avg(dl_dY, subset = cumsum(weight) <= .95))

summary(model.avg(dl_dY, subset = cumsum(weight) <= .95))


# SDNND analysis

# saturated model
mod_SDNND = lm(log_SDNND ~ area_ha_s + summer_air_temp_s + total_pred_cue_s + lt_cue_s, data = d1)

# aic model selection
dl_SDNND = dredge(mod_SDNND, extra = "adjR^2", rank = "AICc")
dl_SDNND

confint(model.avg(dl_SDNND, subset = cumsum(weight) <= .95))

SDNND_TABLE<- dl_SDNND

##adding in 95% CI
confint(model.avg(dl_SDNND, subset = cumsum(weight) <= .95))

summary(model.avg(dl_SDNND, subset = cumsum(weight) <= .95))









#SUPPLEMENTARY ANALYSIS #2
#Mean littoral coupling and trophic position analysis
#code for figure S.4


#read in data
meandat <- read.csv("mean_lc_tp_df.csv")


#littoral coupling
#regression for lake area 

lcr_mlr_mean <- lm(logit_lc ~ log_area, data = meandat)

summary(lcr_mlr_mean)

ggplot(data = meandat, aes(x = log_area, y = logit_lc)) +
  geom_point() + geom_smooth(method = 'lm', color = '#e76254') +
  theme_classic() +
  theme(axis.text = element_text(size = 14))+
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red") +
  labs(x = "Lake Area (log(ha))", y = "Mean Nearshore Coupling (logit)") 


#tp vs lake area
tp_mlr_mean <- lm(TP_pred_mean ~ log_area, data = meandat)

summary(tp_mlr_mean)

ggplot(data = meandat, aes(x = log_area, y = TP_pred_mean)) +
  geom_point() + geom_smooth(method = 'lm', color = '#e76254') +
  theme_classic() +
  theme(axis.text = element_text(size = 14))+
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red") +
  labs(x = "Lake Area (log(ha))", y = "Mean Trophic Position") 

#lc vs temp
lc_mlr_mean_temp <- lm(logit_lc ~ summer_air_temp, data = meandat)

summary(lc_mlr_mean_temp)

ggplot(data = meandat, aes(x = summer_air_temp, y = logit_lc)) +
  geom_point() + geom_smooth(method = 'lm', color = '#e76254') +
  theme_classic() +
  theme(axis.text = element_text(size = 14))+
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red") +
  labs(x = "Summer air temp", y = "Mean coupling (logit)") 

##tp vs temp
tp_mlr_mean_temp <- lm(TP_pred_mean ~ summer_air_temp, data = meandat)

summary(tp_mlr_mean_temp)

ggplot(data = meandat, aes(x = summer_air_temp, y = TP_pred_mean)) +
  geom_point() + geom_smooth(method = 'lm', color = '#e76254') +
  theme_classic() +
  theme(axis.text = element_text(size = 14))+
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red") +
  labs(x = "Summer air temp", y = "Mean TP") 











#SUPPLEMENTARY ANALYSIS #3
##correlation matrix for predictor variables
#code for figure S.2

corr_df_fin <- read.csv("corr_matrix_df.csv")

corr_df_fin <- corr_df_fin%>%
  select(-X)

#generate correlation matrix 
correlation_matrix <- cor(corr_df_fin)
print(correlation_matrix)

#load plot
library(corrplot)

corrplot(correlation_matrix, method = "number", col = colorRampPalette(c("red", "white", "blue"))(100), type = "upper")



#SUPPLEMENTARY ANALYSIS #4
##regressions for other potential drivers
#code for table S.2, S.3, S.4

#competition variables: intra and interspecific (total pred cue and lake trout cue)

reg_df <- read.csv("bsm_analysis_dat.csv")

#dY range - table S2 
tp_comp <- lm(log_dY_range ~ log_total_pred_CUE + log_LaTro_CUE, data = reg_df)

summary(tp_comp)


##get model info for table S2

# Extract coefficients
coefficients <- summary(tp_comp)$coefficients

# Extract intercept and coefficients 
intercept <- coefficients["(Intercept)", "Estimate"]
coef_PRED_CUE <- coefficients["log_total_pred_CUE", "Estimate"]
coef_LT_CUE <- coefficients["log_LaTro_CUE", "Estimate"]

# Create the formatted equation
formatted_equation <- paste("y =", round(intercept, 4), "+", round(coef_PRED_CUE, 4), "(PRED_CUE) +", round(coef_LT_CUE, 4), "(LT_CUE)")

# Print or use the formatted equation
cat(formatted_equation)


# dX range - table S2

lc_comp <- lm(log_dX_range ~ log_total_pred_CUE + log_LaTro_CUE, data = reg_df)

summary(lc_comp)


##get model info for table S2

# Extract coefficients
coefficients <- summary(lc_comp)$coefficients

# Extract intercept and coefficients 
intercept <- coefficients["(Intercept)", "Estimate"]
coef_PRED_CUE <- coefficients["log_total_pred_CUE", "Estimate"]
coef_LT_CUE <- coefficients["log_LaTro_CUE", "Estimate"]

# Create the formatted equation
formatted_equation <- paste("y =", round(intercept, 4), "+", round(coef_PRED_CUE, 4), "(PRED_CUE) +", round(coef_LT_CUE, 4), "(LT_CUE)")

# Print or use the formatted equation
cat(formatted_equation)



##SEAc - table S2

seac_comp <- lm(log_SEAc ~ log_total_pred_CUE + log_LaTro_CUE, data = reg_df)
summary(seac_comp)


##get model info for table s2

# Extract coefficients
coefficients <- summary(seac_comp)$coefficients

# Extract intercept and coefficients 
intercept <- coefficients["(Intercept)", "Estimate"]
coef_PRED_CUE <- coefficients["log_total_pred_CUE", "Estimate"]
coef_LT_CUE <- coefficients["log_LaTro_CUE", "Estimate"]

# Create the formatted equation
formatted_equation <- paste("y =", round(intercept, 4), "+", round(coef_PRED_CUE, 4), "(PRED_CUE) +", round(coef_LT_CUE, 4), "(LT_CUE)")

# Print or use the formatted equation
cat(formatted_equation)



##SDNND
sdnnd_comp <- lm(log_SDNND ~ log_total_pred_CUE + log_LaTro_CUE, data = reg_df)

summary(sdnnd_comp)


##get model info for supp

# Extract coefficients
coefficients <- summary(sdnnd_comp)$coefficients

# Extract intercept and coefficients 
intercept <- coefficients["(Intercept)", "Estimate"]
coef_PRED_CUE <- coefficients["log_total_pred_CUE", "Estimate"]
coef_LT_CUE <- coefficients["log_LaTro_CUE", "Estimate"]

# Create the formatted equation
formatted_equation <- paste("y =", round(intercept, 4), "+", round(coef_PRED_CUE, 4), "(PRED_CUE) +", round(coef_LT_CUE, 4), "(LT_CUE)")

# Print or use the formatted equation
cat(formatted_equation)




# other abiotic metrics : dev index and plittoral

#read in data
supp4_df <- read.csv("supp_analysis_4_df.csv")


#pLittoral - table S3
#dY range
tp_sup <- lm(log_dY_range ~ pLittoral, data = supp4_df)
summary(tp_sup)

#
# Extract coefficients
coefficients <- summary(tp_sup)$coefficients

# Extract intercept and coefficients 
intercept <- coefficients["(Intercept)", "Estimate"]
coef_pLitt <- coefficients["pLittoral", "Estimate"]

# Create the formatted equation
formatted_equation <- paste("y =", round(intercept, 4), "+", round(coef_pLitt, 4), "(pLitt)")

# Print or use the formatted equation
cat(formatted_equation)
#

#dX range
lc_sup <- lm(log_dX_range ~ pLittoral, data = supp4_df)
summary(lc_sup)

# Extract coefficients
coefficients <- summary(lc_sup)$coefficients

# Extract intercept and coefficients 
intercept <- coefficients["(Intercept)", "Estimate"]
coef_pLitt <- coefficients["pLittoral", "Estimate"]

# Create the formatted equation
formatted_equation <- paste("y =", round(intercept, 4), "+", round(coef_pLitt, 4), "(pLitt)")

# Print or use the formatted equation
cat(formatted_equation)


#SEAc
seac_sup <- lm(log_SEAc ~ pLittoral, data = supp4_df)
summary(seac_sup)

#
# Extract coefficients
coefficients <- summary(seac_sup)$coefficients

# Extract intercept and coefficients for LSA and LMD
intercept <- coefficients["(Intercept)", "Estimate"]
coef_pLitt <- coefficients["pLittoral", "Estimate"]

# Create the formatted equation
formatted_equation <- paste("y =", round(intercept, 4), "+", round(coef_pLitt, 4), "(pLitt)")

# Print or use the formatted equation
cat(formatted_equation)
#
#

#sdnnd
sdnnd_sup <- lm(log_SDNND ~ pLittoral, data = supp4_df)
summary(sdnnd_sup)

#
# Extract coefficients
coefficients <- summary(sdnnd_sup)$coefficients

# Extract intercept and coefficients 
intercept <- coefficients["(Intercept)", "Estimate"]
coef_pLitt <- coefficients["pLittoral", "Estimate"]

# Create the formatted equation
formatted_equation <- paste("y =", round(intercept, 4), "+", round(coef_pLitt, 4), "(pLitt)")

# Print or use the formatted equation
cat(formatted_equation)





#Dev Index - table S4
#dY range
tp_sup <- lm(log_dY_range ~ log_dev_index, data = supp4_df)
summary(tp_sup)

#
# Extract coefficients
coefficients <- summary(tp_sup)$coefficients

# Extract intercept and coefficients
intercept <- coefficients["(Intercept)", "Estimate"]
coef_sdi <- coefficients["log_dev_index", "Estimate"]

# Create the formatted equation
formatted_equation <- paste("y =", round(intercept, 4), "+", round(coef_sdi, 4), "(SDI)")

# Print or use the formatted equation
cat(formatted_equation)
#

#DX range
lc_sup <- lm(log_dX_range ~ log_dev_index, data = supp4_df)
summary(lc_sup)

#
#
# Extract coefficients
coefficients <- summary(lc_sup)$coefficients

# Extract intercept and coefficients 
intercept <- coefficients["(Intercept)", "Estimate"]
coef_sdi <- coefficients["log_dev_index", "Estimate"]

# Create the formatted equation
formatted_equation <- paste("y =", round(intercept, 4), "+", round(coef_sdi, 4), "(SDI)")

# Print or use the formatted equation
cat(formatted_equation)


#seac
seac_sup <- lm(log_SEAc ~ log_dev_index, data = supp4_df)
summary(seac_sup)

#
# Extract coefficients
coefficients <- summary(seac_sup)$coefficients

# Extract intercept and coefficients 
intercept <- coefficients["(Intercept)", "Estimate"]
coef_sdi <- coefficients["log_dev_index", "Estimate"]

# Create the formatted equation
formatted_equation <- paste("y =", round(intercept, 4), "+", round(coef_sdi, 4), "(SDI)")

# Print or use the formatted equation
cat(formatted_equation)
#

#sdnnd
sdnnd_sup <- lm(log_SDNND ~ log_dev_index, data = supp4_df)
summary(sdnnd_sup)

# Extract coefficients
coefficients <- summary(sdnnd_sup)$coefficients

# Extract intercept and coefficients
intercept <- coefficients["(Intercept)", "Estimate"]
coef_sdi <- coefficients["log_dev_index", "Estimate"]

# Create the formatted equation
formatted_equation <- paste("y =", round(intercept, 4), "+", round(coef_sdi, 4), "(SDI)")

# Print or use the formatted equation
cat(formatted_equation)




