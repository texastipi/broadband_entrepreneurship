####################################
### Metro & Non-metro Comparison ###
####################################

install.packages("car")
install.packages("psych")
install.packages(c("jtools","interactions","sandwich"), dependencies = T)

library(tidyverse)
library(ggplot2)
library(gridExtra)
library(car)
library(psych)
library(jtools)
library(interactions)
library(sandwich)
library(sjPlot)
library(sjmisc)

## Import dataset ##
bb_entrepreneur <- read.csv("https://raw.githubusercontent.com/texastipi/broadband_entrepreneurship/master/Broadband-Entrepreneurship-TXKSME.csv")
bb_entrepreneur <- bb_entrepreneur %>% 
  mutate(pct_fixed_acs_2018 = pct_fixed_acs_2018/100)   # Change to percentage

## Inspecting variables of interest ##
tx_bb_subset <- bb_entrepreneur %>% 
  select(pct_broadband_FCC, pct_broadband_MS, pct_broadband_mlab, pct_fixed_acs_2018, # Broadband
         IRR2010, RUCC_2013, metro_desc, # Metro-nonmetro
         pct_nonfarm_bea_2018, venturedensity_mean, pct_chg_bea_2012_2018, # Entrepreneurship
         pctagriculture_2019, pctconstruction_2019, pctwholesale_2019, pctretail_2019, pcttransportation_2019, pctinformation_2019, # Control
         pctfinance_2019, pctprofessional_2019, pctedu_healthcare_social_2019, pctother_occupation_2019, pctpublic_admin_2019, pctlessthanhigh_2019, pctbachelors_2019,
         pctgraduate_2019, pctmilennial_2019, pctgenx_2019, pctboomers_2019)

summary(tx_bb_subset)

## Set metro-nonmetro variable into factor ##
tx_bb_subset <- tx_bb_subset %>% 
  mutate(metro_f = as.factor(metro_desc),
         pctagriculture_2019 = pctagriculture_2019/100,
         pctconstruction_2019 = pctconstruction_2019/100,
         pctwholesale_2019 = pctwholesale_2019/100,
         pctretail_2019 = pctretail_2019/100,
         pcttransportation_2019 = pcttransportation_2019/100,
         pctinformation_2019 = pctinformation_2019/100,
         pctfinance_2019 = pctfinance_2019/100,
         pctprofessional_2019 = pctprofessional_2019/100,
         pctedu_healthcare_social_2019 = pctedu_healthcare_social_2019/100,
         pctother_occupation_2019 = pctother_occupation_2019/100,
         pctpublic_admin_2019 = pctpublic_admin_2019/100,
         pctlessthanhigh_2019 = pctlessthanhigh_2019/100, pctbachelors_2019 = pctbachelors_2019/100,
         pctgraduate_2019 = pctgraduate_2019/100)
  

summary(tx_bb_subset$metro_f)
contrasts(tx_bb_subset$metro_f)

## Boxplots of broadband and entrepreneurship measures against metro factor ##

gridExtra::grid.arrange(
  ggplot(tx_bb_subset, aes(x = metro_f, y = pct_broadband_FCC)) + geom_boxplot(notch = T),
  ggplot(tx_bb_subset, aes(x = metro_f, y = pct_broadband_MS)) + geom_boxplot(notch = T),
  ggplot(tx_bb_subset, aes(x = metro_f, y = pct_broadband_mlab)) + geom_boxplot(notch = T),
  ggplot(tx_bb_subset, aes(x = metro_f, y = pct_fixed_acs_2018)) + geom_boxplot(notch = T),
  ggplot(tx_bb_subset, aes(x = metro_f, y = pct_nonfarm_bea_2018)) + geom_boxplot(notch = T),
  ggplot(tx_bb_subset, aes(x = metro_f, y = pct_chg_bea_2012_2018)) + geom_boxplot(notch = T),
  ggplot(tx_bb_subset, aes(x = metro_f, y = venturedensity_mean)) + geom_boxplot(notch = T),
  nrow = 2, ncol = 4
)

#### Metro Factor as Predictor ####

## Nonfarm Proprietors Share ##
# Model 1 with FCC
model_nonfarm_1 <- lm(pct_nonfarm_bea_2018 ~ pct_broadband_FCC + metro_f + pctagriculture_2019 +
                        pctconstruction_2019 + pctwholesale_2019 + pctretail_2019 + pcttransportation_2019 + pctinformation_2019 +
                        pctfinance_2019 + pctprofessional_2019 + pctedu_healthcare_social_2019 + pctother_occupation_2019 +
                        pctpublic_admin_2019 + pctlessthanhigh_2019 + pctbachelors_2019 + pctgraduate_2019,
                      data = tx_bb_subset)
summary(model_nonfarm_1)

# Model 2 with MS added
model_nonfarm_2 <- lm(pct_nonfarm_bea_2018 ~ pct_broadband_FCC + pct_broadband_MS + metro_f + pctagriculture_2019 +
                        pctconstruction_2019 + pctwholesale_2019 + pctretail_2019 + pcttransportation_2019 + pctinformation_2019 +
                        pctfinance_2019 + pctprofessional_2019 + pctedu_healthcare_social_2019 + pctother_occupation_2019 +
                        pctpublic_admin_2019 + pctlessthanhigh_2019 + pctbachelors_2019 + pctgraduate_2019,
                      data = tx_bb_subset)
summary(model_nonfarm_2)

# Model 3 with ACS added
model_nonfarm_3 <- lm(pct_nonfarm_bea_2018 ~ pct_broadband_FCC + pct_broadband_MS + pct_fixed_acs_2018 + metro_f + pctagriculture_2019 +
                        pctconstruction_2019 + pctwholesale_2019 + pctretail_2019 + pcttransportation_2019 + pctinformation_2019 +
                        pctfinance_2019 + pctprofessional_2019 + pctedu_healthcare_social_2019 + pctother_occupation_2019 +
                        pctpublic_admin_2019 + pctlessthanhigh_2019 + pctbachelors_2019 + pctgraduate_2019,
                      data = tx_bb_subset)
summary(model_nonfarm_3)

# Model 4 with M-Lab added
model_nonfarm_4 <- lm(pct_nonfarm_bea_2018 ~ pct_broadband_FCC + pct_broadband_MS + pct_fixed_acs_2018 + pct_broadband_mlab + metro_f + pctagriculture_2019 +
                        pctconstruction_2019 + pctwholesale_2019 + pctretail_2019 + pcttransportation_2019 + pctinformation_2019 +
                        pctfinance_2019 + pctprofessional_2019 + pctedu_healthcare_social_2019 + pctother_occupation_2019 +
                        pctpublic_admin_2019 + pctlessthanhigh_2019 + pctbachelors_2019 + pctgraduate_2019,
                      data = tx_bb_subset)
summary(model_nonfarm_4)


## Trying interaction effects of broadband * metro factor ##

model_nonfarm_1_int <- lm(pct_nonfarm_bea_2018 ~ pct_broadband_FCC * metro_f + pctagriculture_2019 +
                            pctconstruction_2019 + pctwholesale_2019 + pctretail_2019 + pcttransportation_2019 + pctinformation_2019 +
                            pctfinance_2019 + pctprofessional_2019 + pctedu_healthcare_social_2019 + pctother_occupation_2019 +
                            pctpublic_admin_2019 + pctlessthanhigh_2019 + pctbachelors_2019 + pctgraduate_2019,
                          data = tx_bb_subset)
summary(model_nonfarm_1_int)


model_nonfarm_2_int <- lm(pct_nonfarm_bea_2018 ~ pct_broadband_FCC * metro_f + pct_broadband_MS * metro_f + pctagriculture_2019 +
                            pctconstruction_2019 + pctwholesale_2019 + pctretail_2019 + pcttransportation_2019 + pctinformation_2019 +
                            pctfinance_2019 + pctprofessional_2019 + pctedu_healthcare_social_2019 + pctother_occupation_2019 +
                            pctpublic_admin_2019 + pctlessthanhigh_2019 + pctbachelors_2019 + pctgraduate_2019,
                          data = tx_bb_subset)
summary(model_nonfarm_2_int)

model_nonfarm_3_int <- lm(pct_nonfarm_bea_2018 ~ pct_broadband_FCC * metro_f + pct_broadband_MS * metro_f + pct_fixed_acs_2018 * metro_f + pctagriculture_2019 +
                            pctconstruction_2019 + pctwholesale_2019 + pctretail_2019 + pcttransportation_2019 + pctinformation_2019 +
                            pctfinance_2019 + pctprofessional_2019 + pctedu_healthcare_social_2019 + pctother_occupation_2019 +
                            pctpublic_admin_2019 + pctlessthanhigh_2019 + pctbachelors_2019 + pctgraduate_2019,
                          data = tx_bb_subset)
summary(model_nonfarm_3_int)

model_nonfarm_4_int <- lm(pct_nonfarm_bea_2018 ~ pct_broadband_FCC * metro_f + pct_broadband_MS * metro_f + pct_fixed_acs_2018 * metro_f + pct_broadband_mlab * metro_f + pctagriculture_2019 +
                            pctconstruction_2019 + pctwholesale_2019 + pctretail_2019 + pcttransportation_2019 + pctinformation_2019 +
                            pctfinance_2019 + pctprofessional_2019 + pctedu_healthcare_social_2019 + pctother_occupation_2019 +
                            pctpublic_admin_2019 + pctlessthanhigh_2019 + pctbachelors_2019 + pctgraduate_2019,
                          data = tx_bb_subset)
summary(model_nonfarm_4_int)



## Change in nonfarm proprietors share ##
# Model 1 with FCC
model_propchg_1 <- lm(pct_chg_bea_2012_2018 ~ pct_broadband_FCC + metro_f + pctagriculture_2019 +
                        pctconstruction_2019 + pctwholesale_2019 + pctretail_2019 + pcttransportation_2019 + pctinformation_2019 +
                        pctfinance_2019 + pctprofessional_2019 + pctedu_healthcare_social_2019 + pctother_occupation_2019 +
                        pctpublic_admin_2019 + pctlessthanhigh_2019 + pctbachelors_2019 + pctgraduate_2019,
                      data = tx_bb_subset)
summary(model_propchg_1)

# Model 2 with MS added
model_propchg_2 <- lm(pct_chg_bea_2012_2018 ~ pct_broadband_FCC + pct_broadband_MS + metro_f + pctagriculture_2019 +
                        pctconstruction_2019 + pctwholesale_2019 + pctretail_2019 + pcttransportation_2019 + pctinformation_2019 +
                        pctfinance_2019 + pctprofessional_2019 + pctedu_healthcare_social_2019 + pctother_occupation_2019 +
                        pctpublic_admin_2019 + pctlessthanhigh_2019 + pctbachelors_2019 + pctgraduate_2019,
                      data = tx_bb_subset)
summary(model_propchg_2)

# Model 3 with ACS added
model_propchg_3 <- lm(pct_chg_bea_2012_2018 ~ pct_broadband_FCC + pct_broadband_MS + pct_fixed_acs_2018 + metro_f + pctagriculture_2019 +
                        pctconstruction_2019 + pctwholesale_2019 + pctretail_2019 + pcttransportation_2019 + pctinformation_2019 +
                        pctfinance_2019 + pctprofessional_2019 + pctedu_healthcare_social_2019 + pctother_occupation_2019 +
                        pctpublic_admin_2019 + pctlessthanhigh_2019 + pctbachelors_2019 + pctgraduate_2019,
                      data = tx_bb_subset)
summary(model_propchg_3)

# Model 4 with M-Lab added
model_propchg_4 <- lm(pct_chg_bea_2012_2018 ~ pct_broadband_FCC + pct_broadband_MS + pct_fixed_acs_2018 + pct_broadband_mlab + metro_f + pctagriculture_2019 +
                        pctconstruction_2019 + pctwholesale_2019 + pctretail_2019 + pcttransportation_2019 + pctinformation_2019 +
                        pctfinance_2019 + pctprofessional_2019 + pctedu_healthcare_social_2019 + pctother_occupation_2019 +
                        pctpublic_admin_2019 + pctlessthanhigh_2019 + pctbachelors_2019 + pctgraduate_2019,
                      data = tx_bb_subset)
summary(model_propchg_4)

## Interaction models trial
# Model 1 with FCC
model_propchg_1_int <- lm(pct_chg_bea_2012_2018 ~ pct_broadband_FCC * metro_f + pctagriculture_2019 +
                            pctconstruction_2019 + pctwholesale_2019 + pctretail_2019 + pcttransportation_2019 + pctinformation_2019 +
                            pctfinance_2019 + pctprofessional_2019 + pctedu_healthcare_social_2019 + pctother_occupation_2019 +
                            pctpublic_admin_2019 + pctlessthanhigh_2019 + pctbachelors_2019 + pctgraduate_2019,
                      data = tx_bb_subset)
summary(model_propchg_1_int)

# Model 2 with MS added
model_propchg_2_int <- lm(pct_chg_bea_2012_2018 ~ pct_broadband_FCC  + pct_broadband_MS * metro_f + pctagriculture_2019 +
                            pctconstruction_2019 + pctwholesale_2019 + pctretail_2019 + pcttransportation_2019 + pctinformation_2019 +
                            pctfinance_2019 + pctprofessional_2019 + pctedu_healthcare_social_2019 + pctother_occupation_2019 +
                            pctpublic_admin_2019 + pctlessthanhigh_2019 + pctbachelors_2019 + pctgraduate_2019,
                      data = tx_bb_subset)
summary(model_propchg_2_int)

# Model 3 with ACS added
model_propchg_3_int <- lm(pct_chg_bea_2012_2018 ~ pct_broadband_FCC + pct_broadband_MS * metro_f + pct_fixed_acs_2018 * metro_f + pctagriculture_2019 +
                            pctconstruction_2019 + pctwholesale_2019 + pctretail_2019 + pcttransportation_2019 + pctinformation_2019 +
                            pctfinance_2019 + pctprofessional_2019 + pctedu_healthcare_social_2019 + pctother_occupation_2019 +
                            pctpublic_admin_2019 + pctlessthanhigh_2019 + pctbachelors_2019 + pctgraduate_2019,
                      data = tx_bb_subset)
summary(model_propchg_3_int)

# Model 4 with M-Lab added
model_propchg_4_int <- lm(pct_chg_bea_2012_2018 ~ pct_broadband_FCC * metro_f + pct_broadband_MS * metro_f + pct_fixed_acs_2018 + pct_broadband_mlab * metro_f + pctagriculture_2019 +
                            pctconstruction_2019 + pctwholesale_2019 + pctretail_2019 + pcttransportation_2019 + pctinformation_2019 +
                            pctfinance_2019 + pctprofessional_2019 + pctedu_healthcare_social_2019 + pctother_occupation_2019 +
                            pctpublic_admin_2019 + pctlessthanhigh_2019 + pctbachelors_2019 + pctgraduate_2019,
                      data = tx_bb_subset)
summary(model_propchg_4_int)

## Model comparison ##
anova(model_propchg_4, model_propchg_4_int)

## Plot interaction effect ##
plot_model(model_propchg_4_int, type = "pred", terms = c("pct_broadband_MS","metro_f"))

## Full Model with all broadband, interaction, and interaction between metro factor and info industry
model_propchg_4_int_2 <- lm(pct_chg_bea_2012_2018 ~ pct_broadband_FCC * metro_f +
                              pct_broadband_MS * metro_f + pct_fixed_acs_2018 * metro_f + pct_broadband_mlab * metro_f +
                              pctagriculture_2019 + pctconstruction_2019 + pctwholesale_2019 +
                              pctretail_2019 + pcttransportation_2019 + pctinformation_2019 * metro_f +
                              pctfinance_2019 + pctprofessional_2019 + pctedu_healthcare_social_2019 +
                              pctother_occupation_2019 + pctpublic_admin_2019 + pctlessthanhigh_2019 + pctbachelors_2019 + pctgraduate_2019,
                            data = tx_bb_subset)
summary(model_propchg_4_int_2)

#### Subsetting Metro & Nonmetro and Running Models Separately ####

metro_subset <- tx_bb_subset %>% 
  filter(metro_f == "Metro")
nonmetro_subset <- tx_bb_subset %>% 
  filter(metro_f == "Nonmetro")

## Run full nonfarm model to each data subset ##
metro_sub_mod1 <- lm(pct_nonfarm_bea_2018 ~ pct_broadband_FCC + pct_broadband_MS + pct_fixed_acs_2018 + pct_broadband_mlab + pctagriculture_2019 +
                       pctconstruction_2019 + pctwholesale_2019 + pctretail_2019 + pcttransportation_2019 + pctinformation_2019 +
                       pctfinance_2019 + pctprofessional_2019 + pctedu_healthcare_social_2019 + pctother_occupation_2019 +
                       pctpublic_admin_2019 + pctlessthanhigh_2019 + pctbachelors_2019 + pctgraduate_2019,
                     data = metro_subset)

nonmetro_sub_mod1 <- lm(pct_nonfarm_bea_2018 ~ pct_broadband_FCC + pct_broadband_MS + pct_fixed_acs_2018 + pct_broadband_mlab + pctagriculture_2019 +
                          pctconstruction_2019 + pctwholesale_2019 + pctretail_2019 + pcttransportation_2019 + pctinformation_2019 +
                          pctfinance_2019 + pctprofessional_2019 + pctedu_healthcare_social_2019 + pctother_occupation_2019 +
                          pctpublic_admin_2019 + pctlessthanhigh_2019 + pctbachelors_2019 + pctgraduate_2019,
                        data = nonmetro_subset)

summary(metro_sub_mod1)
summary(nonmetro_sub_mod1)

## Run nonfarm model with interaction b/w broadband and information industry proportion ##
metro_info_bb_mod1 <- lm(pct_nonfarm_bea_2018 ~ pct_broadband_FCC * pctinformation_2019 + pct_broadband_MS * pctinformation_2019 + pct_fixed_acs_2018 * pctinformation_2019 + pct_broadband_mlab * pctinformation_2019 +
                           pctagriculture_2019 + pctconstruction_2019 + pctwholesale_2019 + pctretail_2019 +
                           pcttransportation_2019 + pctfinance_2019 + 
                           pctprofessional_2019 + pctedu_healthcare_social_2019 + pctother_occupation_2019 +
                           pctpublic_admin_2019 + pctlessthanhigh_2019 + pctbachelors_2019 + pctgraduate_2019,
                         data = metro_subset)
summary(metro_info_bb_mod1)

nonmetro_info_bb_mod1 <- lm(pct_nonfarm_bea_2018 ~ pct_broadband_FCC * pctinformation_2019 + pct_broadband_MS * pctinformation_2019 + pct_fixed_acs_2018 * pctinformation_2019 + pct_broadband_mlab * pctinformation_2019 +
                           pctagriculture_2019 + pctconstruction_2019 + pctwholesale_2019 + pctretail_2019 +
                           pcttransportation_2019 + pctfinance_2019 + 
                           pctprofessional_2019 + pctedu_healthcare_social_2019 + pctother_occupation_2019 +
                           pctpublic_admin_2019 + pctlessthanhigh_2019 + pctbachelors_2019 + pctgraduate_2019,
                         data = nonmetro_subset)
summary(nonmetro_info_bb_mod1)

plot_model(nonmetro_info_bb_mod1, type = "pred", terms = c("pctinformation_2019","pct_fixed_acs_2018"))

## Run nonfarm change model with interaction b/w broadband and information industry proportion ##
metro_info_bb_chg_mod1 <- lm(pct_chg_bea_2012_2018 ~ pct_broadband_FCC * pctinformation_2019 + pct_broadband_MS * pctinformation_2019 + pct_fixed_acs_2018 * pctinformation_2019 + pct_broadband_mlab * pctinformation_2019 +
                           pctagriculture_2019 + pctconstruction_2019 + pctwholesale_2019 + pctretail_2019 +
                           pcttransportation_2019 + pctfinance_2019 + 
                           pctprofessional_2019 + pctedu_healthcare_social_2019 + pctother_occupation_2019 +
                           pctpublic_admin_2019 + pctlessthanhigh_2019 + pctbachelors_2019 + pctgraduate_2019,
                         data = metro_subset)
summary(metro_info_bb_chg_mod1)
plot_model(metro_info_bb_chg_mod1, type = "pred", terms = c("pctinformation_2019","pct_broadband_MS"))
plot_model(metro_info_bb_chg_mod1, type = "pred", terms = c("pctinformation_2019","pct_fixed_acs_2018"))

nonmetro_info_bb_chg_mod1 <- lm(pct_chg_bea_2012_2018 ~ pct_broadband_FCC * pctinformation_2019 + pct_broadband_MS * pctinformation_2019 + pct_fixed_acs_2018 * pctinformation_2019 + pct_broadband_mlab * pctinformation_2019 +
                              pctagriculture_2019 + pctconstruction_2019 + pctwholesale_2019 + pctretail_2019 +
                              pcttransportation_2019 + pctfinance_2019 + 
                              pctprofessional_2019 + pctedu_healthcare_social_2019 + pctother_occupation_2019 +
                              pctpublic_admin_2019 + pctlessthanhigh_2019 + pctbachelors_2019 + pctgraduate_2019,
                            data = nonmetro_subset)
summary(nonmetro_info_bb_chg_mod1)
plot_model(nonmetro_info_bb_chg_mod1, type = "pred", terms = c("pctinformation_2019","pct_broadband_FCC"))

## Run full nonfarm change model to each data subset ##
metro_sub_chg_mod1 <- lm(pct_chg_bea_2012_2018 ~ pct_broadband_FCC + pct_broadband_MS + pct_fixed_acs_2018 + pct_broadband_mlab + pctagriculture_2019 +
                       pctconstruction_2019 + pctwholesale_2019 + pctretail_2019 + pcttransportation_2019 + pctinformation_2019 +
                       pctfinance_2019 + pctprofessional_2019 + pctedu_healthcare_social_2019 + pctother_occupation_2019 +
                       pctpublic_admin_2019 + pctlessthanhigh_2019 + pctbachelors_2019 + pctgraduate_2019,
                     data = metro_subset)

nonmetro_sub_chg_mod1 <- lm(pct_chg_bea_2012_2018 ~ pct_broadband_FCC + pct_broadband_MS + pct_fixed_acs_2018 + pct_broadband_mlab + pctagriculture_2019 +
                          pctconstruction_2019 + pctwholesale_2019 + pctretail_2019 + pcttransportation_2019 + pctinformation_2019 +
                          pctfinance_2019 + pctprofessional_2019 + pctedu_healthcare_social_2019 + pctother_occupation_2019 +
                          pctpublic_admin_2019 + pctlessthanhigh_2019 + pctbachelors_2019 + pctgraduate_2019,
                        data = nonmetro_subset)
summary(metro_sub_chg_mod1)
summary(nonmetro_sub_chg_mod1)    

# Note: Very significant relationship b/w information industry sector and change in nonfarm proprietorship
# This could mean that information industry is critical in increasing entrepreneurship in non-metro area.


