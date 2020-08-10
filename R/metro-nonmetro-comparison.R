####################################
### Metro & Non-metro Comparison ###
####################################

install.packages("car")
install.packages("psych")

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
tx_bb_entrepreneur_merged_v2 <- read.csv("https://raw.githubusercontent.com/texastipi/broadband_entrepreneurship/master/Broadband-Entrepreneurship-TX-merged_v2.csv")
tx_bb_entrepreneur_merged_v2 <- tx_bb_entrepreneur_merged_v2 %>% 
  mutate(pct_fixed_acs_2018 = pct_fixed_acs_2018/100)   # Change to percentage

## Inspecting variables of interest ##
tx_bb_subset <- tx_bb_entrepreneur_merged_v2 %>% 
  select(pct_broadband_FCC, pct_broadband_MS, pct_broadband_mlab, pct_fixed_acs_2018, # Broadband
         IRR2010, RUCC_2013, metro_desc, # Metro-nonmetro
         pct_nonfarm_bea_2018, venturedensity_mean, pct_chg_bea_2012_2018, # Entrepreneurship
         pctagriculture, pctconstruction, pctwholesale, pctretail, pcttransportation, pctinformation_tech, # Control
         pctfinance, pctprofessional, pcteducation, pctother_occupation, pctpublic, pcthighschool, pctcollege,
         pctmillennial, pctgenx, pctbabyboomer)

summary(tx_bb_subset)

## Set metro-nonmetro variable into factor ##
tx_bb_subset <- tx_bb_subset %>% 
  mutate(metro_f = as.factor(metro_desc))

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
model_nonfarm_1 <- lm(pct_nonfarm_bea_2018 ~ pct_broadband_FCC + metro_f + pctagriculture + pctconstruction + pctwholesale + pctretail + pcttransportation + pctinformation_tech + pctfinance + pctprofessional + pcteducation + pctother_occupation + pctpublic + pcthighschool + pctcollege + pctmillennial + pctgenx + pctbabyboomer,
                      data = tx_bb_subset)
summary(model_nonfarm_1)

# Model 2 with MS added
model_nonfarm_2 <- lm(pct_nonfarm_bea_2018 ~ pct_broadband_FCC + pct_broadband_MS + metro_f + pctagriculture + pctconstruction + pctwholesale + pctretail + pcttransportation + pctinformation_tech + pctfinance + pctprofessional + pcteducation + pctother_occupation + pctpublic + pcthighschool + pctcollege + pctmillennial + pctgenx + pctbabyboomer,
                      data = tx_bb_subset)
summary(model_nonfarm_2)

# Model 3 with ACS added
model_nonfarm_3 <- lm(pct_nonfarm_bea_2018 ~ pct_broadband_FCC + pct_broadband_MS + pct_fixed_acs_2018 + metro_f + pctagriculture + pctconstruction + pctwholesale + pctretail + pcttransportation + pctinformation_tech + pctfinance + pctprofessional + pcteducation + pctother_occupation + pctpublic + pcthighschool + pctcollege + pctmillennial + pctgenx + pctbabyboomer,
                      data = tx_bb_subset)
summary(model_nonfarm_3)

# Model 4 with M-Lab added
model_nonfarm_4 <- lm(pct_nonfarm_bea_2018 ~ pct_broadband_FCC + pct_broadband_MS + pct_fixed_acs_2018 + pct_broadband_mlab + metro_f + pctagriculture + pctconstruction + pctwholesale + pctretail + pcttransportation + pctinformation_tech + pctfinance + pctprofessional + pcteducation + pctother_occupation + pctpublic + pcthighschool + pctcollege + pctmillennial + pctgenx + pctbabyboomer,
                      data = tx_bb_subset)
summary(model_nonfarm_4)


## Trying interaction effects of broadband * metro factor ##

model_nonfarm_1_int <- lm(pct_nonfarm_bea_2018 ~ pct_broadband_FCC * metro_f + pctagriculture + pctconstruction + pctwholesale + pctretail + pcttransportation + pctinformation_tech + pctfinance + pctprofessional + pcteducation + pctother_occupation + pctpublic + pcthighschool + pctcollege + pctmillennial + pctgenx + pctbabyboomer,
                          data = tx_bb_subset)
summary(model_nonfarm_1_int)


model_nonfarm_2_int <- lm(pct_nonfarm_bea_2018 ~ pct_broadband_FCC * metro_f + pct_broadband_MS * metro_f + pctagriculture + pctconstruction + pctwholesale + pctretail + pcttransportation + pctinformation_tech + pctfinance + pctprofessional + pcteducation + pctother_occupation + pctpublic + pcthighschool + pctcollege + pctmillennial + pctgenx + pctbabyboomer,
                          data = tx_bb_subset)
summary(model_nonfarm_2_int)



## Change in nonfarm proprietors share ##
# Model 1 with FCC
model_propchg_1 <- lm(pct_chg_bea_2012_2018 ~ pct_broadband_FCC + metro_f + pctagriculture + pctconstruction + pctwholesale + pctretail + pcttransportation + pctinformation_tech + pctfinance + pctprofessional + pcteducation + pctother_occupation + pctpublic + pcthighschool + pctcollege + pctmillennial + pctgenx + pctbabyboomer,
                      data = tx_bb_subset)
summary(model_propchg_1)

# Model 2 with MS added
model_propchg_2 <- lm(pct_chg_bea_2012_2018 ~ pct_broadband_FCC + pct_broadband_MS + metro_f + pctagriculture + pctconstruction + pctwholesale + pctretail + pcttransportation + pctinformation_tech + pctfinance + pctprofessional + pcteducation + pctother_occupation + pctpublic + pcthighschool + pctcollege + pctmillennial + pctgenx + pctbabyboomer,
                      data = tx_bb_subset)
summary(model_propchg_2)

# Model 3 with ACS added
model_propchg_3 <- lm(pct_chg_bea_2012_2018 ~ pct_broadband_FCC + pct_broadband_MS + pct_fixed_acs_2018 + metro_f + pctagriculture + pctconstruction + pctwholesale + pctretail + pcttransportation + pctinformation_tech + pctfinance + pctprofessional + pcteducation + pctother_occupation + pctpublic + pcthighschool + pctcollege + pctmillennial + pctgenx + pctbabyboomer,
                      data = tx_bb_subset)
summary(model_propchg_3)

# Model 4 with M-Lab added
model_propchg_4 <- lm(pct_chg_bea_2012_2018 ~ pct_broadband_FCC + pct_broadband_MS + pct_fixed_acs_2018 + pct_broadband_mlab + metro_f + pctagriculture + pctconstruction + pctwholesale + pctretail + pcttransportation + pctinformation_tech + pctfinance + pctprofessional + pcteducation + pctother_occupation + pctpublic + pcthighschool + pctcollege + pctmillennial + pctgenx + pctbabyboomer,
                      data = tx_bb_subset)
summary(model_propchg_4)

## Interaction models trial
# Model 1 with FCC
model_propchg_1_int <- lm(pct_chg_bea_2012_2018 ~ pct_broadband_FCC * metro_f + pctagriculture + pctconstruction + pctwholesale + pctretail + pcttransportation + pctinformation_tech + pctfinance + pctprofessional + pcteducation + pctother_occupation + pctpublic + pcthighschool + pctcollege + pctmillennial + pctgenx + pctbabyboomer,
                      data = tx_bb_subset)
summary(model_propchg_1_int)

# Model 2 with MS added
model_propchg_2_int <- lm(pct_chg_bea_2012_2018 ~ pct_broadband_FCC  + pct_broadband_MS * metro_f + pctagriculture + pctconstruction + pctwholesale + pctretail + pcttransportation + pctinformation_tech + pctfinance + pctprofessional + pcteducation + pctother_occupation + pctpublic + pcthighschool + pctcollege + pctmillennial + pctgenx + pctbabyboomer,
                      data = tx_bb_subset)
summary(model_propchg_2_int)

# Model 3 with ACS added
model_propchg_3_int <- lm(pct_chg_bea_2012_2018 ~ pct_broadband_FCC + pct_broadband_MS * metro_f + pct_fixed_acs_2018 * metro_f + pctagriculture + pctconstruction + pctwholesale + pctretail + pcttransportation + pctinformation_tech + pctfinance + pctprofessional + pcteducation + pctother_occupation + pctpublic + pcthighschool + pctcollege + pctmillennial + pctgenx + pctbabyboomer,
                      data = tx_bb_subset)
summary(model_propchg_3_int)

# Model 4 with M-Lab added
model_propchg_4_int <- lm(pct_chg_bea_2012_2018 ~ pct_broadband_FCC * metro_f + pct_broadband_MS * metro_f + pct_fixed_acs_2018 + pct_broadband_mlab * metro_f + pctagriculture + pctconstruction + pctwholesale + pctretail + pcttransportation + pctinformation_tech + pctfinance + pctprofessional + pcteducation + pctother_occupation + pctpublic + pcthighschool + pctcollege + pctmillennial + pctgenx + pctbabyboomer,
                      data = tx_bb_subset)
summary(model_propchg_4_int)

## Model comparison ##
anova(model_propchg_4, model_propchg_4_int)

## Plot interaction effect ##
plot_model(model_propchg_4_int, type = "pred", terms = c("pct_broadband_MS","metro_f"))

