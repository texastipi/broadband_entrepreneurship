#### Regression Exploration ####
set.seed(2021)
library(tidyverse)
library(ggplot2)
library(psych)

d <- read_csv("https://raw.githubusercontent.com/texastipi/broadband_entrepreneurship/master/Broadband-Entrepreneurship-TXKSME.csv")

glimpse(d)

## Only nonmetro counties
## New DV candidates
## 1. Nonfarm proprietorship per capita
## 2. Nonemployer establishment per capita

## New IV candidates
## 1. Population density
## 2. Median Income
## 3. Distance to nearest metropolitan area
## 4. Social capital index
## 5. Education variable only bachelor's degree

# Filter data
d <- d %>% mutate(county_FIPS = as.character(county_FIPS))
d_nm <- d %>% filter(metro_f == "Nonmetro")

# FCC 25/3
mod1_pctnf_25 <- lm(pct_nonfarm_bea_2019 ~ pct25_3_dec_2019_fcc + pctbachelors_2019 + indstry_diversity + med_income_2019 + cbsa_min_dist + sk2014 + pop_dense_2019,
           data = d)

summary(mod1_pctnf_25)

# FCC 100/10
mod1_pctnf_100 <- lm(pct_nonfarm_bea_2019 ~ pct100_10_dec_2019_fcc + pctbachelors_2019 + indstry_diversity + med_income_2019 + cbsa_min_dist + sk2014 + pop_dense_2019,
                     data = d)
summary(mod1_pctnf_100)
# FCC 250/25
mod1_pctnf_250 <- lm(pct_nonfarm_bea_2019 ~ pct250_25_dec_2019_fcc + pctbachelors_2019 + indstry_diversity + med_income_2019 + cbsa_min_dist + sk2014 + pop_dense_2019,
                     data = d)
summary(mod1_pctnf_250)

# FCC 1000/100
mod1_pctnf_1000 <- lm(pct_nonfarm_bea_2019 ~ pct1000_100_dec_2019_fcc + pctbachelors_2019 + indstry_diversity + med_income_2019 + cbsa_min_dist + sk2014 + pop_dense_2019,
                     data = d)
summary(mod1_pctnf_1000)
# ACS broadband subscription
mod1_pctnf_acs <- lm(pct_nonfarm_bea_2019 ~ pct_fixed_acs_2019 + pctbachelors_2019 + indstry_diversity + med_income_2019 + cbsa_min_dist + sk2014 + pop_dense_2019,
                      data = d)
summary(mod1_pctnf_acs)
# BB QoS
mod1_pctnf_qos <- lm(pct_nonfarm_bea_2019 ~ pct_bb_qos + pctbachelors_2019 + indstry_diversity + med_income_2019 + cbsa_min_dist + sk2014 + pop_dense_2019,
                     data = d)
summary(mod1_pctnf_qos)

## Nonfarm per capita
# FCC 25/3
mod1_nfpc_25 <- lm(nonfarmprop_percapita ~ pct25_3_dec_2019_fcc + pctbachelors_2019 + indstry_diversity + med_income_2019 + cbsa_min_dist + sk2014 + pop_dense_2019 + ST - 1,
                  data = d)
summary(mod_nfpc_25)
# FCC 100/10
mod1_nfpc_100 <- lm(nonfarmprop_percapita ~ pct100_10_dec_2019_fcc + pctbachelors_2019 + indstry_diversity + med_income_2019 + cbsa_min_dist + sk2014 + pop_dense_2019 + ST - 1,
                     data = d)
summary(mod1_nfpc_100)
# FCC 250/25
mod1_nfpc_250 <- lm(nonfarmprop_percapita ~ pct250_25_dec_2019_fcc + pctbachelors_2019 + indstry_diversity + med_income_2019 + cbsa_min_dist + sk2014 + pop_dense_2019 + ST - 1,
                     data = d)
summary(mod1_nfpc_250)
# FCC 1000/100
mod1_nfpc_1000 <- lm(nonfarmprop_percapita ~ pct1000_100_dec_2019_fcc + pctbachelors_2019 + indstry_diversity + med_income_2019 + cbsa_min_dist + sk2014 + pop_dense_2019 + ST - 1,
                      data = d)
summary(mod1_nfpc_1000)
# ACS broadband subscription
mod1_nfpc_acs <- lm(nonfarmprop_percapita ~ pct_fixed_acs_2019 + pctbachelors_2019 + indstry_diversity + med_income_2019 + cbsa_min_dist + sk2014 + pop_dense_2019 + ST - 1,
                     data = d)
summary(mod1_nfpc_acs)
# BB QoS
mod1_nfpc_qos <- lm(nonfarmprop_percapita ~ pct_bb_qos + pctbachelors_2019 + indstry_diversity + med_income_2019 + cbsa_min_dist + sk2014 + pop_dense_2019 + ST - 1,
                     data = d)
summary(mod1_nfpc_qos)

## Nonemployer per capita
# FCC 25/3
mod1_nemp_25 <- lm(nonemp_percapita ~ pct25_3_dec_2019_fcc + pctbachelors_2019 + indstry_diversity + med_income_2019 + cbsa_min_dist + sk2014 + pop_dense_2019 + ST - 1,
                  data = d_nm)
summary(mod_nemp_25)
# FCC 100/10
mod1_nemp_100 <- lm(nonemp_percapita ~ pct100_10_dec_2019_fcc + pctbachelors_2019 + indstry_diversity + med_income_2019 + cbsa_min_dist + sk2014 + pop_dense_2019 + ST - 1,
                    data = d_nm)
summary(mod1_nemp_100)
# FCC 250/25
mod1_nemp_250 <- lm(nonemp_percapita ~ pct250_25_dec_2019_fcc + pctbachelors_2019 + indstry_diversity + med_income_2019 + cbsa_min_dist + sk2014 + pop_dense_2019 + ST - 1,
                    data = d_nm)
summary(mod1_nemp_250)
# FCC 1000/100
mod1_nemp_1000 <- lm(nonemp_percapita ~ pct1000_100_dec_2019_fcc + pctbachelors_2019 + indstry_diversity + med_income_2019 + cbsa_min_dist + sk2014 + pop_dense_2019 + ST - 1,
                     data = d_nm)
summary(mod1_nemp_1000)
# ACS broadband subscription
mod1_nemp_acs <- lm(nonemp_percapita ~ pct_fixed_acs_2019 + pctbachelors_2019 + indstry_diversity + med_income_2019 + cbsa_min_dist + sk2014 + pop_dense_2019 + ST - 1,
                    data = d_nm)
summary(mod1_nemp_acs)
# BB QoS
mod1_nemp_qos <- lm(nonemp_percapita ~ pct_bb_qos + pctbachelors_2019 + indstry_diversity + med_income_2019 + cbsa_min_dist + sk2014 + pop_dense_2019 + ST - 1,
                    data = d_nm)
summary(mod1_nemp_qos)



# Model 1 with FCC 25/3 %
mod.1.FCC.25.3 <- lm(pct_nonfarm_bea_2019 ~ pct_millennial_2019 + pctbachelors_2019 +
                       indstry_diversity + digital_distress_2019 + med_income_2019 + cbsa_min_dist + sk2014 + pop_dense_2019 +
                       pct25_3_dec_2019_fcc + ST - 1, data = d_nm)
summary(mod.1.FCC.25.3)
# Model 1 with FCC 100/10 %
mod.1.FCC.100.10 <- lm(pct_nonfarm_bea_2019 ~ pct_millennial_2019 + pctbachelors_2019 +
                         indstry_diversity + digital_distress_2019 + med_income_2019 + cbsa_min_dist + sk2014 + pop_dense_2019 +
                         pct100_10_dec_2019_fcc + ST - 1, data = d_nm)
summary(mod.1.FCC.100.10)
# Model 1 with FCC 250/25 %
mod.1.FCC.250.25 <- lm(pct_nonfarm_bea_2019 ~ pct_millennial_2019 + pctbachelors_2019 +
                         indstry_diversity + digital_distress_2019 + med_income_2019 + cbsa_min_dist + sk2014 + pop_dense_2019 +
                         pct250_25_dec_2019_fcc + ST - 1, data = d_nm)
summary(mod.1.FCC.250.25)
# Model 1 with FCC 1000/100 %
mod.1.FCC.1000.100 <- lm(pct_nonfarm_bea_2019 ~ pct_millennial_2019 + pctbachelors_2019 +
                           indstry_diversity + digital_distress_2019 + med_income_2019 + cbsa_min_dist + sk2014 + pop_dense_2019 +
                           pct1000_100_dec_2019_fcc + ST - 1, data = d_nm)
summary(mod.1.FCC.1000.100)
# Model 1 with ACS Bbnd adoption %
mod.1.ACS <- lm(pct_nonfarm_bea_2019 ~ pct_millennial_2019 + pctbachelors_2019 +
                  indstry_diversity + digital_distress_2019 + med_income_2019 + cbsa_min_dist + sk2014 + pop_dense_2019 +
                  pct_fixed_acs_2019 + ST - 1, data = d_nm)
summary(mod.1.ACS)
# Model 1 with Bbnd QoS measure
mod.1.QoS <- lm(pct_nonfarm_bea_2019 ~ pct_millennial_2019 + pctbachelors_2019 +
                  indstry_diversity + digital_distress_2019 + med_income_2019 + cbsa_min_dist + sk2014 + pop_dense_2019 +
                  pct_bb_qos + ST - 1, data = d_nm)
summary(mod.1.QoS)


# Model 1 with FCC 25/3 %
mod.2.FCC.25.3 <- lm(nonemp_percapita ~ pct_millennial_2019 + pctbachelors_2019 +
                       indstry_diversity + digital_distress_2019 + med_income_2019 + cbsa_min_dist + sk2014 + pop_dense_2019 +
                       pct25_3_dec_2019_fcc + ST - 1, data = d_nm)
summary(mod.2.FCC.25.3)
# Model 1 with FCC 100/10 %
mod.2.FCC.100.10 <- lm(nonemp_percapita ~ pct_millennial_2019 + pctbachelors_2019 +
                         indstry_diversity + digital_distress_2019 + med_income_2019 + cbsa_min_dist + sk2014 + pop_dense_2019 +
                         pct100_10_dec_2019_fcc + ST - 1, data = d_nm)
summary(mod.2.FCC.100.10)
# Model 1 with FCC 250/25 %
mod.2.FCC.250.25 <- lm(nonemp_percapita ~ pct_millennial_2019 + pctbachelors_2019 +
                         indstry_diversity + digital_distress_2019 + med_income_2019 + cbsa_min_dist + sk2014 + pop_dense_2019 +
                         pct250_25_dec_2019_fcc + ST - 1, data = d_nm)
summary(mod.2.FCC.250.25)
# Model 1 with FCC 1000/100 %
mod.2.FCC.1000.100 <- lm(nonemp_percapita ~ pct_millennial_2019 + pctbachelors_2019 +
                           indstry_diversity + digital_distress_2019 + med_income_2019 + cbsa_min_dist + sk2014 + pop_dense_2019 +
                           pct1000_100_dec_2019_fcc + ST - 1, data = d_nm)
summary(mod.2.FCC.1000.100)
# Model 1 with ACS Bbnd adoption %
mod.2.ACS <- lm(nonemp_percapita ~ pct_millennial_2019 + pctbachelors_2019 +
                  indstry_diversity + digital_distress_2019 + med_income_2019 + cbsa_min_dist + sk2014 + pop_dense_2019 +
                  pct_fixed_acs_2019 + ST - 1, data = d_nm)
summary(mod.2.ACS)
# Model 1 with Bbnd QoS measure
mod.2.QoS <- lm(nonemp_percapita ~ pct_millennial_2019 + pctbachelors_2019 +
                  indstry_diversity + digital_distress_2019 + med_income_2019 + cbsa_min_dist + sk2014 + pop_dense_2019 +
                  pct_bb_qos + ST - 1, data = d_nm)
summary(mod.2.QoS)



# Model 1 with FCC 25/3 %
mod.3.FCC.25.3 <- lm(nonfarmprop_percapita ~ pct_millennial_2019 + pctbachelors_2019 +
                       indstry_diversity + digital_distress_2019 + med_income_2019 + cbsa_min_dist + sk2014 + pop_dense_2019 +
                       pct25_3_dec_2019_fcc + ST - 1, data = d_nm)
summary(mod.3.FCC.25.3)
# Model 1 with FCC 100/10 %
mod.3.FCC.100.10 <- lm(nonfarmprop_percapita ~ pct_millennial_2019 + pctbachelors_2019 +
                         indstry_diversity + digital_distress_2019 + med_income_2019 + cbsa_min_dist + sk2014 + pop_dense_2019 +
                         pct100_10_dec_2019_fcc + ST - 1, data = d_nm)
summary(mod.3.FCC.100.10)
# Model 1 with FCC 250/25 %
mod.3.FCC.250.25 <- lm(nonfarmprop_percapita ~ pct_millennial_2019 + pctbachelors_2019 +
                         indstry_diversity + digital_distress_2019 + med_income_2019 + cbsa_min_dist + sk2014 + pop_dense_2019 +
                         pct250_25_dec_2019_fcc + ST - 1, data = d_nm)
summary(mod.3.FCC.250.25)
# Model 1 with FCC 1000/100 %
mod.3.FCC.1000.100 <- lm(nonfarmprop_percapita ~ pct_millennial_2019 + pctbachelors_2019 +
                           indstry_diversity + digital_distress_2019 + med_income_2019 + cbsa_min_dist + sk2014 + pop_dense_2019 +
                           pct1000_100_dec_2019_fcc + ST - 1, data = d_nm)
summary(mod.3.FCC.1000.100)
# Model 1 with ACS Bbnd adoption %
mod.3.ACS <- lm(nonfarmprop_percapita ~ pct_millennial_2019 + pctbachelors_2019 +
                  indstry_diversity + digital_distress_2019 + med_income_2019 + cbsa_min_dist + sk2014 + pop_dense_2019 +
                  pct_fixed_acs_2019 + ST - 1, data = d_nm)
summary(mod.3.ACS)
# Model 1 with Bbnd QoS measure
mod.3.QoS <- lm(nonfarmprop_percapita ~ pct_millennial_2019 + pctbachelors_2019 +
                  indstry_diversity + digital_distress_2019 + med_income_2019 + cbsa_min_dist + sk2014 + pop_dense_2019 +
                  pct_bb_qos + ST - 1, data = d_nm)
summary(mod.3.QoS)


