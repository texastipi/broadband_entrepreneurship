#### Regression Exploration ####
set.seed(2021)
library(tidyverse)
library(ggplot2)
library(psych)
options(scipen = 999)

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


#### Nonfarm Proprietory Share 2019 Models ####
# FCC 25/3
mod1_pctnf_25 <- lm(pct_nonfarm_bea_2019 ~ pct25_3_dec_2019_fcc + pctbachelors_2019 + indstry_diversity + med_income_2019 + 
                      cbsa_min_dist + sk2014 + pop_dense_2019 + digital_distress_2019 + relevel(factor(ST), ref = "TX"),
           data = d_nm)
summary(mod1_pctnf_25)

# FCC 100/10
mod1_pctnf_100 <- lm(pct_nonfarm_bea_2019 ~ pct100_10_dec_2019_fcc + pctbachelors_2019 + indstry_diversity + med_income_2019 + 
                       cbsa_min_dist + sk2014 + pop_dense_2019 + digital_distress_2019 + relevel(factor(ST), ref = "TX"),
                     data = d_nm)
summary(mod1_pctnf_100)
# FCC 250/25
mod1_pctnf_250 <- lm(pct_nonfarm_bea_2019 ~ pct250_25_dec_2019_fcc + pctbachelors_2019 + indstry_diversity + med_income_2019 + 
                       cbsa_min_dist + sk2014 + pop_dense_2019 + digital_distress_2019 + relevel(factor(ST), ref = "TX"),
                     data = d_nm)
summary(mod1_pctnf_250)

# FCC 1000/100
mod1_pctnf_1000 <- lm(pct_nonfarm_bea_2019 ~ pct1000_100_dec_2019_fcc + pctbachelors_2019 + indstry_diversity + med_income_2019 + 
                        cbsa_min_dist + sk2014 + pop_dense_2019 + digital_distress_2019 + relevel(factor(ST), ref = "TX"),
                     data = d_nm)
summary(mod1_pctnf_1000)
# ACS broadband subscription
mod1_pctnf_acs <- lm(pct_nonfarm_bea_2019 ~ pct_fixed_acs_2019 + pctbachelors_2019 + indstry_diversity + med_income_2019 + 
                       cbsa_min_dist + sk2014 + pop_dense_2019 + digital_distress_2019 + relevel(factor(ST), ref = "TX"),
                      data = d_nm)
summary(mod1_pctnf_acs)
# BB QoS
mod1_pctnf_qos <- lm(pct_nonfarm_bea_2019 ~ pct_bb_qos + pctbachelors_2019 + indstry_diversity + med_income_2019 + 
                       cbsa_min_dist + sk2014 + pop_dense_2019 + digital_distress_2019 + relevel(factor(ST), ref = "TX"),
                     data = d_nm)
summary(mod1_pctnf_qos)

## Generating Table ##
library(stargazer)
stargazer(mod1_pctnf_25, mod1_pctnf_100, mod1_pctnf_250, mod1_pctnf_1000, mod1_pctnf_acs, mod1_pctnf_qos, title = "Nonfarm Proprietors Share Regression Results",
          type = "text", style = "ajs",
          digits = 3, dep.var.caption = "Nonfarm Proprietors Share (2019)",
          align = TRUE, dep.var.labels.include = F, intercept.bottom = F, intercept.top = T,
          covariate.labels = c("(Intercept)",
                               "FCC BBnd (25/3)",
                               "FCC BBnd (100/10)",
                               "FCC BBnd (250/25)",
                               "FCC BBnd (1000/100)",
                               "ACS BBnd Sbscr",
                               "BBnd QoS",
                               "Bachelors degree", 
                               "Industry diversity", "Median Income", "Dist. to Closest Metro", "Social Capital", "Pop. Density", "Digital distress", 
                               "State (KS)", "State (ME)"), no.space = TRUE)


#### Nonfarm per capita Models ####
# FCC 25/3
mod1_nfpc_25 <- lm(nonfarmprop_percapita ~ pct25_3_dec_2019_fcc + pctbachelors_2019 + indstry_diversity + med_income_2019 + 
                     cbsa_min_dist + sk2014 + pop_dense_2019 + digital_distress_2019 + relevel(factor(ST), ref = "TX"),
                  data = d_nm)
summary(mod1_nfpc_25)
# FCC 100/10
mod1_nfpc_100 <- lm(nonfarmprop_percapita ~ pct100_10_dec_2019_fcc + pctbachelors_2019 + indstry_diversity + med_income_2019 + 
                      cbsa_min_dist + sk2014 + pop_dense_2019 + digital_distress_2019 + relevel(factor(ST), ref = "TX"),
                     data = d_nm)
summary(mod1_nfpc_100)
# FCC 250/25
mod1_nfpc_250 <- lm(nonfarmprop_percapita ~ pct250_25_dec_2019_fcc + pctbachelors_2019 + indstry_diversity + med_income_2019 + 
                      cbsa_min_dist + sk2014 + pop_dense_2019 + digital_distress_2019 + relevel(factor(ST), ref = "TX"),
                     data = d_nm)
summary(mod1_nfpc_250)
# FCC 1000/100
mod1_nfpc_1000 <- lm(nonfarmprop_percapita ~ pct1000_100_dec_2019_fcc + pctbachelors_2019 + indstry_diversity + med_income_2019 + 
                       cbsa_min_dist + sk2014 + pop_dense_2019 + digital_distress_2019 + relevel(factor(ST), ref = "TX"),
                      data = d_nm)
summary(mod1_nfpc_1000)
# ACS broadband subscription
mod1_nfpc_acs <- lm(nonfarmprop_percapita ~ pct_fixed_acs_2019 + pctbachelors_2019 + indstry_diversity + med_income_2019 + 
                      cbsa_min_dist + sk2014 + pop_dense_2019 + digital_distress_2019 + relevel(factor(ST), ref = "TX"),
                     data = d_nm)
summary(mod1_nfpc_acs)
# BB QoS
mod1_nfpc_qos <- lm(nonfarmprop_percapita ~ pct_bb_qos + pctbachelors_2019 + indstry_diversity + med_income_2019 + 
                      cbsa_min_dist + sk2014 + pop_dense_2019 + digital_distress_2019 + relevel(factor(ST), ref = "TX"),
                     data = d_nm)
summary(mod1_nfpc_qos)

## Generating Tables ##
stargazer(mod1_nfpc_25, mod1_nfpc_100, mod1_nfpc_250, mod1_nfpc_1000, mod1_nfpc_acs, mod1_nfpc_qos, title = "Nonfarm Proprietors Per Capita Results",
          type = "text", style = "ajs",
          digits = 3, dep.var.caption = "Nonfarm Proprietors Per Capita (2019)",
          align = TRUE, dep.var.labels.include = F, intercept.bottom = F, intercept.top = T,
          covariate.labels = c("(Intercept)",
                               "FCC BBnd (25/3)",
                               "FCC BBnd (100/10)",
                               "FCC BBnd (250/25)",
                               "FCC BBnd (1000/100)",
                               "ACS BBnd Sbscr",
                               "BBnd QoS",
                               "Bachelors degree", 
                               "Industry diversity", "Median Income", "Dist. to Closest Metro", "Social Capital", "Pop. Density", "Digital distress", 
                               "State (KS)", "State (ME)"), no.space = TRUE)

#### Nonemployer per capita Models ####
# FCC 25/3
mod1_nemp_25 <- lm(nonemp_percapita ~ pct25_3_dec_2019_fcc + pctbachelors_2019 + indstry_diversity + med_income_2019 + 
                     cbsa_min_dist + sk2014 + pop_dense_2019 + digital_distress_2019 + relevel(factor(ST), ref = "TX"),
                  data = d_nm)
summary(mod1_nemp_25)
# FCC 100/10
mod1_nemp_100 <- lm(nonemp_percapita ~ pct100_10_dec_2019_fcc + pctbachelors_2019 + indstry_diversity + med_income_2019 + 
                      cbsa_min_dist + sk2014 + pop_dense_2019 + digital_distress_2019 + relevel(factor(ST), ref = "TX"),
                    data = d_nm)
summary(mod1_nemp_100)
# FCC 250/25
mod1_nemp_250 <- lm(nonemp_percapita ~ pct250_25_dec_2019_fcc + pctbachelors_2019 + indstry_diversity + med_income_2019 + 
                      cbsa_min_dist + sk2014 + pop_dense_2019 + digital_distress_2019 + relevel(factor(ST), ref = "TX"),
                    data = d_nm)
summary(mod1_nemp_250)
# FCC 1000/100
mod1_nemp_1000 <- lm(nonemp_percapita ~ pct1000_100_dec_2019_fcc + pctbachelors_2019 + indstry_diversity + med_income_2019 + 
                       cbsa_min_dist + sk2014 + pop_dense_2019 + digital_distress_2019 + relevel(factor(ST), ref = "TX"),
                     data = d_nm)
summary(mod1_nemp_1000)
# ACS broadband subscription
mod1_nemp_acs <- lm(nonemp_percapita ~ pct_fixed_acs_2019 + pctbachelors_2019 + indstry_diversity + med_income_2019 + 
                      cbsa_min_dist + sk2014 + pop_dense_2019 + digital_distress_2019 + relevel(factor(ST), ref = "TX"),
                    data = d_nm)
summary(mod1_nemp_acs)
# BB QoS
mod1_nemp_qos <- lm(nonemp_percapita ~ pct_bb_qos + pctbachelors_2019 + indstry_diversity + med_income_2019 + 
                      cbsa_min_dist + sk2014 + pop_dense_2019 + digital_distress_2019 + relevel(factor(ST), ref = "TX"),
                    data = d_nm)
summary(mod1_nemp_qos)

## Generating Tables ##
stargazer(mod1_nemp_25, mod1_nemp_100, mod1_nemp_250, mod1_nemp_1000, mod1_nemp_acs, mod1_nemp_qos, title = "Nonemployer Establishments Per Capita Results",
          type = "text", style = "ajs",
          digits = 3, dep.var.caption = "Nonemployer Business Establishments Per Capita (2019)",
          align = TRUE, dep.var.labels.include = F, intercept.bottom = F, intercept.top = T,
          covariate.labels = c("(Intercept)",
                               "FCC BBnd (25/3)",
                               "FCC BBnd (100/10)",
                               "FCC BBnd (250/25)",
                               "FCC BBnd (1000/100)",
                               "ACS BBnd Sbscr",
                               "BBnd QoS",
                               "Bachelors degree", 
                               "Industry diversity", "Median Income", "Dist. to Closest Metro", "Social Capital", "Pop. Density", "Digital distress", 
                               "State (KS)", "State (ME)"), no.space = TRUE)

