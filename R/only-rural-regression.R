#### Regression with only rural counties ####

library(tidyverse)
library(ggplot2)

d <- read_csv("https://raw.githubusercontent.com/texastipi/broadband_entrepreneurship/master/Broadband-Entrepreneurship-TXKSME.csv")

summary(d$IRR2010)
summary(d$RUCC_2013)

d %>% filter(ST == "TX") %>% select(IRR2010) %>% summary()
d %>% filter(ST == "KS") %>% select(IRR2010) %>% summary()
d %>% filter(ST == "ME") %>% select(IRR2010) %>% summary()
median(d$IRR2010)

#### Based on IRR2010 ####
## Mean cut

d_irr_rural <- d %>% 
  filter(IRR2010 > mean(IRR2010))

library(stargazer)

## Model 1: DV is Nonfarm Proprietors Share

# Model 1 with FCC 25/3 %
mod.1.FCC.25.3_irr_rural <- lm(pct_nonfarm_bea_2018 ~ pct_genz_2018 + pct_millennial_2018 +
                             pct_genx_2018 + pct_boomers_2018 + pctlessthanhigh_2018 + pctbachelors_2018 +
                             pctgraduate_2018 + indstry_diversity + pct_unemployment_2018 + digital_distress +
                             pct25_3_dec_2019_fcc, data = d_irr_rural)

# Model 1 with FCC 100/10 %
mod.1.FCC.100.10_irr_rural <- lm(pct_nonfarm_bea_2018 ~ pct_genz_2018 + pct_millennial_2018 +
                               pct_genx_2018 + pct_boomers_2018 + pctlessthanhigh_2018 + pctbachelors_2018 +
                               pctgraduate_2018 + indstry_diversity + pct_unemployment_2018 + digital_distress +
                               pct100_10_dec_2019_fcc, data = d_irr_rural)

# Model 1 with FCC 250/25 %
mod.1.FCC.250.25_irr_rural <- lm(pct_nonfarm_bea_2018 ~ pct_genz_2018 + pct_millennial_2018 +
                               pct_genx_2018 + pct_boomers_2018 + pctlessthanhigh_2018 + pctbachelors_2018 +
                               pctgraduate_2018 + indstry_diversity + pct_unemployment_2018 + digital_distress +
                               pct250_25_dec_2019_fcc, data = d_irr_rural)

# Model 1 with FCC 1000/100 %
mod.1.FCC.1000.100_irr_rural <- lm(pct_nonfarm_bea_2018 ~ pct_genz_2018 + pct_millennial_2018 +
                                 pct_genx_2018 + pct_boomers_2018 + pctlessthanhigh_2018 + pctbachelors_2018 +
                                 pctgraduate_2018 + indstry_diversity + pct_unemployment_2018 + digital_distress +
                                 pct1000_100_dec_2019_fcc, data = d_irr_rural)

# Model 1 with ACS Bbnd adoption %
mod.1.ACS_irr_rural <- lm(pct_nonfarm_bea_2018 ~ pct_genz_2018 + pct_millennial_2018 +
                        pct_genx_2018 + pct_boomers_2018 + pctlessthanhigh_2018 + pctbachelors_2018 +
                        pctgraduate_2018 + indstry_diversity + pct_unemployment_2018 + digital_distress +
                        pct_fixed_acs_2018, data = d_irr_rural)

# Model 1 with Bbnd QoS measure
mod.1.QoS_irr_rural <- lm(pct_nonfarm_bea_2018 ~ pct_genz_2018 + pct_millennial_2018 +
                        pct_genx_2018 + pct_boomers_2018 + pctlessthanhigh_2018 + pctbachelors_2018 +
                        pctgraduate_2018 + indstry_diversity + pct_unemployment_2018 + digital_distress +
                        pct_bb_qos, data = d_irr_rural)

## Generate Table
stargazer(mod.1.FCC.25.3_irr_rural, mod.1.FCC.100.10_irr_rural, mod.1.FCC.250.25_irr_rural, mod.1.FCC.1000.100_irr_rural,
          mod.1.ACS_irr_rural, mod.1.QoS_irr_rural, title = "Nonfarm Proprietors Share Regression Results",
          type = "text", style = "ajs",
          digits = 3, dep.var.caption = "Nonfarm Proprietors Share (2018)",
          align = TRUE, dep.var.labels.include = F, intercept.bottom = F, intercept.top = T,
          covariate.labels = c("(Intercept)","GenZ", "Millennial", "GenX", "Boomers",
                               "Less than high school", "Bachelors degree", "Graduate degree",
                               "Industry diversity", "Unemployment", "Digital distress",
                               "FCC BBnd (25/3)",
                               "FCC BBnd (100/10)",
                               "FCC BBnd (250/25)",
                               "FCC BBnd (1000/100)",
                               "ACS BBnd Sbscr",
                               "BBnd QoS"), no.space = TRUE)

## Model 2: DV is Venture Density

# Model 2 with FCC 25/3 %
mod.2.FCC.25.3_irr_rural <- lm(vd_mean_20 ~ pct_genz_2018 + pct_millennial_2018 +
                             pct_genx_2018 + pct_boomers_2018 + pctlessthanhigh_2018 + pctbachelors_2018 +
                             pctgraduate_2018 + indstry_diversity + pct_unemployment_2018 + digital_distress +
                             pct25_3_dec_2019_fcc, data = d_irr_rural)

# Model 2 with FCC 100/10 %
mod.2.FCC.100.10_irr_rural <- lm(vd_mean_20 ~ pct_genz_2018 + pct_millennial_2018 +
                               pct_genx_2018 + pct_boomers_2018 + pctlessthanhigh_2018 + pctbachelors_2018 +
                               pctgraduate_2018 + indstry_diversity + pct_unemployment_2018 + digital_distress +
                               pct100_10_dec_2019_fcc, data = d_irr_rural)

# Model 2 with FCC 250/25 %
mod.2.FCC.250.25_irr_rural <- lm(vd_mean_20 ~ pct_genz_2018 + pct_millennial_2018 +
                               pct_genx_2018 + pct_boomers_2018 + pctlessthanhigh_2018 + pctbachelors_2018 +
                               pctgraduate_2018 + indstry_diversity + pct_unemployment_2018 + digital_distress +
                               pct250_25_dec_2019_fcc, data = d_irr_rural)

# Model 2 with FCC 1000/100 %
mod.2.FCC.1000.100_irr_rural <- lm(vd_mean_20 ~ pct_genz_2018 + pct_millennial_2018 +
                                 pct_genx_2018 + pct_boomers_2018 + pctlessthanhigh_2018 + pctbachelors_2018 +
                                 pctgraduate_2018 + indstry_diversity + pct_unemployment_2018 + digital_distress +
                                 pct1000_100_dec_2019_fcc, data = d_irr_rural)

# Model 2 with ACS Bbnd adoption %
mod.2.ACS_irr_rural <- lm(vd_mean_20 ~ pct_genz_2018 + pct_millennial_2018 +
                        pct_genx_2018 + pct_boomers_2018 + pctlessthanhigh_2018 + pctbachelors_2018 +
                        pctgraduate_2018 + indstry_diversity + pct_unemployment_2018 + digital_distress +
                        pct_fixed_acs_2018, data = d_irr_rural)

# Model 2 with Bbnd QoS measure
mod.2.QoS_irr_rural <- lm(vd_mean_20 ~ pct_genz_2018 + pct_millennial_2018 +
                        pct_genx_2018 + pct_boomers_2018 + pctlessthanhigh_2018 + pctbachelors_2018 +
                        pctgraduate_2018 + indstry_diversity + pct_unemployment_2018 + digital_distress +
                        pct_bb_qos, data = d_irr_rural)

## Generate Table
stargazer(mod.2.FCC.25.3_irr_rural, mod.2.FCC.100.10_irr_rural, mod.2.FCC.250.25_irr_rural, mod.2.FCC.1000.100_irr_rural, mod.2.ACS_irr_rural, mod.2.QoS_irr_rural,
          title = "Average Venture Density Regression Results", type = "text", style = "ajs",
          digits = 3, dep.var.caption = "Average Venture Density (2020)",
          align = TRUE, dep.var.labels.include = F, intercept.bottom = F, intercept.top = T,
          covariate.labels = c("(Intercept)","GenZ", "Millennial", "GenX", "Boomers",
                               "Less than high school", "Bachelors degree", "Graduate degree",
                               "Industry diversity", "Unemployment", "Digital distress",
                               "FCC BBnd (25/3)",
                               "FCC BBnd (100/10)",
                               "FCC BBnd (250/25)",
                               "FCC BBnd (1000/100)",
                               "ACS BBnd Sbscr",
                               "BBnd QoS"), no.space = TRUE)

#### Based on RUCC 2013 ####

d_rucc_rural <- d %>% 
  filter(metro_f == "Nonmetro")

## Model 1: DV is Nonfarm Proprietors Share

# Model 1 with FCC 25/3 %
mod.1.FCC.25.3_rucc_nm <- lm(pct_nonfarm_bea_2018 ~ pct_genz_2018 + pct_millennial_2018 +
                                 pct_genx_2018 + pct_boomers_2018 + pctlessthanhigh_2018 + pctbachelors_2018 +
                                 pctgraduate_2018 + indstry_diversity + pct_unemployment_2018 + digital_distress +
                                 pct25_3_dec_2019_fcc, data = d_rucc_rural)

# Model 1 with FCC 100/10 %
mod.1.FCC.100.10_rucc_nm <- lm(pct_nonfarm_bea_2018 ~ pct_genz_2018 + pct_millennial_2018 +
                                   pct_genx_2018 + pct_boomers_2018 + pctlessthanhigh_2018 + pctbachelors_2018 +
                                   pctgraduate_2018 + indstry_diversity + pct_unemployment_2018 + digital_distress +
                                   pct100_10_dec_2019_fcc, data = d_rucc_rural)

# Model 1 with FCC 250/25 %
mod.1.FCC.250.25_rucc_nm <- lm(pct_nonfarm_bea_2018 ~ pct_genz_2018 + pct_millennial_2018 +
                                   pct_genx_2018 + pct_boomers_2018 + pctlessthanhigh_2018 + pctbachelors_2018 +
                                   pctgraduate_2018 + indstry_diversity + pct_unemployment_2018 + digital_distress +
                                   pct250_25_dec_2019_fcc, data = d_rucc_rural)

# Model 1 with FCC 1000/100 %
mod.1.FCC.1000.100_rucc_nm <- lm(pct_nonfarm_bea_2018 ~ pct_genz_2018 + pct_millennial_2018 +
                                     pct_genx_2018 + pct_boomers_2018 + pctlessthanhigh_2018 + pctbachelors_2018 +
                                     pctgraduate_2018 + indstry_diversity + pct_unemployment_2018 + digital_distress +
                                     pct1000_100_dec_2019_fcc, data = d_rucc_rural)

# Model 1 with ACS Bbnd adoption %
mod.1.ACS_rucc_nm <- lm(pct_nonfarm_bea_2018 ~ pct_genz_2018 + pct_millennial_2018 +
                            pct_genx_2018 + pct_boomers_2018 + pctlessthanhigh_2018 + pctbachelors_2018 +
                            pctgraduate_2018 + indstry_diversity + pct_unemployment_2018 + digital_distress +
                            pct_fixed_acs_2018, data = d_rucc_rural)

# Model 1 with Bbnd QoS measure
mod.1.QoS_rucc_nm <- lm(pct_nonfarm_bea_2018 ~ pct_genz_2018 + pct_millennial_2018 +
                            pct_genx_2018 + pct_boomers_2018 + pctlessthanhigh_2018 + pctbachelors_2018 +
                            pctgraduate_2018 + indstry_diversity + pct_unemployment_2018 + digital_distress +
                            pct_bb_qos, data = d_rucc_rural)

## Generate Table
stargazer(mod.1.FCC.25.3_rucc_nm, mod.1.FCC.100.10_rucc_nm, mod.1.FCC.250.25_rucc_nm, mod.1.FCC.1000.100_rucc_nm,
          mod.1.ACS_rucc_nm, mod.1.QoS_rucc_nm, title = "Nonfarm Proprietors Share Regression Results",
          type = "text", style = "ajs",
          digits = 3, dep.var.caption = "Nonfarm Proprietors Share (2018)",
          align = TRUE, dep.var.labels.include = F, intercept.bottom = F, intercept.top = T,
          covariate.labels = c("(Intercept)","GenZ", "Millennial", "GenX", "Boomers",
                               "Less than high school", "Bachelors degree", "Graduate degree",
                               "Industry diversity", "Unemployment", "Digital distress",
                               "FCC BBnd (25/3)",
                               "FCC BBnd (100/10)",
                               "FCC BBnd (250/25)",
                               "FCC BBnd (1000/100)",
                               "ACS BBnd Sbscr",
                               "BBnd QoS"), no.space = TRUE)

## Model 2: DV is Venture Density

# Model 2 with FCC 25/3 %
mod.2.FCC.25.3_rucc_nm <- lm(vd_mean_20 ~ pct_genz_2018 + pct_millennial_2018 +
                               pct_genx_2018 + pct_boomers_2018 + pctlessthanhigh_2018 + pctbachelors_2018 +
                               pctgraduate_2018 + indstry_diversity + pct_unemployment_2018 + digital_distress +
                               pct25_3_dec_2019_fcc, data = d_rucc_rural)

# Model 2 with FCC 100/10 %
mod.2.FCC.100.10_rucc_nm <- lm(vd_mean_20 ~ pct_genz_2018 + pct_millennial_2018 +
                                 pct_genx_2018 + pct_boomers_2018 + pctlessthanhigh_2018 + pctbachelors_2018 +
                                 pctgraduate_2018 + indstry_diversity + pct_unemployment_2018 + digital_distress +
                                 pct100_10_dec_2019_fcc, data = d_rucc_rural)

# Model 2 with FCC 250/25 %
mod.2.FCC.250.25_rucc_nm <- lm(vd_mean_20 ~ pct_genz_2018 + pct_millennial_2018 +
                                 pct_genx_2018 + pct_boomers_2018 + pctlessthanhigh_2018 + pctbachelors_2018 +
                                 pctgraduate_2018 + indstry_diversity + pct_unemployment_2018 + digital_distress +
                                 pct250_25_dec_2019_fcc, data = d_rucc_rural)

# Model 2 with FCC 1000/100 %
mod.2.FCC.1000.100_rucc_nm <- lm(vd_mean_20 ~ pct_genz_2018 + pct_millennial_2018 +
                                   pct_genx_2018 + pct_boomers_2018 + pctlessthanhigh_2018 + pctbachelors_2018 +
                                   pctgraduate_2018 + indstry_diversity + pct_unemployment_2018 + digital_distress +
                                   pct1000_100_dec_2019_fcc, data = d_rucc_rural)

# Model 2 with ACS Bbnd adoption %
mod.2.ACS_rucc_nm <- lm(vd_mean_20 ~ pct_genz_2018 + pct_millennial_2018 +
                          pct_genx_2018 + pct_boomers_2018 + pctlessthanhigh_2018 + pctbachelors_2018 +
                          pctgraduate_2018 + indstry_diversity + pct_unemployment_2018 + digital_distress +
                          pct_fixed_acs_2018, data = d_rucc_rural)

# Model 2 with Bbnd QoS measure
mod.2.QoS_rucc_nm <- lm(vd_mean_20 ~ pct_genz_2018 + pct_millennial_2018 +
                          pct_genx_2018 + pct_boomers_2018 + pctlessthanhigh_2018 + pctbachelors_2018 +
                          pctgraduate_2018 + indstry_diversity + pct_unemployment_2018 + digital_distress +
                          pct_bb_qos, data = d_rucc_rural)

## Generate Table
stargazer(mod.2.FCC.25.3_rucc_nm, mod.2.FCC.100.10_rucc_nm, mod.2.FCC.250.25_rucc_nm, mod.2.FCC.1000.100_rucc_nm, mod.2.ACS_rucc_nm, mod.2.QoS_rucc_nm,
          title = "Average Venture Density Regression Results", type = "text", style = "ajs",
          digits = 3, dep.var.caption = "Average Venture Density (2020)",
          align = TRUE, dep.var.labels.include = F, intercept.bottom = F, intercept.top = T,
          covariate.labels = c("(Intercept)","GenZ", "Millennial", "GenX", "Boomers",
                               "Less than high school", "Bachelors degree", "Graduate degree",
                               "Industry diversity", "Unemployment", "Digital distress",
                               "FCC BBnd (25/3)",
                               "FCC BBnd (100/10)",
                               "FCC BBnd (250/25)",
                               "FCC BBnd (1000/100)",
                               "ACS BBnd Sbscr",
                               "BBnd QoS"), no.space = TRUE)

