#install.packages("QuantPsyc")

library(tidyverse)
library(psych)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(sjPlot)
set.seed(5000)

#### Testing some of the regression modeling ideas ####
## Import dataset
rm(list = ls())
d <- read_csv("https://raw.githubusercontent.com/texastipi/broadband_entrepreneurship/master/Broadband-Entrepreneurship-TXKSME.csv")
glimpse(d)

# Calculate Digital Distress Index informed by Gallardo & Geideman (2019) (https://medium.com/design-and-tech-co/digital-distress-what-is-it-and-who-does-it-affect-part-1-e1214f3f209b)
normalize <- function(x, range = c(0,1)) {
  norm <- (range[2] - range[1])*((x - min(x))/(max(x) - min(x))) + range[1]
  return(norm)
} # Function that normalizes the given vector

d <- d %>% mutate(nonfarmprop_per100 = nonfarmprop_percapita * 100,
                  est_50_cbp_per100 = est_50_cbp_percapita * 100,
                  est_10_cbp_per100 = est_10_cbp_percapita * 100,
                  pct_bb_qos_2 = normalize((scale(pct_broadband_MS)[,] + scale(pct_broadband_mlab)[,]),
                                           range = c(0,100))/100)
#### Overview ####
## In this script, we will explore a more complicated regression models incorporating some of the modifications below
## 1. Natural log transformation for some applicable variable
## 2. Adding lagged DV as an IV to control for potential auto-correlation
## 3. Incorporating the speed element of FCC broadband deployment data

#### Dataset Inspection ####

## Natural log transformation is a typical procedure in econometrics to mitigate non-normal distribution of certain variables
## I'll inspect some of the variables and apply log transformation

## First, looking at histograms of variables of interest
#### Main Independent Variables ####
ggd <- ggplot(d)

grid.arrange( # Histograms of broadband measures
  ggd + geom_histogram(aes(x = pct_bb_fcc_2019)),
  ggd + geom_histogram(aes(x = pct_fixed_acs_2018)),
  ggd + geom_histogram(aes(x = pct_bb_qos_2)),
  nrow = 1, ncol = 3
)

grid.arrange( # Histograms of additional IVs
  ggd + geom_histogram(aes(x = digital_distress)),
  ggd + geom_histogram(aes(x = population_2018)),
  ggd + geom_histogram(aes(x = indstry_diversity)),
  ggd + geom_histogram(aes(x = pct_unemployment_2018)),
  ggd + geom_histogram(aes(x = IRR2010)),
  ggd + geom_histogram(aes(x = firm_2017)),
  nrow = 2, ncol = 3
)

# Try log transformation of population and number of firms

grid.arrange( # Histograms of additional IVs with LNs
  ggd + geom_histogram(aes(x = digital_distress)),
  ggd + geom_histogram(aes(x = log(population_2018))),
  ggd + geom_histogram(aes(x = indstry_diversity)),
  ggd + geom_histogram(aes(x = pct_unemployment_2018)),
  ggd + geom_histogram(aes(x = IRR2010)),
  ggd + geom_histogram(aes(x = log(firm_2017))),
  nrow = 2, ncol = 3
)

grid.arrange( # Histograms of education and age group control varibles
  ggd + geom_histogram(aes(x = pctlessthanhigh_2018)),
  ggd + geom_histogram(aes(x = pctbachelors_2018)),
  ggd + geom_histogram(aes(x = pctgraduate_2018)),
  ggd + geom_histogram(aes(x = pct_genz_2018)),
  ggd + geom_histogram(aes(x = pct_millennial_2018)),
  ggd + geom_histogram(aes(x = pct_genx_2018)),
  ggd + geom_histogram(aes(x = pct_boomers_2018)),
  nrow = 2, ncol = 4
)

#### Dependent Variables ####

grid.arrange( # Histograms of traditional small business measures in raw numbers and percentage
  ggd + geom_histogram(aes(x = est_10_cbp_2018)),
  ggd + geom_histogram(aes(x = nonfarmproprietors_bea_2018)),
  ggd + geom_histogram(aes(x = pct_10_est_cbp_2018)),
  ggd + geom_histogram(aes(x = pct_nonfarm_bea_2018)),
  nrow = 1, ncol = 4
)

## Log transforming the raw numbers

grid.arrange( # Histograms of traditional small business measures in LN raw numbers and percentage
  ggd + geom_histogram(aes(x = log(est_10_cbp_2018))),
  ggd + geom_histogram(aes(x = log(nonfarmproprietors_bea_2018))),
  ggd + geom_histogram(aes(x = pct_10_est_cbp_2018)),
  ggd + geom_histogram(aes(x = pct_nonfarm_bea_2018)),
  nrow = 1, ncol = 4
)

grid.arrange( # Histograms of venture density measures
  ggd + geom_histogram(aes(x = vd_mean_20)),
  ggd + geom_histogram(aes(x = havd_mean_20)),
  nrow = 1, ncol = 2
)

## Log transformin to mitigate the skewed distribution

grid.arrange( # Histograms of natural logged venture density measures
  ggd + geom_histogram(aes(x = log(vd_mean_20))),
  ggd + geom_histogram(aes(x = log(havd_mean_20))),
  nrow = 1, ncol = 2
)

grid.arrange( # Histograms of traditional small business measures per 100 people
  ggd + geom_histogram(aes(x = nonfarmprop_per100)),
  ggd + geom_histogram(aes(x = est_10_cbp_per100)),
  ggd + geom_histogram(aes(x = est_10_cbp_per100)),
  nrow = 1, ncol = 3
)

#### Natural Log Transformation ####
## Based on the inspection above, I'll natural log some of the variables:
## 1. population_2018
## 2. firm_2017
## 3. est_10_cbp_2018
## 4. nonfarmproprietors_bea_2018

d <- d %>% mutate(popLN_2018 = log(population_2018),
                  firmLN_2017 = log(firm_2017),
                  est10LN_2018 = log(est_10_cbp_2018),
                  NFpropLN_2018 = log(nonfarmproprietors_bea_2018))

#### Inspecting Correlations b/w Broadband Measures ####
grid.arrange(
  ggplot(d, aes(x = pct_bb_fcc_2019, y = pct_bb_qos_2)) + 
    geom_point(color = "grey10", alpha = 0.7, size = 3) + 
    geom_smooth(method = "lm", color = "darkorange3") + theme_minimal() + 
    theme(axis.text = element_text(size = 11), axis.title = element_text(size = 12, face = "bold")) +
    labs(x = "FCC Broadband", y = "Broadband QoS") +
    scale_x_continuous(labels = scales::percent, breaks = scales::breaks_pretty()) +
    scale_y_continuous(labels = scales::percent, breaks = scales::breaks_pretty()) +
    ggpubr::stat_cor(p.accuracy = 0.001, r.accuracy = 0.01),
  ggplot(d, aes(x = pct_bb_fcc_2019, y = pct_fixed_acs_2018)) +
    geom_point(color = "grey10", alpha = 0.7, size = 3) + 
    geom_smooth(method = "lm", color = "darkorange3") + theme_minimal() +
    theme(axis.text = element_text(size = 11), axis.title = element_text(size = 12, face = "bold")) +
    labs(x = "FCC Broadband", y = "ACS Fixed BB Subscription") +
    scale_x_continuous(labels = scales::percent, breaks = scales::breaks_pretty()) +
    scale_y_continuous(labels = scales::percent, breaks = scales::breaks_pretty()) +
    ggpubr::stat_cor(p.accuracy = 0.001, r.accuracy = 0.01),
  ggplot(d, aes(x = pct_bb_qos_2, y = pct_fixed_acs_2018)) + 
    geom_point(color = "grey10", alpha = 0.7, size = 3) + 
    geom_smooth(method = "lm", color = "darkorange3") + theme_minimal() +
    theme(axis.text = element_text(size = 11), axis.title = element_text(size = 12, face = "bold")) +
    labs(x = "Microsoft Broadband", y = "ACS Fixed BB Subscription") +
    scale_x_continuous(labels = scales::percent, breaks = scales::breaks_pretty()) +
    scale_y_continuous(labels = scales::percent, breaks = scales::breaks_pretty()) +
    ggpubr::stat_cor(p.accuracy = 0.001, r.accuracy = 0.01),
  nrow = 1, ncol = 3, top = grid::textGrob("Correlations b/w Broadband Measures (Texas)",
                                           gp = grid::gpar(fontface="bold",fontsize=14))
)

#### Regression Modeling ####

## Model 1: DV is Nonfarm Proprietors Share
##          Each broadband variable was introduced separately as they are highly correlated
##          IVs: FCC broadband, Broadband adoption, Broadband QoS, Digital Distress index
##          Controls: Population (LN, 2018), Age cohorts, Education attainment, lagged DV (1y),
##                    Number of firms (LN, 2017), industry diversity index, unemployment rate, rurality

# Model 1 with FCC 25/3 %
mod.1.FCC.25.3 <- lm(pct_nonfarm_bea_2018 ~ pct_genz_2018 + pct_millennial_2018 +
                  pct_genx_2018 + pct_boomers_2018 + pctlessthanhigh_2018 + pctbachelors_2018 +
                  pctgraduate_2018 + indstry_diversity + pct_unemployment_2018 + digital_distress +
                  pct_bb_fcc_2019 * IRR2010, data = d)
summary(mod.1.FCC.25.3)

# Model 1 with FCC 50/5 %
mod.1.FCC.50.5 <- lm(pct_nonfarm_bea_2018 ~ pct_genz_2018 + pct_millennial_2018 +
                       pct_genx_2018 + pct_boomers_2018 + pctlessthanhigh_2018 + pctbachelors_2018 +
                       pctgraduate_2018 + indstry_diversity + pct_unemployment_2018 + digital_distress +
                       pct_bb_fcc_50_5_2019 * IRR2010, data = d)
summary(mod.1.FCC.50.5)

# Model 1 with FCC 100/10 %
mod.1.FCC.100.10 <- lm(pct_nonfarm_bea_2018 ~ pct_genz_2018 + pct_millennial_2018 +
                       pct_genx_2018 + pct_boomers_2018 + pctlessthanhigh_2018 + pctbachelors_2018 +
                       pctgraduate_2018 + indstry_diversity + pct_unemployment_2018 + digital_distress +
                       pct_bb_fcc_100_10_2019 * IRR2010, data = d)
summary(mod.1.FCC.100.10)

plot_model(mod.1.FCC.100.10, type = "pred", terms = c("pct_bb_fcc_100_10_2019", "IRR2010"),
           title = "",
           axis.title = c("FCC Broadband Availability (100/10Mbps, 2019)", "Share of Nonfarm Proprietorship (%, 2018)"),
           legend.title = "Rurality Index (IRR)")

# Model 1 with FCC 250/25 %
mod.1.FCC.250.25 <- lm(pct_nonfarm_bea_2018 ~ pct_genz_2018 + pct_millennial_2018 +
                       pct_genx_2018 + pct_boomers_2018 + pctlessthanhigh_2018 + pctbachelors_2018 +
                       pctgraduate_2018 + indstry_diversity + pct_unemployment_2018 + digital_distress +
                       pct_bb_fcc_250_25_2019 * IRR2010, data = d)
summary(mod.1.FCC.250.25)

plot_model(mod.1.FCC.250.25, type = "pred", terms = c("pct_bb_fcc_250_25_2019", "IRR2010"),
           title = "",
           axis.title = c("FCC Broadband Availability (250/25Mbps, 2019)", "Share of Nonfarm Proprietorship (%, 2018)"),
           legend.title = "Rurality Index (IRR)")

# Model 1 with ACS Bbnd adoption %
mod.1.ACS <- lm(pct_nonfarm_bea_2018 ~ pct_genz_2018 + pct_millennial_2018 +
                  pct_genx_2018 + pct_boomers_2018 + pctlessthanhigh_2018 + pctbachelors_2018 +
                  pctgraduate_2018 + indstry_diversity + pct_unemployment_2018 + digital_distress +
                  pct_fixed_acs_2018 * IRR2010, data = d)
summary(mod.1.ACS)

# Model 1 with Bbnd QoS measure
mod.1.QoS <- lm(pct_nonfarm_bea_2018 ~ pct_genz_2018 + pct_millennial_2018 +
                  pct_genx_2018 + pct_boomers_2018 + pctlessthanhigh_2018 + pctbachelors_2018 +
                  pctgraduate_2018 + indstry_diversity + pct_unemployment_2018 + digital_distress +
                  pct_bb_qos_2 * IRR2010, data = d)
summary(mod.1.QoS)

## Model 2: DV is Venture Density
##          Each broadband variable was introduced separately as they are highly correlated
##          IVs: FCC broadband, Broadband adoption, Broadband QoS, Digital Distress index
##          Controls: Population (LN, 2018), Age cohorts, Education attainment, lagged DV (1y),
##                    Number of firms (LN, 2017), industry diversity index, unemployment rate, rurality

# Model 2 with FCC 25/3 %
mod.2.FCC.25.3 <- lm(vd_mean_20 ~ pct_genz_2018 + pct_millennial_2018 +
                       pct_genx_2018 + pct_boomers_2018 + pctlessthanhigh_2018 + pctbachelors_2018 +
                       pctgraduate_2018 + indstry_diversity + pct_unemployment_2018 + digital_distress +
                       pct_bb_fcc_2019 * IRR2010, data = d)
summary(mod.2.FCC.25.3)

plot_model(mod.2.FCC.25.3, type = "pred", terms = c("pct_bb_fcc_2019", "IRR2010"),
           title = "",
           axis.title = c("FCC Broadband Availability (25/3Mbps, 2019)", "Venture Density (2020)"),
           legend.title = "Rurality Index (IRR)")

# Model 2 with FCC 50/5 %
mod.2.FCC.50.5 <- lm(vd_mean_20 ~ pct_genz_2018 + pct_millennial_2018 +
                       pct_genx_2018 + pct_boomers_2018 + pctlessthanhigh_2018 + pctbachelors_2018 +
                       pctgraduate_2018 + indstry_diversity + pct_unemployment_2018 + digital_distress +
                       pct_bb_fcc_50_5_2019 * IRR2010, data = d)
summary(mod.2.FCC.50.5)

plot_model(mod.2.FCC.50.5, type = "pred", terms = c("pct_bb_fcc_50_5_2019", "IRR2010"),
           title = "",
           axis.title = c("FCC Broadband Availability (50/5Mbps, 2019)", "Venture Density (2020)"),
           legend.title = "Rurality Index (IRR)")

# Model 2 with FCC 100/10 %
mod.2.FCC.100.10 <- lm(vd_mean_20 ~ pct_genz_2018 + pct_millennial_2018 +
                         pct_genx_2018 + pct_boomers_2018 + pctlessthanhigh_2018 + pctbachelors_2018 +
                         pctgraduate_2018 + indstry_diversity + pct_unemployment_2018 + digital_distress +
                         pct_bb_fcc_100_10_2019 * IRR2010, data = d)
summary(mod.2.FCC.100.10)

plot_model(mod.2.FCC.100.10, type = "pred", terms = c("pct_bb_fcc_100_10_2019", "IRR2010"),
           title = "",
           axis.title = c("FCC Broadband Availability (100/10Mbps, 2019)", "Venture Density (2020)"),
           legend.title = "Rurality Index (IRR)")

# Model 2 with FCC 250/25 %
mod.2.FCC.250.25 <- lm(vd_mean_20 ~ pct_genz_2018 + pct_millennial_2018 +
                         pct_genx_2018 + pct_boomers_2018 + pctlessthanhigh_2018 + pctbachelors_2018 +
                         pctgraduate_2018 + indstry_diversity + pct_unemployment_2018 + digital_distress +
                         pct_bb_fcc_250_25_2019 * IRR2010, data = d)
summary(mod.2.FCC.250.25)

plot_model(mod.2.FCC.250.25, type = "pred", terms = c("pct_bb_fcc_250_25_2019", "IRR2010"),
           title = "",
           axis.title = c("FCC Broadband Availability (250/25Mbps, 2019)", "Venture Density (2020)"),
           legend.title = "Rurality Index (IRR)")

# Model 2 with ACS Bbnd adoption %
mod.2.ACS <- lm(vd_mean_20 ~ pct_genz_2018 + pct_millennial_2018 +
                  pct_genx_2018 + pct_boomers_2018 + pctlessthanhigh_2018 + pctbachelors_2018 +
                  pctgraduate_2018 + indstry_diversity + pct_unemployment_2018 + digital_distress +
                  pct_fixed_acs_2018 * IRR2010, data = d)
summary(mod.2.ACS)

plot_model(mod.2.ACS, type = "pred", terms = c("pct_fixed_acs_2018", "IRR2010"),
           title = "",
           axis.title = c("ACS Fixed Broadband Subscription (%, 2018)", "Venture Density (2020)"),
           legend.title = "Rurality Index (IRR)")

# Model 2 with Bbnd QoS measure
mod.2.QoS <- lm(vd_mean_20 ~ pct_genz_2018 + pct_millennial_2018 +
                  pct_genx_2018 + pct_boomers_2018 + pctlessthanhigh_2018 + pctbachelors_2018 +
                  pctgraduate_2018 + indstry_diversity + pct_unemployment_2018 + digital_distress +
                  pct_bb_qos_2 * IRR2010, data = d)
summary(mod.2.QoS)

plot_model(mod.2.QoS, type = "pred", terms = c("pct_bb_qos_2", "IRR2010"),
           title = "",
           axis.title = c("Broadband Quality of Service", "Venture Density (2020)"),
           legend.title = "Rurality Index (IRR)")
