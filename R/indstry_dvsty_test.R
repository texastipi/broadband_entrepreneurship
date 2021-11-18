#### Regression Modeling Testing the Interaction b/w Industry Diversity ####
## Import data and call libraries
library(tidyverse)
library(ggplot2)
library(sjPlot)
set.seed(5000)
options(scipen = 999)

d <- read_csv("https://raw.githubusercontent.com/texastipi/broadband_entrepreneurship/master/Broadband-Entrepreneurship-TXKSME.csv")

## Check the previously fitted regressio models
# The models are saved in the RData environment
summary(mod.1.FCC.25.3)

#### Fitting Models with Additional Interaction Terms ####
## Inspect three way interaction
## How to interpret the three-way interaction: 
## http://www.jeremydawson.co.uk/slopes.htm
## https://stats.idre.ucla.edu/stata/faq/how-can-i-understand-a-3-way-continuous-interaction-stata-12/
## Plotting three-way interaction in R:
## https://cran.r-project.org/web/packages/sjPlot/vignettes/plot_interactions.html
## Broadband x Rurality x Industry Diversity

#### Model 1: DV is Nonfarm Proprietors Share ####

# Model 1 with FCC 25/3 %
mod.1.FCC.25.3.3int <- lm(pct_nonfarm_bea_2019 ~ pct_genz_2019 + pct_millennial_2019 +
                       pct_genx_2019 + pct_boomers_2019 + pctlessthanhigh_2019 + pctbachelors_2019 +
                       pctgraduate_2019 + indstry_diversity + pct_unemployment_2019 + digital_distress_2019 +
                       pct25_3_dec_2019_fcc * IRR2010 * indstry_diversity, data = d)

# Model 1 with FCC 100/10 %
mod.1.FCC.100.10.3int <- lm(pct_nonfarm_bea_2019 ~ pct_genz_2019 + pct_millennial_2019 +
                         pct_genx_2019 + pct_boomers_2019 + pctlessthanhigh_2019 + pctbachelors_2019 +
                         pctgraduate_2019 + indstry_diversity + pct_unemployment_2019 + digital_distress_2019 +
                         pct100_10_dec_2019_fcc * IRR2010 * indstry_diversity, data = d)

# Model 1 with FCC 250/25 %
mod.1.FCC.250.25.3int <- lm(pct_nonfarm_bea_2019 ~ pct_genz_2019 + pct_millennial_2019 +
                         pct_genx_2019 + pct_boomers_2019 + pctlessthanhigh_2019 + pctbachelors_2019 +
                         pctgraduate_2019 + indstry_diversity + pct_unemployment_2019 + digital_distress_2019 +
                         pct250_25_dec_2019_fcc * IRR2010 * indstry_diversity, data = d)

# Model 1 with FCC 1000/100 %
mod.1.FCC.1000.100.3int <- lm(pct_nonfarm_bea_2019 ~ pct_genz_2019 + pct_millennial_2019 +
                           pct_genx_2019 + pct_boomers_2019 + pctlessthanhigh_2019 + pctbachelors_2019 +
                           pctgraduate_2019 + indstry_diversity + pct_unemployment_2019 + digital_distress_2019 +
                           pct1000_100_dec_2019_fcc * IRR2010 * indstry_diversity, data = d)

# Model 1 with ACS Bbnd adoption %
mod.1.ACS.3int <- lm(pct_nonfarm_bea_2019 ~ pct_genz_2019 + pct_millennial_2019 +
                  pct_genx_2019 + pct_boomers_2019 + pctlessthanhigh_2019 + pctbachelors_2019 +
                  pctgraduate_2019 + indstry_diversity + pct_unemployment_2019 + digital_distress_2019 +
                  pct_fixed_acs_2019 * IRR2010 * indstry_diversity, data = d)

# Model 1 with Bbnd QoS measure
mod.1.QoS.3int <- lm(pct_nonfarm_bea_2019 ~ pct_genz_2019 + pct_millennial_2019 +
                  pct_genx_2019 + pct_boomers_2019 + pctlessthanhigh_2019 + pctbachelors_2019 +
                  pctgraduate_2019 + indstry_diversity + pct_unemployment_2019 + digital_distress_2019 +
                  pct_bb_qos * IRR2010 * indstry_diversity, data = d)

# Check summaries
summary(mod.1.FCC.25.3.3int)    # Very significant three-way interaction here
summary(mod.1.FCC.100.10.3int)    # Also quite significant three-way interaction effect
summary(mod.1.FCC.250.25.3int)    # Significant three-way interaction also found
summary(mod.1.FCC.1000.100.3int)    # No significant interaction effects
summary(mod.1.ACS.3int)   # Significant interaction effects
summary(mod.1.QoS.3int)   # No significant interaction effects

## Results table output
library(stargazer)
stargazer(mod.1.FCC.25.3.3int, mod.1.FCC.100.10.3int, mod.1.FCC.250.25.3int, mod.1.FCC.1000.100.3int, mod.1.ACS.3int, mod.1.QoS.3int,
          title = "Nonfarm Proprietors Share Regression Results", type = "html", style = "ajs",
          digits = 3, dep.var.caption = "Nonfarm Proprietors Share (2019)",
          align = TRUE, dep.var.labels.include = F, intercept.bottom = F, intercept.top = T,
          covariate.labels = c("(Intercept)","GenZ", "Millennial", "GenX", "Boomers",
                               "Less than high school", "Bachelors degree", "Graduate degree",
                               "Industry diversity", "Unemployment", "Digital distress",
                               "FCC BBnd (25/3)",
                               "FCC BBnd (100/10)",
                               "FCC BBnd (250/25)",
                               "FCC BBnd (1000/100)",
                               "ACS BBnd Sbscr",
                               "BBnd QoS",
                               "Rurality Index (2010)",
                               "FCC BBnd (25/3) x Rurality",
                               "FCC BBnd (25/3) x Indstry Diversity",
                               "FCC BBnd (100/10) x Rurality",
                               "FCC BBnd (100/10) x Indstry Diversity",
                               "FCC BBnd (250/25) x Rurality",
                               "FCC BBnd (250/25) x Indstry Diversity",
                               "FCC BBnd (1000/100) x Rurality",
                               "FCC BBnd (1000/100) x Indstry Diversity",
                               "ACS BBnd Sbscr x Rurality",
                               "ACS BBnd Sbscr x Indstry Diversity",
                               "BBnd QoS x Rurality",
                               "BBnd QoS x Indstry Diversity",
                               "Indstry Diversity x Rurality",
                               "Ind_D x FCC25_3 x IRR",
                               "Ind_D x FCC100_10 x IRR",
                               "Ind_D x FCC250_25 x IRR",
                               "Ind_D x FCC1000_100 x IRR",
                               "Ind_D x ACS_sbsc x IRR",
                               "Ind_D x QoS x IRR"), no.space = TRUE,
          out = "regression_3way_interaction.doc")


## Plotting the interactions
## Mean and SD to use
## IRR2010: M = 0.54, SD = 0.09
## Industry Diversity: M = 0.87, SD = 0.03
plot_model(mod.1.FCC.25.3.3int, type = "int", mdrt.values = "meansd")
fcc25_3_3way_slope <- plot_model(mod.1.FCC.25.3.3int, type = "pred", terms = c("pct25_3_dec_2019_fcc", "indstry_diversity", "IRR2010"),
                                 title = "", ci.lvl = NA,
                                 axis.title = c("FCC Broadband Availability (25/3Mbps, 2019)", "Share of Nonfarm Proprietorship (%, 2019)"),
                                 legend.title = "Industry Diversity") +
  legend_style(inside = F, pos = "bottom",  base.theme = theme_sjplot()) + 
  scale_color_sjplot(palette = "viridis", labels = c("-1SD", "Mean", "+1SD"))

plot_model(mod.1.FCC.100.10.3int, type = "int", mdrt.values = "meansd")
fcc100_10_3way_slope <- plot_model(mod.1.FCC.100.10.3int, type = "pred", terms = c("pct100_10_dec_2019_fcc", "indstry_diversity", "IRR2010"),
           title = "", ci.lvl = NA,
           axis.title = c("FCC Broadband Availability (100/10Mbps, 2019)", "Share of Nonfarm Proprietorship (%, 2019)"),
           legend.title = "Industry Diversity") +
  legend_style(inside = F, pos = "bottom",  base.theme = theme_sjplot()) + 
  scale_color_sjplot(palette = "viridis", labels = c("-1SD", "Mean", "+1SD"))

plot_model(mod.1.FCC.250.25.3int, type = "int", mdrt.values = "meansd")
fcc_250_25_3way_slope <- plot_model(mod.1.FCC.250.25.3int, type = "pred", terms = c("pct250_25_dec_2019_fcc", "indstry_diversity", "IRR2010"),
           title = "", ci.lvl = NA,
           axis.title = c("FCC Broadband Availability (250/25Mbps, 2019)", "Share of Nonfarm Proprietorship (%, 2019)"),
           legend.title = "Industry Diversity") +
  legend_style(inside = F, pos = "bottom",  base.theme = theme_sjplot()) + 
  scale_color_sjplot(palette = "viridis", labels = c("-1SD", "Mean", "+1SD"))

plot_model(mod.1.ACS.3int, type = "int", mdrt.values = "meansd")
acs_3way_slope <- plot_model(mod.1.ACS.3int, type = "pred", terms = c("pct_fixed_acs_2019", "indstry_diversity", "IRR2010"),
           title = "", ci.lvl = NA,
           axis.title = c("ACS Fixed Broadband Subscription (%, 2019)", "Share of Nonfarm Proprietorship (%, 2019)"),
           legend.title = "Industry Diversity") +
  legend_style(inside = F, pos = "bottom",  base.theme = theme_sjplot()) + 
  scale_color_sjplot(palette = "viridis", labels = c("-1SD", "Mean", "+1SD"))

## Export plots
ggsave("fcc25_3_3way_nf.png",
       plot = fcc25_3_3way_slope, width = 10, height = 6, units = "in", dpi = 600)
ggsave("fcc100_10_3way_nf.png",
       plot = fcc100_10_3way_slope, width = 10, height = 6, units = "in", dpi = 600)
ggsave("fcc250_25_3way_nf.png",
       plot = fcc_250_25_3way_slope, width = 10, height = 6, units = "in", dpi = 600)
ggsave("acs_3way_nf.png",
       plot = acs_3way_slope, width = 10, height = 6, units = "in", dpi = 600)

#### Model 2: DV is Venture Density ####

# Model 2 with FCC 25/3 %
mod.2.FCC.25.3.3int <- lm(vd_mean_20 ~ pct_genz_2019 + pct_millennial_2019 +
                       pct_genx_2019 + pct_boomers_2019 + pctlessthanhigh_2019 + pctbachelors_2019 +
                       pctgraduate_2019 + indstry_diversity + pct_unemployment_2019 + digital_distress_2019 +
                       pct25_3_dec_2019_fcc * IRR2010 * indstry_diversity, data = d)

# Model 2 with FCC 100/10 %
mod.2.FCC.100.10.3int <- lm(vd_mean_20 ~ pct_genz_2019 + pct_millennial_2019 +
                         pct_genx_2019 + pct_boomers_2019 + pctlessthanhigh_2019 + pctbachelors_2019 +
                         pctgraduate_2019 + indstry_diversity + pct_unemployment_2019 + digital_distress_2019 +
                         pct100_10_dec_2019_fcc * IRR2010 * indstry_diversity, data = d)

# Model 2 with FCC 250/25 %
mod.2.FCC.250.25.3int <- lm(vd_mean_20 ~ pct_genz_2019 + pct_millennial_2019 +
                         pct_genx_2019 + pct_boomers_2019 + pctlessthanhigh_2019 + pctbachelors_2019 +
                         pctgraduate_2019 + indstry_diversity + pct_unemployment_2019 + digital_distress_2019 +
                         pct250_25_dec_2019_fcc * IRR2010 * indstry_diversity, data = d)

# Model 2 with FCC 1000/100 %
mod.2.FCC.1000.100.3int <- lm(vd_mean_20 ~ pct_genz_2019 + pct_millennial_2019 +
                           pct_genx_2019 + pct_boomers_2019 + pctlessthanhigh_2019 + pctbachelors_2019 +
                           pctgraduate_2019 + indstry_diversity + pct_unemployment_2019 + digital_distress_2019 +
                           pct1000_100_dec_2019_fcc * IRR2010 * indstry_diversity, data = d)

# Model 2 with ACS Bbnd adoption %
mod.2.ACS.3int <- lm(vd_mean_20 ~ pct_genz_2019 + pct_millennial_2019 +
                  pct_genx_2019 + pct_boomers_2019 + pctlessthanhigh_2019 + pctbachelors_2019 +
                  pctgraduate_2019 + indstry_diversity + pct_unemployment_2019 + digital_distress_2019 +
                  pct_fixed_acs_2019 * IRR2010 * indstry_diversity, data = d)

# Model 2 with Bbnd QoS measure
mod.2.QoS.3int <- lm(vd_mean_20 ~ pct_genz_2019 + pct_millennial_2019 +
                  pct_genx_2019 + pct_boomers_2019 + pctlessthanhigh_2019 + pctbachelors_2019 +
                  pctgraduate_2019 + indstry_diversity + pct_unemployment_2019 + digital_distress_2019 +
                  pct_bb_qos * IRR2010 * indstry_diversity, data = d)

# Check summaries
summary(mod.2.FCC.25.3.3int)    # No significant three-way interaction here
summary(mod.2.FCC.100.10.3int)    # No significant three-way interaction here
summary(mod.2.FCC.250.25.3int)    # No significant three-way interaction. Only interaction b/w diversity and rurality
summary(mod.2.FCC.1000.100.3int)    # No significant three-way interaction. Only interaction b/w diversity and rurality
summary(mod.2.ACS.3int)   # No significant three-way interaction here
summary(mod.2.QoS.3int)   # No significant three-way interaction. Only interaction b/w diversity and rurality

## Results table output
stargazer(mod.2.FCC.25.3.3int, mod.2.FCC.100.10.3int, mod.2.FCC.250.25.3int, mod.2.FCC.1000.100.3int, mod.2.ACS.3int, mod.2.QoS.3int,
          title = "Average Venture Density (2020)", type = "html", style = "ajs",
          digits = 3, dep.var.caption = "Nonfarm Proprietors Share (2019)",
          align = TRUE, dep.var.labels.include = F, intercept.bottom = F, intercept.top = T,
          covariate.labels = c("(Intercept)","GenZ", "Millennial", "GenX", "Boomers",
                               "Less than high school", "Bachelors degree", "Graduate degree",
                               "Industry diversity", "Unemployment", "Digital distress",
                               "FCC BBnd (25/3)",
                               "FCC BBnd (100/10)",
                               "FCC BBnd (250/25)",
                               "FCC BBnd (1000/100)",
                               "ACS BBnd Sbscr",
                               "BBnd QoS",
                               "Rurality Index (2010)",
                               "FCC BBnd (25/3) x Rurality",
                               "FCC BBnd (25/3) x Indstry Diversity",
                               "FCC BBnd (100/10) x Rurality",
                               "FCC BBnd (100/10) x Indstry Diversity",
                               "FCC BBnd (250/25) x Rurality",
                               "FCC BBnd (250/25) x Indstry Diversity",
                               "FCC BBnd (1000/100) x Rurality",
                               "FCC BBnd (1000/100) x Indstry Diversity",
                               "ACS BBnd Sbscr x Rurality",
                               "ACS BBnd Sbscr x Indstry Diversity",
                               "BBnd QoS x Rurality",
                               "BBnd QoS x Indstry Diversity",
                               "Indstry Diversity x Rurality",
                               "Ind_D x FCC25_3 x IRR",
                               "Ind_D x FCC100_10 x IRR",
                               "Ind_D x FCC250_25 x IRR",
                               "Ind_D x FCC1000_100 x IRR",
                               "Ind_D x ACS_sbsc x IRR",
                               "Ind_D x QoS x IRR"), no.space = TRUE,
          out = "regression_3way_interaction_vd.doc")

## Plotting the interactions
## Mean and SD to use
## IRR2010: M = 0.54, SD = 0.09
## Industry Diversity: M = 0.87, SD = 0.03

fcc250_25_int_vd <- plot_model(mod.2.FCC.250.25.3int, type = "pred", terms = c("IRR2010", "indstry_diversity"),
           title = "", ci.lvl = NA,
           axis.title = c("Rurality Index (IRR)", "Venture Density (2020)"),
           legend.title = "Industry Diversity") +
  legend_style(inside = F, pos = "bottom",  base.theme = theme_sjplot()) + 
  scale_color_sjplot(palette = "viridis", labels = c("-1SD", "Mean", "+1SD"))

fcc1000_100_int_vd <- plot_model(mod.2.FCC.1000.100.3int, type = "pred", terms = c("IRR2010", "indstry_diversity"),
           title = "", ci.lvl = NA,
           axis.title = c("Rurality Index (IRR)", "Venture Density (2020)"),
           legend.title = "Industry Diversity") +
  legend_style(inside = F, pos = "bottom",  base.theme = theme_sjplot()) + 
  scale_color_sjplot(palette = "viridis", labels = c("-1SD", "Mean", "+1SD"))

qos_int_vd <- plot_model(mod.2.QoS.3int, type = "pred", terms = c("IRR2010", "indstry_diversity"),
           title = "", ci.lvl = NA,
           axis.title = c("Rurality Index (IRR)", "Venture Density (2020)"),
           legend.title = "Industry Diversity") +
  legend_style(inside = F, pos = "bottom",  base.theme = theme_sjplot()) + 
  scale_color_sjplot(palette = "viridis", labels = c("-1SD", "Mean", "+1SD"))

## Export plots
ggsave("fcc250_25_3way_vd.png",
       plot = fcc250_25_int_vd, width = 10, height = 6, units = "in", dpi = 600)
ggsave("fcc1000_100_3way_vd.png",
       plot = fcc1000_100_int_vd, width = 10, height = 6, units = "in", dpi = 600)
ggsave("qos_3way_vd.png",
       plot = qos_int_vd, width = 10, height = 6, units = "in", dpi = 600)

