#### Regression Outlier Detection ####
set.seed(5000)
library(tidyverse)
library(car)

## Outliers are detected by inspectinv
## 1. Q-Q Plot
## 2. Outlier Test (Bonferroni p)
## 3. Cook's Distance
## 4. Studentized residuals

## Functions in the 'car' packate are helpful

## Workflow:
## Identify outlier using outlierTest(), qqPlot()
## Inspect influencePlot() for influential observations
## Look at avPlots() for influential observations

#### Nonfarm proprietors share model ####
## Broadband 25/3
qqPlot(mod.1.FCC.25.3)  # Obs 204, 325 potential outliers

outlierTest(mod.1.FCC.25.3)   # Obs 325 significant outlier

influenceIndexPlot(mod.1.FCC.25.3)
influencePlot(mod.1.FCC.25.3)   # Obs 272, 252, 208, 204, 325 influential

avPlots(mod.1.FCC.25.3, ask = F, id.method = "identify")  # Obs 331 potentially influential and meaningful

## Broadband 100/10
qqPlot(mod.1.FCC.100.10)  # Obs 204, 325 potential outliers

outlierTest(mod.1.FCC.100.10)   # Obs 325 significant outlier

influenceIndexPlot(mod.1.FCC.100.10)
influencePlot(mod.1.FCC.100.10)   # Obs 272, 252, 208, 204, 325 influential

avPlots(mod.1.FCC.100.10, ask = F, id.method = "identify")  # Obs 355, 256 potentially influential and meaningful

## Broadband 250/25
qqPlot(mod.1.FCC.250.25)  # Obs 126, 325 potential outliers

outlierTest(mod.1.FCC.250.25)   # Obs 325 significant outlier

influenceIndexPlot(mod.1.FCC.250.25)
influencePlot(mod.1.FCC.250.25)   # Obs 126, 208, 252, 272, 325 influential

avPlots(mod.1.FCC.250.25, ask = F, id.method = "identify")  # Obs 213, 23 potentially influential and meaningful

## Broadband 1000/100
qqPlot(mod.1.FCC.1000.100)  # Obs 204, 325 potential outliers

outlierTest(mod.1.FCC.1000.100)   # Obs 325 significant outlier

influenceIndexPlot(mod.1.FCC.1000.100)
influencePlot(mod.1.FCC.1000.100)   # Obs 204, 208, 252, 272, 325 influential

avPlots(mod.1.FCC.1000.100, ask = F, id.method = "identify")  # Obs 46, 105, 229, 152 potentially influential and meaningful

## Broadband adoption
qqPlot(mod.1.ACS)  # Obs 126, 325 potential outliers

outlierTest(mod.1.ACS)   # Obs 325 significant outlier

influenceIndexPlot(mod.1.ACS)
influencePlot(mod.1.ACS)   # Obs 126, 208, 252, 272, 325 influential

avPlots(mod.1.ACS, ask = F, id.method = "identify")  # Obs 152, 229, 176, 190 potentially influential and meaningful

## Broadband QoS
qqPlot(mod.1.QoS)  # Obs 204, 325 potential outliers

outlierTest(mod.1.QoS)   # Obs 325 significant outlier

influenceIndexPlot(mod.1.QoS)
influencePlot(mod.1.QoS)   # Obs 204, 208, 252, 272, 325 influential

avPlots(mod.1.QoS, ask = F, id.method = "identify")  # Obs 164, 341, 277 potentially influential and meaningful

#### Venture Density model ####
## Broadband 25/3
qqPlot(mod.2.FCC.25.3)  # Obs 343, 253 potential outliers

outlierTest(mod.2.FCC.25.3)   # Obs 42, 343, 253, 320, 345 significant outlier

influenceIndexPlot(mod.2.FCC.25.3)
influencePlot(mod.2.FCC.25.3)   # Obs 252, 253, 277, 343 influential

avPlots(mod.2.FCC.25.3, ask = F, id.method = "identify")  # Obs 190, 285, 331 potentially influential and meaningful

## Broadband 100/10
qqPlot(mod.2.FCC.100.10)  # Obs 253, 343 potential outliers

outlierTest(mod.2.FCC.100.10)   # Obs 253, 343, 42, 320, 345 significant outlier

influenceIndexPlot(mod.2.FCC.100.10)
influencePlot(mod.2.FCC.100.10)   # Obs 252, 253, 277, 343 influential

avPlots(mod.2.FCC.100.10, ask = F, id.method = "identify")  # Obs 190, 256, 178 potentially influential and meaningful

## Broadband 250/25
qqPlot(mod.2.FCC.250.25)  # Obs 253, 343 potential outliers

outlierTest(mod.2.FCC.250.25)   # Obs 253, 343, 42, 320, 345, 370 significant outlier

influenceIndexPlot(mod.2.FCC.250.25)
influencePlot(mod.2.FCC.250.25)   # Obs 252, 253, 277, 343 influential

avPlots(mod.2.FCC.250.25, ask = F, id.method = "identify")  # Obs 190, 372, 23, 213, 178 potentially influential and meaningful

## Broadband 1000/100
qqPlot(mod.2.FCC.1000.100)  # Obs 253, 343 potential outliers

outlierTest(mod.2.FCC.1000.100)   # Obs 343, 253, 42, 320, 345 significant outlier

influenceIndexPlot(mod.2.FCC.1000.100)
influencePlot(mod.2.FCC.1000.100)   # Obs 252, 253, 277, 343 influential

avPlots(mod.2.FCC.1000.100, ask = F, id.method = "identify")  # Obs 155, 190, 46, 105, 152, 229 potentially influential and meaningful

## Broadband adoption
qqPlot(mod.2.ACS)  # Obs 253, 343 potential outliers

outlierTest(mod.2.ACS)   # Obs 253, 343, 42, 320, 345 significant outlier

influenceIndexPlot(mod.2.ACS)
influencePlot(mod.2.ACS)   # Obs 252, 253, 277, 343 influential

avPlots(mod.2.ACS, ask = F, id.method = "identify")  # Obs 138, 190, 152, 152, 229, 176 potentially influential and meaningful

## Broadband QoS
qqPlot(mod.2.QoS)  # Obs 253, 343 potential outliers

outlierTest(mod.2.QoS)   # Obs 253, 343, 42, 320, 370, 345 significant outlier

influenceIndexPlot(mod.2.QoS)
influencePlot(mod.2.QoS)   # Obs 252, 253, 277, 343 influential

avPlots(mod.2.QoS, ask = F, id.method = "identify")  # Obs 155, 190, 341, 164 potentially influential and meaningful

## Outlier & Influencer

outlier_influencer <- c(204, 325, 272, 252, 208, 204, 325, 331, 355, 256, 126, 325, 126, 208, 252, 272, 325, 213, 23, 204, 208, 252, 272, 325,
                        46, 105, 229, 152, 126, 325, 126, 208, 252, 272, 325, 152, 229, 176, 190, 204, 208, 252, 272, 325, 164, 341, 277,
                        343, 253, 343, 253, 320, 345, 42, 252, 253, 277, 343, 190, 285, 331, 253, 343, 253, 343, 42, 320, 345, 252, 253, 277, 343,
                        190, 256, 178, 253, 343, 253, 343, 42, 320, 345, 370, 252, 253, 277, 343, 190, 372, 23, 213, 178,
                        253, 343, 343, 253, 42, 320, 345, 252, 253, 277, 343, 155, 190, 46, 105, 152, 229, 253, 343, 253, 343, 42, 320, 345,
                        252, 253, 277, 343, 138, 190, 152, 152, 229, 176, 253, 343, 253, 343, 42, 320, 370, 345, 252, 253, 277, 343,
                        155, 190, 341, 164)
outlier_influencer <- unique(outlier_influencer)

#### Regression Analysis without Outliers ####
library(stargazer)
## Remove outlier observations
d_nooutlier <- d[-outlier_influencer, ]

## Model 1: DV is Nonfarm Proprietors Share

# Model 1 with FCC 25/3 %
mod.1.FCC.25.3_noout <- lm(pct_nonfarm_bea_2018 ~ pct_genz_2018 + pct_millennial_2018 +
                       pct_genx_2018 + pct_boomers_2018 + pctlessthanhigh_2018 + pctbachelors_2018 +
                       pctgraduate_2018 + indstry_diversity + pct_unemployment_2018 + digital_distress +
                       pct25_3_dec_2019_fcc * IRR2010, data = d_nooutlier)

# Model 1 with FCC 100/10 %
mod.1.FCC.100.10_noout <- lm(pct_nonfarm_bea_2018 ~ pct_genz_2018 + pct_millennial_2018 +
                         pct_genx_2018 + pct_boomers_2018 + pctlessthanhigh_2018 + pctbachelors_2018 +
                         pctgraduate_2018 + indstry_diversity + pct_unemployment_2018 + digital_distress +
                         pct100_10_dec_2019_fcc * IRR2010, data = d_nooutlier)

# Model 1 with FCC 250/25 %
mod.1.FCC.250.25_noout <- lm(pct_nonfarm_bea_2018 ~ pct_genz_2018 + pct_millennial_2018 +
                         pct_genx_2018 + pct_boomers_2018 + pctlessthanhigh_2018 + pctbachelors_2018 +
                         pctgraduate_2018 + indstry_diversity + pct_unemployment_2018 + digital_distress +
                         pct250_25_dec_2019_fcc * IRR2010, data = d_nooutlier)

# Model 1 with FCC 1000/100 %
mod.1.FCC.1000.100_noout <- lm(pct_nonfarm_bea_2018 ~ pct_genz_2018 + pct_millennial_2018 +
                           pct_genx_2018 + pct_boomers_2018 + pctlessthanhigh_2018 + pctbachelors_2018 +
                           pctgraduate_2018 + indstry_diversity + pct_unemployment_2018 + digital_distress +
                           pct1000_100_dec_2019_fcc * IRR2010, data = d_nooutlier)

# Model 1 with ACS Bbnd adoption %
mod.1.ACS_noout <- lm(pct_nonfarm_bea_2018 ~ pct_genz_2018 + pct_millennial_2018 +
                  pct_genx_2018 + pct_boomers_2018 + pctlessthanhigh_2018 + pctbachelors_2018 +
                  pctgraduate_2018 + indstry_diversity + pct_unemployment_2018 + digital_distress +
                  pct_fixed_acs_2018 * IRR2010, data = d_nooutlier)

# Model 1 with Bbnd QoS measure
mod.1.QoS_noout <- lm(pct_nonfarm_bea_2018 ~ pct_genz_2018 + pct_millennial_2018 +
                  pct_genx_2018 + pct_boomers_2018 + pctlessthanhigh_2018 + pctbachelors_2018 +
                  pctgraduate_2018 + indstry_diversity + pct_unemployment_2018 + digital_distress +
                  pct_bb_qos * IRR2010, data = d_nooutlier)

## Generate Table
stargazer(mod.1.FCC.25.3_noout, mod.1.FCC.100.10_noout, mod.1.FCC.250.25_noout, mod.1.FCC.1000.100_noout,
          mod.1.ACS_noout, mod.1.QoS_noout, title = "Nonfarm Proprietors Share Regression Results",
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
                               "BBnd QoS",
                               "Rurality Index (2010)",
                               "FCC BBnd (25/3) x Rurality",
                               "FCC BBnd (100/10) x Rurality",
                               "FCC BBnd (250/25) x Rurality",
                               "FCC BBnd (1000/100) x Rurality",
                               "ACS BBnd Sbscr x Rurality",
                               "BBnd QoS x Rurality"), no.space = TRUE)

## Model 2: DV is Venture Density

# Model 2 with FCC 25/3 %
mod.2.FCC.25.3_noout <- lm(vd_mean_20 ~ pct_genz_2018 + pct_millennial_2018 +
                       pct_genx_2018 + pct_boomers_2018 + pctlessthanhigh_2018 + pctbachelors_2018 +
                       pctgraduate_2018 + indstry_diversity + pct_unemployment_2018 + digital_distress +
                       pct25_3_dec_2019_fcc * IRR2010, data = d_nooutlier)

# Model 2 with FCC 100/10 %
mod.2.FCC.100.10_noout <- lm(vd_mean_20 ~ pct_genz_2018 + pct_millennial_2018 +
                         pct_genx_2018 + pct_boomers_2018 + pctlessthanhigh_2018 + pctbachelors_2018 +
                         pctgraduate_2018 + indstry_diversity + pct_unemployment_2018 + digital_distress +
                         pct100_10_dec_2019_fcc * IRR2010, data = d_nooutlier)

# Model 2 with FCC 250/25 %
mod.2.FCC.250.25_noout <- lm(vd_mean_20 ~ pct_genz_2018 + pct_millennial_2018 +
                         pct_genx_2018 + pct_boomers_2018 + pctlessthanhigh_2018 + pctbachelors_2018 +
                         pctgraduate_2018 + indstry_diversity + pct_unemployment_2018 + digital_distress +
                         pct250_25_dec_2019_fcc * IRR2010, data = d_nooutlier)

# Model 2 with FCC 1000/100 %
mod.2.FCC.1000.100_noout <- lm(vd_mean_20 ~ pct_genz_2018 + pct_millennial_2018 +
                           pct_genx_2018 + pct_boomers_2018 + pctlessthanhigh_2018 + pctbachelors_2018 +
                           pctgraduate_2018 + indstry_diversity + pct_unemployment_2018 + digital_distress +
                           pct1000_100_dec_2019_fcc * IRR2010, data = d_nooutlier)

# Model 2 with ACS Bbnd adoption %
mod.2.ACS_noout <- lm(vd_mean_20 ~ pct_genz_2018 + pct_millennial_2018 +
                  pct_genx_2018 + pct_boomers_2018 + pctlessthanhigh_2018 + pctbachelors_2018 +
                  pctgraduate_2018 + indstry_diversity + pct_unemployment_2018 + digital_distress +
                  pct_fixed_acs_2018 * IRR2010, data = d_nooutlier)

# Model 2 with Bbnd QoS measure
mod.2.QoS_noout <- lm(vd_mean_20 ~ pct_genz_2018 + pct_millennial_2018 +
                  pct_genx_2018 + pct_boomers_2018 + pctlessthanhigh_2018 + pctbachelors_2018 +
                  pctgraduate_2018 + indstry_diversity + pct_unemployment_2018 + digital_distress +
                  pct_bb_qos * IRR2010, data = d_nooutlier)

## Generate Table
stargazer(mod.2.FCC.25.3_noout, mod.2.FCC.100.10_noout, mod.2.FCC.250.25_noout, mod.2.FCC.1000.100_noout, mod.2.ACS_noout, mod.2.QoS_noout,
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
                               "BBnd QoS",
                               "Rurality Index (2010)",
                               "FCC BBnd (25/3) x Rurality",
                               "FCC BBnd (100/10) x Rurality",
                               "FCC BBnd (250/25) x Rurality",
                               "FCC BBnd (1000/100) x Rurality",
                               "ACS BBnd Sbscr x Rurality",
                               "BBnd QoS x Rurality"), no.space = TRUE)

#### Plot Interaction ####
library(sjPlot)
plot_model(mod.1.QoS_noout, type = "pred", terms = c("pct_bb_qos", "IRR2010"),
                    title = "", ci.lvl = NA,
                    axis.title = c("Broadband Quality of Service", "Share of Nonfarm Proprietorship (%, 2018)"),
                    legend.title = "Rurality Index (IRR)") + 
  legend_style(inside = F, pos = "bottom",  base.theme = theme_sjplot()) + 
  scale_color_sjplot(palette = "viridis", labels = c("-1SD", "Mean", "+1SD"))

plot_model(mod.2.ACS_noout, type = "pred", terms = c("pct_fixed_acs_2018", "IRR2010"),
                    title = "", ci.lvl = NA,
                    axis.title = c("ACS Fixed Broadband Subscription (%, 2018)", "Venture Density (2020)"),
                    legend.title = "Rurality Index (IRR)") + 
  legend_style(inside = F, pos = "bottom", base.theme = theme_sjplot()) +
  scale_color_sjplot(palette = "viridis", labels = c("-1SD", "Mean", "+1SD"))

plot_model(mod.2.QoS_noout, type = "pred", terms = c("pct_bb_qos", "IRR2010"),
                    title = "", ci.lvl = NA,
                    axis.title = c("Broadband Quality of Service", "Venture Density (2020)"),
                    legend.title = "Rurality Index (IRR)") + 
  legend_style(inside = F, pos = "bottom", base.theme = theme_sjplot()) +
  scale_color_sjplot(palette = "viridis", labels = c("-1SD", "Mean", "+1SD"))



