library(tidyverse)
library(psych)
library(ggplot2)
library(gridExtra)
library(ggpubr)
set.seed(5000)

#### Testing some of the regression modeling ideas ####
## Import dataset
d <- read_csv("https://raw.githubusercontent.com/texastipi/broadband_entrepreneurship/master/Broadband-Entrepreneurship-TXKSME.csv")
d <- d %>% mutate(pct_fixed_acs_2018 = pct_fixed_acs_2018/100)
str(d)
colnames(d)

#### Explore some of the variables ####

## Framework for understanding various broadband measures:
## 1. Availability: FCC availability
## 2. Adoption: ACS subscription
## 3. Quality of service: Microsoft & M-Lab test reporting

## Instead of looking at the broadband measures as it is, here I try to argue that what's important is
## examining the correlation b/w availability and other subscription or QoS measures.
## Having broadband available is not enough (quite widely accepted notion), it's how it is transferred to adoption and
## appropriate quality of service. One way of looking at this is to use the correlation between these measures as
## independent variables

## First I want to see whether we could integrate M-Lab and Microsoft
bb_qos <- d %>% select(pct_broadband_MS, pct_broadband_mlab)
alpha(bb_qos)
# Alpha is 0.82, the correlation is 0.7. I'll create a composite score of the two
bb_qos_score <- scoreItems(keys = c(1:length(bb_qos)), impute = "mean", items = bb_qos)
# Add the score to the dataset
d$pct_bb_qos <- bb_qos_score$scores[,]
# Explore the distribution
grid.arrange(ggplot(d, aes(x = pct_broadband_MS)) + geom_histogram(),
             ggplot(d, aes(x = pct_broadband_mlab)) + geom_histogram(),
             ggplot(d, aes(x = pct_bb_qos)) + geom_histogram(),
             nrow = 1, ncol = 3)
# Explore the correlation b/w M-Lab, MS, and the composite score
grid.arrange(ggscatter(d, x = "pct_broadband_MS", y = "pct_broadband_mlab",
                       add = "reg.line", conf.int = T, cor.coef = T, cor.method = "pearson"),
             ggscatter(d, x = "pct_broadband_MS", y = "pct_bb_qos",
                       add = "reg.line", conf.int = T, cor.coef = T, cor.method = "pearson"),
             ggscatter(d, x = "pct_broadband_mlab", y = "pct_bb_qos",
                       add = "reg.line", conf.int = T, cor.coef = T, cor.method = "pearson"),
             nrow = 1, ncol = 3)
# Descriptive statistics
describe(select(d, pct_broadband_MS, pct_broadband_mlab, pct_bb_qos))

## Secondly I'll explore correlation b/w availability (FCC), adoption (Subscription), and QoS (composite score) in general
describe(select(d, pct_bb_fcc_2019, pct_fixed_acs_2018, pct_bb_qos))

grid.arrange(ggscatter(d, x = "pct_bb_fcc_2019", y = "pct_fixed_acs_2018",
                       add = "reg.line", conf.int = T, cor.coef = T, cor.method = "pearson"),
             ggscatter(d, x = "pct_bb_fcc_2019", y = "pct_bb_qos",
                       add = "reg.line", conf.int = T, cor.coef = T, cor.method = "pearson"),
             ggscatter(d, x = "pct_fixed_acs_2018", y = "pct_bb_qos",
                       add = "reg.line", conf.int = T, cor.coef = T, cor.method = "pearson"),
             nrow = 1, ncol = 3)

# Standardizing the broadband measures
d <- d %>% mutate(s.pct_bb_fcc_2019 = scale(pct_bb_fcc_2019),
             s.pct_fixed_acs_2018 = scale(pct_fixed_acs_2018),
             s.pct_bb_qos = scale(pct_bb_qos))

grid.arrange(ggscatter(d, x = "s.pct_bb_fcc_2019", y = "s.pct_fixed_acs_2018",
                       add = "reg.line", conf.int = T, cor.coef = T, cor.method = "pearson"),
             ggscatter(d, x = "s.pct_bb_fcc_2019", y = "s.pct_bb_qos",
                       add = "reg.line", conf.int = T, cor.coef = T, cor.method = "pearson"),
             ggscatter(d, x = "s.pct_fixed_acs_2018", y = "s.pct_bb_qos",
                       add = "reg.line", conf.int = T, cor.coef = T, cor.method = "pearson"),
             nrow = 1, ncol = 3)

# Create an index averaging broadband measures
d <- d %>% mutate(bb_composite = (pct_bb_fcc_2019 + pct_fixed_acs_2018 + pct_bb_qos)/3,
                  bb_composite_w = (pct_bb_fcc_2019*.25) + (pct_fixed_acs_2018*.25) + (pct_bb_qos*.5))

describe(select(d, pct_bb_fcc_2019, pct_fixed_acs_2018, pct_bb_qos, s.pct_bb_fcc_2019, s.pct_fixed_acs_2018, s.pct_bb_qos,
                bb_composite, bb_composite_w))

## Although significant correlations, there seems to be quite a discrepencies b/w the measures
## I'll now take a look at the difference b/w the measures
## Differences b/w the measures will represent how availability is not translated into QoS for each county
## I calculate first the raw difference by simply substracting the two numbers
## Secondly, I calculate percentage difference b/w the two measures by dividing the absolute difference by their average

d <- d %>% mutate(bb_avail_adopt.raw = (pct_bb_fcc_2019 - pct_fixed_acs_2018),
                  bb_avail_qos.raw = (pct_bb_fcc_2019 - pct_bb_qos),
                  bb_adopt_qos.raw = (pct_fixed_acs_2018 - pct_bb_qos),
                  bb_avail_adopt.rs = scales::rescale((s.pct_bb_fcc_2019 - s.pct_fixed_acs_2018)),
                  bb_avail_qos.rs = scales::rescale((s.pct_bb_fcc_2019 - s.pct_bb_qos)),
                  bb_adopt_qos.rs = scales::rescale((s.pct_fixed_acs_2018 - s.pct_bb_qos)),
                  bb_avail_adopt.pct = abs((pct_bb_fcc_2019 - pct_fixed_acs_2018))/((pct_bb_fcc_2019 + pct_fixed_acs_2018)/2),
                  bb_avail_qos.pct = abs((pct_bb_fcc_2019 - pct_bb_qos))/((pct_bb_fcc_2019 + pct_bb_qos)/2),
                  bb_adopt_qos.pct = abs((pct_fixed_acs_2018 - pct_bb_qos))/((pct_fixed_acs_2018 + pct_bb_qos)/2))
# Descriptive statistics
describe(select(d, bb_avail_adopt.raw, bb_avail_qos.raw, bb_adopt_qos.raw,
                bb_avail_adopt.pct, bb_avail_qos.pct, bb_adopt_qos.pct,
                bb_avail_adopt.rs, bb_avail_qos.rs, bb_adopt_qos.rs))
# Examine how the differences correlate according to rurality
# Q: Do rural areas tend to expereince greater difference in availability and QoS, etc.?

# Raw substractions

ggscatter(d, x = "IRR2010", y = "bb_avail_qos.raw",
          add = "reg.line", conf.int = T, cor.coef = T, cor.method = "pearson")
ggscatter(d, x = "IRR2010", y = "bb_avail_adopt.raw",
          add = "reg.line", conf.int = T, cor.coef = T, cor.method = "pearson")
ggscatter(d, x = "IRR2010", y = "bb_adopt_qos.raw",
          add = "reg.line", conf.int = T, cor.coef = T, cor.method = "pearson")

# Standardized differences rescaled to 0-1

ggscatter(d, x = "IRR2010", y = "bb_avail_qos.rs",
          add = "reg.line", conf.int = T, cor.coef = T, cor.method = "pearson")
ggscatter(d, x = "IRR2010", y = "bb_avail_adopt.rs",
          add = "reg.line", conf.int = T, cor.coef = T, cor.method = "pearson")
ggscatter(d, x = "IRR2010", y = "bb_adopt_qos.rs",
          add = "reg.line", conf.int = T, cor.coef = T, cor.method = "pearson")

# Difference in percentages

ggscatter(d, x = "IRR2010", y = "bb_avail_qos.pct",
          add = "reg.line", conf.int = T, cor.coef = T, cor.method = "pearson")
ggscatter(d, x = "IRR2010", y = "bb_avail_adopt.pct",
          add = "reg.line", conf.int = T, cor.coef = T, cor.method = "pearson")
ggscatter(d, x = "IRR2010", y = "bb_adopt_qos.pct",
          add = "reg.line", conf.int = T, cor.coef = T, cor.method = "pearson")

## We can see that rural areas tend to experience larger discrepancies b/w availability, adoption, and QoS

#### Regression Modeling ####
## See whether the composite QoS variables work inside our previous models ##

mod1.1 <- lm(pct_nonfarm_bea_2018 ~ pct_bb_fcc_2019 + pct_fixed_acs_2018 + pct_bb_qos + IRR2010 + 
             pctagriculture_2019 + pctconstruction_2019 + pctwholesale_2019 + pctretail_2019 + pcttransportation_2019 + 
             pctinformation_2019 + pctfinance_2019 + pctprofessional_2019 + pctedu_healthcare_social_2019 + pctother_occupation_2019 + 
             pctpublic_admin_2019 + indstry_diversity + pctlessthanhigh_2019 + pctbachelors_2019 + pctgraduate_2019 + pctmilennial_2019 + 
             pctgenx_2019 + pctboomers_2019,
           data = d)
summary(mod1.1)

mod2.1 <- lm(pct_chg_bea_2012_2018 ~ pct_bb_fcc_2019 + pct_fixed_acs_2018 + pct_bb_qos + IRR2010 + 
             pctagriculture_2019 + pctconstruction_2019 + pctwholesale_2019 + pctretail_2019 + pcttransportation_2019 + 
             pctinformation_2019 + pctfinance_2019 + pctprofessional_2019 + pctedu_healthcare_social_2019 + pctother_occupation_2019 + 
             pctpublic_admin_2019 + indstry_diversity + pctlessthanhigh_2019 + pctbachelors_2019 + pctgraduate_2019 + pctmilennial_2019 + 
             pctgenx_2019 + pctboomers_2019,
           data = d)
summary(mod2.1)

mod3.1 <- lm(venturedensity_mean ~ pct_bb_fcc_2019 + pct_fixed_acs_2018 + pct_bb_qos + IRR2010 + 
               pctagriculture_2019 + pctconstruction_2019 + pctwholesale_2019 + pctretail_2019 + pcttransportation_2019 + 
               pctinformation_2019 + pctfinance_2019 + pctprofessional_2019 + pctedu_healthcare_social_2019 + pctother_occupation_2019 + 
               pctpublic_admin_2019 + indstry_diversity + pctlessthanhigh_2019 + pctbachelors_2019 + pctgraduate_2019 + pctmilennial_2019 + 
               pctgenx_2019 + pctboomers_2019,
             data = d)
summary(mod3.1)

## Explore models with the difference b/w broadband measures as IVs
# Percentage difference
mod1.2 <- lm(pct_nonfarm_bea_2018 ~ bb_avail_qos.pct + bb_avail_adopt.pct + bb_adopt_qos.pct + IRR2010 + 
               pctagriculture_2019 + pctconstruction_2019 + pctwholesale_2019 + pctretail_2019 + pcttransportation_2019 + 
               pctinformation_2019 + pctfinance_2019 + pctprofessional_2019 + pctedu_healthcare_social_2019 + pctother_occupation_2019 + 
               pctpublic_admin_2019 + indstry_diversity + pctlessthanhigh_2019 + pctbachelors_2019 + pctgraduate_2019 + pctmilennial_2019 + 
               pctgenx_2019 + pctboomers_2019,
             data = d)
summary(mod1.2)

mod2.2 <- lm(pct_chg_bea_2012_2018 ~ bb_avail_qos.pct + bb_avail_adopt.pct + bb_adopt_qos.pct + IRR2010 + 
               pctagriculture_2019 + pctconstruction_2019 + pctwholesale_2019 + pctretail_2019 + pcttransportation_2019 + 
               pctinformation_2019 + pctfinance_2019 + pctprofessional_2019 + pctedu_healthcare_social_2019 + pctother_occupation_2019 + 
               pctpublic_admin_2019 + indstry_diversity + pctlessthanhigh_2019 + pctbachelors_2019 + pctgraduate_2019 + pctmilennial_2019 + 
               pctgenx_2019 + pctboomers_2019,
             data = d)
summary(mod2.2)

mod3.2 <- lm(venturedensity_mean ~ bb_avail_qos.pct + bb_avail_adopt.pct + bb_adopt_qos.pct + IRR2010 + 
               pctagriculture_2019 + pctconstruction_2019 + pctwholesale_2019 + pctretail_2019 + pcttransportation_2019 + 
               pctinformation_2019 + pctfinance_2019 + pctprofessional_2019 + pctedu_healthcare_social_2019 + pctother_occupation_2019 + 
               pctpublic_admin_2019 + indstry_diversity + pctlessthanhigh_2019 + pctbachelors_2019 + pctgraduate_2019 + pctmilennial_2019 + 
               pctgenx_2019 + pctboomers_2019,
             data = d)
summary(mod3.2)
# Raw difference
mod1.3 <- lm(pct_nonfarm_bea_2018 ~ bb_avail_qos.raw + bb_avail_adopt.raw + bb_adopt_qos.raw + IRR2010 + 
               pctagriculture_2019 + pctconstruction_2019 + pctwholesale_2019 + pctretail_2019 + pcttransportation_2019 + 
               pctinformation_2019 + pctfinance_2019 + pctprofessional_2019 + pctedu_healthcare_social_2019 + pctother_occupation_2019 + 
               pctpublic_admin_2019 + indstry_diversity + pctlessthanhigh_2019 + pctbachelors_2019 + pctgraduate_2019 + pctmilennial_2019 + 
               pctgenx_2019 + pctboomers_2019,
             data = d)
summary(mod1.3)

mod2.3 <- lm(pct_chg_bea_2012_2018 ~ bb_avail_qos.raw + bb_avail_adopt.raw + bb_adopt_qos.raw + IRR2010 + 
               pctagriculture_2019 + pctconstruction_2019 + pctwholesale_2019 + pctretail_2019 + pcttransportation_2019 + 
               pctinformation_2019 + pctfinance_2019 + pctprofessional_2019 + pctedu_healthcare_social_2019 + pctother_occupation_2019 + 
               pctpublic_admin_2019 + indstry_diversity + pctlessthanhigh_2019 + pctbachelors_2019 + pctgraduate_2019 + pctmilennial_2019 + 
               pctgenx_2019 + pctboomers_2019,
             data = d)
summary(mod2.3)

mod3.3 <- lm(venturedensity_mean ~ bb_avail_qos.raw + bb_avail_adopt.raw + bb_adopt_qos.raw + IRR2010 + 
               pctagriculture_2019 + pctconstruction_2019 + pctwholesale_2019 + pctretail_2019 + pcttransportation_2019 + 
               pctinformation_2019 + pctfinance_2019 + pctprofessional_2019 + pctedu_healthcare_social_2019 + pctother_occupation_2019 + 
               pctpublic_admin_2019 + indstry_diversity + pctlessthanhigh_2019 + pctbachelors_2019 + pctgraduate_2019 + pctmilennial_2019 + 
               pctgenx_2019 + pctboomers_2019,
             data = d)
summary(mod3.3)

## Explore the composite index variable 

mod1.4 <- lm(pct_nonfarm_bea_2018 ~ bb_composite_w + IRR2010 + 
               pctagriculture_2019 + pctconstruction_2019 + pctwholesale_2019 + pctretail_2019 + pcttransportation_2019 + 
               pctinformation_2019 + pctfinance_2019 + pctprofessional_2019 + pctedu_healthcare_social_2019 + pctother_occupation_2019 + 
               pctpublic_admin_2019 + indstry_diversity + pctlessthanhigh_2019 + pctbachelors_2019 + pctgraduate_2019 + pctmilennial_2019 + 
               pctgenx_2019 + pctboomers_2019,
             data = d)
summary(mod1.4)

mod2.4 <- lm(pct_chg_bea_2012_2018 ~ bb_composite_w + IRR2010 + 
               pctagriculture_2019 + pctconstruction_2019 + pctwholesale_2019 + pctretail_2019 + pcttransportation_2019 + 
               pctinformation_2019 + pctfinance_2019 + pctprofessional_2019 + pctedu_healthcare_social_2019 + pctother_occupation_2019 + 
               pctpublic_admin_2019 + indstry_diversity + pctlessthanhigh_2019 + pctbachelors_2019 + pctgraduate_2019 + pctmilennial_2019 + 
               pctgenx_2019 + pctboomers_2019,
             data = d)
summary(mod2.4)

mod3.4 <- lm(venturedensity_mean ~ bb_composite_w + IRR2010 + 
               pctagriculture_2019 + pctconstruction_2019 + pctwholesale_2019 + pctretail_2019 + pcttransportation_2019 + 
               pctinformation_2019 + pctfinance_2019 + pctprofessional_2019 + pctedu_healthcare_social_2019 + pctother_occupation_2019 + 
               pctpublic_admin_2019 + indstry_diversity + pctlessthanhigh_2019 + pctbachelors_2019 + pctgraduate_2019 + pctmilennial_2019 + 
               pctgenx_2019 + pctboomers_2019,
             data = d)
summary(mod3.4)




