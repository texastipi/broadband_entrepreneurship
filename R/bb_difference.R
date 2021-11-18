#### Inspecting Correlation b/w Broadband Measures ####
library(tidyverse)
library(ggplot2)
library(ggpubr)

#### Calculate difference index ####
# 1. Standardize each broadband measure
# 2. Calculate the difference
# 3. Rescale (min-max normalize) to ragne from 0 to 1

d_bbcor <- d %>% 
  mutate(bb_avail_adoption.r = normalize(scale(pct25_3_dec_2019_fcc) - scale(pct_fixed_acs_2019)),
         bb_avail_qos.r = normalize(scale(pct25_3_dec_2019_fcc) - scale(pct_bb_qos)),
         bb_adoption_qos.r = normalize(scale(pct_fixed_acs_2019) - scale(pct_bb_qos)))

# Check summary
d_bbcor %>% select(bb_avail_adoption.r, bb_avail_qos.r, bb_adoption_qos.r) %>% summary()

#### Draw plots ####
## Availability-Adoption
ggscatter(d_bbcor, x = "IRR2010", y = "bb_avail_adoption.r",
          add = "reg.line",
          conf.int = T) +
  stat_cor(method = "pearson", label.x = 0, label.y = 1)

## Availability-QoS
ggscatter(d_bbcor, x = "IRR2010", y = "bb_avail_qos.r",
          add = "reg.line",
          conf.int = T) +
  stat_cor(method = "pearson", label.x = 0, label.y = 1)

## Adoption-QoS
ggscatter(d_bbcor, x = "IRR2010", y = "bb_adoption_qos.r",
          add = "reg.line",
          conf.int = T) +
  stat_cor(method = "pearson", label.x = 0, label.y = 1)
