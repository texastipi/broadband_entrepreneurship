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
glimpse(d)

#### Overview ####
## In this script, we will explore a more complicated regression models incorporating some of the modifications below
## 1. Natural log transformation for some applicable variable
## 2. Adding lagged DV as an IV to control for potential auto-correlation
## 3. Incorporating the speed element of FCC broadband deployment data


