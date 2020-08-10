#### FCC Data Imputation Test ####
install.packages("devtools")
devtools::install_github("Chicago/RSocrata")
library(tidyverse)
library(RSocrata)

fcc_area_2020 <- read.socrata(
  "https://opendata.fcc.gov/resource/tun5-dwjh.json",
  app_token = "VVKDIbbHVFF1HBztnQifUisCN"
)
