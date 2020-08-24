##########################################
#### FCC Data Computation Replication ####
##########################################

## Replicate the FCC data using raw 477 data
## Import libraries

library(RSocrata)
library(tidyverse)
library(ggplot2)

#### Import FCC 477 Data of Three States ####

## Write query for three states ME, KS, TX

fcc_2019_txksme <- read.socrata(url = "https://opendata.fcc.gov/resource/9r8r-g7ut.json?stateabbr=TX",
                                app_token = "VVKDIbbHVFF1HBztnQifUisCN")



