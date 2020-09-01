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
# Create temporary file space to download the dataset
temp <- tempfile()
temp2 <- tempfile()
# Download dataset for each state, unzip and import to data frame
# Texas as of June 2019
download.file("https://transition.fcc.gov/form477/BroadbandData/Fixed/Jun19/Version%201/TX-Fixed-Jun2019.zip", temp)
unzip(zipfile = temp, exdir = temp2)
fcc477_2019_tx <- read.csv(file.path(temp2, "TX-Fixed-Jun2019-v1.csv"), header = T)
# Kansas as of June 2019
download.file("https://transition.fcc.gov/form477/BroadbandData/Fixed/Jun19/Version%201/KS-Fixed-Jun2019.zip", temp)
unzip(zipfile = temp, exdir = temp2)
fcc477_2019_ks <- read.csv(file.path(temp2, "KS-Fixed-Jun2019-v1.csv"), header = T)
# Maine as of June 2019
download.file("https://transition.fcc.gov/form477/BroadbandData/Fixed/Jun19/Version%201/ME-Fixed-Jun2019.zip", temp)
unzip(zipfile = temp, exdir = temp2)
fcc477_2019_me <- read.csv(file.path(temp2, "ME-Fixed-Jun2019-v1.csv"), header = T)

# Texas as of Dec 2018
download.file("https://www.fcc.gov/form477/BroadbandData/Fixed/Dec18/Version%202/TX-Fixed-Dec2018.zip", temp)
unzip(zipfile = temp, exdir = temp2)
fcc477_2018_tx <- read.csv(file.path(temp2, "TX-Fixed-Dec2018-v2.csv"), header = T)
# Kansas as of Dec 2018
download.file("https://www.fcc.gov/form477/BroadbandData/Fixed/Dec18/Version%202/KS-Fixed-Dec2018.zip", temp)
unzip(zipfile = temp, exdir = temp2)
fcc477_2018_ks <- read.csv(file.path(temp2, "KS-Fixed-Dec2018-v2.csv"), header = T)
# Maine as of Dec 2018
download.file("https://www.fcc.gov/form477/BroadbandData/Fixed/Dec18/Version%202/ME-Fixed-Dec2018.zip", temp)
unzip(zipfile = temp, exdir = temp2)
fcc477_2018_me <- read.csv(file.path(temp2, "ME-Fixed-Dec2018-v2.csv"), header = T)
## Append the data of three states into one data frame ##
fcc477_Jun2019_txksme <- rbind(fcc477_2019_tx, fcc477_2019_ks, fcc477_2019_me)
fcc477_Dec2018_txksme <- rbind(fcc477_2018_tx, fcc477_2018_ks, fcc477_2018_me)

## Import FCC Staff's block estimates ##
## The dataset provides FCC's estimates of population, households, and housing units per census block ##
## The links are available at https://www.fcc.gov/reports-research/data/staff-block-estimates ##
download.file("https://www.fcc.gov/file/17838/download", temp)
unzip(zipfile = temp, exdir = temp2)
fcc_staff_est_us2018 <- read.csv(file.path(temp2, "us2018.csv"), header = T)

fcc_staff_est_county2018 <- read.csv("https://www.fcc.gov/file/17821/download", header = T)
unlink(temp)
unlink(temp2)
rm(temp)
rm(temp2)

#### Clean Dataset ####
fcc477_Jun2019_txksme <- fcc477_Jun2019_txksme %>% 
  mutate(BlockCode = as.character(BlockCode),
         TechCode = as.character(TechCode),
         Consumer = as.character(Consumer)) %>% 
  select(Provider_Id, ProviderName, StateAbbr, BlockCode, TechCode, Consumer, MaxAdDown, MaxAdUp) %>% 
  mutate(county_fips = str_sub(BlockCode, 1, 5)) %>% 
  filter(TechCode != c("60"))

str(fcc477_Jun2019_txksme)

fcc477_Dec2018_txksme <- fcc477_Dec2018_txksme %>% 
  mutate(BlockCode = as.character(BlockCode),
         TechCode = as.character(TechCode),
         Consumer = as.character(Consumer)) %>% 
  select(Provider_Id, ProviderName, StateAbbr, BlockCode, TechCode, Consumer, MaxAdDown, MaxAdUp) %>% 
  mutate(county_fips = str_sub(BlockCode, 1, 5)) %>% 
  filter(TechCode != c("60"))

## Staff Estimates for US Census Block Level ##
fcc_staff_est_us2018 <- fcc_staff_est_us2018 %>% 
  mutate(block_fips = as.character(block_fips),
         county_fips = str_sub(block_fips, 1, 5)) %>% 
  select(stateabbr, county_fips, block_fips, ends_with("2018"))

## County Level Staff Estimates ##
fcc_staff_est_county2018 <- fcc_staff_est_county2018 %>% 
  select(Id, Id2, Geography, ends_with("2018")) %>% 
  rename(hu2018 = `Housing.Unit.Estimate..as.of.July.1....2018`,
         pop2018 = `Population.Estimate..as.of.July.1....2018`)

## Get Census Population numbers ##
library(censusapi)
# Add key to .Renviron
Sys.setenv(CENSUS_KEY="9001b546c2d77876a089119664dc25a4235eea37")
# Reload .Renviron
readRenviron("~/.Renviron")
# Check to see that the expected key is output in your R console
Sys.getenv("CENSUS_KEY")

# 2019 County populations
census_pop2019 <- getCensus(name = "pep/population",
                            vintage = 2019,
                            vars = c("GEO_ID","POP"),
                            region = "county:*",
                            regionin = "state:48,20,23")
census_pop2019 <- census_pop2019 %>% 
  mutate(county_fips = paste(state, county, sep = ""))

#### Calculating the Broadband Availability ####
## General process:
## 1. Code blocks based on presence of broadband service at 25/3 Mbps level
## 2. If the block has 25/3 Mbps service the block is considered to have broadband availability
## 3. Aggregate at the county level
## 4. Merge with staff estimates and calculate percentages

### Data as of June 2019 ###
# Step 1 Code blocks based on persence of broadband service (25/3)
fcc477_Jun2019_txksme <- fcc477_Jun2019_txksme %>% 
  mutate(broadband_f = as.factor(ifelse(MaxAdDown >= 25 & MaxAdUp >= 3, 1, 0)))
# Join the staff estimates and filter only consumer services
fcc477_Jun2019_txksme <- fcc477_Jun2019_txksme %>% 
  left_join(., fcc_staff_est_us2018, by = c("BlockCode" = "block_fips")) %>% 
  filter(Consumer == 1)

# Aggregate at the census block level
# Summing the broadband factor variable would let us know if there is a census block with broadband
# availability or not (if there are, the sum would be greater than 0, if there is non, sum would be 0)
fcc477_txksme_block <- fcc477_Jun2019_txksme %>% 
  filter(broadband_f == "1") %>% 
  distinct(BlockCode, .keep_all = T)

# Aggregate at the county level and merge with FCC staff estimates
fcc477_txksme_county <- fcc477_txksme_block %>% 
  group_by(county_fips.x) %>% summarise(pop_bb_county2018 = sum(pop2018))
fcc477_txksme_county <- fcc477_txksme_county %>% 
  left_join(., fcc_staff_est_county2018, by = c("county_fips.x" = "Id2"))

#### Calculating FCC Deployment Percentages ####
## 477 Data as of June 2019 & Staff Estimates as of 2018 ##
# Calculate broadband percentage by dividing census block population with broadband by total county
# population
fcc477_txksme_county <- fcc477_txksme_county %>% 
  mutate(bb_pct = pop_bb_county2018 / pop2018)
fcc477_txksme_county <- fcc477_txksme_county %>% 
  mutate(state_fips = str_sub(county_fips.x, 1,2))

# Filter for only Texas
fcc477_tx_county <- fcc477_txksme_county %>% filter(state_fips == "48")

ggplot(fcc477_tx_county, aes(x = bb_pct)) + geom_histogram()


### Data as of Dec 2018 ###
# Step 1 Code blocks based on persence of broadband service (25/3)
fcc477_Dec2018_txksme <- fcc477_Dec2018_txksme %>% 
  mutate(broadband_f = as.factor(ifelse(MaxAdDown >= 25 & MaxAdUp >= 3, 1, 0)))
# Join the staff estimates and filter only consumer services
fcc477_Dec2018_txksme <- fcc477_Dec2018_txksme %>% 
  left_join(., fcc_staff_est_us2018, by = c("BlockCode" = "block_fips")) %>% 
  filter(Consumer == 1)

# Aggregate at the census block level
# Summing the broadband factor variable would let us know if there is a census block with broadband
# availability or not (if there are, the sum would be greater than 0, if there is non, sum would be 0)
fcc477_txksme_block_2018 <- fcc477_Dec2018_txksme %>% 
  filter(broadband_f == "1") %>% 
  distinct(BlockCode, .keep_all = T)

# Aggregate at the county level and merge with FCC staff estimates
fcc477_txksme_county_2018 <- fcc477_txksme_block_2018 %>% 
  group_by(county_fips.x) %>% summarise(pop_bb_county2018 = sum(pop2018))
fcc477_txksme_county_2018 <- fcc477_txksme_county_2018 %>% 
  left_join(., fcc_staff_est_county2018, by = c("county_fips.x" = "Id2"))

#### Calculating FCC Deployment Percentages ####
## 477 Data as of June 2019 & Staff Estimates as of 2018 ##
# Calculate broadband percentage by dividing census block population with broadband by total county
# population
fcc477_txksme_county_2018 <- fcc477_txksme_county_2018 %>% 
  mutate(bb_pct = pop_bb_county2018 / pop2018)
fcc477_txksme_county_2018 <- fcc477_txksme_county_2018 %>% 
  mutate(state_fips = str_sub(county_fips.x, 1,2))

# Filter for only Texas
fcc477_tx_county_2018 <- fcc477_txksme_county_2018 %>% filter(state_fips == "48")

ggplot(fcc477_tx_county_2018, aes(x = bb_pct)) + geom_histogram()


#### Validating the Calculation ####
### Here the above calculation will be validated using the data as of Dec 2017
### The ending numbers will be cross matched against the numbers in the corresponding FCC deployment report
temp <- tempfile()
temp2 <- tempfile()
# Texas as of Dec 2017
download.file("https://www.fcc.gov/form477/BroadbandData/Fixed/Dec17/Version%203/TX-Fixed-Dec2017.zip", temp)
unzip(zipfile = temp, exdir = temp2)
fcc477_2017_tx <- read.csv(file.path(temp2, "TX-Fixed-Dec2017-v3.csv"), header = T)
# Kansas as of Dec 2017
download.file("https://www.fcc.gov/form477/BroadbandData/Fixed/Dec17/Version%203/KS-Fixed-Dec2017.zip", temp)
unzip(zipfile = temp, exdir = temp2)
fcc477_2017_ks <- read.csv(file.path(temp2, "KS-Fixed-Dec2017-v3.csv"), header = T)
# Maine as of Dec 2017
download.file("https://www.fcc.gov/form477/BroadbandData/Fixed/Dec17/Version%203/ME-Fixed-Dec2017.zip", temp)
unzip(zipfile = temp, exdir = temp2)
fcc477_2017_me <- read.csv(file.path(temp2, "ME-Fixed-Dec2017-v3.csv"), header = T)

unlink(temp)
unlink(temp2)
rm(temp)
rm(temp2)

fcc477_Dec2017_txksme <- rbind(fcc477_2017_tx, fcc477_2017_ks, fcc477_2017_me)

fcc477_Dec2017_txksme <- fcc477_Dec2017_txksme %>% 
  mutate(BlockCode = as.character(BlockCode),
         TechCode = as.character(TechCode),
         Consumer = as.character(Consumer)) %>% 
  select(Provider_Id, ProviderName, StateAbbr, BlockCode, TechCode, Consumer, MaxAdDown, MaxAdUp) %>% 
  mutate(county_fips = str_sub(BlockCode, 1, 5)) %>% 
  filter(TechCode != c("60"))

### Data as of Dec 2017 ###
# Step 1 Code blocks based on persence of broadband service (25/3)
fcc477_Dec2017_txksme <- fcc477_Dec2017_txksme %>% 
  mutate(broadband_f = as.factor(ifelse(MaxAdDown >= 25 & MaxAdUp >= 3, 1, 0)))
# Join the staff estimates and filter only consumer services
fcc477_Dec2017_txksme <- fcc477_Dec2017_txksme %>%
  left_join(., fcc_staff_est_us2018, by = c("BlockCode" = "block_fips")) %>% 
  filter(Consumer == 1)

# Aggregate at the census block level
# Summing the broadband factor variable would let us know if there is a census block with broadband
# availability or not (if there are, the sum would be greater than 0, if there is non, sum would be 0)
fcc477_txksme_block_2017 <- fcc477_Dec2017_txksme %>% 
  filter(broadband_f == "1") %>% 
  distinct(BlockCode, .keep_all = T)

# Aggregate at the county level and merge with FCC staff estimates
fcc477_txksme_county_2017 <- fcc477_txksme_block_2017 %>% 
  group_by(county_fips.x) %>% summarise(pop_bb_county2018 = sum(pop2018))
fcc477_txksme_county_2017 <- fcc477_txksme_county_2017 %>% 
  left_join(., fcc_staff_est_county2018, by = c("county_fips.x" = "Id2"))

#### Calculating FCC Deployment Percentages ####
## 477 Data as of June 2019 & Staff Estimates as of 2018 ##
# Calculate broadband percentage by dividing census block population with broadband by total county
# population
fcc477_txksme_county_2017 <- fcc477_txksme_county_2017 %>% 
  mutate(bb_pct = pop_bb_county2018 / pop2018)
fcc477_txksme_county_2017 <- fcc477_txksme_county_2017 %>% 
  mutate(state_fips = str_sub(county_fips.x, 1,2))

# Filter for only Texas
fcc477_tx_county_2017 <- fcc477_txksme_county_2017 %>% filter(state_fips == "48")

ggplot(fcc477_tx_county_2017, aes(x = bb_pct)) + geom_histogram()

