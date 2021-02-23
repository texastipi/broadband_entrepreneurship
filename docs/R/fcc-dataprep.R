#### FCC Data Retrieval Test ####
#install.packages("devtools")
#devtools::install_github("Chicago/RSocrata")
library(tidyverse)
library(RSocrata)

rm(list=ls())

#### Code for retrieving data from FCC API endpoints ####
# The FCC API used here is Area Tables that provides population of various regional categories
# with broadband availability at different speed blocks
# The data provides information on at least 
# how many broadband providers are in the regional categories providing service at certain broadband speed
## SQL Query
# The query is made to retrieve only county regions of Texas, Maine, and Kansas
# The query gets the population information for 25/3, 100/10, 250/25, 1000/100 Mbps

fcc_AT_dec_2019 <- read.socrata(
  "https://opendata.fcc.gov/resource/ws2a-amik.json?$where=type = 'county' AND (starts_with(id,'20') OR starts_with(id,'48') OR starts_with(id,'23')) AND (speed = '25' OR speed = '100' OR speed = '250' OR speed = '1000')&$order=id",
  app_token = "WHT9nk7BMfD3HeJF6rxayRokn",
  email = "jwroycechoi@gmail.com",
  password = "Wodnjs58640!"
)

#### Check the validity of using the area table API ####
# This section will check whether using area table results in identical numbers calculated from raw
# Form 477 data that is in the dataset created

# Download staff estimate data
temp <- tempfile()
temp2 <- tempfile()

download.file("https://www.fcc.gov/file/17838/download", temp)  # 2018 Estimates
download.file("https://www.fcc.gov/file/19314/download", temp)  # 2019 Estimates
unzip(zipfile = temp, exdir = temp2)
fcc_staff_est_us2018 <- read.csv(file.path(temp2, "us2018.csv"), header = T)
fcc_staff_est_us2019 <- read.csv(file.path(temp2, "us2019.csv"), header = T)

fcc_staff_est_county2018 <- read.csv("https://www.fcc.gov/file/17821/download", header = T)
unlink(temp)
unlink(temp2)
rm(temp)
rm(temp2)

## Staff Estimates for US Census Block Level ##
# Year 2018
fcc_staff_est_us2018 <- fcc_staff_est_us2018 %>% 
  mutate(block_fips = as.character(block_fips),
         county_fips = str_sub(block_fips, 1, 5)) %>% 
  select(stateabbr, county_fips, block_fips, ends_with("2018"))
# Year 2019
fcc_staff_est_us2019 <- fcc_staff_est_us2019 %>% 
  mutate(block_fips = as.character(block_fips),
         county_fips = str_sub(block_fips, 1, 5)) %>% 
  select(stateabbr, county_fips, block_fips, ends_with("2019"))


## County Level Staff Estimates ##
# Year 2018
fcc_staff_est_county2018 <- fcc_staff_est_county2018 %>% 
  select(Id, Id2, Geography, ends_with("2018")) %>% 
  rename(hu2018 = `Housing.Unit.Estimate..as.of.July.1....2018`,
         pop2018 = `Population.Estimate..as.of.July.1....2018`)

# Year 2019: Calcualted from the fcc_staff_est_us2019
fcc_staff_est_county2019 <- fcc_staff_est_us2019 %>% 
  group_by(county_fips) %>% summarise(hu2019 = sum(hu2019, na.rm = T),
                                      pop2019 = sum(pop2019, na.rm = T),
                                      hh2019 = sum(hh2019, na.rm = T))

## Aggregating the area table dataset ##
glimpse(fcc_AT_dec_2019)

# County population with at least 1 provider 

fcc_AT_dec_2019_grouped <- fcc_AT_dec_2019 %>% 
  filter(tech == "acfow") %>% mutate_at(vars(starts_with("has_")), as.numeric) %>% 
  mutate(pop25_3 = ifelse(speed == "25" & (has_1 > 0 | has_2 > 0 | has_3more > 0), rowSums(select(., starts_with("has_"), -has_0)), 0),
         pop100_10 = ifelse(speed == "100" & (has_1 > 0 | has_2 > 0 | has_3more > 0), rowSums(select(., starts_with("has_"), -has_0)), 0),
         pop250_25 = ifelse(speed == "250" & (has_1 > 0 | has_2 > 0 | has_3more > 0), rowSums(select(., starts_with("has_"), -has_0)), 0),
         pop1000_100 = ifelse(speed == "1000" & (has_1 > 0 | has_2 > 0 | has_3more > 0), rowSums(select(., starts_with("has_"), -has_0)), 0)) %>% 
  group_by(id) %>% summarise(pop25_3 = sum(pop25_3),
                             pop100_10 = sum(pop100_10),
                             pop250_25 = sum(pop250_25),
                             pop1000_100 = sum(pop1000_100)) %>% 
  left_join(., fcc_staff_est_county2018, by = c("id" = "Id2")) %>% 
  left_join(., select(fcc_staff_est_county2019, county_fips, pop2019), by = c("id" = "county_fips")) %>% 
  mutate(pct25_3 = pop25_3/pop2018,
         pct100_10 = pop100_10/pop2018,
         pct250_25 = pop250_25/pop2018,
         pct1000_100 = pop1000_100/pop2018,
         pct25_3_2019 = pop25_3/pop2019,
         pct100_10_2019 = pop100_10/pop2019,
         pct250_25_2019 = pop250_25/pop2019,
         pct1000_100_2019 = pop1000_100/pop2019)

#### Function that goes through a list of dataframes and do the cleaning process above ####
dflst <- list(dec_2019 = fcc_AT_dec_2019,
              dec_2016 = fcc_AT_dec_2016)

## Test

glimpse(df)
summary(df)
str(dfs)
table(df$mth_yr)
glimpse(fcc_staff_est_us2019)
gsub("^\\D+","",names(dflst[1]))
gsub("\\_.*","",names(dflst[1]))

dfs <- vector("list", length(dflst))
for (i in seq_along(dflst)) {
  if (gsub("\\_.*","",names(dflst[i])) == "jun") {
    staff_est <- fcc_staff_est_us2019 %>% select(., county_fips, ends_with(as.character(as.numeric(gsub("[^[:digit:]]","", names(dflst)[i]))-1)))
  } else {staff_est <- fcc_staff_est_us2019 %>% select(., county_fips, ends_with(gsub("[^[:digit:]]", "", names(dflst)[i])))}
  
  dfs[[i]] <- dflst[[i]] %>% 
    filter(tech == "acfow") %>% mutate_at(vars(starts_with("has_")), as.numeric) %>% 
    mutate(pop25_3 = ifelse(speed == "25" & (has_1 > 0 | has_2 > 0 | has_3more > 0), rowSums(select(., starts_with("has_"), -has_0)), 0),
           pop100_10 = ifelse(speed == "100" & (has_1 > 0 | has_2 > 0 | has_3more > 0), rowSums(select(., starts_with("has_"), -has_0)), 0),
           pop250_25 = ifelse(speed == "250" & (has_1 > 0 | has_2 > 0 | has_3more > 0), rowSums(select(., starts_with("has_"), -has_0)), 0),
           pop1000_100 = ifelse(speed == "1000" & (has_1 > 0 | has_2 > 0 | has_3more > 0), rowSums(select(., starts_with("has_"), -has_0)), 0)) %>% 
    group_by(id) %>% summarise(pop25_3 = sum(pop25_3),
                               pop100_10 = sum(pop100_10),
                               pop250_25 = sum(pop250_25),
                               pop1000_100 = sum(pop1000_100),
                               mth_yr = names(dflst)[i]) %>% 
    left_join(., staff_est, by = c("id" = "county_fips")) %>%
    mutate(pct25_3 = (pop25_3/select(., starts_with("pop20")))[,1],
           pct100_10 = (pop100_10/select(., starts_with("pop20")))[,1],
           pct250_25 = (pop250_25/select(., starts_with("pop20")))[,1],
           pct1000_100 = (pop1000_100/select(., starts_with("pop20")))[,1])
}
df <- bind_rows(dfs)
df <- df %>% 
  pivot_wider(names_from = mth_yr,
              names_glue = "{.value}_{mth_yr}_fcc",
              values_from = c(pct25_3, pct100_10, pct250_25, pct1000_100)) %>% 
  gather(var, val, pct25_3_dec_2016_fcc:pct1000_100_dec_2019_fcc, na.rm = T) %>% 
  group_by(id, var) %>% 
  distinct(val) %>% 
  spread(var, val) %>% mutate_at(vars(-id), ~ifelse(.x > 1, NA, .x))

## As function
getFCCpct <- function(dflst) {
  dfs <- vector("list", length(dflst))
  for (i in seq_along(dflst)) {
    if (gsub("\\_.*","",names(dflst[i])) == "jun") {
      staff_est <- fcc_staff_est_us2019 %>% select(., county_fips, ends_with(as.character(as.numeric(gsub("[^[:digit:]]","", names(dflst)[i]))-1)))
    } else {staff_est <- fcc_staff_est_us2019 %>% select(., county_fips, ends_with(gsub("[^[:digit:]]", "", names(dflst)[i])))}
    
    dfs[[i]] <- dflst[[i]] %>% 
      filter(tech == "acfow") %>% mutate_at(vars(starts_with("has_")), as.numeric) %>% 
      mutate(pop25_3 = ifelse(speed == "25" & (has_1 > 0 | has_2 > 0 | has_3more > 0), rowSums(select(., starts_with("has_"), -has_0)), 0),
             pop100_10 = ifelse(speed == "100" & (has_1 > 0 | has_2 > 0 | has_3more > 0), rowSums(select(., starts_with("has_"), -has_0)), 0),
             pop250_25 = ifelse(speed == "250" & (has_1 > 0 | has_2 > 0 | has_3more > 0), rowSums(select(., starts_with("has_"), -has_0)), 0),
             pop1000_100 = ifelse(speed == "1000" & (has_1 > 0 | has_2 > 0 | has_3more > 0), rowSums(select(., starts_with("has_"), -has_0)), 0)) %>% 
      group_by(id) %>% summarise(pop25_3 = sum(pop25_3),
                                 pop100_10 = sum(pop100_10),
                                 pop250_25 = sum(pop250_25),
                                 pop1000_100 = sum(pop1000_100),
                                 mth_yr = names(dflst)[i]) %>% 
      left_join(., staff_est, by = c("id" = "county_fips")) %>%
      mutate(pct25_3 = (pop25_3/select(., starts_with("pop20")))[,1],
             pct100_10 = (pop100_10/select(., starts_with("pop20")))[,1],
             pct250_25 = (pop250_25/select(., starts_with("pop20")))[,1],
             pct1000_100 = (pop1000_100/select(., starts_with("pop20")))[,1])
  }
  df <- bind_rows(dfs)
  df <- df %>% 
    pivot_wider(names_from = mth_yr,
                names_glue = "{.value}_{mth_yr}_fcc",
                values_from = c(pct25_3, pct100_10, pct250_25, pct1000_100)) %>% 
    gather(var, val, pct25_3_dec_2016_fcc:pct1000_100_dec_2019_fcc, na.rm = T) %>% 
    group_by(id, var) %>% 
    distinct(val) %>% 
    spread(var, val) %>% mutate_at(vars(-id), ~ifelse(.x > 1, NA, .x))
  
  return(df)
}

fcc_test <- getFCCpct(dflst = dflst)

#### Validation: Compare with the previous calculation ####
d <- read_csv("https://raw.githubusercontent.com/texastipi/broadband_entrepreneurship/master/Broadband-Entrepreneurship-TXKSME.csv")

gridExtra::grid.arrange(
  ggplot(fcc_AT_dec_2019_grouped) + geom_histogram(aes(x = pct25_3_2019)),
  ggplot(d) + geom_histogram(aes(x = pct_bb_fcc_2019)),
  ncol = 2, nrow = 2
)   # The distribution of broadband measure seems almost identical

test <- d %>% select(county_FIPS, pct_bb_fcc_2019, pct_bb_fcc_100_10_2019, pct_bb_fcc_250_25_2019) %>% 
  mutate(county_FIPS = as.character(county_FIPS)) %>% 
  left_join(., select(fcc_AT_dec_2019_grouped, id, pct25_3_2019, pct100_10_2019, pct250_25_2019),
            by = c("county_FIPS" = "id"))

gridExtra::grid.arrange(
  ggplot(test, aes(x = pct_bb_fcc_2019, y = pct25_3_2019)) + geom_point() + geom_smooth(method = "lm") +
    ggpubr::stat_cor(p.accuracy = 0.001, r.accuracy = 0.01),
  ggplot(test, aes(x = pct_bb_fcc_100_10_2019, y = pct100_10_2019)) + geom_point() + geom_smooth(method = "lm") +
    ggpubr::stat_cor(p.accuracy = 0.001, r.accuracy = 0.01),
  ggplot(test, aes(x = pct_bb_fcc_250_25_2019, y = pct250_25_2019)) + geom_point() + geom_smooth(method = "lm") +
    ggpubr::stat_cor(p.accuracy = 0.001, r.accuracy = 0.01),
  ncol = 3
)   # Correlation coefficients are almost 1. Therefore, using the area table APIs seems viable


#### Older method using raw 477 data ####

### Data as of June 2019 ###
# Step 1 Code blocks based on persence of broadband service (25/3)
fcc477_Jun2019_txksme <- fcc477_Jun2019_txksme %>% 
  mutate(broadband_f = as.factor(ifelse(MaxAdDown >= 25 & MaxAdUp >= 3, 1, 0)),
         bb_50_5_f = as.factor(ifelse(MaxAdDown >= 50 & MaxAdUp >= 5, 1, 0)),
         bb_100_10_f = as.factor(ifelse(MaxAdDown >= 100 & MaxAdUp >= 10, 1, 0)),
         bb_250_25_f = as.factor(ifelse(MaxAdDown >= 250 & MaxAdUp >= 25, 1, 0)))
# Join the staff estimates and filter only consumer services
fcc477_Jun2019_txksme <- fcc477_Jun2019_txksme %>% 
  left_join(., fcc_staff_est_us2018, by = c("BlockCode" = "block_fips")) %>% 
  filter(Consumer == 1)

# Aggregate at the census block level
# Summing the broadband factor variable would let us know if there is a census block with broadband
# availability or not (if there are, the sum would be greater than 0, if there is non, sum would be 0)
fcc477_txksme_block_1 <- fcc477_Jun2019_txksme %>% 
  filter(broadband_f == "1") %>% 
  distinct(BlockCode, .keep_all = T)
fcc477_txksme_block_2 <- fcc477_Jun2019_txksme %>% 
  filter(bb_50_5_f == "1") %>% 
  distinct(BlockCode, .keep_all = T)
fcc477_txksme_block_3 <- fcc477_Jun2019_txksme %>% 
  filter(bb_100_10_f == "1") %>% 
  distinct(BlockCode, .keep_all = T)
fcc477_txksme_block_4 <- fcc477_Jun2019_txksme %>% 
  filter(bb_250_25_f == "1") %>% 
  distinct(BlockCode, .keep_all = T)

# Aggregate at the county level and merge with FCC staff estimates
fcc477_txksme_county_1 <- fcc477_txksme_block_1 %>% 
  group_by(county_fips.x) %>% summarise(pop_bb_county2018 = sum(pop2018)) %>% 
  left_join(., fcc_staff_est_county2018, by = c("county_fips.x" = "Id2"))
fcc477_txksme_county_2 <- fcc477_txksme_block_2 %>% 
  group_by(county_fips.x) %>% summarise(pop_bb_county2018 = sum(pop2018)) %>% 
  left_join(., fcc_staff_est_county2018, by = c("county_fips.x" = "Id2"))
fcc477_txksme_county_3 <- fcc477_txksme_block_3 %>% 
  group_by(county_fips.x) %>% summarise(pop_bb_county2018 = sum(pop2018)) %>% 
  left_join(., fcc_staff_est_county2018, by = c("county_fips.x" = "Id2"))
fcc477_txksme_county_4 <- fcc477_txksme_block_4 %>% 
  group_by(county_fips.x) %>% summarise(pop_bb_county2018 = sum(pop2018)) %>% 
  left_join(., fcc_staff_est_county2018, by = c("county_fips.x" = "Id2"))

#### Calculating FCC Deployment Percentages ####
## 477 Data as of June 2019 & Staff Estimates as of 2018 ##
# Calculate broadband percentage by dividing census block population with broadband by total county
# population
fcc477_txksme_county_1 <- fcc477_txksme_county_1 %>% 
  mutate(pct_bb_fcc_2019 = pop_bb_county2018 / pop2018)
fcc477_txksme_county_2 <- fcc477_txksme_county_2 %>% 
  mutate(pct_bb_fcc_50_5_2019 = pop_bb_county2018 / pop2018)
fcc477_txksme_county_3 <- fcc477_txksme_county_3 %>% 
  mutate(pct_bb_fcc_100_10_2019 = pop_bb_county2018 / pop2018)
fcc477_txksme_county_4 <- fcc477_txksme_county_4 %>% 
  mutate(pct_bb_fcc_250_25_2019 = pop_bb_county2018 / pop2018)

fcc477_txksme_county <- fcc477_txksme_county_1 %>% 
  left_join(., select(fcc477_txksme_county_2, county_fips.x, pct_bb_fcc_50_5_2019),
            by = c("county_fips.x")) %>% 
  left_join(., select(fcc477_txksme_county_3, county_fips.x, pct_bb_fcc_100_10_2019),
            by = c("county_fips.x")) %>% 
  left_join(., select(fcc477_txksme_county_4, county_fips.x, pct_bb_fcc_250_25_2019),
            by = c("county_fips.x")) %>% 
  select(county_fips.x, pct_bb_fcc_2019, pct_bb_fcc_50_5_2019, pct_bb_fcc_100_10_2019, pct_bb_fcc_250_25_2019)

fcc477_txksme_county[is.na(fcc477_txksme_county)] <- 0




