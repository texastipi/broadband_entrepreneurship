library(tigris)
library(sf)
library(tidyverse)
library(ggplot2)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

#### Import the merged dataset V2 ####
allstate_import <- read.csv("https://raw.githubusercontent.com/texastipi/broadband_entrepreneurship/master/Broadband-Entrepreneurship-TXKSME.csv")

## Kansas dataset
ks_bb_entrepreneur_merged_v2 <- allstate_import %>% 
  filter(ST == "KS")

#### Set up spatial dataframe ####
## Kansas dataset
ks_county <- counties("Kansas")
ks_bb_entrepreneur_merged_v3 <- sp::merge(ks_county, ks_bb_entrepreneur_merged_v2,
                                          by.x = "GEOID", by.y = "county_FIPS")
## All data
county <- counties(c("Texas", "Kansas", "Maine"))
allstate_geo <- sp::merge(county, allstate_import,
                          by.x = "GEOID", by.y = "county_FIPS")
## Getting the Metro & Micro areas
tx <- tracts("TX", cb = T)
me <- tracts("ME", cb = T)
ks <- tracts("KS", cb = T)
cb <- core_based_statistical_areas(cb = T)
str(cb)
# Filter for metro, each state
kmsa <- cb %>% 
  filter(grepl("KS", NAME)) %>% 
  filter(LSAD == "M1")
txmsa <- cb %>% 
  filter(grepl("TX", NAME)) %>% 
  filter(LSAD == "M1")
memsa <- cb %>% 
  filter(grepl("ME", NAME)) %>% 
  filter(LSAD == "M1")

ggplot(kmsa) + geom_sf()

# Leave only within states
w1_ks <- st_within(ks, kmsa)
w1_tx <- st_within(tx, txmsa)
w1_me <- st_within(me, memsa)

w2_ks <- map_lgl(w1_ks, function(x) {
  if (length(x) == 1) {
    return(TRUE)
  } else {
    return(FALSE)
  }
})

w2_tx <- map_lgl(w1_tx, function(x) {
  if (length(x) == 1) {
    return(TRUE)
  } else {
    return(FALSE)
  }
})

w2_me <- map_lgl(w1_me, function(x) {
  if (length(x) == 1) {
    return(TRUE)
  } else {
    return(FALSE)
  }
})
# Subset 
kmsa2 <- ks[w2_ks,] %>% st_union()
txmsa2 <- tx[w2_tx,] %>% st_union()
memsa2 <- me[w2_me,] %>% st_union()

#### Maps ####
state_ls <- c("KS", "TX", "ME")
msa_ls <- list(kmsa2, txmsa2, memsa2)
fcc_plot_ls <- list()
ms_plot_ls <- list()
acs_plot_ls <- list()
## FCC 25/3 Mbps
# Loop to create a list of maps
for (i in 1:length(state_ls)) {
  fcc_plot_ls[[i]] <- allstate_geo %>% filter(ST == state_ls[i]) %>% 
    ggplot() + 
    geom_sf(aes(fill = pct25_3_dec_2019_fcc), lwd = 0.1) +
    scale_fill_gradientn(colours = RColorBrewer::brewer.pal(5, "YlOrRd"),
                         name = "FCC Broadband\n at 25/3 Mbps (%)",
                         label = scales::percent) +
    geom_sf(data = msa_ls[[i]], fill = alpha("honeydew3", 0), color = alpha("ivory2", 1), lwd = 0.7) +
    labs(caption = "Source: FCC Form477 Data as of December 2019") +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major = element_blank(),
          legend.title = element_text(face = "bold", hjust = 0.5),
          plot.caption = element_text(face = "italic"))
}
## MS Broadband
for (i in 1:length(state_ls)) {
  ms_plot_ls[[i]] <- allstate_geo %>% filter(ST == state_ls[i]) %>% 
    ggplot() + 
    geom_sf(aes(fill = pct_broadband_MS), lwd = 0.1) +
    scale_fill_gradientn(colours = RColorBrewer::brewer.pal(5, "YlOrRd"),
                         name = "Microsoft\n Broadband (%)",
                         label = scales::percent) +
    geom_sf(data = msa_ls[[i]], fill = alpha("honeydew3", 0), color = alpha("ivory4", 1), lwd = 0.7) +
    labs(caption = "Source: Microsoft as of November 2019") +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major = element_blank(),
          legend.title = element_text(face = "bold", hjust = 0.5),
          plot.caption = element_text(face = "italic"))
}
# Export maps
ggsave("ks-fcc-map.png",
       plot = fcc_plot_ls[[1]], width = 10, height = 6, units = "in", dpi = 600)
ggsave("tx-fcc-map.png",
       plot = fcc_plot_ls[[2]], width = 10, height = 6, units = "in", dpi = 600)
ggsave("me-fcc-map.png",
       plot = fcc_plot_ls[[3]], width = 10, height = 6, units = "in", dpi = 600)
ggsave("ks-ms-map.png",
       plot = ms_plot_ls[[1]], width = 10, height = 6, units = "in", dpi = 600)
ggsave("tx-ms-map.png",
       plot = ms_plot_ls[[2]], width = 10, height = 6, units = "in", dpi = 600)
ggsave("me-ms-map.png",
       plot = ms_plot_ls[[3]], width = 10, height = 6, units = "in", dpi = 600)

#### ACS Any broadband ####
library(censusapi)
# Add key to .Renviron
Sys.setenv(CENSUS_KEY="9001b546c2d77876a089119664dc25a4235eea37")
# Reload .Renviron
readRenviron("~/.Renviron")
# Check to see that the expected key is output in your R console
Sys.getenv("CENSUS_KEY")

acs_anybb <- getCensus(name = "acs/acs5/subject", vintage = 2019,
                          vars = "S2801_C02_014E",
                          region = "county:*",
                          regionin = "state:20,23,48")

acs_anybb <- acs_anybb %>% 
  mutate(geoid = paste0(state, county))
str(acs_anybb)

allstate_geo <- left_join(allstate_geo,
                          select(acs_anybb, geoid, S2801_C02_014E),
                          by = c("GEOID" = "geoid")) %>% 
  mutate(pct_anybroadband_acs_2019 = S2801_C02_014E/100)

glimpse(allstate_geo)

## Map ACS Any broadband
for (i in 1:length(state_ls)) {
  acs_plot_ls[[i]] <- allstate_geo %>% filter(ST == state_ls[i]) %>% 
    ggplot() + 
    geom_sf(aes(fill = pct_anybroadband_acs_2019), lwd = 0.1) +
    scale_fill_gradientn(colours = RColorBrewer::brewer.pal(5, "YlOrRd"),
                         name = "Households with\nAny Broadband\nSubscription (%)",
                         label = scales::percent) +
    geom_sf(data = msa_ls[[i]], fill = alpha("honeydew3", 0), color = alpha("ivory2", 1), lwd = 0.7) +
    labs(caption = "Source: American Community Survey 5-year Estimates (2019)") +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major = element_blank(),
          legend.title = element_text(face = "bold", hjust = 0.5),
          plot.caption = element_text(face = "italic"))
}
# Export maps
ggsave("ks-acs-map.png",
       plot = acs_plot_ls[[1]], width = 10, height = 6, units = "in", dpi = 600)
ggsave("tx-acs-map.png",
       plot = acs_plot_ls[[2]], width = 10, height = 6, units = "in", dpi = 600)
ggsave("me-acs-map.png",
       plot = acs_plot_ls[[3]], width = 10, height = 6, units = "in", dpi = 600)

#### Entrepreneurship map ####
nf_prop_maps <- list()
nf_prop_chg_maps <- list()
vd_maps <- list()
## % of nonfarm proprietorship
for (i in 1:length(state_ls)) {
  nf_prop_maps[[i]] <- allstate_geo %>% filter(ST == state_ls[i]) %>% 
    ggplot() + 
    geom_sf(aes(fill = pct_nonfarm_bea_2018), lwd = 0.1) +
    scale_fill_gradientn(colours = RColorBrewer::brewer.pal(5, "YlOrRd"),
                         name = "Nonfarm\nProprietorship (%)",
                         label = scales::percent) +
    geom_sf(data = msa_ls[[i]], fill = alpha("honeydew3", 0), color = alpha("ivory2", 1), lwd = 0.7) +
    labs(caption = "Source: Bureau of Economic Analysis (2018)") +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major = element_blank(),
          legend.title = element_text(face = "bold", hjust = 0.5),
          plot.caption = element_text(face = "italic"))
}

## Change in nonfarm proprietorship
for (i in 1:length(state_ls)) {
  nf_prop_chg_maps[[i]] <- allstate_geo %>% filter(ST == state_ls[i]) %>% 
    ggplot() + 
    geom_sf(aes(fill = pct_chg_bea_2012_2018), lwd = 0.1) +
    scale_fill_gradientn(colours = RColorBrewer::brewer.pal(5, "YlOrRd"),
                         name = "Change in Nonfarm\nProprietorship\n2012-2018 (%)",
                         label = scales::percent) +
    geom_sf(data = msa_ls[[i]], fill = alpha("honeydew3", 0), color = alpha("ivory2", 1), lwd = 0.7) +
    labs(caption = "Source: Bureau of Economic Analysis (2012, 2018)") +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major = element_blank(),
          legend.title = element_text(face = "bold", hjust = 0.5),
          plot.caption = element_text(face = "italic"))
}

## Average venture density 2019-2020
allstate_geo <- allstate_geo %>% 
  mutate(vd_mean = (vd_mean_19 + vd_mean_20)/2)

for (i in 1:length(state_ls)) {
  vd_maps[[i]] <- allstate_geo %>% filter(ST == state_ls[i]) %>% 
    ggplot() + 
    geom_sf(aes(fill = vd_mean), lwd = 0.1) +
    scale_fill_gradientn(colours = RColorBrewer::brewer.pal(5, "YlOrRd"),
                         name = "Average\nVenture\nDensity\n(per 100 people)") +
    geom_sf(data = msa_ls[[i]], fill = alpha("honeydew3", 0), color = alpha("ivory4", 1), lwd = 0.7) +
    labs(caption = "Source: GoDaddy (2019-2020)") +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major = element_blank(),
          legend.title = element_text(face = "bold"),
          plot.caption = element_text(face = "italic"))
}

# Export maps
# Nonfarm proprietorship
ggsave("ks-nonfarm-map.png",
       plot = nf_prop_maps[[1]], width = 10, height = 6, units = "in", dpi = 600)
ggsave("tx-nonfarm-map.png",
       plot = nf_prop_maps[[2]], width = 10, height = 6, units = "in", dpi = 600)
ggsave("me-nonfarm-map.png",
       plot = nf_prop_maps[[3]], width = 10, height = 6, units = "in", dpi = 600)

# Nonfarm proprietorship change
ggsave("ks-nonfarm-chg-map.png",
       plot = nf_prop_chg_maps[[1]], width = 10, height = 6, units = "in", dpi = 600)
ggsave("tx-nonfarm-chg-map.png",
       plot = nf_prop_chg_maps[[2]], width = 10, height = 6, units = "in", dpi = 600)
ggsave("me-nonfarm-chg-map.png",
       plot = nf_prop_chg_maps[[3]], width = 10, height = 6, units = "in", dpi = 600)

# Venture density
ggsave("ks-vd-map.png",
       plot = vd_maps[[1]], width = 10, height = 6, units = "in", dpi = 600)
ggsave("tx-vd-map.png",
       plot = vd_maps[[2]], width = 10, height = 6, units = "in", dpi = 600)
ggsave("me-vd-map.png",
       plot = vd_maps[[3]], width = 10, height = 6, units = "in", dpi = 600)




