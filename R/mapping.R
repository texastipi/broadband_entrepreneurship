library(tigris)
library(sf)
library(tidyverse)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

## Getting the Metro & Micro areas
ks <- tracts("KS", cb = T)
cb <- core_based_statistical_areas(cb = T)
str(cb)
# Filter for Kansas metro
kmsa <- cb %>% 
  filter(grepl("KS", NAME)) %>% 
  filter(LSAD == "M1")

ggplot(kmsa) + geom_sf()

# Leave only within KS
w1 <- st_within(ks, kmsa)

w2 <- map_lgl(w1, function(x) {
  if (length(x) == 1) {
    return(TRUE)
  } else {
    return(FALSE)
  }
})

# Subset 
kmsa2 <- ks[w2,]

# Plot
ggplot() + 
  geom_sf(data = ks_bb_entrepreneur_merged_v3, aes(fill = pct25_3_dec_2019_fcc), lwd = 0) +
  scale_fill_gradientn(colours = brewer.pal(5, "YlOrRd"),
                       name = "FCC Broadband\n at 25/3 Mbps (%)",
                       label = scales::percent) +
  geom_sf(data = kmsa2, fill = NA, color = "white") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank())

#### Kansas FCC & MS ####
## FCC 25/3 Mbps
ks_fcc_map <- ggplot() + 
  geom_sf(data = ks_bb_entrepreneur_merged_v3, aes(fill = pct25_3_dec_2019_fcc), lwd = 0.1) +
  scale_fill_gradientn(colours = brewer.pal(5, "YlOrRd"),
                       name = "FCC Broadband\n at 25/3 Mbps (%)",
                       label = scales::percent) +
  labs(caption = "Source: FCC Form477 Data as of December 2019") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank())

## MS Broadband
ks_ms_map <- ggplot() + 
  geom_sf(data = ks_bb_entrepreneur_merged_v3, aes(fill = pct_broadband_MS), lwd = 0.1) +
  scale_fill_gradientn(colours = brewer.pal(5, "YlOrRd"),
                       name = "Microsoft\n Broadband (%)",
                       label = scales::percent) +
  labs(caption = "Source: Microsoft as of November 2019") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank())

ggsave("ks-fcc-map.png",
       plot = ks_fcc_map, width = 10, height = 6, units = "in", dpi = 600)

ggsave("ks-ms-map.png",
       plot = ks_ms_map, width = 10, height = 6, units = "in", dpi = 600)

## FCC 25/3 Histogram
ks_fcc_hist <- ggplot(ks_bb_entrepreneur_merged_v2, aes(x = pct25_3_dec_2019_fcc)) + 
  geom_histogram() + theme_minimal() + 
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 12, face = "bold")) + 
  xlab("FCC Broadband at 25/3 Mbps (%)") + ylab("Frequency") +
  scale_x_continuous(labels = scales::percent, breaks = scales::breaks_pretty()) +
  labs(caption = "Source: FCC Form477 Data as of December 2019")

## MS Broadband
ks_ms_hist <- ggplot(ks_bb_entrepreneur_merged_v2, aes(x = pct_broadband_MS)) + 
  geom_histogram() + theme_minimal() + 
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 12, face = "bold")) + 
  xlab("Microsoft Broadband (%)") + ylab("Frequency") +
  scale_x_continuous(labels = scales::percent, breaks = scales::breaks_pretty(),
                     limits = c(0,1)) +
  labs(caption = "Source: Microsoft as of November 2019")

ggsave("ks-fcc-hist.png",
       plot = ks_fcc_hist, width = 10, height = 6, units = "in", dpi = 600)

ggsave("ks-ms-hist.png",
       plot = ks_ms_hist, width = 10, height = 6, units = "in", dpi = 600)

#### ACS Any broadband ####
acs_ks_anybb <- getCensus(name = "acs/acs5/subject", vintage = 2019,
                          vars = "S2801_C02_014E",
                          region = "county:*",
                          regionin = "state:20")
str(acs_ks_anybb)
glimpse(ks_bb_entrepreneur_merged_v3)

acs_ks_anybb <- acs_ks_anybb %>% 
  mutate(geoid = paste0(state, county))
str(acs_ks_anybb)

ks_bb_entrepreneur_merged_v3 <- left_join(ks_bb_entrepreneur_merged_v3,
                                          select(acs_ks_anybb, geoid, S2801_C02_014E),
                                          by = c("GEOID" = "geoid")) %>% 
  mutate(pct_anybroadband_acs_2019 = S2801_C02_014E/100)

glimpse(ks_bb_entrepreneur_merged_v3)

ks_bb_entrepreneur_merged_v3 <- ks_bb_entrepreneur_merged_v3 %>% 
  mutate(pct_anybroadband_acs_2019 = pct_anybroadband_acs_2018)

## Map Kansas ACS Any broadband

ks_acs_map <- ggplot() + 
  geom_sf(data = ks_bb_entrepreneur_merged_v3, aes(fill = pct_anybroadband_acs_2019), lwd = 0) +
  scale_fill_gradientn(colours = brewer.pal(5, "YlOrRd"),
                       name = "ACS Household with\nAny Broadband Subscription (%)",
                       label = scales::percent) +
  geom_sf(data = kmsa2, fill = NA, color = alpha("honeydew3", 0.8), lwd = 0.6) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank())

ggsave("ks-acs-map.png",
       plot = ks_acs_map, width = 10, height = 6, units = "in", dpi = 600)
