#### Frequency Histograms and Plots ####
library(tidyverse)
library(ggplot2)
rm(list = ls())
d <- read_csv("https://raw.githubusercontent.com/texastipi/broadband_entrepreneurship/master/Broadband-Entrepreneurship-TXKSME.csv")
names(d)

#### Distribution histograms of variables ####
# Broadband 25/3
hist_fcc25_3 <- ggplot(d) +
  geom_histogram(aes(x = pct25_3_dec_2019_fcc)) +
  theme_minimal() +
  xlab("FCC 25/3 Mbps Availability") +
  ylab("Count") +
  scale_x_continuous(labels = scales::percent) +
  theme(axis.title = element_text(size = 11, face = "bold"),
        axis.text = element_text(size = 11))

# Broadband 100/10
hist_fcc100_10 <- ggplot(d) +
  geom_histogram(aes(x = pct100_10_dec_2019_fcc)) +
  theme_minimal() +
  xlab("FCC 100/10 Mbps Availability") +
  ylab("Count") +
  scale_x_continuous(labels = scales::percent) +
  theme(axis.title = element_text(size = 11, face = "bold"),
        axis.text = element_text(size = 11))

# Broadband 250/25
hist_fcc250_25 <- ggplot(d) +
  geom_histogram(aes(x = pct250_25_dec_2019_fcc)) +
  theme_minimal() +
  xlab("FCC 250/25 Mbps Availability") +
  ylab("Count") +
  scale_x_continuous(labels = scales::percent) +
  theme(axis.title = element_text(size = 11, face = "bold"),
        axis.text = element_text(size = 11))

# Broadband 1000/100
hist_fcc1000_100 <- ggplot(d) +
  geom_histogram(aes(x = pct1000_100_dec_2019_fcc)) +
  theme_minimal() +
  xlab("FCC 1000/100 Mbps Availability") +
  ylab("Count") +
  scale_x_continuous(labels = scales::percent) +
  theme(axis.title = element_text(size = 11, face = "bold"),
        axis.text = element_text(size = 11))

# Broadband adoption
hist_bbadopt <- ggplot(d) +
  geom_histogram(aes(x = pct_fixed_acs_2018)) +
  theme_minimal() +
  xlab("Fixed broadband adoption (ACS, 2018)") +
  ylab("Count") +
  scale_x_continuous(labels = scales::percent) +
  theme(axis.title = element_text(size = 11, face = "bold"),
        axis.text = element_text(size = 11))

# Braodband Quality of Service
hist_bbqos <- ggplot(d) +
  geom_histogram(aes(x = pct_bb_qos)) +
  theme_minimal() +
  xlab("Broadband quality of service") +
  ylab("Count") +
  scale_x_continuous(labels = scales::percent) +
  theme(axis.title = element_text(size = 11, face = "bold"),
        axis.text = element_text(size = 11))

## Entrepreneurship Measures ##
# Nonfarm proprietors share
hist_nfprop <- ggplot(d) +
  geom_histogram(aes(x = pct_nonfarm_bea_2018)) +
  theme_minimal() +
  xlab("Nonfarm proprietors share") +
  ylab("Count") +
  scale_x_continuous(labels = scales::percent) +
  theme(axis.title = element_text(size = 11, face = "bold"),
        axis.text = element_text(size = 11))

# Average venture density
hist_vd <- ggplot(d) +
  geom_histogram(aes(x = vd_mean_20)) +
  theme_minimal() +
  xlab("Average venture density") +
  ylab("Count") +
  theme(axis.title = element_text(size = 11, face = "bold"),
        axis.text = element_text(size = 11))

## Rurality Measures ##
# IRR (2010)
hist_irr <- ggplot(d) +
  geom_histogram(aes(x = IRR2010)) +
  theme_minimal() +
  xlab("IRR (2010)") +
  ylab("Count") +
  theme(axis.title = element_text(size = 11, face = "bold"),
        axis.text = element_text(size = 11))

# RUCC (2013)
# Make RUCC categorical
d <- d %>% mutate(RUCC_cat = factor(RUCC_2013))
hist_rucc <- ggplot(d) +
  geom_histogram(aes(x = RUCC_cat), stat = "count") +
  theme_minimal() +
  xlab("RUCC (2013)") +
  ylab("Count") +
  theme(axis.title = element_text(size = 11, face = "bold"),
        axis.text = element_text(size = 11))


histogram_grid <- gridExtra::grid.arrange(hist_fcc25_3, hist_fcc100_10, hist_fcc250_25, hist_fcc1000_100,
                        hist_nfprop, hist_vd, hist_irr, hist_rucc,
                        ncol = 2 , nrow = 4, top = "Distribution Histograms")

#### Scatter plots of broadband & entrepreneurship measures ####
## Nonfarm proprietors share ##
# Broadband 25/3
scat_fcc25_3_nfprop <- ggplot(d, aes(x = pct25_3_dec_2019_fcc, y = pct_nonfarm_bea_2018)) +
  geom_point(color = "grey50", size = 1.5) + geom_smooth(se = F, color = "red") +
  xlab("FCC 25/3 Mbps Availability") +
  ylab("Nonfarm proprietors share") +
  theme_minimal() + theme(axis.title = element_text(size = 11, face = "bold"),
                          axis.text = element_text(size = 11)) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent)
# Broadband 100/10
scat_fcc100_10_nfprop <- ggplot(d, aes(x = pct100_10_dec_2019_fcc, y = pct_nonfarm_bea_2018)) +
  geom_point(color = "grey50", size = 1.5) + geom_smooth(se = F, color = "red") +
  xlab("FCC 100/10 Mbps Availability") +
  ylab("Nonfarm proprietors share") +
  theme_minimal() + theme(axis.title = element_text(size = 11, face = "bold"),
                          axis.text = element_text(size = 11)) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent)
# Broadband 250/25
scat_fcc250_25_nfprop <- ggplot(d, aes(x = pct250_25_dec_2019_fcc, y = pct_nonfarm_bea_2018)) +
  geom_point(color = "grey50", size = 1.5) + geom_smooth(se = F, color = "red") +
  xlab("FCC 250/25 Mbps Availability") +
  ylab("Nonfarm proprietors share") +
  theme_minimal() + theme(axis.title = element_text(size = 11, face = "bold"),
                          axis.text = element_text(size = 11)) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent)
# Broadband 1000/100
scat_fcc1000_100_nfprop <- ggplot(d, aes(x = pct1000_100_dec_2019_fcc, y = pct_nonfarm_bea_2018)) +
  geom_point(color = "grey50", size = 1.5) + geom_smooth(se = F, color = "red") +
  xlab("FCC 1000/100 Mbps Availability") +
  ylab("Nonfarm proprietors share") +
  theme_minimal() + theme(axis.title = element_text(size = 11, face = "bold"),
                          axis.text = element_text(size = 11)) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent)
# Broadband adoption
scat_bbadopt_nfprop <- ggplot(d, aes(x = pct_fixed_acs_2018, y = pct_nonfarm_bea_2018)) +
  geom_point(color = "grey50", size = 1.5) + geom_smooth(se = F, color = "red") +
  xlab("Fixed broadband adoption (ACS, 2018)") +
  ylab("Nonfarm proprietors share") +
  theme_minimal() + theme(axis.title = element_text(size = 11, face = "bold"),
                          axis.text = element_text(size = 11)) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent)
# Broadband QoS
scat_bbqos_nfprop <- ggplot(d, aes(x = pct_bb_qos, y = pct_nonfarm_bea_2018)) +
  geom_point(color = "grey50", size = 1.5) + geom_smooth(se = F, color = "red") +
  xlab("Broadband quality of service") +
  ylab("Nonfarm proprietors share") +
  theme_minimal() + theme(axis.title = element_text(size = 11, face = "bold"),
                          axis.text = element_text(size = 11)) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent)

bb_nfprop_grid <- gridExtra::grid.arrange(scat_fcc25_3_nfprop, scat_fcc100_10_nfprop, scat_fcc250_25_nfprop, scat_fcc1000_100_nfprop,
                                          scat_bbadopt_nfprop, scat_bbqos_nfprop,
                                          nrow = 3, ncol = 2, top = "Broadband & Nonfarm Proprietor Share")

## Average Venture Density ##
# Broadband 25/3
scat_bb25_3_vd <- ggplot(d, aes(x = pct25_3_dec_2019_fcc, y = vd_mean_20)) +
  geom_point(color = "grey50", size = 1.5) + geom_smooth(se = F, color = "red") +
  xlab("FCC 25/3 Mbps Availability") +
  ylab("Average venture density") +
  theme_minimal() + theme(axis.title = element_text(size = 11, face = "bold"),
                          axis.text = element_text(size = 11)) +
  scale_x_continuous(labels = scales::percent)
# Broadband 100/10
scat_bb100_10_vd <- ggplot(d, aes(x = pct100_10_dec_2019_fcc, y = vd_mean_20)) +
  geom_point(color = "grey50", size = 1.5) + geom_smooth(se = F, color = "red") +
  xlab("FCC 100/10 Mbps Availability") +
  ylab("Average venture density") +
  theme_minimal() + theme(axis.title = element_text(size = 11, face = "bold"),
                          axis.text = element_text(size = 11)) +
  scale_x_continuous(labels = scales::percent)
# Broadband 250/25
scat_bb250_25_vd <- ggplot(d, aes(x = pct250_25_dec_2019_fcc, y = vd_mean_20)) +
  geom_point(color = "grey50", size = 1.5) + geom_smooth(se = F, color = "red") +
  xlab("FCC 250/25 Mbps Availability") +
  ylab("Average venture density") +
  theme_minimal() + theme(axis.title = element_text(size = 11, face = "bold"),
                          axis.text = element_text(size = 11)) +
  scale_x_continuous(labels = scales::percent)
# Broadband 1000/100
scat_bb1000_100_vd <- ggplot(d, aes(x = pct1000_100_dec_2019_fcc, y = vd_mean_20)) +
  geom_point(color = "grey50", size = 1.5) + geom_smooth(se = F, color = "red") +
  xlab("FCC 1000/100 Mbps Availability") +
  ylab("Average venture density") +
  theme_minimal() + theme(axis.title = element_text(size = 11, face = "bold"),
                          axis.text = element_text(size = 11)) +
  scale_x_continuous(labels = scales::percent)
# Broadband adoption
scat_bbadopt_vd <- ggplot(d, aes(x = pct_fixed_acs_2018, y = vd_mean_20)) +
  geom_point(color = "grey50", size = 1.5) + geom_smooth(se = F, color = "red") +
  xlab("Fixed broadband adoption (ACS, 2018)") +
  ylab("Average venture density") +
  theme_minimal() + theme(axis.title = element_text(size = 11, face = "bold"),
                          axis.text = element_text(size = 11)) +
  scale_x_continuous(labels = scales::percent)
# Broadband QoS
scat_bbqos_vd <- ggplot(d, aes(x = pct_bb_qos, y = vd_mean_20)) +
  geom_point(color = "grey50", size = 1.5) + geom_smooth(se = F, color = "red") +
  xlab("Broadband quality of service") +
  ylab("Average venture density") +
  theme_minimal() + theme(axis.title = element_text(size = 11, face = "bold"),
                          axis.text = element_text(size = 11)) +
  scale_x_continuous(labels = scales::percent)

bb_vd_grid <- gridExtra::grid.arrange(scat_bb25_3_vd, scat_bb100_10_vd, scat_bb250_25_vd, scat_bb1000_100_vd,
                                          scat_bbadopt_vd, scat_bbqos_vd,
                                          nrow = 3, ncol = 2, top = "Broadband & Average Venture Density")

#### Scatter plots of Broadband / Entrepreneurship measures with rurality measures ####
## IRR 2010 ##
# Broadband 25/3
scat_bb25_3_irr <- ggplot(d, aes(x = IRR2010, y = pct25_3_dec_2019_fcc)) +
  geom_point(color = "grey50", size = 1.5) + geom_smooth(se = F, color = "red") +
  xlab("IRR (2010)") +
  ylab("FCC (25/3)") +
  theme_minimal() + theme(axis.title = element_text(size = 11, face = "bold"),
                          axis.text = element_text(size = 11)) +
  scale_y_continuous(labels = scales::percent)
# Broadband 100/10
scat_bb100_10_irr <- ggplot(d, aes(x = IRR2010, y = pct100_10_dec_2019_fcc)) +
  geom_point(color = "grey50", size = 1.5) + geom_smooth(se = F, color = "red") +
  xlab("IRR (2010)") +
  ylab("FCC (100/10)") +
  theme_minimal() + theme(axis.title = element_text(size = 11, face = "bold"),
                          axis.text = element_text(size = 11)) +
  scale_y_continuous(labels = scales::percent)
# Broadband 250/25
scat_bb250_25_irr <- ggplot(d, aes(x = IRR2010, y = pct250_25_dec_2019_fcc)) +
  geom_point(color = "grey50", size = 1.5) + geom_smooth(se = F, color = "red") +
  xlab("IRR (2010)") +
  ylab("FCC (250/25)") +
  theme_minimal() + theme(axis.title = element_text(size = 11, face = "bold"),
                          axis.text = element_text(size = 11)) +
  scale_y_continuous(labels = scales::percent)
# Broadband 1000/100
scat_bb1000_100_irr <- ggplot(d, aes(x = IRR2010, y = pct1000_100_dec_2019_fcc)) +
  geom_point(color = "grey50", size = 1.5) + geom_smooth(se = F, color = "red") +
  xlab("IRR (2010)") +
  ylab("FCC (1000/100)") +
  theme_minimal() + theme(axis.title = element_text(size = 11, face = "bold"),
                          axis.text = element_text(size = 11)) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1))
# Broadband adoption
scat_bbadopt_irr <- ggplot(d, aes(x = IRR2010, y = pct_fixed_acs_2018)) +
  geom_point(color = "grey50", size = 1.5) + geom_smooth(se = F, color = "red") +
  xlab("IRR (2010)") +
  ylab("Broadband adoption") +
  theme_minimal() + theme(axis.title = element_text(size = 11, face = "bold"),
                          axis.text = element_text(size = 11)) +
  scale_y_continuous(labels = scales::percent)
# Broadband QoS
scat_bbqos_irr <- ggplot(d, aes(x = IRR2010, y = pct_bb_qos)) +
  geom_point(color = "grey50", size = 1.5) + geom_smooth(se = F, color = "red") +
  xlab("IRR (2010)") +
  ylab("Broadband QoS") +
  theme_minimal() + theme(axis.title = element_text(size = 11, face = "bold"),
                          axis.text = element_text(size = 11)) +
  scale_y_continuous(labels = scales::percent)
## Entrepreneurship
# Nonfarm proprietors share
scat_nfprop_irr <- ggplot(d, aes(x = IRR2010, y = pct_nonfarm_bea_2018)) +
  geom_point(color = "grey50", size = 1.5) + geom_smooth(se = F, color = "red") +
  xlab("IRR (2010)") +
  ylab("Nonfarm Prprtr") +
  theme_minimal() + theme(axis.title = element_text(size = 11, face = "bold"),
                          axis.text = element_text(size = 11)) +
  scale_y_continuous(labels = scales::percent)
# Average venture density
scat_vd_irr <- ggplot(d, aes(x = IRR2010, y = vd_mean_20)) +
  geom_point(color = "grey50", size = 1.5) + geom_smooth(se = F, color = "red") +
  xlab("IRR (2010)") +
  ylab("Venture Density") +
  theme_minimal() + theme(axis.title = element_text(size = 11, face = "bold"),
                          axis.text = element_text(size = 11)) +
  scale_y_continuous(labels = scales::percent)

bb_ent_irr_grid <- gridExtra::grid.arrange(scat_bb25_3_irr, scat_bb100_10_irr, scat_bb250_25_irr, scat_bb1000_100_irr,
                                          scat_bbadopt_irr, scat_bbqos_irr,
                                          scat_nfprop_irr, scat_vd_irr,
                                          nrow = 4, ncol = 2, top = "Broadband, Entreprenuership & IRR")

## RUCC (2013) ##
# Broadband 25/3
scat_bb25_3_rucc <- ggplot(d, aes(x = RUCC_cat, y = pct25_3_dec_2019_fcc)) +
  geom_boxplot(outlier.color = "red", fill = "grey90") +
  geom_point(color = "grey50", size = 1.5, alpha = 0.5) +
  xlab("RUCC (2013)") + ylab("FCC (25/3)") +
  theme_minimal() + theme(axis.title = element_text(size = 11, face = "bold"),
                          axis.text = element_text(size = 11)) +
  scale_y_continuous(labels = scales::percent)
# Broadband 100/10
scat_bb100_10_rucc <- ggplot(d, aes(x = RUCC_cat, y = pct100_10_dec_2019_fcc)) +
  geom_boxplot(outlier.color = "red", fill = "grey90") +
  geom_point(color = "grey50", size = 1.5, alpha = 0.5) +
  xlab("RUCC (2013)") + ylab("FCC (100/10)") +
  theme_minimal() + theme(axis.title = element_text(size = 11, face = "bold"),
                          axis.text = element_text(size = 11)) +
  scale_y_continuous(labels = scales::percent)
# Broadband 250/25
scat_bb250_25_rucc <- ggplot(d, aes(x = RUCC_cat, y = pct250_25_dec_2019_fcc)) +
  geom_boxplot(outlier.color = "red", fill = "grey90") +
  geom_point(color = "grey50", size = 1.5, alpha = 0.5) +
  xlab("RUCC (2013)") + ylab("FCC (250/25)") +
  theme_minimal() + theme(axis.title = element_text(size = 11, face = "bold"),
                          axis.text = element_text(size = 11)) +
  scale_y_continuous(labels = scales::percent)
# Broadband 1000/100
scat_bb1000_100_rucc <- ggplot(d, aes(x = RUCC_cat, y = pct1000_100_dec_2019_fcc)) +
  geom_boxplot(outlier.color = "red", fill = "grey90") +
  geom_point(color = "grey50", size = 1.5, alpha = 0.5) +
  xlab("RUCC (2013)") + ylab("FCC (1000/100)") +
  theme_minimal() + theme(axis.title = element_text(size = 11, face = "bold"),
                          axis.text = element_text(size = 11)) +
  scale_y_continuous(labels = scales::percent)
# Broadband adoption
scat_bbadopt_rucc <- ggplot(d, aes(x = RUCC_cat, y = pct_fixed_acs_2018)) +
  geom_boxplot(outlier.color = "red", fill = "grey90") +
  geom_point(color = "grey50", size = 1.5, alpha = 0.5) +
  xlab("RUCC (2013)") + ylab("Broadband adoption") +
  theme_minimal() + theme(axis.title = element_text(size = 11, face = "bold"),
                          axis.text = element_text(size = 11)) +
  scale_y_continuous(labels = scales::percent)
# Broadband QoS
scat_bbqos_rucc <- ggplot(d, aes(x = RUCC_cat, y = pct_bb_qos)) +
  geom_boxplot(outlier.color = "red", fill = "grey90") +
  geom_point(color = "grey50", size = 1.5, alpha = 0.5) +
  xlab("RUCC (2013)") + ylab("Broadband QoS") +
  theme_minimal() + theme(axis.title = element_text(size = 11, face = "bold"),
                          axis.text = element_text(size = 11)) +
  scale_y_continuous(labels = scales::percent)
## Entrepreneurship
# Nonfarm proprietors share
scat_nfprop_rucc <- ggplot(d, aes(x = RUCC_cat, y = pct_nonfarm_bea_2018)) +
  geom_boxplot(outlier.color = "red", fill = "grey90") +
  geom_point(color = "grey50", size = 1.5, alpha = 0.5) + 
  xlab("RUCC (2013)") +
  ylab("Nonfarm Prprtr") +
  theme_minimal() + theme(axis.title = element_text(size = 11, face = "bold"),
                          axis.text = element_text(size = 11)) +
  scale_y_continuous(labels = scales::percent)
# Average venture density
scat_vd_rucc <- ggplot(d, aes(x = RUCC_cat, y = vd_mean_20)) +
  geom_boxplot(outlier.color = "red", fill = "grey90") +
  geom_point(color = "grey50", size = 1.5, alpha = 0.5) + 
  xlab("RUCC (2013)") +
  ylab("Venture Density") +
  theme_minimal() + theme(axis.title = element_text(size = 11, face = "bold"),
                          axis.text = element_text(size = 11)) +
  scale_y_continuous(labels = scales::percent)
  
bb_ent_rucc_grid <- gridExtra::grid.arrange(scat_bb25_3_rucc, scat_bb100_10_rucc, scat_bb250_25_rucc, scat_bb1000_100_rucc,
                                       scat_bbadopt_rucc, scat_bbqos_rucc,
                                       scat_nfprop_rucc, scat_vd_rucc,
                                       nrow = 4, ncol = 2, top = "Broadband, Entrepreneurship & RUCC")


# Save as pdf file
pdf(file = "variable-plots.pdf", paper = "letter")

gridExtra::grid.arrange(hist_fcc25_3, hist_fcc100_10, hist_fcc250_25, hist_fcc1000_100,
                        hist_nfprop, hist_vd, hist_irr, hist_rucc,
                        ncol = 2 , nrow = 4, top = "Distribution Histograms")

gridExtra::grid.arrange(scat_fcc25_3_nfprop, scat_fcc100_10_nfprop, scat_fcc250_25_nfprop, scat_fcc1000_100_nfprop,
                                          scat_bbadopt_nfprop, scat_bbqos_nfprop,
                                          nrow = 3, ncol = 2, top = "Broadband & Nonfarm Proprietor Share")

gridExtra::grid.arrange(scat_bb25_3_vd, scat_bb100_10_vd, scat_bb250_25_vd, scat_bb1000_100_vd,
                        scat_bbadopt_vd, scat_bbqos_vd,
                        nrow = 3, ncol = 2, top = "Broadband & Average Venture Density")

gridExtra::grid.arrange(scat_bb25_3_irr, scat_bb100_10_irr, scat_bb250_25_irr, scat_bb1000_100_irr,
                        scat_bbadopt_irr, scat_bbqos_irr,
                        scat_nfprop_irr, scat_vd_irr,
                        nrow = 4, ncol = 2, top = "Broadband, Entreprenuership & IRR")

gridExtra::grid.arrange(scat_bb25_3_rucc, scat_bb100_10_rucc, scat_bb250_25_rucc, scat_bb1000_100_rucc,
                        scat_bbadopt_rucc, scat_bbqos_rucc,
                        scat_nfprop_rucc, scat_vd_rucc,
                        nrow = 4, ncol = 2, top = "Broadband, Entrepreneurship & RUCC")
dev.off()


