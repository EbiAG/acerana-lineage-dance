library(tidyverse) # data handling
library(lme4) # Fit mixed effects models
library(performance) # Model diagnostics
library(modelbased) # For contrast estimates and CI
library(emmeans) # For posthoc marginal means analysis


# Load data ---------------------------------------------------------------

cer_dat <- read.csv("data/temp/acerana_lineage_dance_duration.csv")
table(cer_dat$lineage, cer_dat$location, cer_dat$colonyID)
# We have 3 sub species of Apis cerana, 3 locations and 3 colony IDs
d <- cer_dat %>% group_by(lineage, location) %>% 
  summarise(n_col = length(unique(colonyID)), colonies = list(unique(colonyID)))
# We have 3 Ac cerana (Acc) colonies in Bangalore, and 2 in Kullu. 
# These are the same colonies in both locations, so we should include this in the analysis
# We also have 2 indica (Aci) colonies in Bangalore and 3 kashmirensis (Ack) in 3 colonies

# Table with individual numbers per colony
d_1 <- cer_dat %>% group_by(lineage, location, colonyID) %>% summarise(n_bee = length(unique(beeID)))
write.csv(d_1, "data/output/Table_IndividualForagerNumbers.csv", row.names = F)


# Dance distance slope comparison -------------------------------------------------

cer_dat <- cer_dat %>% 
  mutate(sub_lin = case_when(
    lineage == "Apis cerana cerana" & location == "Bangalore" ~ "Acc_B",
    lineage == "Apis cerana cerana" & location == "Kullu" ~ "Acc_Ku",
    lineage == "Apis cerana indica" & location == "Bangalore" ~ "Aci_B",
    lineage == "Apis cerana kashmirensis" & location == "Kashmir" ~ "Ack_Ka",
  ), sub_lin_col = paste(sub_lin, str_extract(colonyID, "\\d+"), sep="_"))

cer_dat_sum <- cer_dat %>% 
  group_by(sub_lin, sub_lin_col, beeID, distance, dance) %>% 
  summarise(mean_duration = mean(duration))

## LMM ---------------------------------------------------------------------

all_mod1 <- lmer(mean_duration~distance*sub_lin + (distance|sub_lin_col), data = cer_dat_sum, REML = F)
# Convergence issue with this model. Retry with simpler random effect structure

all_mod2 <- lmer(mean_duration~distance*sub_lin + (1|sub_lin_col), data = cer_dat_sum, REML = F)
# No convergence issue or other warnings
check_model(all_mod2)
# Homogeneity plot shows slight increase with fit, Normality plot is slightly skewed at both ends.
# Normality of residuals plot is good. No major outliers. Posterior predictive plot also shows a good fit
# The LMM shows reasonable validation of model assumptions. It can potentially be slightly improved by using an NLMM
# However, to maintain comparisons across the literature we obtain the slopes from this model
estimate_slopes(all_mod2, trend = "distance", at=c("sub_lin"))
all_mod2_slopes <- emtrends(all_mod2, pairwise~sub_lin, var="distance")
all_mod2_slopes
# The 3 subpopulations differ in the slopes in the following manner: Aci_B > Acc_B > Ack_Ka
# Within Bangalore, Aci has higher slope than Acc
# Within the same subpopulation of Acc, the bangalore lineage has a significantly higher slope
# The only non-significant difference in slope is between Acc in Kullu and Ack in Kashmir

## Output model results ----------------------------------------------------

# Obtain intercept value differences between subpopulations by setting distance = 0 
all_mod2_emm <- emmeans(all_mod2, ~sub_lin, at = list(distance=0))
write.csv(all_mod2_emm %>% as.data.frame(), "data/output/LMM_slopes_intercept_marginalmeans.csv", row.names = F)

# Output slopes
write.csv(all_mod2_slopes$emtrends %>% as.data.frame(), "data/output/LMM_slopes_trends.csv", row.names = F)

# Contrasts
write.csv(all_mod2_slopes$contrasts %>% as.data.frame(), "data/output/LMM_slopes_contrasts.csv", row.names = F)

## Model Predictions -------------------------------------------------------

all_mod2_pred <- emmip(all_mod2, distance~sub_lin, cov.keep="distance", CIs = T, plotit = F, at = list(distance = seq(0,500, by=1)))
write.csv(all_mod2_pred, "data/temp/LMM_slopes_predictions.csv", row.names = F)