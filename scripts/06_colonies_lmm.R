library(tidyverse) # data handling
library(lme4) # Fit mixed effects models
library(performance) # Model diagnostics
library(modelbased) # For contrast estimates and CI
library(emmeans) # For posthoc marginal means analysis

# Load data ---------------------------------------------------------------

cer_dat <- read.csv("data/temp/acerana_lineage_dance_duration.csv") %>% 
  mutate(sub = case_when(
    lineage == "Apis cerana cerana" & location == "Bangalore" ~ "Acc_B",
    lineage == "Apis cerana cerana" & location == "Kullu" ~ "Acc_Ku",
    lineage == "Apis cerana indica" & location == "Bangalore" ~ "Aci_B",
    lineage == "Apis cerana kashmirensis" & location == "Kashmir" ~ "Ack_Ka",
  ), sub_col = paste(sub, str_extract(colonyID, "\\d+"), sep="_"))

cer_dat_sum <- cer_dat %>% 
  group_by(sub, sub_col, beeID, distance, dance) %>% 
  summarise(mean_duration = mean(duration)) %>% ungroup() %>% 
  mutate(col_beeID = paste(sub_col, beeID, sep="_"))


# LMM for colony differences ----------------------------------------------

col_mod1 <- lmer(mean_duration~distance*sub_col + (distance|col_beeID), data = cer_dat_sum, REML = F)
# Convergence issues

col_mod2 <- lmer(mean_duration~distance*sub_col + (1|col_beeID), data = cer_dat_sum, REML = F)
check_model(col_mod2)
# Homogeneity is okay. Normalityp lot shows some skew
# Normality of residuals plot is good. Posterior predictive plot is good. No outliers
col_mod2_slopes <- emtrends(col_mod2, pairwise~sub_col, var="distance")
col_mod2_slopes$emtrends


## Output model results ----------------------------------------------------

# Obtain intercept value differences between sublineages by setting distance = 0 
col_mod2_emm <- emmeans(col_mod2, ~sub_col, at = list(distance=0))
write.csv(col_mod2_emm %>% as.data.frame(), "data/output/LMM_colonies_intercept_marginalmeans.csv", row.names = F)

# Output slopes
write.csv(col_mod2_slopes$emtrends %>% as.data.frame(), "data/output/LMM_colonies_slopes_trends.csv", row.names = F)

# Contrasts
write.csv(col_mod2_slopes$contrasts %>% as.data.frame(), "data/output/LMM_colonies_slopes_contrasts.csv", row.names = F)

## Model Predictions -------------------------------------------------------

col_mod2_pred <- emmip(col_mod2, distance~sub_col, cov.keep="distance", CIs = T, plotit = F, at = list(distance = seq(0,500, by=1)))
write.csv(col_mod2_pred, "data/temp/LMM_colonies_predictions.csv", row.names = F)