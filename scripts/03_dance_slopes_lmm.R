library(tidyverse) # data handling
library(lme4) # Fit mixed effects models
library(performance) # Model diagnostics
library(DHARMa) # Residual diagnostics
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
d_1 <- cer_dat %>% group_by(lineage, location, colonyID) %>% summarise(n_bee = length(unique(beeID)))
write.csv(d_1, "data/output/Table_IndividualForagerNumbers.csv", row.names = F)

# We should run 3 models:
# 1. Sub species differences in dance dialects - 
#   comparing the dances of Acc Aci nd Ack. The Acc will only include data from Bangalore
# 2. Differences in dialects amongst two subspecies in the same location -
#   Comparing the dances of Acc and Aci in Bangalore
# 3. Differences in dialects within the same lineage in 2 environments -
#   Comparing the dances of Acc in Bangalore and Kullu

# Alternatively we can run one model with all the different lineage combinations


# Single model comparison -------------------------------------------------

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

summary(all_mod2) # Random effect variance is 0.007, which is 13.2% of the total variance (residual variance is 0.046)
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
write.csv(all_mod2_emm %>% as.data.frame(), "data/output/LMM_all_intercept_marginalmeans.csv", row.names = F)

# Output slopes
write.csv(all_mod2_slopes$emtrends %>% as.data.frame(), "data/output/LMM_all_slopes_trends.csv", row.names = F)

# Contrasts
write.csv(all_mod2_slopes$contrasts %>% as.data.frame(), "data/output/LMM_all_slopes_contrasts.csv", row.names = F)

## Model Predictions -------------------------------------------------------

all_mod2_pred <- emmip(all_mod2, distance~sub_lin, cov.keep="distance", CIs = T, plotit = F, at = list(distance = seq(0,500, by=1)))
write.csv(all_mod2_pred, "data/temp/LMM_all_predictions.csv", row.names = F)

## Random slope of individual BeeIID nested within ColonyID ------------------
# random slopes of beeID
cer_dat <- cer_dat %>%
  mutate(sub_lin_col_beeID = paste(sub_lin_col, beeID, sep = "_"))
all_mod_ind1 <- lmer(duration ~ distance * sub_lin + (distance|sub_lin_col/beeID), data = cer_dat, REML = F)
# Convergence issues with individual slopes. Retry with simpler random effect structure
all_mod_ind2 <- lmer(duration ~ distance * sub_lin + (1|sub_lin_col/beeID), data = cer_dat, REML = F)
# Convergence issues
all_mod_ind3 <- lmer(duration ~ distance * sub_lin + (1|sub_lin_col_beeID), data = cer_dat, REML = F)
check_model(all_mod_ind3)
# Homogeneity is good. Some outliers. Normality is okay. Posterior predictive plot is good. Normality of random eeffects is also good.
sim_out <- simulateResiduals(all_mod_ind2)
plot(sim_out) # QQ plot looks. Homogeneity shows some variiance but looks reasonable

summary(all_mod_ind3) # variance explained by random effect is 0.028, which is 32% (residual variance is 0.057)
estimate_slopes(all_mod_ind2, trend = "distance", at = c("sub_lin"))
all_mod_ind3_slopes <- emtrends(all_mod_ind3, pairwise ~ sub_lin, var = "distance")
all_mod_ind3_slopes

## Output model results ----------------------------------------------------
# Obtain intercept value differences between subpopulations by setting distance = 0
all_mod_ind3_emm <- emmeans(all_mod_ind3, ~sub_lin, at = list(distance = 0), lmerTest.limit = 13572, pbkrtest.limit = 13572)
write.csv(all_mod_ind3_emm %>% as.data.frame(), "data/output/LMM_all_re_individual_intercept_marginalmeans.csv", row.names = F)

# Output slopes
write.csv(all_mod_ind3_slopes$emtrends %>% as.data.frame(), "data/output/LMM_all_re_individual_intercept_trends.csv", row.names = F)

# Contrasts
write.csv(all_mod_ind3_slopes$contrasts %>% as.data.frame(), "data/output/LMM_all_re_individual_intercept_contrasts.csv", row.names = F)


## Model Predictions -------------------------------------------------------

all_mod_ind3_pred <- emmip(all_mod_ind3, distance ~ sub_lin, cov.keep = "distance", CIs = T, plotit = F, at = list(distance = seq(0, 500, by = 1)))
write.csv(all_mod_ind3_pred, "data/temp/LMM_all_re_individual_intercept_predictions.csv", row.names = F)

## Q1 - comparison across sub-populations ----------------------------------
cer_dat_q1 <- cer_dat_sum %>% filter(sub_lin %in% c("Acc_Ku", "Aci_B", "Ack_Ka"))
q1_mod1 <- lmer(mean_duration~distance*sub_lin + (distance|sub_lin_col), data = cer_dat_q1, REML = F)
# Convergence issue with this model. Retry with simpler random effect structure

q1_mod2 <- lmer(mean_duration~distance*sub_lin + (1|sub_lin_col), data = cer_dat_q1, REML = F)
# No convergence issue or other warnings
check_model(q1_mod2)
# Homogeneity plot shows slight increase with fit, Normality plot is slightly skewed at both ends.
# Normality of residuals plot is good. No major outliers. Posterior predictive plot also shows a good fit
# Normality of random effects plot is also good.
# the LMM shows reasonable validation of assumptions.
estimate_slopes(q1_mod2, trend = "distance", at=c("sub_lin"))
q1_mod2_slopes <- emtrends(q1_mod2, pairwise~sub_lin, var="distance")
q1_mod2_slopes
# Aci_B has the highest slope (slope = 0.0040) and Acc_Ku and Ack_Ka has similar slopes (0.0016 and 0.0018 respectively)
# The slope values are similar as in the combined model


## Q2 - comparison in a common garden experiment ---------------------------
cer_dat_q2 <- cer_dat_sum %>% filter(sub_lin %in% c("Acc_B", "Aci_B"))
q2_mod1 <- lmer(mean_duration~distance*sub_lin + (distance|sub_lin_col), data = cer_dat_q2, REML = F)
# Convergence issue with this model. Retry with simpler random effect structure

q2_mod2 <- lmer(mean_duration~distance*sub_lin + (1|sub_lin_col), data = cer_dat_q2, REML = F)
# No convergence issue or other warnings
check_model(q2_mod2)
# Homogeneity plot shows slight increase with fit, Normality plot is slightly skewed at both ends.
# Normality of residuals plot is good. No major outliers. Posterior predictive plot also shows a good fit
# Normality of random effects plot is also good.
# the LMM shows reasonable validation of assumptions.
estimate_slopes(q2_mod2, trend = "distance", at=c("sub_lin"))
q2_mod2_slopes <- emtrends(q2_mod2, pairwise~sub_lin, var="distance")
q2_mod2_slopes
# Aci_B has significantly higher slope than Acc_B (0.0040 and 0.0022 respectively)
# Slope values are the same as in the combined model


## Q3 - comparison across locations ---------------------------
cer_dat_q3 <- cer_dat_sum %>% filter(sub_lin %in% c("Acc_B", "Acc_Ku"))
q3_mod1 <- lmer(mean_duration~distance*sub_lin + (distance|sub_lin_col), data = cer_dat_q3, REML = F)
# Singular fit issue. Simplifying random effect structure

q3_mod2 <- lmer(mean_duration~distance*sub_lin + (1|sub_lin_col), data = cer_dat_q3, REML = F)
# No convergence issue or other warnings
check_model(q3_mod2)
# Homogeneity plot shows slight increase with fit, Normality plot is slightly skewed at both ends.
# Normality of residuals plot is good. No major outliers. Posterior predictive plot also shows a good fit
# Normality of random effects plot is also good.
# the LMM shows reasonable validation of assumptions.
estimate_slopes(q3_mod2, trend = "distance", at=c("sub_lin"))
q3_mod2_slopes <- emtrends(q3_mod2, pairwise~sub_lin, var="distance")
q3_mod2_slopes
# Acc_B has significantly higher slope than Acc_Ku (0.0022 and 0.0016 respectively)
# Slope values are the same as in the combined model

# The individual models have the same slopes and the same stastistical results as the combined model. 
# Either of them can be used interchangeably
