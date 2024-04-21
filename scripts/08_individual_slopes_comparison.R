library(tidyverse) # data handling
library(lme4) # Fit mixed effects models
library(performance) # Model diagnostics
library(modelbased) # For contrast estimates and CI
library(emmeans) # For posthoc marginal means analysis


# Load data ---------------------------------------------------------------

cer_dat_ind <- read.csv("data/temp/acerana_lineage_dance_duration.csv") %>% 
  mutate(sub_lin = case_when(
    lineage == "Apis cerana cerana" & location == "Bangalore" ~ "Acc_B",
    lineage == "Apis cerana cerana" & location == "Kullu" ~ "Acc_Ku",
    lineage == "Apis cerana indica" & location == "Bangalore" ~ "Aci_B",
    lineage == "Apis cerana kashmirensis" & location == "Kashmir" ~ "Ack_Ka",
  ), sub_lin_col = paste(sub_lin, str_extract(colonyID, "\\d+"), sep="_")) %>% 
  group_by(sub_lin, sub_lin_col, beeID, distance, dance) %>% 
  summarise(mean_duration = mean(duration)) %>% 
  group_by(sub_lin, sub_lin_col, beeID) %>% 
  filter(length(unique(distance)) > 2) %>% 
  mutate(col_beeID = paste(sub_lin_col, beeID, sep="_")) %>% 
  ungroup()

cer_dat_ind %>% summarise(length(unique(col_beeID)))
cer_dat_ind %>% group_by(sub_lin_col) %>% summarise(length(unique(col_beeID)))
table(cer_dat_ind$col_beeID)

# Identify individuals with values only at 3 or 4 distances
# We use 3-4, since from 5 distances onwards we will have data for individuals for at least 300m 
cer_dat_ind_sub <- cer_dat_ind %>% group_by(col_beeID) %>% filter(length(unique(distance)) < 5) %>% # Select individuals with dances at 3 or 4 distances
  distinct(distance) %>% # Get one row per distance value
  filter(max(distance) - min(distance) < 300) %>% # Select individuals for which the range of data is less than 300m. These wwill be removed
  summarise() %>% pull(col_beeID) # get a list of individuals. There are 53 individuals

length(unique(cer_dat_ind_sub))

cer_dat_ind_rem <- cer_dat_ind %>% filter(!(col_beeID %in% cer_dat_ind_sub)) # Remove individuals whiich are present in the list to remove
cer_dat_ind_rem %>% summarise(length(unique(col_beeID))) # Gives 28 remaining individuals
cer_dat_ind_rem %>% group_by(sub_lin_col) %>% summarise(length(unique(col_beeID))) # Only few individuals per colony

# If we shortlist individuals which have dances for at least a distance of 300m, we are only left with 28 individuals from 81.
# Since this number is very very low, we will not use this shortlist
  
# LMM for individual slopes -----------------------------------------------

ind_mod1 <- lmer(mean_duration~distance*col_beeID + (distance|sub_lin_col), data = cer_dat_ind, REML = F)
# Boundary fit issue

ind_mod2 <- lmer(mean_duration~distance*col_beeID + (1|sub_lin_col), data = cer_dat_ind, REML = F)
# Boundary fit issue. Individual models  for each sublineage also gives the same issue.
check_model(ind_mod2)
# Homogeneity plot shows somee increase with fit. Normality plot shows some skew at both ends.
# No major outliers. Posterior predictive plot looks good. High collinearity present (so uncertainty in estimates may be inflated)
# Either we can use this simple model as is, or we can go for an even simpler model without random effects

ind_mod3 <- lm(mean_duration~distance*col_beeID, data = cer_dat_ind)
check_model(ind_mod3)
# More or less the same diagnostic plots as the second model as expected.

mod2_slopes <- emtrends(ind_mod2, pairwise~col_beeID, var="distance")
mod2_slopes <- mod2_slopes$emtrends %>% as.data.frame()
mod3_slopes <- emtrends(ind_mod3, pairwise~col_beeID, var="distance")
mod3_slopes <- mod3_slopes$emtrends %>% as.data.frame()

mod3_slopes$distance.trend - mod2_slopes$distance.trend
# We get the same slope values from both models since the differences are very close to 0. We can use either model


# Compare slopes ----------------------------------------------------------

ind_slopes <- mod3_slopes %>% 
  filter(distance.trend > 0) %>%  # Remove individuals with slopes less than 0. These are 4 individuals
  separate_wider_delim(col_beeID, delim = "_", names =  c("sub_lin", "location", "colonyID", "beeID"), too_many = "merge") %>% # Separate into individual columns. Some names have _, so use too_many argument to merge these names
  mutate(sub_lin = paste(sub_lin, location, sep="_")) %>% select(!location) # Add location to sub lineage name

ind_slopes_cv <- ind_slopes %>% 
  group_by(sub_lin, colonyID) %>% 
  summarise(n_bees = n(), mean_slopes = mean(distance.trend), sd_slopes = sd(distance.trend), cv_slopes = sd_slopes/mean_slopes) %>% 
  filter(!is.na(cv_slopes))


## Fit LM to slope coef of var ---------------------------------------------
slopes_cv_mod1 <- lm(cv_slopes~sub_lin, data = ind_slopes_cv)
check_model(slopes_cv_mod1)
# Homogeneity is okay (too few points to see any pattern). Normality is okay. 
# No major outliers. Posterior predictive plot is not good
slopes_cv_em <- emmeans(slopes_cv_mod1, pairwise~sub_lin)
slopes_cv_em
# No significant difference in the coefficient of variation of individual slopes across sublineages.


# Output results ----------------------------------------------------------

## Slopes of individuals ---------------------------------------------------
options(scipen = 999)
cer_dat_ind_range <- cer_dat_ind %>%
  group_by(sub_lin_col, beeID) %>%
  summarise(min_distance = min(distance), max_distance = max(distance)) %>%
  separate(sub_lin_col, into = c("sub_lin", "location", "colonyID"), sep = "_")

ind_slopes_csv <- ind_slopes %>%
  separate(sub_lin, into = c("sub_lin", "location"), sep = "_") %>% # Separate into individual columns
  left_join(cer_dat_ind_range) %>% # Add min and max distance values
  mutate(
    sub_lin = factor(sub_lin, levels = c("Aci", "Acc", "Ack")),
    location = factor(location, levels = c("B", "Ku", "Ka")),
    slope = paste(round(distance.trend, 4), "\n", "[", round(lower.CL, 4), " - ", round(upper.CL, 4), "]", sep = ""),
    range = paste(min_distance, " - ", max_distance, sep = "")
  ) %>% # Change levels of sub_lin for labelling purposes
  arrange(sub_lin, location, colonyID, beeID) %>% # Arrange data
  mutate(
    sub_lin = case_when(
      sub_lin == "Aci" ~ "A. indica",
      sub_lin == "Acc" ~ "A. c. cerana",
      sub_lin == "Ack" ~ "A. c. kashmirensis"
    ),
    location = case_when(
      location == "B" ~ "Bangalore",
      location == "Ku" ~ "Kullu",
      location == "Ka" ~ "Kashmir"
    )
  ) %>%
  dplyr::select(sub_lin, location, colonyID, beeID, slope, range) %>%
  rename("Lineage" = sub_lin, "Location" = location, "Colony" = colonyID, "Bee ID" = beeID, "Slope" = slope, "Range" = range)

write.csv(ind_slopes_csv, "data/output/LM_ind_slopes.csv", row.names = F)

## Average range within colonies ---------------------------------------------------

cer_dat_col_range <- cer_dat_ind_range %>%
  right_join(ind_slopes %>%
    separate(sub_lin, into = c("sub_lin", "location"), sep = "_")) %>%
  group_by(sub_lin, location, colonyID) %>%
  summarise(
    n_bees = n(),
    min_min_d = min(min_distance),
    max_min_d = max(min_distance),
    min_max_d = min(max_distance),
    max_max_d = max(max_distance)
  ) %>%
  filter(n_bees > 1) %>%
  mutate(
    sub_lin = factor(sub_lin, levels = c("Aci", "Acc", "Ack")),
    location = factor(location, levels = c("B", "Ku", "Ka")),
    range_min_d = paste(min_min_d, " - ", max_min_d, sep = ""),
    range_max_d = paste(min_max_d, " - ", max_max_d, sep = "")
  ) %>% # Change levels of sub_lin for labelling purposes
  arrange(sub_lin, location, colonyID) %>% # Arrange data
  mutate(
    sub_lin = case_when(
      sub_lin == "Aci" ~ "A. indica",
      sub_lin == "Acc" ~ "A. c. cerana",
      sub_lin == "Ack" ~ "A. c. kashmirensis"
    ),
    location = case_when(
      location == "B" ~ "Bangalore",
      location == "Ku" ~ "Kullu",
      location == "Ka" ~ "Kashmir"
    )
  ) %>%
  dplyr::select(sub_lin, location, colonyID, n_bees, range_min_d, range_max_d) %>%
  rename("Lineage" = sub_lin, "Location" = location, "Colony" = colonyID, "No. of Bees" = n_bees, "Minimum Distance Range" = range_min_d, "Maximum Distance Range" = range_max_d)  

write.csv(cer_dat_col_range, "data/output/Table_Colony_Individual_DistanceRanges.csv", row.names = F)

## Predictions of individual data ------------------------------------------
# Get IDs of bees with slopes greeater than 0
ind_ids_keep <- mod3_slopes %>% filter(distance.trend > 0) %>% pull(col_beeID) %>% as.character()

ind_mod3_pred <- emmip(ind_mod3, distance~col_beeID, cov.keep="distance", CIs = T, plotit = F, at = list(distance = seq(0,500, by=5)))
ind_mod3_pred <- ind_mod3_pred %>% 
  select(!c(xvar, tvar)) %>% # Remove unwanted columns
  filter(col_beeID %in% ind_ids_keep) %>% # Keep only individuals with slopes greater than 0
  separate_wider_delim(col_beeID, delim = "_", names =  c("sub_lin", "location", "colonyID", "beeID"), too_many = "merge") %>% # Separate into individual columns. Some names have _, so use too_many argument to merge these names
  mutate(sub_lin = paste(sub_lin, location, sep="_")) %>% select(!location) # Add location to sub lineage name

write.csv(ind_mod3_pred, "data/temp/LM_ind_predictions.csv", row.names = F)

## Results of LM comparing coef of var in slopes ---------------------------
# marginal means
write.csv(slopes_cv_em$emmeans %>% as.data.frame(), "data/output/LM_ind_cv_slopes_marginalmeans.csv", row.names = F)

# contrasts
write.csv(slopes_cv_em$contrasts %>% as.data.frame(), "data/output/LM_ind_cv_slopes_contrasts.csv", row.names = F)
