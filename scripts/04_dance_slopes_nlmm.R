library(tidyverse) # data handling
library(lme4) # Fit mixed effects models
library(performance) # Model diagnostics
library(DHARMa) # Model diagnostics
library(modelbased) # For contrast estimates and CI
library(emmeans) # For posthoc marginal means analysis
library(nlme) #For nonlinear models
library(aomisc) #For logarithmic function

# Load data ---------------------------------------------------------------

cer_dat <- read.csv("data/temp/acerana_lineage_dance_duration.csv")

cer_dat <- cer_dat %>% 
  mutate(sub_lin = case_when(
    lineage == "Apis cerana cerana" & location == "Bangalore" ~ "Acc_B",
    lineage == "Apis cerana cerana" & location == "Kullu" ~ "Acc_Ku",
    lineage == "Apis cerana indica" & location == "Bangalore" ~ "Aci_B",
    lineage == "Apis cerana kashmirensis" & location == "Kashmir" ~ "Ack_Ka",
  ), sub_lin_col = paste(sub_lin, str_extract(colonyID, "\\d+"), sep="_"))

cer_dat_sum <- cer_dat %>% 
  group_by(sub_lin, sub_lin_col, beeID, distance, dance) %>% 
  dplyr::summarise(mean_duration = mean(duration)) %>% 
  mutate(sub_lin = factor(sub_lin))


# Single model comparison -------------------------------------------------

## NLMM --------------------------------------------------------------------
# The logarithmic function is a good fit for how the waggle phase duration changes with distance
# The function is of the form:
# Y ~ a + b*log(X)

# The aomisc package has a self-starting function that can be used with nls() called NLS.logCurve()
# https://www.statforbiology.com/2020/stat_nls_usefulfunctions/#logarithmic-function
# Once starting values are obtained from this function, we can then fit it to the nlme function with random effects

### Get starting values -----------------------------------------------------
# We obtain starting values using the function nls() for the model without interaction and nldList() for the model with interaction
nls_cer <- nls(mean_duration~NLS.logCurve(distance,a,b), data = cer_dat_sum) # For model without interaction
summary(nls_cer) # a = -2.18, b = 0.58
nls_cer_int <- nlsList(mean_duration~NLS.logCurve(distance,a,b)|sub_lin, data = cer_dat_sum) # For model with interaction
summary(nls_cer_int) # a: Acc_B=-1.79, Acc_Ku=-0.55, Aci_B=-3.32, Ack_Ka=-1.22; b: Acc_B=0.48, Acc_Ku=0.21, Aci_B=0.86, Ack_Ka=0.37


### Fit NLMM ----------------------------------------------------------------

# Model without interaction
nlmm_cer <- nlme(mean_duration~NLS.logCurve(distance,a,b), fixed=a+b~1, random=b~1|sub_lin_col, data = cer_dat_sum,
                 start=c(-2.18, 0.58), control = nlmeControl(opt="nlminb", msMaxIter = 100))
# Model with interaction
nlmm_cer_int <- nlme(mean_duration~NLS.logCurve(distance,a,b), fixed=a+b~sub_lin, random=b~1|sub_lin_col, data = cer_dat_sum,
                 start=c(-1.79, 0.48, -0.55, 0.21, -3.32, 0.86, -1.22, 0.37), control = nlmeControl(opt="nlminb", msMaxIter = 100))
anova(nlmm_cer, nlmm_cer_int)
# Model with interaction is significantly better

### Model diagnostics -------------------------------------------------------
plot(nlmm_cer_int) # Variation is okay. Shows some skew
qqnorm(nlmm_cer_int$residuals)
qqline(nlmm_cer_int$residuals) # Residuals also show some skew at both ends
nlmm_cer_int_ranef <- ranef(nlmm_cer_int)$b
qqnorm(nlmm_cer_int_ranef)
qqline(nlmm_cer_int_ranef) # Random effects normally distributed except for one point each at either end



### Controlling for heteroscedasticity --------------------------------------
nlmm_cer_het <- nlme(mean_duration~NLS.logCurve(distance,a,b), fixed=a+b~sub_lin, random=b~1|sub_lin_col, data = cer_dat_sum,
                     start=c(-1.79, 0.48, -0.55, 0.21, -3.32, 0.86, -1.22, 0.37), weights = varPower(30,~distance),
                     control = nlmeControl(opt="nlminb", msMaxIter = 100))
plot(nlmm_cer_het) # Variation is much better now. Some skew is still present
qqnorm(nlmm_cer_het$residuals)
qqline(nlmm_cer_het$residuals) # Residuals still show some skew at both ends
nlmm_cer_het_ranef <- ranef(nlmm_cer_het)$b
qqnorm(nlmm_cer_het_ranef)
qqline(nlmm_cer_het_ranef) # Random effects normally distributed except for one point each at either end

summary(nlmm_cer_het)
emmeans(nlmm_cer_het, pairwise~sub_lin, param = "b")
# The slope equivalent values show the same trend as in the LMM single model - Aci_B > Acc_B > Ack_Ka > Acc_Ku
# Values of b are 0.775 Aci_B, 0.427 Acc_B, 0.328 Ack_Ka, and 0.183 Acc_Ku
# Comparison of the contrasts show significant differences for all comparisons.
# This is also similar to the results from the signle LMM, with the exception that Acc_Ku and Ack_Ka are also different in this case


### Output model results ----------------------------------------------------
# marginal means
# Intercept
a_emmeans <- emmeans(nlmm_cer_het, pairwise~sub_lin, param = "a")
write.csv(a_emmeans$emmeans %>% as.data.frame(), "data/output/NLMM_intercept_marginalmeans.csv", row.names = F)

# Slope
b_emmeans <- emmeans(nlmm_cer_het, pairwise~sub_lin, param = "b")
write.csv(b_emmeans$emmeans %>% as.data.frame(), "data/output/NLMM_slopes_marginalmeans.csv", row.names = F)

# Contrasts
# Intercept
write.csv(a_emmeans$contrasts %>% as.data.frame(), "data/output/NLMM_intercept_contrasts.csv", row.names = F)
# Slope
write.csv(b_emmeans$contrasts %>% as.data.frame(), "data/output/NLMM_slopes_contrasts.csv", row.names = F)


### Model predictions -------------------------------------------------------
distance <- rep(seq(1,520, by=1),4)
sub_lin = rep(c("Acc_B", "Acc_Ku", "Aci_B", "Ack_Ka"), each=520)
pred <- as.data.frame(cbind(distance,sub_lin))
pred$distance <- as.numeric(pred$distance)
pred$sub_lin <- as.factor(pred$sub_lin)
str(pred)

nlmm_cer_fits <- cbind(pred, fit=predict(nlmm_cer_het, newdata = pred, level=0))
write.csv(nlmm_cer_fits, "data/temp/NLMM_slopes_predictions.csv", row.names = F)
