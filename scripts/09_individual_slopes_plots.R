library(tidyverse)
library(grid)
library(gtable)
library(gridExtra)
library(cowplot)
library(paletteer)
library(ggpp)
library(ggh4x)


# Individual Slopes -------------------------------------------------------


## Load data ---------------------------------------------------------------

# Prediction data
ind_pred <- read.csv("data/temp/LM_ind_predictions.csv") %>% 
  mutate(col_beeID = paste(sub_lin, colonyID, beeID, sep="_")) %>% 
  mutate(colonyID = paste("Colony", colonyID, sep="_")) %>% 
  mutate(sub_lin = factor(sub_lin, levels = c("Aci_B", "Acc_B", "Acc_Ku", "Ack_Ka")), colonyID = as.factor(colonyID))

# Replace IDs with numbers
beeid_n <- ind_pred %>%  
  mutate(sub_lin_col = paste(sub_lin, str_extract(colonyID, "\\d+"), sep="_")) %>% 
  group_by(sub_lin, colonyID, sub_lin_col) %>%
  distinct(beeID) %>% 
  mutate(bee_n = seq(1, n()), col_beeID = paste(sub_lin_col, beeID, sep="_")) %>% 
  ungroup() %>% select(col_beeID, bee_n)


# Raw data
ind_dat <- read.csv("data/temp/acerana_lineage_dance_duration.csv") %>% 
  mutate(sub_lin = case_when(
    lineage == "Apis cerana cerana" & location == "Bangalore" ~ "Acc_B",
    lineage == "Apis cerana cerana" & location == "Kullu" ~ "Acc_Ku",
    lineage == "Apis cerana indica" & location == "Bangalore" ~ "Aci_B",
    lineage == "Apis cerana kashmirensis" & location == "Kashmir" ~ "Ack_Ka",
  ), sub_lin_col = paste(sub_lin, str_extract(colonyID, "\\d+"), sep="_")) %>% 
  group_by(sub_lin, sub_lin_col, colonyID, beeID, distance, dance) %>% 
  summarise(mean_duration = mean(duration)) %>% ungroup() %>% 
  mutate(col_beeID = paste(sub_lin_col, beeID, sep="_")) %>% 
  filter(col_beeID %in% unique(ind_pred$col_beeID)) %>% 
  left_join(beeid_n) %>% mutate(sub_lin = factor(sub_lin, levels = c("Aci_B", "Acc_B", "Acc_Ku", "Ack_Ka")), colonyID = as.factor(colonyID))

# Add numbers to individual prediction data also
ind_pred <- ind_pred %>% left_join(beeid_n)

# Remove data from Acc_B Colony 3 since it has only 1 individual
ind_dat <- ind_dat %>% filter(sub_lin_col != "Acc_B_3")
ind_pred <- ind_pred %>% filter(!(sub_lin == "Acc_B" & colonyID == "Colony_3"))

# Change levels of sub_lin for labelling purposes
levels(ind_dat$sub_lin) <- c("A. indica", "A. c. cerana (B)", "A. c. cerana (K)", 
                             "A. c. kashmirensis")
levels(ind_pred$sub_lin) <- c("A. indica", "A. c. cerana (B)", "A. c. cerana (K)", 
                             "A. c. kashmirensis")

# Change levels of colony for labelling purposes
levels(ind_dat$colonyID) <- c("Colony 1", "Colony 2", "Colony 3")
levels(ind_pred$colonyID) <- c("Colony 1", "Colony 2", "Colony 3")

## Make plot -------------------------------------------------------------
# Get colours
col_ind <- paletteer_d("ggthemes::Hue_Circle", n=15) # Since we have max 15 individuals in a colony
col_ind_new <- col_ind[c(1,15,8,4,10,3,9,12,7,2,6,13,5,14,11)] # Reorder so that colonies with only few individuals have distinct colours for individuals

# Get sample sizes
n_bee <- ind_dat %>%
  group_by(sub_lin, colonyID) %>%
  summarise(n_bees = length(unique(beeID))) %>%
  ungroup()

ind_slopes_plot <- ggplot() +
  geom_point(data = ind_dat, aes(x = distance, y = mean_duration, group = as.factor(bee_n), colour = as.factor(bee_n)), alpha = 0) +
  # geom_ribbon(data = ind_pred, aes(x=distance, y=yvar, ymin=LCL, ymax=UCL, group=col_beeID, fill=col_beeID), alpha=0.1) +
  geom_line(data = ind_pred, aes(x = distance, y = yvar, colour = as.factor(bee_n)), linewidth = 1, alpha = 0.7) +
  stat_summary(data = ind_dat, aes(x = distance, y = mean_duration, group = as.factor(bee_n), colour = as.factor(bee_n)), fun.data = "mean_sdl", position = position_dodge(width = 20), alpha = 0.7) +
  facet_grid(sub_lin ~ colonyID) +
  # scale_color_paletteer_d("ggthemes::Hue_Circle") +
  scale_color_manual(values = col_ind_new) +
  scale_x_continuous(name = "Distance (m)") +
  scale_y_continuous(name = "Waggle Phase Duration (s)") +
  theme_bw() +
  theme(
    legend.position = "none", axis.text = element_text(size = 12, colour = "black"),
    axis.title = element_text(colour = "black", face = "bold", size = 14)
  ) +
  theme(panel.grid = element_blank(), strip.text = element_text(colour = "black", face = "italic", size = 14)) +
  theme(strip.background = element_blank(), strip.placement = "outside", panel.background = element_blank()) +
  geom_text_npc(data = n_bee, aes(label = paste("n = ", n_bees)), npcx = 0.05, npcy = 0.95)
ind_slopes_plot

# Remove empty facets
# Convert to grob object for removing blank facets
ind_slopes_plot_g <- ggplotGrob(ind_slopes_plot)

# Remove specific panels from the grid object
# ggplot3.4.0: The numbering follows the format of going from top left to bottom right with an increase in x value first then an increase in y value. So panel-1-1 is the top right most plot, next plot in the row is panel-2-1 and the row finishes at panel-5-1. panel-6-1 is the first plot in second row, panel-7-1 second plot and panel-1-2 is third plot. Use this numbering to remove blank facets
idx <- which(ind_slopes_plot_g$layout$name %in% 
               c("panel-1-3", "panel-2-3", "panel-3-3"))
for (i in idx) {
  ind_slopes_plot_g$grobs[[i]] <- nullGrob()
}

# Plot new
grid.newpage()
grid.draw(ind_slopes_plot_g)

# Save plot
ggsave("plots/LM_Individual_Slopes_MeanSD.png", plot=ind_slopes_plot_g, height = 35, width = 25, units = "cm")



# Coefficient of variation ------------------------------------------------

ind_slopes_cv <- read.csv("data/output/LM_ind_slopes.csv") %>% 
  rename("colonyID" = "Colony") %>% 
  separate(Slope, into = "slope", extra = "drop", sep = "\n") %>% 
  mutate(slope = as.numeric(slope), sub_lin = paste(Lineage, Location, sep = "_")) %>% 
  group_by(sub_lin, colonyID) %>% 
  summarise(n_bees = n(), mean_slopes = mean(slope), sd_slopes = sd(slope), cv_slopes = sd_slopes/mean_slopes) %>% 
  filter(!is.na(cv_slopes)) %>% mutate(sub_lin = factor(sub_lin, levels = c(
    "A. indica_Bangalore", "A. c. cerana_Bangalore", "A. c. cerana_Kullu",
    "A. c. kashmirensis_Kashmir")))

# Change levels of sub_lin for labelling purposes
levels(ind_slopes_cv$sub_lin) <- c(
  "A. indica", "A. c. cerana (B)", "A. c. cerana (K)",
  "A. c. kashmirensis"
)

col <- c("#EF5350", "#9CCC65", "#26A69A", "#AB47BC") # Colours for Aci_B, Acc_B, Acc_Ku, Ack_Ka

ind_slopes_cv_plot <- ggplot(ind_slopes_cv, aes(x=sub_lin, y = cv_slopes, colour = sub_lin)) +
  geom_point(size = 3, alpha = 0.5) +
  stat_summary(fun.data="mean_sdl", size=1.2) +
  scale_y_continuous(name="Coefficient of variation in individual slopes") +
  scale_colour_manual(values=col) + 
  theme_bw() +
  theme(legend.position = "none", axis.text = element_text(colour="black", size=12), axis.title =
          element_text(colour="black", size=16, face="bold"), panel.grid = element_blank(),
        axis.title.x = element_blank(), axis.text.x = element_text(face = "italic"))
ind_slopes_cv_plot
ggsave("plots/LM_Individual_Slopes_CoefVar.png", plot=ind_slopes_cv_plot, height = 20, width = 20, units = "cm")

# Combine Plots -----------------------------------------------------------
# Create layout
# lay <- rbind(
#   c(1, 2, 2),
#   c(NA, 2, 2)
# )
lay <- rbind(
  c(1, 1, 2),
  c(1, 1, NA)
)
# This layout will make the 1st plot twice as high and wide as second plot

# Rotate x axis labels for cv plot
ind_slopes_cv_plot <- ind_slopes_cv_plot + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) + 
  scale_y_continuous(name="Coefficient of variation \nin individual slopes")
ind_slopes_cv_plot

# Combine plots
# multi_plot <- ggdraw() +
#   draw_plot(ind_slopes_cv_plot, x = 0, y = 0.46, width = 0.35, height = 0.5) +
#   draw_plot(ind_slopes_plot_g, x = 0.36, y = 0, width = 0.65, height = 1) +
#   draw_plot_label(label = c("A", "B"), size = 15, x = c(0, 0.35), y = c(1, 1))
# multi_plot

multi_plot <- ggdraw() +
  draw_plot(ind_slopes_plot_g, x = 0, y = 0, width = 0.65, height = 1) +
  draw_plot(ind_slopes_cv_plot, x = 0.66, y = 0.46, width = 0.35, height = 0.5) +
  draw_plot_label(label = c("A", "B"), size=15, x=c(0,0.65), y = c(1, 1)) +
  theme(plot.background = element_rect(fill = "#FFFFFF", colour = NA))
multi_plot

# Save plot
ggsave("plots/LM_Individual_Slopes_Combined_v2.png", plot = multi_plot, height = 25, width = 40, units = "cm")


# Histogram of individual distances ---------------------------------------
ind_dat_dist <- read.csv("data/temp/acerana_lineage_dance_duration.csv") %>% 
  mutate(sub_lin = case_when(
    lineage == "Apis cerana cerana" & location == "Bangalore" ~ "Acc_B",
    lineage == "Apis cerana cerana" & location == "Kullu" ~ "Acc_Ku",
    lineage == "Apis cerana indica" & location == "Bangalore" ~ "Aci_B",
    lineage == "Apis cerana kashmirensis" & location == "Kashmir" ~ "Ack_Ka",
  ), sub_lin_col = paste(sub_lin, str_extract(colonyID, "\\d+"), sep="_")) %>% 
  group_by(sub_lin, sub_lin_col, colonyID, beeID, distance, dance) %>% 
  summarise(mean_duration = mean(duration)) %>% ungroup() %>% 
  mutate(col_beeID = paste(sub_lin_col, beeID, sep="_")) %>% 
  group_by(sub_lin, sub_lin_col, colonyID, beeID, col_beeID) %>% 
  summarise(min_distance = min(distance), max_distance = max(distance)) %>% 
  mutate(dist_pres = ifelse(max_distance - min_distance == 0, "Single", "Multiple")) %>% 
  pivot_longer(cols = c("min_distance", "max_distance"), names_to = "distance", values_to = "value") %>% 
  mutate(
    distance = factor(distance, levels = c("min_distance", "max_distance")),
    sub_lin = factor(sub_lin, levels = c("Aci_B", "Acc_B", "Acc_Ku", "Ack_Ka")), 
    colonyID = as.factor(colonyID),
    dist_pres = factor(dist_pres, levels = c("Single", "Multiple"))
  )

# Change levels of sub_lin for labelling purposes
levels(ind_dat_dist$sub_lin) <- c("A. indica", "A. c. cerana (B)", "A. c. cerana (K)", 
                             "A. c. kashmirensis")

# Boxplot of individuals which were present in only one distance
ind_dist_pres <- ind_dat_dist %>% 
  group_by(sub_lin, sub_lin_col, colonyID, dist_pres) %>% 
  summarise(n = n()) %>% 
  group_by(sub_lin, sub_lin_col, colonyID) %>% 
  mutate(n_perc = n/sum(n) * 100)

col <- c("#EF5350", "#9CCC65", "#26A69A", "#AB47BC") # Colours for Aci_B, Acc_B, Acc_Ku, Ack_Ka
ind_dist_pres_plot <- ggplot(ind_dist_pres, aes(x = dist_pres, y = n_perc, colour = sub_lin)) +
  geom_line(aes(group = sub_lin_col), linetype = "dashed", linewidth = 0.8, alpha = 0.4) +
  geom_point(size = 3, alpha = 0.8) +
  stat_summary(fun = "mean", size = 1) +
  scale_colour_manual(values = col) +
  scale_y_continuous(name = "Percentage of Individuals") +
  scale_x_discrete(name = "Presence at Single/Multiple Distances") +
  facet_wrap2(~factor(sub_lin, levels = c("A. indica", "A. c. cerana (B)", "A. c. cerana (K)", "A. c. kashmirensis")), 
              nrow = 2, strip = strip_themed(text_x = elem_list_text(colour = col))) +
  theme_bw() +
  theme(axis.text = element_text(size=16, colour="black"), 
        axis.title = element_text(size=18, colour="black"),
        panel.grid = element_blank(), 
        panel.background = element_blank(),
        legend.position = "none", 
        strip.text = element_text(size=16, face="italic"), 
        strip.background = element_blank(), 
        strip.placement = "outside")
ind_dist_pres_plot
# Save plot
ggsave("plots/Individual_Distance_Presence.png", plot=ind_dist_pres_plot, height = 20, width = 30, units = "cm")


ind_dat_dist_mult <- ind_dat_dist %>% 
  filter(dist_pres == "Multiple") %>% 
  pivot_wider(names_from = "distance", values_from = value) %>% 
  group_by(sub_lin, sub_lin_col, colonyID) %>% 
  arrange(min_distance, max_distance, .by_group = TRUE)

n_labels = ind_dat_dist_mult %>% 
  summarise(n = n())

col_grid <- rgb(235, 235, 235, 150, maxColorValue = 255)

ind_dat_dist_mult_plot <- ggplot(
  ind_dat_dist %>%  filter(dist_pres == "Multiple"), 
  aes(x = distance, y = value, colour = sub_lin)
  ) +
  geom_line(
    aes(group = col_beeID), 
    linetype = "dashed", linewidth = 0.8, alpha = 0.2) +
  geom_point(aes(group = col_beeID), size = 2, alpha = 0.4) +
  stat_summary(fun = "mean", size = 1) +
  scale_colour_manual(values = col) +
  facet_grid2(colonyID ~ factor(sub_lin, levels = c("A. indica", "A. c. cerana (B)", "A. c. cerana (K)", "A. c. kashmirensis")), 
              axes = "all", strip = strip_themed(text_x = elem_list_text(colour = col))) +
  scale_y_continuous(name = "Distance Value", limits = c(0, 500), breaks = c(seq(50, 500, 50)),
                     labels = c("50", "", "", "", "250", "", "", "", "", "500")) +
  scale_x_discrete(name = "Distance Range", labels = c("Min", "Max")) +
  theme_bw() +
  theme(
        axis.text = element_text(size = 12, colour="black"),
        axis.title = element_text(size=18, colour="black"),
        panel.grid = element_blank(), 
        panel.background = element_blank(),
        legend.position = "none", 
        strip.text.x = element_text(size=16, face="italic"),
        strip.text.y = element_text(size = 14),
        strip.background = element_blank(), 
        strip.placement = "outside",
        panel.grid.major.y = element_line(linetype = "dashed", colour = col_grid)
        ) +
  geom_text_npc(data = n_labels, aes(label = paste("n = ", n)), npcx = 0.05, npcy = 0.95)
ind_dat_dist_mult_plot

# Remove empty facets
# Convert to grob object for removing blank facets
ind_dat_dist_mult_plot_g <- ggplotGrob(ind_dat_dist_mult_plot)

# Remove specific panels from the grid object
# ggplot3.4.0: The numbering follows the format of going from top left to bottom right with an increase in x value first then an increase in y value. So panel-1-1 is the top right most plot, next plot in the row is panel-2-1 and the row finishes at panel-5-1. panel-6-1 is the first plot in second row, panel-7-1 second plot and panel-1-2 is third plot. Use this numbering to remove blank facets
idx <- which(ind_dat_dist_mult_plot_g$layout$name %in% 
               c("panel-3-3", "panel-2-4", "axis-b-1-3", "axis-b-3-3", "axis-l-3-1", "axis-l-3-3"))
for (i in idx) {
  ind_dat_dist_mult_plot_g$grobs[[i]] <- nullGrob()
}

# Plot new
grid.newpage()
grid.draw(ind_dat_dist_mult_plot_g)

# Save plot
ggsave("plots/Individual_MinMax_Distance.png", plot=ind_dat_dist_mult_plot_g, height = 25, width = 35, units = "cm")
