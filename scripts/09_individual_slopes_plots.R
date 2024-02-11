library(tidyverse)
library(grid)
library(gtable)
library(gridExtra)
library(cowplot)
library(paletteer)


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

ind_slopes_plot <- ggplot(data = ind_dat, aes(x=distance, y = mean_duration, group=as.factor(bee_n), colour=as.factor(bee_n))) +
  #geom_ribbon(data = ind_pred, aes(x=distance, y=yvar, ymin=LCL, ymax=UCL, group=col_beeID, fill=col_beeID), alpha=0.1) +
  geom_line(data = ind_pred, aes(x=distance, y=yvar, colour=as.factor(bee_n)), linewidth=1, alpha=0.7) +
  stat_summary(fun.data="mean_sdl", position = position_dodge(width=20), alpha=0.7) +
  facet_grid(sub_lin~colonyID) +
  # scale_color_paletteer_d("ggthemes::Hue_Circle") +
  scale_color_manual(values = col_ind_new) +
  scale_x_continuous(name="Distance (m)") +
  scale_y_continuous(name = "Waggle Phase Duration (s)") +
  theme_bw() +
  theme(legend.position = "none", axis.text = element_text(size=12, colour="black"), 
        axis.title = element_text(colour="black", face = "bold", size=14)) +
  theme(panel.grid = element_blank(), strip.text = element_text(colour="black", face = "italic", size=14)) +
  theme(strip.background = element_blank(), strip.placement = "outside", panel.background = element_blank())
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


# Coefficient of variation ------------------------------------------------

ind_slopes_cv <- read.csv("data/output/LM_ind_slopes.csv") %>% 
  group_by(sub_lin, colonyID) %>% 
  summarise(n_bees = n(), mean_slopes = mean(distance.trend), sd_slopes = sd(distance.trend), cv_slopes = sd_slopes/mean_slopes) %>% 
  filter(!is.na(cv_slopes)) %>% mutate(sub_lin = factor(sub_lin, levels = c("Aci_B", "Acc_B", "Acc_Ku", "Ack_Ka")))

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

# Combine Plots -----------------------------------------------------------
# Create layout
lay <- rbind(c(1,2,2),
             c(NA,2,2))
# This layout will make the 2nd plot twice as high and wide as first plot

# Rotate x axis labels for cv plot
ind_slopes_cv_plot <- ind_slopes_cv_plot + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) + 
  scale_y_continuous(name="Coefficient of variation \nin individual slopes")
ind_slopes_cv_plot

# Combine plots
multi_plot <- ggdraw() +
  draw_plot(ind_slopes_cv_plot, x = 0, y = 0.46, width = 0.35, height = 0.5) +
  draw_plot(ind_slopes_plot_g, x = 0.35, y = 0, width = 0.65, height = 1) +
  draw_plot_label(label = c("A", "B"), size=15, x=c(0,0.35), y = c(1, 1))
multi_plot

# Save plot
ggsave("plots/LM_Individual_Slopes_Combined.png", plot = multi_plot, height = 25, width = 40, units = "cm")
