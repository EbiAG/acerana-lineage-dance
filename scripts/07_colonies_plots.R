library(tidyverse)
library(ggpp)
library(ggh4x)

# Load data ---------------------------------------------------------------

# Raw data
cer_dat <- read.csv("data/temp/acerana_lineage_dance_duration.csv") %>% 
  mutate(sub_lin = case_when(
    lineage == "Apis cerana cerana" & location == "Bangalore" ~ "Acc_B",
    lineage == "Apis cerana cerana" & location == "Kullu" ~ "Acc_Ku",
    lineage == "Apis cerana indica" & location == "Bangalore" ~ "Aci_B",
    lineage == "Apis cerana kashmirensis" & location == "Kashmir" ~ "Ack_Ka",
  ), sub_lin_col = paste(sub_lin, str_extract(colonyID, "\\d+"), sep="_")) %>% 
  group_by(sub_lin, sub_lin_col, beeID, distance, dance) %>% 
  summarise(mean_duration = mean(duration)) %>% ungroup() %>% 
  mutate(sub_lin = factor(sub_lin, levels = c("Aci_B", "Acc_B", "Acc_Ku", "Ack_Ka")))

# Model predictions from LMM
cer_slopes <- read.csv("data/temp/LMM_colonies_predictions.csv") %>% 
  mutate(sub_lin = str_sub(xvar, end=-3), sub_lin = factor(sub_lin, levels = c("Aci_B", "Acc_B", "Acc_Ku", "Ack_Ka"))) 


# Change levels of sub_lin for labelling purposes
levels(cer_dat$sub_lin) <- c("A. indica", "A. c. cerana (B)", "A. c. cerana (K)", 
                             "A. c. kashmirensis")
levels(cer_slopes$sub_lin) <- c(
  "A. indica", "A. c. cerana (B)", "A. c. cerana (K)",
  "A. c. kashmirensis"
)

# Plot --------------------------------------------------------------------
col <- c("#EF5350", "#9CCC65", "#26A69A", "#AB47BC") # Colours for Aci_B, Acc_B, Acc_Ku, Ack_Ka


## Mean_SD distributions ----------------------------------------------------

# Text for slope information
slope_text <- data.frame(sub_lin = c("A. indica", "A. c. cerana (B)", "A. c. cerana (K)", 
                             "A. c. kashmirensis"),
                         label = c("Colony 1 slope = 0.0046\nColony 2 slope = 0.0036",
                                   "Colony 1 slope = 0.0033\nColony 2 slope = 0.0019\nColony 3 slope = 0.0026",
                                   "Colony 1 slope = 0.0013\nColony 2 slope = 0.0019",
                                   "Colony 1 slope = 0.0018\nColony 2 slope = 0.0021\nColony 3 slope = 0.0017"),
                         colour_val = col)

levels(slope_text$sub_lin) <- c(
  "A. indica", "A. c. cerana (B)", "A. c. cerana (K)",
  "A. c. kashmirensis"
)

cer_plot_mean <- ggplot() +
  geom_ribbon(data = cer_slopes, aes(x=distance, ymin=LCL, ymax=UCL, group=sub_col, fill=sub_lin), alpha=0.1) +
  geom_line(data = cer_slopes, aes(x=distance, y=yvar, colour=sub_lin, group=sub_col), linewidth=1.2) +
  stat_summary(data = cer_dat, aes(x=distance, y = mean_duration, group=interaction(sub_lin_col, distance), colour=sub_lin, fill=sub_lin),
               fun.data="mean_sdl", position = position_dodge(width=20)) +
  scale_color_manual(values=col) +
  scale_fill_manual(values=col) +
  facet_wrap2(~factor(sub_lin, levels = c("A. indica", "A. c. cerana (B)", "A. c. cerana (K)", "A. c. kashmirensis")), 
              nrow = 2, strip = strip_themed(text_x = elem_list_text(colour = col))) +
  theme_bw() +
  theme(axis.text = element_text(size=16, colour="black"), 
        axis.title = element_text(size=18, colour="black")) +
  theme(panel.grid = element_blank(), panel.background = element_blank()) +
  scale_x_continuous(name="Distance (m)", breaks=c(100,200,300,400,500)) +
  scale_y_continuous(name="Waggle Phase Duration (s)", limits = c(0,3.1), breaks=c(0,1,2,3)) +
  theme(legend.position = "none", strip.text = element_text(size=16, face="italic"), 
        strip.background = element_blank(), strip.placement = "outside") +
  geom_text_npc(data = slope_text, mapping = aes(label = label, colour = sub_lin), fontface="bold", npcx = 0.05, npcy = 0.95)
cer_plot_mean
ggsave("plots/LMM_Colonies_Slopes_MeanSD.png", plot = cer_plot_mean, height = 25, width = 25, units = "cm")