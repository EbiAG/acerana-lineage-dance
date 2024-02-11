library(tidyverse)
library(ggtext)

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
  summarise(mean_duration = mean(duration)) %>% 
  ungroup() %>% mutate(sub_lin = factor(sub_lin, levels = c("Aci_B", "Acc_B", "Acc_Ku", "Ack_Ka")))

# Model predictions from LMM
cer_slopes <- read.csv("data/temp/LMM_all_predictions.csv") %>% 
  mutate(sub_lin = factor(sub_lin, levels = c("Aci_B", "Acc_B", "Acc_Ku", "Ack_Ka")))


# Plot --------------------------------------------------------------------
col <- c("#EF5350", "#9CCC65", "#26A69A", "#AB47BC") # Colours for Aci_B, Acc_B, Acc_Ku, Ack_Ka


## Mean_SD distributions ----------------------------------------------------
cer_plot_mean <- ggplot() +
  geom_ribbon(data = cer_slopes, aes(x=distance, ymin=LCL, ymax=UCL, group=sub_lin, fill=sub_lin), alpha=0.1) +
  geom_line(data = cer_slopes, aes(x=distance, y=yvar, colour=sub_lin), linewidth=1.2) +
  stat_summary(data = cer_dat, aes(x=distance, y = mean_duration, group=interaction(sub_lin, distance), colour=sub_lin, fill=sub_lin), 
                   fun.data="mean_sdl", position = position_dodge(width=20)) +
  scale_color_manual(values=col) +
  scale_fill_manual(values=col) +
  theme_bw() +
  theme(axis.ticks.x = element_blank(), axis.text = element_text(size=16, colour="black"), 
        axis.title = element_text(size=18, colour="black")) +
  theme(panel.grid = element_blank(), panel.background = element_blank()) +
  scale_x_continuous(name="Distance (m)", breaks=c(100,200,300,400,500)) +
  scale_y_continuous(name="Waggle Phase Duration (s)", limits = c(0,3.1), breaks=c(0,1,2,3)) +
  theme(legend.position = "none") +
  geom_richtext(aes(x = 10, y = 3), label = "*A. indica* : Intercept = 0.304, Slope = 0.0039", 
                colour = "#EF5350", size = 3.5, hjust = 0, fill = NA, label.color = NA) +
  geom_richtext(aes(x = 10, y = 2.9, ), label = "*A. c. cerana* (B): Intercept = 0.256, Slope = 0.0022", 
                colour = "#9CCC65", size = 3.5, hjust = 0, fill = NA, label.color = NA) +
  geom_richtext(aes(x = 10, y = 2.8, ), label = "*A. c. cerana* (K): Intercept = 0.246, Slope = 0.0016", 
                colour = "#26A69A", size = 3.5, hjust = 0, fill = NA, label.color = NA) +
  geom_richtext(aes(x = 10, y = 2.7, ), label = "*A. c. kashmirensis* : Intercept = 0.264, Slope = 0.0018", 
                colour = "#AB47BC", size = 3.5, hjust = 0, fill = NA, label.color = NA)
cer_plot_mean
ggsave("plots/LMM_Slopes_MeanSD.png", plot = cer_plot_mean, height = 15, width = 20, units = "cm")
