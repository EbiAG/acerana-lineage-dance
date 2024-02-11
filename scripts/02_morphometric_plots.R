library(tidyverse)
library(factoextra)
library(ggpubr)
library(ggbeeswarm)
library(cowplot)
library(magick)


# Load data ---------------------------------------------------------------

morph_dat <- read.csv("data/temp/acerana_lineage_morphometric.csv") %>% 
  mutate(lineage = case_when(
    lineage == "Apis cerana indica (Bangalore)"~"A. indica",
    lineage == "Apis cerana cerana (Bangalore)"~"A. c. cerana (B)",
    lineage == "Apis cerana cerana (Kullu)"~"A. c. cerana (K)",
    lineage == "Apis cerana kashmirensis (Kashmir)"~"A. c. kashmirensis"
  )) %>% 
  arrange(match(lineage, c("A. indica", "A. c. cerana (B)", "A. c. cerana (K)", 
                              "A. c. kashmirensis"))) # Arrange in specific order

cva_dat <- read.csv("data/raw/morphometric/geometric_morph_cva_scores.csv") %>% 
  rename_with(str_to_lower, everything()) %>% 
  mutate(lineage = case_when(
    lineage == "A.indica"~"A. indica",
    lineage == "A.c.cerana (Bangalore)"~"A. c. cerana (B)",
    lineage == "A.c.cerana (Kullu)"~"A. c. cerana (K)",
    lineage == "A.c.kashmirensis"~"A. c. kashmirensis"
  ))

morph_dat$lineage <- factor(morph_dat$lineage, levels = c("A. indica", "A. c. cerana (B)", "A. c. cerana (K)", 
                             "A. c. kashmirensis"))
cva_dat$lineage <- factor(cva_dat$lineage, levels = c("A. indica", "A. c. cerana (B)", "A. c. cerana (K)", 
                                                            "A. c. kashmirensis"))


# PCA of all characteristics ----------------------------------------------

morph_pca <- prcomp(morph_dat[,5:22], scale = T, center = T)

# Eigen values
get_eigenvalue(morph_pca)
# 35.78% of variance explained by PC1 and 2. PC1 explains 22.86% of variance

morph_pca_var <- get_pca_var(morph_pca)
morph_pca_var$coord # Coordinates
round(morph_pca_var$contrib,3) 
# Dimension 1 is id, lfw, wfw (~18%), cub 1 (12%) and k19 (12%, -ve)
round(morph_pca_var$cos2,3) # Quality of representation

# Plot PCA
col <- c("#EF5350", "#9CCC65", "#26A69A", "#AB47BC") # Colours for Aci_B, Acc_B, Acc_Ku, Ack_Ka
# Plot PCA For moving average
plot_morph_pca <- fviz_pca_ind(morph_pca,
  select.var = list(contrib = 5), habillage = morph_dat$lineage,
  addEllipses = TRUE, ellipse.level = 0.5, ellipse.type="norm",
  ellipse.alpha = 0, label = "var", repel = T,
  pointsize = 4, mean.point = F
) +
  # geom_point(aes(colour=morph_dat$lineage), size=3) +
  scale_color_manual(values = col) +
  scale_shape_manual(values = c(19, 19, 19, 19)) +
  labs(title = "PCA of morphometric characteristics", x = "PC1 (22.86%)", y = "PC2 (12.92%)") +
  theme_bw() +
  theme(
    axis.text = element_text(size = 14, colour = "black"),
    axis.title = element_text(size = 16, colour = "black"),
    plot.title = element_text(size = 20),
    legend.text = element_text(size = 16)
  ) +
  theme(
    panel.grid = element_blank(), legend.position = c(0.2, 0.2), legend.title = element_blank(),
    legend.text = element_text(face = "italic"), legend.background = element_blank()
  )
plot_morph_pca


# Plot of CVA -------------------------------------------------------------
plot_morph_cva <- ggplot(cva_dat, aes(x = cv1, y = cv2, group = lineage, colour = lineage)) +
  geom_point(size = 4) +
  labs(title = "CVA of wing-shape characteristics", x = "CV1 (81.02%)", y = "CV2 (11.73%)") +
  scale_color_manual(values = col) +
  stat_ellipse(level = 0.5, type="norm") +
  theme_bw() +
  theme(
    axis.text = element_text(size = 14, colour = "black"),
    axis.title = element_text(size = 16, colour = "black"),
    plot.title = element_text(size = 20),
    legend.text = element_text(size = 16)
  ) +
  theme(panel.grid = element_blank(), legend.position = c(0.9, 0.2), legend.title = element_blank(), legend.text = element_text(face = "italic"), legend.background = element_blank())
plot_morph_cva


# Combine plot ------------------------------------------------------------

morph_plot <- ggarrange(plot_morph_pca, plot_morph_cva,
                        labels=c("a)", "b)"),
                        nrow=1, ncol=2,
                        common.legend = T, legend = "bottom")
morph_plot


# Combine plot including other images -------------------------------------

# Extract legend from cva plot in bottom position
cva_leg <- get_legend(plot_morph_cva + theme(legend.position = "bottom"))

# Create right side of multipanel plot with legend
multi_plot_right <- plot_grid(plot_morph_pca + theme(legend.position = "none"), plot_morph_cva + theme(legend.position = "none"),
                              cva_leg, nrow=3, rel_heights = c(1,1,0.1), labels = c("D", "E", ""))
multi_plot_right

# Create left side of multipanel plot with images
multi_plot_left <- ggdraw() +
  draw_image("data/raw/fig1/exp_loc.jpg", x=0, y=0.5, width=0.5, height=0.5) +
  draw_image("data/raw/fig1/phylo_tree.png", x=0.5, y=0.5, width=0.5, height=0.48) +
  draw_image("data/raw/fig1/foragers_crop.jpg", x=0, y=0, width = 1, height=0.5) +
  draw_plot_label(label=c("A", "B", "C"), size=15, x=c(0,0.5, 0), y = c(1, 1, 0.5)) +
  draw_label("A. indica", x=0.17, y=0.4, colour="#EF5350", fontface = "italic", size=16) +
  draw_label("A. c. cerana", x=0.49, y=0.4, colour="#62a398", fontface = "italic", size=16) +
  draw_label("A. c. kashmirensis", x=0.82, y=0.4, colour="#AB47BC", fontface = "italic", size=16)
multi_plot_left

# Combine multipanel plot sides
multi_plot_full <- plot_grid(multi_plot_left, multi_plot_right, nrow=1,
                             rel_widths = c(1,0.62))
multi_plot_full

# Save multipanel plot
save_plot("plots/Morphometric_Analysis_Combined_Images.png", plot = multi_plot_full, base_height = 12)
