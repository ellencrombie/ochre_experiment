# importing the data - data that includes the repeats (so we can have standard deviation on the graph)

data_repeats_2 <- read.csv("data_080925_1417.csv")

# install packages
install.packages("RColorBrewer")
install.packages("patchwork")
library(RColorBrewer)
library("patchwork")

# grouped bar chart
data_repeats_proc_2 <- data_repeats_2 %>% 
  group_by(iron_ochre_amendement_percent, phosphorous_loading_conc) %>% 
  summarise(mean_P = mean(clean_P, na.rm=T), 
            sd_P = sd(clean_P, na.rm = T))

data_repeats_proc_3 <- data_repeats_proc_2 %>% drop_na()
                  
(barchart <- ggplot(data_repeats_proc_2, aes(x = factor(iron_ochre_amendement_percent),
                                y = mean_P,
                                fill = factor(phosphorous_loading_conc))) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymax = mean_P + sd_P, ymin = mean_P - sd_P),
                position = position_dodge(width = 0.9),
                width = 0.2,
                color = "black") +
  labs(x = "Ochre amendment (%)",
       y = "Mean phosphate sorption (%)",
       fill = "Phosphate loading 
concentration (mg P L⁻¹)") +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(expand = c(0, 0)) +  # remove space at the bottom
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black")
  ))

# grouped bar chart with 0 removed and larger text
(barchart_3 <- ggplot(data_repeats_proc_3, aes(x = factor(iron_ochre_amendement_percent),
                                              y = mean_P,
                                              fill = factor(phosphorous_loading_conc))) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymax = mean_P + sd_P, ymin = mean_P - sd_P),
                position = position_dodge(width = 0.9),
                width = 0.2,
                color = "black") +
  labs(x = "Ochre amendment (%)",
       y = "Mean phosphate sorption (%)",
       fill = "Phosphate loading \nconcentration (mg P L⁻¹)") +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal(base_size = 14) +   # increase all text sizes
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size = 16),   # axis titles
    axis.text  = element_text(size = 14),   # axis tick labels
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 12),
    plot.title   = element_text(size = 18, face = "bold")
  ))

ggsave(filename = "bar chart.JPEG", plot = barchart)


# pH data presentation

ph <- read.csv("ph_080925.csv")

ph_proc <- ph %>% 
  group_by(pH) %>% 
  summarise(mean_P = mean(PO4removed_p, na.rm=T), 
            sd_P = sd(PO4removed_p, na.rm = T))

(ph_barchart_2 <- ggplot(ph_proc, aes(x = factor(pH), y = mean_P)) +
  geom_bar(position = position_dodge(), stat = "identity", 
           fill = "#B3DF8A", width = 0.5) +  # make bars skinnier
  geom_errorbar(aes(ymax = mean_P + sd_P, ymin = mean_P - sd_P),
                position = position_dodge(width = 0.5),  # match bar width
                width = 0.2,
                color = "black") +
  labs(x = "pH",
       y = "Mean phosphate sorption (%)") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black")
  ))

# bar chart with bigger text
(ph_barchart_2 <- ggplot(ph_proc, aes(x = factor(pH), y = mean_P)) +
  geom_bar(position = position_dodge(), stat = "identity", 
           fill = "#B3DF8A", width = 0.5) +
  geom_errorbar(aes(ymax = mean_P + sd_P, ymin = mean_P - sd_P),
                position = position_dodge(width = 0.5),
                width = 0.2,
                color = "black") +
  labs(x = "pH",
       y = "Mean phosphate sorption (%)") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal(base_size = 14) +   # increase all text sizes
  theme(
    panel.grid   = element_blank(),
    axis.line    = element_line(colour = "black"),
    axis.title   = element_text(size = 16),  # axis labels
    axis.text    = element_text(size = 14),  # tick labels
    legend.title = element_text(size = 14),  # if you add legend later
    legend.text  = element_text(size = 12),
    plot.title   = element_text(size = 18, face = "bold") # if you add a title
  ))


# Box Plots for loading/ochre seperately

# simple box plot
boxplot_1 <- ggplot(data_repeats_2, aes(x = factor(iron_ochre_amendement_percent), y = normal_percent)) +
  geom_boxplot(fill = "lightblue", color = "darkgreen") +
  xlab("Iron Ochre Amendment (%)") +
  ylab("Mean Phosphate Sorption (%)") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),      # removes all grid lines
    axis.line = element_line(color = "black")  # keeps axis lines
  )
# phosphate laoding graph 
boxplot_2 <- ggplot(data_repeats_2, aes(x = factor(phosphorous_loading_conc), y = normal_percent)) +
  geom_boxplot(fill = "lightblue", color = "darkgreen") +
  xlab("Phosphate loading concentration (mg/L)") +
  ylab("Mean Phsophate Sorption (%)") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),           # removes all grid lines
    axis.line = element_line(color = "black") # keeps axis lines
  )
 
boxplot_1 + boxplot_2

install.packages("cowplot")
library(cowplot)
plot_grid(boxplot_1, boxplot_2, labels=c("A","B"))
