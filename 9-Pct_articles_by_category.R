#Corinne Bassett
#Standalone script: faceted bar chart of percent of articles by composition
#metric, faceted by metric category, with Avian and Carbon shown side by side.
#Categories and metric names are kept exactly as stored in ListofUFCompMetrics.xlsx.
#Depends on pct.avian and pct.carbon from 2-PctArticlesbyMetricsbyAvianCarbon.R.

library(tidyverse)
library(ggh4x)   # facet_manual() for a two-row layout with equal bar widths

# Combine the carbon and avian percent-of-articles tables into long form, with a
# Group column distinguishing the two. Set category order for the facet layout.
pct.combined <- bind_rows(
  pct.avian  |> mutate(Group = "Avian"),
  pct.carbon |> mutate(Group = "Carbon")
) |>
  mutate(
    Category = factor(Category,
      levels = c("size", "structure", "species",
                 "tree characteristics", "vegetation layer type")),
    # Order metrics within each panel by mean percent across the two groups
    Column = fct_reorder(Column, Percent, .fun = mean, .desc = TRUE)
  )

# Group colors matching the other scripts (e.g. 7-VegLayerTypes.R)
group_colors <- c("Avian" = "#404788FF", "Carbon" = "#FDE725FF")

# Each panel spans grid columns equal to its metric count, so bar widths are
# identical across panels. Row 1: size (5), structure (4), species (2), padded
# with "#" to 13 columns; Row 2: tree characteristics (7), vegetation layer (6).
design <- "AAAAABBBBCC##
DDDDDDDEEEEEE"

dodge <- position_dodge(width = 0.9)

faceted_combined_barplot <- pct.combined |>
  ggplot(aes(x = Column, y = Percent, fill = Group)) +
  geom_col(position = dodge, width = 0.8, color = "black", linewidth = 0.2) +
  geom_text(aes(label = paste0(round(Percent, 1), "%")),
            position = dodge, vjust = -0.3, size = 2) +
  facet_manual(~ Category, design = design, scales = "free_x") +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 105)) +
  scale_fill_manual(values = group_colors) +
  labs(
    title = "Percent of Avian and Carbon articles by Composition Metric and Category",
    x = "Composition Metric",
    y = "Percent of Articles",
    fill = "Group"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    strip.background = element_rect(fill = "grey90", color = NA),
    strip.text = element_text(face = "bold"),
    panel.grid.major.x = element_blank()
  )

print(faceted_combined_barplot)

ggsave("figs/pct_articles_by_category.pdf", plot = faceted_combined_barplot,
       width = 12, height = 8, units = "in", dpi = 300)
