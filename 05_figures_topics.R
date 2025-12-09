# 05_figures_topics.R
# ------------------------------------------------------------------
# Purpose:
#   Create topic-evolution figures for the paper using the standard
#   topic-count outputs produced by 01_topic_analysis.R:
#     - output/topics/articles_per_topic_<group>_<year>.csv
#
# This script produces:
#   1) A single-group line plot (example: religious minorities)
#   2) A 2×2 faceted plot for all four groups
#
# Inputs (relative to project root):
#   output/topics/articles_per_topic_*.csv
#     with columns:
#       - Topic
#       - ArticleCount
#       - Group
#       - Year
#
# Outputs (relative to project root):
#   output/figures/topics_religious_by_topic.pdf / .tiff
#   output/figures/topics_evolution_all_groups.pdf / .tiff
# ------------------------------------------------------------------

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(stringr)

# --------------------------- Paths & setup -----------------------------------

topic_counts_dir <- file.path("output", "topics")
figures_dir      <- file.path("output", "figures")

if (!dir.exists(topic_counts_dir)) {
  stop("Directory not found: ", topic_counts_dir,
       "\nPlease run master.R / 01_topic_analysis.R first.")
}

if (!dir.exists(figures_dir)) {
  dir.create(figures_dir, recursive = TRUE)
}

# --------------------------- 1. Load topic count files -----------------------

# We expect files named like:
#   articles_per_topic_lgbt_2002.csv
#   articles_per_topic_religious_2010.csv
# etc.
topic_count_files <- list.files(
  topic_counts_dir,
  pattern    = "^articles_per_topic_.*\\.csv$",
  full.names = TRUE
)

if (length(topic_count_files) == 0) {
  stop("No 'articles_per_topic_*.csv' files found in: ", topic_counts_dir)
}

# Read and combine all topic-count files
topic_counts <- topic_count_files |>
  lapply(function(path) {
    df <- read_csv(path, show_col_types = FALSE)
    
    # Standardize column names (lowercase for safety)
    nm <- tolower(names(df))
    names(df) <- nm
    
    # Expect columns: topic, articlecount, group, year
    if (!all(c("topic","articlecount","group","year") %in% names(df))) {
      stop("Expected columns (topic, articlecount, group, year) not found in file: ", path)
    }
    
    df <- df |>
      transmute(
        Topic       = topic,
        ArticleCount = articlecount,
        Group       = group,
        Year        = as.integer(year)
      )
    
    df
  }) |>
  bind_rows()

# Make sure factors/labels are nice
topic_counts$Group <- factor(
  topic_counts$Group,
  levels = c("disabled", "lgbt", "migrants", "religious"),
  labels = c("Disabled people", "LGBTQ+", "Migrants", "Religious minorities")
)

# All years present in the data (used for axis breaks)
all_years <- sort(unique(topic_counts$Year))

# ---------------------------------------------------------------------------
# 2. Single-group line plot (example: Religious minorities)
# ---------------------------------------------------------------------------

group_to_plot <- "Religious minorities"

single_group_df <- topic_counts |>
  filter(Group == group_to_plot)

if (nrow(single_group_df) == 0) {
  warning("No rows found for group: ", group_to_plot)
} else {
  p_single <- ggplot(single_group_df,
                     aes(x = Year, y = ArticleCount,
                         color = Topic, group = Topic)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.8) +
    scale_x_continuous(
      breaks = all_years,
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    scale_y_continuous(labels = label_number(big.mark = ",")) +
    labs(
      title = "Articles about religious minorities by topic",
      x = "Year",
      y = "Number of articles",
      color = "Topic"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "right",
      legend.box      = "vertical",
      plot.title      = element_text(face = "bold", hjust = 0.5),
      axis.text.x     = element_text(angle = 90, vjust = 0.5, hjust = 1)
    )
  
  # Save single-group figure
  ggsave(
    filename = file.path(figures_dir, "topics_religious_by_topic.pdf"),
    plot     = p_single,
    device   = cairo_pdf,
    width    = 8, height = 5, units = "in"
  )
  
  ggsave(
    filename = file.path(figures_dir, "topics_religious_by_topic.tiff"),
    plot     = p_single,
    dpi      = 600, compression = "lzw",
    width    = 8, height = 5, units = "in"
  )
}

# ---------------------------------------------------------------------------
# 3. Multi-group faceted plot (all four groups)
# ---------------------------------------------------------------------------

# Aggregate in case there are multiple rows per (Group, Year, Topic)
aggregated_data <- topic_counts |>
  group_by(Group, Year, Topic) |>
  summarise(
    TotalArticleCount = sum(ArticleCount, na.rm = TRUE),
    .groups = "drop"
  )

# Fill any missing Year–Topic–Group combinations with zero
aggregated_data <- aggregated_data |>
  complete(Group, Topic, Year = all_years,
           fill = list(TotalArticleCount = 0))

# Custom color palette for topics (optional; you can adjust)
custom_colors <- c(
  "crime and punishment"     = "#E64B35",
  "health and safety"        = "#4DBBD5",
  "policy"                   = "#00A087",
  "quality of life"          = "#F39B7F",
  "cultural"                 = "#EFC000",
  "legality"                 = "#7E6148",
  "politics"                 = "#3C5488",
  "sports"                   = "#8491B4",
  "fairness and equality"    = "#91D1C2",
  "morality"                 = "#B09C85",
  "public opinion"           = "#DC0000"
)

p_topics <- ggplot(aggregated_data,
                   aes(x = Year,
                       y = TotalArticleCount,
                       color = Topic,
                       group = Topic)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  facet_wrap(~ Group, ncol = 2) +
  scale_x_continuous(
    breaks = all_years,
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  scale_color_manual(values = custom_colors, na.value = "#666666") +
  labs(
    title = "Topic evolution in US news by group",
    x = "Year",
    y = "Number of articles",
    color = "Topic"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.box      = "horizontal",
    strip.text      = element_text(face = "bold"),
    plot.title      = element_text(face = "bold"),
    axis.text.x     = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

# Show in R (optional)
print(p_topics)

# Save multi-group figure
ggsave(
  filename = file.path(figures_dir, "topics_evolution_all_groups.pdf"),
  plot     = p_topics,
  device   = cairo_pdf,
  width    = 8, height = 8, units = "in"
)

ggsave(
  filename = file.path(figures_dir, "topics_evolution_all_groups.tiff"),
  plot     = p_topics,
  dpi      = 600, compression = "lzw",
  width    = 8, height = 8, units = "in"
)
