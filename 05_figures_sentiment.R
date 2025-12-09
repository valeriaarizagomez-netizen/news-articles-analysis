# 05_figures_sentiment.R
# ------------------------------------------------------------------
# Purpose:
#   Visualize average sentiment over time for all four groups using
#   aggregated yearly sentiment scores.
#
# Expected input files (relative to project root):
#   output/sentiment/average_sentiment_DISAB.csv
#   output/sentiment/average_sentiment_LGBT.csv
#   output/sentiment/average_sentiment_MIGRANTS.csv
#   output/sentiment/average_sentiment_RELIG.csv
#
# Each CSV should contain:
#   - Year             : year (numeric or character)
#   - AverageSentiment : numeric sentiment score
#   - group            : short group code (e.g., "DISAB", "LGBT")
#
# Output:
#   - output/figures/sentiment_over_time_all_groups.pdf
#   - output/figures/sentiment_over_time_all_groups.tiff
# ------------------------------------------------------------------

library(readr)
library(dplyr)
library(ggplot2)
library(scales)

# --------------------------- Paths & setup -----------------------------------

sentiment_dir <- file.path("output", "sentiment")
figures_dir   <- file.path("output", "figures")

if (!dir.exists(sentiment_dir)) {
  stop("Directory not found: ", sentiment_dir,
       "\nPlease run the sentiment analysis and aggregation first.")
}

if (!dir.exists(figures_dir)) {
  dir.create(figures_dir, recursive = TRUE)
}

# --------------------------- 1. Files to read --------------------------------

file_names_to_read <- c(
  "average_sentiment_DISAB.csv",
  "average_sentiment_LGBT.csv",
  "average_sentiment_MIGRANTS.csv",
  "average_sentiment_RELIG.csv"
)

all_groups_sentiment_list <- list()

for (file_name in file_names_to_read) {
  file_path <- file.path(sentiment_dir, file_name)
  
  if (file.exists(file_path)) {
    message("Reading file: ", file_path)
    df <- read_csv(
      file_path,
      col_types = cols(
        Year            = col_character(),
        AverageSentiment = col_double(),
        group           = col_character()
      )
    )
    all_groups_sentiment_list[[file_name]] <- df
  } else {
    warning("File not found: ", file_path)
  }
}

if (length(all_groups_sentiment_list) == 0) {
  stop("No aggregated sentiment CSVs could be read from: ", sentiment_dir)
}

# --------------------------- 2. Combine and clean ----------------------------

combined_sentiment_data <- bind_rows(all_groups_sentiment_list)

# Convert Year to numeric for plotting
combined_sentiment_data <- combined_sentiment_data %>%
  mutate(Year = as.numeric(Year))

# Map short group codes to nice labels for the legend
combined_sentiment_data <- combined_sentiment_data %>%
  mutate(
    GroupLabel = dplyr::case_when(
      group %in% c("DISAB", "disab", "disabled")      ~ "Disabled people",
      group %in% c("LGBT", "lgbt")                    ~ "LGBTQ+",
      group %in% c("MIGRANTS", "migrants", "MIGR")    ~ "Migrants",
      group %in% c("RELIG", "relig", "religious")     ~ "Religious minorities",
      TRUE                                            ~ group
    )
  )

# Check structure (optional)
print("Head of combined sentiment data:")
print(head(combined_sentiment_data))

# All years (for x-axis breaks)
all_years <- sort(unique(combined_sentiment_data$Year))

# --------------------------- 3. Plot sentiment over time ---------------------

p_sentiment <- ggplot(
  combined_sentiment_data,
  aes(x = Year, y = AverageSentiment, color = GroupLabel, group = GroupLabel)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = all_years,
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    labels = number_format(accuracy = 0.01),
    limits = c(-1, 1)  # optional, since your sentiment_score is in [-1, 1]
  ) +
  labs(
    title  = "Average sentiment score over time by group",
    x      = "Year",
    y      = "Average sentiment score",
    color  = "Group"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title   = element_text(hjust = 0.5, face = "bold"),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

print(p_sentiment)

# --------------------------- 4. Save figure ----------------------------------

ggsave(
  filename = file.path(figures_dir, "sentiment_over_time_all_groups.pdf"),
  plot     = p_sentiment,
  device   = cairo_pdf,
  width    = 8, height = 5, units = "in"
)

ggsave(
  filename = file.path(figures_dir, "sentiment_over_time_all_groups.tiff"),
  plot     = p_sentiment,
  dpi      = 600, compression = "lzw",
  width    = 8, height = 5, units = "in"
)
