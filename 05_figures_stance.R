# 05_figures_stance.R
# ------------------------------------------------------------------
# Purpose:
#   Create stance-visualization figures using the stance predictions
#   produced by the Python script (03_stance_analysis.py).
#
# Expected input files (relative to project root):
#   output/stance/stance_predictions_*.csv
#
# Each CSV should contain (at minimum):
#   - text          : article text (or snippet)
#   - year          : publication year (integer or numeric)
#   - target        : stance target string (e.g., "disabled people")
#   - stance_label  : "supporting", "opposing", or "neutral"
#   - stance_score  : model confidence for the chosen label
#
# Output:
#   - output/figures/stance_proportions_all_groups.pdf
#   - output/figures/stance_proportions_all_groups.tiff
# ------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(stringr)
library(scales)
library(tools)

# --------------------------- Paths & setup -----------------------------------

stance_dir  <- file.path("output", "stance")
figures_dir <- file.path("output", "figures")

if (!dir.exists(stance_dir)) {
  stop("Directory not found: ", stance_dir,
       "\nPlease run the Python stance analysis (03_stance_analysis.py) first.")
}

if (!dir.exists(figures_dir)) {
  dir.create(figures_dir, recursive = TRUE)
}

# --------------------------- 1. Load stance prediction files -----------------

# We expect files named like:
#   stance_predictions_disabled_people.csv
#   stance_predictions_migrants.csv
#   stance_predictions_lgbtq_plus_people.csv
#   stance_predictions_religious_people.csv
stance_files <- list.files(
  stance_dir,
  pattern    = "^stance_predictions_.*\\.csv$",
  full.names = TRUE
)

if (length(stance_files) == 0) {
  stop("No 'stance_predictions_*.csv' files found in: ", stance_dir)
}

read_one <- function(path) {
  df <- read_csv(path, show_col_types = FALSE)
  
  # Add dataset label based on filename
  name <- file_path_sans_ext(basename(path))           # e.g. "stance_predictions_disabled_people"
  name <- sub("^stance_predictions_", "", name)        # e.g. "disabled_people"
  df$dataset_raw <- name
  
  df
}

raw <- bind_rows(lapply(stance_files, read_one))

# --------------------------- 2. Basic checks & normalization -----------------

# We expect at least these columns:
needed_cols <- c("stance_label", "year")
missing_cols <- setdiff(needed_cols, names(raw))

if (length(missing_cols) > 0) {
  stop("Missing required columns in stance data: ",
       paste(missing_cols, collapse = ", "))
}

# Standardize stance_label
raw <- raw %>%
  mutate(
    stance_label = tolower(stance_label),
    stance_label = recode(stance_label,
                          "favor"  = "supporting",
                          "against" = "opposing",
                          .default = stance_label),
    stance_label = factor(
      stance_label,
      levels = c("opposing", "neutral", "supporting")
    )
  )

# Ensure year is integer
if (!is.integer(raw$year)) {
  raw$year <- as.integer(raw$year)
}

# Map dataset_raw to nice labels for facets
raw <- raw %>%
  mutate(
    dataset = case_when(
      str_detect(dataset_raw, "disabled")            ~ "Disabled people",
      str_detect(dataset_raw, "migrant")             ~ "Migrants",
      str_detect(dataset_raw, "lgbt") |
        str_detect(dataset_raw, "lgbtq")             ~ "LGBTQ+",
      str_detect(dataset_raw, "religious")           ~ "Religious minorities",
      TRUE                                           ~ dataset_raw
    )
  )

# Keep only rows with non-missing year and stance_label
raw <- raw %>%
  filter(!is.na(year), !is.na(stance_label))

# --------------------------- 3. Plot: proportional stance per year ----------

# This is your core figure:
# stacked bar per year, faceted by group (dataset)
p1_data <- raw %>%
  count(dataset, year, stance_label) %>%
  group_by(dataset, year) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

# All years for consistent x-axis
all_years <- sort(unique(p1_data$year))

p1 <- ggplot(
  p1_data,
  aes(x = factor(year), y = prop, fill = stance_label)
) +
  geom_col() +
  facet_wrap(~ dataset, ncol = 2) +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(
    values = c(
      "opposing"   = "#F03C00",   # red
      "neutral"    = "#999999",   # gray
      "supporting" = "#0072B2"    # blue
    ),
    drop = FALSE
  ) +
  labs(
    title = "Yearly stance distribution across minoritized groups",
    x     = "Year",
    y     = "Proportion of articles",
    fill  = "Stance"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.box      = "horizontal",
    strip.text      = element_text(face = "bold"),
    plot.title      = element_text(face = "bold", size = 13),
    axis.text.x     = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

print(p1)

# --------------------------- 4. Save figure ----------------------------------

ggsave(
  filename = file.path(figures_dir, "stance_proportions_all_groups.pdf"),
  plot     = p1,
  device   = cairo_pdf,
  width    = 8, height = 8, units = "in"
)

ggsave(
  filename = file.path(figures_dir, "stance_proportions_all_groups.tiff"),
  plot     = p1,
  dpi      = 600, width = 8, height = 8, units = "in"
)

# --------------------------- 5. Optional: save CSV summary -------------------

summary_counts <- p1_data %>%
  arrange(dataset, year, stance_label)

write_csv(
  summary_counts,
  file.path("output", "stance", "stance_yearly_counts_and_props_all_groups.csv")
)
