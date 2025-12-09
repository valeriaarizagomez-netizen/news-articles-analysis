# 05_figures_sentiment_timeseries.R
# ------------------------------------------------------------------
# Purpose:
#   Plot the average yearly sentiment scores for four minoritized groups
#   using a cleaned Excel file containing one column per group.
#
# Expected input (relative to project root):
#   data/graphs/sentiment.xlsx
#
# Required columns:
#   "Year", "Migants", "Religious minorities", "LGBTQ+", "Disabled people"
#   (Note: "Migants" is corrected to "Migrants" in the script.)
#
# Outputs:
#   output/figures/sentiment_timeseries.pdf
#   output/figures/sentiment_timeseries.tiff
# ------------------------------------------------------------------

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# --------------------------- Paths & setup -----------------------------------

data_path   <- file.path("data", "graphs", "sentiment.xlsx")
figures_dir <- file.path("output", "figures")

if (!file.exists(data_path)) {
  stop("Data file not found: ", data_path,
       "\nPlace 'sentiment.xlsx' in the folder: data/graphs/")
}
if (!dir.exists(figures_dir)) {
  dir.create(figures_dir, recursive = TRUE)
}

# --------------------------- 1. Load data ------------------------------------

df_raw <- read_excel(data_path)

sent_cols <- c("Migants", "Religious minorities", "LGBTQ+", "Disabled people")

# --------------------------- 2. Clean numeric columns -------------------------

df <- df_raw %>%
  filter(!is.na(Year), Year != "-") %>%
  mutate(
    Year = as.numeric(Year),
    across(
      all_of(sent_cols),
      ~ as.numeric(gsub(",", ".", ifelse(as.character(.) == "-", NA_character_, as.character(.))))
    )
  )

# --------------------------- 3. Long format ----------------------------------

df_long <- df %>%
  pivot_longer(
    cols      = all_of(sent_cols),
    names_to  = "group_raw",
    values_to = "value"
  ) %>%
  mutate(
    group = case_when(
      group_raw == "Migants"              ~ "Migrants",
      group_raw == "Religious minorities" ~ "Religious Minorities",
      group_raw == "LGBTQ+"               ~ "LGBTQ+",
      group_raw == "Disabled people"      ~ "Disabled People",
      TRUE ~ group_raw
    ),
    group = factor(group,
                   levels = c("Migrants", "LGBTQ+", "Disabled People", "Religious Minorities"))
  )

# --------------------------- 4. Colors ---------------------------------------

cols <- c(
  "Migrants"             = "#fe4c10",
  "LGBTQ+"               = "#fdcf35",
  "Disabled People"      = "#8195ab",
  "Religious Minorities" = "#473b54"
)

# --------------------------- 5. Plot -----------------------------------------

p <- ggplot(df_long, aes(x = Year, y = value, color = group)) +
  geom_line(size = 1.3, na.rm = TRUE) +
  geom_point(size = 2.4, na.rm = TRUE) +
  scale_color_manual(values = cols) +
  scale_x_continuous(breaks = seq(min(df$Year), max(df$Year), by = 2)) +
  scale_y_continuous(limits = c(-0.4, 0.4)) +   # <<—— ADDED HERE
  labs(x = "", y = "", color = "") +
  theme_minimal(base_size = 13) +
  theme(
    panel.background   = element_rect(fill = "#f5f5f5", color = NA),
    plot.background    = element_rect(fill = "#f5f5f5", color = NA),
    panel.grid.major   = element_line(color = "#e0e0e0", linewidth = 0.3),
    panel.grid.minor   = element_blank(),
    axis.line.x        = element_blank(),
    axis.line.y        = element_blank(),
    legend.position    = "right",
    legend.title       = element_blank()
  )

print(p)

# --------------------------- 6. Save outputs ---------------------------------

ggsave(
  filename = file.path(figures_dir, "sentiment_timeseries.pdf"),
  plot     = p,
  device   = cairo_pdf,
  width    = 7, height = 4.5, units = "in"
)

ggsave(
  filename = file.path(figures_dir, "sentiment_timeseries.tiff"),
  plot     = p,
  dpi      = 600, compression = "lzw",
  width    = 7, height = 4.5, units = "in"
)
