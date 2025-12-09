# 05_figures_saliencefocus.R
# ------------------------------------------------------------------
# Purpose:
#   Plot the salience/focus time series for four groups using data
#   stored in an Excel file (one column per group, one row per year).
#
# Expected input (relative to project root):
#   data/graphs/saliencefocus.xlsx
#
# The Excel file should have columns like:
#   Year | Migrants | LGBTQ+ | Disabled People | Religious Minorities
#
# Outputs (relative to project root):
#   output/figures/saliencefocus_time_series.pdf
#   output/figures/saliencefocus_time_series.tiff
# ------------------------------------------------------------------

library(readxl)
library(tidyverse)

# --------------------------- Paths & setup -----------------------------------

# Location of the Excel data file (place your .xlsx here in the repo)
data_path    <- file.path("data", "graphs", "saliencefocus.xlsx")

# Location to save figures
figures_dir  <- file.path("output", "figures")

if (!file.exists(data_path)) {
  stop("Data file not found: ", data_path,
       "\nPlease place 'saliencefocus.xlsx' in the 'data/graphs' folder.")
}

if (!dir.exists(figures_dir)) {
  dir.create(figures_dir, recursive = TRUE)
}

# --------------------------- 1. Read the data --------------------------------

# If your data are on the first sheet and have a header row:
df <- read_excel(data_path)

# Ensure Year is numeric (if it's not already)
if (!is.numeric(df$Year)) {
  df <- df %>% mutate(Year = as.numeric(Year))
}

# --------------------------- 2. Reshape to long format -----------------------

# We assume all columns except 'Year' are group series
df_long <- df %>%
  pivot_longer(
    cols      = -Year,
    names_to  = "group",
    values_to = "value"
  )

# Order the groups in the legend
df_long$group <- factor(
  df_long$group,
  levels = c("Migrants", "LGBTQ+", "Disabled People", "Religious Minorities")
)

# --------------------------- 3. Define colors --------------------------------

cols <- c(
  "Migrants"             = "#fe4c10",  # bright orange-red
  "LGBTQ+"               = "#473b54",  # dark purple
  "Disabled People"      = "#8195ab",  # blue-grey
  "Religious Minorities" = "#fdcf35"   # yellow
)

# --------------------------- 4. Plot -----------------------------------------

p <- ggplot(df_long, aes(x = Year, y = value, color = group)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(values = cols) +
  scale_x_continuous(
    breaks = seq(min(df$Year, na.rm = TRUE),
                 max(df$Year, na.rm = TRUE),
                 by = 2)
  ) +
  labs(
    x     = "",
    y     = "",
    color = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.background  = element_rect(fill = "#f5f5f5", color = NA),
    plot.background   = element_rect(fill = "#f5f5f5", color = NA),
    panel.grid.major  = element_line(color = "#e6e6e6", linewidth = 0.3),
    panel.grid.minor  = element_blank(),
    axis.line.x       = element_blank(),
    axis.line.y       = element_blank(),
    legend.position   = c(0.92, 0.35),
    legend.justification = c("right", "center"),
    legend.key        = element_blank()
  )

print(p)

# --------------------------- 5. Save figure ----------------------------------

ggsave(
  filename = file.path(figures_dir, "saliencefocus_time_series.pdf"),
  plot     = p,
  device   = cairo_pdf,
  width    = 7, height = 4.5, units = "in"
)

ggsave(
  filename = file.path(figures_dir, "saliencefocus_time_series.tiff"),
  plot     = p,
  dpi      = 600, compression = "lzw",
  width    = 7, height = 4.5, units = "in"
)
