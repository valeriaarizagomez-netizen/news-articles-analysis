# 05_figures_intersectional_mentions.R
# ------------------------------------------------------------------
# Purpose:
#   Visualize intersectional mentions across minoritized groups using
#   a grouped (clustered) horizontal bar chart.
#
# Expected input (relative to project root):
#   data/graphs/intersection.xlsx
#
# Excel file structure:
#   - First column: row group labels (e.g., "LGBTQ+ communities",
#     "Religious Minorities", "Migrants", "Disabled people").
#   - Remaining columns: counts or frequencies of mentions of each
#     column group within the row group.
#   Example columns:
#     [1] "RowGroup" (unnamed in file, will be renamed)
#     [2] "Disabled people"
#     [3] "Migrants"
#     [4] "Religious Minorities"
#     [5] "LGBTQ+ communities"
#
# The diagonal (RowGroup == ColGroup) is dropped so that only
# cross-group intersectional mentions are shown.
#
# Outputs (relative to project root):
#   output/figures/intersectional_mentions_bars.pdf
#   output/figures/intersectional_mentions_bars.tiff
# ------------------------------------------------------------------

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# --------------------------- Paths & setup -----------------------------------

data_path   <- file.path("data", "graphs", "intersection.xlsx")
figures_dir <- file.path("output", "figures")

if (!file.exists(data_path)) {
  stop("Data file not found: ", data_path,
       "\nPlease place 'intersection.xlsx' in the 'data/graphs' folder.")
}
if (!dir.exists(figures_dir)) {
  dir.create(figures_dir, recursive = TRUE)
}

# --------------------------- 1. Load data ------------------------------------

df_raw <- read_excel(data_path)

# Rename first column to RowGroup if it lacks a meaningful name
names(df_raw)[1] <- "RowGroup"

# --------------------------- 2. Long format + remove diagonal -----------------

df_long <- df_raw %>%
  pivot_longer(
    cols      = -RowGroup,
    names_to  = "ColGroup",
    values_to = "value"
  ) %>%
  filter(!is.na(value)) %>%       # drop blanks
  filter(RowGroup != ColGroup)    # drop diagonal (self-mentions)

# --------------------------- 3. Factor order (optional) ----------------------

df_long$RowGroup <- factor(
  df_long$RowGroup,
  levels = c(
    "LGBTQ+ communities",
    "Religious Minorities",
    "Migrants",
    "Disabled people"
  )
)

df_long$ColGroup <- factor(
  df_long$ColGroup,
  levels = c(
    "Disabled people",
    "Migrants",
    "Religious Minorities",
    "LGBTQ+ communities"
  )
)

# --------------------------- 4. Colors ---------------------------------------

cols <- c(
  "Disabled people"      = "#8195ab",
  "Migrants"             = "#fe4c10",
  "Religious Minorities" = "#473b54",
  "LGBTQ+ communities"   = "#fdcf35"
)

# --------------------------- 5. Plot -----------------------------------------

p <- ggplot(
  df_long,
  aes(x = RowGroup, y = value, fill = ColGroup)
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width    = 0.7
  ) +
  scale_fill_manual(values = cols) +
  labs(
    title = "Intersectional mentions across minoritized groups",
    x     = "",
    y     = "",
    fill  = ""
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.background   = element_rect(fill = "#f5f5f5", color = NA),
    plot.background    = element_rect(fill = "#f5f5f5", color = NA),
    panel.grid.major.x = element_line(linewidth = 0.3, colour = "#d3d3d3"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position    = "top"
  ) +
  coord_flip()  # horizontal bars

print(p)

# --------------------------- 6. Save figure ----------------------------------

ggsave(
  filename = file.path(figures_dir, "intersectional_mentions_bars.pdf"),
  plot     = p,
  device   = cairo_pdf,
  width    = 7, height = 4.5, units = "in"
)

ggsave(
  filename = file.path(figures_dir, "intersectional_mentions_bars.tiff"),
  plot     = p,
  dpi      = 600, compression = "lzw",
  width    = 7, height = 4.5, units = "in"
)
