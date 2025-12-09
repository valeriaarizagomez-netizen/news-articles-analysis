# 05_figures_proportional_centering.R
# ------------------------------------------------------------------
# Purpose:
#   Create a horizontal 100% stacked bar plot showing, for each
#   minoritized group, the proportion of articles that merely
#   mention the group vs. explicitly center the group.
#
# Expected input (relative to project root):
#   data/graphs/proportional_centering.xlsx
#
# The Excel file must contain these columns:
#   "Minoritized Group"
#   "Total articles mentioning MG"
#   "Articles centering MG"
#
# Outputs (relative to project root):
#   output/figures/proportional_centering.pdf
#   output/figures/proportional_centering.tiff
# ------------------------------------------------------------------

library(readxl)
library(tidyverse)

# --------------------------- Paths & setup -----------------------------------

data_path   <- file.path("data", "graphs", "proportional_centering.xlsx")
figures_dir <- file.path("output", "figures")

if (!file.exists(data_path)) {
  stop("Data file not found: ", data_path,
       "\nPlease place 'proportional_centering.xlsx' in the 'data/graphs' folder.")
}
if (!dir.exists(figures_dir)) {
  dir.create(figures_dir, recursive = TRUE)
}

# --------------------------- 1. Load data ------------------------------------

df <- read_excel(data_path)

required_cols <- c(
  "Minoritized Group",
  "Total articles mentioning MG",
  "Articles centering MG"
)

missing_cols <- setdiff(required_cols, names(df))
if (length(missing_cols) > 0) {
  stop("Missing expected columns in Excel file: ",
       paste(missing_cols, collapse = ", "))
}

# --------------------------- 2. Compute percentages --------------------------

df_pct <- df %>%
  mutate(
    Total_pct = (`Total articles mentioning MG` - `Articles centering MG`) /
      `Total articles mentioning MG` * 100,
    Centering_pct = `Articles centering MG` /
      `Total articles mentioning MG` * 100
  ) %>%
  select(`Minoritized Group`, Total_pct, Centering_pct)

# --------------------------- 3. Long format ----------------------------------

df_long <- df_pct %>%
  pivot_longer(
    cols      = c(Total_pct, Centering_pct),
    names_to  = "type",
    values_to = "value"
  ) %>%
  mutate(
    type = recode(
      type,
      "Total_pct"     = "Total articles mentioning MG",
      "Centering_pct" = "Articles centering MG"
    ),
    type = factor(
      type,
      levels = c("Total articles mentioning MG", "Articles centering MG")
    )
  )

# Optional: enforce consistent order of groups in the figure
df_long$`Minoritized Group` <- factor(
  df_long$`Minoritized Group`,
  levels = c("Migrants", "LGBTQ+", "Religious Minorities", "Disabled People")
)

# --------------------------- 4. Colors ---------------------------------------

cols <- c(
  "Total articles mentioning MG" = "#b5b5b5",  # grey
  "Articles centering MG"        = "#fe4c10"   # orange
)

# --------------------------- 5. Plot -----------------------------------------

p <- ggplot(df_long,
            aes(y = `Minoritized Group`, x = value, fill = type)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = cols) +
  scale_x_continuous(
    limits = c(0, 100),
    expand = c(0, 0),
    breaks = seq(0, 100, by = 10)
  ) +
  labs(x = "", y = "", fill = "") +
  theme_minimal(base_size = 13) +
  theme(
    panel.background   = element_rect(fill = "#f5f5f5", color = NA),
    plot.background    = element_rect(fill = "#f5f5f5", color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(linewidth = 0.3, colour = "#d3d3d3"),
    legend.position    = "top",
    axis.text.y        = element_text(size = 12)
  )

print(p)

# --------------------------- 6. Save outputs ---------------------------------

ggsave(
  filename = file.path(figures_dir, "proportional_centering.pdf"),
  plot     = p,
  device   = cairo_pdf,
  width    = 7, height = 4.5, units = "in"
)

ggsave(
  filename = file.path(figures_dir, "proportional_centering.tiff"),
  plot     = p,
  dpi      = 600, compression = "lzw",
  width    = 7, height = 4.5, units = "in"
)
