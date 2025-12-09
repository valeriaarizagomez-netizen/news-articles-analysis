# 05_figures_proportionalsalience.R
# ------------------------------------------------------------------
# Purpose:
#   Plot the proportional salience of each group (Migrants, LGBTQ+,
#   Disabled People, Religious Minorities) as a percentage of the
#   total number of articles per year.
#
# Expected input (relative to project root):
#   data/graphs/proportionalsalience.xlsx
#
# The Excel file should have columns:
#   Year | Total Articles | Migrants | LGBTQ+ | Religious Minorities | Disabled People
#
# Outputs (relative to project root):
#   output/figures/proportionalsalience_stacked_bar.pdf
#   output/figures/proportionalsalience_stacked_bar.tiff
# ------------------------------------------------------------------

library(readxl)
library(tidyverse)

# --------------------------- Paths & setup -----------------------------------

data_path   <- file.path("data", "graphs", "proportionalsalience.xlsx")
figures_dir <- file.path("output", "figures")

if (!file.exists(data_path)) {
  stop("Data file not found: ", data_path,
       "\nPlease place 'proportionalsalience.xlsx' in the 'data/graphs' folder.")
}

if (!dir.exists(figures_dir)) {
  dir.create(figures_dir, recursive = TRUE)
}

# --------------------------- 1. Read the data --------------------------------

df <- read_excel(data_path)

# Ensure Year is numeric (for ordering)
if (!is.numeric(df$Year)) {
  df <- df %>% mutate(Year = as.numeric(Year))
}

# Sanity check for required columns
required_cols <- c("Year", "Total Articles", "Migrants", "LGBTQ+",
                   "Religious Minorities", "Disabled People")

missing_cols <- setdiff(required_cols, names(df))
if (length(missing_cols) > 0) {
  stop("Missing expected columns in Excel file: ",
       paste(missing_cols, collapse = ", "))
}

# --------------------------- 2. Convert counts to percentages ----------------

df_pct <- df %>%
  mutate(
    Migrants_pct            = Migrants / `Total Articles` * 100,
    LGBTQ_pct               = `LGBTQ+` / `Total Articles` * 100,
    ReligiousMinorities_pct = `Religious Minorities` / `Total Articles` * 100,
    DisabledPeople_pct      = `Disabled People` / `Total Articles` * 100
  ) %>%
  mutate(
    Total_pct = 100 - (Migrants_pct + LGBTQ_pct +
                         ReligiousMinorities_pct + DisabledPeople_pct)
  ) %>%
  select(
    Year,
    Total_pct,
    Migrants_pct,
    LGBTQ_pct,
    ReligiousMinorities_pct,
    DisabledPeople_pct
  )

# --------------------------- 3. Reshape to long format -----------------------

df_long <- df_pct %>%
  pivot_longer(
    cols      = -Year,
    names_to  = "group",
    values_to = "value"
  ) %>%
  mutate(
    group = recode(
      group,
      "Total_pct"               = "Total Articles",
      "Migrants_pct"            = "Migrants",
      "LGBTQ_pct"               = "LGBTQ+",
      "ReligiousMinorities_pct" = "Religious Minorities",
      "DisabledPeople_pct"      = "Disabled People"
    ),
    group = factor(
      group,
      levels = c(
        "Total Articles",
        "Migrants",
        "LGBTQ+",
        "Religious Minorities",
        "Disabled People"
      )
    )
  )

# --------------------------- 4. Colors ---------------------------------------

cols <- c(
  "Migrants"             = "#fe4c10",  # bright orange-red
  "LGBTQ+"               = "#fdcf35",  # yellow
  "Disabled People"      = "#8195ab",  # blue-grey
  "Religious Minorities" = "#473b54",  # dark purple
  "Total Articles"       = "#9c9c9c"   # grey
)

# --------------------------- 5. Plot -----------------------------------------

p <- ggplot(df_long, aes(x = factor(Year), y = value, fill = group)) +
  geom_col(width = 0.9) +
  scale_fill_manual(values = cols) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(0, 100, 10)
  ) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(
    x    = "",
    y    = "",
    fill = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.background   = element_rect(fill = "#f5f5f5", color = NA),
    plot.background    = element_rect(fill = "#f5f5f5", color = NA),
    panel.grid.major.y = element_line(linewidth = 0.3, colour = "#e0e0e0"),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x        = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position    = "top"
  )

print(p)

# --------------------------- 6. Save figure ----------------------------------

ggsave(
  filename = file.path(figures_dir, "proportionalsalience_stacked_bar.pdf"),
  plot     = p,
  device   = cairo_pdf,
  width    = 7, height = 4.5, units = "in"
)

ggsave(
  filename = file.path(figures_dir, "proportionalsalience_stacked_bar.tiff"),
  plot     = p,
  dpi      = 600, compression = "lzw",
  width    = 7, height = 4.5, units = "in"
)
