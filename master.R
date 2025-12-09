# master.R
# ------------------------------------------------------------------------------
# Purpose:
#   Run topic analysis for all four minoritized groups (LGBT, Migrants,
#   Religious, Disabled) and all years 2002–2021, using the function defined in
#   code/01_topic_analysis.R.
#
# Usage:
#   In R (with working directory set to the project root):
#     source("master.R")
#
# Outputs:
#   For each group and year, the following files are created in output/topics/:
#     - topic_terms_<group>_<year>.csv
#     - articles_per_topic_<group>_<year>.csv
#     - doc_topics_<group>_<year>.csv
# ------------------------------------------------------------------------------

# Load the topic analysis function
source(file.path("code", "01_topic_analysis.R"))

# Define groups and corresponding folders in data/processed/
groups <- data.frame(
  label  = c("lgbt",      "migrants",  "religious", "disabled"),
  folder = c("LGBT",      "Migrants",  "Religious", "Disabled"),
  stringsAsFactors = FALSE
)

# Years to analyze
years <- 2002:2021

# Threshold for high-confidence topic assignment (max theta)
theta_threshold <- 0.5

# Loop over years and groups
for (y in years) {
  message("===== YEAR: ", y, " =====")
  for (i in seq_len(nrow(groups))) {
    
    run_topic_analysis(
      group_label   = groups$label[i],
      group_folder  = groups$folder[i],
      year          = y,
      threshold     = theta_threshold
    )
    
  }
}
