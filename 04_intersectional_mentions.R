# 04_intersectional_mentions.R
# --------------------------------------------------------------------
# Purpose:
#   Identify intersectional mentions in news articles by searching for
#   a set of keywords (e.g., religious-minority terms) within articles
#   belonging to a base group (e.g., disabled people).
#
# Inputs (relative to project root):
#   - data/processed/<GROUP_FOLDER>/*.RDS
#       Each .RDS must contain:
#         - body  (article text)
#
# Outputs:
#   - output/intersection/keyword_analysis_<group_label>_<keyword_set>.csv
#       -> total mentions and document counts per keyword
#   - output/intersection/kwic_<group_label>_<keyword_set>.csv
#       -> keywords-in-context examples
# --------------------------------------------------------------------

library(quanteda)
library(dplyr)
library(tibble)

# -------------------------- USER SETTINGS ------------------------------------

# Base group (where we are searching for intersectional mentions)
# Examples:
#   group_label  = "disabled"
#   group_folder = "Disabled"
group_label  <- "disabled"
group_folder <- "Disabled"

# Name of the keyword set (used in output filenames)
keyword_set_name <- "religion"

# Keyword list (example: religious-minority terms)
keywords <- c(
  "muslim", "moslem", "islam", "hindu", "jew", "protestant",
  "religio", "orthodox", "sunni", "shiite", "fundamentalist",
  "judaism", "secular", "mosque", "buddhis", "shia",
  "evangelical", "anglican"
)

# -------------------------- PATH SETUP ---------------------------------------

input_folder <- file.path("data", "processed", group_folder)
output_dir   <- file.path("output", "intersection")

if (!dir.exists(input_folder)) {
  stop("Input folder not found: ", input_folder)
}

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# -------------------------- LOAD AND COMBINE DATA ----------------------------

rds_files <- list.files(input_folder, pattern = "\\.RDS$", full.names = TRUE)

if (length(rds_files) == 0) {
  stop("No .RDS files found in: ", input_folder)
}

message("Found ", length(rds_files), " RDS files in ", input_folder)

combined_data <- rds_files |>
  lapply(readRDS) |>
  bind_rows()

if (!"body" %in% names(combined_data)) {
  stop("Column 'body' not found in combined data. It is required for text analysis.")
}

message("Total articles loaded: ", nrow(combined_data))

# -------------------------- CORPUS AND TOKENS --------------------------------

corp <- corpus(combined_data, text_field = "body")

toks <- tokens(
  corp,
  what             = "word",
  remove_punct     = TRUE,
  remove_numbers   = TRUE,
  remove_symbols   = TRUE,
  remove_separators = TRUE
)

toks <- tokens_remove(
  toks,
  pattern   = c(stopwords("en"), "*-time", "updated-*", "gmt", "bst"),
  valuetype = "glob"
)

toks <- tokens_tolower(toks)

# -------------------------- KEYWORD FREQUENCIES ------------------------------

# Create dfm and select only the keyword features
dfm_keywords <- dfm(toks) |>
  dfm_select(
    pattern         = keywords,
    valuetype       = "fixed",
    case_insensitive = TRUE
  )

# Total frequency of each keyword
keyword_frequencies <- textstat_frequency(dfm_keywords)

# Document frequency: in how many articles each keyword appears
doc_frequencies <- colSums(dfm_keywords > 0)

doc_frequencies_df <- tibble(
  feature = names(doc_frequencies),
  docfreq = as.numeric(doc_frequencies)
) |>
  mutate(feature = tolower(feature))

# Combine total and document frequencies
keyword_analysis <- keyword_frequencies |>
  rename(total_mentions = frequency) |>
  select(feature, total_mentions) |>
  mutate(feature = tolower(feature)) |>
  left_join(doc_frequencies_df, by = "feature") |>
  arrange(desc(total_mentions))

# Save keyword analysis
keyword_out_path <- file.path(
  output_dir,
  paste0("keyword_analysis_", group_label, "_", keyword_set_name, ".csv")
)

write.csv(keyword_analysis, file = keyword_out_path, row.names = FALSE)

message("Keyword analysis saved to: ", keyword_out_path)

# -------------------------- KEYWORDS IN CONTEXT (KWIC) -----------------------

all_kwic_results <- list()

for (kw in keywords) {
  current_kwic <- kwic(
    toks,
    pattern          = kw,
    window           = 7,   # 7 words before and after
    n                = 10,  # up to 10 matches per keyword
    valuetype        = "fixed",
    case_insensitive = TRUE
  )
  
  if (nrow(current_kwic) > 0) {
    all_kwic_results[[kw]] <- current_kwic
  } else {
    message("No matches found for keyword: ", kw)
  }
}

if (length(all_kwic_results) > 0) {
  combined_kwic_df <- bind_rows(all_kwic_results, .id = "keyword_searched") |>
    as_tibble() |>
    select(keyword_searched, docname, pre, keyword, post)
  
  kwic_out_path <- file.path(
    output_dir,
    paste0("kwic_", group_label, "_", keyword_set_name, ".csv")
  )
  
  write.csv(combined_kwic_df, file = kwic_out_path, row.names = FALSE)
  
  message("KWIC results saved to: ", kwic_out_path)
} else {
  message("No KWIC results to save.")
}
