# 00_collect_factiva.R
# Purpose:
#   Filter a Factiva export (CSV) for four minoritized groups (migrants, LGBTQ+,
#   religious minorities, disabled people) between 2002 and 2021.
#
# Data:
#   - The Factiva data are NOT included in the replication package (license).
#   - To run this script, the user must:
#       1) have Factiva access,
#       2) export the relevant articles to CSV,
#       3) save the file as: data/raw/factiva_export.csv
#
# Output:
#   data/processed/factiva_<group>_2002_2021.rds
#   where <group> is one of: migrants, lgbtq, religious, disabled

# -----------------------------
# 1. Setup
# -----------------------------
library(dplyr)
library(stringr)

# Path (relative to project root) where the Factiva CSV export should be placed
factiva_raw_path <- "data/raw/factiva_export.csv"

if (!file.exists(factiva_raw_path)) {
  stop(
    "Factiva CSV export not found.\n",
    "Expected file: ", factiva_raw_path, "\n\n",
    "Please export your Factiva data to CSV and save it at this path.\n",
    "See data/raw/README_factiva.txt for instructions."
  )
}

# -----------------------------
# 2. Load the raw Factiva data
# -----------------------------

# NOTE:
# - 'stringsAsFactors = FALSE' keeps text columns as character.
# - Adjust 'sep' or 'encoding' if your CSV differs.
factiva_raw <- read.csv(
  factiva_raw_path,
  stringsAsFactors = FALSE
)

# EXPECTED COLUMNS (adjust names here if your export differs):
# - publication_date_mdy
# - source_code
# - title
# - snippet
# - body

required_cols <- c("publication_date_mdy", "source_code", "title", "snippet", "body")
missing_cols <- setdiff(required_cols, names(factiva_raw))

if (length(missing_cols) > 0) {
  stop(
    "The following required columns are missing from the Factiva CSV:\n",
    paste(missing_cols, collapse = ", "), "\n\n",
    "Please adjust 00_collect_factiva.R or your Factiva export accordingly."
  )
}

# -----------------------------
# 3. Filter by year and source
# -----------------------------

# Adjust date format here if needed.
# Example assumes publication_date_mdy is in YYYY-MM-DD or similar.
factiva_raw <- factiva_raw %>%
  mutate(
    # Try to parse as Date; adjust format string if necessary:
    # as.Date(publication_date_mdy, format = "%m/%d/%Y") etc.
    pub_date = as.Date(publication_date_mdy),
    pub_year = as.integer(format(pub_date, "%Y"))
  )

years <- 2002:2021

factiva_filtered <- factiva_raw %>%
  filter(!is.na(pub_year), pub_year %in% years)

# Source filter:
#   - "WP" for Washington Post only (like your original code)
#   - remove this filter if you used all sources
source_pattern <- "WP"

factiva_filtered <- factiva_filtered %>%
  filter(str_detect(source_code, fixed(source_pattern)))

# -----------------------------
# 4. Keyword sets
# -----------------------------

keyword_sets <- list(
  migrants = c(
    "migrant*", "asylum", "refugee*", "migration",
    "border crossing", "freedom of movement", "free movement"
  ),
  
  lgbtq = c(
    "agender", "ambiguous sexuality", "androgynous", "aromantic", "asexual",
    "bisexual", "bigender", "born a man", "born a woman",
    "cross dresser", "drag king", "drag queen", "dyk", "gay",
    "genderqueer", "gender queer", "genderless", "gender variant",
    "gender neutral", "genderneutral", "gender fluid", "genderfluid",
    "gender identity disorder", "gender minorit*", "gender minority",
    "third gender", "hermaphrodit*", "homosexual", "intersex",
    "lesbi*", "LGBT", "nonbinary", "non binary", "non-binary",
    "same-sex", "same sex", "sexual minorit*", "sexual identity",
    "transsexual*", "transexual*", "trans*", "trans man", "transgender*",
    "trans woman", "transman", "transwoman", "transwomen", "transmen",
    "transvestite*", "transvestit*", "tranni*", "two-spirit",
    "pansexual*", "queer", "QPOC", "QTPO"
  ),
  
  religious = c(
    "muslim*", "moslem", "islam*", "hindu*", "jew*", "protestant*",
    "religio*", "orthodox*", "sunni*", "shiite*", "fundamentalist*",
    "judaism*", "secular*", "mosque*", "buddhis*", "shia*",
    "evangelical*", "anglican*"
  ),
  
  disabled = c(
    "disabilit*", "disabled", "impair*", "handicapp*", "wheelchair*",
    "impairment*", "palsy*", "neurodiverse*", "neurodivergent*",
    "autistic*", "deaf*", "accessibilit*", "elderly", "blind*",
    "autism*", "chronic illness*", "chronic disease*",
    "chronically ill*", "chronic pain*", "dyslexi*", "deafness*",
    "paraplegic*", "blindness*", "epilep*", "quadriplegic*"
  )
)

# Helper: turn keyword vector into a regex.
# - Escapes regex meta characters except the * which we treat as a wildcard.
# - Replaces "*" with ".*" so "migrant*" matches "migrants", "migration" etc.
make_regex <- function(terms) {
  terms <- stringr::str_trim(terms)
  
  escaped <- stringr::str_replace_all(
    terms,
    "([\\.\\+\\?\\^\\$\\(\\)\\[\\]\\{\\}\\|\\\\])",
    "\\\\\\1"
  )
  
  escaped <- stringr::str_replace_all(escaped, "\\*", ".*")
  
  stringr::str_c(escaped, collapse = "|")
}

# Ensure output directory exists
if (!dir.exists("data/processed")) {
  dir.create("data/processed", recursive = TRUE)
}

# -----------------------------
# 5. Loop over groups, filter, and save
# -----------------------------

for (group_name in names(keyword_sets)) {
  message("Processing group: ", group_name)
  
  pattern <- make_regex(keyword_sets[[group_name]])
  
  # Filter by keywords in body, title, OR snippet (case-insensitive)
  filtered_group <- factiva_filtered %>%
    filter(
      str_detect(body,    regex(pattern, ignore_case = TRUE)) |
        str_detect(title,   regex(pattern, ignore_case = TRUE)) |
        str_detect(snippet, regex(pattern, ignore_case = TRUE))
    ) %>%
    # Remove duplicates by article body
    distinct(body, .keep_all = TRUE)
  
  output_path <- paste0(
    "data/processed/factiva_",
    group_name,
    "_",
    min(years), "_", max(years),
    ".rds"
  )
  
  saveRDS(filtered_group, file = output_path)
  
  message("  Saved ", nrow(filtered_group), " rows to: ", output_path)
}
