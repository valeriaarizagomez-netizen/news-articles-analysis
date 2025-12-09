# 01_topic_analysis.R
# ------------------------------------------------------------------------------
# Purpose:
#   Run seeded LDA topic models for one minoritized group and one year, using
#   pre-processed article-level .RDS files, and save:
#     - top terms per topic
#     - high-confidence article-level topic assignments (theta > threshold)
#     - article counts per human-readable topic label
#
# Expected project structure (relative to project root):
#   code/01_topic_analysis.R           <- this file
#   data/processed/LGBT/*.RDS          <- per-article data for LGBT group
#   data/processed/Migrants/*.RDS      <- per-article data for Migrants group
#   data/processed/Religious/*.RDS     <- per-article data for Religious group
#   data/processed/Disabled/*.RDS      <- per-article data for Disabled group
#   dictionary/topics.yml              <- seeded LDA topic dictionary
#   output/topics/                     <- will be created if it does not exist
#
# Required columns in each .RDS object:
#   - body                   : article text
#   - publication_date_mdy   : date string that can be parsed by as.Date()
#   - optionally pub_year    : if not present, it will be created
#
# Outputs (for each group + year):
#   - output/topics/topic_terms_<group>_<year>.csv
#   - output/topics/articles_per_topic_<group>_<year>.csv
#   - output/topics/doc_topics_<group>_<year>.csv
# ------------------------------------------------------------------------------

# --------------------------- Load required packages ---------------------------

library(quanteda)
library(seededlda)
library(dplyr)
library(stringr)

# ----------------------------- Helper functions -------------------------------

# Map numeric topic indices (1, 2, ...) to human-readable labels.
label_topics <- function(topic_index_vector) {
  topic_char <- as.character(topic_index_vector)
  dplyr::case_when(
    topic_char == "1"  ~ "morality",
    topic_char == "2"  ~ "fairness and equality",
    topic_char == "3"  ~ "legality",
    topic_char == "4"  ~ "policy",
    topic_char == "5"  ~ "crime and punishment",
    topic_char == "6"  ~ "health and safety",
    topic_char == "7"  ~ "quality of life",
    topic_char == "8"  ~ "cultural",
    topic_char == "9"  ~ "public opinion",
    topic_char == "10" ~ "politics",
    topic_char == "11" ~ "sports",
    TRUE               ~ topic_char
  )
}

# Main function: run topic model for one group and one year.
run_topic_analysis <- function(group_label  = "lgbt",
                               group_folder = "LGBT",
                               year         = 2014,
                               threshold    = 0.5) {
  
  message("=== Topic analysis: group = ", group_label,
          " (folder: ", group_folder, "), year = ", year, " ===")
  
  # ------------------------- 1. Load and combine data ------------------------
  
  input_folder <- file.path("data", "processed", group_folder)
  
  if (!dir.exists(input_folder)) {
    stop("Input folder not found: ", input_folder)
  }
  
  rds_files <- list.files(input_folder, pattern = "\\.RDS$", full.names = TRUE)
  
  if (length(rds_files) == 0) {
    stop("No .RDS files found in folder: ", input_folder)
  }
  
  message("  Found ", length(rds_files), " RDS files. Reading and combining...")
  
  combined_data <- rds_files |>
    lapply(readRDS) |>
    dplyr::bind_rows()
  
  # ------------------------- 2. Ensure year variable -------------------------
  
  if (!"pub_year" %in% names(combined_data)) {
    if (!"publication_date_mdy" %in% names(combined_data)) {
      stop("Columns 'pub_year' or 'publication_date_mdy' not found in combined_data.")
    }
    
    combined_data <- combined_data %>%
      dplyr::mutate(
        pub_date = as.Date(publication_date_mdy),
        pub_year = as.integer(format(pub_date, "%Y"))
      )
  }
  
  combined_data_year <- combined_data %>%
    dplyr::filter(pub_year == year)
  
  if (nrow(combined_data_year) == 0) {
    warning("No articles found for group ", group_label,
            " in year ", year, ". Skipping.")
    return(invisible(NULL))
  }
  
  message("  Articles in ", year, ": ", nrow(combined_data_year))
  
  # ------------------------- 3. Corpus and tokenization ----------------------
  
  if (!"body" %in% names(combined_data_year)) {
    stop("Column 'body' not found in combined_data_year.")
  }
  
  corp <- quanteda::corpus(combined_data_year, text_field = "body")
  
  toks <- quanteda::tokens(
    corp,
    what = "word",
    remove_punct     = TRUE,
    remove_numbers   = TRUE,
    remove_symbols   = TRUE,
    remove_separators = TRUE
  )
  
  toks <- quanteda::tokens_remove(
    toks,
    pattern   = c(quanteda::stopwords("en"), "*-time", "updated-*", "gmt", "bst"),
    valuetype = "glob"
  )
  
  dfm_all <- quanteda::dfm(toks)
  us_dfm  <- dfm_all  # untrimmed dfm to preserve all potential seed terms
  
  # ------------------------- 4. Dictionary and model -------------------------
  
  dict_path <- file.path("dictionary", "topics.yml")
  
  if (!file.exists(dict_path)) {
    stop("Dictionary file not found: ", dict_path,
         ". Place 'topics.yml' in the 'dictionary' folder.")
  }
  
  dict_topic <- quanteda::dictionary(file = dict_path)
  
  # Set seed and threads for reproducibility
  set.seed(123)
  options(seededlda_threads = 1)
  
  tmod_slda <- seededlda::textmodel_seededlda(
    us_dfm,
    dictionary = dict_topic
    # Additional options can be added here if needed:
    # residual     = 1,
    # batch_size   = 0.02,
    # min_termfreq = 5
  )
  
  # ------------------------- 5. Top terms per topic --------------------------
  
  topic_terms <- quanteda.textmodels::terms(tmod_slda, 10)
  
  if (!dir.exists(file.path("output", "topics"))) {
    dir.create(file.path("output", "topics"), recursive = TRUE)
  }
  
  topic_terms_path <- file.path(
    "output", "topics",
    paste0("topic_terms_", group_label, "_", year, ".csv")
  )
  
  write.csv(
    topic_terms,
    file      = topic_terms_path,
    row.names = TRUE
  )
  
  message("  Top terms per topic saved to: ", topic_terms_path)
  
  # ------------------------- 6. Topic assignment with threshold --------------
  
  doc_topic_matrix <- tmod_slda$theta
  doc_topic_df     <- as.data.frame(doc_topic_matrix)
  
  max_theta              <- apply(doc_topic_df, 1, max)
  dominant_topic_indices <- apply(doc_topic_df, 1, which.max)
  
  keep_idx <- which(max_theta > threshold)
  
  if (length(keep_idx) == 0) {
    warning("No documents with max topic probability > ", threshold,
            " for group ", group_label, " in year ", year, ". Skipping.")
    return(invisible(NULL))
  }
  
  predominant_topic_docs <- data.frame(
    doc_id      = keep_idx,
    topic_index = dominant_topic_indices[keep_idx],
    theta       = max_theta[keep_idx]
  )
  
  predominant_topic_docs <- predominant_topic_docs %>%
    dplyr::mutate(
      topic_label = label_topics(topic_index)
    )
  
  # ------------------------- 7. Join back to article data --------------------
  
  combined_data_year_with_id <- combined_data_year %>%
    dplyr::mutate(doc_id = dplyr::row_number())
  
  data_final <- combined_data_year_with_id %>%
    dplyr::left_join(predominant_topic_docs, by = "doc_id")
  
  data_final_filtered <- data_final %>%
    dplyr::filter(!is.na(topic_label))
  
  # ------------------------- 8. Article counts per topic ---------------------
  
  articles_per_topic_df <- data_final_filtered %>%
    dplyr::count(topic_label, name = "ArticleCount") %>%
    dplyr::mutate(
      Group = group_label,
      Year  = year
    ) %>%
    dplyr::rename(Topic = topic_label)
  
  articles_per_topic_path <- file.path(
    "output", "topics",
    paste0("articles_per_topic_", group_label, "_", year, ".csv")
  )
  
  write.csv(
    articles_per_topic_df,
    file      = articles_per_topic_path,
    row.names = FALSE
  )
  
  message("  High-confidence article counts saved to: ",
          articles_per_topic_path)
  
  # ------------------------- 9. Save article-level topics --------------------
  
  doc_topics_path <- file.path(
    "output", "topics",
    paste0("doc_topics_", group_label, "_", year, ".csv")
  )
  
  write.csv(
    data_final_filtered,
    file      = doc_topics_path,
    row.names = FALSE
  )
  
  message("  Article-level topics (theta > ", threshold,
          ") saved to: ", doc_topics_path)
  
  invisible(list(
    topic_terms      = topic_terms,
    articles_per_topic = articles_per_topic_df,
    doc_topics       = data_final_filtered
  ))
}
