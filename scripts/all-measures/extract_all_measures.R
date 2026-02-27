# setwd("Replace this string with the path to the all-measures directory of this repo and uncomment")
# The Toolkit only needs to be loaded once
load("../../Toolkit_v2.1.RData")

### SOME NOTES:
# If you are supplying your own datasets/corpus, adjust the names of the datasets in the grain_sizes and weight_options lists. The
# functions in this script will be concatenating values in these two lists in order to get the name of your dataset.

# Edit any one of these lists to only obtain those measures.
# Pass reuslts into extract_master_list_measures.py to filter for desired words after

# List of grain sizes
# Keys are grain sizes, and values are names of datasets of scored words for 
grain_sizes <- list(
  "PG" = "scored_words_PG", 
  "ONC" = "scored_words_ONC", 
  "OC" = "scored_words_OC", 
  "OR" = "scored_words_OR"
)

# List of weight options
# Keys are options, and values are strings appended to dataset names to obtain measures of the appropriate weight
weight_options <- list(
  "default" = "", 
  "noposition" = "_noposition", 
  "freq" = "_freq", 
  "freq_noposition" = "_freq_noposition"
)

# List of individual measures to obtain
measures <- c("PG", "GP", "PG_freq", "G_freq", "P_freq")

# List of measure statistics to obtain
statistics <- c("mean", "median", "max", "min", "sd")

# Extracts the summary statistics for a dataset of the desired grain size and weight, and returns the information corresponding
# to the desired measure.
# Note that if you are trying to extract a summary from a dataset of a different name, you will need to edit the paste0 command in this function (changing this to be a parameter soon with full mapping-pipeline release)
extract_summary <- function(grain, weight, measure) {
  # Create the appropriate dataset name
  dataset <- paste0(grain, weight)

  summary <- summarize_words(get(dataset), measure)
  colnames(summary)[1] <- "spelling"
  colnames(summary)[2] <- "pronunciation"

  return(summary)
}

# Filter out undersired statistics
filter_columns <- function(df, statistics) {
  col_names <- colnames(df)

  keep_cols <- sapply(col_names, function(col) {
    if (col %in% c("spelling", "pronunciation")) {
      return(TRUE) # Always keep these columns
    }
    if (grepl("\\.", col)) {
      suffix <- sub(".*\\.", "", col) # Extract part after last period
      return(suffix %in% statistics) # Exclude columns with unwanted statistics
    }
    return(TRUE) # Keep columns without a period
  })

  return(df[, keep_cols, drop = FALSE])
}

# Extracts all measures from appropriately named datasets based on the provided parameters.
get_measures <- function(grain_sizes, weight_options, measures, statistics) {
  results_list <- list()

  i <- 1
  for (measure in measures) {
    for (grain in names(grain_sizes)) {
      for (weight in names(weight_options)) {
        print(paste0("doing iter ", i, " for dataset ", grain_sizes[[grain]], weight_options[[weight]], " and measure ", measure))

        summary_result <- extract_summary(grain_sizes[[grain]], weight_options[[weight]], measure)

        if (length(results_list) == 0) {
          # Include spelling and pronunciation columns only in the first iteration
          results_list[[paste0(grain, "_", weight, "_", measure)]] <- summary_result
        } else {
          # Subsequent summaries exclude spelling and pronunciation columns
          results_list[[paste0(grain, "_", weight, "_", measure)]] <- summary_result[, -c(1, 2)]
        }
        i <- i + 1
      }
    }
  }
  
  # Combine all summaries into a single data frame
  combined_results <- do.call(cbind, results_list)
  colnames(combined_results)[1:2] <- c("spelling", "pronunciation")
  
  combined_results <- filter_columns(combined_results, statistics)

  return(combined_results)
}

# Execution. This will take some time to run!
all_measures <- get_measures(grain_sizes, weight_options, measures, statistics)
write.csv(all_measures, "all_measures.csv")

# OPTIONALLY: If you also want the PP measures that are precomputed to be included, include these lines:
# corpus_measures <- read.csv("../precalculated/pp_measures.csv")
# all_measures_with_pp <- merge(all_measures, corpus_measures, by = "spelling")
# write.csv(all_measures_with_pp, "all_measures_with_pp.csv")