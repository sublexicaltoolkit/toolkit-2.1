# These are some shared helper functions used by various Toolkit pipelines.
# In this repo, they are sourced specifically by  scripts/all-measures/, 
# scripts/full-pipeline/, and scripts/batch-mapping/.
# We assume the Toolkit is already loaded.
# 
# Dataset naming conventions follow that of the Toolkit:
# <root><grain suffix><weight suffix>
# e.g. "scored_words" + "_PG" + "_freq" = scored_words_PG_freq. 
# Grain sizes and weight options provide the suffixes for these datasets:
#   grain_sizes    <- list("PG" = "_PG", "ONC" = "_ONC", "OC" = "_OC", "OR" = "_OR")
#   weight_options <- list("default" = "", "noposition" = "_noposition",
#                          "freq" = "_freq", "freq_noposition" = "_freq_noposition")
#
# Note that any datasets created by the functions below will be written to the
# global R environment.


### TABLES ###

# Calls make_tables with the weight/positional arguments as described above in 
# weight_options. freq_data is the vector of frequencies, and should be supplied
# in the order that corresponds to the entries in the mapped_words dataset, and
# be of the same length. If the provided weight includes "freq" these 
# frequencies will be used in computation.
make_tables_for_weight <- function(mapped_words, weight, freq_data) {
    switch(weight,
        default         = make_tables(mapped_words),
        noposition      = make_tables(mapped_words, positional = FALSE),
        freq            = make_tables(mapped_words, weight = freq_data),
        freq_noposition = make_tables(mapped_words, weight = freq_data, positional = FALSE),
        stop("Unknown weight option: ", weight)
    )
}

# Builds any tables in the grain_sizes x weight_options grid which do not exist
# in the global environment, for the Toolkit's wordlist. For the full Toolkit
# freq_data must line up with the words in the all_words datasets, as in
# make_tables_for_weight. For the Toolkit corpus this list is wordlist_v2_1_merged$freq.
# The words_root and tables_root parameters supply the names that should be used
# in the created datasets, with the suffixes appended as described above.
# Returns the names of the tables created.
build_missing_tables <- function(grain_sizes, weight_options, freq_data = wordlist_v2_1_merged$freq,
                                 words_root = "all_words", tables_root = "all_tables") {
    dataset_names <- c()

    for (grain in names(grain_sizes)) {
        for (weight in names(weight_options)) {
            var_name <- paste0(tables_root, grain_sizes[[grain]], weight_options[[weight]])
            if (exists(var_name, envir = .GlobalEnv)) next

            message("Building ", var_name)
            mapped_words <- get(paste0(words_root, grain_sizes[[grain]]))
            assign(var_name, make_tables_for_weight(mapped_words, weight, freq_data), envir = .GlobalEnv)

            dataset_names <- c(dataset_names, var_name)
        }
    }

    return(dataset_names)
}


### BATCH PIPELINE: map_<grain> -> make_tables -> map_value ###

# Maps spellings to pronunciations at every grain size in grain_sizes, and saves
# saving each result as <output_name><grain suffix>.
# These outputs have the same format as the Toolkit's all_words datasets.
batch_map_words <- function(spellings, pronunciations, grain_sizes, output_name) {
    dataset_names <- c()

    for (grain in names(grain_sizes)) {
        var_name <- paste0(output_name, grain_sizes[[grain]])

        # Call the relevant map function (map_PG, map_ONC, ...)
        # FALSE here means no progress bar is printed
        data <- get(paste0("map", grain_sizes[[grain]]))(spellings, pronunciations, FALSE)
        assign(var_name, data, envir = .GlobalEnv)

        dataset_names <- c(dataset_names, var_name)
    }

    return(dataset_names)
}

# Calls make_tables on every <mapped_words_name><grain suffix> dataset for all
# weight_options supplied. Saves the results as <output_name><grain suffix><weight suffix>. 
# If batch-mapping functions are used, the mapped_words_name here will be the 
# same as output_name in batch_map_words (above).
# freq_data is needed only if weight_options includes a frequency-based weight,
# and must be in the same order as the spellings that were mapped.
# Outputs have the same format as the Toolkit's all_tables datasets.
batch_make_tables <- function(mapped_words_name, grain_sizes, weight_options, freq_data, output_name) {
    dataset_names <- c()

    for (grain in names(grain_sizes)) {
        mapped_words <- get(paste0(mapped_words_name, grain_sizes[[grain]]))
        for (weight in names(weight_options)) {
            var_name <- paste0(output_name, grain_sizes[[grain]], weight_options[[weight]])
            assign(var_name, make_tables_for_weight(mapped_words, weight, freq_data), envir = .GlobalEnv)

            dataset_names <- c(dataset_names, var_name)
        }
    }

    return(dataset_names)
}

# Calls map_value for the spellings and pronunciations for every
# <table_name><grain suffix><weight suffix> table, and saves each result as
# <output_name><grain suffix><weight suffix>. As before, the naming schemes
# continue to chain between the batch-mapping functions, so table_name here
# is the same as the value of output_name passed to batch_make_tables.
# Outputs here have the same format as the Toolkit's scored_words family
# of datasets.
batch_map_values <- function(spellings, pronunciations, table_name, grain_sizes, weight_options, output_name) {
    dataset_names <- c()

    for (grain in names(grain_sizes)) {
        for (weight in names(weight_options)) {
            data_table <- get(paste0(table_name, grain_sizes[[grain]], weight_options[[weight]]))
            var_name <- paste0(output_name, grain_sizes[[grain]], weight_options[[weight]])

            assign(var_name, map_value(spellings, pronunciations, grain, data_table), envir = .GlobalEnv)

            dataset_names <- c(dataset_names, var_name)
        }
    }

    return(dataset_names)
}


### SCORING WORDS FROM THE TOOLKIT CORPUS ###

# Provides the row number(s) in the provided corpus wordlist corresponding to
# the provided spelling(s), in the order in which they appear. If spellings
# is NULL, all row numbers will be returned.
# Spellings with more than one pronunciation will return row numbers for 
# all of their rows.
# The function will stop if any of the spellings provided are not found in
# the supplied corpus.
# Assumes the supplied corpus has a column named "spellings", as it does in the
# Toolkit.
corpus_rows <- function(spellings = NULL, corpus = wordlist_v2_1_merged) {
    if (is.null(spellings)) return(seq_len(nrow(corpus)))

    missing <- setdiff(spellings, corpus$spelling)
    if (length(missing) > 0) {
        stop("Not in the corpus: ", paste(missing, collapse = ", "))
    }

    return(which(corpus$spelling %in% spellings))
}

# Scores the corresponding rows of the supplied corpus (i.e. producing datasets
# of the scored_words family) for all combinations of the provided grain_sizes
# and weight_options. 
# Results are saved to the global environment as <output_name><grain suffix><weight suffix>. 
# If the scored_words dataset(s) already exist and the provided rows are desired,
# they can be obtained by passing the root name of that scored_words dataset
# to scored_root (the name of the dataset will be built from the grain_sizes x 
# weight_options combinations).
# If scoring needs to be completed for the provided row(s), outputs from a
# make_tables function will be looked for with the provided tables_root root 
# name.
score_corpus_rows <- function(rows, grain_sizes, weight_options, output_name, corpus = wordlist_v2_1_merged,
                              tables_root = "all_tables", scored_root = "scored_words") {
    dataset_names <- c()

    for (grain in names(grain_sizes)) {
        for (weight in names(weight_options)) {
            suffix <- paste0(grain_sizes[[grain]], weight_options[[weight]])
            shipped_name <- paste0(scored_root, suffix)
            var_name <- paste0(output_name, suffix)

            if (exists(shipped_name, envir = .GlobalEnv)) {
                data <- get(shipped_name)[rows]
            } else {
                message("Scoring ", length(rows), " words against ", tables_root, suffix)
                data <- map_value(corpus$spelling[rows], corpus$pronunciation[rows], grain, get(paste0(tables_root, suffix)))
            }
            assign(var_name, data, envir = .GlobalEnv)

            dataset_names <- c(dataset_names, var_name)
        }
    }

    return(dataset_names)
}


### MEASURE EXTRACTION ###

# Wrapper for summarize_words with the root scored_words dataset table name,
# grain size, and weight provided individually. Useful when summaries across
# many grain sizes and weights are desired when called within a loop.
extract_summary <- function(root, grain, weight, measure) {
    summary <- summarize_words(get(paste0(root, grain, weight)), measure)
    colnames(summary)[1:2] <- c("spelling", "pronunciation")

    return(summary)
}

# Trims any undesired columns from the output of a summarize_words call, such 
# that only those statistics listed in the provided statistics vector remain
# in the output.
# Choose from "mean", "median", "max", "min", and "sd".
filter_columns <- function(df, statistics) {
    keep_cols <- sapply(colnames(df), function(col) {
        if (col %in% c("spelling", "pronunciation")) return(TRUE)
        if (grepl("\\.", col)) return(sub(".*\\.", "", col) %in% statistics)
        return(TRUE)
    })

    return(df[, keep_cols, drop = FALSE])
}

# Bulk function for obtaining all statistics, for all provided measures, for all
# provided grain_size x weight_option combinations, for scored_words datasets with
# the mapped_values_name as the root. See the batch-mapping pipeline for example
# usage.
get_measures <- function(mapped_values_name, grain_sizes, weight_options, measures, statistics) {
    results_list <- list()

    for (measure in measures) {
        for (grain in names(grain_sizes)) {
            for (weight in names(weight_options)) {
                label <- paste0(grain, "_", weight, "_", measure)
                message("Summarizing ", label)

                summary_result <- extract_summary(mapped_values_name, grain_sizes[[grain]], weight_options[[weight]], measure)

                # Keep spelling and pronunciation from the first summary only
                if (length(results_list) == 0) {
                    results_list[[label]] <- summary_result
                } else {
                    results_list[[label]] <- summary_result[, -c(1, 2)]
                }
            }
        }
    }

    combined_results <- do.call(cbind, results_list)
    colnames(combined_results)[1:2] <- c("spelling", "pronunciation")

    return(filter_columns(combined_results, statistics))
}

# Appends the parsing probability measures to a get_measures output. If a word
# lacks a parsing probability entry in the supplied pp_summary, NA is listed
# for all pp values.
# For the Toolkit's corpus, pp_summary is parsed_corpus[[4]].
add_pp_measures <- function(measures_df, pp_summary = parsed_corpus[[4]],
                            pp_cols = c("pp_mean", "pp_min", "pp_freq_mean", "pp_freq_min")) {
    matched <- pp_summary[match(measures_df$spelling, pp_summary$spelling), pp_cols, drop = FALSE]
    rownames(matched) <- NULL

    return(cbind(measures_df, matched))
}
