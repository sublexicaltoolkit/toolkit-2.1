# Parsing Probabilities Pipeline
# Follows Section 6.4 of the Toolkit Guide
#
# This script walks through computing parsing probabilities (PP) for the full
# corpus and inspecting the results. PP quantifies how likely each letter is to
# be parsed as a particular grapheme, given the statistics of the corpus.

# setwd("replace/with/path/to/parsing-probabilities")
load("../../Toolkit_v2.1.RData")

# --- Running parse_corpus ---
# With the Toolkit loaded and all_words_PG available, compute parsing
# probabilities for the entire corpus. NOTE: this will take a long time to run, as it 
# builds a tree of all possible grapheme parsings for every word.
parsed_corpus <- parse_corpus(wordlist_v2_1_merged, all_words_PG)

# --- Outputs ---
# parsed_corpus is a list of four elements:

# [[1]] pp_table: letter-grapheme parsing probabilities
head(parsed_corpus[[1]])

# [[2]] pp_reduced: reduced choice sets (which grapheme options actually occur)
head(parsed_corpus[[2]])

# [[3]] corpus_parsed: full word-letter-grapheme breakdown with PP values
head(parsed_corpus[[3]])

# [[4]] corpus_pp: per-word PP summary (pp_mean, pp_min, pp_freq_mean, pp_freq_min)
head(parsed_corpus[[4]])

# --- Querying PP for Specific Words ---
parsed_corpus[[4]][parsed_corpus[[4]]$spelling == "yacht", ]
parsed_corpus[[4]][parsed_corpus[[4]]$spelling == "reach", ]

# Per-letter breakdown for "yacht"
parsed_corpus[[3]][parsed_corpus[[3]]$spelling == "yacht", ]

# --- Merging PP into All Measures ---
# If you have an all_measures dataframe, you can merge PP values into it:
# all_measures <- merge(all_measures, parsed_corpus[[4]], by = "spelling")
