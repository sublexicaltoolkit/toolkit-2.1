# TODO update with respective toolkit guide section 
#
# Computes every measure for a corpus from scratch using the Toolkit's functions.
# In order, this pipeline will: 
# 1) Strip accents from grapheme strings
# 2) Map all words at every grain size 
# 3) Build frequency and consistency tables for all words
# 4) Score each word against the generated tables
# 5) Summarize all of the measures 
# 6) Add and compute parsing probabilities

# setwd("<path to scripts/full-pipeline>")
load("../../Toolkit_v2.1.RData")
source("../lib/pipeline_functions.R")

# For this example, we take a small subset of the Toolkit's wordlist and treat
# it as its own corpus.
# To run this script for your own corpus, just replace this line with a data
# frame containing three columns: spelling, pronunciation, and freq, all of the
# same length, with respective entries corresponding.
my_corpus <- wordlist_v2_1_merged[seq(1, nrow(wordlist_v2_1_merged), by = 100), ]

grain_sizes <- list("PG" = "_PG", "ONC" = "_ONC", "OC" = "_OC", "OR" = "_OR")
weight_options <- list("default" = "", "noposition" = "_noposition", "freq" = "_freq", "freq_noposition" = "_freq_noposition")
measures <- c("PG", "GP", "PG_freq", "G_freq", "P_freq")
statistics <- c("mean", "median", "max", "min", "sd")

my_corpus$spelling <- replace_accented_vowels(my_corpus$spelling)

# map_GRAIN -> make_tables -> map_value, saving my_words_*, my_tables_*, and my_scored_*
batch_map_words(my_corpus$spelling, my_corpus$pronunciation, grain_sizes, "my_words")
batch_make_tables("my_words", grain_sizes, weight_options, my_corpus$freq, "my_tables")
batch_map_values(my_corpus$spelling, my_corpus$pronunciation, "my_tables", grain_sizes, weight_options, "my_scored")

my_measures <- get_measures("my_scored", grain_sizes, weight_options, measures, statistics)

# Parsing probabilities for this corpus (parse_corpus needs the PG mapping)
my_parsed <- parse_corpus(my_corpus, my_words_PG)
my_measures <- add_pp_measures(my_measures, my_parsed[[4]])

# Full output of measures from this generated corpus
write.csv(my_measures, "full_pipeline_measures.csv", row.names = FALSE)
