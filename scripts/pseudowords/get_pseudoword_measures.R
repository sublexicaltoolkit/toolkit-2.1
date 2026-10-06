# TODO update with toolkit guide section
#
# Pseudowords must be scored against the Toolkit's precomputed tables.
# The output here has the same format as the the real-word measures output
#  (<grain>_<weight>_<measure>.<statistic>), so the two can be vertically
# stacked if desired.
#
# Input: a CSV with a "spelling" and a "pronunciation" column, the pronunciation
#  in the Toolkit's phoneme notation. The pronunciations.csv in this folder is 
# the example used here.
#
# If this hangs, one or more spelling/pronunciation pairs cannot be mapped; see
# scripts/troubleshooting/hanging/ to find them.

# setwd("<path to scripts/pseudowords>")
load("../../Toolkit_v2.1.RData")
source("../lib/pipeline_functions.R")

pseudowords <- read.csv("pronunciations.csv")

grain_sizes <- list("PG" = "_PG", "ONC" = "_ONC", "OC" = "_OC", "OR" = "_OR")
weight_options <- list("default" = "", "noposition" = "_noposition", "freq" = "_freq", "freq_noposition" = "_freq_noposition")
measures <- c("PG", "GP", "PG_freq", "G_freq", "P_freq")
statistics <- c("mean", "median", "max", "min", "sd")

# Build the _noposition and _freq_noposition tables which are not preprocessed:
build_missing_tables(grain_sizes, weight_options)

# Score every pseudoword against every all_tables_* table
batch_map_values(pseudowords$spelling, pseudowords$pronunciation, "all_tables", grain_sizes, weight_options, "pw_scored")

# Extract all of the computed measures
pseudoword_measures <- get_measures("pw_scored", grain_sizes, weight_options, measures, statistics)

write.csv(pseudoword_measures, "pseudoword_measures.csv", row.names = FALSE)
