# TODO update with section from the Toolkit guide 
#
# Builds a dataset with all of the measures computed by the Toolkit for the
# provided list of words in the Toolkit corpus.
# The output will contain one row per word and one column per <grain>_<weight>_<measure>.<statistic>
# combination (e.g. PG_default_PG.mean) along with the four parsing-probability 
# (PP) columns.
#
# By default this example script runs on a few example words. Set target_words <- NULL 
# to regenerate the measures for the whole corpus (22,257 entries and takes a long time).
#
# This script will extract preprocessed measures for words already in the Toolkit.
# For extracting measures from pseudowords, see scripts/pseudowords/.
# To build measures from your own corpus, see full-pipeline/.


# setwd("<path to scripts/all-measures>")
load("../../Toolkit_v2.1.RData")
source("../lib/pipeline_functions.R")

# Spellings to extract measures for
target_words <- c("penguin", "toolkit", "yacht", "reach", "wristwatch")

# Remove entries from any of these lists to leave those measures out
grain_sizes <- list("PG" = "_PG", "ONC" = "_ONC", "OC" = "_OC", "OR" = "_OR")
weight_options <- list("default" = "", "noposition" = "_noposition", "freq" = "_freq", "freq_noposition" = "_freq_noposition")
measures <- c("PG", "GP", "PG_freq", "G_freq", "P_freq")
statistics <- c("mean", "median", "max", "min", "sd")

# The _noposition and _freq_noposition tables are not in the most recent Toolkit RData
# file, so they must first be built:
build_missing_tables(grain_sizes, weight_options)

# Score the target words at every grain/weight combination against the Toolkit's
# corpus-wide tables (all_tables_*).
# This is the last step of the batch pipeline (see scripts/batch-mapping/). The first
# two steps aren't needed, since the corpus words are already mapped (all_words_*),
# and some of the tables come preprocessed while the others were built above.
# The Toolkit also provides default-weight scores (scored_words_PG, _ONC, _OC, _OR),
# so we only need to recompute for the weight options that were not preprocessed.
target <- wordlist_v2_1_merged[corpus_rows(target_words), ]
batch_map_values(target$spelling, target$pronunciation, "all_tables", grain_sizes, weight_options, "target_scored")

# Extract the measures and append the pp measures
all_measures <- get_measures("target_scored", grain_sizes, weight_options, measures, statistics)
all_measures <- add_pp_measures(all_measures, parsed_corpus[[4]])

write.csv(all_measures, "all_measures_v2.1.csv", row.names = FALSE)
