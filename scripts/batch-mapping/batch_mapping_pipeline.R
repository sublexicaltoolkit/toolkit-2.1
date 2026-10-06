# TODO update with Toolkit section
#
# Processes a list of spellings and pronunciations in batch, with optional
# frequencies, through the map_GRAIN -> make_tables -> map_value pipeline at 
# every grain size and weight.
# Each function call will save the generated datasets to the global environment
# under the user-supplied naming scheme, and will append descriptive suffixes in
# the same format as the Toolkit's all_words, all_tables, and scored_words
# datasets. This is especially useful when datasets need to be referenced several times.
#
# The functions themselves are defined in scripts/lib/pipeline_functions.R.

# setwd("<path to scripts/batch-mapping>")
load("../../Toolkit_v2.1.RData")
source("../lib/pipeline_functions.R")

# These lists must keep exactly this form. Remove entries to skip grain sizes or weights.
# The values are the suffixes of the datasets produced.
grain_sizes <- list(
    "PG" = "_PG",
    "ONC" = "_ONC",
    "OC" = "_OC",
    "OR" = "_OR"
)
weight_options <- list(
    "default" = "",
    "noposition" = "_noposition",
    "freq" = "_freq",
    "freq_noposition" = "_freq_noposition"
)

# To illustrate, we first take a small subset of the Toolkit's wordlist for the
# following words:
example <- wordlist_v2_1_merged[wordlist_v2_1_merged$spelling %in% c("cat", "cap", "cape", "cast", "chat", "coat"), ]
s <- example$spelling
p <- example$pronunciation
w <- example$freq

# We set the root name to "mwords", which will yield:
# mwords_PG, mwords_ONC, mwords_OC, mwords_OR  (like all_words_*)
l1 <- batch_map_words(s, p, grain_sizes, "mwords")

# Note that these functions proceed such that the output_name parameter of the
# previous function is exactly the input name of the subsequent function. The
# make_tables step of the pipeline now outputs:
# mtables_PG, mtables_PG_noposition, mtables_PG_freq, mtables_PG_freq_noposition, ...  (like all_tables_*)
l2 <- batch_make_tables("mwords", grain_sizes, weight_options, w, "mtables")

# Finally, for map_value:
# mvalues_PG, mvalues_PG_noposition, ...  (like scored_words_*)
l3 <- batch_map_values(s, p, "mtables", grain_sizes, weight_options, "mvalues")

summarize_words(mvalues_PG, "PG")

# Calling rm() on the results will remove all of the generated datasets from
# the global environment
rm(list = c(l1, l2, l3))
