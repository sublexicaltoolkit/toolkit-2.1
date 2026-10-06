# TODO update with Toolkit guide section once done
#
# Parsing probability measures (PP) describe how likely any letter is to be
# parsed as a particular grapheme (i.e. how to split the word at the grapheme
# level) given the Toolkit's corpus statistics.
# 
# In the Toolkit, parsed_corpus is the preprocessed dataset from calling
# parse_corpus for the Toolkit wordlist.

# setwd("<path to scripts/parsing-probabilities>")
load("../../Toolkit_v2.1.RData")
library(stringr)

# parse_corpus requires a data frame like the Toolkit wordlist, with spelling,
# pronunciation, and freq columns, as well as the output from calling map_PG on
# that dataset.
# Taking a smaller chunk from the wordlist as an example:
my_corpus <- wordlist_v2_1_merged[1:200, ]
my_parsed <- parse_corpus(my_corpus, map_PG(my_corpus$spelling, my_corpus$pronunciation))

# The following use the preprocessed parsed_corpus to illustrate outputs.
# parsed_corpus is a list of four elements; see the Toolkit guide for descriptions
# of each.

# [[1]] pp_table (parsing probability lookup)
head(parsed_corpus[[1]])

# [[2]] pp_reduced
head(parsed_corpus[[2]])

# [[3]] corpus_parsed
head(parsed_corpus[[3]])

# [[4]] corpus_pp
head(parsed_corpus[[4]])

# The parsed_corpus output can be queried for specific words to obtain their pp
# and pp2 values. Query on the 4th element here:
parsed_corpus[[4]][parsed_corpus[[4]]$spelling == "yacht", ]
parsed_corpus[[4]][parsed_corpus[[4]]$spelling == "reach", ]

# Querying the 3rd element yields the per-letter parsing probability breakdown
# for the queried word
parsed_corpus[[3]][parsed_corpus[[3]]$spelling == "yacht", ]

# --- Merging PP into All Measures ---
# PP columns can be matched onto a table of measures by spelling. add_pp_measures() 
# from scripts/lib/pipeline_functions.R adds pp_mean, pp_min, pp_freq_mean, and 
# pp_freq_min to a dataset consisting of rows for each word by spelling and can
# be used to include the parsed_corpus data like so (all_measures must exist
# in the environment first, and is the full dataset containing all Toolkit measures
# for all corpus words; alternatively this function can simply append these columns
# to any dataset with a "spelling" column)
# source("../lib/pipeline_functions.R")
# all_measures <- add_pp_measures(all_measures, parsed_corpus[[4]])
