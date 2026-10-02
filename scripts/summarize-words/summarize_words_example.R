# TOOD add toolkit guide section
# 
# summarize_words computes the summary statistics (mean, median, max, min, sd) 
# for on measure from "PG", "GP", "PG_freq", "P_freq", and "G_freq", or a 
# parsing measure ("pp", "pp_count", "pp2", "pp2_count"), across the units for
# a word. See the Toolkit guide for descriptions.
#
# mode = "default" summarizes across all units of the word.
# mode = "ONC" gives separate statistics for the  onset, nucleus, and coda units
# within a word. Note that parsing measures are only available in default mode.

# setwd("<path to scripts/summarize-words>")
load("../../Toolkit_v2.1.RData")

# We can take the following two words, processed by map_value already and in
# the scored_words dataset, as examples:
toolkit <- scored_words_PG[which(wordlist_v2_1_merged$spelling == "toolkit")]
penguin <- scored_words_PG[which(wordlist_v2_1_merged$spelling == "penguin")]

# Default: 

# Spelling consistency (PG) of "toolkit":
summarize_words(mapped_words = toolkit, parameter = "PG")

# Reading consistency (GP) of "toolkit":
summarize_words(mapped_words = toolkit, parameter = "GP")

# Note that mapped_words can be supplied as a list:
summarize_words(mapped_words = c(toolkit, penguin), parameter = "GP")

# Mean parsing probability of "penguin":
summarize_words(mapped_words = penguin, parameter = "pp")

# ONC: 

summarize_words(mapped_words = penguin, parameter = "PG", mode = "ONC")

# To summarize all words from the corpus for each of the two modes:
all_words_PG_probability <- summarize_words(mapped_words = scored_words_PG, parameter = "PG")
all_words_PG_onc <- summarize_words(mapped_words = scored_words_PG, parameter = "PG", mode = "ONC")
