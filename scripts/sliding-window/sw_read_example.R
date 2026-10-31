# TODO update with toolkit guide section once updated
#
# Sliding-window pronunciation generation

# setwd("<path to scripts/sliding-window>")
load("../../Toolkit_v2.1.RData")
# Load the parsing and sliding window algorithm functions
source("../lib/sw_toolkit_parse.R")
source("../lib/sw_functions.R")
options(scipen = 999) # don't use scientific notation when writing output

# Build the phoneme pairs, dataset for the Toolkit wordlist, and exceptions
# (a, the, hm, shh)
pairs_PG <- attested_pairs("phoneme")
entries  <- lex_entries(scored_words_PG)
exmap    <- lex_exceptions(entries)

# Example for a real word in the reading direction, providing the "cat"
# spelling and other parameters prepared above
# This call produces fewer than 1e5 pronunciations, so those will be enumerated
# in the prons object of the output
cat_read <- sw_read_word("cat", pairs_PG, exmap, entries, enum_limit = 1e5)
cat_read$t1                              # one row per parse
cat_read$t2                              # one row per word
cat_read$prons

# Example for a pseudoword
blease <- sw_read_word("blease", pairs_PG, exmap, entries)
blease$t1
blease$t2

# Respective t1 and t2 outputs can be stacked and viewed as their own datasets:
all_t1 <- rbind(cat_read$t1, blease$t1)
all_t2 <- rbind(cat_read$t2, blease$t2)
all_t1
all_t2
