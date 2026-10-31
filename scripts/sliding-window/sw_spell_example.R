# TODO update with relevant section of toolkit guide
# Sliding-window spelling generation

# setwd("<path to scripts/sliding-window>")
load("../../Toolkit_v2.1.RData")
source("../lib/sw_toolkit_parse.R")
source("../lib/sw_functions.R")
options(scipen = 999)

# Build the letter-group pairs and the entry list
pairs_PG <- attested_pairs("grapheme")
entries  <- lex_entries(scored_words_PG)

# Real-word example
kat <- sw_spell_word("k@t", pairs_PG, entries, enum_limit = 1e5)
kat$t3                                   # one row per parse
kat$t4                                   # one row per pronunciation
kat$spellings

# Pseudoword example
bliz <- sw_spell_word("bliz", pairs_PG, entries, enum_limit = 1e5)
bliz$t3
bliz$t4
bliz$spellings

# Respective t3 and t4 outputs can be stacked and viewed as their own datasets:
all_t3 <- rbind(kat$t3, bliz$t3)
all_t4 <- rbind(kat$t4, bliz$t4)
all_t3
all_t4
