# Generating Pseudoword Pronunciations with pw_read
# Follows Section 6.5 of the Toolkit Guide
#
# pw_read generates possible pronunciations for pseudowords (or any spelling)
# based on the corpus statistics computed by parse_corpus.
#
# min_map controls how many phoneme options are included at each grapheme
# position: a value of 1 returns only the most probable; lower values include
# less probable phoneme mappings. A value of 0 shows all valid options.
#
# min_pp controls how many grapheme parses are considered: a value of 1 uses
# only the most probable parse; lower values include alternative parses above
# the threshold. A value of 0 yields all valid parses.
#
# Note: pw_read only considers whether grapheme parses are possible in
# isolation - each grapheme in a parse must exist independently in the corpus,
# but the resulting pronunciation sequence is not checked for phonotactic
# validity. See "yacht".

# setwd("replace/with/path/to/pw-fcns")
load("../../Toolkit_v2.1.RData")

# parsed_corpus must be available. If not preloaded, compute it:
# parsed_corpus <- parse_corpus(wordlist_v2_1_merged, all_words_PG)

# ===========================================================================
# a) Most Common Usage
# ===========================================================================

# Get the most likely pronunciation for a pseudoword at the PG level.
pw_read(
  target        = "floke",
  parsed_corpus = parsed_corpus,
  PG_table      = all_tables_PG,
  min_map = 1)

# Lower min_map to see more pronunciation options:
pw_read(
  target        = "floke",
  parsed_corpus = parsed_corpus,
  PG_table      = all_tables_PG,
  min_map       = 0.01)

# Return only the pronunciation candidates without scoring:
pw_read(
  target        = "floke",
  parsed_corpus = parsed_corpus,
  PG_table      = all_tables_PG,
  score         = FALSE)

# ===========================================================================
# b) Calling pw_read on a list of words
# ===========================================================================

# pw_read accepts a vector of spellings and returns combined results:
pw_read(
  target        = c("sebe", "floke", "brin"),
  parsed_corpus = parsed_corpus,
  PG_table      = all_tables_PG)

# ===========================================================================
# c) Real Word vs. Pseudoword
# ===========================================================================

# pw_read can also be called on real words, to see alternative readings:
pw_read(
  target        = "reach",
  parsed_corpus = parsed_corpus,
  PG_table      = all_tables_PG)

# --- Pseudoword ---
pw_read(
  target        = "blafe",
  parsed_corpus = parsed_corpus,
  PG_table      = all_tables_PG)

# ===========================================================================
# d) Using All Parameters
# ===========================================================================

# Score at all levels, with all parameters explicitly specified:
pw_read(
  target        = "floke",
  parsed_corpus = parsed_corpus,
  PG_table      = all_tables_PG,
  OC_table      = all_tables_OC,
  OR_table      = all_tables_OR,
  level         = "all",
  score         = TRUE,
  min_pp        = 0.5,
  min_map       = 0.05,
  mean_score    = 0.3,
  max_options   = 500,
  param         = "GP",
  summary_mode  = "default")

# ===========================================================================
# Additional Examples
# ===========================================================================

# --- Multi-level scoring ---
# Score at PG, OC, and OR levels simultaneously:
pw_read(
  target        = "floke",
  parsed_corpus = parsed_corpus,
  PG_table      = all_tables_PG,
  OC_table      = all_tables_OC,
  OR_table      = all_tables_OR,
  level         = "all")

# --- Including alternative grapheme parses ---
# Set min_pp < 1 to consider less probable grapheme parsings:
pw_read(
  target        = "floke",
  parsed_corpus = parsed_corpus,
  PG_table      = all_tables_PG,
  min_pp        = 0.5)

# Set min_pp = 0 to include all valid grapheme parses:
pw_read(
  target        = "floke",
  parsed_corpus = parsed_corpus,
  PG_table      = all_tables_PG,
  min_pp        = 0)

# --- ONC summary mode ---
# Get scored pronunciations with consistency broken down by onset, nucleus,
# coda:
pw_read(
  target        = "floke",
  parsed_corpus = parsed_corpus,
  PG_table      = all_tables_PG,
  min_map       = 0.01,
  summary_mode  = "ONC")
