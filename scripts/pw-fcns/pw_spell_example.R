# Generating Pseudoword Spellings with pw_spell
# Follows Section 6.5 of the Toolkit Guide
#
# pw_spell generates possible spellings for a given pronunciation string.
# Valid levels: "PG", "OC", "OR", or "all".
# Supplying a value of 1 to min_map yields only the most probable spelling;
# lower values of min_map set smaller lower bounds on the probability and thus
# yield additional, though less probable, spellings. A value of 0 shows all
# valid options.

# setwd("replace/with/path/to/pw-fcns")
load("../../Toolkit_v2.1.RData")

# ===========================================================================
# a) Most Common Usage
# ===========================================================================

# Get the most likely spelling for a pronunciation at the PG level.
# This is the simplest and most common call: provide a target pronunciation,
# the level, and the appropriate table.
pw_spell(target = "sib", PG_table = all_tables_PG)

# By default, min_map = 1, which returns only the most probable spelling(s).
# Lower min_map to see more options above a probability threshold:
pw_spell(target = "sib", PG_table = all_tables_PG, min_map = 0.01)

# Set score = FALSE to return only the spelling strings without scoring them:
pw_spell(target = "sib", PG_table = all_tables_PG, min_map = 0.01, score = FALSE)

# ===========================================================================
# b) Individual Word vs. List of Words
# ===========================================================================

# --- Individual word ---
pw_spell(target = "fl5t", PG_table = all_tables_PG)

# --- List of words ---
# pw_spell accepts a vector of pronunciations and returns results for all:
pw_spell(
  target   = c("fl5t", "sib", "gr8t"),
  PG_table = all_tables_PG)

# ===========================================================================
# c) Real Word vs. Pseudoword
# ===========================================================================

# --- Real word ---
# "cat" = /k@t/ in Toolkit notation:
pw_spell(target = "k@t", PG_table = all_tables_PG)

# --- Pseudoword ---
# A novel pronunciation /blEf/ that is not in the corpus:
pw_spell(target = "blEf", PG_table = all_tables_PG)

# ===========================================================================
# d) Using All Parameters
# ===========================================================================

# First, compute parsed_corpus for PP scoring (if not already available):
# parsed_corpus <- parse_corpus(wordlist_v2_1_merged, all_words_PG)

# Generate spellings at the OC level, scored by parsing probability (PP),
# with all parameters explicitly specified:
pw_spell(
  target        = "fl5k",
  level         = "OC",
  score         = TRUE,
  PG_table      = all_tables_PG,
  OC_table      = all_tables_OC,
  OR_table      = all_tables_OR,
  min_map       = 0.05,
  max_options   = 500,
  param         = "pp",
  mean_score    = 0.3,
  parsed_corpus = parsed_corpus,
  summary_mode  = "default")

# ===========================================================================
# Additional Examples
# ===========================================================================

# --- Different Grain Sizes ---

# OC level:
pw_spell(target = "sib", level = "OC", OC_table = all_tables_OC)

# OR level:
pw_spell(target = "sib", level = "OR", OR_table = all_tables_OR)

# All levels at once:
pw_spell(
  target   = "sib",
  level    = "all",
  PG_table = all_tables_PG,
  OC_table = all_tables_OC,
  OR_table = all_tables_OR)

# --- ONC summary mode ---
# Get scored spellings with consistency broken down by onset, nucleus, coda:
pw_spell(
  target       = "sib",
  PG_table     = all_tables_PG,
  min_map      = 0.01,
  summary_mode = "ONC")
