# Generating Pseudoword Pronunciations with pw_read
# Follows Section 6.5 of the Toolkit Guide
#
# pw_read generates possible pronunciations for pseudowords (or any spelling)
# based on the corpus statistics computed by parse_corpus. Supplying a value of
# 1 to min_map yields only the most probably pronunciation; lower values of 
# min_map set smaller lower bounds on the probability and thus yield additional,
# though less probable, pronunciations. A value of 0 shows all valid options.

# setwd("replace/with/path/to/pw-read")
load("../../Toolkit_v2.1.RData")

# --- Basic Usage ---
# Generate the most likely pronunciation for a pseudoword at PG level:
result <- pw_read(
  target        = "floke",
  level         = "PG",
  parsed_corpus = parsed_corpus,
  PG_table      = all_tables_PG,
  min_map       = 1)

# The result contains possible phoneme sequences and their scores.
result

# --- Multi-level Scoring ---
# Score at all grain sizes (PG, OC, OR) simultaneously:
result <- pw_read(
  target        = "floke",
  level         = "all",
  parsed_corpus = parsed_corpus,
  PG_table      = all_tables_PG,
  OC_table      = all_tables_OC,
  OR_table      = all_tables_OR,
  min_map       = 1,
  score         = TRUE)

result

# --- Adjusting Thresholds ---
# Lower min_map to see more pronunciation options:
result <- pw_read(
  target        = "floke",
  level         = "all",
  parsed_corpus = parsed_corpus,
  PG_table      = all_tables_PG,
  OC_table      = all_tables_OC,
  OR_table      = all_tables_OR,
  min_map       = 0.5,
  min_pp        = 0,
  score         = TRUE)

result

# --- Another example from USAGE SCRIPTS 2.1.R ---
pw_read(
  target        = "sebe",
  level         = "PG",
  parsed_corpus = parsed_corpus,
  PG_table      = all_tables_PG,
  min_map       = 1)
