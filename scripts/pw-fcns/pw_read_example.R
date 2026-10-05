# TODO associated section of Toolkit guide
#
# pw_read generates possible pronunciations for a spelling (a pseudoword or a real word), using
# the parsing probabilities in parsed_corpus to choose grapheme parses and the PG table to choose
# phonemes for each grapheme. Here, "possible" means that each individual
# phoneme-grapheme mapping exists in the Toolkit corpus, and does not necessarily
# mean that all generated pronunciations are phonotactically possible.

# setwd("<path to scripts/pw-fcns>")
load("../../Toolkit_v2.1.RData")

# ===========================================================================
# Common Usage
# ===========================================================================

# Obtain most likely pronunciation of a pseudoword:
pw_read(
  target        = "floke",
  parsed_corpus = parsed_corpus,
  PG_table      = all_tables_PG)

# Setting min_map to 0.01 means all mappings in generated pronunciations must
# have a GP consistency of at least 0.01
pw_read(
  target        = "floke",
  parsed_corpus = parsed_corpus,
  PG_table      = all_tables_PG,
  min_map       = 0.01)

# Set score to FALSE to obtain only the list of pronunciations, without computing
# their summary statistics
pw_read(
  target        = "floke",
  parsed_corpus = parsed_corpus,
  PG_table      = all_tables_PG,
  score         = FALSE)

# ===========================================================================
# Calling pw_read with Lists
# ===========================================================================

# Lists can be supplied to process multiple grapheme strings together
pw_read(
  target        = c("sebe", "floke", "brin"),
  parsed_corpus = parsed_corpus,
  PG_table      = all_tables_PG)

# ===========================================================================
# Real and Pseudowords
# ===========================================================================

# pw_read can be called for both real words and pseudowords:
pw_read(
  target        = "reach",
  parsed_corpus = parsed_corpus,
  PG_table      = all_tables_PG)

pw_read(
  target        = "blafe",
  parsed_corpus = parsed_corpus,
  PG_table      = all_tables_PG)

# ===========================================================================
# All pw_read Parameters
# ===========================================================================

# Here, we obtain pronunciations for the pseudoword spelling "floke" at all
# levels (PG, OC, and OR), forcing a minimum parsing probability of 0.5 and 
# a minimum GP consistency of 0.05 for all mappings. We also restrict the final
# list to only include pronunciations whose mean GP consistency (across all
# mappings) is at minimum 0.3. Finally, we sort the scored output on parsing
# probability.
pw_read(
  target        = "floke",
  level         = "all",
  parsed_corpus = parsed_corpus,
  PG_table      = all_tables_PG,
  OC_table      = all_tables_OC,
  OR_table      = all_tables_OR,
  score         = TRUE,
  min_pp        = 0.5,
  min_map       = 0.05,
  mean_score    = 0.3,
  max_options   = 500,
  param         = "GP",
  summary_mode  = "default",
  pp_param      = "pp")

# ===========================================================================
# Analyzing Mispronunciatons
# ===========================================================================

# Here, we obtain every legal (phonotactically possible) reading of the real
# word "reach", sorted by mean GP consistency, and set a few other filtering
# parameters. 
# Note that for larger words, such analyses may not always be possible without
# raising the min_map floor to reduce the number of candidates.
reach_readings <- pw_read(
  target        = "reach",
  parsed_corpus = parsed_corpus,
  PG_table      = all_tables_PG,
  min_pp        = 0,
  min_map       = 0.01,
  max_options   = 1e6)$scores_gp
reach_readings <- reach_readings[order(-reach_readings$mean), ]
head(reach_readings[, c("pronunciation", "mean", "pp")], 10)

# We can also see if any of the generated possible pronunciations for "reach"
# under the constraints above happen to be real pronunciations present
# in the lexicon:
intersect(reach_readings$pronunciation, wordlist_v2_1_merged$pronunciation)

# ===========================================================================
# Additional Examples
# ===========================================================================

# Calling pw_read for multiple grain sizes at once:
pw_read(
  target        = "sebe",
  level         = "all",
  parsed_corpus = parsed_corpus,
  PG_table      = all_tables_PG,
  OC_table      = all_tables_OC,
  OR_table      = all_tables_OR)

# Set min_pp to 0 to compute for all parses
pw_read(
  target        = "floke",
  parsed_corpus = parsed_corpus,
  PG_table      = all_tables_PG,
  min_pp        = 0)

# As with pw_spell, ONC mode separates statistics by onset, nucleus, and coda
pw_read(
  target        = "floke",
  parsed_corpus = parsed_corpus,
  PG_table      = all_tables_PG,
  min_map       = 0.01,
  summary_mode  = "ONC")

# pp2 (the new parsing probability measure) can also be used as the parsing
# probability indicator
pw_read(
  target        = "bluise",
  parsed_corpus = parsed_corpus,
  PG_table      = all_tables_PG,
  pp_param      = "pp2")
