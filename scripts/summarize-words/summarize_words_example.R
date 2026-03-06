# summarize_words Usage Examples
# Demonstrates the default and ONC modes of summarize_words.
#
# summarize_words computes summary statistics (mean, median, max, min, sd)
# for a chosen measure across all units in a word.
#
# In default mode, statistics are computed across all units.
# In ONC mode, statistics are broken down by syllabic position:
#   onset, nucleus, and coda.

# setwd("replace/with/path/to/summarize-words")
load("../../Toolkit_v2.1.RData")

# ===========================================================================
# Default Mode
# ===========================================================================

# Summarize spelling consistency (PG) for a single word:
summarize_words(
  mapped_words = scored_words_PG[
    which(wordlist_v2_1_merged$spelling == "toolkit")],
  parameter = "PG")

# Summarize reading consistency (GP) for a single word:
summarize_words(
  mapped_words = scored_words_PG[
    which(wordlist_v2_1_merged$spelling == "toolkit")],
  parameter = "GP")

# Summarize across the entire corpus:
all_words_PG_probability <- summarize_words(
  mapped_words = scored_words_PG,
  parameter = "PG")
head(all_words_PG_probability)

# ===========================================================================
# ONC Mode (New in v2.1)
# ===========================================================================

# In ONC mode, summarize_words breaks down the measure by syllabic position.
# Instead of a single set of statistics across all units, you get separate
# statistics for onset (O), nucleus (N), and coda (C) units.
#
# Output columns:
#   mean_onset, median_onset, max_onset, min_onset, sd_onset,
#   mean_nucleus, median_nucleus, max_nucleus, min_nucleus, sd_nucleus,
#   mean_coda, median_coda, max_coda, min_coda, sd_coda

# Single word — ONC breakdown for "penguin":
summarize_words(
  mapped_words = scored_words_PG[
    which(wordlist_v2_1_merged$spelling == "penguin")],
  parameter = "PG",
  mode = "ONC")

# Compare default vs. ONC for the same word:
summarize_words(
  mapped_words = scored_words_PG[
    which(wordlist_v2_1_merged$spelling == "penguin")],
  parameter = "PG")

summarize_words(
  mapped_words = scored_words_PG[
    which(wordlist_v2_1_merged$spelling == "penguin")],
  parameter = "PG",
  mode = "ONC")

# ONC mode across the full corpus:
all_words_PG_onc <- summarize_words(
  mapped_words = scored_words_PG,
  parameter = "PG",
  mode = "ONC")
head(all_words_PG_onc)
