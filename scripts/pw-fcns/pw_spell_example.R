# TODO associated section of toolkit guide 
# 
# pw_spell generates possible spellings for a given pronunciation, according to
# the Toolkit's rules.
#
# min_map = 1 (the default) returns only the most probable spelling(s). This
# value can be set between 0 and 1 inclusive to set a lower bound on spelling
# probability, with 0 accepting all possible spellings.

# setwd("<path to scripts/pw-fcns>")
load("../../Toolkit_v2.1.RData")

# ===========================================================================
# Common Usage
# ===========================================================================

# Obtain the  most likely spelling of the /sib/ pseudoword:
pw_spell(target = "sib", PG_table = all_tables_PG)

# With min_map set to 0.01 we instead now obtain all possible spellings
# above a 1% probability:
sib_spellings <- pw_spell(target = "sib", PG_table = all_tables_PG, min_map = 0.01)$scores_pg
# View the first few in reverse order of mean probability (highest first):
head(sib_spellings[order(-sib_spellings$mean), ])

# Alternatively, we can just obtain the spellings themselves without computing
# the associated probabilities by setting score=FALSE
head(pw_spell(target = "sib", PG_table = all_tables_PG, min_map = 0.01, score = FALSE))

# ===========================================================================
# Calling pw_spell with Lists
# ===========================================================================

# pw_spell can be supplied a vector of pronunciations to process multiple words
# at one; passing in wordlistv2_merged$pronunciation for instance would run the
# function on the entire lexicon
pw_spell(target = c("fl5t", "sib", "gr8t"), PG_table = all_tables_PG)

# ===========================================================================
# Real vs. Pseudowords
# ===========================================================================

# pw_spell can be called for both pronunciations of real words as well as
# pseudowords
pw_spell(target = "k@t", PG_table = all_tables_PG)

pw_spell(target = "blEf", PG_table = all_tables_PG)

# ===========================================================================
# All pw_spell Parameters
# ===========================================================================

# Here, we obtain all spellings for "fl5k", at the OC level, scoring the results
# and forcing a minimum probability (of the phoneme->grapheme mappings) of 0.05.
# We set an upper bound on the number of produced spellings to 500, where the
# the mean GP consistency is set to at least 0.5.
pw_spell(
  target        = "fl5k",
  level         = "OC",
  PG_table      = all_tables_PG,
  OC_table      = all_tables_OC,
  OR_table      = all_tables_OR,
  score         = TRUE,
  min_map       = 0.05,
  max_options   = 500,
  param         = "GP",
  mean_score    = 0.5,
  parsed_corpus = parsed_corpus,
  summary_mode  = "default")

# ===========================================================================
# Additional Examples
# ===========================================================================

# Spellings differ depending on the grain size used to generate
pw_spell(target = "sib", level = "OC", OC_table = all_tables_OC)
pw_spell(target = "sib", level = "OR", OR_table = all_tables_OR)

# Call for all levels at once
pw_spell(
  target   = "sib",
  level    = "all",
  PG_table = all_tables_PG,
  OC_table = all_tables_OC,
  OR_table = all_tables_OR)

# Parsing probability measures can also be used for scoring
pw_spell(
  target        = "fl5k",
  level         = "OC",
  PG_table      = all_tables_PG,
  OC_table      = all_tables_OC,
  min_map       = 0.05,
  param         = "pp",
  parsed_corpus = parsed_corpus)

# In ONC mode, consistency is defined for the onset, nucleus, and coda units
# separately:
pw_spell(
  target       = "sib",
  PG_table     = all_tables_PG,
  min_map      = 0.01,
  summary_mode = "ONC")
