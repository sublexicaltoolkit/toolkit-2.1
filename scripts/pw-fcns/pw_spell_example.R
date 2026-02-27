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

# --- Most Likely Spelling ---
# Get the most likely spelling for /sib/ at PG level:
pw_spell(target = "sib", level = "PG", PG_table = all_tables_PG, min_map = 1)

# --- All Spellings Above a Threshold ---
# All spellings for /sib/ at PG level with at least 1% spelling probability:
pw_spell(target = "sib", level = "PG", PG_table = all_tables_PG, min_map = 0.01, score = FALSE)

# --- OC Level ---
# Most likely spelling at OC level:
pw_spell(target = "sib", level = "OC", OC_table = all_tables_OC, min_map = 1)

# --- OR Level ---
# Most likely spelling at OR level:
pw_spell(target = "sib", level = "OR", OR_table = all_tables_OR, min_map = 1)

# --- Most Likely at All Levels ---
# Most likely spellings for /sib/ at all levels:
pw_spell(
  target   = "sib",
  level    = "all",
  PG_table = all_tables_PG,
  OC_table = all_tables_OC,
  OR_table = all_tables_OR,
  min_map  = 1)
