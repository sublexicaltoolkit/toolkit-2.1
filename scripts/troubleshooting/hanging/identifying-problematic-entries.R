# TODO update with toolkit guide section
#
# Sometimes, a batch processing call will hang instead of erroring if some
# spelling/pronunciation pairs cannot be mapped. If this is the case, make the
# calls one at a time in an explicit loop. Note that this hanging can be the
# result of an error or an NA return value, which each occur under different
# circumstances (see below).
# The following script can be used as a direct replacement to a batch call
# to map_value. Replace the following with yours:
# my_pseudowords
# The level and relevant tables in the map_value call on line 39

# setwd("<path to scripts/troubleshooting/hanging>")
load("../../../Toolkit_v2.1.RData")

# In this exmaple, the last two pairs cannot be mapped. "cat" to /dag/ will
# cause an error, and "floke" as /fl0k/ does not work because "0" is not a 
# Toolkit-recognized phoneme, and this is a case where NA would be thrown
my_pseudowords <- data.frame(
    spell = c("floke", "blafe", "cat", "floke"),
    pron  = c("flok", "bl8f", "dag", "fl0k"),
    stringsAsFactors = FALSE
)

# Data structure to collect all problematic entries:
error_df <- data.frame(
    spell = character(),
    pron = character(),
    stringsAsFactors = FALSE
)

# Iterate over the dataset
for (i in 1:nrow(my_pseudowords)) {
    s <- my_pseudowords$spell[i]
    p <- my_pseudowords$pron[i]
    # This tryCatch statement will collect all errors into the error_df for you
    # to examine
    summary <- tryCatch(
        summarize_words(map_value(s, p, "ONC", all_tables_ONC), "PG"),
        error = function(e) NULL
    )
    if (is.null(summary) || is.na(summary$mean)) {
        error_df <- rbind(error_df, data.frame(spell = s, pron = p))
    }
}

error_df
