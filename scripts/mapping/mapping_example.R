# TODO add toolkit guide section
#
# This script illustrates basic use of map_<grain>, the set of functions used
# to map phonemes and graphemes at any of the supported grain sizes (PG, OR,
# OC, ONC).

# setwd("<path to scripts/mapping>")
load("../../Toolkit_v2.1.RData")

# Before using the functions, apply replace_accented_vowels to the lexicon, as
# accents are not supported. This is useful especially when pulling from a large
# dataset when such vowels may or may not have been replaced already in the
# spelling. For instance, this preprocessing would need to be done on words
# like this: 
replace_accented_vowels(c("café", "naïve", "résumé"))

# Every map_* function returns a list of two elements:
#   [[1]] a matrix with the phonemes, graphemes, and position codes of each unit
#         (1 = word-initial, 2 = syllable-initial, 3 = syllable-medial, 4 = syllable-final,
#         5 = word-final)
#   [[2]] TRUE if the word was successfully mapped, FALSE otherwise
# For example, mapping "toolkit" at the PG level using the Toolkit's 
# representation of the pronunciation:
toolkit_PG <- map_PG("toolkit", "tUlk1t")
toolkit_PG[[1]][[1]]
toolkit_PG[[2]]

# Now mapping the word at the other supported grain sizes:
map_ONC("toolkit", "tUlk1t")[[1]][[1]]
map_OC("toolkit", "tUlk1t")[[1]][[1]]
map_OR("toolkit", "tUlk1t")[[1]][[1]]

# The following is an example where we attempt to map a list of spellings and
# pronunciations at once. In this case, the "cat" spelling is aligned with the
# "dag" pronunciation (the Toolkit representation for the pronunciation "dog").
# Since this mapping is invalid the Toolkit is unable to process it. The final
# two lines illustrate what the expected behavior looks like for such a case
# from the Toolkit:
mapped <- map_PG(c("penguin", "cat", "phone"), c("pENgw1n", "dag", "fon"))
mapped[[2]]
mapped[[1]][[2]]
