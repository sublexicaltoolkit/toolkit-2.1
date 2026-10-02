# TODO add toolkit guide section
# 
# word_pattern returns every word in mapped_words containing the supplied phoneme,
# spelled with the supplied grapheme, at the given position.
# Positions: "wi", "si", "sm", "sf", "wf" for word and syllable initial, medial, and final.
# Alternatively, any single parameter (phoneme, grapheme, or position) can be
# supplied with "any" to lift the restriction on that parameter.
# Usage: word_pattern(scored_words_<grain>, p, g, pos)

# setwd("<path to scripts/word-pattern>")
load("../../Toolkit_v2.1.RData")

# Obtain all words  beginning with the grapheme "ho", pronounced any way:
word_pattern(scored_words_PG, "any", "ho", "wi")

# Obtain all words where /f/ is spelled "ph" in any position:
f_ph <- word_pattern(scored_words_PG, "f", "ph", "any")
# Get the number of words that matched the filter, and look at the first few
length(f_ph)
head(f_ph)

# Obtain all words with phoneme /S/ at the start of the word, regardless of its spelling:
S_initial <- word_pattern(scored_words_PG, "S", "any", "wi")
head(S_initial)

# Other grain sizes: words ending in the orthographic rime "a_e" + "n", as in "cane":
word_pattern(scored_words_OR, "any", "a_en", "wf")
