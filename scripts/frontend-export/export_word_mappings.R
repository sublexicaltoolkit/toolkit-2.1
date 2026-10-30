# setwd("Replace this string with the path to the frontend-export directory of this repo and uncomment")
# The Toolkit only needs to be loaded once
load("../../Toolkit_v2.0.RData")

# Exports each word's unit-by-unit mapping for the website's Phonology & Orthography
# results page (syllable breakdown, onset/rime sorting, and "which min").
#
# One row per spelling + pronunciation. For each grain size, the units are joined with "|":
#   <grain>_phonemes   h|E|l|o
#   <grain>_graphemes  h|e|ll|o
#   <grain>_positions  wi|sf|si|wf   (word-initial, syllable-initial/medial/final, word-final)
#   <grain>_PG, _GP, _PG_freq, _P_freq, _G_freq   per-unit values from the scored_words datasets
# Words the toolkit could not map (PG_accuracy == FALSE) get empty cells.
#
# Output: ../../data/precalculated/word_mappings.csv
# Run from this directory with: Rscript export_word_mappings.R

# Keys are grain sizes, values are the scored datasets to read (type weighted, positional)
grain_sizes <- list(
  "PG" = "scored_words_PG",
  "OR" = "scored_words_OR"
)

value_rows <- c("PG", "GP", "PG_freq", "P_freq", "G_freq")
unit_rows <- c("phoneme", "grapheme", "position", value_rows)
unit_names <- c("phonemes", "graphemes", "positions", value_rows)

# Turns one scored word (a data frame with one column per unit) into a named vector of joined strings
flatten_word <- function(word, grain) {
  out <- setNames(rep("", length(unit_rows)), paste0(grain, "_", unit_names))
  if (!isTRUE(as.logical(word["PG_accuracy", 1]))) return(out)

  units <- word[, !is.na(unlist(word["phoneme", ])), drop = FALSE]
  for (i in seq_along(unit_rows)) {
    values <- unlist(units[unit_rows[i], ])
    values[is.na(values)] <- ""
    out[i] <- paste(values, collapse = "|")
  }
  out
}

export <- NULL
for (grain in names(grain_sizes)) {
  scored <- get(grain_sizes[[grain]])
  rows <- t(vapply(scored, flatten_word, character(length(unit_rows)), grain = grain))
  keys <- data.frame(
    spelling = vapply(scored, function(w) w["spelling", 1], ""),
    pronunciation = vapply(scored, function(w) w["pronunciation", 1], ""),
    stringsAsFactors = FALSE
  )
  grain_df <- cbind(keys, as.data.frame(rows, stringsAsFactors = FALSE))
  grain_df <- grain_df[!duplicated(grain_df[, c("spelling", "pronunciation")]), ]

  export <- if (is.null(export)) grain_df else merge(export, grain_df, by = c("spelling", "pronunciation"), all = TRUE, sort = FALSE)
}

write.csv(export, "../../data/precalculated/word_mappings.csv", row.names = FALSE)
cat("Wrote", nrow(export), "words\n")
