# This script contains the tools and functions necessary to generate a spanning table of all
# Toolkit units for a corpus of words. An example using the Toolkit's preprocessed datasets
# is shown.
load("Toolkit_v2.1.RData")

### PRELIMINARIES ###

library(dplyr)
library(purrr)
library(tidyr)
library(openxlsx)

# Transcription conversion tools if Toolkit -> IPA is desired
inhousechars <- c("5", "O", "8", "o", "2", "Er", "1r", "Ur", "C", "T", "D", "G", "i", "3r", "a", "c", "u", "U", "1", "@", "E", "e", "^", "b", "d", "f", "g", "h", "j", "k", "l", "m", "n", "N", "p", "r", "s", "S", "t", "v", "w", "z", "Z")
ipachars <- c("aɪ", "aʊ", "eɪ", "oʊ", "ɔɪ", "ɛɹ", "ɪɹ", "ʊɹ", "tʃ", "θ", "ð", "dʒ", "i", "ɚ", "ɑ", "ɔ", "u", "ʊ", "ɪ", "æ", "ɛ", "ə", "ʌ", "b", "d", "f", "g", "h", "j", "k", "l", "m", "n", "ŋ", "p", "ɹ", "s", "ʃ", "t", "v", "w", "z", "ʒ")
inhouse_to_ipa <- function(word) {
  output <- ""
  
  i <- 1
  while (i <= nchar(word)) {
    matched <- FALSE
    
    for (j in 1:length(inhousechars)) {
      inhousekey <- inhousechars[j]
      ipakey <- ipachars[j]
      
      if (substr(word, i, i + nchar(inhousekey) - 1) == inhousekey) {
        output <- paste0(output, ipakey)
        i <- i + nchar(inhousekey)
        matched <- TRUE
        break
      }
    }
    
    if (!matched) {
      output <- paste0(output, substr(word, i, i))
      i <- i + 1
    }
  }
  
  return(output)
}
ipa_to_inhouse <- function(word) {
  output <- ""
  
  i <- 1
  while (i <= nchar(word)) {
    matched <- FALSE
    
    for (j in 1:length(ipachars)) {
      ipakey <- ipachars[j]
      inhousekey <- inhousechars[j]
      
      if (substr(word, i, i + nchar(ipakey) - 1) == ipakey) {
        output <- paste0(output, inhousekey)
        i <- i + nchar(ipakey)
        matched <- TRUE
        break
      }
    }
    
    if (!matched) {
      output <- paste0(output, substr(word, i, i))
      i <- i + 1
    }
  }
  
  return(output)
}

# Functions for phoneme-grapheme pair extraction at a specific grain size, given a wordlist with columns spelling, 
# pronunciation, and syllables.
    # scored_words: a dataset of the scored_words family (output from the Toolkit map_value function)
    # spellings: an optional list of spellings, of the same length as the scored_words dataset supplied.
    #            used for cross-verification of spellings.
    # col_prefix: prefix to be used for column naming for a given grain size, corresponding to the grain size of evaluation
    #             acceptable entries: PG, OR, OC, ONC, SY (syllable)
    # toolkit_to_ipa: TRUE if the Toolkit encoding of the phonemes should be converted to IPA in the output.
extract_pg_pairs <- function(scored_words, spellings=NULL, col_prefix="PG", toolkit_to_ipa=FALSE) {
    # vowels and consonants for labeling of unit types
    vowels <- c(
        "5", "O", "8", "je", "j3r", "ju", "jU", "o", "2",
        "@", "a", "e", "E", "3r", "i", "1", "c", "u", "U", "^",
                            "3") # 3 added for the purposes of ONC classification
    consonants <- c("G", "gz", "kS", "nj", "C", "T", "D", "Z", "N",
                        "b", "d", "f", "g", "h", "j", "k", "l", "m", "n", "p",
                        "r", "s", "S", "t", "v", "w", "z")
    vowels_sorted <- vowels[order(-nchar(vowels))]
    consonants_sorted <- consonants[order(-nchar(consonants))]

    # subfunction to classify units according to whether or not they belong to an onset or a rime
    or_classifier <- function(phoneme) {
        while (phoneme != "") {
            for (v in vowels_sorted) {
                if (phoneme == v) {
                    return("r")
                }
            }
            for (c in consonants_sorted) {
                if (phoneme == c) {
                    return("o")
                }
            }
            phoneme <- substr(phoneme, 1, nchar(phoneme) - 1)
        }
        return(NA)
    }

    # subfunction for classification of onset/nucleus/coda for ONC case
    onc_classifier <- function(p_list, syllables) {
        # classify each phoneme using the OR classifier, since this tells us if we have a 
        # consonant or a vowel sound for the ONC unit
        orc <- lapply(p_list, or_classifier)

        results <- list()

        cur_syll_idx <- 1
        cur_syll <- syllables[[cur_syll_idx]]
        chars_consumed <- 0
        syll_has_onset <- FALSE
        syll_has_nucleus <- FALSE

        for (i in seq_along(p_list)) {
            phoneme <- p_list[[i]]
            classification <- orc[[i]]

            # check if current syllable done
            if (chars_consumed >= nchar(cur_syll)) {
                cur_syll_idx <- cur_syll_idx + 1
                if (cur_syll_idx <= length(syllables)) {
                    cur_syll <- syllables[[cur_syll_idx]]
                    chars_consumed <- 0
                    syll_has_onset <- FALSE
                    syll_has_nucleus <- FALSE
                }
            }

            if (classification == "o") {
                # if this syllable already has an onset then this must be the coda
                if (syll_has_onset) {
                    results[[length(results) + 1]] <- "c"
                } else {
                    # again if no onset but we already saw a nucleus this must be the coda
                    if (syll_has_nucleus) {
                        results[[length(results) + 1]] <- "c"
                    } else{
                        results[[length(results) + 1]] <- "o"
                        syll_has_onset <- TRUE
                    }
                }
            } else {
                # vowel sounds will always be nuclei (start of a rime)
                results[[length(results) + 1]] <- "n"
                syll_has_nucleus <- TRUE
            }
            
            chars_consumed <- chars_consumed + nchar(phoneme)
        }

        return(results)
    }

    # First pass: determining number of max pairs
    max_pairs <- 0
    all_pairs <- list()
    all_inhouse_phonemes <- list()
    all_graphemes <- list()

    
}

# Extracts units for the words provided at the desired level (PG, OR, OC, ONC, SY (syllable)). Handles scored_words dataset generation under
# the hood (extract_pg_pairs does not need to be interfaced with directly given a wordlist with spelling and pronunciation
# columns)
extract_units_at_level <- function(wordlist, level) {
    all_words_dataset <- switch(level,
        PG=map_PG(wordlist$spelling, wordlist$pronunciation),
        OR=map_OR(wordlist$spelling, wordlist$pronunciation),
        OC=map_OC(wordlist$spelling, wordlist$pronunciation),
        ONC=)
}

### EXECUTION ###

# Replace this with your own wordlist. If supplying your own wordlist, the functions below assume
# your wordlist has columns "spelling", "pronunciation", "freq", and "syllables", all of equal
# length.
wordlist <- wordlist_v2_1_merged

