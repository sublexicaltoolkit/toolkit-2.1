library(stringdist)

# Must have the 2.1 toolkit environment loaded
# load("path/to/Toolkit_v2.1.RData")

# Function from Bob to compute edit-distance matrices between
# two words
edit_distance_matrices <- function(mat1, mat2) {
  # Convert matrices to character vectors
  vec1 <- as.character(as.vector(mat1))
  vec2 <- as.character(as.vector(mat2))
  
  # Initialize the distance matrix
  n <- length(vec1)
  m <- length(vec2)
  dist_matrix <- matrix(0, n + 1, m + 1)
  
  # Fill the base cases
  for (i in 1:(n + 1)) dist_matrix[i, 1] <- i - 1
  for (j in 1:(m + 1)) dist_matrix[1, j] <- j - 1
  
  # Compute the distances
  for (i in 2:(n + 1)) {
    for (j in 2:(m + 1)) {
      cost <- ifelse(vec1[i - 1] == vec2[j - 1], 0, 1)
      dist_matrix[i, j] <- min(
        dist_matrix[i - 1, j] + 1,      # deletion
        dist_matrix[i, j - 1] + 1,      # insertion
        dist_matrix[i - 1, j - 1] + cost # substitution
      )
    }
  }
  
  # Return the distance
  dist_matrix[n + 1, m + 1]
}

# Pronunciations required if not letter distance
# If provided must be at least one valid pronunciation per item
get_orthographic_neighbors <- function(item_spellings, item_pronunciations, type="letter", maxdist=1) {
    nitems <- length(item_spellings)
    neighbors <- list()

    if (type == "letter"){
        for(j in 1:nitems){
        mydists <- stringdist(item_spellings[j],wordlist_v2_1_merged$spelling) #find letter distance between the target and every real word in the v2.0 list
        neighbors[[j]] <- wordlist_v2_1_merged[which(mydists<=maxdist),] #select just the real words that are within the maximum allowed distance
        }
    } else {
        level_table <- switch(type,
                                PG = all_words_PG,
                                OR = all_words_OR,
                                OC = all_words_OC,
                                ONC = all_words_ONC)
        level_func <- switch(type,
                                PG = map_PG,
                                OR = map_OR,
                                OC = map_OC,
                                ONC = map_ONC)

        for(j in 1:nitems){
            mygraphemes1 <- NA
            mydists <- matrix(NA, length(level_table[[1]])) 
            
            tryCatch({mygraphemes1 <- level_func(spelling=item_spellings[j], pronunciation=item_pronunciations[j])[[1]][[1]][2,]},error=function(e)NA)

            for(i in 1:length(level_table[[1]])){ #for each word in the version 2.0 list
                lexicongraphemes <- level_table[[1]][[i]][2,] #extract graphemes
                mydists[i] <- edit_distance_matrices(mygraphemes1,lexicongraphemes) #compute the distance between the target's graphemes and this word's graphemes
            }
            neighbors[[j]] <- wordlist_v2_1_merged[which(mydists<=maxdist),]  #select just the real words that are within the maximum allowed distance
        }
    }
    return(neighbors)
}

# Defined for each phonemic level. Specify as "PG", "OC", "OR", "ONC"
get_phonological_neighbors <- function(item_spellings, item_pronunciations, type, maxdist=1) {
    nitems <- length(item_spellings)
    neighbors <- list()
    level_table <- switch(type,
                            PG = all_words_PG,
                            OR = all_words_OR,
                            OC = all_words_OC,
                            ONC = all_words_ONC)
    level_func <- switch(type,
                            PG = map_PG,
                            OR = map_OR,
                            OC = map_OC,
                            ONC = map_ONC)

    for(j in 1:nitems){
        myphonemes1 <- NA
        mydists <- matrix(NA, length(level_table[[1]]))

        tryCatch({myphonemes1 <- level_func(spelling=item_spellings[j], pronunciation=item_pronunciations[j])[[1]][[1]][1,]},error=function(e)NA)

        for(i in 1:length(level_table[[1]])){ #for each word in the version 2.0 list
            lexiconphonemes <- level_table[[1]][[i]][1,] # extract phonemes
            mydists[i] <- edit_distance_matrices(myphonemes1,lexiconphonemes) #compute the distance between the target's phonemes and phonemes of this word
        }
        neighbors[[j]] <- wordlist_v2_1_merged[which(mydists<=maxdist),]  #select just the real words that are within the maximum allowed distance
    }

    return(neighbors)
}

# Assume words has a spelling column and neighbors_df is a list of dataframes containing spelling and
# pronunciation columns corresponding to all respective neighbors of words in words in the same order.
# Produces a dataframe whose columns are "target", "neighbor_spelling", and "neighbor_pronunciation",
# with all neighbors. Removes duplicate entries (where the value of target is the same as the value
# of neighbor_pronunciation) as the above functions are inclusive of the zero-distance neighbor.
create_word_neighbor_df <- function(words, ndf) {
    df <- data.frame(
        target = character(),
        neighbor_spelling = character(),
        neighbor_pronunciation = character(),
        stringsAsFactors = FALSE
    )

    for (i in seq_along(ndf)) {
        target_word <- words$spelling[i]
        neighbors_df <- ndf[[i]]

        neighbors_df <- neighbors_df %>%
            select(spelling, pronunciation) %>%
            mutate(target = target_word) %>%
            select(target, spelling, pronunciation)

        colnames(neighbors_df) <- c("target", "neighbor_spelling", "neighbor_pronunciation")

        df <- bind_rows(df, neighbors_df)
    }

    # get_orthographic_neighbors returns neighbors <= distance, so duplicates are included in the output
    df <- df %>%
        filter(target != neighbor_spelling)

    return(df)
}

# Examples (from Bob, modified to work with these functions
# Uncomment any of the following

items <- c("blease","floke")
prons <- c("blis","flok")

# # By default, graphemic neighbors are at the letter level.

# letter_neighbors <- get_orthographic_neighbors(items, prons)

# # But we can also get the neighbors for any other unit

# pg_neighbors <- get_orthographic_neighbors(items, prons, "PG", 1)
# oc_neighbors <- get_orthographic_neighbors(items, prons, "OC", 1)
# or_neighbors <- get_orthographic_neighbors(items, prons, "OR", 1)
# onc_neighbors <- get_orthographic_neighbors(items, prons, "ONC", 1)

# # And we can also obtain the phonemic neighbors at any level

# pg_phoneme_neighbors <- get_phonological_neighbors(items, prons, "PG", 1)
# oc_phoneme_neighbors <- get_phonological_neighbors(items, prons, "OC", 1)
# or_phoneme_neighbors <- get_phonological_neighbors(items, prons, "OR", 1)
# onc_phoneme_neighbors <- get_phonological_neighbors(items, prons, "ONC", 1)