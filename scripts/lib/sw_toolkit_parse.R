# Parsing code for the sliding window (scripts/sliding-window)
# sw_parse and sw_spell_parse are largely identical to pw_read and pw_spell,
# but instead capture the parses directly, and the unit options at each
# position for each parse, which are not otherwise returned by those functions
# and are required for the sliding window.
# The remaining components of those functions are removed so that computations
# not required by the sliding window algorithm are not performed (i.e. no
# Cartesian product, etc.)
# This code used for parsing is from the 2.1 version of the Toolkit.

library(stringr)
library(data.tree)

# Parsing measures for each parse; pp_freq and pp2_freq are the token-weighted pp and pp2
# (each letter weighted by log10(word frequency + 1)).
MEASURES <- c("pp", "pp2", "pp_freq", "pp2_freq")

.sw_pp2_context <- function(allbranches_df, spelling,
                             branch_col = "branch_no",
                             grapheme_col = "branch_grapheme_no") {
  required <- c(
    "Letter_Occurrence", "index2", "reduced",
    branch_col, grapheme_col
  )
  missing <- setdiff(required, names(allbranches_df))
  if (length(missing) > 0L) {
    stop("Cannot construct PP2 context; missing columns: ", paste(missing, collapse = ", "))
  }

  occurrence_rows <- allbranches_df[
    !duplicated(allbranches_df$Letter_Occurrence),
    c("Letter_Occurrence", "reduced"),
    drop = FALSE
  ]
  if (nrow(occurrence_rows) == 0L) {
    return(data.frame(
      Letter_Occurrence = character(0),
      string_position = integer(0),
      pp2_component = integer(0),
      pp2_role = integer(0),
      pp2_signature = character(0),
      stringsAsFactors = FALSE
    ))
  }

  spelling_letters <- strsplit(toupper(spelling), "", fixed = TRUE)[[1]]
  occurrence_position <- function(x) {
    letter <- sub("[0-9]+$", "", x)
    occurrence <- suppressWarnings(as.integer(sub("^[^0-9]+", "", x)))
    positions <- which(spelling_letters == letter)
    if (!is.finite(occurrence) || occurrence < 1L || occurrence > length(positions)) {
      return(NA_integer_)
    }
    positions[occurrence]
  }
  occurrence_rows$string_position <- vapply(
    occurrence_rows$Letter_Occurrence,
    occurrence_position,
    integer(1)
  )

  n_occ <- nrow(occurrence_rows)
  parent <- seq_len(n_occ)
  find_root <- function(i) {
    while (parent[i] != i) {
      parent[i] <<- parent[parent[i]]
      i <- parent[i]
    }
    i
  }
  union_nodes <- function(i, j) {
    ri <- find_root(i)
    rj <- find_root(j)
    if (ri != rj) parent[rj] <<- ri
  }

  reduced_contains <- function(reduced_value, candidate) {
    if (is.na(reduced_value) || !nzchar(reduced_value)) return(FALSE)
    candidate %in% strsplit(reduced_value, ",", fixed = TRUE)[[1]]
  }

  group_key <- paste(
    allbranches_df[[branch_col]],
    allbranches_df[[grapheme_col]],
    sep = "\r"
  )
  branch_graphemes <- split(seq_len(nrow(allbranches_df)), group_key)
  occurrence_index <- setNames(seq_len(n_occ), occurrence_rows$Letter_Occurrence)

  for (rows in branch_graphemes) {
    group <- allbranches_df[rows, , drop = FALSE]
    group <- group[!duplicated(group$Letter_Occurrence), , drop = FALSE]
    if (nrow(group) < 2L) next

    candidates <- unique(as.character(group$index2))
    candidates <- candidates[!is.na(candidates)]
    if (length(candidates) != 1L) next
    candidate <- candidates[1]

    survives <- mapply(
      reduced_contains,
      as.character(group$reduced),
      MoreArgs = list(candidate = candidate),
      USE.NAMES = FALSE
    )
    if (!all(survives)) next

    nodes <- unname(occurrence_index[as.character(group$Letter_Occurrence)])
    nodes <- nodes[is.finite(nodes)]
    if (length(nodes) > 1L) {
      for (j in nodes[-1]) union_nodes(nodes[1], j)
    }
  }

  roots <- vapply(seq_len(n_occ), find_root, integer(1))
  component_order <- order(
    vapply(unique(roots), function(r) {
      min(occurrence_rows$string_position[roots == r], na.rm = TRUE)
    }, numeric(1))
  )
  ordered_roots <- unique(roots)[component_order]
  component_id <- match(roots, ordered_roots)

  component_signature <- vapply(seq_len(n_occ), function(i) {
    members <- which(component_id == component_id[i])
    members <- members[order(
      occurrence_rows$string_position[members],
      occurrence_rows$Letter_Occurrence[members],
      na.last = TRUE
    )]
    paste0(
      sub("[0-9]+$", "", occurrence_rows$Letter_Occurrence[members]),
      "{", occurrence_rows$reduced[members], "}",
      collapse = ">"
    )
  }, character(1))

  # Identify the focal member within its ordered component. Letter identity is
  # insufficient when a component contains the same letter more than once
  # (e.g., the two Es in LEAGUE).
  component_role <- vapply(seq_len(n_occ), function(i) {
    members <- which(component_id == component_id[i])
    members <- members[order(
      occurrence_rows$string_position[members],
      occurrence_rows$Letter_Occurrence[members],
      na.last = TRUE
    )]
    match(i, members)
  }, integer(1))

  data.frame(
    Letter_Occurrence = occurrence_rows$Letter_Occurrence,
    string_position = occurrence_rows$string_position,
    pp2_component = component_id,
    pp2_role = component_role,
    pp2_signature = component_signature,
    stringsAsFactors = FALSE
  )
}

# Returns a list of parses and their associated units and options per unit
sw_parse <- function(target, min_pp = 0) {
  target <- as.character(target)
  if (!grepl("^[A-Za-z]+$", target)) stop("A spelling must contain letters only: ", target)

  ## 2. Build internal mapping objects (LOCAL to pw_read)
  #create within-function lists of mapping options: (1) merge si/sm/sf into a single 'internal mappings'; and (2) convert graphemes to uppercase as needed for data.tree
  internal_mappings <- rbind(syllable_initial_mappings,syllable_medial_mappings,syllable_final_mappings)
  parse_corpus_word_initial_mappings <- word_initial_mappings
  parse_corpus_word_final_mappings <- word_final_mappings
  
  #word final must include final W and H even though they are illegal--any parses that end in a final -H or -W will be pruned; this adds those to the end of the list
  parse_corpus_word_final_mappings[nrow(parse_corpus_word_final_mappings)+1,] <- parse_corpus_word_final_mappings[nrow(parse_corpus_word_final_mappings),]
  parse_corpus_word_final_mappings[nrow(parse_corpus_word_final_mappings),]$grapheme <- "h"
  parse_corpus_word_final_mappings[nrow(parse_corpus_word_final_mappings),]$phoneme <- "h"
  parse_corpus_word_final_mappings[nrow(parse_corpus_word_final_mappings)+1,] <- parse_corpus_word_final_mappings[nrow(parse_corpus_word_final_mappings),]
  parse_corpus_word_final_mappings[nrow(parse_corpus_word_final_mappings),]$grapheme <- "w"
  parse_corpus_word_final_mappings[nrow(parse_corpus_word_final_mappings),]$phoneme <- "w"
  
  #must do in UPPERCASE, because data.tree reserves "count" for internal purposes, so any word with that string in it (like "account") will not parse
  #but it allows you to process them in uppercase (i.e., "count" is reserved but "COUNT" is not )
  parse_corpus_word_initial_mappings$grapheme <- toupper(parse_corpus_word_initial_mappings$grapheme)
  internal_mappings$grapheme <- toupper(internal_mappings$grapheme)
  parse_corpus_word_final_mappings$grapheme <- toupper(parse_corpus_word_final_mappings$grapheme)

  #extract needed values from parsed_corpus:
  parsing_table <- parsed_corpus[[1]]
  reduced_table <- parsed_corpus[[2]]
  corpus_parsed_table <- parsed_corpus[[3]]

  ## 3. Define internal functions.
  #internal function: for parsing strings of letters into potential graphemes--critical function!
  parse_string <- function(startword){
    
    consonants <- c("b","c","d","f","g","h","j","k","l","m","n","p","q","r","s","t","v","x","z")
    vowels <- c("a|e|i|o|u|y") #including Y because it can literally be the only vowel, like in MYTH, but not W because it never is ALONE as the vowel (always OW or AW, etc.)
    
    #consonants will be removed to search for _E mappings--note that this DOES exclude W in addition to Y, because W can be part of _E mappings as in OWE
    remove_consonants <- c("B"="_","C"="_","D"="_","F"="_","G"="_","H"="_","J"="_","K"="_","L"="_","M"="_","N"="_","P"="_","Q"="_","R"="_","S"="_","T"="_","V"="_","X"="_","Z"="_")
    
    #need custom internal functions to handle the _E's (e.g., in ACED given A_E leave behind)
    remove_first <- function(word, char) {
      sub(char, "", word, fixed = TRUE)
    } #this one creates the correct lefttree leaves when there's a _E (e.g., ACED becomes CD)
    process_string <- function(string, word) {
      # Step 1: Extract characters before the underscore
      before_underscore <- sub("_(.*)", "", string)
      
      # Step 2: Remove the identified prefix from the word
      remaining_word <- sub(paste0("^", before_underscore), "", word)
      
      # Step 3: Find the first occurrence of 'E' in 'remaining_word' and return what's after it
      after_e <- sub("^.*?E", "", remaining_word)
      
      return(after_e)
    } #this one creates whats needed to check if those options are valid (e.g., ACED becomes D)
    prune_tree <- function(node) {
      #store the list of child nodes pre-pruning, because this may change during the for loop
      children_to_check <- node$children
      
      for (child in children_to_check) {
        #check each node that is a leaf--this is because we're just looking at the illegal word-final graphemes
        if (child$isLeaf) {
          #if the grapheme is in this list (W or H), remove the entire branch
          if (child$name == "W" | child$name == "H") {
            node$RemoveChild(child$name)
          }
        } else {
          #recursion through the remaining nodes...
          prune_tree(child)
          
          #this part removes the entire branch if it's dead (i.e., if the leaf node was removed, then the whole branch it was on is removed)
          if (length(child$children) == 0) {
            node$RemoveChild(child$name)
          }
        }
      }
    } #this happens at the end--it prunes branches that are orthotactically illegal (current, just word-final [H] or [W] graphemes)
    
    #starting it up...FIND ALL OPTIONS FOR WORD INITIAL GRAPHEMES AND INITIALIZE TREE STRUCTURE
    myword <- toupper(startword) #current string
    options_1_left <- myword
    options_2_left <- myword
    options_3_left <- myword
    options_4_left <- myword
    options_1e_left <- myword
    options_2e_left <- myword
    options_ue_left <- myword
    options_base_left <- myword #this is needed to revert back if a _E option would be illegal
    
    myletters <- data.frame(str_split_fixed(myword, "", max(nchar(myword))))
    skeleton <- gsub("_+","_",str_replace_all(myword,remove_consonants)) #replaces all orthographic consonants with _ and then reduces it only one _ for clusters (e.g., ARRANGE becomes A__A__E and then A_A_E)
    myskeleton <- data.frame(str_split_fixed(skeleton, "", max(nchar(skeleton))))
    
    options_1 <- unique(subset(parse_corpus_word_initial_mappings, grapheme == paste0(myletters[,1]))$grapheme)
    #longer options require a check that the word is long enough, e.g., don't look for 4-letter options like OUGH if the word is only 3 letters
    if(nchar(myword)>1){  options_2 <- unique(subset(parse_corpus_word_initial_mappings, grapheme == paste0(myletters[,1],myletters[,2]))$grapheme)} else {options_2<-NULL}
    if(nchar(myword)>2){  options_3 <- unique(subset(parse_corpus_word_initial_mappings, grapheme == paste0(myletters[,1],myletters[,2],myletters[,3]))$grapheme)} else {options_3<-NULL}
    if(nchar(myword)>3){  options_4 <- unique(subset(parse_corpus_word_initial_mappings, grapheme == paste0(myletters[,1],myletters[,2],myletters[,3],myletters[,4]))$grapheme)} else {options_4<-NULL}
    
    #check for V_E like A_E in ACE
    if(ncol(myskeleton)>2){  options_1e <- unique(subset(parse_corpus_word_initial_mappings, grapheme == paste0(myskeleton[,1],myskeleton[,2],myskeleton[,3]))$grapheme)} else {options_1e<-NULL}
    #UPDATE: 10/10/24: this can sometimes ID an option that isn't real, e.g. for AWESOME, it will pick out AWE...but that's not a silent E option. So, this will be reset to NULL because it lacks an _:
    if(length(options_1e) && grepl("_",options_1e)){options_1e <- options_1e} else {options_1e <- NULL}
    #check for V__E like U_E in URGE [but it'll be called U_E not U__E]
    if(ncol(myskeleton)>3){  ifelse(ncol(myskeleton)>3, options_2e <- unique(subset(parse_corpus_word_initial_mappings, grapheme == paste0(myskeleton[,1],myskeleton[,2],myskeleton[,3],myskeleton[,4]))$grapheme), options_2e <- character(0))} else {options_2e<-NULL}
    if(length(options_2e) && grepl("_",options_2e)){options_2e <- options_2e} else {options_2e <- NULL}
    #check for V__E when there is a U that may be a consonant like in USQUE or OGUE [make V_UE into V_E]
    if(ncol(myskeleton)>3){  ifelse(ncol(myskeleton)>3 & myskeleton[,3] == "U", options_ue <- unique(subset(parse_corpus_word_initial_mappings, grapheme == paste0(myskeleton[,1],myskeleton[,2],myskeleton[,4]))$grapheme), options_ue <- character(0))} else {options_ue<-NULL}
    if(length(options_ue) && grepl("_",options_ue)){options_ue <- options_ue} else {options_ue <- NULL}
    
    #for each option above, determine what string is left. _E options must also check whether what's left includes a vowel, otherwise the whole option is a NO
    if(length(options_1)) options_1_left <- sub(options_1,"",myword) else options_1_left <- NULL
    if(length(options_2)) options_2_left <- sub(options_2,"",myword) else options_2_left <- NULL
    if(length(options_3)) options_3_left <- sub(options_3,"",myword) else options_3_left <- NULL
    if(length(options_4)) options_4_left <- sub(options_4,"",myword) else options_4_left <- NULL
    
    #1e: this should remove the vowels, e.g., if U_E in URGENT return just RG_NT
    #UPDATE 10/8/24: an underscore is kept where the E was, so that there is no confusion about what the next grapheme could be
    #e.g., without this, HOMEMADE will eventually become HMMADE, and the code will think that MM is an option--now it won't, because it becomes M_MADE, and this blocks a spurious MM
    
    #1e: remove letters before the underscore
    if(length(options_1e)) {
      pattern_before_underscore <- strsplit(sub("_.*", "", options_1e), NULL)[[1]]  
      #loop to remove those letters from options_1e_left
      for (char in pattern_before_underscore) {
        options_1e_left <- sub(char, "", options_1e_left, fixed = TRUE)}
      #replace the first occurrence of "E" remaining with "_"
      options_1e_left <- sub("E", "_", options_1e_left)}
    
    #1e: undo 1e changes if it would be illegal
    if (!grepl(vowels, options_1e_left, ignore.case = TRUE)) { #if TRUE, there is no vowel left...but check to see if maybe that's because the string is done
      if (process_string(options_1e,options_base_left) == "") {options_1e_left <- options_1e_left} #if TRUE, then indeed the string is done and we're good to go 
      else { options_1e_left <-options_base_left #this is for when there is no vowel left AND the word isn't done--RESET!
      options_1e <- character(0)} 
    } else { options_1e_left <- options_1e_left} #this is for when there IS a vowel left, in which case again we're good to go
    
    #2e: remove letters before the underscore
    if(length(options_2e)) {
      pattern_before_underscore <- strsplit(sub("_.*", "", options_2e), NULL)[[1]]  
      #loop to remove those letters from options_2e_left
      for (char in pattern_before_underscore) {
        options_2e_left <- sub(char, "", options_2e_left, fixed = TRUE)}
      #replace the first occurrence of "E" remaining with "_"
      options_2e_left <- sub("E", "_", options_2e_left)}
    
    #2e: undo 2e changes if it would be illegal
    if (!grepl(vowels, options_2e_left, ignore.case = TRUE)) { #if TRUE, there is no vowel left...but check to see if maybe that's because the string is done
      if (process_string(options_2e,options_base_left) == "") {options_2e_left <- options_2e_left} #if TRUE, then indeed the string is done and we're good to go 
      else { options_2e_left <-options_base_left #this is for when there is no vowel left AND the word isn't done--RESET!
      options_2e <- character(0)} 
    } else { options_2e_left <- options_2e_left} #this is for when there IS a vowel left, in which case again we're good to go
    
    
    #ue: remove letters before the underscore
    if(length(options_ue)) {
      pattern_before_underscore <- strsplit(sub("_.*", "", options_ue), NULL)[[1]]  
      #loop to remove those letters from options_ue_left
      for (char in pattern_before_underscore) {
        options_ue_left <- sub(char, "", options_ue_left, fixed = TRUE)}
      #replace the first occurrence of "E" remaining with "_"
      options_ue_left <- sub("E", "_", options_ue_left)}
    
    
    #ue: undo ue changes if it would be illegal
    if (!grepl(vowels, options_ue_left, ignore.case = TRUE)) { #if TRUE, there is no vowel left...but check to see if maybe that's because the string is done
      if (process_string(options_ue,options_base_left) == "") {options_ue_left <- options_ue_left} #if TRUE, then indeed the string is done and we're good to go 
      else { options_ue_left <-options_base_left #this is for when there is no vowel left AND the word isn't done--RESET!
      options_ue <- character(0)} 
    } else { options_ue_left <- options_ue_left} #this is for when there IS a vowel left, in which case again we're good to go
    
    
    ##initialize tree structures
    mytree <- Node$new(myword)
    wi1 <- mytree$AddChild(options_1)
    if(length(options_2)>0)   {  wi2 <- mytree$AddChild(options_2)  }
    if(length(options_3)>0)   {  wi3 <- mytree$AddChild(options_3)  }
    if(length(options_4)>0)   {  wi4 <- mytree$AddChild(options_4)  }
    if(length(options_1e)>0)   {  wi1e <- mytree$AddChild(options_1e)  }
    if(length(options_2e)>0)   {  wi2e <- mytree$AddChild(options_2e)  }
    if(length(options_ue)>0)   {  wiue <- mytree$AddChild(options_ue)  }
    
    lefttree <- Node$new(myword)
    if(options_1_left=="")   {wi1 <- lefttree$AddChild("9")} else {wi1 <- lefttree$AddChild(options_1_left)}   #NOTE: "9" is used to indicate the string is done!
    if(length(options_2)>0)   {if(options_2_left==""){wi2 <- lefttree$AddChild("9")}else{wi2 <- lefttree$AddChild(options_2_left)}}
    if(length(options_3)>0)   {if(options_3_left==""){wi3 <- lefttree$AddChild("9")}else{wi3 <- lefttree$AddChild(options_3_left)}}
    if(length(options_4)>0)   {if(options_4_left==""){wi4 <- lefttree$AddChild("9")}else{wi4 <- lefttree$AddChild(options_4_left)}}
    if(length(options_1e)>0)  {if(options_1e_left==""){wi1e <- lefttree$AddChild("9")}else{wi1e <- lefttree$AddChild(options_1e_left)}}
    if(length(options_2e)>0)  {if(options_2e_left==""){wi2e <- lefttree$AddChild("9")}else{wi2e <- lefttree$AddChild(options_2e_left)}}
    if(length(options_ue)>0)  {if(options_ue_left==""){wiue <- lefttree$AddChild("9")}else{wiue <- lefttree$AddChild(options_ue_left)}}
    
    
    ##tree now contains all potential word initial graphemes, and options_left tracks what is left to parse for each of those options
    
    ############################################################################
    ################BEGIN LOOPING THROUGH REMAINING LETTERS HERE################
    ############################################################################
    
    counter <- 1 #start the counter at 1 -- this will tick up as lefttree leave's become 9's
    
    while (any(as.matrix(as.data.frame(lefttree$leaves)) != "9")) { ##this says, while ANY of the leaves are other than 9...continue what follows
      #so you will continue to go through the following until every leaf, i.e., every parse path, is done--the indication being that lefttree's leaves are all 9's
      
      if(counter > length(mytree$leaves)){counter <- 1} #if it's skipped past an unfinished path, the counter can exceed the number of leaves
      #this will catch that and restart the counter, which should result in working back around to the unfinished path
      
      while (lefttree$leaves[[counter]]$name=="9") {counter <- counter + 1} #if a path has just finished and the counter went up, but it's now onto a path that 
      #was ALREADY finished (which happens if the next path had a longer grapheme), then it needs to keep increasing the counter until this is no longer true
      
      my_leaf <- lefttree$leaves[[counter]]$name #take the first lefttree leaf, as this is what you next need to parse
      #but as the parse path is resolved the lefttree leaf becomes a 9 and that will make the counter tick up to go to the next leaf
      #UPDATE: part of 10/8/24 fix to handle silent E--now that an _ is left, if it's penultimate like in HOMEMADE at the end, you have D_...
      #so, remove _ from my_leaf IFF it is the FINAL character...
      my_leaf  <- sub("_$", "", my_leaf)
      
      mytree_path <- mytree$leaves[[counter]]$path[-1]  #this is critical! you need the whole path you're working on from mytree
      #in order to know where you're adding the next grapheme
      #the [-1] is because the base word is included in the path but isn't used to climb the tree
      
      lefttree_path <- lefttree$leaves[[counter]]$path[-1]  #the same is needed to know how to update lefttree, including placing 9's
      #
      
      if(my_leaf != "9") { #only do these things if my_leaf isn't a 9, meaning the lefttree path isn't done.
        #if it is ==9, then instead after this the counter will tick up and start from above]
        #UPDATED: 10/14/24: added option to search for 5-letter graphemes (which is needed for, e.g., W-EIGHE-D) and VV_E silent E options wrapped around GU (needed for, e.g., LEAGUE)
        
        internal_options_1_left <- my_leaf
        internal_options_2_left <- my_leaf
        internal_options_3_left <- my_leaf
        internal_options_4_left <- my_leaf
        internal_options_5_left <- my_leaf
        internal_options_1e_left <- my_leaf
        internal_options_2e_left <- my_leaf
        internal_options_ue_left <- my_leaf
        internal_options_2e2_left <- my_leaf
        internal_options_base_left <- my_leaf #this is needed to revert back if a _E option would be illegal
        
        myletters_left <- data.frame(str_split_fixed(my_leaf, "", max(nchar(my_leaf))))
        skeleton_left <- gsub("_+","_",str_replace_all(my_leaf,remove_consonants)) 
        myskeleton_left <- data.frame(str_split_fixed(skeleton_left, "", max(nchar(skeleton_left))))
        
        #now have to determine if this would be a word-final mapping--if its length is the same as the remaining length it would be!
        #UPDATED: 10/8/24: there could now be an _ in the leftover string if, e.g. O_E was just removed (and there are still more letters to parse)
        #if so, it should not be counted as remaining letters!
        #UPDATED: 10/14/24: added option to search for 5-letter graphemes (which is needed for, e.g., W-EIGHE-D) and VV_E silent E options wrapped around GU (needed for, e.g., LEAGUE)
        if(ncol(myletters_left)-as.numeric("_" %in% myletters_left)==1){check_mappings <- parse_corpus_word_final_mappings} else {check_mappings <- internal_mappings}
        if(length(myletters_left)>0){internal_options_1 <- unique(subset(check_mappings, grapheme == paste0(myletters_left[,1]))$grapheme)} else {internal_options_1 <- NULL }
        
        if(ncol(myletters_left)-as.numeric("_" %in% myletters_left)==2){check_mappings <- parse_corpus_word_final_mappings} else {check_mappings <- internal_mappings}
        if(length(myletters_left)>1){internal_options_2 <- unique(subset(check_mappings, grapheme == paste0(myletters_left[,1],myletters_left[,2]))$grapheme)} else {internal_options_2 <- NULL}
        
        if(ncol(myletters_left)-as.numeric("_" %in% myletters_left)==3){check_mappings <- parse_corpus_word_final_mappings} else {check_mappings <- internal_mappings}
        if(length(myletters_left)>2){internal_options_3 <- unique(subset(check_mappings, grapheme == paste0(myletters_left[,1],myletters_left[,2],myletters_left[,3]))$grapheme)} else { internal_options_3 <- NULL}
        
        if(ncol(myletters_left)-as.numeric("_" %in% myletters_left)==4){check_mappings <- parse_corpus_word_final_mappings} else {check_mappings <- internal_mappings}
        if(length(myletters_left)>3){internal_options_4 <- unique(subset(check_mappings, grapheme == paste0(myletters_left[,1],myletters_left[,2],myletters_left[,3],myletters_left[,4]))$grapheme)} else { internal_options_4 <- NULL}
        
        if(ncol(myletters_left)-as.numeric("_" %in% myletters_left)==5){check_mappings <- parse_corpus_word_final_mappings} else {check_mappings <- internal_mappings}
        if(length(myletters_left)>4){internal_options_5 <- unique(subset(check_mappings, grapheme == paste0(myletters_left[,1],myletters_left[,2],myletters_left[,3],myletters_left[,4],myletters_left[,5]))$grapheme)} else { internal_options_5 <- NULL}
        
        #and UPDATED 10/8/24: now that the options have been determined, you can get rid of any _ that is still present, which would have been holding place from a removed "silent E"
        my_leaf <- gsub("_","",my_leaf)
        
        #note: by definition final_E mappings are never the end of the word (words ending in vowels never end in final_E, e.g., in PACE the end is actually C, not A_E) therefore these are never drawn from word-final positions
        ifelse(ncol(myskeleton_left) > 2, internal_options_1e <- unique(subset(internal_mappings, grapheme == paste0(myskeleton_left[,1],myskeleton_left[,2],myskeleton_left[,3]))$grapheme), internal_options_1e <- character(0))
        #UPDATE: 10/10/24: this can sometimes ID an option that isn't real, e.g. for AWESOME, it will pick out AWE...but that's not a silent E option. So, this will be reset to NULL because it lacks an _:
        if(length(internal_options_1e) && grepl("_",internal_options_1e)){internal_options_1e <- internal_options_1e} else {internal_options_1e <- character(0)}
        ifelse(ncol(myskeleton_left) > 3, internal_options_2e <- unique(subset(internal_mappings, grapheme == paste0(myskeleton_left[,1],myskeleton_left[,2],myskeleton_left[,3],myskeleton_left[,4]))$grapheme), internal_options_2e <- character(0))
        if(length(internal_options_2e) && grepl("_",internal_options_2e)){internal_options_2e <- internal_options_2e} else {internal_options_2e <- character(0)}
        ifelse(ncol(myskeleton_left) > 3, ifelse( myskeleton_left[,3] == "U",internal_options_ue <- unique(subset(internal_mappings, grapheme == paste0(myskeleton_left[,1],myskeleton_left[,2],myskeleton_left[,4]))$grapheme), internal_options_ue <- character(0)), internal_options_ue <- character(0))
        if(length(internal_options_ue) && grepl("_",internal_options_ue)){internal_options_ue <- internal_options_ue} else {internal_options_ue <- character(0)}
        #NOTE: 2e2 will run IFF the 4th column is a U, because it seems we only need to check for a double-vowel-silent E, like EA_E, with two intervening letters (the double __) when it's in LEAGUE, i.e., when the intervening grapheme is GU [and this is only because the U is typically assumed to be a vowel, but it is not in GU])
        if(ncol(myskeleton_left)>3){
          if(myskeleton_left[,4] == "U" && ncol(myskeleton_left)>3){
            ifelse(ncol(myskeleton_left) > 4, internal_options_2e2 <- unique(subset(internal_mappings, grapheme == paste0(myskeleton_left[,1],myskeleton_left[,2],myskeleton_left[,3],myskeleton_left[,5]))$grapheme), internal_options_2e2 <- character(0))
            if(length(internal_options_2e2) && grepl("_",internal_options_2e2)){internal_options_2e2 <- internal_options_2e2} else {internal_options_2e2 <- character(0)}
          } else {internal_options_2e2 <- character(0)}
        } else {internal_options_2e2 <- character(0)}
        
        if(length(internal_options_1)) internal_options_1_left <- sub(internal_options_1,"",my_leaf) else internal_options_1_left <- internal_options_1_left
        if(length(internal_options_2)) internal_options_2_left <- sub(internal_options_2,"",my_leaf) else internal_options_2_left <- internal_options_2_left
        if(length(internal_options_3)) internal_options_3_left <- sub(internal_options_3,"",my_leaf) else internal_options_3_left <- internal_options_3_left
        if(length(internal_options_4)) internal_options_4_left <- sub(internal_options_4,"",my_leaf) else internal_options_4_left <- internal_options_4_left
        if(length(internal_options_5)) internal_options_5_left <- sub(internal_options_5,"",my_leaf) else internal_options_5_left <- internal_options_5_left
        
        #UPDATE: same as the 10/8/24 update, address issue of "silent E" graphemes leading to false options like HOMEMADE --> MMADE
        if(length(internal_options_1e)) {
          pattern_before_underscore <- strsplit(sub("_.*", "", internal_options_1e), NULL)[[1]]  
          #loop to remove those letters from internal_options_1e_left
          for (char in pattern_before_underscore) {
            internal_options_1e_left <- sub(char, "", internal_options_1e_left, fixed = TRUE)}
          #replace the first occurrence of "E" remaining with "_"
          internal_options_1e_left <- sub("E", "_", internal_options_1e_left)}
        
        #1e: undo 1e changes if it would be illegal, including removing it if it's not actually ending in _E in the first place
        if( length(internal_options_1e) >0) {if (!grepl("_E$", internal_options_1e)) {internal_options_1e <- character(0)}} #if this is true, then it doesn't even end in _E, so change it to character(0)!
        if (!grepl(vowels, internal_options_1e_left, ignore.case = TRUE)) { #if TRUE, there is no vowel left...but check to see if maybe that's because the string is done
          if (process_string(internal_options_1e,internal_options_base_left) == "") {internal_options_1e_left <- internal_options_1e_left} #if TRUE, then indeed the string is done and we're good to go 
          else { internal_options_1e_left <-internal_options_base_left #this is for when there is no vowel left AND the word isn't done--RESET!
          internal_options_1e <- character(0)} 
        } else { internal_options_1e_left <- internal_options_1e_left} #this is for when there IS a vowel left, in which case again we're good to go
        
        if(length(internal_options_2e)) {
          pattern_before_underscore <- strsplit(sub("_.*", "", internal_options_2e), NULL)[[1]]  
          #loop to remove those letters from internal_options_2e_left
          for (char in pattern_before_underscore) {
            internal_options_2e_left <- sub(char, "", internal_options_2e_left, fixed = TRUE)}
          #replace the first occurrence of "E" remaining with "_"
          internal_options_2e_left <- sub("E", "_", internal_options_2e_left)}
        
        #2e: undo 2e changes if it would be illegal, including removing it if it's not actually ending in _E in the first place
        if (length(internal_options_2e) > 0) {if (!grepl("_E$", internal_options_2e)) {internal_options_2e <- character(0)}} #if this is true, then it doesn't even end in _E, so change it to character(0)!
        if (!grepl(vowels, internal_options_2e_left, ignore.case = TRUE)) { #if TRUE, there is no vowel left...but check to see if maybe that's because the string is done
          if (process_string(internal_options_2e,internal_options_base_left) == "") {internal_options_2e_left <- internal_options_2e_left} #if TRUE, then indeed the string is done and we're good to go 
          else { internal_options_2e_left <-internal_options_base_left #this is for when there is no vowel left AND the word isn't done--RESET!
          internal_options_2e <- character(0)} 
        } else { internal_options_2e_left <- internal_options_2e_left} #this is for when there IS a vowel left, in which case again we're good to go
        
        
        if(length(internal_options_ue)) {
          pattern_before_underscore <- strsplit(sub("_.*", "", internal_options_ue), NULL)[[1]]  
          #loop to remove those letters from internal_options_ue_left
          for (char in pattern_before_underscore) {
            internal_options_ue_left <- sub(char, "", internal_options_ue_left, fixed = TRUE)}
          #replace the first occurrence of "E" remaining with "_"
          internal_options_ue_left <- sub("E", "_", internal_options_ue_left)}
        
        #ue: undo ue changes if it would be illegal, including removing it if it's not actually ending in _E in the first place
        if (length(internal_options_ue) >0) {if (!grepl("_E$", internal_options_ue)) {internal_options_ue <- character(0)}} #if this is true, then it doesn't even end in _E, so change it to character(0)!
        if (!grepl(vowels, internal_options_ue_left, ignore.case = TRUE)) { #if TRUE, there is no vowel left...but check to see if maybe that's because the string is done
          if (process_string(internal_options_ue,internal_options_base_left) == "") {internal_options_ue_left <- internal_options_ue_left} #if TRUE, then indeed the string is done and we're good to go 
          else { internal_options_ue_left <-internal_options_base_left #this is for when there is no vowel left AND the word isn't done--RESET!
          internal_options_ue <- character(0)} 
        } else { internal_options_ue_left <- internal_options_ue_left} #this is for when there IS a vowel left, in which case again we're good to go
        
        if(length(internal_options_2e2)) {
          pattern_before_underscore <- strsplit(sub("_.*", "", internal_options_2e2), NULL)[[1]]  
          #loop to remove those letters from internal_options_2e2_left
          for (char in pattern_before_underscore) {
            internal_options_2e2_left <- sub(char, "", internal_options_2e2_left, fixed = TRUE)}
          #replace the first occurrence of "E" remaining with "_"
          internal_options_2e2_left <- sub("E", "_", internal_options_2e2_left)}
        
        #2e: undo 2e2 changes if it would be illegal, including removing it if it's not actually ending in _E in the first place
        if (length(internal_options_2e2) > 0) {if (!grepl("_E$", internal_options_2e2)) {internal_options_2e2 <- character(0)}} #if this is true, then it doesn't even end in _E, so change it to character(0)!
        if (!grepl(vowels, internal_options_2e2_left, ignore.case = TRUE)) { #if TRUE, there is no vowel left...but check to see if maybe that's because the string is done
          if (process_string(internal_options_2e2,internal_options_base_left) == "") {internal_options_2e2_left <- internal_options_2e2_left} #if TRUE, then indeed the string is done and we're good to go 
          else { internal_options_2e2_left <-internal_options_base_left #this is for when there is no vowel left AND the word isn't done--RESET!
          internal_options_2e2 <- character(0)} 
        } else { internal_options_2e2_left <- internal_options_2e2_left} #this is for when there IS a vowel left, in which case again we're good to go
        
        
        ##UPDATE tree structures
        if(length(internal_options_1)>0) { internal <- mytree$Climb(mytree_path)$AddChild(internal_options_1) } 
        if(length(internal_options_2)>0)   { internal <- mytree$Climb(mytree_path)$AddChild(internal_options_2) } 
        if(length(internal_options_3)>0)   { internal <- mytree$Climb(mytree_path)$AddChild(internal_options_3) } 
        if(length(internal_options_4)>0)   { internal <- mytree$Climb(mytree_path)$AddChild(internal_options_4) } 
        if(length(internal_options_5)>0)   { internal <- mytree$Climb(mytree_path)$AddChild(internal_options_5) } 
        if(length(internal_options_1e)>0)   { internal <- mytree$Climb(mytree_path)$AddChild(internal_options_1e) } 
        if(length(internal_options_2e)>0)   { internal <- mytree$Climb(mytree_path)$AddChild(internal_options_2e) } 
        if(length(internal_options_ue)>0)   { internal <- mytree$Climb(mytree_path)$AddChild(internal_options_ue) } 
        if(length(internal_options_2e2)>0)   { internal <- mytree$Climb(mytree_path)$AddChild(internal_options_2e2) } 
        
        
        #if the word is finished, you'll get an error trying to assign "" to the lefttree -- so check if the word-final is occurring and if so 9 is entered to signal that
        if(length(internal_options_1)>0) {
          if(ncol(myletters_left)==1) {internal <- lefttree$Climb(lefttree_path)$AddChild("9")} else 
          {  internal <- lefttree$Climb(lefttree_path)$AddChild(internal_options_1_left)  }}
        
        if(length(internal_options_2)>0)   {
          if(ncol(myletters_left)==2) {internal <- lefttree$Climb(lefttree_path)$AddChild("9")} else 
          {  internal <- lefttree$Climb(lefttree_path)$AddChild(internal_options_2_left)  }}
        
        if(length(internal_options_3)>0)   {
          if(ncol(myletters_left)==3) {internal <- lefttree$Climb(lefttree_path)$AddChild("9")} else 
          {  internal <- lefttree$Climb(lefttree_path)$AddChild(internal_options_3_left)  }}
        
        if(length(internal_options_4)>0)   {
          if(ncol(myletters_left)==4) {internal <- lefttree$Climb(lefttree_path)$AddChild("9")} else 
          {  internal <- lefttree$Climb(lefttree_path)$AddChild(internal_options_4_left)  }}
        
        if(length(internal_options_5)>0)   {
          if(ncol(myletters_left)==5) {internal <- lefttree$Climb(lefttree_path)$AddChild("9")} else 
          {  internal <- lefttree$Climb(lefttree_path)$AddChild(internal_options_5_left)  }}
        
        if(length(internal_options_1e)>0)   {  internal <- lefttree$Climb(lefttree_path)$AddChild(internal_options_1e_left)  }
        if(length(internal_options_2e)>0)   {  internal <- lefttree$Climb(lefttree_path)$AddChild(internal_options_2e_left)  }
        if(length(internal_options_ue)>0)   {  internal <- lefttree$Climb(lefttree_path)$AddChild(internal_options_ue_left)  }
        if(length(internal_options_2e2)>0)   {  internal <- lefttree$Climb(lefttree_path)$AddChild(internal_options_2e2_left)  }
        
      } #this closes the if that says do this only if my_leaf (which is from lefttree) is NOT a 9
      
      #CRITICAL! this runs when my_leaf IS a 9, but ALSO it will run if it wasn't a 9 earlier but NOW is a 9 because of updating
      #either way, it results in the counter updating to move to the next leaf
      
      if(lefttree$leaves[[counter]]$name == "9" ){ counter <- counter + 1}
      
    } #end updating
    
    #final step: prune the branches that posit anything orthotactically illegal
    #current list is just word-final [H] or [W] (those are illegal as final GRAPHEMES, not as final phonemes)
    #the function to do this appears earlier, outside the loop... ("prune_tree")
    prune_tree(mytree)
    
    return(mytree)
  }

  #internal function: to extract all branches into a list format
  #note that it also adds new branches if it finds an LE sequence in case it's nonlinear like in TABLE..and same for nonlinear RE like ACRE and in ONE/ONCE morphemes
  #it does not consider that possibility if the word begins with LE, though (so it doesn't do it for, e.g., LEAP)
  extract_branches <- function(node, path = c()) {
    path <- c(path, node$name) # Append current node name to the path
    
    if (node$isLeaf) {
      branch <- paste(path, collapse = "-")
      
      # Extract the original word
      original_word <- sub("-.*", "", branch)  # Get the part before the first dash (i.e., the word)
      
      # Split the branch into graphemes (remove the original word)
      graphemes <- unlist(strsplit(sub(paste0(original_word, "-"), "", branch), "-"))
      
      # Check for "L" followed by "E" in the graphemes, skipping the first "L-E" immediately after the word
      modified_branches <- list(branch)  # Start with the original branch
      
      # Iterate through the graphemes to find "L-E" sequences
      le_indices <- which(graphemes == "L" & c(graphemes[-1], "") == "E")
      
      # Skip the first "L-E" if it's right after the original word
      if (length(le_indices) > 0 && le_indices[1] == 1) {
        le_indices <- le_indices[-1]  # Remove the first occurrence of L-E
      }
      
      # If there are any remaining "L-E" sequences to modify
      if (length(le_indices) > 0) {
        for (i in seq_along(le_indices)) {
          # Modify the corresponding "L-E" to "_E-L"
          modified_graphemes <- graphemes
          modified_graphemes[le_indices[i]] <- "_E"
          modified_graphemes[le_indices[i] + 1] <- "L"
          
          # Create a modified branch string
          modified_branch <- paste(c(original_word, modified_graphemes), collapse = "-")
          modified_branches <- c(modified_branches, modified_branch)
        }
      }
      
      # Iterate through the graphemes to find "R-E" sequences
      re_indices <- which(graphemes == "R" & c(graphemes[-1], "") == "E")
      
      # Skip the first "R-E" if it's right after the original word
      if (length(re_indices) > 0 && re_indices[1] == 1) {
        re_indices <- re_indices[-1]  # Remove the first occurrence of R-E
      }
      
      # If there are any remaining "R-E" sequences to modify
      if (length(re_indices) > 0) {
        for (i in seq_along(re_indices)) {
          # Modify the corresponding "R-E" to "_E-R"
          modified_graphemes <- graphemes
          modified_graphemes[re_indices[i]] <- "_E"
          modified_graphemes[re_indices[i] + 1] <- "R"
          
          # Create a modified branch string
          modified_branch <- paste(c(original_word, modified_graphemes), collapse = "-")
          modified_branches <- c(modified_branches, modified_branch)
        }
      }
      
      
      # Check for "O" followed by "N" followed by "E" in the graphemes
      one_indices <-suppressWarnings(which(graphemes == "O" & c(graphemes[-1], "") == "N" & c(graphemes[-(1:2)], "") == "E"))
      if (length(one_indices) > 0) {
        for (i in seq_along(one_indices)) {
          modified_graphemes <- graphemes
          modified_graphemes[one_indices[i]] <- "O"
          modified_graphemes[one_indices[i] + 1] <- "_E"
          modified_graphemes[one_indices[i] + 2] <- "N"
          
          modified_branch <- paste(c(original_word, modified_graphemes), collapse = "-")
          modified_branches <- c(modified_branches, modified_branch)
        }
      }
      
      # Check for "O" followed by "N" followed by "C" followed by "E" in the graphemes
      once_indices <-suppressWarnings(which(graphemes == "O" & c(graphemes[-1], "") == "N" & c(graphemes[-(1:2)], "") == "C" & c(graphemes[-(1:3)], "") == "E"))
      if (length(once_indices) > 0) {
        for (i in seq_along(once_indices)) {
          modified_graphemes <- graphemes
          modified_graphemes[once_indices[i]] <- "O"
          modified_graphemes[once_indices[i] + 1] <- "_E"
          modified_graphemes[once_indices[i] + 2] <- "N"
          modified_graphemes[once_indices[i] + 3] <- "C"
          
          modified_branch <- paste(c(original_word, modified_graphemes), collapse = "-")
          modified_branches <- c(modified_branches, modified_branch)
        }
      }
      
      return(modified_branches)
    } else {
      branches <- list()
      for (child in node$children) {
        branches <- c(branches, extract_branches(child, path)) # Recursively traverse children
      }
      return(branches)
    }
  }

  #this is a function called by extract_graphemes_for_letters
  count_letters_and_extract_graphemes <- function(branches) {
    # Extract the original word
    original_word <- sub("-.*", "", branches[[1]])  # Extract the first part before the dash
    
    # Count occurrences of each letter in the original word
    letter_counts <- table(strsplit(original_word, "")[[1]])
    
    # Transform branches into sets of unique graphemes
    grapheme_branches <- lapply(branches, function(x) {
      # Split by dash and remove the first element (the original word)
      graphemes <- unlist(strsplit(sub("^[^-]*-", "", x), "-"))
      return(graphemes)  # Return graphemes for each branch without filtering for uniqueness
    })
    
    return(list(letter_counts = letter_counts, grapheme_branches = grapheme_branches))
  }

  #this takes the output of extract_branches and gives a list of the possible graphemes for each letter
  extract_graphemes_for_letters <- function(branches) {
    results <- count_letters_and_extract_graphemes(branches)
    grapheme_branches <- results$grapheme_branches
    
    # Initialize a list to hold unique graphemes for each letter occurrence
    letter_occurrences <- list()
    
    # Initialize counters for each letter
    letter_total_counts <- as.list(results$letter_counts)
    
    # Initialize a data frame to store results
    results_df <- data.frame(
      Letter_Occurrence = character(),
      Graphemes = character(),
      Position = character(),
      branch_no = integer(),
      branch_grapheme_no = integer(),
      stringsAsFactors = FALSE
    )
    
    # Iterate over each branch
    for (branch_no in seq_along(grapheme_branches)) {
      branch <- grapheme_branches[[branch_no]]
      # Initialize counts for current branch
      branch_letter_counts <- setNames(as.list(rep(0, length(letter_total_counts))), names(letter_total_counts))
      
      # Iterate over graphemes in each branch
      for (i in seq_along(branch)) {
        grapheme <- branch[i]
        
        # Determine position
        position <- if (i == 1) {
          "wi"  # Word-initial
        } else if (i == length(branch)) {
          "wf"  # Word-final
        } else {
          "wm"  # Word-middle
        }
        
        # Split grapheme into individual letters, ignoring underscores
        grapheme_letters <- unlist(strsplit(grapheme, ""))
        grapheme_letters <- grapheme_letters[grapheme_letters != "_"]
        
        # For each letter in the grapheme, process in original order
        for (letter in grapheme_letters) {
          # Update the branch letter counter
          branch_letter_counts[[letter]] <- branch_letter_counts[[letter]] + 1
          
          # Create an occurrence key using the current count for the letter
          occurrence_key <- paste0(letter, branch_letter_counts[[letter]])
          
          # Store the result in the results_df, preserving original letter order
          results_df <- rbind(
            results_df,
            data.frame(
              Letter_Occurrence = occurrence_key, 
              Graphemes = grapheme, 
              Position = position,
              branch_no = branch_no,
              branch_grapheme_no = i,
              stringsAsFactors = FALSE
            )
          )
        }
      }
    }
    
    return(results_df)
  }

  #indexing functions
  add_parse_number <- function(output_df, string_length) {
    #how many total rows? this divided by the length of the target string tells you how many parses
    total_rows <- nrow(output_df)
    parse_no <- rep(1:(total_rows / string_length), each = string_length)
    #now add those parse indexes
    output_df$parse_no <- parse_no
    return(output_df)
  }
  add_grapheme_index <- function(df) {
    df$grapheme_index <- NA
    current_counter <- 1
    accumulated_letters <- ""
    accumulated_graphemes <- ""
    set_flag <- FALSE 
    
    for (j in 1:max(df$parse_no)) {
      rows <- which(df$parse_no == j)
      current_counter <- 1  # Reset the counter for each parse
      accumulated_letters <- "" # Reset accumulated strings for the next parse
      accumulated_graphemes <- "" # Reset accumulated strings for the next parse
      
      for (i in min(rows):max(rows)) {
        # Update accumulated strings
        accumulated_letters <- paste0(accumulated_letters, 
                                      substr(df$Letter_Occurrence[i], 1, 1))
        if(set_flag == FALSE){
          accumulated_graphemes <- paste0(accumulated_graphemes, 
                                          gsub("_", "", df$Graphemes[i]))}
        
        # Assign grapheme_index with the full Graphemes value
        df$grapheme_index[i] <- paste0(current_counter, df$Graphemes[i])
        
        # Check if accumulated letters and graphemes are equal
        if (accumulated_letters == accumulated_graphemes) {
          current_counter <- current_counter + 1  # Tick up the counter
          set_flag <- FALSE
        } else {
          # If there's a mismatch, we do not increment the counter,
          # and we set a flag to NOT update the accumulated graphemes...
          set_flag <- TRUE
        }
      }
      
      
    }
    
    return(df)
  }

  ##BEGIN
  #parse and extract branches
  branches <- extract_branches(parse_string(target))
  
  ##if word ends in -ES, strip off the S, parse the string without that, add -S to each resulting branch, and then gather all unique branches from those two
  if(grepl("es$", target)){
    target2 <- substr(target, 1, nchar(target) - 1)
    branches2 <- extract_branches(parse_string(target2))
    branches2 <- lapply(branches2, function(branch) {
      # Add "S" before the first occurrence of "-"
      if (grepl("-", branch)) {
        branch <- sub("-", "S-", branch)
      }
      # Add "-S" to the end of the string
      paste(branch, "-S", sep = "")
    })
    branches <- unique(c(branches,branches2))
  }
  
  ##if word ends in -ED, strip off the D, parse the string without that, add -D to each resulting branch, and then gather all unique branches from those two
  if(grepl("ed$", target)){
    target2 <- substr(target, 1, nchar(target) - 1)
    branches2 <- extract_branches(parse_string(target2))
    branches2 <- lapply(branches2, function(branch) {
      # Add "D" before the first occurrence of "-"
      if (grepl("-", branch)) {
        branch <- sub("-", "D-", branch)
      }
      # Add "-D" to the end of the string
      paste(branch, "-D", sep = "")
    })
    branches <- unique(c(branches,branches2))
  }
  
  #run the function to get formatted results as a data frame
  allbranches_df <- extract_graphemes_for_letters(branches)
  allbranches_df <- add_parse_number(allbranches_df,nchar(target)) #keep track of which parse is which!
  
  #for each letter, determine the choices available
  allbranches_df$index2 <- paste0(allbranches_df$Graphemes,"_",allbranches_df$Position)
  allbranches_df$choices <- allbranches_df$index2
  for (j in seq_len(length(allbranches_df$Letter_Occurrence))) {
    allbranches_df$choices[j] <- paste0(sort(unique(allbranches_df[allbranches_df$Letter_Occurrence==allbranches_df[j,]$Letter_Occurrence,]$index2)),collapse=",")
  }

  #(pseudo)-morphological indices:
  allbranches_df$suffix_class <- ifelse(grepl("ed$", target, ignore.case = TRUE), "EDERES",
                                        ifelse(grepl("er$", target, ignore.case = TRUE), "EDERES",
                                               ifelse(grepl("es$", target, ignore.case = TRUE), "EDERES",
                                                      "OTHER")))
  
  #determine what those choices reduce to, in the corpus:
  allbranches_df$order <- seq_len(nrow(allbranches_df)) #CRITICAL! needed to re-order the parses after all the merge is done
  allbranches_df <- merge(allbranches_df,reduced_table,by="choices",all.x=T) #CRITICAL: added 12/16: all.x needed in case a novel string has been encountered
  allbranches_df$Letter <- substring(allbranches_df$Letter_Occurrence,1,1)

  pp2_context <- .sw_pp2_context(allbranches_df, target)
  allbranches_df <- merge(
    allbranches_df,
    pp2_context,
    by = "Letter_Occurrence",
    all.x = TRUE
  )

  # Add the legacy PP indices and the expanded-context PP2 indices.
  allbranches_df$index3 <- paste0(allbranches_df$Letter,"_in_",allbranches_df$index2,"_and_",allbranches_df$reduced,"_SC_",allbranches_df$suffix_class)
  allbranches_df$index5 <- paste0(allbranches_df$Letter,"_role_",allbranches_df$pp2_role,"_in_",allbranches_df$index2,"_and_CTX_",allbranches_df$pp2_signature,"_SC_",allbranches_df$suffix_class)

  pp_lookup <- parsing_table[, setdiff(names(parsing_table), "index4"), drop = FALSE]
  allbranches_df <- merge(allbranches_df,pp_lookup,by="index3",all.x=T) #note that all.x=T because impossible parses need to be kept

  pp2_lookup <- unique(corpus_parsed_table[, c("index5", "pp2", "pp2_freq", "pp2_count"), drop = FALSE])
  allbranches_df <- merge(allbranches_df,pp2_lookup,by="index5",all.x=T)
  allbranches_df <- allbranches_df[order(allbranches_df$order),] #re-order into original parses
  allbranches_df$order <- NULL #now drop the order variable
  allbranches_df$pp[is.na(allbranches_df$pp)] <- 0 #replace NAs, which means the parse is NEVER used in the corpus, with 0's
  allbranches_df$pp_freq[is.na(allbranches_df$pp_freq)] <- 0 #replace NAs, which means the parse is NEVER used in the corpus, with 0's
  allbranches_df$pp2[is.na(allbranches_df$pp2)] <- 0
  allbranches_df$pp2_freq[is.na(allbranches_df$pp2_freq)] <- 0

  # NEW: the mean of each measure over the letters of each parse, as pw_read computes pp
  scores <- aggregate(allbranches_df[MEASURES], by = list(parse_no = allbranches_df$parse_no),
                      FUN = mean)

  # NEW: which parses to keep, by pw_read's rule on pp
  if (isTRUE(all.equal(min_pp, 1))) {
    best_score <- max(scores$pp)
    tie_tolerance <- sqrt(.Machine$double.eps) * max(1, abs(best_score))
    scores <- scores[abs(scores$pp - best_score) <= tie_tolerance, ]
  } else {
    scores <- scores[scores$pp >= min_pp, ]
  }
  if (!nrow(scores)) return(NULL)

  # NEW: each parse's graphemes, found as make_options() in pw_read finds them, and every
  # phoneme each one can map to. pw_read's min_map = 0 keeps all of them, at any position.
  temp <- add_grapheme_index(allbranches_df)
  allbranches_df$index2 <- paste0(temp$grapheme_index, "_", temp$Position)
  gp <- all_tables_PG$gp
  parses <- lapply(scores$parse_no, function(top_parse) {
    top_graphemes <- cbind(allbranches_df[allbranches_df$parse_no==top_parse,]$Graphemes,allbranches_df[allbranches_df$parse_no==top_parse,]$Position,allbranches_df[allbranches_df$parse_no==top_parse,]$index2)
    top_graphemes <- top_graphemes[!duplicated(top_graphemes), , drop = FALSE]
    g <- tolower(top_graphemes[, 1])
    list(parse_no = top_parse, g = g, o = lapply(g, function(x) gp$phoneme[gp$grapheme == x]))
  })
  structure(parses, scores = scores[MEASURES])
}

# Returns a list of parses, position codes for each of the phonemes, the
# phonemes themselves as units, and options per phoneme unit
sw_spell_parse <- function(target) {
  #phonemes to check that require remapping as biphones (e.g., "ju", "ks")
  phonemes_to_check <- paste(c("ju","je","jU","j3","ks","kS","nj","gz","gZ"), collapse = "|")

  #INTERNAL function: prepare word for processing--prep_pword differs a bit from from the prep_word function in map_PG/map_OR, etc.
  prep_pword <- function(word_index){
    
    
    #get parsed syllables
    hold0 <- as.data.frame(as.vector(sapply(target[word_index],parse_syllables_PWSPELL))) #hold on to the list of syllables
    hold0 <- hold0[hold0!="",] #this will be able to identify if its a word like /ko ko/ or /ha ha/ that repeats the same syllable twice
    parsed_syll <- matrix(strsplit(hold0,"   ")[[1]])
    
    
    #replace empty syllable cells with NA, but don't overwrite this...so store it separately
    syll_count <- parsed_syll 
    tryCatch({syll_count[syll_count=="",] <- NA}, #tryCatch needed because this fails on monosyllabic
             error=function(e) {syll_count <<- 1})
    
    #get syllable count
    syll_count <- length(na.omit(syll_count))
    
    ##create 12345 structure to capture phoneme position w/in syllables (1 = word-initial, 2 = syllable-initial, 3 = medial, 4 = syllable-final, 5 = word-final)
    syll_struc <- parsed_syll
    
    #make all word-initial phonemes 1:
    tryCatch({syll_struc[1,] <- sub('.','1',syll_struc[1,])},
             error=function(e) {syll_struc <<- sub('.','1',syll_struc)} )  
    
    #make all syll-initial phonemes 2:
    for (i in 2:10){
      tryCatch(syll_struc[i,] <- {sub('.','2',syll_struc[i,])},
               error=function(e) NA)}
    
    #make all syll-medial phonemes 3:
    my.index <- matrix(NA,nrow=10) #get # of phonemes per syllable...
    for (i in 1:10){
      tryCatch({my.index[i] <- nchar(syll_struc[i,])},
               error=function(e) NA)} #will replace from 2nd to (length-1)th with 3's...
    
    #to get the index for monosyllabic words:
    my.index[1,] <- nchar(syll_struc[1]) 
    
    for (i in 1:10){tryCatch({
      substring(syll_struc[i,],2,my.index[i]-1) <- paste(rep("3",my.index[i]-2),collapse="")},
      error=function(e) NA)}
    
    #to do it for monosyllabic words:
    if(max(nchar(syll_struc)==1 & syll_count==1)) { 
      syll_struc <- 1
    } else {
      ifelse(syll_count==1, substring(syll_struc[1],2,my.index[1,]-1) <- paste(rep("3",my.index[1,]-2),collapse=""), syll_struc<-syll_struc)
    }
    
    #make all syll-final phonemes 4:
    for (i in 1:10){ #update my.index so that it only has the number of final char, and only if those are not also the first char (i.e., the maximum from 
      #previous my.index, excluding when that max is 1)
      ifelse (my.index[i] > 1, my.index[i] <- my.index[i], my.index[i] <- 0)}
    for (i in 1:10){tryCatch({
      substring(syll_struc[i,],my.index[i],my.index[i]) <- "4"},
      error=function(e) NA)}
    
    #make all word-final phonemes 5
    #this is easiest by IDing the last 4 and making it a 5...because that will avoide the monophonemic issue
    #e.g., EYE has just a 1, word-initial, per the current Toolkit schema...this won't overwrite that
    #syll_count has already IDed the last syllable, so:
    monophonemic <- FALSE
    ifelse (nchar(syll_struc[1])==1 & syll_count==1,monophonemic <- TRUE,monophonemic <- FALSE)
    ifelse (monophonemic==FALSE & syll_count >1, str_sub(syll_struc[syll_count,],-1,-1) <- "5",syll_struc<-syll_struc)
    #finally, get monosyllabic finished:
    ifelse (monophonemic==FALSE & syll_count ==1, str_sub(syll_struc[syll_count],-1,-1) <- "5",syll_struc<-syll_struc)
    
    #these need to be set to FALSE before trying to map a new word!
    map_j <- FALSE
    map_j_wi <- FALSE
    
    #make syll_struc just a string as well as the other strings
    #the 0 indicates nothing has been mapped yet
    syll_struc0 <- paste(syll_struc,collapse="")
    parsed_syll0 <- paste(parsed_syll,collapse="")
    
    return(matrix(c(syll_struc0,parsed_syll0)))
  }

  #INTERNAL function: Maximum Onset Principle phonological parse function--different from the one in map_PG/map_OR, etc. because it has the remapped biphone representations (e.g., "ju" = \u0153)
  parse_syllables_PWSPELL <- function(target,word_index) {
    
    vowels <- c("5", "O", "8", "je", "j3r", "ju", "jU", "o", "2", 
                "@", "a", "e", "E", "3r", "i", "1", "c", "u", "U", "^",
                "\u0153","\u2211","\u00AE","\u2020") #this line has the special cases for PW SPELL
    
    consonants <- c("G", "gz", "kS", "nj", "C", "T", "D", "Z", "N",
                    "b", "d", "f", "g", "h", "j", "k", "l", "m", "n", "p", 
                    "r", "s", "S", "t", "v", "w", "z",
                    "\u00A5","\u00F8","\u03C0","\u00E5","\u00DF") #this line has the special cases for PW SPELL
    
    
    syll_init_cons <- c("spl", "spr", "str", "skr", "skw", "pl", "pr", "G", "tr", "C", "tw", "kl", "kr", "kw", "bl", "br", "dr", "dw", "gl", "gr", "fl", "fr", "Tr", "Sr", "sl", "st", "sp", "sk", "sm", "sn", "sf", "bj", "fj", "vj","mj","kj","hj","nj","pj","tj","Cj","dj","Tj","Gj","gj","Sj","sj","gw")
    
    all_phonemes <- c(vowels, syll_init_cons, consonants)
    
    phonemes <- c()
    remaining <- target[word_index]
    vowel_count <- 0
    
    while(nchar(remaining) > 0) {
      matched <- FALSE
      
      if (length(phonemes) == 0 || (length(phonemes) > 0 && phonemes[length(phonemes)] %in% vowels)) {
        for (p in syll_init_cons) {
          if (startsWith(remaining, p)) {
            phonemes <- c(phonemes, p)
            remaining <- substring(remaining, nchar(p) + 1)
            matched <- TRUE
            break
          }
        }
      }
      
      if (!matched) {
        for (p in all_phonemes) {
          if (startsWith(remaining, p)) {
            phonemes <- c(phonemes, p)
            remaining <- substring(remaining, nchar(p) + 1)
            matched <- TRUE
            if (p %in% vowels) {
              vowel_count <- vowel_count + 1
            }
            break
          }
        }
      }
      
      if (!matched) {
        break
      }
    }
    
    pattern <- paste(ifelse(phonemes %in% vowels, "V", "C"), collapse = "")
    
    if (vowel_count < 2) {
      return(target[word_index])
    }
    
    rules <- list(
      "CCCVCC" = 5,
      "CCVCC" = 4,
      "CVCCC" = 4,
      "CCCVC" = 4,
      "CVCC" = 3,
      "CCVC" = 3,
      "CVC" = 2,
      "CVV" = 2,
      "VCC" = 2,
      "VCV" = 1,
      "VV" = 1
    )
    
    split_index <- 0
    
    for (rule in names(rules)) {
      if (startsWith(pattern, rule)) {
        split_index <- rules[[rule]]
        break
      }
    }
    
    if (split_index >= 1) {
      left <- paste(phonemes[1:split_index], collapse = "")
      right <- paste(phonemes[(split_index + 1):length(phonemes)], collapse = "")
      
      first_three_right <- substr(right, 1, 3)
      first_two_right <- substr(right, 1, 2)
      first_one_right <- substr(right, 1, 1)
      
      special_case <- FALSE
      
      if (first_one_right == "N") {
        left <- paste0(left, first_one_right)
        right <- substr(right, 2, nchar(right))
        special_case <- TRUE
      }
      
      if (!special_case) {
        if (all(substr(first_two_right, 1, 1) %in% consonants, 
                substr(first_two_right, 2, 2) %in% consonants)) {
          
          if (!(first_two_right %in% syll_init_cons)) {
            left <- paste0(left, substr(right, 1, 1))
            right <- substr(right, 2, nchar(right))
          }
        }
      }
      
      result <- paste(left, " ", right)
      
      if (vowel_count > 2) {
        right_split <- parse_syllables_PWSPELL(right)
        result <- paste(left, " ", right_split)
      }
      
      return(result)
      
    }
    
    return(target[word_index])
  }

  #correspondences for biphones
  correspondences <- c("ju"	= "\u0153","je" = "\u2211", "jU" = "\u00AE", "j3" = "\u2020", "ks" = "\u00A5", "kS" = "\u00F8", "nj" = "\u03C0", "gz" = "\u00E5", "gZ" = "\u00DF")
  correspondences_reverse <- setNames(names(correspondences), correspondences)

  #INTERNAL functions: in order to turn biphones into single characters and back again
  replace_patterns <- function(text, correspondences) {
    for (pattern in names(correspondences)) {
      replacement <- correspondences[[pattern]]
      text <- gsub(pattern, replacement, text, fixed = TRUE)
    }
    return(text)
  }
  reverse_replace_patterns <- function(matrix, mapping) {
    for (i in 1:nrow(matrix)) {
      for (j in 1:ncol(matrix)) {
        if (matrix[i, j] %in% names(mapping)) {
          replacement <- mapping[matrix[i, j]]
          matrix[i, j] <- replacement
        }
      }
    }
    return(matrix)
  }

  mylist1 <- list()
  mylist2 <- list()

  #first check if the word has a biphone that could be alternatively parsed, if so use the TRUE as a flag that that word will need a secondary parse in the main for loop below
  check_biphones <- matrix(NA,nrow=length(target))
  for(i in 1:length(target)) {
    check_biphones[i] <- grepl(phonemes_to_check, target[i], ignore.case = FALSE)
  }

  #prep the word for processing
  for(i in 1:length(target)) {
    mylist1[[i]] <- prep_pword(i)
  }

  #now mylist1 has matrices where each word's positional information is in row1 and and the corresponding phonemes are in row2
  #turn these into columns, i.e., make 135 into 1, 3 and 5 (one position/phoneme per column, instead of a string)
  for(i in 1:length(target)){
    mylist2[[i]] <- matrix(strsplit(mylist1[[i]][1,],"")[[1]],ncol=length(strsplit(mylist1[[i]][1,],"")[[1]])) #turn the positions into columns
    mylist2[[i]] <- rbind(mylist2[[i]],matrix(strsplit(mylist1[[i]][2,],"")[[1]],ncol=length(strsplit(mylist1[[i]][2,],"")[[1]]))) #rbind that with the phonemes
  }

  # from pw_spell's get_word_matrix(), at the PG level
  get_word_matrix <- function(i, use_biphone = FALSE) {
    if (!use_biphone) {
      mat <- mylist2[[i]]
      return(rbind(mat, mat[2, ]))
    }

    original_target <- target[i]
    on.exit({ target[i] <<- original_target }, add = TRUE)
    target[i] <<- replace_patterns(target[i], correspondences)

    alt1 <- prep_pword(i)
    alt2 <- matrix(strsplit(alt1[1, ], "")[[1]], ncol = length(strsplit(alt1[1, ], "")[[1]]))
    alt2 <- rbind(alt2, matrix(strsplit(alt1[2, ], "")[[1]], ncol = length(strsplit(alt1[2, ], "")[[1]])))
    alt2 <- reverse_replace_patterns(alt2, correspondences_reverse)
    rbind(alt2, alt2[2, ])
  }

  # NEW: from pw_spell's generate_spellings_from_matrix(), stopping before combine_vectors()
  # builds the product
  letter_group_options <- function(word_mat) {
    mytemp <- list()
    suppressWarnings({
      for (j in 1:ncol(word_mat)) {
        this_rows <- all_tables_PG[["pg"]][all_tables_PG[["pg"]]$phoneme == word_mat[2, j], ]
        this_vals <- as.numeric(as.matrix(this_rows[, as.numeric(word_mat[1, j]) + 2, drop = FALSE]))
        this_graph <- as.character(this_rows[[2]])
        if (length(this_vals) == 0) {
          mytemp[[j]] <- character(0)
        } else {
          mytemp[[j]] <- this_graph[which(is.finite(this_vals) & this_vals > 0)]
        }
      }
    })
    mytemp
  }

  # NEW: one parse from the single sounds and, with a two-sound unit, one keeping it together
  mats <- list(get_word_matrix(1))
  if (check_biphones[1] == TRUE) mats <- c(mats, list(get_word_matrix(1, use_biphone = TRUE)))
  lapply(seq_along(mats), function(k) {
    list(parse_no = k, pos = as.character(mats[[k]][1, ]), ph = as.character(mats[[k]][2, ]),
         o = lapply(letter_group_options(mats[[k]]), as.character))
  })
}
