###TO PROCESS WORDS

#first: load the pg mapping libraries:
require(readxl)

word_initial_mappings <- read_excel("lexicon/word initial mappings_merged.xlsx")
word_final_mappings <- read_excel("lexicon/word final mappings_merged.xlsx")
syllable_medial_mappings <- read_excel("lexicon/syllable medial mappings_merged.xlsx")
syllable_final_mappings <- read_excel("lexicon/syllable final mappings_merged.xlsx")
syllable_initial_mappings <- read_excel("lexicon/syllable initial mappings_merged.xlsx")


#second: load word list
wordlist_v2_1_merged <- read_excel("lexicon/wordlist_v2.1_merged.xlsx")


#third: load functions
replace_accented_vowels <- function(text) {
  # Define a mapping of accented vowels to unaccented vowels
  accented_vowels <- c("á", "é", "ó", "ú", "à", "è", "ì", "ò", "ù", "ä", "ë", "ï", "ö")
  unaccented_vowels <- c("a", "e", "o", "u", "a", "e", "i", "o", "u", "a", "e", "i", "o")
  
  # Replace accented vowels with unaccented vowels
  for (i in seq_along(accented_vowels)) {
    text <- gsub(accented_vowels[i], unaccented_vowels[i], text, fixed = TRUE)
  }
  
  return(text)
}

map_PG <- function(spelling, pronunciation, map_progress=FALSE){
  
  #required packages
  require(stringr)
  require(stringi)
  
  #list vowels and consonants per in-house code
  vowels <- c("5", "O", "8", "je", "j3r", "ju", "jU", "o", "2", 
              "@", "a", "e", "E", "3r", "i", "1", "c", "u", "U", "^")
  
  #ES_ADD:  R is now in this list
  consonants <- c("G", "gz", "kS", "nj", "C", "T", "D", "Z", "N",
                  "b", "d", "f", "g", "h", "j", "k", "l", "m", "n", "p", 
                  "r", "R", "s", "S", "t", "v", "w", "z")
  
  ####################################
  #write internal functions
  ####################################
  
  #prepare word for processing function
  prep_word <- function(word_index){
    
    #get parsed syllables
    hold0 <- as.data.frame(as.vector(sapply(pronunciation[word_index],parse_syllables))) #hold on to the list of syllables
    hold0 <- hold0[hold0!="",] #this will be able to identify if its a word like /ko ko/ or /ha ha/ that repeats the same syllable twice
    parsed_syll <- matrix(strsplit(hold0,"   ")[[1]])
    
    #get letter string, make sure its all lower case
    letter_str <- tolower(spelling[word_index])
    
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
    letter_str0 <- letter_str
    
    return(matrix(c(syll_struc0,parsed_syll0,letter_str0)))
  }
  
  #Maximum Onset Principle phonological parse function
  parse_syllables <- function(pronunciation,word_index=1) {
    library(stringr)
    
    # NEW 12/14/25: don't allow non-rhotic /3/
    has_bad_3 <- function(x) {grepl("3(?!.*r)", x, perl = TRUE)}
    
    # NEW 12/14/25: lock down the *exact* string we are parsing in this call
    current_string <- if (length(pronunciation) == 1L) pronunciation else pronunciation[word_index]
    
    # NEW: fail fast on illegal "3" anywhere
    if (has_bad_3(current_string)) {
      stop(structure(
        list(
          message = paste0("__PARSE_FAIL__:", current_string),
          input = current_string,
          output = NA_character_,
          recon = NA_character_
        ),
        class = c("parse_fail", "error", "condition")
      ))
    }
    
    # NEW 12/14/25: helper for the “collapse back to input” check
    fail_parse <- function(input, output) {
      recon <- gsub(" ", "", output, fixed = TRUE)
      if (!identical(recon, input)) {
        stop(structure(
          list(
            message = paste0("__PARSE_FAIL__:", input),
            input = input, output = output, recon = recon
          ),
          class = c("parse_fail", "error", "condition")
        ))
      }
      output
    }
    
    vowels <- c("5", "O", "8", "je", "j3r", "ju", "jU", "o", "2", 
                "@", "a", "e", "E", "3r", "i", "1", "c", "u", "U", "^")
    
    #ES_ADD: R is now in this list
    consonants <- c("G", "gz", "kS", "nj", "C", "T", "D", "Z", "N",
                    "b", "d", "f", "g", "h", "j", "k", "l", "m", "n", "p", 
                    "r", "R", "s", "S", "t", "v", "w", "z")
    
    #ES_ADD: now includes /Dj/, /Dr/, /jw/, /hw/, /bw/, /Cw/, /skj/,/flw/ and /njw/ as clusters
    syll_init_cons <- c("skl", "flw", "spl", "spr", "str", "skr", "skw", "skj", "njw", "bw", "Cw", "jw", "sw", "pl", "pr", "Dj", "tr", "tw", "kl", "kr", "kw", "bl", "br", "dr", "dw", "gl", "gr", "fl", "fr", "Tr", "Sr", "sl", "st", "sp", "sk", "sm", "sn", "sf", "bj", "fj", "vj","mj","kj","hj", "nj", "pj","tj","Cj","dj","Tj","Gj","gj","Sj","sj","gw", "Dr", "hw", "G", "C")
    
    #ES_ADD: /gn/ is also a valid cluster BUT ONLY WORD-INITIAL (e.g., GNOMO is GNO-MO, but DESIGNIO is DE-SIG-NIO)
    if(substring(current_string,1,2) == "gn") #if the first two sounds are /gn/, allow it as a cluster
      syll_init_cons <- c("skl", "flw", "spl", "spr", "str", "skr", "skw", "skj", "njw", "gn", "bw", "Cw", "jw", "sw", "pl", "pr", "Dj", "tr", "tw", "kl", "kr", "kw", "bl", "br", "dr", "dw", "gl", "gr", "fl", "fr", "Tr", "Sr", "sl", "st", "sp", "sk", "sm", "sn", "sf", "bj", "fj", "vj","mj","kj","hj", "nj", "pj","tj","Cj","dj","Tj","Gj","gj","Sj","sj","gw", "Dr", "hw", "G", "C")
    
    all_phonemes <- c(vowels, syll_init_cons, consonants)
    
    
    phonemes <- c()
    remaining <- current_string
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
      return(current_string)
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
        right_split <- parse_syllables(right)
        result <- paste(left, " ", right_split)
      }
      
      return(fail_parse(current_string, result))
      
    }
    
    return(current_string)
  }
  
  #word-final function
  map_wf <- function(p_wf,syll_struc0,parsed_syll0,letter_str0){
    
    #CRITICAL: if this is the second time around because the map_j flag was set, then p_wf must be overwritten with  p_wf_jointPRIOR
    #notice that the flag was set based on jointPOST, but when looping back through again we now encounter the vowel before the /j/
    #and so must grab that /j/. NB: it has to consider the scenario where /ju/ is one back OR one forward...and it should only do so when it's actually gotten back to the /u/
    if(p_wf =="u")
      tryCatch({
        ifelse(map_j == TRUE & my.index == flag.index_j, 
               if(str_sub(parsed_syll0,-2,-1)=="ju")
                 p_wf<- str_sub(parsed_syll0,-2,-1)
               else
                 if(str_sub(parsed_syll0,-3,-2)=="ju")
                   p_wf<- str_sub(parsed_syll0,-3,-2),
               p_wf <- p_wf)
      },    error = function(e) NA)
    
    
    #look at the phoneme in front of the p_wf, to check if its a vowel. If it is, then keep /z/ --> ES, /s/ --> ES, and /d/ --> ED from being an option
    p_wf_prior <- str_sub(parsed_syll0,-2,-2)
    no_z_es <- FALSE
    no_d_ed <- FALSE
    no_s_es <- FALSE
    if(p_wf_prior %in% vowels & p_wf == "d" | p_wf_prior %in% vowels & p_wf == "z" | p_wf_prior %in% vowels & p_wf == "s") {
      no_z_es <- TRUE
      no_d_ed <- TRUE
      no_s_es <- TRUE
    }else {
      no_z_es <- no_z_es
      no_s_es <- no_s_es
      no_d_ed <- no_d_ed
    }
    
    #ES_ADD: don't allow any vowel to [H+VOWEL] if the H is needed for [CH] with /C/
    no_aeiou_haeiou <- FALSE
    ifelse(p_wf_prior == "C" & p_wf %in% c("a","i","E","o","u") & stri_sub(letter_str0,-3,-3) == "c", no_aeiou_haeiou <- TRUE, no_aeiou_haeiou <- no_aeiou_haeiou)
    
    #EN_ADD: don't allow any vowel to [H+VOWEL] if the H is needed for [H] with /h/
    no_aeiou_haeiou <- no_aeiou_haeiou
    ifelse(p_wf_prior == "h" & p_wf %in% c("a","i","E","o","u") & stri_sub(letter_str0,-2,-2) == "h", no_aeiou_haeiou <- TRUE, no_aeiou_haeiou <- no_aeiou_haeiou)
    
    #don't allow /z/ --> DS if p_wf_prior is actually a /d/
    no_z_ds <- FALSE
    if(p_wf_prior == "d" & p_wf == "z")
      no_z_ds <- TRUE
    
    #don't allow /d/ --> LD if p_wf_prior is actually a /l/
    no_d_ld <- FALSE
    if(p_wf_prior == "l" & p_wf == "d")
      no_d_ld <- TRUE
    
    #don't allow /t/ --> CT or --> BT if p_wf_prior is actually a /k/ or /b/
    no_t_ct <- FALSE
    no_t_bt <- FALSE
    if(p_wf_prior == "k" & p_wf == "t") {no_t_ct <- TRUE}
    if(p_wf_prior == "b" & p_wf == "t") {no_t_bt <- TRUE}
    
    #don't allow /e/[schwa] --> IA or EA if p_wf_prior is actually a /j/ or /i/, or /2/ [/oi/]
    #EN_ADD (4/22/25): also block it if the I is needed for [SI] like in A-SI-A (i.e., block it from being A-S-IA)--but not if the E is needed as in NAUSEA
    no_e_ia <- FALSE
    no_e_ea <- FALSE
    if(p_wf_prior == "j" & p_wf == "e" | p_wf_prior == "i" & p_wf == "e" | p_wf_prior == "2" & p_wf == "e" | p_wf_prior %in% c("S","Z") & p_wf == "e") {
      no_e_ia <- TRUE
    }
    if(p_wf_prior == "j" & p_wf == "e" | p_wf_prior == "i" & p_wf == "e" | p_wf_prior == "2" & p_wf == "e" | p_wf_prior %in% c("S") & p_wf == "e") {
      no_e_ea <- TRUE
    }
    
    #don't allow /k/ --> LK if p_wf_prior is actually a /l/
    no_k_lk <- FALSE
    if(p_wf_prior == "l" & p_wf == "k") {
      no_k_lk <- TRUE
    }
    
    #don't allow /n/ --> LN if p_wf_prior is actually a /l/
    no_n_ln <- FALSE
    if(p_wf_prior == "l" & p_wf == "n") {
      no_n_ln <- TRUE
    }
    
    
    #don't allow /f/ --> LF if p_wf_prior is actually a /l/
    no_f_lf <- FALSE
    if(p_wf_prior == "l" & p_wf == "f") {
      no_f_lf <- TRUE
    }
    
    #don't allow /m/ --> LM if p_wf_prior is actually a /l/
    no_m_lm <- FALSE
    if(p_wf_prior == "l" & p_wf == "m") {
      no_m_lm <- TRUE
    }
    
    #don't allow /s/ --> TZ if p_wf_prior is actually a /t/
    no_s_tz <- FALSE
    if(p_wf_prior == "t" & p_wf == "s") {
      no_s_tz <- TRUE
    }
    
    #don't allow /v/ --> LV if p_wf_prior is actually a /l/
    no_v_lv <- FALSE
    if(p_wf_prior == "l" & p_wf == "v") {
      no_v_lv <- TRUE
    }
    
    #EN_ADD: make sure [TI] is used for /S/ even if the [I] could go to [IE] or [IA] like in "portia", which are schwa /e/ options
    no_ieia_schwa <- FALSE
    if(p_wf == "e" & p_wf_prior == "S" & stri_sub(letter_str0,-3,-3) == "t"){ #if the word ends in schwa but next is a /S/ and there will be a letter T in the right spot...
      no_ieia_schwa <- TRUE
    }
    
    #NEW 12/15/25: don't allow /C/ --> TCH if p_wf_prior is actually /t/
    no_C_tch <- FALSE
    if(p_wf_prior == "t" & p_wf == "C") {
      no_C_tch <- TRUE
    }
    
    #look at the 2 phonemes in front of the p_wf, to check if they are "3r", like in ACRES "8k3rz". If they are, then keep /z/ --> ES and /d/ --> ED from being an option
    #also, in order to block /l/ mapped to SL if there is an /s/ like in HASSLE
    p_wf_2prior <- str_sub(parsed_syll0,-3,-2)
    if(p_wf_2prior == "3r"){
      no_z_es <- TRUE
      no_d_ed <- TRUE
    }
    if(p_wf_2prior == "se"){
      no_l_sl <- TRUE
    } else {no_l_sl <- FALSE}
    
    #extra block of /z/ --> ES: for ONES and such, which need to map the /^/ to _e
    ifelse(p_wf_2prior == "^n" & p_wf == "z", no_z_es <- TRUE, no_z_es <- no_z_es)
    
    ##CHANGED THIS TO ADDRESS THE "ABANDONED" ISSUE: was "e+Consonant" now "e+L
    #look at the 2 phonemes in front of the p_wf, to check if they are "e+L", like in TABLES "t8belz". If they are, then keep /z/ --> ES and /d/ --> ED from being an option
    p_wf_anteprior <- str_sub(parsed_syll0,-3,-3)
    if(p_wf_anteprior == "e" & p_wf_prior == "l"){
      no_z_es <- TRUE
      no_d_ed <- TRUE
    }else {
      no_z_es <- no_z_es
      no_d_ed <- no_d_ed
    }
    
    #look at the final two phonemes jointly to see if they are /ks/ AND whether the last grapheme is X -OR!- XE
    p_wf_jointprior <- str_sub(parsed_syll0,-2,-1)
    wf_x <- str_sub(letter_str0,-1,-1)
    map_x = FALSE
    ifelse (wf_x=="x", ifelse(p_wf_jointprior=="ks",map_x <- TRUE, map_x <- FALSE), map_x <- FALSE)
    
    #now check if its ending in XE...
    wf_xe <- str_sub(letter_str0,-2,-1)
    map_xe = FALSE
    ifelse (wf_xe=="xe", ifelse(p_wf_jointprior=="ks",map_xe <- TRUE, map_xe <- FALSE), map_xe <- FALSE)
    
    #find the options:
    g_opt <- word_final_mappings[word_final_mappings$phoneme==p_wf,]
    
    #if it's gone WAY wrong, there will be no options and it will crash. The following lines will put in a dummy placeholder phoneme and grapheme "6" to avoid this:
    if(nrow(g_opt)==0){
      g_opt <- word_final_mappings[word_final_mappings$phoneme=="s",][1,] #pull out the first option for 's' (which is possible in any position so guaranteed to be in the list of options)
      g_opt[1,1] <- "6"
      g_opt[1,2] <- "6"
    }
    
    #remove the following as needed
    ifelse(p_wf_prior == "p" & p_wf == "t", g_opt <- g_opt[!g_opt$grapheme=="pt",], g_opt <- g_opt)
    ifelse(p_wf_prior == "b" & p_wf == "t", g_opt <- g_opt[!g_opt$grapheme=="bt",], g_opt <- g_opt)
    ifelse(no_z_es == TRUE, g_opt  <- g_opt[!g_opt$grapheme=="es",], g_opt <- g_opt)
    ifelse(no_s_es == TRUE, g_opt  <- g_opt[!g_opt$grapheme=="es",], g_opt <- g_opt)
    ifelse(no_z_ds == TRUE, g_opt  <- g_opt[!g_opt$grapheme=="ds",], g_opt <- g_opt)
    ifelse(no_d_ed == TRUE, g_opt  <- g_opt[!g_opt$grapheme=="ed",], g_opt <- g_opt)
    ifelse(no_d_ld == TRUE, g_opt  <- g_opt[!g_opt$grapheme=="ld",], g_opt <- g_opt)
    ifelse(no_e_ia == TRUE, g_opt  <- g_opt[!g_opt$grapheme=="ia",], g_opt <- g_opt)
    ifelse(no_e_ea == TRUE, g_opt  <- g_opt[!g_opt$grapheme=="ea",], g_opt <- g_opt)
    ifelse(no_t_ct == TRUE, g_opt  <- g_opt[!g_opt$grapheme=="ct",], g_opt <- g_opt)
    ifelse(no_t_bt == TRUE, g_opt  <- g_opt[!g_opt$grapheme=="bt",], g_opt <- g_opt)
    ifelse(no_k_lk == TRUE, g_opt  <- g_opt[!g_opt$grapheme=="lk",], g_opt <- g_opt)
    ifelse(no_n_ln == TRUE, g_opt  <- g_opt[!g_opt$grapheme=="ln",], g_opt <- g_opt)
    ifelse(no_f_lf == TRUE, g_opt  <- g_opt[!g_opt$grapheme=="lf",], g_opt <- g_opt)
    ifelse(no_m_lm == TRUE, g_opt  <- g_opt[!g_opt$grapheme=="lm",], g_opt <- g_opt)
    ifelse(no_s_tz == TRUE, g_opt  <- g_opt[!g_opt$grapheme=="tz",], g_opt <- g_opt)
    ifelse(no_v_lv == TRUE, g_opt  <- g_opt[!g_opt$grapheme=="lv",], g_opt <- g_opt)
    ifelse(no_l_sl == TRUE, g_opt  <- g_opt[!g_opt$grapheme=="sl",], g_opt <- g_opt)
    ifelse(no_C_tch == TRUE, g_opt  <- g_opt[!g_opt$grapheme=="tch",], g_opt <- g_opt)
    ifelse(no_ieia_schwa == TRUE, g_opt  <- g_opt[!g_opt$grapheme %in% c("ie","ia"),], g_opt <- g_opt)
    #ES_ADD: ones added for Spanish here
    ifelse(no_aeiou_haeiou == TRUE, g_opt  <- g_opt[!g_opt$grapheme %in% c("ha","he","hi","ho","hu"),], g_opt <- g_opt)
    
    
    #overwrite those options if the map_x flag is TRUE, i.e., switch it to the 'x' grapheme options
    ifelse(map_x, g_opt <- word_final_mappings[word_final_mappings$grapheme=="x",], g_opt <- g_opt)
    
    #do similar if the map_xe flag is TRUE
    ifelse(map_xe, g_opt <- word_final_mappings[word_final_mappings$grapheme=="x",], g_opt <- g_opt)
    
    n_opt <- length(g_opt$grapheme)
    search_lists <- list()
    
    for(j in (1:n_opt)){
      search_lists[[j]] <- matrix(NA,nrow=1,ncol=2)
      #first col should give grapheme latest location
      search_lists[[j]][1] <- max(unlist(gregexpr(g_opt$grapheme[j], letter_str0))) + nchar(g_opt$grapheme[j]) -1 #the max() gets the latest location of the START
      #of the grapheme...the nchar() gets the length of the grapheme...and the -1 is to get the final position. e.g., the DG in EDGE starts at char 2, its length is 2,
      #so the position it ends is 2+2-1 = 3
      #the above line returns false results sometimes when the grapheme isn't even present--e.g., it will return MIXED as having an AI_E just because the value comes out as 2 instead of -1, due to the length of AI_E
      #to fix this, simply overwrite the search_lists[[j]][1] value with gregexpr(g_opt$grapheme[j], letter_str0)[[1]][1] if that equals -1
      ifelse (gregexpr(g_opt$grapheme[j], letter_str0)[[1]][1] == -1, search_lists[[j]][1] <- -1, search_lists[[j]][1] <- search_lists[[j]][1])
      #second col gives the grapheme length 
      search_lists[[j]][2] <- nchar(g_opt$grapheme[j])
    }
    
    #unlist to get a matrix for selecting the grapheme
    select_g <- t(matrix(unlist(search_lists),ncol=n_opt,nrow=2))
    
    #code to get which grapheme is the match--note that it finds the max of latest position, but there can be ties
    #so it further looks at max of grapheme length to break the tie. the code is longer still because
    #we need the original index of the grapheme, i.e., the absolute number, not relative (e.g., if the tie is between
    #grapheme #2 and #8, and #2 should win because its longer, this code returns #2 [as opposed to #1, meaning the first of
    #the tied options])
    g_wf <- which(select_g[,1]==max(select_g[,1]))[which.max(select_g[which(select_g[,1]==max(select_g[,1])),2])]
    g_wf <- g_opt$grapheme[g_wf]
    
    #NOTE: to make sure this FAILS for things it doesn't know, it needs to fail when the max of search list is <1
    ifelse(max(select_g[,1]) < 1, g_wf <- "FAILED", g_wf <- g_wf)
    
    #update the letter_str reflect what has been mapped... 
    #note: this is where the string must be reversed, as well as the grapheme, in order to only replace the LAST instance
    #there does not seem to be any ready functions for doing so otherwise (only replace-first or replace-all functions)
    letter_str.latest <- stri_reverse(str_replace(stri_reverse(letter_str0),stri_reverse(g_wf),"_"))
    
    #if there is an _ underscore followed by something OTHER than "e", this is a FAIL! set the flag.index and restart...
    if( str_sub(letter_str.latest,-2,-2)=="_"){
      if(str_sub(letter_str.latest,-1,-1)!="e"){
        if(p_wf_prior == "j"){
          flag.restart <<- TRUE
          flag.index_j <<- my.index
          map_j <<- TRUE
        }else{
          flag.restart <<- TRUE
          flag.index <<- my.index
        }}
    }
    
    
    #and same issue but if the _ is even more out of place, i.e, if it's not the next-to-last...
    if( tail(unlist(gregexpr('_', letter_str.latest)),1) == nchar(letter_str.latest)-1 | tail(unlist(gregexpr('_', letter_str.latest)),1) == nchar(letter_str.latest) | tail(unlist(gregexpr('_', letter_str.latest)),1) == -1) { #if there is an _ anywhere but final or next to final...tail() is needed because there could be two __
      flag.restart <- flag.restart
    }else{
      if(p_wf_prior == "j"){
        flag.restart <<- TRUE
        map_j <<- TRUE
        flag.index_j <<- my.index
      }else{
        flag.restart <<- TRUE
        flag.index <<- my.index #index will be for THIS pass because it's word final--currently this means the word will not get mapped after 10 aborts
      }}
    
    
    #if the replacement with _ procedure has left the last character as an E, then keep the _ so that the silent/final E can be mapped
    #if however the last character is now the _, simply remove it
    ifelse( str_sub(letter_str.latest,-1,-1)=="e", letter_str.latest <- letter_str.latest, letter_str.latest <- str_sub(letter_str.latest, end = -2))
    
    #now also update the parsed_syll to reflect what has been mapped. this is easier in general it's always the last phoneme
    #however, the X scenario must trigger removing /ks/
    parsed_syll.latest <- str_sub(parsed_syll0,1,-(nchar(p_wf)+1))
    
    #to handle /ks/ for X...the flag map_x should trigger 1 more phoneme to be removed
    ifelse (map_x, parsed_syll.latest <- str_sub(parsed_syll0,1,-3), parsed_syll.latest <- parsed_syll.latest)
    
    #to handle /ks/ for XE...the flag map_xe should trigger 1 more phoneme to be removed, 
    ifelse (map_xe, parsed_syll.latest <- str_sub(parsed_syll0,1,-3), parsed_syll.latest <- parsed_syll.latest)
    
    #update p.wf to correctly reflect that the /k/ was added for the X, i.e., make it /ks/ not just /s/
    ifelse (map_x, p_wf <- p_wf_jointprior, p_wf <- p_wf)
    ifelse (map_xe, p_wf <- p_wf_jointprior, p_wf <- p_wf)
    
    
    #finally, have the latest syll_struc updated...if the X flag is set to remove an additional final syllable
    syll_struc.latest <- str_sub(syll_struc0,1,-(nchar(p_wf)+1))
    
    #list structure to store the mappings just obtained: the P, the G, and the position
    PGlist <- list()
    
    #the following stores the PHONEME, the GRAPHEME, and the POSITION, in that order:
    PGlist <- append(PGlist,matrix(c(p_wf,g_wf,"5",syll_struc0,syll_struc.latest,parsed_syll.latest,letter_str.latest)))
    
    return(PGlist)
  }
  
  #syllable-medial function
  map_m <- function(p_m,syll_struc.latest,parsed_syll.latest,letter_str.latest){
    
    #CRITICAL: if this is the second time around because the map_j flag was set, then p_m must be overwritten with the p_m_jointPRIOR
    #notice that the flag was set based on jointPOST, but when looping back through again we now encounter the vowel before the /j/
    #and so must grab that /j/. NB: it has to consider the scenario where /ju/ is one back OR one forward...and it should only do so when it's actually gotten back to the /u/
    if(p_m =="u")
      tryCatch({
        ifelse(map_j == TRUE & my.index == flag.index_j, 
               if(str_sub(parsed_syll.latest,-2,-1)=="ju")
                 p_m<- str_sub(parsed_syll.latest,-2,-1)
               else
                 if(str_sub(parsed_syll.latest,-3,-2)=="ju")
                   p_m<- str_sub(parsed_syll.latest,-3,-2),
               p_m <- p_m)
      },    error = function(e) NA)
    
    #do the same if its "je" instead of "ju"
    if(p_m =="e")
      tryCatch({
        ifelse(map_j == TRUE & my.index == flag.index_j, 
               if(str_sub(parsed_syll.latest,-2,-1)=="je")
                 p_m<- str_sub(parsed_syll.latest,-2,-1)
               else
                 if(str_sub(parsed_syll.latest,-3,-2)=="je")
                   p_m<- str_sub(parsed_syll.latest,-3,-2),
               p_m <- p_m)
      },    error = function(e) NA)
    
    #do the same if its "jU" instead of "ju"
    if(p_m =="U")
      tryCatch({
        ifelse(map_j == TRUE & my.index == flag.index_j, 
               if(str_sub(parsed_syll.latest,-2,-1)=="jU")
                 p_m<- str_sub(parsed_syll.latest,-2,-1)
               else
                 if(str_sub(parsed_syll.latest,-3,-2)=="jU")
                   p_m<- str_sub(parsed_syll.latest,-3,-2),
               p_m <- p_m)
      },    error = function(e) NA)
    
    #do the same if its "j3" instead of "j3"
    if(p_m =="3")
      tryCatch({
        ifelse(map_j == TRUE & my.index == flag.index_j, 
               if(str_sub(parsed_syll.latest,-2,-1)=="j3")
                 p_m<- str_sub(parsed_syll.latest,-2,-1)
               else
                 if(str_sub(parsed_syll.latest,-3,-2)=="j3")
                   p_m<- str_sub(parsed_syll.latest,-3,-2),
               p_m <- p_m)
      },    error = function(e) NA)
    
    
    #look at the phoneme in front of the p_m, to check if its a vowel. If it is, then keep /z/ --> ES from being an option
    #if its /c/ or /3/ and current p_m is /r/, don't allow /r/ --> OR ... or /d/ --> ED
    p_m_prior <- str_sub(parsed_syll.latest,-2,-2)
    no_z_es <- FALSE
    no_r_or <- FALSE
    no_d_ed <- FALSE
    no_s_tz <- FALSE
    no_t_pt <- FALSE
    no_a_aha <- FALSE
    no_v_lv <- FALSE
    no_u_wo <- FALSE
    no_k_lk <- FALSE
    no_n_gn <- FALSE #ES_ADD for words like GNOMO
    ifelse(p_m_prior %in% vowels & p_m == "z", no_z_es <- TRUE, no_z_es <- no_z_es)
    ifelse(p_m_prior %in% vowels & p_m == "d", no_d_ed <- TRUE, no_z_es <- no_d_ed)
    ifelse(p_m_prior == "c" & p_m == "r" | p_m_prior == "3" & p_m == "r", no_r_or <- TRUE, no_r_or <- no_r_or)
    ifelse(p_m_prior == "t" & p_m == "s" , no_s_tz <- TRUE, no_s_tz <- no_s_tz)
    ifelse(p_m_prior == "p" & p_m == "t" , no_t_pt <- TRUE, no_t_pt <- no_t_pt)
    ifelse(p_m_prior == "h" & p_m == "@" , no_a_aha <- TRUE, no_a_aha <- no_a_aha)
    ifelse(p_m_prior == "l" & p_m == "v" , no_v_lv <- TRUE, no_v_lv <- no_v_lv)
    ifelse(p_m_prior == "w" & p_m == "u" , no_u_wo <- TRUE, no_u_wo <- no_u_wo)
    ifelse(p_m_prior == "l" & p_m == "k" , no_k_lk <- TRUE, no_k_lk <- no_k_lk)
    ifelse(p_m_prior == "g" & p_m == "n", no_n_gn <- TRUE, no_n_gn <- no_n_gn) #ES_ADD for words like GNOMO
    
    #ES_ADD: don't allow /w/ to [HU] if the H is needed for [CH] with /C/
    no_w_hu <- FALSE
    ifelse(p_m_prior == "C" & p_m == "w", no_w_hu <- TRUE, no_w_hu <- no_w_hu)
    
    #ES_ADD: don't allow any vowel to [H+VOWEL] if the H is needed for [CH] with /C/ or /S/ or for [H] with /h/, /T/, /D/ or /f/ (i.e., due to /T/ or /D/ to TH and /f/ to PH) or for [GH] with /g/ or for [WH] for /w/
    no_aeiou_haeiou <- FALSE
    ifelse(p_m_prior %in% c("C","S") & p_m %in% c("a","i","E","o","u") & stri_sub(letter_str.latest,-3,-3) == "c" || p_m_prior %in% c("h","T","D","f") & p_m %in% c("a","i","E","o","u") & stri_sub(letter_str.latest,-2,-2) == "h" || p_m_prior %in% c("g","w") & p_m %in% c("a","i","E","o","u") & stri_sub(letter_str.latest,-3,-3) %in% c("g","w"), no_aeiou_haeiou <- TRUE, no_aeiou_haeiou <- no_aeiou_haeiou)
    
    #ES_ADD: don't do /j/ to [HI] if preceded by /C/ because the H will be needed for that
    no_j_hi <- FALSE
    ifelse(p_m == "j" & p_m_prior == "C", no_j_hi <- TRUE, no_j_hi <- no_j_hi)
    
    #ES_ADD: don't allow /r/ to [OR] if the O is needed for /o/--note an no_r_or flag exists for English but for the /3/ sound
    ifelse(p_m_prior == "o" & p_m == "r", no_r_or <- TRUE, no_r_or <- no_r_or)
    
    #ES_ADD: be proactive in looking for ñ mappings
    check_for_enye <- FALSE
    if(stri_sub(letter_str.latest,-1,-1) == "ñ") check_for_enye <- TRUE
    if(p_m == "j" & p_m_prior == "n" & check_for_enye == TRUE) p_m <- "nj"
    
    #EN_ADD: proactively looks for [gn] to /nj/ mappings, although maybe that only is needed for "lasagna"
    check_for_gn <- FALSE
    if(stri_sub(letter_str.latest,-2,-1) == "gn" & p_m == "j" & p_m_prior == "n") check_for_gn <- TRUE
    if(check_for_gn == TRUE) p_m <- "nj" #THIS WILL ALSO REQUIRE UPDATING SYLLABIC POSITION from medial to syll init
    
    #don't allow /e/[schwa] --> IA or EA or IE or IA_E if p_m_prior is actually a /j/ or /i/
    no_e_ia <- FALSE
    no_e_ea <- FALSE
    no_e_ie <- FALSE
    no_e_ia_e <- FALSE
    if(p_m_prior == "j" & p_m == "e" | p_m_prior == "i" & p_m == "e") {
      no_e_ia <- TRUE
      no_e_ea <- TRUE
      no_e_ie <- TRUE
      no_e_ia_e <- TRUE
    }
    
    #EN_ADD: also don't allow schwa to IA or IE if it's after /S/ spelled TI, i.e., the I is needed for TI-->/s/
    if(p_m_prior == "S" & p_m == "e" & stri_sub(letter_str.latest,-3,-3) == "t") {
      no_e_ia <- TRUE
      no_e_ie <- TRUE
    }
    
    #ES_ADD: don't allow /E/--> IE, or to AYE, if p_m_prior is actually /j/
    no_E_ie <- FALSE
    no_E_aye <- FALSE
    if(p_m_prior == "j" & p_m == "E") {
      no_E_ie <- TRUE
      no_E_aye <- TRUE
    }
    
    #don't allow /e/[schwa] --> UI if it's after /w/ like PENGUIN
    no_e_ui <- FALSE
    if(p_m_prior == "w" & p_m == "e") { no_e_ui <- TRUE}
    
    #if its /w/ , don't allow ^ to be o_e because it needs to be just _e as in ANYONE (i.e., it will map it as ^ to O_E leaving /w/ without a mapping)
    #and don't allow /1/ to be ui because it's probably a word like PENGUIN
    #but a hypothetical e.g. THWUILT read /Tw1lt/ could map UI to /1/, so allow it if there is a [W] before [UI]
    no_o_e <- FALSE
    no_1_ui <- FALSE
    ifelse(p_m_prior == "w" & p_m == "^", no_o_e <- TRUE, no_o_e <- no_o_e)
    ifelse(p_m_prior == "w" & p_m == "1" & stri_sub(letter_str.latest,-3,-3) != "w", no_1_ui <- TRUE, no_1_ui <- no_1_ui) #added & stri_sub(letter_str.latest,-3,-3) != "w" for hypothetical THWUILT~/Tw1lt/
    
    #don't allow /3/ to map to HE medially unless its after /p/, because currently only SHEPHERD does this
    no_3_he <- FALSE
    ifelse(p_m_prior != "p" & p_m == "3", no_3_he <- TRUE, no_3_he <- no_3_he)
    
    #don't allow /m/ --> LM if preceded by /l/
    no_m_lm <- FALSE
    ifelse(p_m_prior == "l" & p_m == "m", no_m_lm <- TRUE, no_m_lm <- no_m_lm)
    
    #look at the final two phonemes jointly to see if they are /ks/ AND whether the last grapheme is X -OR!- XE
    p_m_jointprior <- str_sub(parsed_syll.latest,-2,-1)
    m_x <- str_sub(letter_str.latest,-1,-1)
    map_x = FALSE
    ifelse (m_x=="x", ifelse(p_m_jointprior=="ks",map_x <- TRUE, map_x <- FALSE), map_x <- FALSE)
    
    #now check if its ending in XE...
    m_xe <- str_sub(letter_str.latest,-2,-1)
    map_xe = FALSE
    ifelse (m_xe=="xe", ifelse(p_m_jointprior=="ks",map_xe <- TRUE, map_xe <- FALSE), map_xe <- FALSE)
    
    #check 2prior, if it's /3r/ then keep the z_es and d_ed blocks:
    p_m_2prior <- str_sub(parsed_syll.latest,-3,-2)
    if(p_m_2prior == "3r"){
      ifelse(p_m == "z",no_z_es <- TRUE, no_z_es <- no_z_es)
      ifelse(p_m == "d",no_d_ed <- TRUE, no_d_ed <- no_d_ed)
    }
    
    
    
    #find the options:
    g_opt <- syllable_medial_mappings[syllable_medial_mappings$phoneme==p_m,]
    
    #if it's gone WAY wrong, there will be no options and it will crash. The following lines will put in a dummy placeholder phoneme and grapheme "6" to avoid this:
    if(nrow(g_opt)==0){
      g_opt <- syllable_medial_mappings[syllable_medial_mappings$phoneme=="s",][1,] #pull out the first option for 's' (which is possible in any position so guaranteed to be in the list of options)
      g_opt[1,1] <- "6"
      g_opt[1,2] <- "6"
    }
    
    #CRITICAL if this is the second time around because whatever comes AFTER this failed, then flag.index will match the current my.index
    #currently the code thinks this may be because a silent H was assigned, so this should BLOCK that from happening
    if(flag.index == my.index){
      if(p_m == "3"){g_opt <- g_opt[-which(g_opt$grapheme=="he"),]} #removes the row from g_opt that has "he" as an option for /3/
    }
    
    #or it could be there is an X coming up an so the syllabic structure is problematic at an earlier point, which happens with EXHAUST
    #to check that, look for an X and remap the whole structure and then restart
    if(flag.index == my.index){
      if (grepl("x",letter_str.latest) & grepl("gz",parsed_syll.latest)) { #if there is an X and /gz/ coming up...
        stri_sub(syll_struc0, unlist(gregexpr('gz', parsed_syll.latest))+1,unlist(gregexpr('gz', parsed_syll.latest))+1) <- "4" #make the /gz/ syllable final
        if(stri_sub(syll_struc0, unlist(gregexpr('gz', parsed_syll.latest))+2,unlist(gregexpr('gz', parsed_syll.latest))+2)=="3"){ #if whatever was after was medial...
          stri_sub(syll_struc0, unlist(gregexpr('gz', parsed_syll.latest))+2,unlist(gregexpr('gz', parsed_syll.latest))+2) <- "2" #make it syll_initial
        }}
      syll_struc0 <<- syll_struc0
      flag.restart <<- TRUE
      flag.index <<- 0 #reset the index!
    }
    
    if(flag.index == my.index){
      if (grepl("x",letter_str.latest) & grepl("ks",parsed_syll.latest)) { #if there is an X and /ks/ coming up...
        stri_sub(syll_struc0, unlist(gregexpr('ks', parsed_syll.latest))+1,unlist(gregexpr('ks', parsed_syll.latest))+1) <- "4" #make the /ks/ syllable final
        if(stri_sub(syll_struc0, unlist(gregexpr('ks', parsed_syll.latest))+2,unlist(gregexpr('ks', parsed_syll.latest))+2)=="3"){ #if whatever was after was medial...
          stri_sub(syll_struc0, unlist(gregexpr('ks', parsed_syll.latest))+2,unlist(gregexpr('ks', parsed_syll.latest))+2) <- "2" #make it syll_initial
        }}
      syll_struc0 <<- syll_struc0
      flag.restart <<- TRUE
      flag.index <<- 0 #reset the index!
    }
    
    if(flag.index == my.index){
      if (grepl("x",letter_str.latest) & grepl("kS",parsed_syll.latest)) { #if there is an X and /kS/ coming up...
        stri_sub(syll_struc0, unlist(gregexpr('kS', parsed_syll.latest))+1,unlist(gregexpr('kS', parsed_syll.latest))+1) <- "4" #make the /kS/ syllable final
        if(stri_sub(syll_struc0, unlist(gregexpr('kS', parsed_syll.latest))+2,unlist(gregexpr('kS', parsed_syll.latest))+2)=="3"){ #if whatever was after was medial...
          stri_sub(syll_struc0, unlist(gregexpr('kS', parsed_syll.latest))+2,unlist(gregexpr('kS', parsed_syll.latest))+2) <- "2" #make it syll_initial
        }}
      syll_struc0 <<- syll_struc0
      flag.restart <<- TRUE
      flag.index <<- 0 #reset the index!
    }
    
    
    #the following lines remove things as needed
    ifelse(no_z_es==TRUE, g_opt  <- g_opt[!g_opt$grapheme=="es",], g_opt <- g_opt)
    ifelse(no_d_ed==TRUE, g_opt  <- g_opt[!g_opt$grapheme=="ed",], g_opt <- g_opt)
    ifelse(no_r_or==TRUE, g_opt  <- g_opt[!g_opt$grapheme=="or",], g_opt <- g_opt)
    ifelse(no_o_e==TRUE, g_opt  <- g_opt[!g_opt$grapheme=="o_e",], g_opt <- g_opt)
    ifelse(no_3_he==TRUE, g_opt  <- g_opt[!g_opt$grapheme=="he",], g_opt <- g_opt)
    ifelse(no_e_ie==TRUE, g_opt  <- g_opt[!g_opt$grapheme=="ie",], g_opt <- g_opt)
    ifelse(no_e_ia_e==TRUE, g_opt  <- g_opt[!g_opt$grapheme=="ia_e",], g_opt <- g_opt)
    ifelse(no_e_ea==TRUE, g_opt  <- g_opt[!g_opt$grapheme=="ea",], g_opt <- g_opt)
    ifelse(no_e_ia==TRUE, g_opt  <- g_opt[!g_opt$grapheme=="ia",], g_opt <- g_opt)
    ifelse(no_1_ui==TRUE, g_opt  <- g_opt[!g_opt$grapheme=="ui",], g_opt <- g_opt)
    ifelse(no_m_lm==TRUE, g_opt  <- g_opt[!g_opt$grapheme=="lm",], g_opt <- g_opt)
    ifelse(no_e_ui==TRUE, g_opt  <- g_opt[!g_opt$grapheme=="ui",], g_opt <- g_opt)
    ifelse(no_s_tz==TRUE, g_opt  <- g_opt[!g_opt$grapheme=="tz",], g_opt <- g_opt)
    ifelse(no_t_pt==TRUE, g_opt  <- g_opt[!g_opt$grapheme=="pt",], g_opt <- g_opt)
    ifelse(no_a_aha==TRUE, g_opt  <- g_opt[!g_opt$grapheme=="aha",], g_opt <- g_opt)
    ifelse(no_v_lv==TRUE, g_opt  <- g_opt[!g_opt$grapheme=="lv",], g_opt <- g_opt)
    ifelse(no_u_wo==TRUE, g_opt  <- g_opt[!g_opt$grapheme=="wo",], g_opt <- g_opt)
    ifelse(no_k_lk==TRUE, g_opt  <- g_opt[!g_opt$grapheme=="lk",], g_opt <- g_opt)
    #ES_ADD: more blocking of options, would be explained above
    ifelse(no_E_ie==TRUE, g_opt  <- g_opt[!g_opt$grapheme=="ie",], g_opt <- g_opt)
    ifelse(no_E_aye==TRUE, g_opt  <- g_opt[!g_opt$grapheme=="aye",], g_opt <- g_opt)
    ifelse(no_w_hu==TRUE, g_opt  <- g_opt[!g_opt$grapheme=="hu",], g_opt <- g_opt)
    ifelse(no_n_gn==TRUE, g_opt  <- g_opt[!g_opt$grapheme=="gn",], g_opt <- g_opt)
    ifelse(no_aeiou_haeiou==TRUE, g_opt  <- g_opt[!g_opt$grapheme %in% c("ha","he","hi","ho","hu"),], g_opt <- g_opt)
    ifelse(no_j_hi==TRUE, g_opt  <- g_opt[!g_opt$grapheme=="hi",], g_opt <- g_opt)
    
    
    #overwrite those options if the map_x flag is TRUE, i.e., switch it to the 'x' grapheme options
    ifelse(map_x, g_opt <- syllable_medial_mappings[syllable_medial_mappings$grapheme=="x",], g_opt <- g_opt)
    
    #do similar if the map_xe flag is TRUE
    ifelse(map_xe, g_opt <- syllable_medial_mappings[syllable_medial_mappings$grapheme=="x",], g_opt <- g_opt)
    
    n_opt <- length(g_opt$grapheme)
    search_lists <- list()
    
    for(j in (1:n_opt)){
      search_lists[[j]] <- matrix(NA,nrow=1,ncol=2)
      #first col should give grapheme latest location
      search_lists[[j]][1] <- max(unlist(gregexpr(g_opt$grapheme[j], letter_str.latest))) + nchar(g_opt$grapheme[j]) -1 #the max() gets the latest location of the START
      #of the grapheme...the nchar() gets the length of the grapheme...and the -1 is to get the final position. e.g., the DG in EDGE starts at char 2, its length is 2,
      #so the position it ends is 2+2-1 = 3
      #the above line returns false results sometimes when the grapheme isn't even present--e.g., it will return MIXED as having an AI_E just because the value comes out as 2 instead of -1, due to the length of AI_E
      #to fix this, simply overwrite the search_lists[[j]][1] value with gregexpr(g_opt$grapheme[j], letter_str.latest)[[1]][1] if that equals -1
      ifelse (gregexpr(g_opt$grapheme[j], letter_str.latest)[[1]][1] == -1, search_lists[[j]][1] <- -1, search_lists[[j]][1] <- search_lists[[j]][1])
      #second col gives the grapheme length 
      search_lists[[j]][2] <- nchar(g_opt$grapheme[j])
    }
    
    #unlist to get a matrix for selecting the grapheme
    select_g <- t(matrix(unlist(search_lists),ncol=n_opt,nrow=2))
    
    #code to get which grapheme is the match--note that it finds the max of latest position, but there can be ties
    #so it further looks at max of grapheme length to break the tie. the code is longer still because
    #we need the original index of the grapheme, i.e., the absolute number, not relative (e.g., if the tie is between
    #grapheme #2 and #8, and #2 should win because its longer, this code returns #2 [as opposed to #1, meaning the first of
    #the tied options])
    g_m <- which(select_g[,1]==max(select_g[,1]))[which.max(select_g[which(select_g[,1]==max(select_g[,1])),2])]
    g_m <- g_opt$grapheme[g_m]
    
    #ES_ADD: with the new proactive ñ check, the syllabic position may not actually be "3" here--it should be whatever the /n/ had (probably 2)
    #so we no longer hard code in "3", per se
    true_syllabic_position <- "3" #default is that we're currently mapping a medial position
    if(check_for_enye == TRUE & g_m == "ñ" & p_m == "nj") #if the check_for_enye was confirmed, p_m was updated, and we did set g_m to be ñ, then:
      true_syllabic_position <- as.character(stri_sub(syll_struc0, unlist(gregexpr('nj', parsed_syll.latest)),unlist(gregexpr('nj', parsed_syll.latest))))
    
    #EN_ADD: similar to the practive ñ check, the check for gn to /nj/ also needs this update in position
    if(check_for_gn == TRUE & g_m == "gn" & p_m == "nj") #if the check_for_gn was confirmed, p_m was updated, and we did set g_m to be gn, then:
      true_syllabic_position <- as.character(stri_sub(syll_struc0, unlist(gregexpr('nj', parsed_syll.latest)),unlist(gregexpr('nj', parsed_syll.latest))))
    
    
    #NOTE: to make sure this FAILS for things it doesn't know, it needs to fail when the max of search list is <1
    ifelse(max(select_g[,1]) < 1, g_m <- "FAILED", g_m <- g_m)
    
    ##NOW: if it failed because it couldn't make a /j/ that should have been /ju/ or some other biphone like that, catch that now
    p_m_jointpost <- stri_sub(parsed_syll0,-my.index,-my.index+1) #combine the current p_m with the one that was most recently mapped
    
    map_j = FALSE
    ifelse(p_m_jointpost == "ju" & g_m == "FAILED" , map_j <- TRUE, map_j <- map_j) #if this gets set as true, it means we need to remap...
    
    #do the same except for /je/ as in BINOCULAR /be na kje l3r/
    ifelse(p_m_jointpost == "je" & g_m == "FAILED" , map_j <- TRUE, map_j <- map_j) #if this gets set as true, it means we need to remap...
    
    #do the same except for /jU/ as in BINOCULAR /be na kjU l3r/
    ifelse(p_m_jointpost == "jU" & g_m == "FAILED" , map_j <- TRUE, map_j <- map_j) #if this gets set as true, it means we need to remap...
    
    #do the same except for /j3/ as in FAILURE /f8l j3r/
    ifelse(p_m_jointpost == "j3" & g_m == "FAILED" , map_j <- TRUE, map_j <- map_j) #if this gets set as true, it means we need to remap...
    
    #do the same except for /nj/ as in FAILURE /le za nje/--NOTE its p_m_jointPRIOR
    if(p_m_jointprior == "nj" & g_m == "FAILED"){
      map_j <- TRUE
    } else { map_j <- map_j} #if this gets set as true, it means we need to remap...and it's an "nj" situation
    
    #ES_ADD: now says if "n" or with || if "ñ", to get nj flag
    #ES_ADD: also, if there is a /nju/ OR /nje/ OR /nj3/ sequence, the earlier flag will have been set for /ju/ or /je/ or /j3/, but if there is ñ it means it needs to be /nj/ not /ju/ etc., i.e., this flag here for SPANISH should be used
    if(map_j == TRUE) {
      get_mapped <- unlist(strsplit(syll_struc0, split=syll_struc.latest, fixed=TRUE))[2] #this retrieves the syll_struc that was mapped up to this point
      #CAREFUL! don't change the syllabic structure if this /j/ should be a /nj/--but exclude when we have a full /nju/ sequence (that's the new "p_m_jointpost != "ju" part)
      if("n" %in% c(stri_sub(letter_str.latest,-1,-1) ,stri_sub(letter_str.latest,-3,-3)) & !p_m_jointpost %in% c("je","j3","ju") || "ñ" %in% c(stri_sub(letter_str.latest,-1,-1) ,stri_sub(letter_str.latest,-3,-3))  & p_m_prior=="n" ){ #if the letter string up to here ends N and the p_m_prior == "n", then this is probably a rare /nj/ mapping as in lasaGNa or seNor
        flag.nj<<-TRUE
        get_tomap_position <- stri_sub(syll_struc0,-my.index-1,-my.index-1) #get position that the /j/ should be, which should match whatever the /n/ is in front of it
        stri_sub(syll_struc.latest,-1,-1) <- get_tomap_position #update that syllabic position
        syll_struc0 <<- paste0(syll_struc.latest,get_mapped) #make the new syll_struc0
      } else {
        get_mapped_position <- stri_sub(syll_struc0,-my.index+1,-my.index+1) #get position that the /j/ should be, which should match whatever the mapped vowel was
        if(get_mapped_position==5){get_mapped_position <- 3} #if it happens to be a situation where the string ENDS in /je/, this is actually an illegal string--don't try to map it to 5 wf, keep/make it 3 sm (should cause exit from processing and return NA)
        stri_sub(syll_struc.latest,-1,-1) <- get_mapped_position #update that syllabic position
        syll_struc0 <<- paste0(syll_struc.latest,get_mapped)} #make the new syll_struc0
    }
    
    if (map_j == TRUE) {
      map_j <<- TRUE
      if(flag.nj==TRUE){
        flag.index_j <<- my.index #note the index will be the current one if the problem was /nj/ as opposed to /ju/ because the latter is encountered earlier (u is after the /j/, n is before it...)
      }else{
        flag.index_j <<- my.index-1} #this is critical! it must be the case that the map_j flag is only taking effect with the flag.index matches...which is one earlier than it was originally caught
      
      flag.restart <<- TRUE
    } #NOTE: earlier in the code there is a p_m_jointpost overwriting of p_m if the map_j flag was set as true the first time around, critically needed!
    
    #update the letter_str reflect what has been mapped... 
    #note: this is where the string must be reversed, as well as the grapheme, in order to only replace the LAST instance
    #there does not seem to be any ready functions for doing so otherwise (only replace-first or replace-all functions)
    letter_str.latest <- stri_reverse(str_replace(stri_reverse(letter_str.latest),stri_reverse(g_m),"_"))
    
    #if there is an _ underscore followed by something OTHER than "e", this is a FAIL! set the flag.index and restart...
    if( str_sub(letter_str.latest,-2,-2) == "_"){
      if(str_sub(letter_str.latest,-1,-1) != "e"){
        flag.restart <<- TRUE
        if (p_m=="j") { flag.index_j <<- my.index-1
        map_j <<- TRUE
        }else{
          flag.index <<- my.index-1
        }}}
    
    #and same issue but if the _ is even more out of place, i.e, if it's not the next-to-last...
    if( tail(unlist(gregexpr('_', letter_str.latest)),1) == nchar(letter_str.latest)-1 | tail(unlist(gregexpr('_', letter_str.latest)),1) == nchar(letter_str.latest) | tail(unlist(gregexpr('_', letter_str.latest)),1) == -1) { #if there is an _ anywhere but final or next to final...tail() is needed because there could be two __
      flag.restart <- flag.restart
    }else{
      flag.restart <<- TRUE
      if (p_m_jointprior=="nj" & p_m_jointpost != "ju") {
        flag.index_j <<- my.index #index will be the current one if the problem is /nj/
        map_j <<- TRUE
        flag.nj <<- TRUE
        stri_sub(syll_struc0,-flag.index_j,-flag.index_j)<-"2" #NOTE: the index just set should have the syllabic position become a 2, because the /nj/ must be that
        syll_struc0 <<- syll_struc0
      }else{
        if(p_m=="j") {
          flag.index_j <<- my.index-1 #index will be for previous pass if the problem is /ju/
          map_j <<- TRUE
        }
        flag.index <<- my.index-1
      }}
    
    #ES_ADD: the lines above that handle [gn] to /nj/ like in LASAGNA, to map /nj/ correctly, won't catch ñ like in ALIÑADA, but this here will do that specifically
    if (p_m_jointprior=="nj" & p_m_jointpost != "ju" & stri_sub(letter_str.latest,-1,-1) == "ñ") { 
      flag.restart <<- TRUE
      flag.index_j <<- my.index #index will be the current one if the problem is /nj/
      map_j <<- TRUE
      flag.nj <<- TRUE
      stri_sub(syll_struc0,-flag.index_j,-flag.index_j)<-"2" #NOTE: the index just set should have the syllabic position become a 2, because the /nj/ must be that
      syll_struc0 <<- syll_struc0
    }
    
    #if the replacement with _ procedure has left the last 2 characters as_E,  keep it that way so that the silent/final E can be mapped
    #if however the last character is now the _, simply remove it
    ifelse( str_sub(letter_str.latest,-2,-1)=="_e", letter_str.latest <- letter_str.latest, letter_str.latest <- str_sub(letter_str.latest, end = -2))
    
    #make sure there aren't TWO underscores, which will happen when there are clusters, like in ACQUIESCENCE (the N and the C cluster before the final E)
    letter_str.latest <- gsub("__","_",letter_str.latest)
    
    #update p.wf to correctly reflect that the /k/ was added for the X, i.e., make it /ks/ not just /s/
    ifelse (map_x, p_m <- p_m_jointprior, p_m <- p_m)
    ifelse (map_xe, p_m <- p_m_jointprior, p_m <- p_m)
    
    #now also update the parsed_syll to reflect what has been mapped. make sure it removes extra if there was a biphone like /ks/
    parsed_syll.latest <- str_sub(parsed_syll.latest,1,-(nchar(p_m)+1))
    
    #finally, have the latest syll_struc updated...removes more than one if the phoneme is longer as in /ks/
    syll_struc.latest <- str_sub(syll_struc.latest,1,-(nchar(p_m)+1))
    
    #list structure to store the mappings just obtained: the P, the G, and the position
    PGlist <- list()
    
    
    #the following stores the PHONEME, the GRAPHEME, and the POSITION, in that order:
    #ES_ADD: no longer hardcoded "3", now says "true syllabic position" because it may be altered if an ñ was proactively caught
    PGlist <- append(PGlist,matrix(c(p_m,g_m,true_syllabic_position,syll_struc.latest,parsed_syll.latest,letter_str.latest)))
    
    return(PGlist)
    print(PGlist)
  }
  
  #syllable-initial function
  map_si <- function(p_si,syll_struc.latest,parsed_syll.latest,letter_str.latest){
    
    #if map_j is TRUE and flag.nj is set, then this should pick up that a p_si == "j" needs to become "nj"
    if(map_j == TRUE & flag.nj == TRUE){
      if(str_sub(parsed_syll.latest,-2,-1) == "nj"){
        flag.nj <<- FALSE #reset
        map_j <<- FALSE #reset
        p_si <- str_sub(parsed_syll.latest,-2,-1)
      }} #this will make p_si "nj" so long as the flag.nj was raised and there actually IS an "nj" to map in the right position
    
    #look at the phoneme in front of the p_m, to check if its a vowel. If it is, then keep /z/ --> ES from being an option
    #if its /k/ or /b/ and the current p_is /t/, then block the BT and CT options for /t/
    #and block /e/ [schwa] from being "ia" if p_si_prior is a vowel..and block /m/ --> LM if /l/ precedes
    p_si_prior <- str_sub(parsed_syll.latest,-(nchar(p_si)+1),-(nchar(p_si)+1)) #ES_ADD: this was just hardcoded as -2, -2, but Spanish pointed out that when the lines above switch p_si to be "nj" instead of "j", then the prior phoneme is one character earlier (i.e., it's now -3,-3 not )
    no_z_es <- FALSE
    no_t_bt <- FALSE
    no_t_ct <- FALSE
    no_e_ia <- FALSE
    no_e_he <- FALSE
    no_m_lm <- FALSE
    no_m_thm <- FALSE
    no_s_tz  <- FALSE
    no_n_gn <- FALSE
    no_n_kn <- FALSE
    no_n_nn <- FALSE
    no_d_dd <- FALSE
    no_t_tt <- FALSE
    no_k_lk <- FALSE
    no_k_kk <- FALSE
    no_j_ill <- FALSE
    no_j_ll <- FALSE
    no_l_sl <- FALSE
    no_r_rr <- FALSE
    no_f_lf <- FALSE
    no_C_tch <- FALSE
    no_T_tth <- FALSE
    no_b_pb <- FALSE
    no_1_hi <- FALSE
    no_s_ss <- FALSE
    no_l_ll <- FALSE
    no_m_mm <- FALSE
    no_s_cs <- FALSE
    ifelse(p_si_prior %in% vowels, no_z_es <- TRUE, no_z_es <- no_z_es)
    ifelse(p_si_prior %in% vowels & p_si == "e", no_e_ia <- TRUE, no_e_ia <- no_e_ia)
    ifelse(p_si_prior %in% vowels & p_si == "e" & stri_sub(letter_str.latest,-3,-3)=="i", no_e_ia <- FALSE, no_e_ia <- no_e_ia) #except to no /e/ [schwa] --> IA: if the letter string has ANOTHER i, as it does in Hawaiian
    ifelse(p_si_prior == "b" & p_si == "t", no_t_bt <- TRUE, no_t_bt <- no_t_bt)
    ifelse(p_si_prior == "k" & p_si == "t", no_t_ct <- TRUE, no_t_ct <- no_t_ct)
    ifelse(p_si_prior == "l" & p_si == "m", no_m_lm <- TRUE, no_m_lm <- no_m_lm)
    ifelse(p_si_prior == "T" & p_si == "m" |p_si_prior == "D" & p_si == "m", no_m_thm <- TRUE, no_m_thm <- no_m_thm)
    ifelse(p_si_prior == "t" & p_si == "s", no_s_tz <- TRUE, no_s_tz <- no_s_tz)
    ifelse(p_si_prior == "g" & p_si == "n", no_n_gn <- TRUE, no_n_gn <- no_n_gn)
    ifelse(p_si_prior == "k" & p_si == "n", no_n_kn <- TRUE, no_n_kn <- no_n_kn)
    ifelse(p_si_prior == "n" & p_si == "n", no_n_nn <- TRUE, no_n_nn <- no_n_nn)
    ifelse(p_si_prior == "d" & p_si == "d", no_d_dd <- TRUE, no_d_dd <- no_d_dd)
    ifelse(p_si_prior == "t" & p_si == "t", no_t_tt <- TRUE, no_t_tt <- no_t_tt)
    ifelse(p_si_prior == "l" & p_si == "k", no_k_lk <- TRUE, no_k_lk <- no_k_lk)
    ifelse(p_si_prior == "k" & p_si == "k", no_k_kk <- TRUE, no_k_kk <- no_k_kk)
    ifelse(p_si_prior == "i" & p_si == "j", no_j_ill <- TRUE, no_j_ill <- no_j_ill)
    ifelse(p_si_prior == "l" & p_si == "j", no_j_ll <- TRUE, no_j_ll <- no_j_ll)
    ifelse(p_si_prior == "s" & p_si == "l" | p_si_prior == "z" & p_si == "l", no_l_sl <- TRUE, no_l_sl <- no_l_sl)
    ifelse(p_si_prior == "r" & p_si == "r", no_r_rr <- TRUE, no_r_rr <- no_r_rr)
    ifelse(p_si_prior == "t" & p_si == "C", no_C_tch <- TRUE, no_C_tch <- no_C_tch)
    ifelse(p_si_prior == "t" & p_si == "T", no_T_tth <- TRUE, no_T_tth <- no_T_tth)
    ifelse(p_si_prior == "p" & p_si == "b", no_b_pb <- TRUE, no_b_pb <- no_b_pb)
    ifelse(p_si_prior == "8" & p_si == "1" | p_si_prior == "5" & p_si == "1" , no_1_hi <- TRUE, no_1_hi <- no_1_hi)
    ifelse(p_si_prior == "z" & p_si == "s", no_s_ss <- TRUE, no_s_ss <- no_s_ss)
    ifelse(p_si_prior == "l" & p_si == "l", no_l_ll <- TRUE, no_l_ll <- no_l_ll)
    ifelse(p_si_prior == "m" & p_si == "m", no_m_mm <- TRUE, no_m_mm <- no_m_mm)
    ifelse(p_si_prior == "k" & p_si == "s", no_s_cs <- TRUE, no_s_cs <- no_s_cs)
    ifelse(p_si_prior != "i" & p_si == "e", no_e_he <- TRUE, no_e_he <- no_e_he) #this will block /e/ [schwa] --> HE unless its after /i/, as in VEHEMENT
    
    #ES_ADD: look at the phoneme in front of the p_si, to check if its /h/. If it is, and p_si is /w/, then don't allow [JU] --> /w/ because it needs to be to /hw/
    no_w_ju <- FALSE
    ifelse(p_si_prior == "h" & p_si == "w", no_w_ju <- TRUE, no_w_ju <- no_w_ju)
    
    #look at the final two phonemes jointly to see if they are /ks/ or /gz/ or /kS/ AND whether the last grapheme is X -OR!- XE
    p_si_jointprior <- str_sub(parsed_syll.latest,-2,-1)
    si_x <- str_sub(letter_str.latest,-1,-1)
    if(map_x == FALSE){
      if(si_x=="x" | si_x=="xh"){ #"xh" added as an option because of words like EXHAUST ... 
        if(p_si_jointprior=="ks" | p_si_jointprior=="gz" | p_si_jointprior=="kS"){
          map_x <<- TRUE
          flag.index <<- my.index
        }}}
    
    #now check if its ending in XE...
    si_xe <- str_sub(letter_str.latest,-2,-1)
    if(map_xe == FALSE){
      if (si_xe=="xe") {
        if(p_si_jointprior=="ks" | p_si_jointprior=="gz" | p_si_jointprior=="kS"){
          map_xe <<- TRUE
          flag.index <<- my.index
        }}}
    
    #must also check p_si_jointprior against a possible XI as in OBNOXIOUS, which means it would be "xi" not "x" or "xe"...creates a new map_xi flag
    si_xi <- str_sub(letter_str.latest,-2,-1)
    if(map_xi == FALSE) {
      if(si_xi=="xi"){
        if(p_si_jointprior=="ks" | p_si_jointprior=="gz" | p_si_jointprior=="kS"){
          map_xi <<- TRUE
          flag.index <<- my.index
        }}}
    
    #if map_j is true and flag.index_j == my.index, we need to update the p_si to be p_si_jointprior if it's /ju/ or a related one like /je/
    if( flag.index_j == my.index & map_j == TRUE & p_si_jointprior == "ju" | flag.index_j == my.index & map_j == TRUE & p_si_jointprior == "jU" | flag.index_j == my.index & map_j == TRUE & p_si_jointprior == "j3" | flag.index_j == my.index & map_j == TRUE & p_si_jointprior == "je")
      p_si <- p_si_jointprior
    
    #find the options:
    g_opt <- syllable_initial_mappings[syllable_initial_mappings$phoneme==p_si,]
    
    #if it's gone WAY wrong, there will be no options and it will crash. The following lines will put in a dummy placeholder phoneme and grapheme "6" to avoid this:
    if(nrow(g_opt)==0){
      g_opt <- syllable_initial_mappings[syllable_initial_mappings$phoneme=="s",][1,] #pull out the first option for 's' (which is possible in any position so guaranteed to be in the list of options)
      g_opt[1,1] <- "6"
      g_opt[1,2] <- "6"
    }
    
    #the following line removes /z/ ES as an option when its after a vowel
    ifelse(no_z_es == TRUE, g_opt  <- g_opt[!g_opt$grapheme=="es",], g_opt <- g_opt)
    
    #the following line removes /e/ IA as an option when its after a vowel
    ifelse(no_e_ia == TRUE, g_opt  <- g_opt[!g_opt$grapheme=="ia",], g_opt <- g_opt)
    
    #remove these as needed:
    if( no_t_ct == TRUE ) {g_opt  <- g_opt[!g_opt$grapheme=="ct",]}
    if( no_t_bt == TRUE ) {g_opt  <- g_opt[!g_opt$grapheme=="bt",]}
    if( no_m_lm == TRUE ) {g_opt  <- g_opt[!g_opt$grapheme=="lm",]}
    if( no_m_thm == TRUE ) {g_opt  <- g_opt[!g_opt$grapheme=="thm",]}
    if( no_s_tz == TRUE ) {g_opt  <- g_opt[!g_opt$grapheme=="tz",]}
    if( no_n_gn == TRUE ) {g_opt  <- g_opt[!g_opt$grapheme=="gn",]}
    if( no_n_kn == TRUE ) {g_opt  <- g_opt[!g_opt$grapheme=="kn",]}
    if( no_n_nn == TRUE ) {g_opt  <- g_opt[!g_opt$grapheme=="nn",]}
    if( no_d_dd == TRUE ) {g_opt  <- g_opt[!g_opt$grapheme=="dd",]}
    if( no_t_tt == TRUE ) {g_opt  <- g_opt[!g_opt$grapheme=="tt",]}
    if( no_k_lk == TRUE ) {g_opt  <- g_opt[!g_opt$grapheme=="lk",]}
    if( no_k_kk == TRUE ) {g_opt  <- g_opt[!g_opt$grapheme=="kk",]}
    if( no_j_ill == TRUE ) {g_opt  <- g_opt[!g_opt$grapheme=="ill",]}
    if( no_j_ll == TRUE ) {g_opt  <- g_opt[!g_opt$grapheme=="ll",]}
    if( no_l_sl == TRUE ) {g_opt  <- g_opt[!g_opt$grapheme=="sl",]}
    if( no_r_rr == TRUE ) {g_opt  <- g_opt[!g_opt$grapheme=="rr",]}
    if( no_C_tch == TRUE ) {g_opt  <- g_opt[!g_opt$grapheme=="tch",]}
    if( no_T_tth == TRUE ) {g_opt  <- g_opt[!g_opt$grapheme=="tth",]}
    if( no_b_pb == TRUE ) {g_opt  <- g_opt[!g_opt$grapheme=="pb",]}
    if( no_e_he == TRUE ) {g_opt  <- g_opt[!g_opt$grapheme=="he",]}
    if( no_1_hi == TRUE ) {g_opt  <- g_opt[!g_opt$grapheme=="hi",]}
    if( no_s_ss == TRUE ) {g_opt  <- g_opt[!g_opt$grapheme=="ss",]}
    if( no_l_ll == TRUE ) {g_opt  <- g_opt[!g_opt$grapheme=="ll",]}
    if( no_m_mm == TRUE ) {g_opt  <- g_opt[!g_opt$grapheme=="mm",]}
    if( no_s_cs == TRUE ) {g_opt  <- g_opt[!g_opt$grapheme=="cs",]}
    if( no_w_ju == TRUE ) {g_opt  <- g_opt[!g_opt$grapheme=="ju",]} #ES_ADD
    
    
    #CRITICAL if this is the second time around because whatever comes AFTER this failed, then flag.index will match the current my.index
    #currently the code thinks this may be because /l/ was mapped to SL when the S needed to be left behind for a /s/ or /z/
    if(flag.index == my.index){
      if(p_si == "l"){g_opt <- g_opt[-which(g_opt$grapheme=="sl"),]} #removes the row from g_opt that has "sl" as an option for /l/
    }
    
    #or it could be there is an X coming up and so the syllabic structure is problematic at an earlier point, which happens with EXHAUST
    #to check that, look for an X and remap the whole structure and then restart
    #NOTE::DO NOT do this if any map_x flag is already set to TRUE
    if(map_x == FALSE & map_xe == FALSE & map_xi == FALSE){ #this only takes place if the map_x flag wasn't ALREADY set
      if(flag.index == my.index){
        if (grepl("x",letter_str.latest) & grepl("gz",parsed_syll.latest)) { #if there is an X and /gz/ coming up...
          stri_sub(syll_struc0, unlist(gregexpr('gz', parsed_syll.latest))+1,unlist(gregexpr('gz', parsed_syll.latest))+1) <- "4" #make the /gz/ syllable final
          if(stri_sub(syll_struc0, unlist(gregexpr('gz', parsed_syll.latest))+2,unlist(gregexpr('gz', parsed_syll.latest))+2)=="3"){ #if whatever was after was medial...
            stri_sub(syll_struc0, unlist(gregexpr('gz', parsed_syll.latest))+2,unlist(gregexpr('gz', parsed_syll.latest))+2) <- "2" #make it syll_initial
          }}
        syll_struc0 <<- syll_struc0
        flag.restart <<- TRUE
        flag.index <<- 0 #reset the index!
      }}
    
    if(map_x == FALSE & map_xe == FALSE & map_xi == FALSE){
      if(flag.index == my.index){
        if (grepl("x",letter_str.latest) & grepl("ks",parsed_syll.latest)) { #if there is an X and /ks/ coming up...
          stri_sub(syll_struc0, unlist(gregexpr('ks', parsed_syll.latest))+1,unlist(gregexpr('ks', parsed_syll.latest))+1) <- "4" #make the /ks/ syllable final
          if(stri_sub(syll_struc0, unlist(gregexpr('ks', parsed_syll.latest))+2,unlist(gregexpr('ks', parsed_syll.latest))+2)=="3"){ #if whatever was after was medial...
            stri_sub(syll_struc0, unlist(gregexpr('ks', parsed_syll.latest))+2,unlist(gregexpr('ks', parsed_syll.latest))+2) <- "2" #make it syll_initial
          }}
        syll_struc0 <<- syll_struc0
        flag.restart <<- TRUE
        flag.index <<- 0 #reset the index!
      }}
    
    if(map_x == FALSE & map_xe == FALSE & map_xi == FALSE){
      if(flag.index == my.index){
        if (grepl("x",letter_str.latest) & grepl("kS",parsed_syll.latest)) { #if there is an X and /kS/ coming up...
          stri_sub(syll_struc0, unlist(gregexpr('kS', parsed_syll.latest))+1,unlist(gregexpr('kS', parsed_syll.latest))+1) <- "4" #make the /kS/ syllable final
          if(stri_sub(syll_struc0, unlist(gregexpr('kS', parsed_syll.latest))+2,unlist(gregexpr('kS', parsed_syll.latest))+2)=="3"){ #if whatever was after was medial...
            stri_sub(syll_struc0, unlist(gregexpr('kS', parsed_syll.latest))+2,unlist(gregexpr('kS', parsed_syll.latest))+2) <- "2" #make it syll_initial
          }}
        syll_struc0 <<- syll_struc0
        flag.restart <<- TRUE
        flag.index <<- 0 #reset the index!
      }}
    
    #overwrite those options if the map_x flag is TRUE, i.e., switch it to the 'x' grapheme options
    #MAJOR NOTE: X cannot be in syllable initial, in fact! so if this flag has been set, it's actually an indication that
    #the X and its phonemes, /ks/ or whatever, should instead be syllable final (of the preceding syllable)
    #this requires a major fix: it  means the previously-assigned PG MAY be wrong, AND, this PG actually is
    #going to need to be syllable-final. So the easiest thing to do is use this flag to change the initial syll_struc and then
    #COMPLETELY restart the mapping. In essence, this flag should catch the issue and force a restart of the entire mapping...
    #THIS SHOUDL ONLY HAPPEN IF A FLAG WAS SET!
    if(map_x == TRUE | map_xe == TRUE | map_xi == TRUE) {
      if(flag.index == my.index){
        get_mapped <- unlist(strsplit(syll_struc0, split=syll_struc.latest, fixed=TRUE))[2] #this retrieves the syll_struc that was mapped up to this point
        ifelse(get_mapped!="5",stri_sub(get_mapped,1,1) <- "2",get_mapped<-get_mapped) #this makes the new syll_struc for that later part of the word correctly show what is syll-initial -UNLESS- it's actually the end of the word, like GALAXY (so in that case a 5)
        stri_sub(syll_struc.latest,-1,-1) <- "4" #this changes the part that was currently being mapped to correctly show its syll-final
        syll_struc0 <<- paste0(syll_struc.latest,get_mapped) #finally, make the new syll_struc0
        flag.restart <<- TRUE #you actually do need to enforce a restart here because it IS possible for it to not fail at this step--this happens for EXAMINATION because the /z/ of the /gz/ gets mapped to X, which leaves it to get stuck trying to then do the /g/ with only an E leftover
      }}
    
    n_opt <- length(g_opt$grapheme)
    search_lists <- list()
    
    for(j in (1:n_opt)){
      search_lists[[j]] <- matrix(NA,nrow=1,ncol=2)
      #first col should give grapheme latest location
      search_lists[[j]][1] <- max(unlist(gregexpr(g_opt$grapheme[j], letter_str.latest))) + nchar(g_opt$grapheme[j]) -1 #the max() gets the latest location of the START
      #of the grapheme...the nchar() gets the length of the grapheme...and the -1 is to get the final position. e.g., the DG in EDGE starts at char 2, its length is 2,
      #so the position it ends is 2+2-1 = 3
      #the above line returns false results sometimes when the grapheme isn't even present--e.g., it will return MIXED as having an AI_E just because the value comes out as 2 instead of -1, due to the length of AI_E
      #to fix this, simply overwrite the search_lists[[j]][1] value with gregexpr(g_opt$grapheme[j], letter_str.latest)[[1]][1] if that equals -1
      ifelse (gregexpr(g_opt$grapheme[j], letter_str.latest)[[1]][1] == -1, search_lists[[j]][1] <- -1, search_lists[[j]][1] <- search_lists[[j]][1])
      #second col gives the grapheme length 
      search_lists[[j]][2] <- nchar(g_opt$grapheme[j])
    }
    
    #unlist to get a matrix for selecting the grapheme
    select_g <- t(matrix(unlist(search_lists),ncol=n_opt,nrow=2))
    
    #code to get which grapheme is the match--note that it finds the max of latest position, but there can be ties
    #so it further looks at max of grapheme length to break the tie. the code is longer still because
    #we need the original index of the grapheme, i.e., the absolute number, not relative (e.g., if the tie is between
    #grapheme #2 and #8, and #2 should win because its longer, this code returns #2 [as opposed to #1, meaning the first of
    #the tied options])
    g_si <- which(select_g[,1]==max(select_g[,1]))[which.max(select_g[which(select_g[,1]==max(select_g[,1])),2])]
    g_si <- g_opt$grapheme[g_si]
    
    #NOTE: to make sure this FAILS for things it doesn't know, it needs to fail when the max of search list is <1
    ifelse(max(select_g[,1]) < 1, g_si <- "FAILED", g_si <- g_si)
    
    ##NOW: if it failed because it couldn't make a /j/ that should have been /ju/ or some other biphone like that, catch that now
    p_si_jointpost <- stri_sub(parsed_syll0,-my.index,-my.index+1) #combine the current p_si with the one that was most recently mapped
    if(p_si_jointpost == "ju" & g_si == "FAILED" | p_si_jointpost == "jU" & g_si == "FAILED" | p_si_jointpost == "j3" & g_si == "FAILED" | p_si_jointpost == "je" & g_si == "FAILED"){
      map_j <<- TRUE #if this gets set as true, it means we need to remap...
      flag.index_j <<- my.index-1
      #the current position should match the upcoming one UNLESS! it was a 5:
      if(stri_sub(syll_struc0,nchar(syll_struc.latest)+1,nchar(syll_struc.latest)+1) != "5"){
        stri_sub(syll_struc0,nchar(syll_struc.latest)+1,nchar(syll_struc.latest)+1) <- stri_sub(syll_struc0,nchar(syll_struc.latest),nchar(syll_struc.latest))
      }else{
        #REVISED 12/15/25: a "word" ending in -ju/-jU/j3/je is pathological in English if it isn't spelled with separate graphemes for /j/ and /vowel/, so this case should actually be a failure
        #so the else is no longer needed
        # stri_sub(syll_struc0,nchar(syll_struc.latest),nchar(syll_struc.latest)) <- stri_sub(syll_struc0,nchar(syll_struc.latest)+1,nchar(syll_struc.latest)+1) #swapped order if word final...
      }
      flag.restart <<- TRUE
    }
    syll_struc0 <<- syll_struc0
    
    #now catch instances where this may have failed because the previously mapped phoneme was too "greedy" OR maybe not greedy enough!
    #for example, in TOUGHER, it will have assigned the /3/ to HE, which means the /f/ is left to be a G which it cannot!
    #in CHRYSANTHEMUM, it will have assigned /e/ [schwa] to HE, and so /T/ will be unmappable with only the T left...
    #in EXHIBITION, it fails because /schwa/ maps to just the I, and then /s/ fails on EXH--this is tricky because the syllabic structure needs to be corrected so that the schwa is syll_initial and /ks/is final...
    if (g_si == "FAILED") {
      if(map_j == FALSE)
        if(p_si_jointprior == "ks" | p_si_jointprior == "kS" | p_si_jointprior == "gz"){
          get_mapped <- unlist(strsplit(syll_struc0, split=syll_struc.latest, fixed=TRUE))[2]
          ifelse(get_mapped!="5",stri_sub(get_mapped,1,1) <- "2",get_mapped<-get_mapped) #it should start as a 2, because the current 2 is about to become a 4...UNLESS it was word final, like GALAXY. NB: this code may be redundant with earlier...
          stri_sub(syll_struc.latest,-1,-1) <- "4" #make the current a 4...
          syll_struc0 <<- paste0(syll_struc.latest,get_mapped) #make the new syll_struc0
          flag.restart <<- TRUE
        }else{
          flag.index <<- my.index-1 #this sets the flag index to be PRIOR to what just failed if it wasn't an X issue
          flag.restart <<- TRUE
        }else{
          flag.restart <<- TRUE
        }}
    
    #update the letter_str reflect what has been mapped... 
    #note: this is where the string must be reversed, as well as the grapheme, in order to only replace the LAST instance
    #there does not seem to be any ready functions for doing so otherwise (only replace-first or replace-all functions)
    letter_str.latest <- stri_reverse(str_replace(stri_reverse(letter_str.latest),stri_reverse(g_si),"_"))
    
    #if the replacement with _ makes the last character now the _, simply remove it
    if( str_sub(letter_str.latest,-1,-1)=="_"){letter_str.latest <- str_sub(letter_str.latest, end = -2)}
    
    #if there is an _ underscore followed by something OTHER than "e", this is a FAIL! set the flag.index and restart...
    if(tail(unlist(gregexpr("_", letter_str.latest)), n=1) != -1){ #this will be FALSE if the underscore was cleared, in which case nothing will be done
      if(stri_sub(letter_str.latest,tail(unlist(gregexpr("_", letter_str.latest)), n=1)+1,tail(unlist(gregexpr("_", letter_str.latest)), n=1)+1) != "e"){ #it had better be 'e', otherwise:
        #need to now repeat the work to check if actually this could be a map_j situation
        if(p_si_jointpost %in% c("ju","jU","j3","je")){
          flag.restart <<- TRUE
          map_j <<- TRUE
          flag.index_j <<- my.index-1
        }else{
          flag.restart <<- TRUE
          flag.index <<- my.index
        }}}
    
    #now also update the parsed_syll to reflect what has been mapped. this is easier in general it's always the last phoneme
    parsed_syll.latest <- str_sub(parsed_syll.latest,1,-(nchar(p_si)+1))
    
    #finally, have the latest syll_struc updated
    syll_struc.latest <- str_sub(syll_struc.latest,1,-(nchar(p_si)+1))
    
    #list structure to store the mappings just obtained: the P, the G, and the position
    PGlist <- list()
    
    #the following stores the PHONEME, the GRAPHEME, and the POSITION, in that order:
    PGlist <- append(PGlist,matrix(c(p_si,g_si,"2",syll_struc.latest,parsed_syll.latest,letter_str.latest)))
    
    return(PGlist)
    print(PGlist)
  }
  
  #syllable-final function
  map_sf <- function(p_sf,syll_struc.latest,parsed_syll.latest,letter_str.latest){
    
    ##CRITICAL: words that have an internal final E will break the syllabic structure--this flag will catch that to restart with an updated syll_struc0
    #example is BASEMENT, parsed by MOP as /b8/ + /sment/. The script will fail when the p_sf is a vowel but there is a final E, e.g.,
    #BASEMENT will be left with BA_E searching for p_sf = 8 but that's not allowed (by definition _E cannot be syllable final)
    if(str_sub(letter_str.latest,-2,-1)=="_e"){ 
      to_map <- "342" #this finds the current problematic sequence, which is a "423", and makes it the needed "342"
      stri_sub(syll_struc0,nchar(syll_struc.latest),-(nchar(syll_struc0)-nchar(syll_struc.latest)-1)) <- to_map
      syll_struc0 <<- syll_struc0
      flag.restart <<- TRUE
    } 
    
    #look at the phoneme in front of the p_sf, to check if its a vowel. If it is, then keep /z/ --> ES from being an option or /d/ --> ED
    p_sf_prior <- str_sub(parsed_syll.latest,-2,-2)
    no_z_es <- FALSE
    no_d_ed <- FALSE
    ifelse(p_sf_prior %in% vowels & p_sf == "z", no_z_es <- TRUE, no_z_es <- no_z_es)
    ifelse(p_sf_prior %in% vowels & p_sf == "d", no_d_ed <- TRUE, no_d_ed <- no_d_ed)
    
    #look at the phoneme in front of the p_sf, to check if its /h/ or /D/ or /T/ or /C/ or /S/. If it is, then don't allow silent H options if the current p_sf is a vowel
    #ES_ADD: this is an issue for English-Spanish, because for example ADHERENCE versus ADHERENCIA--you need to parse the H in English but in Spanish its with the vowel
    #ES_ADD: to deal with that, we can consider that in English if it's /D/ there would be a letter [T] and so you do use this block in that case (but if there isn't a T, you don't set this block for /D/)
    #ES_ADD: this also has to stop, e.g., GHETTO from being G-HE-TT-O and WHISPER from being W-HI-S-P-E-R and CHIANTI from being C-HI-A-N-T-I
    no_silent_H <- FALSE
    ifelse(p_sf_prior %in% c("h","C","T","S","g","w","k") & p_sf %in% vowels | p_sf_prior == "D" & p_sf %in% vowels & str_sub(letter_str.latest,-3,-3) %in% c("t"), no_silent_H <- TRUE, no_silent_H <- no_silent_H)
    
    #EN_ADD: HUGO is /hju go/ in EN but /u go/ in ES. So, one has U --> /ju/ and the other HU /u/. Without this check, the latter but not the former will parse.
    no_u_hu <- FALSE
    ifelse(p_sf_prior == "j" & p_sf == "u" & stri_sub(letter_str.latest,-2,-2) == "h", no_u_hu <- TRUE, no_u_hu <- no_u_hu)
    
    #look at the phoneme in front of the p_sf, to check if its /3/. If it is, then don't allow r to ro if the current p_sf is /r/ (because it's probably IRON or something like that)
    no_r_ro <- FALSE
    ifelse(p_sf_prior == "3" & p_sf == "r", no_r_ro <- TRUE, no_r_ro <- no_r_ro)
    
    #look at the phoneme in front of the p_sf, to check if its /l/. If it is, then don't allow /m/ to LM if the current p_sf is /m/ (because it's probably CALMLY or something like that where the L *is* being pronounced)
    no_m_lm <- FALSE
    ifelse(p_sf_prior == "l" & p_sf == "m", no_m_lm <- TRUE, no_m_lm <- no_m_lm)
    
    #look at the phoneme in front of the p_sf, to check if its /l/. If it is, then don't allow /m/ to LM if the current p_sf is /m/ (because it's probably CALMLY or something like that where the L *is* being pronounced)
    no_t_ct <- FALSE
    ifelse(p_sf_prior == "k" & p_sf == "t", no_t_ct <- TRUE, no_t_ct <- no_t_ct)
    
    #look at the phoneme in front of the p_sf, to check if its /w/. If it is, then don't allow /1/ to UI if the current p_sf is /1/ (because it's probably CUI-SINE or something like that where the U is a /w/)
    no_1_ui <- FALSE
    ifelse(p_sf_prior == "w" & p_sf == "1", no_1_ui <- TRUE, no_1_ui <- no_1_ui)
    
    #look at the phoneme in front of the p_sf, to check if its /S/. If it is, then don't allow /1/ to HI if the current p_sf is /1/ (because it's probably CHI-VAL-RY or something like that)
    no_1_hi <- FALSE
    ifelse(p_sf_prior == "S" & p_sf == "1", no_1_hi <- TRUE, no_1_hi <- no_1_hi)
    
    #more specific case: don't do it if the sound is /f/ that may be a PH or GH
    ifelse(p_sf_prior == "f" & p_sf %in% vowels & stri_sub(letter_str.latest,-2,-2) == "h", no_silent_H <- TRUE, no_silent_H <- no_silent_H)
    
    #look at the phoneme in front of the p_sf, to check if its /w/. If it is, then don't allow /e/ [schwa] --> UA 
    no_e_ua <- FALSE
    ifelse(p_sf_prior == "w" & p_sf == "e", no_e_ua <- TRUE, no_e_ua <- no_e_ua)
    
    #look at the phoneme in front of the p_sf, to check if its one that needs to block HO
    no_ho <- FALSE
    ifelse(p_sf_prior %in% c("w","C","S","k","t") , no_ho <- TRUE, no_ho <- no_ho)
    
    #EN_ADD: don't allow schwa /e/ to IE or IA if there's a preceding /S/ to map to TI (i.e., the I is needed for TI for /S/)
    #EN_ADD (4/22/25): same issue if the /S/ must map to [C] as in specially (i.e., to make it S-P-E-CI-A-LL-Y not S-P-E-C-IA-LL-Y
    no_e_iaie <- FALSE
    if(p_sf == "e" & p_sf_prior == "S" & stri_sub(letter_str.latest,-3,-3) %in% c("c","t")){
      no_e_iaie <- TRUE
    }
    
    #look at the final two phonemes jointly to see if they are /ks/ AND whether the last grapheme is X -OR!- XE
    p_sf_jointprior <- str_sub(parsed_syll.latest,-2,-1)
    sf_x <- str_sub(letter_str.latest,-1,-1)
    map_x = FALSE
    ifelse (sf_x=="x", ifelse(p_sf_jointprior=="ks" | p_sf_jointprior=="kS" | p_sf_jointprior=="gz" | p_sf_jointprior=="gZ",map_x <- TRUE, map_x <- FALSE), map_x <- FALSE)
    
    #now check if its ending in XE...
    sf_xe <- str_sub(letter_str.latest,-2,-1)
    map_xe = FALSE
    ifelse (sf_xe=="xe", ifelse(p_sf_jointprior=="ks" | p_sf_jointprior=="kS" | p_sf_jointprior=="gz" | p_sf_jointprior=="gZ",map_xe <- TRUE, map_xe <- FALSE), map_xe <- FALSE)
    
    #now check if its ending in XI...
    sf_xi <- str_sub(letter_str.latest,-2,-1)
    map_xi = FALSE
    ifelse (sf_xi=="xi", ifelse(p_sf_jointprior=="ks" | p_sf_jointprior=="kS" | p_sf_jointprior=="gz" | p_sf_jointprior=="gZ",map_xi <- TRUE, map_xi <- FALSE), map_xi <- FALSE)
    
    #catch if its an 3rd like tired, in which case block d_ed...ALSO block /ju/ to EU if we have here /iju/ like in REUSE
    p_sf_jointprior2 <- str_sub(parsed_syll.latest,-3,-1)
    if(p_sf_jointprior2=="3rd")
      no_d_ed <- TRUE
    no_ju_eu <- FALSE
    if(p_sf_jointprior2=="iju" & p_sf == "ju" | p_sf_jointprior2=="iju" & p_sf == "u")
      no_ju_eu <- TRUE
    
    #CRITICAL: if this is the second time around because the map_j flag was set, then p_sf must be overwritten
    #notice that the flag was set based on jointPOST, but when looping back through again we now encounter the vowel before the /j/
    #and so must grab that /j/. NB: it has to consider the scenario where /ju/ is one back OR one forward...and it should only do so when it's actually gotten back to the /u/
    if(p_sf =="u")
      tryCatch({
        ifelse(map_j == TRUE & my.index == flag.index_j, 
               if(str_sub(parsed_syll.latest,-2,-1)=="ju"){
                 p_sf<- str_sub(parsed_syll.latest,-2,-1)
                 map_j <- FALSE #added to clear flags on 4/22/25
                 flag.index_j <- 0 }#added to clear flags on 4/22/25
               else
                 if(str_sub(parsed_syll.latest,-3,-2)=="ju"){
                   p_sf<- str_sub(parsed_syll.latest,-3,-2)
                   map_j <- FALSE #added to clear flags on 4/22/25
                   flag.index_j <- 0 }, #added to clear flags on 4/22/25
               p_sf <- p_sf)
      },    error = function(e) NA)
    
    #do the same but if its "je" instead of "ju"
    if(p_sf =="e")
      tryCatch({
        ifelse(map_j == TRUE & my.index == flag.index_j, 
               if(str_sub(parsed_syll.latest,-2,-1)=="je"){
                 p_sf<- str_sub(parsed_syll.latest,-2,-1)
                 map_j <- FALSE #added to clear flags on 4/22/25
                 flag.index_j <- 0 }#added to clear flags on 4/22/25
               else
                 if(str_sub(parsed_syll.latest,-3,-2)=="je"){
                   p_sf<- str_sub(parsed_syll.latest,-3,-2)
                   map_j <- FALSE #added to clear flags on 4/22/25
                   flag.index_j <- 0 }, #added to clear flags on 4/22/25
               p_sf <- p_sf)
      },    error = function(e) NA)
    
    #do the same but if its "jU" instead of "ju"
    if(p_sf =="U")
      tryCatch({
        ifelse(map_j == TRUE & my.index == flag.index_j, 
               if(str_sub(parsed_syll.latest,-2,-1)=="jU"){
                 p_sf<- str_sub(parsed_syll.latest,-2,-1)
                 map_j <- FALSE #added to clear flags on 4/22/25
                 flag.index_j <- 0 }#added to clear flags on 4/22/25
               else
                 if(str_sub(parsed_syll.latest,-3,-2)=="jU"){
                   p_sf<- str_sub(parsed_syll.latest,-3,-2)
                   map_j <- FALSE #added to clear flags on 4/22/25
                   flag.index_j <- 0 }, #added to clear flags on 4/22/25
               p_sf <- p_sf)
      },    error = function(e) NA)
    
    #or if flag.ts is set, try that biphone!
    if(flag.index == my.index & flag.ts == TRUE){p_sf <- "ts"}
    
    #find the options:
    g_opt <- syllable_final_mappings[syllable_final_mappings$phoneme==p_sf,]
    
    #if it's gone WAY wrong, there will be no options and it will crash. The following lines will put in a dummy placeholder phoneme and grapheme "6" to avoid this:
    #ES_ADD: or it could be a weird syllable-final /nj/ like in A-BUÑ-UE-LAR, which means we'd be on p_sf_jointprior == "nj" and [ñ] wants to be mapped
    if(nrow(g_opt)==0){
      if(p_sf_jointprior == "nj" && nrow(syllable_final_mappings[syllable_final_mappings$phoneme==p_sf,])>0) {p_sf <- "nj" #if we're in the Spanish situation and should map /nj/ make that the p_sf and grab options again
      g_opt <- syllable_final_mappings[syllable_final_mappings$phoneme==p_sf,]
      } else { #or else no we really just need to put in a 6 to end the iteration
        g_opt <- syllable_final_mappings[syllable_final_mappings$phoneme=="s",][1,] #pull out the first option for 's' (which is possible in any position so guaranteed to be in the list of options)
        g_opt[1,1] <- "6"
        g_opt[1,2] <- "6"
      }}
    
    #the following line removes /z/ ES as an option when its after a vowel
    ifelse(no_z_es == TRUE, g_opt  <- g_opt[!g_opt$grapheme=="es",], g_opt <- g_opt)
    
    #the following line removes /d/ ED as an option when its after a vowel
    ifelse(no_d_ed == TRUE, g_opt  <- g_opt[!g_opt$grapheme=="ed",], g_opt <- g_opt)
    
    #remove options as needed
    ifelse(no_silent_H, g_opt <- g_opt[str_sub(g_opt$grapheme,1,1)!="h",], g_opt <- g_opt)
    ifelse(no_e_ua, g_opt <- g_opt[g_opt$grapheme!="ua",], g_opt <- g_opt)
    ifelse(no_r_ro, g_opt <- g_opt[g_opt$grapheme!="ro",], g_opt <- g_opt)
    ifelse(no_m_lm, g_opt <- g_opt[g_opt$grapheme!="lm",], g_opt <- g_opt)
    ifelse(no_t_ct, g_opt <- g_opt[g_opt$grapheme!="ct",], g_opt <- g_opt)
    ifelse(no_1_ui, g_opt <- g_opt[g_opt$grapheme!="ui",], g_opt <- g_opt)
    ifelse(no_1_hi, g_opt <- g_opt[g_opt$grapheme!="hi",], g_opt <- g_opt)
    ifelse(no_ju_eu, g_opt <- g_opt[g_opt$grapheme!="eu",], g_opt <- g_opt)
    ifelse(no_ho, g_opt <- g_opt[g_opt$grapheme!="ho",], g_opt <- g_opt)
    #EN_ADD options here:
    ifelse(no_u_hu, g_opt <- g_opt[g_opt$grapheme!="hu",], g_opt <- g_opt)
    ifelse(no_e_iaie, g_opt <- g_opt[!g_opt$grapheme %in% c("ie","ia"),], g_opt <- g_opt)
    
    #overwrite those options if the map_x flag is TRUE, i.e., switch it to the 'x' grapheme options
    ifelse(map_x, g_opt <- syllable_final_mappings[syllable_final_mappings$grapheme=="x",], g_opt <- g_opt)
    
    #and rewrite the phoneme to be the jointprior one if its /ks/ and if the X flag is true
    if (map_x == TRUE & p_sf_jointprior == "ks" | map_x == TRUE & p_sf_jointprior == "kS" | map_x == TRUE & p_sf_jointprior == "gz" | map_x == TRUE &  p_sf_jointprior=="gZ"){
      p_sf <- p_sf_jointprior
    } else {
      p_sf <- p_sf }
    
    #do similar if the map_xe flag is TRUE
    ifelse(map_xe, g_opt <- syllable_final_mappings[syllable_final_mappings$grapheme=="x",], g_opt <- g_opt)
    if (map_xe == TRUE & p_sf_jointprior == "ks" | map_xe == TRUE & p_sf_jointprior == "kS" | map_xe == TRUE & p_sf_jointprior == "gz" | map_xe == TRUE & p_sf_jointprior == "gZ"){
      p_sf <- p_sf_jointprior
    } else {
      p_sf <- p_sf }
    
    #do similar if the map_xi flag is TRUE
    ifelse(map_xi, g_opt <- syllable_final_mappings[syllable_final_mappings$grapheme=="xi",], g_opt <- g_opt)
    if (map_xi == TRUE & p_sf_jointprior == "ks" | map_xi == TRUE & p_sf_jointprior == "kS" | map_xi == TRUE & p_sf_jointprior == "gz" | map_xi == TRUE & p_sf_jointprior == "gZ"){
      p_sf <- p_sf_jointprior
    } else {
      p_sf <- p_sf }
    
    #CRITICAL if this is the second time around because whatever comes AFTER this failed, then flag.index will match the current my.index
    #first check if its an X issue... ###BUT DON'T DO THIS if we've already raised the map_x flag! as shown by SIXTEEN
    #to check that, look for an X and remap the whole structure and then restart
    if(map_x != TRUE & map_xe !=TRUE & map_xi != TRUE){
      if(flag.index == my.index){
        if (grepl("x",letter_str.latest) & grepl("gz",parsed_syll.latest)) { #if there is an X and /gz/ coming up...
          stri_sub(syll_struc0, unlist(gregexpr('gz', parsed_syll.latest))+1,unlist(gregexpr('gz', parsed_syll.latest))+1) <- "4" #make the /gz/ syllable final
          if(stri_sub(syll_struc0, unlist(gregexpr('gz', parsed_syll.latest))+2,unlist(gregexpr('gz', parsed_syll.latest))+2)=="4"){ #if whatever was after was syll-final
            stri_sub(syll_struc0, unlist(gregexpr('gz', parsed_syll.latest))+2,unlist(gregexpr('gz', parsed_syll.latest))+2) <- "2" #make it syll_initial
          }
          if(stri_sub(syll_struc0, unlist(gregexpr('gz', parsed_syll.latest))+3,unlist(gregexpr('gz', parsed_syll.latest))+3)=="3"){ #if whatever was after THAT was syll-medial
            stri_sub(syll_struc0, unlist(gregexpr('gz', parsed_syll.latest))+3,unlist(gregexpr('gz', parsed_syll.latest))+3) <- "2" #make it syll-initial
          }
          syll_struc0 <<- syll_struc0
          flag.restart <<- TRUE
          flag.index <<- 0 #reset the index!
        }}}
    
    if(map_x != TRUE & map_xe !=TRUE & map_xi != TRUE){
      if(flag.index == my.index){
        if (grepl("x",letter_str.latest) & grepl("gZ",parsed_syll.latest)) { #if there is an X and /gZ/ coming up...
          stri_sub(syll_struc0, unlist(gregexpr('gZ', parsed_syll.latest))+1,unlist(gregexpr('gZ', parsed_syll.latest))+1) <- "4" #make the /gz/ syllable final
          if(stri_sub(syll_struc0, unlist(gregexpr('gZ', parsed_syll.latest))+2,unlist(gregexpr('gZ', parsed_syll.latest))+2)=="4"){ #if whatever was after was syll-final
            stri_sub(syll_struc0, unlist(gregexpr('gZ', parsed_syll.latest))+2,unlist(gregexpr('gZ', parsed_syll.latest))+2) <- "2" #make it syll_initial
          }
          if(stri_sub(syll_struc0, unlist(gregexpr('gZ', parsed_syll.latest))+3,unlist(gregexpr('gZ', parsed_syll.latest))+3)=="3"){ #if whatever was after THAT was syll-medial
            stri_sub(syll_struc0, unlist(gregexpr('gZ', parsed_syll.latest))+3,unlist(gregexpr('gZ', parsed_syll.latest))+3) <- "2" #make it syll-initial
          }
          syll_struc0 <<- syll_struc0
          flag.restart <<- TRUE
          flag.index <<- 0 #reset the index!
        }}}
    
    if(map_x != TRUE & map_xe !=TRUE & map_xi != TRUE){
      if(flag.index == my.index){
        if (grepl("x",letter_str.latest) & grepl("kS",parsed_syll.latest)) { #if there is an X and /kS/ coming up...
          stri_sub(syll_struc0, unlist(gregexpr('kS', parsed_syll.latest))+1,unlist(gregexpr('kS', parsed_syll.latest))+1) <- "4" #make the /kS/ syllable final
          if(stri_sub(syll_struc0, unlist(gregexpr('kS', parsed_syll.latest))+2,unlist(gregexpr('kS', parsed_syll.latest))+2)=="4"){ #if whatever was after was syll-final
            stri_sub(syll_struc0, unlist(gregexpr('kS', parsed_syll.latest))+2,unlist(gregexpr('kS', parsed_syll.latest))+2) <- "2" #make it syll_initial
          }
          if(stri_sub(syll_struc0, unlist(gregexpr('kS', parsed_syll.latest))+3,unlist(gregexpr('kS', parsed_syll.latest))+3)=="3"){ #if whatever was after THAT was syll-medial
            stri_sub(syll_struc0, unlist(gregexpr('kS', parsed_syll.latest))+3,unlist(gregexpr('kS', parsed_syll.latest))+3) <- "2" #make it initial
          }
          syll_struc0 <<- syll_struc0
          flag.restart <<- TRUE
          flag.index <<- 0 #reset the index!
        }}}
    
    if(map_x != TRUE & map_xe !=TRUE & map_xi != TRUE){
      if(flag.index == my.index){
        if (grepl("x",letter_str.latest) & grepl("ks",parsed_syll.latest)) { #if there is an X and /ks/ coming up...
          stri_sub(syll_struc0, unlist(gregexpr('ks', parsed_syll.latest))+1,unlist(gregexpr('ks', parsed_syll.latest))+1) <- "4" #make the /ks/ syllable final
          if(stri_sub(syll_struc0, unlist(gregexpr('ks', parsed_syll.latest))+2,unlist(gregexpr('ks', parsed_syll.latest))+2)=="4"){ #if whatever was after was syll-final
            stri_sub(syll_struc0, unlist(gregexpr('ks', parsed_syll.latest))+2,unlist(gregexpr('ks', parsed_syll.latest))+2) <- "2" #make it syll_initial
          }
          if(stri_sub(syll_struc0, unlist(gregexpr('ks', parsed_syll.latest))+3,unlist(gregexpr('ks', parsed_syll.latest))+3)=="3"){ #if whatever was after THAT was syll-medial
            stri_sub(syll_struc0, unlist(gregexpr('ks', parsed_syll.latest))+3,unlist(gregexpr('ks', parsed_syll.latest))+3) <- "2" #make it initial
          }
          syll_struc0 <<- syll_struc0
          flag.restart <<- TRUE
          flag.index <<- 0 #reset the index!
        }}}
    
    #if it wasn't an X issue, then currently the code thinks this may be because a silent H was assigned, so this should BLOCK that from happening
    if(flag.index == my.index & p_sf == "3"){g_opt <- g_opt[!stri_sub(g_opt$grapheme,1,1)=="h",]}
    if(flag.index == my.index & p_sf == "e"){g_opt <- g_opt[!stri_sub(g_opt$grapheme,1,1)=="h",]}
    
    n_opt <- length(g_opt$grapheme)
    search_lists <- list()
    
    for(j in (1:n_opt)){
      search_lists[[j]] <- matrix(NA,nrow=1,ncol=2)
      #first col should give grapheme latest location
      search_lists[[j]][1] <- max(unlist(gregexpr(g_opt$grapheme[j], letter_str.latest))) + nchar(g_opt$grapheme[j]) -1 #the max() gets the latest location of the START
      #of the grapheme...the nchar() gets the length of the grapheme...and the -1 is to get the final position. e.g., the DG in EDGE starts at char 2, its length is 2,
      #so the position it ends is 2+2-1 = 3
      #the above line returns false results sometimes when the grapheme isn't even present--e.g., it will return MIXED as having an AI_E just because the value comes out as 2 instead of -1, due to the length of AI_E
      #to fix this, simply overwrite the search_lists[[j]][1] value with gregexpr(g_opt$grapheme[j], letter_str.latest)[[1]][1] if that equals -1
      ifelse (gregexpr(g_opt$grapheme[j], letter_str.latest)[[1]][1] == -1, search_lists[[j]][1] <- -1, search_lists[[j]][1] <- search_lists[[j]][1])
      #second col gives the grapheme length 
      search_lists[[j]][2] <- nchar(g_opt$grapheme[j])
    }
    
    #unlist to get a matrix for selecting the grapheme
    select_g <- t(matrix(unlist(search_lists),ncol=n_opt,nrow=2))
    
    #code to get which grapheme is the match--note that it finds the max of latest position, but there can be ties
    #so it further looks at max of grapheme length to break the tie. the code is longer still because
    #we need the original index of the grapheme, i.e., the absolute number, not relative (e.g., if the tie is between
    #grapheme #2 and #8, and #2 should win because its longer, this code returns #2 [as opposed to #1, meaning the first of
    #the tied options])
    g_sf <- which(select_g[,1]==max(select_g[,1]))[which.max(select_g[which(select_g[,1]==max(select_g[,1])),2])]
    g_sf <- g_opt$grapheme[g_sf]
    
    #NOTE: to make sure this FAILS for things it doesn't know, it needs to fail when the max of search list is <1
    ifelse(max(select_g[,1]) < 1, g_sf <- "FAILED", g_sf <- g_sf)
    
    #ES_ADD: this could be a failure to map /nj/ as in AN-CHA/AN-CHO; catch that here, it doesn't require updating positions so you can do it within this function
    if(g_sf == "FAILED" & p_sf_jointprior == "nj" && nrow(syllable_final_mappings[syllable_final_mappings$phoneme==p_sf,])>0){ #ES_ADD
      p_sf <- p_sf_jointprior #if it's failed and we're looking at an 'nj' situation, see if you can resolve that now...it's a long chunk of code here repeating
      g_opt <- syllable_final_mappings[syllable_final_mappings$phoneme==p_sf,] #ES_ADD
      n_opt <- length(g_opt$grapheme)#ES_ADD
      search_lists <- list() #ES_ADD
      for(j in (1:n_opt)){ #ES_ADD
        search_lists[[j]] <- matrix(NA,nrow=1,ncol=2) #ES_ADD
        search_lists[[j]][1] <- max(unlist(gregexpr(g_opt$grapheme[j], letter_str.latest))) + nchar(g_opt$grapheme[j]) -1 #ES_ADD
        ifelse (gregexpr(g_opt$grapheme[j], letter_str.latest)[[1]][1] == -1, search_lists[[j]][1] <- -1, search_lists[[j]][1] <- search_lists[[j]][1]) #ES_ADD
        search_lists[[j]][2] <- nchar(g_opt$grapheme[j])#ES_ADD
      }#ES_ADD
      select_g <- t(matrix(unlist(search_lists),ncol=n_opt,nrow=2))#ES_ADD
      g_sf <- which(select_g[,1]==max(select_g[,1]))[which.max(select_g[which(select_g[,1]==max(select_g[,1])),2])] #ES_ADD
      g_sf <- g_opt$grapheme[g_sf] #ES_ADD
      ifelse(max(select_g[,1]) < 1, g_sf <- "FAILED", g_sf <- g_sf) #ES_ADD
    } #ES_ADD
    
    #now catch instances where this may have failed because the previously mapped phoneme was too "greedy" OR potentially not greedy enough
    #for example, in CLOTHESLINE, it will have assigned the /l/ to SL, which means the /z/ is left to map from just CLOTHE (no S left!)
    #another example: EXHAUST is first parsed IG-ZOST and so the /c/ is mapped to just AU, and then it fails b/c /z/ can't map to XH
    if (g_sf == "FAILED") {
      if( paste0(p_sf,PGlist[[length(PGlist)-5]]) == "ts") { ##try to "nip in the bud" a potential /ts/ biphone...if the current p_sf = s is coming before trying to map "t"
        stri_sub(syll_struc0,nchar(syll_struc.latest)+1,nchar(syll_struc.latest)+1) <- "4" #will make the previously mapped 's' a p_sf to join the 't'
        syll_struc0 <<- syll_struc0
        flag.index <<- my.index-1 #this sets the flag index to be PRIOR to what just failed
        flag.ts <<- TRUE
        flag.restart <<- TRUE 
      }else{
        flag.index <<- my.index-1 #this sets the flag index to be PRIOR to what just failed
        flag.restart <<- TRUE
      }} 
    
    #update the letter_str reflect what has been mapped... 
    #note: this is where the string must be reversed, as well as the grapheme, in order to only replace the LAST instance
    #there does not seem to be any ready functions for doing so otherwise (only replace-first or replace-all functions)
    letter_str.latest <- stri_reverse(str_replace(stri_reverse(letter_str.latest),stri_reverse(g_sf),"_"))
    
    #if the replacement with _ procedure has left the last character as an E, then keep the _ so that the silent/final E can be mapped
    #if however the last character is now the _, simply remove it
    ifelse( str_sub(letter_str.latest,-1,-1)=="e", letter_str.latest <- letter_str.latest, letter_str.latest <- str_sub(letter_str.latest, end = -2))
    
    #now also update the parsed_syll to reflect what has been mapped. this is easier in general it's always the last phoneme
    #however, note that it removes more than 1 if the p_sf is a biphone 
    parsed_syll.latest <- str_sub(parsed_syll.latest,1,-(nchar(p_sf)+1))
    
    #and update syll_struc if the X flag is set, so that it becomes sf-sf:
    ifelse(map_x, str_sub(syll_struc.latest,-2,-1) <- "44", syll_struc.latest <- syll_struc.latest)
    ifelse(map_xe, str_sub(syll_struc.latest,-2,-1) <- "44", syll_struc.latest <- syll_struc.latest)
    
    #finally, have the latest syll_struc updated..note that TWO are removed if the p_sf is longer than 1 (as it is with /ks/)
    syll_struc.latest <- str_sub(syll_struc.latest,1,-(nchar(p_sf)+1))
    
    #list structure to store the mappings just obtained: the P, the G, and the position
    PGlist <- list()
    
    #the following stores the PHONEME, the GRAPHEME, and the POSITION, in that order:
    PGlist <- append(PGlist,matrix(c(p_sf,g_sf,"4",syll_struc.latest,parsed_syll.latest,letter_str.latest)))
    
    return(PGlist)
    print(PGlist)
  }
  
  #word-initial function
  map_wi <- function(p_wi,syll_struc.latest,parsed_syll.latest,letter_str.latest){
    
    tryCatch({
      ifelse(map_j_wi == TRUE, p_wi <- str_sub(parsed_syll0,1,2), p_wi <- p_wi) #if the map_j_wi flag was set the first time through, this will be sure to map the /ju/ or /je/ or /jU/
    },    error = function(e) NA)
    
    
    #find the options:
    g_opt <- word_initial_mappings[word_initial_mappings$phoneme==p_wi,]
    
    #if it's gone WAY wrong, there will be no options and it will crash. The following lines will put in a dummy placeholder phoneme and grapheme "6" to avoid this:
    if(nrow(g_opt)==0){
      g_opt <- word_initial_mappings[word_initial_mappings$phoneme=="s",][1,] #pull out the first option for 's' (which is possible in any position so guaranteed to be in the list of options)
      g_opt[1,1] <- "6"
      g_opt[1,2] <- "6"
    }
    
    n_opt <- length(g_opt$grapheme)
    search_lists <- list()
    
    for(j in (1:n_opt)){
      search_lists[[j]] <- matrix(NA,nrow=1,ncol=2)
      #first col should give grapheme latest location
      search_lists[[j]][1] <- max(unlist(gregexpr(g_opt$grapheme[j], letter_str.latest))) + nchar(g_opt$grapheme[j]) -1 #the max() gets the latest location of the START
      #of the grapheme...the nchar() gets the length of the grapheme...and the -1 is to get the final position. e.g., the DG in EDGE starts at char 2, its length is 2,
      #so the position it ends is 2+2-1 = 3
      #the above line returns false results sometimes when the grapheme isn't even present--e.g., it will return MIXED as having an AI_E just because the value comes out as 2 instead of -1, due to the length of AI_E
      #to fix this, simply overwrite the search_lists[[j]][1] value with gregexpr(g_opt$grapheme[j], letter_str.latest)[[1]][1] if that equals -1
      ifelse (gregexpr(g_opt$grapheme[j], letter_str.latest)[[1]][1] == -1, search_lists[[j]][1] <- -1, search_lists[[j]][1] <- search_lists[[j]][1])
      #second col gives the grapheme length 
      search_lists[[j]][2] <- nchar(g_opt$grapheme[j])
    }
    
    #unlist to get a matrix for selecting the grapheme
    select_g <- t(matrix(unlist(search_lists),ncol=n_opt,nrow=2))
    
    #code to get which grapheme is the match--note that it finds the max of latest position, but there can be ties
    #so it further looks at max of grapheme length to break the tie. the code is longer still because
    #we need the original index of the grapheme, i.e., the absolute number, not relative (e.g., if the tie is between
    #grapheme #2 and #8, and #2 should win because its longer, this code returns #2 [as opposed to #1, meaning the first of
    #the tied options])
    g_wi <- which(select_g[,1]==max(select_g[,1]))[which.max(select_g[which(select_g[,1]==max(select_g[,1])),2])]
    g_wi <- g_opt$grapheme[g_wi]
    
    #NOTE: to make sure this FAILS for things it doesn't know, it needs to fail when the max of search list is <1
    ifelse(max(select_g[,1]) < 1, g_wi <- "FAILED", g_wi <- g_wi)
    
    #now check for word-initial /ju/ or /je/ or /jU/
    map_j_wi = FALSE
    
    #check to see if this is a word beginning with /ju/, etc. (as opposed to /j/ + /u/)
    p_wi_jointpost <- paste0(p_wi,stri_sub(parsed_syll0,-(my.index-1),-(my.index-1))) #this pastes the correct phoneme_wi with the previously mapped phoneme, whatever it was
    ifelse(p_wi_jointpost == "ju" & g_wi == "FAILED" |p_wi_jointpost == "je" & g_wi == "FAILED" |p_wi_jointpost == "jU" & g_wi == "FAILED" , map_j_wi <- TRUE, map_j_wi <- FALSE) #if this gets set as true, it means we need to remap...
    
    #now do what's needed to restart the whole thing if we have /ju/ and the mapping failed
    if(map_j_wi == TRUE) {
      flag.restart <<- TRUE
      map_j_wi <<- map_j_wi
    } else { flag.restart <<- FALSE }
    
    
    #update the letter_str reflect what has been mapped... 
    #note: this is where the string must be reversed, as well as the grapheme, in order to only replace the LAST instance
    #there does not seem to be any ready functions for doing so otherwise (only replace-first or replace-all functions)
    letter_str.latest <- stri_reverse(str_replace(stri_reverse(letter_str.latest),stri_reverse(g_wi),"_"))
    
    #if the replacement with _ procedure has left the last character as an E, then keep the _ so that the silent/final E can be mapped
    #if however the last character is now the _, simply remove it
    ifelse( str_sub(letter_str.latest,-1,-1)=="e", letter_str.latest <- letter_str.latest, letter_str.latest <- str_sub(letter_str.latest, end = -2))
    
    #catch the FAIL if we're NOT now left with ""
    ifelse(letter_str.latest !="", g_wi <- "FAILED", g_wi <- g_wi)
    
    #now also update the parsed_syll to reflect what has been mapped. this is easier in general it's always the last phoneme
    #however, the X scenario must trigger removing /ks/
    parsed_syll.latest <- str_sub(parsed_syll.latest,1,-(nchar(p_wi)+1))
    
    #finally, have the latest syll_struc updated
    syll_struc.latest <- str_sub(syll_struc.latest,1,-2)
    
    #list structure to store the mappings just obtained: the P, the G, and the position
    PGlist <- list()
    
    #the following stores the PHONEME, the GRAPHEME, and the POSITION, in that order:
    PGlist <- append(PGlist,matrix(c(p_wi,g_wi,"1",syll_struc.latest,parsed_syll.latest,letter_str.latest)))
    
    #and finally, set the flag to indicate the whole word was mapped
    ifelse(flag.restart==TRUE,mapped_wi <<- FALSE, mapped_wi <<- TRUE)
    
    return(PGlist)
    print(PGlist)
  }
  
  #formatting data for output function
  generate_custom_sequence <- function(length) {
    sequence <- c()
    count <- 1
    skip <- 0
    
    while (length(sequence) < length) {
      if (skip < 3) {
        sequence <- c(sequence, count)
        count <- count + 1
      } else {
        count <- count + 3  # Skip three numbers
        skip <- -1  # Reset skip
      }
      
      skip <- skip + 1
    }
    
    return(sequence)
  }
  
  #strong catcher of problems: added 12/13/25
  count_failed <- function(x) sum(unlist(x) == "FAILED", na.rm = TRUE)
  
  ####################################
  #process words and create outputs (main function)
  ####################################
  
  all_words <- list()
  cleared <- matrix(NA,nrow=length(spelling))
  
  if(map_progress==TRUE) #only show progress bar if map_progress == TRUE; defaults to NO
    pb = txtProgressBar(min = 0, max = length(spelling), initial = 0) 
  
  for (i in 1:length(spelling)){
    
    word_index <- i
    
    #get the 3-row matrix from prep_word (syll_struc0,parsed_syll0, and parsed_syll0 in that order)
    #NEW 12/14/25: if the input string is so deficient it can't be parsed, catch that and return an NA frame
    prepped_word <- tryCatch(
      prep_word(word_index),
      error = function(e) {
        msg <- conditionMessage(e)
        # only intercept your parse failures (however you signal them)
        if (inherits(e, "parse_fail") || grepl("^__PARSE_FAIL__:", msg)) {
          # Force a “safe” pronunciation for THIS item so prep_word/map_PG can proceed
          pronunciation[word_index] <<- "a"
          # Now run prep_word again; everything downstream stays identical
          return(prep_word(word_index))
        }
        
        stop(e)  # all other errors should still error out
      }
    )
    
    syll_struc0 <- prepped_word[1,]
    parsed_syll0 <- prepped_word[2,]
    letter_str0 <- prepped_word[3,]
    
    mapped_wi <- FALSE 
    map_j_wi <- FALSE 
    map_j <- FALSE
    my.index <- 0 
    flag.index <- 0
    flag.index_j <- 0
    flag.nj <- 0
    flag.ts <- 0
    map_x <- FALSE
    map_xe <- FALSE
    map_xi <- FALSE
    count.restarts <- 0
    counter.FAILED <- 0 #added 12/13/25: stronger catch of problems
    failed_prev <- 0 #added 12/13/25: stronger catch of problems
    
    while (mapped_wi == FALSE & count.restarts < 10){
      
      flag.restart <- FALSE
      
      if(map_j_wi==TRUE){
        str_sub(syll_struc0,1,2) <- "1" #the syll_struc should be 1 at the front instead of 13, so that /ju/ is encountered as word initial as soon as the /u/ is encountered
      } else { syll_struc0 <- syll_struc0}
      
      #a matrix that has the current (last) phoneme and its position
      to_map <- matrix(NA,nrow=1,ncol=2)
      to_map[1,1] <- str_sub(parsed_syll0,-1,-1)
      to_map[1,2] <- str_sub(syll_struc0,-1,-1)
      
      if(nchar(syll_struc0)==1){
        p_wi <- to_map[1,1]
        my.index <- 1 
        PGlist <- matrix(map_wi(p_wi = p_wi, syll_struc.latest = syll_struc0, parsed_syll.latest = parsed_syll0, letter_str.latest = letter_str0))
      } else {
        if (to_map[1,2] == 5) {
          p_wf <- to_map[1,1]
          my.index <- 1 #p_wf will always be the first thing to try to map
          PGlist <- map_wf(p_wf = p_wf,syll_struc0 = syll_struc0,parsed_syll0 = parsed_syll0,letter_str0 = letter_str0)
        } }
      #12/13/25: counter for problems other than restarts:
      failed_now  <- count_failed(PGlist)
      counter.FAILED <- counter.FAILED + max(0, failed_now - failed_prev)
      failed_prev <- failed_now
      if (counter.FAILED >= 3) {
        flag.restart <- TRUE   # or just break, depending on what you want
      }
      
      # if the first mapping step uses up all letters but leaves phonemes,
      # treat it as an immediate failure so the existing FAILED branch handles it
      if (PGlist[[length(PGlist)]] == "" && PGlist[[length(PGlist) - 1]] != "") {
        PGlist[[2]] <- "FAILED"
      }
      
      ###WORD-FINAL should now be assigned. the while loop will go through the remaining phonemes until none are left
      #HOWEVER! words that fail immediately (i.e., their last phoneme can't map onto anything in the spelling) will get hung up here
      #so add a check that says if you failed out of the gate, return "NA" and move on to the next item
      if (PGlist[[2]]=="FAILED"){
        cleared[i] <- FALSE 
        my.format <- generate_custom_sequence(nchar(syll_struc0)*3)
        all_words[[i]] <- rev(data.frame(matrix(unlist(PGlist)[my.format],nrow=3)))
        all_words[[i]] <- all_words[[i]][,!is.na(all_words[[i]][1,])]
        all_words[[i]] <- data.frame(all_words[[i]])
        count.restarts = 10 #critical! if you don't set this to max out the restarts it will go on forever
        
      } else {  #NOTE: this is an if...else that says IF it already failed, just assigned FALSE to clear and put that unfinished data frame into the all_words results... ELSE go on to do the while loop!
        
        while (PGlist[[length(PGlist)]] != "") {
          
          if(flag.restart == TRUE){
            count.restarts <- count.restarts+1
            break}
          
          to_map[1,1] <- str_sub(PGlist[[length(PGlist)-1]],-1,-1)
          to_map[1,2] <- str_sub(PGlist[[length(PGlist)-2]],-1,-1)
          syll_struc.latest <- PGlist[[length(PGlist)-2]]
          parsed_syll.latest <- PGlist[[length(PGlist)-1]]
          letter_str.latest <- PGlist[[length(PGlist)]]
          
          if (to_map[1,2] == 3) {
            p_m <- to_map[1,1]
            my.index <- my.index + 1 #increase my.index only if it's been determined that the next phoneme is this position
            PGlist <- append(PGlist,map_m(p_m = p_m, syll_struc.latest = syll_struc.latest, parsed_syll.latest = parsed_syll.latest, letter_str.latest = letter_str.latest))
          } 
          #12/13/25: counter for problems other than restarts:
          failed_now  <- count_failed(PGlist)
          counter.FAILED <- counter.FAILED + max(0, failed_now - failed_prev)
          failed_prev <- failed_now
          if (counter.FAILED >= 3) {
            flag.restart <- TRUE   # or just break, depending on what you want
          }
          
          if(flag.restart == TRUE){
            count.restarts <- count.restarts+1
            break}
          
          to_map[1,1] <- str_sub(PGlist[[length(PGlist)-1]],-1,-1)
          to_map[1,2] <- str_sub(PGlist[[length(PGlist)-2]],-1,-1)
          syll_struc.latest <- PGlist[[length(PGlist)-2]]
          parsed_syll.latest <- PGlist[[length(PGlist)-1]]
          letter_str.latest <- PGlist[[length(PGlist)]]
          
          #ES_ADD: a word-initial X mapped to /ks/ as in XENOFOBIA will be finished at this point--after mapping medial because the /s/ is medial--so it will be done but it will show it ended on medial posotion (3) and the mapped_wi won't be set
          #ES_ADD: check for this situation by seeing if to_map[1,2] is EMPTY
          if( to_map[1,2] == ""){ #nothing is left to map, therefore we're done
            mapped_wi <- TRUE #so set that to be true
            PGlist[[length(PGlist)-3]] <- 1 #from the end of the list, this holds the syllabic position--it will have been left as 3 (medial) but should now be 1 (word initial)
          }
          
          if (to_map[1,2] == 2) {
            p_si <- to_map[1,1]
            my.index <- my.index + 1 #increase my.index only if it's been determined that the next phoneme is this position
            PGlist <- append(PGlist,map_si(p_si = p_si, syll_struc.latest = syll_struc.latest, parsed_syll.latest = parsed_syll.latest, letter_str.latest = letter_str.latest))
          } 
          #12/13/25: counter for problems other than restarts:
          failed_now  <- count_failed(PGlist)
          counter.FAILED <- counter.FAILED + max(0, failed_now - failed_prev)
          failed_prev <- failed_now
          if (counter.FAILED >= 3) {
            flag.restart <- TRUE   # or just break, depending on what you want
          }
          
          if(flag.restart == TRUE){
            count.restarts <- count.restarts+1
            break}
          
          to_map[1,1] <- str_sub(PGlist[[length(PGlist)-1]],-1,-1)
          to_map[1,2] <- str_sub(PGlist[[length(PGlist)-2]],-1,-1)
          syll_struc.latest <- PGlist[[length(PGlist)-2]]
          parsed_syll.latest <- PGlist[[length(PGlist)-1]]
          letter_str.latest <- PGlist[[length(PGlist)]]
          
          if (to_map[1,2] == 4) {
            p_sf <- to_map[1,1]
            my.index <- my.index + 1 #increase my.index only if it's been determined that the next phoneme is this position
            PGlist <- append(PGlist,map_sf(p_sf = p_sf, syll_struc.latest = syll_struc.latest, parsed_syll.latest = parsed_syll.latest, letter_str.latest = letter_str.latest))
          } 
          #12/13/25: counter for problems other than restarts:
          failed_now  <- count_failed(PGlist)
          counter.FAILED <- counter.FAILED + max(0, failed_now - failed_prev)
          failed_prev <- failed_now
          if (counter.FAILED >= 3) {
            flag.restart <- TRUE   # or just break, depending on what you want
          }
          
          if(flag.restart == TRUE){
            count.restarts <- count.restarts+1
            break}
          
          to_map[1,1] <- str_sub(PGlist[[length(PGlist)-1]],-1,-1)
          to_map[1,2] <- str_sub(PGlist[[length(PGlist)-2]],-1,-1)
          syll_struc.latest <- PGlist[[length(PGlist)-2]]
          parsed_syll.latest <- PGlist[[length(PGlist)-1]]
          letter_str.latest <- PGlist[[length(PGlist)]]
          
          if (to_map[1,2] == 1) {
            p_wi <- to_map[1,1]
            my.index <- my.index + 1 #increase my.index only if it's been determined that the next phoneme is this position
            PGlist <- append(PGlist,map_wi(p_wi = p_wi, syll_struc.latest = syll_struc.latest, parsed_syll.latest = parsed_syll.latest, letter_str.latest = letter_str.latest))
          }
          #12/13/25: counter for problems other than restarts:
          failed_now  <- count_failed(PGlist)
          counter.FAILED <- counter.FAILED + max(0, failed_now - failed_prev)
          failed_prev <- failed_now
          if (counter.FAILED >= 3) {
            flag.restart <- TRUE   # or just break, depending on what you want
          }     
          
          if(flag.restart == TRUE){
            count.restarts <- count.restarts+1
            break}
          
          #NEW 12/15/25: some deficient strings can parse all of the phonemes while leaving behind letters--those should count as failures but aren't otherwise caught:
          if(PGlist[[length(PGlist)]] == "" && PGlist[[length(PGlist)-1]] != ""){
            failed_now  <- count_failed(PGlist)+1
            counter.FAILED <- counter.FAILED + max(0, failed_now - failed_prev)
            failed_prev <- failed_now
          }
          if (counter.FAILED >= 3) {
            flag.restart <- TRUE   # or just break, depending on what you want
          }     
          
          
          if(mapped_wi == TRUE) break
        } #this ends the interior while loop that iterates through the phonemes
        
        my.format <- generate_custom_sequence(nchar(syll_struc0)*3)
        my.format <- my.format + 1
        my.format[1:3]<-my.format[1:3]-1
        all_words[[i]] <- rev(data.frame(matrix(unlist(PGlist)[my.format],nrow=3)))
        all_words[[i]] <- all_words[[i]][,!is.na(all_words[[i]][1,])]
        all_words[[i]] <- data.frame(all_words[[i]])
        cleared[i] <- ifelse(mapped_wi == TRUE & !"FAILED" %in% (all_words[[i]][2,]) == TRUE,TRUE,FALSE)
        
      } #this ends the if...else that skips the while loop if it failed immediately
      
      if(map_progress==TRUE) #only show progress bar if map_progress == TRUE; defaults to NO
        setTxtProgressBar(pb,i)
      
    } #this ends the exterior while loop that runs through the words
  }
  return(list(all_words,cleared))
}

# v2.1 optimization: retain last map_PG call and reuse it when the same
# spelling/pronunciation vectors are requested again (e.g., by map_ONC/OC/OR).
map_PG <- local({
  map_PG_uncached <- map_PG
  .last_spelling <- NULL
  .last_pronunciation <- NULL
  .last_result <- NULL
  
  function(spelling, pronunciation, map_progress = FALSE) {
    spelling_chr <- as.character(spelling)
    pronunciation_chr <- as.character(pronunciation)
    
    if (!is.null(.last_spelling) &&
        identical(spelling_chr, .last_spelling) &&
        identical(pronunciation_chr, .last_pronunciation)) {
      return(.last_result)
    }
    
    out <- map_PG_uncached(spelling = spelling, pronunciation = pronunciation, map_progress = map_progress)
    .last_spelling <<- spelling_chr
    .last_pronunciation <<- pronunciation_chr
    .last_result <<- out
    out
  }
})

map_ONC <- function(spelling, pronunciation, map_progress=FALSE){
  
  library(stringi)
  
  #list vowels per in-house code
  vowels <- c("5", "O", "8", "je", "j3", "ju", "jU", "o", "2", 
              "@", "a", "e", "E", "3", "i", "1", "c", "u", "U", "^")
  
  #first run all the words through map_PG
  hold <- map_PG(spelling,pronunciation, map_progress = map_progress)
  
  #only show progress bar if map_progress == TRUE; defaults to NO
  if(map_progress==TRUE) 
    pb = txtProgressBar(min = 0, max = length(spelling), initial = 0) 
  
  #needed internal function for creating different clusters:
  insertSpacesMatching <- function(originalString, matchString) {
    originalString <- gsub("\\s", "", originalString)  # Remove any existing spaces from the original string
    spaces <- gregexpr(" ", matchString)[[1]] - 1  # Find the positions of spaces in the match string and adjust for indexing
    
    for (pos in spaces) {
      originalString <- paste0(substr(originalString, 1, pos), " ", substr(originalString, pos + 1, nchar(originalString)))
    }
    
    return(originalString)
  }
  
  for(i in 1:length(spelling)){
    tryCatch(
      {if(which(is.na(hold[[1]][[i]][1,])) > 0)
        hold[[1]][[i]] <- hold[[1]][[i]][,-which(is.na(hold[[1]][[i]][1,]))] #remove excess columns (comes from remapping of diphthongs, X, etc.)
      }, error=function(e) NA)
    
    hold[[1]][[i]][4,] <- hold[[1]][[i]][1,]
    hold[[1]][[i]][4,][hold[[1]][[i]][1,] %in% vowels] <- "V"
    hold[[1]][[i]][4,][!hold[[1]][[i]][1,] %in% vowels] <- "C"
    
    #label columns with alphabetic indices for later cbinding
    colnames(hold[[1]][[i]]) <- c(letters,LETTERS)[1:ncol(hold[[1]][[i]])]
    
    #get complete CV string
    CVstring <- paste0(hold[[1]][[i]][4,],collapse="")
    
    #get complete position string
    posstring <- paste0(hold[[1]][[i]][3,],collapse="")
    
    #get column indices for eventual pasting
    colstring <- paste0(c(letters,LETTERS)[1:nchar(CVstring)],collapse="")
    
    #find the positions of 'V' characters
    split_positions1 <- gregexpr("V", CVstring)[[1]]
    
    #insert spaces before AND AFTER 'V' characters -- whatever you do HERE is what determines ONC, OC, or OR
    CVstring <- gsub("V", " V ", CVstring)
    
    #make the posstring and colstring match the regrouped CVstring
    posstring <- insertSpacesMatching(posstring,CVstring)
    colstring <- insertSpacesMatching(colstring,CVstring)
    
    #find the positions of '2' characters
    split_positions2 <- gregexpr("2", posstring)[[1]]
    
    #insert spaces before '2' characters
    posstring <- gsub("2", " 2", posstring)
    
    #make the CVstring and colstring match the regrouped posstring
    CVstring <- insertSpacesMatching(CVstring,posstring)
    colstring <- insertSpacesMatching(colstring,posstring)
    
    #vowel-initial words will have whitespace at the start, strip that off:
    CVstring <- trimws(CVstring,"left")
    colstring <- trimws(colstring,"left")
    posstring <- trimws(posstring,"left")
    
    #convert colstring to matrix of column indices to paste together:
    colstring <- as.matrix(read.table(text=colstring))
    
    #make a matrix to hold the remapping
    result <- matrix(NA,ncol=length(colstring),nrow=4)
    
    #now loop through each to-be-combined element of colstring to fill in result matrix
    for (j in 1:length(colstring)) {
      cols_to_paste <- unlist(strsplit(colstring[j], ""))
      result[, j] <- apply(hold[[1]][[i]][, cols_to_paste, drop = FALSE], 1, paste, collapse = "")
    }
    result <- as.data.frame(result)
    
    #format the new ONC matrix: onset clusters will appear as 13(3) or 23(3) and should be just 1 or 2...codas will be 3(3)4 or 3(3)5 and should be 4 or 5, but nuclei should be left alone
    hold[[1]][[i]] <- result
    
    #run through each position code in row 3--since nucleii are always only one phoneme (in English anyway...), it is the length>1 mappings that need adjusting:
    #if the minimum is <3 keep just that (e.g., 13 becomes 1), if the maximum>3 keep just that (e.g., 35 becomes 5)
    
    for (j in 1:ncol(hold[[1]][[i]])){
      if(nchar(hold[[1]][[i]][4,j]) > 1) #only need a change if there is a cluster
        if(as.numeric(substr(hold[[1]][[i]][3,j],1,1)) < 3) { #if it starts 1 or 2
          hold[[1]][[i]][3,j] <- substr(hold[[1]][[i]][3,j],1,1)} else { #keep the first thing (the 1 or 2)
            hold[[1]][[i]][3,j] <- substr(hold[[1]][[i]][3,j],nchar(hold[[1]][[i]][3,j]),nchar(hold[[1]][[i]][3,j])) #otherwise keep the last thing (it'll be 4 or 5)
            
          }}
    
    hold[[1]][[i]] <- data.frame(hold[[1]][[i]][-4,]) #remove VC structural
    
    if(map_progress == TRUE) #only do progress bar if requested
      setTxtProgressBar(pb,i)
  }
  
  return(hold)
}
map_OC <- function(spelling, pronunciation, map_progress=FALSE){
  
  library(stringi)
  
  #list vowels per in-house code
  vowels <- c("5", "O", "8", "je", "j3", "ju", "jU", "o", "2", 
              "@", "a", "e", "E", "3", "i", "1", "c", "u", "U", "^")
  
  #first run all the words through map_PG
  hold <- map_PG(spelling,pronunciation, map_progress = map_progress)
  
  #only show progress bar if map_progress == TRUE; defaults to NO
  if(map_progress==TRUE) 
    pb = txtProgressBar(min = 0, max = length(spelling), initial = 0) 
  
  #needed internal function for creating different clusters:
  insertSpacesMatching <- function(originalString, matchString) {
    originalString <- gsub("\\s", "", originalString)  # Remove any existing spaces from the original string
    spaces <- gregexpr(" ", matchString)[[1]] - 1  # Find the positions of spaces in the match string and adjust for indexing
    
    for (pos in spaces) {
      originalString <- paste0(substr(originalString, 1, pos), " ", substr(originalString, pos + 1, nchar(originalString)))
    }
    
    return(originalString)
  }
  
  for(i in 1:length(spelling)){
    tryCatch(
      {if(which(is.na(hold[[1]][[i]][1,])) > 0)
        hold[[1]][[i]] <- hold[[1]][[i]][,-which(is.na(hold[[1]][[i]][1,]))] #remove excess columns (comes from remapping of diphthongs, X, etc.)
      }, error=function(e) NA)
    
    hold[[1]][[i]][4,] <- hold[[1]][[i]][1,]
    hold[[1]][[i]][4,][hold[[1]][[i]][1,] %in% vowels] <- "V"
    hold[[1]][[i]][4,][!hold[[1]][[i]][1,] %in% vowels] <- "C"
    
    #label columns with alphabetic indices for later cbinding
    colnames(hold[[1]][[i]]) <- c(letters,LETTERS)[1:ncol(hold[[1]][[i]])]
    
    #get complete CV string
    CVstring <- paste0(hold[[1]][[i]][4,],collapse="")
    
    #get complete position string
    posstring <- paste0(hold[[1]][[i]][3,],collapse="")
    
    #get column indices for eventual pasting
    colstring <- paste0(c(letters,LETTERS)[1:nchar(CVstring)],collapse="")
    
    #find the positions of 'V' characters
    split_positions1 <- gregexpr("V", CVstring)[[1]]
    
    #insert spaces AFTER 'V' characters -- whatever you do HERE is what determines ONC, OC, or OR
    CVstring <- gsub("V", "V ", CVstring)
    
    #make the posstring and colstring match the regrouped CVstring
    posstring <- insertSpacesMatching(posstring,CVstring)
    colstring <- insertSpacesMatching(colstring,CVstring)
    
    #find the positions of '2' characters
    split_positions2 <- gregexpr("2", posstring)[[1]]
    
    #insert spaces before '2' characters
    posstring <- gsub("2", " 2", posstring)
    
    #make the CVstring and colstring match the regrouped posstring
    CVstring <- insertSpacesMatching(CVstring,posstring)
    colstring <- insertSpacesMatching(colstring,posstring)
    
    #vowel-final words will have whitespace at the end, strip that off:
    CVstring <- trimws(CVstring,"right")
    colstring <- trimws(colstring,"right")
    posstring <- trimws(posstring,"right")
    
    #and some words will have duplicated internal whitespaces, strip those off:
    CVstring <- gsub("\\s+", " ", CVstring)
    colstring <- gsub("\\s+", " ", colstring)
    posstring <- gsub("\\s+", " ", posstring)
    
    #convert colstring to matrix of column indices to paste together:
    colstring <- as.matrix(read.table(text=colstring))
    
    #make a matrix to hold the remapping
    result <- matrix(NA,ncol=length(colstring),nrow=4)
    
    #now loop through each to-be-combined element of colstring to fill in result matrix
    for (j in 1:length(colstring)) {
      cols_to_paste <- unlist(strsplit(colstring[j], ""))
      result[, j] <- apply(hold[[1]][[i]][, cols_to_paste, drop = FALSE], 1, paste, collapse = "")
    }
    result <- as.data.frame(result)
    
    #format the new OC matrix: onset clusters will appear as 13(3) or 23(3) and should be just 1 or 2...codas will be 3(3)4 or 3(3)5 and should be 4 or 5, but nuclei should be left alone
    hold[[1]][[i]] <- result
    
    #run through each position code in row 3--oncleii may look like 13(3) or 23(3) and should become just 1 or 2...codas can be like 3(3)4 or 3(3)5 and should become just 4 or 5
    #if the minimum is <3 keep just that (e.g., 13 becomes 1), if the maximum>3 keep just that (e.g., 35 becomes 5)
    #REWRITE: if the min = 1, make it 1...if the max = 5, make it five. if neither of those conditions are true, than min<3 = min, if max>3 =max.
    for (j in 1:ncol(hold[[1]][[i]])) {
      # only need a change if there is a cluster
      if (nchar(hold[[1]][[i]][4, j]) > 1) {
        
        pos_str <- hold[[1]][[i]][3, j]
        
        # If position string is missing or empty, do nothing
        if (is.na(pos_str) || pos_str == "") next
        
        # Extract first and last characters
        first_char <- substr(pos_str, 1, 1)
        last_char  <- substr(pos_str, nchar(pos_str), nchar(pos_str))
        
        # Coerce to numeric safely
        first_num <- suppressWarnings(as.numeric(first_char))
        last_num  <- suppressWarnings(as.numeric(last_char))
        
        # If either fails to parse, do nothing
        if (is.na(first_num) || is.na(last_num)) next
        
        # Now apply the ORIGINAL map_OC logic, numerically:
        if (first_num == 1) {
          hold[[1]][[i]][3, j] <- 1
        } else if (last_num == 5) {
          hold[[1]][[i]][3, j] <- 5
        } else if (first_num < 3) {
          # starts with 1 or 2
          hold[[1]][[i]][3, j] <- first_num
        } else {
          # otherwise keep the last thing (typically 4)
          hold[[1]][[i]][3, j] <- last_num
        }
      }
    }
    
    
    
    hold[[1]][[i]] <- data.frame(hold[[1]][[i]][-4,]) #remove VC structural
    
    if(map_progress == TRUE) #only do progress bar if requested
      setTxtProgressBar(pb,i)
  }
  
  return(hold)
}
map_OR <- function(spelling, pronunciation, map_progress=FALSE){
  
  library(stringi)
  
  #list vowels per in-house code
  vowels <- c("5", "O", "8", "je", "j3", "ju", "jU", "o", "2", 
              "@", "a", "e", "E", "3", "i", "1", "c", "u", "U", "^")
  
  #first run all the words through map_PG
  hold <- map_PG(spelling,pronunciation, map_progress = map_progress)
  
  #only show progress bar if map_progress == TRUE; defaults to NO
  if(map_progress==TRUE) 
    pb = txtProgressBar(min = 0, max = length(spelling), initial = 0) 
  
  #needed internal function for creating different clusters:
  insertSpacesMatching <- function(originalString, matchString) {
    originalString <- gsub("\\s", "", originalString)  # Remove any existing spaces from the original string
    spaces <- gregexpr(" ", matchString)[[1]] - 1  # Find the positions of spaces in the match string and adjust for indexing
    
    for (pos in spaces) {
      originalString <- paste0(substr(originalString, 1, pos), " ", substr(originalString, pos + 1, nchar(originalString)))
    }
    
    return(originalString)
  }
  
  for(i in 1:length(spelling)){
    tryCatch(
      {if(which(is.na(hold[[1]][[i]][1,])) > 0)
        hold[[1]][[i]] <- hold[[1]][[i]][,-which(is.na(hold[[1]][[i]][1,]))] #remove excess columns (comes from remapping of diphthongs, X, etc.)
      }, error=function(e) NA)
    
    hold[[1]][[i]][4,] <- hold[[1]][[i]][1,]
    hold[[1]][[i]][4,][hold[[1]][[i]][1,] %in% vowels] <- "V"
    hold[[1]][[i]][4,][!hold[[1]][[i]][1,] %in% vowels] <- "C"
    
    #label columns with alphabetic indices for later cbinding
    colnames(hold[[1]][[i]]) <- c(letters,LETTERS)[1:ncol(hold[[1]][[i]])]
    
    #get complete CV string
    CVstring <- paste0(hold[[1]][[i]][4,],collapse="")
    
    #get complete position string
    posstring <- paste0(hold[[1]][[i]][3,],collapse="")
    
    #get column indices for eventual pasting
    colstring <- paste0(c(letters,LETTERS)[1:nchar(CVstring)],collapse="")
    
    #find the positions of 'V' characters
    split_positions1 <- gregexpr("V", CVstring)[[1]]
    
    #insert spaces BEFORE 'V' characters -- whatever you do HERE is what determines ONC, OC, or OR
    CVstring <- gsub("V", " V", CVstring)
    
    #make the posstring and colstring match the regrouped CVstring
    posstring <- insertSpacesMatching(posstring,CVstring)
    colstring <- insertSpacesMatching(colstring,CVstring)
    
    #find the positions of '2' characters
    split_positions2 <- gregexpr("2", posstring)[[1]]
    
    #insert spaces before '2' characters
    posstring <- gsub("2", " 2", posstring)
    
    #make the CVstring and colstring match the regrouped posstring
    CVstring <- insertSpacesMatching(CVstring,posstring)
    colstring <- insertSpacesMatching(colstring,posstring)
    
    #vowel-final words will have whitespace at the beginning, strip that off:
    CVstring <- trimws(CVstring,"left")
    colstring <- trimws(colstring,"left")
    posstring <- trimws(posstring,"left")
    
    #and some words will have duplicated internal whitespaces, strip those off:
    CVstring <- gsub("\\s+", " ", CVstring)
    colstring <- gsub("\\s+", " ", colstring)
    posstring <- gsub("\\s+", " ", posstring)
    
    #convert colstring to matrix of column indices to paste together:
    colstring <- as.matrix(read.table(text=colstring))
    
    #make a matrix to hold the remapping
    result <- matrix(NA,ncol=length(colstring),nrow=4)
    
    #now loop through each to-be-combined element of colstring to fill in result matrix
    for (j in 1:length(colstring)) {
      cols_to_paste <- unlist(strsplit(colstring[j], ""))
      result[, j] <- apply(hold[[1]][[i]][, cols_to_paste, drop = FALSE], 1, paste, collapse = "")
    }
    result <- as.data.frame(result)
    
    #format the new OR matrix: onset clusters will appear as 13(3) or 23(3) and should be just 1 or 2...codas will be 3(3)4 or 3(3)5 and should be 4 or 5, but nuclei should be left alone
    hold[[1]][[i]] <- result
    
    #run through each position code in row 3--rimes may look like 3(3)4 or 3(3)5 and should become just 3 or 5...onsets can be like 13(3) or 23(3) and should become just 1 or 2
    #if the minimum is <3 keep just that (e.g., 13 becomes 1), if the maximum>3 keep just that (e.g., 35 becomes 5)
    #REWRITE: FOR RIMES, if the min = 1, make it 1...UNLESS if the max = 5, make it five (always make it 5 if there's a 5). if neither of those conditions are true, than min<3 = min, if max>3 =max.
    for (j in 1:ncol(hold[[1]][[i]])){
      if(nchar(hold[[1]][[i]][4,j]) > 1) #only need a change if there is a cluster
        if(as.numeric(substr(hold[[1]][[i]][3,j],nchar(hold[[1]][[i]][3,j]),nchar(hold[[1]][[i]][3,j]))) == 5){ #new line
          hold[[1]][[i]][3,j] <- 5 #new line
        } else { #new line
          if(as.numeric(substr(hold[[1]][[i]][3,j],1,1)) == 1){ #new line
            hold[[1]][[i]][3,j] <- 1 #new line
          } else { #new line, but the following are from before:
            if(as.numeric(substr(hold[[1]][[i]][3,j],1,1)) < 3) { #if it starts 1 or 2
              hold[[1]][[i]][3,j] <- substr(hold[[1]][[i]][3,j],1,1)} else { #keep the first thing (the 1 or 2) #NEW: Except now that would only be from a 2, since a 1 was already made one
                hold[[1]][[i]][3,j] <- substr(hold[[1]][[i]][3,j],nchar(hold[[1]][[i]][3,j]),nchar(hold[[1]][[i]][3,j])) #otherwise keep the last thing (it'll be 4 or 5) #NEW: Except now that will only be for a 4, since a 5 was already made five
              }}}} #new: added two more }}
    
    hold[[1]][[i]] <- data.frame(hold[[1]][[i]][-4,]) #remove VC structural
    
    if(map_progress == TRUE) #only do progress bar if requested
      setTxtProgressBar(pb,i)
  }
  
  return(hold)
}

make_tables <- function(mapped_words,weight=FALSE,positional=TRUE){
  
  library(dplyr)
  library(tidyr)
  
  #create a dummy word to ensure place holders for all syllabic positions:
  dummy_word <- map_PG("hardy","hardi")
  dummy_word[[1]][[1]][,1]<-c(7,7,1)
  dummy_word[[1]][[1]][,2]<-c(7,7,3)
  dummy_word[[1]][[1]][,3]<-c(7,7,4)
  dummy_word[[1]][[1]][,4]<-c(7,7,2)
  dummy_word[[1]][[1]][,5]<-c(7,7,5)
  
  #if weight != FALSE, dummy word should have a dummy frequency value
  if(length(weight)>1){
    dummy_word[[1]][[1]][4,] <- 1
  }
  
  #internal function convert_matrix_nofreq()
  convert_matrix_nofreq <- function(mat) {
    df <- as.data.frame(t(mat))
    colnames(df) <- c("phoneme", "grapheme", "position")
    separator <- data.frame(phoneme = "----------", grapheme = "----------", position = "----------")
    return(rbind(df, separator))
  }
  
  #internal function convert_matrix_freq()
  convert_matrix_freq <- function(mat) {
    df <- as.data.frame(t(mat))
    colnames(df) <- c("phoneme", "grapheme", "position", "weight")
    separator <- data.frame(phoneme = "----------", grapheme = "----------", position = "----------", weight = "----------")
    return(rbind(df, separator))
  }
  
  #internal_function append_freq
  append_freq <- function(mapped_words,weight){
    for(i in 1:length(weight)){
      mapped_words[[1]][[i]][4,]<-weight[i]
    }
    return(mapped_words)
  }
  
  if(length(weight)>1){mapped_words <- append_freq(mapped_words,weight)} #appends frequencies if provided, otherwise does nothing
  
  if(length(weight)==1){
    all_words_df <- do.call(rbind, lapply(mapped_words[[1]], convert_matrix_nofreq))
    all_words_df <- rbind(all_words_df, convert_matrix_nofreq(dummy_word[[1]][[1]])) #tack on the dummy word to ensure all 5 positions are present
    all_words_df <- all_words_df[1:(nrow(all_words_df) - 1), ]
    rownames(all_words_df) <- NULL
  } else {
    all_words_df <- do.call(rbind, lapply(mapped_words[[1]], convert_matrix_freq))
    all_words_df <- rbind(all_words_df, convert_matrix_freq(dummy_word[[1]][[1]])) #tack on the dummy word to ensure all 5 positions are present
    all_words_df <- all_words_df[1:(nrow(all_words_df) - 1), ]
    rownames(all_words_df) <- NULL
  }
  
  filtered_df <- all_words_df %>%
    filter(phoneme != "----------" & grapheme != "----------")
  
  
  if(positional==FALSE){
    filtered_df$position <- 1 #if you want no positions at all, turn all positions into "1" at this point
  }
  
  if(length(weight)>1){
    phoneme_grapheme_frequency <- filtered_df %>%
      group_by(position, phoneme, grapheme) %>%
      summarize(freq = sum(as.numeric(weight))) %>%
      ungroup()
  } else {
    phoneme_grapheme_frequency <- filtered_df %>%
      group_by(position, phoneme, grapheme) %>%
      summarize(freq = n()) %>%
      ungroup()
  }
  
  result_df <- phoneme_grapheme_frequency %>%
    spread(key = position, value = freq, fill = 0)
  
  if(positional==FALSE){
    result_df$`2` <- result_df$`1`
    result_df$`3` <- result_df$`1`
    result_df$`4` <- result_df$`1`
    result_df$`5` <- result_df$`1`}
  
  pg <- result_df %>%
    group_by(phoneme) %>%
    mutate(across(`1`:`5`, 
                  ~ round((. / sum(.)), 4), 
                  .names = "prob_{.col}")) %>%
    ungroup() %>%
    transmute(phoneme = paste(phoneme), grapheme = paste(grapheme),
              wi = prob_1, si = prob_2, sm = prob_3, sf = prob_4, wf = prob_5) %>%
    mutate(across(c(wi, si, sm, sf, wf), 
                  ~ as.character(ifelse(is.na(.), "0", .))))
  
  gp <- result_df %>%
    group_by(grapheme) %>%
    mutate(across(`1`:`5`, 
                  ~ round((. / sum(.)), 4), 
                  .names = "prob_{.col}")) %>%
    ungroup() %>%
    transmute(phoneme = paste(phoneme), grapheme = paste(grapheme),
              wi = prob_1, si = prob_2, sm = prob_3, sf = prob_4, wf = prob_5) %>%
    mutate(across(c(wi, si, sm, sf, wf), 
                  ~ as.character(ifelse(is.na(.), "0", .))))
  
  result_df <- result_df %>%
    rename(wi = `1`, si = `2`, sm = `3`, sf = `4`, wf = `5`)
  
  
  phoneme_freq <- result_df %>%
    group_by(phoneme) %>%
    summarise(wi = round(log10(sum(wi) + 1), 4),
              si = round(log10(sum(si) + 1), 4),
              sm = round(log10(sum(sm) + 1), 4),
              sf = round(log10(sum(sf) + 1), 4),
              wf = round(log10(sum(wf) + 1), 4)) %>%
    ungroup()
  
  grapheme_freq <- result_df %>%
    group_by(grapheme) %>%
    summarise(wi = round(log10(sum(wi) + 1), 4),
              si = round(log10(sum(si) + 1), 4),
              sm = round(log10(sum(sm) + 1), 4),
              sf = round(log10(sum(sf) + 1), 4),
              wf = round(log10(sum(wf) + 1), 4)) %>%
    ungroup()
  
  phoneme_grapheme_freq <- result_df %>%
    group_by(phoneme, grapheme) %>%
    summarise(wi = round(log10(sum(wi) + 1), 4),
              si = round(log10(sum(si) + 1), 4),
              sm = round(log10(sum(sm) + 1), 4),
              sf = round(log10(sum(sf) + 1), 4),
              wf = round(log10(sum(wf) + 1), 4)) %>%
    ungroup()
  
  
  #now drop the 7's from the dummy word
  pg <- pg[pg$phoneme!="7",]
  gp <- gp[gp$grapheme!="7",]
  phoneme_freq <- phoneme_freq[phoneme_freq$phoneme!="7",]
  grapheme_freq <- grapheme_freq[grapheme_freq$grapheme!="7",]
  phoneme_grapheme_freq <- phoneme_grapheme_freq[phoneme_grapheme_freq$grapheme!="7",]
  
  return(list(pg=pg,gp=gp,p_freq=phoneme_freq,g_freq=grapheme_freq,pg_freq=phoneme_grapheme_freq))
}

.add_pp2_context <- function(allbranches_df, spelling,
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

parse_corpus <- function(corpus,mapped_corpus){
  
  library(progress)
  library(readxl)
  library(data.tree)
  
  ## 1. Check that globals exist
  required_globals <- c(
    "word_initial_mappings",
    "word_final_mappings",
    "syllable_medial_mappings",
    "syllable_final_mappings",
    "syllable_initial_mappings"
  )
  
  missing_globals <- required_globals[!vapply(required_globals, exists, logical(1), inherits = TRUE)]
  if (length(missing_globals) > 0) {
    stop("Missing required mapping objects in the global environment: ",
         paste(missing_globals, collapse = ", "))
  }
  
  ## 2. Build internal mapping objects (LOCAL to parse_corpus)
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
  
  ## 3. Define internal functions.
  #internal function: for parsing strings of letters into potential graphemes--critical function!
  parse_string <- function(startword){
    
    consonants <- c("b","c","d","f","g","h","j","k","l","m","n","p","q","r","s","t","v","x","z")
    vowels <- c("a|e|i|o|u|y") #including Y because it can literally be the only vowel, like in MYTH, but not W because it never is ALONE as the vowel (always OW or AW, etc.)
    
    #consonants will be removed to search for _E mappings--note that this DOES exclude W in addition to Y, because W can be part of _E mappings as in OWE
    remove_consonants <- c("B"="_","C"="_","D"="_","F"="_","G"="_","H"="_","J"="_","K"="_","L"="_","M"="_","N"="_","P"="_","Q"="_","R"="_","S"="_","T"="_","V"="_","X"="_","Z"="_")
    
    #need custom internal functions to handle the _E's (e.g., in ACED given A_E left behind)
    remove_first <- function(word, char) {
      sub(char, "", word, fixed = TRUE)
    } #this one creates the correct lefttree leaves when there's an _E (e.g., ACED becomes CD)
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
    
    has_unfinished_leaves <- function(tree) {
      lv <- tree$leaves
      if (length(lv) == 0L) return(FALSE)
      for (ii in seq_along(lv)) {
        if (!identical(lv[[ii]]$name, "9")) return(TRUE)
      }
      FALSE
    }
    
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
    
    while (has_unfinished_leaves(lefttree)) { ##this says, while ANY of the leaves are other than 9...continue what follows
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
  
  #cache parse-string output within this corpus pass
  parse_string_raw <- parse_string
  .parse_string_cache <- new.env(hash = TRUE, parent = emptyenv())
  parse_string <- function(startword) {
    key <- toupper(startword)
    if (exists(key, envir = .parse_string_cache, inherits = FALSE)) {
      return(get(key, envir = .parse_string_cache, inherits = FALSE))
    }
    value <- parse_string_raw(startword)
    assign(key, value, envir = .parse_string_cache)
    value
  }
  
  #cache branch extraction per spelling/stem
  .branches_cache <- new.env(hash = TRUE, parent = emptyenv())
  get_branches_cached <- function(word) {
    key <- toupper(word)
    if (exists(key, envir = .branches_cache, inherits = FALSE)) {
      return(get(key, envir = .branches_cache, inherits = FALSE))
    }
    value <- extract_branches(parse_string(word))
    assign(key, value, envir = .branches_cache)
    value
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
  
  parsed_words <- list()
  
  ##now, process all the words:
  
  pb <- NULL
  
  for(j in 1:length(corpus$spelling)){
    
    #pick a word
    myword <- corpus$spelling[j]
    
    #parse and extract branches
    branches <- get_branches_cached(myword)
    
    ##limited morphology: specifically for final -ES and -ED:
    #if word ends in -ES, strip off the S, parse the string without that, add -S to each resulting branch, and then gather all unique branches from those two
    if(grepl("es$", myword)){
      myword2 <- substr(myword, 1, nchar(myword) - 1)
      branches2 <- get_branches_cached(myword2)
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
    
    #if word ends in -ED, strip off the D, parse the string without that, add -D to each resulting branch, and then gather all unique branches from those two
    if(grepl("ed$", myword)){
      myword2 <- substr(myword, 1, nchar(myword) - 1)
      branches2 <- get_branches_cached(myword2)
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
    
    # Run the function to get formatted results as a data frame
    allbranches_df <- extract_graphemes_for_letters(branches)
    
    #to turn output of map_PG() into the string that needs to match:
    mycorrect <- paste(toupper(mapped_corpus[[1]][[j]][2,]),collapse="-")
    
    #which branch is correct, then?
    correctbranch <- which(sub("^[^-]*-", "", branches) == mycorrect) #the sub() is being used to ignore the initial word on the branches, because those may not match if they've been modified as for nonlinear E
    
    #get the results for just that one--use "index1" for this
    correctbranch_df <- extract_graphemes_for_letters(branches[correctbranch])
    correctbranch_df$index1 <- paste0(correctbranch_df$Letter_Occurrence,correctbranch_df$Graphemes,"_",correctbranch_df$Position)
    
    #now add this info onto allbranches_df
    allbranches_df$index1 <- paste0(allbranches_df$Letter_Occurrence,allbranches_df$Graphemes,"_",allbranches_df$Position)
    allbranches_df$correct <- 0
    allbranches_df[allbranches_df$index1 %in% correctbranch_df$index1,]$correct <- 1
    
    #create "choices" variable, e.g., if O could be O, OU, or OUGH, and it's OU, this will tell you that OU was picked when the choices were among those 3 options
    #use index2 for the purpose of making these position-specific
    #note that sort() is now incorporated so that all choices will be listed alphabetically
    allbranches_df$index2 <- paste0(allbranches_df$Graphemes,"_",allbranches_df$Position)
    allbranches_df$choices <- allbranches_df$index2
    for(i in 1:length(allbranches_df$Letter_Occurrence)){
      allbranches_df$choices[i] <- paste0(sort(unique(allbranches_df[allbranches_df$Letter_Occurrence==allbranches_df[i,]$Letter_Occurrence,]$index2)),collapse=",")
    }
    
    parsed_words[[j]] <- allbranches_df
    
    if (!is.null(pb)) pb$tick()
  }
  
  #easiest to remove duplicates at this step--if the string is the same and the parse is the same, it doesn't matter if the pronunciation differs!
  #just add the spelling and frequency first
  for(i in 1:length(parsed_words)){
    parsed_words[[i]]$spelling <- corpus$spelling[i]
    parsed_words[[i]]$frequency <- corpus$freq[i]
  }
  #now remove dups (i.e., )
  parsed_words_nodups <- parsed_words[!duplicated(parsed_words)]
  
  #reformat into matrix
  mystats <- list()
  for(i in 1:length(parsed_words_nodups)){
    temp <- parsed_words_nodups[[i]][parsed_words_nodups[[i]]$correct==1, , drop = FALSE]
    # Branch identifiers are needed to construct PP2 contexts, but they must not
    # alter the legacy de-duplication that defines PP.
    temp <- temp[, setdiff(names(temp), c("branch_no", "branch_grapheme_no")), drop = FALSE]
    mystats[[i]] <- temp[!duplicated(temp), , drop = FALSE]
  }
  mystats <- do.call(rbind,mystats)
  
  
  ##to reduce choices to only ones that occur and add in Letter index:
  mystats$reduced <- mystats$choices
  for(i in 1:length(unique(mystats$choices))){
    mystats[mystats$choices==unique(mystats$choices)[i],]$reduced <- paste0(sort(unique(mystats[mystats$choices==unique(mystats$choices)[i],]$index2)),collapse=",")
  }
  mystats$Letter <- substring(mystats$Letter_Occurrence,1,1)

  # Construct the expanded, cross-letter PP2 context from the same reduced
  # alternatives used by PP. Letters are connected when a surviving candidate
  # grapheme can contain them together. The signature ignores unrelated letters
  # elsewhere in the word, allowing structurally comparable regions to pool.
  reduced_lookup <- unique(mystats[, c("choices", "reduced"), drop = FALSE])
  pp2_context_by_word <- vector("list", length(parsed_words_nodups))
  for (i in seq_along(parsed_words_nodups)) {
    this_word <- parsed_words_nodups[[i]]
    this_word$.pp2_order <- seq_len(nrow(this_word))
    this_word <- merge(this_word, reduced_lookup, by = "choices", all.x = TRUE)
    this_word <- this_word[order(this_word$.pp2_order), , drop = FALSE]
    this_word$.pp2_order <- NULL
    this_word$Letter <- substring(this_word$Letter_Occurrence, 1, 1)

    this_context <- .add_pp2_context(this_word, unique(this_word$spelling)[1])
    this_context$spelling <- unique(this_word$spelling)[1]
    pp2_context_by_word[[i]] <- this_context
  }
  pp2_context <- unique(do.call(rbind, pp2_context_by_word))
  mystats$.pp2_order <- seq_len(nrow(mystats))
  mystats <- merge(
    mystats,
    pp2_context,
    by = c("spelling", "Letter_Occurrence"),
    all.x = TRUE
  )
  mystats <- mystats[order(mystats$.pp2_order), , drop = FALSE]
  mystats$.pp2_order <- NULL
  
  #create index for our basic, brute morphology (just word-final ED and ES)
  mystats$suffix_class <- ifelse(grepl("ed$", mystats$spelling, ignore.case = TRUE), "EDERES",
                                 ifelse(grepl("er$", mystats$spelling, ignore.case = TRUE), "EDERES",
                                        ifelse(grepl("es$", mystats$spelling, ignore.case = TRUE), "EDERES",
                                               "OTHER")))
  
  
  
  ##To get the "parsing probability" (PP) stats; includes suffix_class flag
  mystats$index3 <- paste0(mystats$Letter,
                           "_in_",
                           mystats$index2,
                           "_and_",
                           mystats$reduced,
                           "_SC_",
                           mystats$suffix_class)
  
  mystats$index4 <- paste0(mystats$Letter,
                           "_in_",
                           mystats$reduced,
                           "_SC_",
                           mystats$suffix_class)

  mystats$index5 <- paste0(mystats$Letter,
                           "_role_",
                           mystats$pp2_role,
                           "_in_",
                           mystats$index2,
                           "_and_CTX_",
                           mystats$pp2_signature,
                           "_SC_",
                           mystats$suffix_class)

  mystats$index6 <- paste0(mystats$Letter,
                           "_role_",
                           mystats$pp2_role,
                           "_in_CTX_",
                           mystats$pp2_signature,
                           "_SC_",
                           mystats$suffix_class)
  
  pp_table <- aggregate(data=mystats,FUN=length,spelling~index3*index4) #this gets the numerators
  colnames(pp_table)[3] <- "numer"
  temp <- aggregate(data=mystats,FUN=length,spelling~index4) #this gets the denominators
  colnames(temp)[2] <- "denom"
  pp_table <- merge(pp_table,temp,by="index4")
  pp_table$pp <- pp_table$numer/pp_table$denom
  pp_table$pp_count <- pp_table$numer
  #and get frequency-weighted version:
  temp_freq1 <- aggregate(data=mystats,FUN=sum,log10(frequency+1)~index3*index4) #log10 freq weighted numerator (adds +1 to raw freq so its never 0)
  colnames(temp_freq1)[3] <- "numer"
  temp_freq2 <- aggregate(data=mystats,FUN=sum,log10(frequency+1)~index4) #log10 freq weighted denominator (adds +1 to raw freq so its never 0)
  colnames(temp_freq2)[2] <- "denom"
  temp_freq1 <- merge(temp_freq1,temp_freq2,by="index4")
  temp_freq1$pp_freq <- temp_freq1$numer/temp_freq1$denom
  pp_table <- merge(pp_table,temp_freq1[,c(2,5)],by="index3")
  pp_table$numer <- NULL
  pp_table$denom <- NULL
  pp_table <- pp_table[, c("index3", "index4", "pp", "pp_freq", "pp_count"), drop = FALSE]
  pp_table <- pp_table[order(pp_table$index4),] #sort for easier viewing

  # PP2 uses the expanded context signature but otherwise follows the same
  # numerator/denominator and frequency-weighting definitions as PP.
  pp2_table <- aggregate(data=mystats,FUN=length,spelling~index5*index6)
  colnames(pp2_table)[3] <- "numer"
  temp <- aggregate(data=mystats,FUN=length,spelling~index6)
  colnames(temp)[2] <- "denom"
  pp2_table <- merge(pp2_table,temp,by="index6")
  pp2_table$pp2 <- pp2_table$numer/pp2_table$denom
  pp2_table$pp2_count <- pp2_table$numer

  temp_freq1 <- aggregate(data=mystats,FUN=sum,log10(frequency+1)~index5*index6)
  colnames(temp_freq1)[3] <- "numer"
  temp_freq2 <- aggregate(data=mystats,FUN=sum,log10(frequency+1)~index6)
  colnames(temp_freq2)[2] <- "denom"
  temp_freq1 <- merge(temp_freq1,temp_freq2,by="index6")
  temp_freq1$pp2_freq <- temp_freq1$numer/temp_freq1$denom
  pp2_table <- merge(pp2_table,temp_freq1[,c("index5","pp2_freq")],by="index5")
  pp2_table <- pp2_table[, c("index5", "index6", "pp2", "pp2_freq", "pp2_count"), drop = FALSE]
  pp2_table <- pp2_table[order(pp2_table$index6), , drop = FALSE]

  corpus_parsed <- merge(mystats,pp_table[,c("index3","pp","pp_freq","pp_count")],by="index3") #merge PP's into the parsed corpus
  corpus_parsed <- merge(corpus_parsed,pp2_table[,c("index5","pp2","pp2_freq","pp2_count")],by="index5")
  # Preserve the exact legacy column order and append all new fields. This lets
  # existing code that reads the original columns continue to behave unchanged.
  legacy_corpus_parsed_columns <- c(
    "index3", "Letter_Occurrence", "Graphemes", "Position", "index1",
    "correct", "index2", "choices", "spelling", "frequency", "reduced",
    "Letter", "suffix_class", "index4", "pp", "pp_freq"
  )
  corpus_parsed <- corpus_parsed[, c(
    legacy_corpus_parsed_columns,
    setdiff(names(corpus_parsed), legacy_corpus_parsed_columns)
  ), drop = FALSE]
  corpus_pp <- aggregate(data=corpus_parsed,FUN=mean,pp~spelling)
  colnames(corpus_pp)[2] <- "pp_mean"
  corpus_pp$pp_min <- aggregate(data=corpus_parsed,FUN=min,pp~spelling)[,2]
  corpus_pp$pp_freq_mean <- aggregate(data=corpus_parsed,FUN=mean,pp_freq~spelling)[,2]
  corpus_pp$pp_freq_min <- aggregate(data=corpus_parsed,FUN=min,pp_freq~spelling)[,2]
  corpus_pp$pp_count_mean <- aggregate(data=corpus_parsed,FUN=mean,pp_count~spelling)[,2]
  corpus_pp$pp_count_min <- aggregate(data=corpus_parsed,FUN=min,pp_count~spelling)[,2]
  corpus_pp$pp2_mean <- aggregate(data=corpus_parsed,FUN=mean,pp2~spelling)[,2]
  corpus_pp$pp2_min <- aggregate(data=corpus_parsed,FUN=min,pp2~spelling)[,2]
  corpus_pp$pp2_freq_mean <- aggregate(data=corpus_parsed,FUN=mean,pp2_freq~spelling)[,2]
  corpus_pp$pp2_freq_min <- aggregate(data=corpus_parsed,FUN=min,pp2_freq~spelling)[,2]
  corpus_pp$pp2_count_mean <- aggregate(data=corpus_parsed,FUN=mean,pp2_count~spelling)[,2]
  corpus_pp$pp2_count_min <- aggregate(data=corpus_parsed,FUN=min,pp2_count~spelling)[,2]
  
  
  #also get the pp_reduced: a way to determine which choices ultimately never occur within the corpus. critical for pw_read()!
  pp_reduced <- aggregate(data=mystats,FUN=length,spelling~reduced*choices)[,c(1,2)]
  
  return(list(pp_table,pp_reduced,corpus_parsed,corpus_pp))
}

parse_corpus <- compiler::cmpfun(parse_corpus)

visualize_parse_tree <- function(spelling,
                                 pronunciation = NULL,
                                 show_plot = FALSE,
                                 include_suffix_variants = TRUE) {
  stopifnot(is.character(spelling), length(spelling) == 1L)
  if (!is.null(pronunciation)) {
    stopifnot(is.character(pronunciation), length(pronunciation) == 1L)
  }
  
  library(data.tree)
  library(stringr)
  
  required_globals <- c(
    "word_initial_mappings",
    "word_final_mappings",
    "syllable_medial_mappings",
    "syllable_final_mappings",
    "syllable_initial_mappings"
  )
  missing_globals <- required_globals[!vapply(required_globals, exists, logical(1), inherits = TRUE)]
  if (length(missing_globals) > 0) {
    stop("Missing required mapping objects in the global environment: ",
         paste(missing_globals, collapse = ", "))
  }
  
  body_expr <- body(parse_corpus)
  if (!is.call(body_expr) || !identical(body_expr[[1]], as.name("{"))) {
    stop("Could not inspect parse_corpus body.")
  }
  exprs <- as.list(body_expr)
  
  pick_assign <- function(name) {
    idx <- which(vapply(exprs, function(e) {
      is.call(e) &&
        identical(e[[1]], as.name("<-")) &&
        identical(e[[2]], as.name(name))
    }, logical(1)))
    if (length(idx) == 0L) {
      stop("Could not locate assignment for ", name, " inside parse_corpus.")
    }
    if (name == "parse_string" && length(idx) > 1L) {
      return(exprs[[idx[1]]]) # use the core parser, not the cache wrapper
    }
    if (length(idx) != 1L) {
      stop("Could not uniquely locate assignment for ", name, " inside parse_corpus.")
    }
    exprs[[idx]]
  }
  
  parse_env <- new.env(parent = parent.frame())
  parse_env$word_initial_mappings <- get("word_initial_mappings", inherits = TRUE)
  parse_env$word_final_mappings <- get("word_final_mappings", inherits = TRUE)
  parse_env$syllable_medial_mappings <- get("syllable_medial_mappings", inherits = TRUE)
  parse_env$syllable_final_mappings <- get("syllable_final_mappings", inherits = TRUE)
  parse_env$syllable_initial_mappings <- get("syllable_initial_mappings", inherits = TRUE)
  
  parse_env$internal_mappings <- rbind(parse_env$syllable_initial_mappings,
                                       parse_env$syllable_medial_mappings,
                                       parse_env$syllable_final_mappings)
  parse_env$parse_corpus_word_initial_mappings <- parse_env$word_initial_mappings
  parse_env$parse_corpus_word_final_mappings <- parse_env$word_final_mappings
  
  parse_env$parse_corpus_word_final_mappings[nrow(parse_env$parse_corpus_word_final_mappings) + 1, ] <-
    parse_env$parse_corpus_word_final_mappings[nrow(parse_env$parse_corpus_word_final_mappings), ]
  parse_env$parse_corpus_word_final_mappings[nrow(parse_env$parse_corpus_word_final_mappings), ]$grapheme <- "h"
  parse_env$parse_corpus_word_final_mappings[nrow(parse_env$parse_corpus_word_final_mappings), ]$phoneme <- "h"
  parse_env$parse_corpus_word_final_mappings[nrow(parse_env$parse_corpus_word_final_mappings) + 1, ] <-
    parse_env$parse_corpus_word_final_mappings[nrow(parse_env$parse_corpus_word_final_mappings), ]
  parse_env$parse_corpus_word_final_mappings[nrow(parse_env$parse_corpus_word_final_mappings), ]$grapheme <- "w"
  parse_env$parse_corpus_word_final_mappings[nrow(parse_env$parse_corpus_word_final_mappings), ]$phoneme <- "w"
  
  parse_env$parse_corpus_word_initial_mappings$grapheme <- toupper(parse_env$parse_corpus_word_initial_mappings$grapheme)
  parse_env$internal_mappings$grapheme <- toupper(parse_env$internal_mappings$grapheme)
  parse_env$parse_corpus_word_final_mappings$grapheme <- toupper(parse_env$parse_corpus_word_final_mappings$grapheme)
  
  eval(pick_assign("parse_string"), envir = parse_env)
  eval(pick_assign("extract_branches"), envir = parse_env)
  
  parse_string <- get("parse_string", envir = parse_env, inherits = FALSE)
  extract_branches <- get("extract_branches", envir = parse_env, inherits = FALSE)
  
  tree <- parse_string(spelling)
  branches <- extract_branches(tree)
  
  if (include_suffix_variants && grepl("es$", spelling, ignore.case = TRUE)) {
    spelling2 <- substr(spelling, 1, nchar(spelling) - 1)
    branches2 <- extract_branches(parse_string(spelling2))
    branches2 <- lapply(branches2, function(branch) {
      if (grepl("-", branch)) branch <- sub("-", "S-", branch)
      paste(branch, "-S", sep = "")
    })
    branches <- unique(c(branches, branches2))
  }
  
  if (include_suffix_variants && grepl("ed$", spelling, ignore.case = TRUE)) {
    spelling2 <- substr(spelling, 1, nchar(spelling) - 1)
    branches2 <- extract_branches(parse_string(spelling2))
    branches2 <- lapply(branches2, function(branch) {
      if (grepl("-", branch)) branch <- sub("-", "D-", branch)
      paste(branch, "-D", sep = "")
    })
    branches <- unique(c(branches, branches2))
  }
  
  branches <- unique(as.character(unlist(branches, use.names = FALSE)))
  
  correct_branches <- character(0)
  if (!is.null(pronunciation)) {
    mapped <- map_PG(spelling, pronunciation, map_progress = FALSE)
    target_parse <- paste(toupper(mapped[[1]][[1]][2, ]), collapse = "-")
    keep <- sub("^[^-]*-", "", branches) == target_parse
    correct_branches <- branches[keep]
  }
  
  if (show_plot) {
    if (requireNamespace("igraph", quietly = TRUE) && length(branches) > 0L) {
      branch_tokens <- strsplit(branches, "-", fixed = TRUE)
      node_labels <- list()
      edge_from <- character(0)
      edge_to <- character(0)
      
      for (tok in branch_tokens) {
        if (length(tok) < 1L) next
        ids <- character(length(tok))
        ids[1] <- tok[1]
        node_labels[[ids[1]]] <- tok[1]
        if (length(tok) > 1L) {
          for (ii in 2:length(tok)) {
            ids[ii] <- paste(ids[ii - 1], tok[ii], sep = "/")
            node_labels[[ids[ii]]] <- tok[ii]
            edge_from <- c(edge_from, ids[ii - 1])
            edge_to <- c(edge_to, ids[ii])
          }
        }
      }
      
      edges_df <- unique(data.frame(from = edge_from, to = edge_to, stringsAsFactors = FALSE))
      node_ids <- unique(c(edges_df$from, edges_df$to))
      vertices_df <- data.frame(
        name = node_ids,
        label = vapply(node_ids, function(id) node_labels[[id]], character(1)),
        stringsAsFactors = FALSE
      )
      
      highlight_nodes <- character(0)
      highlight_edges <- character(0)
      if (length(correct_branches) > 0L) {
        hit_tokens <- strsplit(correct_branches, "-", fixed = TRUE)
        for (tok in hit_tokens) {
          ids <- character(length(tok))
          ids[1] <- tok[1]
          highlight_nodes <- c(highlight_nodes, ids[1])
          if (length(tok) > 1L) {
            for (ii in 2:length(tok)) {
              ids[ii] <- paste(ids[ii - 1], tok[ii], sep = "/")
              highlight_nodes <- c(highlight_nodes, ids[ii])
              highlight_edges <- c(highlight_edges, paste(ids[ii - 1], ids[ii], sep = "->"))
            }
          }
        }
      }
      highlight_nodes <- unique(highlight_nodes)
      highlight_edges <- unique(highlight_edges)
      
      g <- igraph::graph_from_data_frame(edges_df, directed = TRUE, vertices = vertices_df)
      edge_df <- igraph::as_data_frame(g, what = "edges")
      edge_keys <- paste(edge_df$from, edge_df$to, sep = "->")
      edge_col <- ifelse(edge_keys %in% highlight_edges, "firebrick3", "gray70")
      edge_wd <- ifelse(edge_keys %in% highlight_edges, 3, 1)
      v_col <- ifelse(igraph::V(g)$name %in% highlight_nodes, "gold", "lightblue")
      v_frame <- ifelse(igraph::V(g)$name %in% highlight_nodes, "firebrick3", "gray40")
      
      root_id <- branch_tokens[[1]][1]
      root_idx <- which(igraph::V(g)$name == root_id)
      lo <- igraph::layout_as_tree(g, root = root_idx[1], circular = FALSE)
      
      old_par <- par(no.readonly = TRUE)
      on.exit(par(old_par), add = TRUE)
      par(mar = c(1, 1, 1, 1))
      plot(
        g,
        layout = lo,
        vertex.label = igraph::V(g)$label,
        vertex.color = v_col,
        vertex.frame.color = v_frame,
        vertex.size = 22,
        vertex.label.cex = 0.9,
        edge.color = edge_col,
        edge.width = edge_wd,
        edge.arrow.size = 0.25
      )
    } else {
      plot_obj <- plot(tree)
      if (!is.null(plot_obj)) {
        print(plot_obj)
      }
    }
  } else {
    print(tree, "level")
  }
  
  if (length(branches) > 0L) {
    cat("\nCandidate branches (", length(branches), "):\n", sep = "")
    cat(paste0("  ", branches), sep = "\n")
    cat("\n")
  }
  
  if (!is.null(pronunciation)) {
    cat("\nPronunciation target: ", pronunciation, "\n", sep = "")
    if (length(correct_branches) > 0L) {
      cat("Matching branch(es):\n")
      cat(paste0("  * ", correct_branches), sep = "\n")
      cat("\n")
    } else {
      cat("No branch matched the map_PG-derived parse for this pronunciation.\n")
    }
  }
  
  invisible(list(
    tree = tree,
    branches = branches,
    pronunciation = pronunciation,
    matching_branches = correct_branches
  ))
}

map_value <- function(spelling, pronunciation, level, tables, progress=FALSE, parsed_corpus=NULL) {
  
  if(progress==TRUE){
  pb = txtProgressBar(min = 0, max = length(spelling), initial = 0) }
  mylist <- list()
  pp_cache <- list()
  
  position_mapping <- c("1" = "wi", "2" = "si", "3" = "sm", "4" = "sf", "5" = "wf")
  expanded_parsing_columns <- c("pp_count", "pp2_role", "index5", "index6", "pp2", "pp2_freq", "pp2_count")
  has_expanded_parsing <- !is.null(parsed_corpus) &&
    length(parsed_corpus) >= 3L &&
    all(expanded_parsing_columns %in% names(parsed_corpus[[3]]))
  
  if (!is.null(parsed_corpus)) {
    stopifnot(is.list(parsed_corpus), length(parsed_corpus) >= 2)
    stopifnot(exists("pw_read"))
    
    get_pair_pp <- function(spell, target_graphemes) {
      target_graphemes <- unname(toupper(as.character(target_graphemes)))
      cache_key <- paste(c(toupper(spell), target_graphemes), collapse = "\r")
      
      if (is.null(pp_cache[[cache_key]])) {
        pp_detail <- tryCatch({
          pp_out <- pw_read(
            target = spell,
            parsed_corpus = parsed_corpus,
            PG_table = tables,
            level = "PG",
            score = FALSE,
            min_pp = 0,
            min_map = 0,
            max_options = 1,
            return_parse_pp_only = TRUE,
            target_graphemes = target_graphemes
          )
          
          if (nrow(pp_out) == 0L) {
            list(
              letter = character(0),
              pp = numeric(0),
              pp_count = numeric(0),
              pp2 = numeric(0),
              pp2_count = numeric(0)
            )
          } else {
            list(
              letter = as.character(pp_out$letter),
              pp = suppressWarnings(as.numeric(pp_out$pp)),
              pp_count = suppressWarnings(as.numeric(pp_out$pp_count)),
              pp2 = suppressWarnings(as.numeric(pp_out$pp2)),
              pp2_count = suppressWarnings(as.numeric(pp_out$pp2_count))
            )
          }
        }, error = function(e) {
          list(
            letter = character(0),
            pp = numeric(0),
            pp_count = numeric(0),
            pp2 = numeric(0),
            pp2_count = numeric(0)
          )
        })
        
        pp_cache[[cache_key]] <<- pp_detail
      }
      
      pp_cache[[cache_key]]
    }
    
  }
  
  for( i in 1:length(spelling)) {
    #sets to default to PG if level does not match ONC, OC, or OR
    ifelse(level == "ONC", value <- map_ONC(spelling[i], pronunciation[i], map_progress = FALSE),
           ifelse(level == "OC", value <- map_OC(spelling[i], pronunciation[i], map_progress = FALSE),
                  ifelse(level == "OR", value <- map_OR(spelling[i], pronunciation[i], map_progress = FALSE),
                         value <- map_PG(spelling[i], pronunciation[i], map_progress = FALSE))))
    
    
    
    df <- as.data.frame(value[[1]][[1]])
    
    while(all(is.na(df[,1]))) {
      df <- df[,-1]
    }
    
    rownames(df) <- c("phoneme", "grapheme", "position")
    
    df["position", ] <- position_mapping[unlist(df["position", ])]
    
    df[c("PG", "GP", "PG_freq", "P_freq", "G_freq"), ] <- NA
    
    tryCatch({
      for (col in colnames(df)) {
        phoneme_val <- df["phoneme", col]
        grapheme_val <- df["grapheme", col]
        position_val <- df["position", col]
        
        df["PG", col] <- subset(tables$pg, phoneme == phoneme_val & grapheme == grapheme_val)[,position_val]
        df["GP", col] <- subset(tables$gp, phoneme == phoneme_val & grapheme == grapheme_val)[,position_val]
        df["PG_freq", col] <- subset(tables$pg_freq, phoneme == phoneme_val & grapheme == grapheme_val)[,position_val]
        df["P_freq", col] <- subset(tables$p_freq, phoneme == phoneme_val)[,position_val]
        df["G_freq", col] <- subset(tables$g_freq, grapheme == grapheme_val)[,position_val]
      }}, error=function(e) {NA})
    
    df["spelling", 1] <- spelling[i]
    df["spelling", -1] <- ""
    
    df["pronunciation", 1] <- pronunciation[i]
    df["pronunciation", -1] <- ""
    
    df["PG_accuracy", 1] <- value[[2]] #encode if the word actually was not mappable, i.e., was "wrong"
    df["PG_accuracy", -1] <- ""
    
    if (!is.null(parsed_corpus)) {
      pg_value <- if (level == "PG") {
        value
      } else {
        map_PG(spelling[i], pronunciation[i], map_progress = FALSE)
      }
      pg_ok <- isTRUE(suppressWarnings(as.logical(pg_value[[2]])[1]))
      target_graphemes <- if (pg_ok) {
        as.character(pg_value[[1]][[1]][2, ])
      } else {
        character(0)
      }
      
      parsing_detail <- if (length(target_graphemes) > 0L) {
        pp_detail <- get_pair_pp(spelling[i], target_graphemes)
        pp_detail
      } else {
        list(
          pp = rep(NA_real_, nchar(spelling[i])),
          pp_count = rep(NA_real_, nchar(spelling[i])),
          pp2 = rep(NA_real_, nchar(spelling[i])),
          pp2_count = rep(NA_real_, nchar(spelling[i]))
        )
      }
      # An older parsed_corpus remains fully usable for legacy pp. Expanded
      # measures are exposed only when the supplied object actually contains
      # their supporting counts and contexts.
      parsing_parameters <- if (has_expanded_parsing) {
        c("pp", "pp_count", "pp2", "pp2_count")
      } else {
        "pp"
      }
      for (parsing_parameter in parsing_parameters) {
        parameter_values <- parsing_detail[[parsing_parameter]]
        if (length(parameter_values) == 0L) {
          parameter_values <- rep(NA_real_, nchar(spelling[i]))
        }

        parameter_text <- paste(
          formatC(parameter_values, format = "f", digits = 3),
          collapse = ", "
        )
        df[parsing_parameter, 1] <- parameter_text
        df[parsing_parameter, -1] <- ""
        attr(df, paste0(parsing_parameter, "_values")) <- suppressWarnings(as.numeric(parameter_values))
      }
    }
    
    mylist[[i]] <- df
    names(mylist[[i]]) <- spelling[i]
    
    if(progress==TRUE){
    setTxtProgressBar(pb,i)}
  }
  
  return(mylist)
}

word_pattern <- function(mapped_words, phoneme, grapheme, position) {
  
  #all for one thing to be "any" -- set up scenarios for what to do based on indices, e.g., any phoneme == scenario 1 . scenario 4 is for fully specified requests
  scenario <- ifelse(phoneme == "any",1,
                     ifelse(grapheme == "any",2,
                            ifelse(position == "any",3,
                                   4)))
  
  matching_words <- c()
  
  for (i in seq_along(mapped_words)) {
    df <- mapped_words[[i]]
    if (!("spelling" %in% rownames(df))) {
      next 
    }
    
    word_values <- as.character(df["spelling", ])
    
    word <- word_values[which.max(nchar(word_values))]
    
    if (nchar(word) == 0) next  
    
    #FULLY SPECIFIED:
    if (scenario == 4)
      for (j in 1:ncol(df)) {
        if (df["phoneme", j] == phoneme &&
            df["grapheme", j] == grapheme &&
            df["position", j] == position) {
          
          matching_words <- c(matching_words, word)
          break
        }
      }
    
    #ANY POSITION:
    if (scenario == 3)
      for (j in 1:ncol(df)) {
        if (df["phoneme", j] == phoneme &&
            df["grapheme", j] == grapheme
        ) {
          
          matching_words <- c(matching_words, word)
          break
        }
      }
    
    #ANY GRAPHEME:
    if (scenario == 2)
      for (j in 1:ncol(df)) {
        if (df["phoneme", j] == phoneme &&
            df["position", j] == position) {
          
          matching_words <- c(matching_words, word)
          break
        }
      }
    
    #ANY PHONEME:
    if (scenario == 1)
      for (j in 1:ncol(df)) {
        if (df["grapheme", j] == grapheme &&
            df["position", j] == position) {
          
          matching_words <- c(matching_words, word)
          break
        }
      }
    
  }
  
  return(matrix(matching_words))
}
summarize_words <- function(mapped_words, parameter, mode = c("default", "ONC")) {
  mode <- match.arg(mode)
  parsing_parameters <- c("pp", "pp_count", "pp2", "pp2_count")
  parameter_normalized <- tolower(as.character(parameter)[1])
  if (mode == "ONC" && parameter_normalized %in% parsing_parameters) {
    message("Parsing parameters cannot be used with mode = 'ONC'; use mode = 'default'.")
    return(invisible(NULL))
  }
  
  vowels <- c(
    "5", "O", "8", "je", "j3r", "ju", "jU", "o", "2",
    "@", "a", "e", "E", "3", "3r", "i", "1", "c", "u", "U", "^"
  )
  
  pg_accuracy_true <- function(mw) {
    val <- mw["PG_accuracy", ][1]
    if (is.logical(val)) return(isTRUE(val))
    identical(toupper(as.character(val)), "TRUE")
  }
  
  parameter_values <- function(mw, parameter) {
    values <- mw[parameter, ]
    parameter_name <- tolower(as.character(parameter)[1])
    if (parameter_name %in% parsing_parameters) {
      parsing_values <- attr(mw, paste0(parameter_name, "_values"), exact = TRUE)
      if (!is.null(parsing_values)) return(suppressWarnings(as.numeric(parsing_values)))
      parsing_text <- as.character(values[1])
      if (is.na(parsing_text) || !nzchar(parsing_text)) return(numeric(0))
      return(suppressWarnings(as.numeric(trimws(strsplit(parsing_text, ",", fixed = TRUE)[[1]]))))
    }
    phonemes <- as.character(mw["phoneme", ])
    values[!is.na(phonemes) & nzchar(phonemes)]
  }
  
  classify_onc_cols <- function(mw) {
    ph  <- as.character(mw["phoneme", ])
    pos <- as.character(mw["position", ])
    mapped_cols <- which(!is.na(ph) & nzchar(ph))
    if (length(mapped_cols) > 0L) {
      ph <- ph[seq_len(max(mapped_cols))]
      pos <- pos[seq_len(max(mapped_cols))]
    }
    
    if (length(ph) > 0 && ph[1] == mw["spelling", 1]) {
      ph  <- ph[-1]
      pos <- pos[-1]
      offset <- 1L
    } else {
      offset <- 0L
    }
    
    pos_norm <- tolower(pos)
    pos_norm[pos_norm == "1"] <- "wi"
    pos_norm[pos_norm == "2"] <- "si"
    pos_norm[pos_norm == "3"] <- "sm"
    pos_norm[pos_norm == "4"] <- "sf"
    pos_norm[pos_norm == "5"] <- "wf"
    
    is_vowel <- ph %in% vowels
    
    if (any(is_vowel)) {
      n <- length(ph)
      starts <- sort(unique(c(1L, which(pos_norm == "si"))))
      starts <- starts[starts >= 1L & starts <= n]
      ends <- c(starts[-1] - 1L, n)
      
      idx_O <- integer(0)
      idx_N <- integer(0)
      idx_C <- integer(0)
      
      for (k in seq_along(starts)) {
        seg_idx <- seq.int(starts[k], ends[k])
        seg_v_idx <- seg_idx[is_vowel[seg_idx]]
        
        if (length(seg_v_idx) == 0) {
          seg_pos <- pos_norm[seg_idx]
          idx_O <- c(idx_O, seg_idx[!is_vowel[seg_idx] & seg_pos %in% c("wi", "si", "sm")])
          idx_C <- c(idx_C, seg_idx[!is_vowel[seg_idx] & seg_pos %in% c("sf", "wf")])
        } else {
          first_v <- min(seg_v_idx)
          last_v  <- max(seg_v_idx)
          cons_idx <- seg_idx[!is_vowel[seg_idx]]
          
          idx_N <- c(idx_N, seg_v_idx)
          idx_O <- c(idx_O, cons_idx[cons_idx < first_v])
          idx_C <- c(idx_C, cons_idx[cons_idx > last_v])
        }
      }
      
      return(list(
        O = sort(unique(idx_O + offset)),
        N = sort(unique(idx_N + offset)),
        C = sort(unique(idx_C + offset))
      ))
    }
    
    is_onset_pos <- pos_norm %in% c("wi", "si")
    is_coda_pos  <- pos_norm %in% c("sm", "sf", "wf")
    
    list(
      O = which(!is_vowel & is_onset_pos) + offset,
      N = integer(0),
      C = which(!is_vowel & is_coda_pos) + offset
    )
  }
  
  stats_vec_observed <- function(x) {
    x_raw <- suppressWarnings(as.numeric(x))
    if (length(x_raw) == 0) {
      return(rep(NA_real_, 5))
    }
    x_raw[!is.finite(x_raw)] <- 0
    c(
      mean(x_raw),
      stats::median(x_raw),
      max(x_raw),
      min(x_raw),
      stats::sd(x_raw)
    )
  }
  
  uncertain_default <- c(NA_real_, NA_real_, NA_real_, 0, NA_real_)
  uncertain_onc <- rep(NA_real_, 15)
  
  if (mode == "default") {
    mystats <- data.frame(
      spelling = rep(NA_character_, length(mapped_words)),
      pronunciation = rep(NA_character_, length(mapped_words)),
      mean = rep(NA_real_, length(mapped_words)),
      median = rep(NA_real_, length(mapped_words)),
      max = rep(NA_real_, length(mapped_words)),
      min = rep(NA_real_, length(mapped_words)),
      sd = rep(NA_real_, length(mapped_words)),
      stringsAsFactors = FALSE
    )
    
    for (i in seq_along(mapped_words)) {
      mw <- mapped_words[[i]]
      mystats[i, 1] <- mw["spelling", 1]
      mystats[i, 2] <- mw["pronunciation", 1]
      
      if (pg_accuracy_true(mw)) {
        mystats[i, 3:7] <- stats_vec_observed(parameter_values(mw, parameter))
      } else {
        mystats[i, 3:7] <- uncertain_default
      }
    }
    
    return(mystats)
  }
  
  onc_names <- c(
    "mean_onset", "median_onset", "max_onset", "min_onset", "sd_onset",
    "mean_nucleus", "median_nucleus", "max_nucleus", "min_nucleus", "sd_nucleus",
    "mean_coda", "median_coda", "max_coda", "min_coda", "sd_coda"
  )
  mystats <- data.frame(
    spelling = rep(NA_character_, length(mapped_words)),
    pronunciation = rep(NA_character_, length(mapped_words)),
    matrix(NA_real_, nrow = length(mapped_words), ncol = length(onc_names)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  names(mystats)[-(1:2)] <- onc_names
  
  for (i in seq_along(mapped_words)) {
    mw <- mapped_words[[i]]
    mystats[i, 1] <- mw["spelling", 1]
    mystats[i, 2] <- mw["pronunciation", 1]
    
    if (pg_accuracy_true(mw)) {
      idx <- classify_onc_cols(mw)
      data <- mw[parameter, ]
      mystats[i, 3:7]   <- stats_vec_observed(data[idx$O])
      mystats[i, 8:12]  <- stats_vec_observed(data[idx$N])
      mystats[i, 13:17] <- stats_vec_observed(data[idx$C])
    } else {
      mystats[i, 3:17] <- uncertain_onc
    }
  }
  
  mystats
}


#spell pseudowords
pw_spell <- function(target, level = "PG", PG_table = NULL, OC_table = NULL, OR_table = NULL, score = TRUE, min_map = 1, max_options = 1000, param = "pg",  mean_score = 0, parsed_corpus = NULL, summary_mode = c("default", "ONC")) {
  library(stringr)
  
  #phonemes to check that require remapping as biphones (e.g., "ju", "ks")
  phonemes_to_check <- paste(c("ju","je","jU","j3","ks","kS","nj","gz","gZ"), collapse = "|")
  
  level <- toupper(level)
  if (!level %in% c("PG", "OC", "OR", "ALL")) {
    stop("Unknown level: ", level, ". Use 'PG', 'OC', 'OR', or 'all'.")
  }
  
  if (level == "ALL") {
    if (is.null(PG_table) || is.null(OC_table) || is.null(OR_table)) {
      stop("When level = 'all', PG_table, OC_table, and OR_table must all be provided.")
    }
    return(list(
      PG = pw_spell(target = target, level = "PG", PG_table = PG_table, OC_table = OC_table, OR_table = OR_table, min_map = min_map, max_options = max_options, param = param, score = score, mean_score = mean_score, parsed_corpus = parsed_corpus, summary_mode = summary_mode),
      OC = pw_spell(target = target, level = "OC", PG_table = PG_table, OC_table = OC_table, OR_table = OR_table, min_map = min_map, max_options = max_options, param = param, score = score, mean_score = mean_score, parsed_corpus = parsed_corpus, summary_mode = summary_mode),
      OR = pw_spell(target = target, level = "OR", PG_table = PG_table, OC_table = OC_table, OR_table = OR_table, min_map = min_map, max_options = max_options, param = param, score = score, mean_score = mean_score, parsed_corpus = parsed_corpus, summary_mode = summary_mode)
    ))
  }
  
  if (level == "PG") {
    tables <- PG_table
  } else if (level == "OC") {
    tables <- OC_table
  } else {
    tables <- OR_table
  }
  
  if (is.null(tables)) {
    stop(level, "_table must be provided when level = '", level, "'.")
  }
  
  selected_param <- toupper(param)
  if (!selected_param %in% c("PG", "GP", "PG_FREQ", "P_FREQ", "G_FREQ", "PP", "PP_COUNT", "PP2", "PP2_COUNT")) {
    stop("param must be one of: PG, GP, PG_freq, P_freq, G_freq, pp, pp_count, pp2, or pp2_count.")
  }
  
  summary_mode <- match.arg(summary_mode)
  if (summary_mode == "ONC" && mean_score != 0) {
    stop("mean_score thresholding is only available when summary_mode = 'default'.")
  }
  
  score_row <- switch(
    selected_param,
    "PG" = "PG",
    "GP" = "GP",
    "PG_FREQ" = "PG_freq",
    "P_FREQ" = "P_freq",
    "G_FREQ" = "G_freq",
    "PP" = "pp",
    "PP_COUNT" = "pp_count",
    "PP2" = "pp2",
    "PP2_COUNT" = "pp2_count"
  )
  
  if (!is.numeric(min_map) || length(min_map) != 1 || is.na(min_map)) {
    stop("min_map must be a single numeric value.")
  }
  if (!is.numeric(max_options) || length(max_options) != 1 || is.na(max_options) || max_options < 1) {
    stop("max_options must be a single numeric value >= 1.")
  }
  max_options <- as.integer(max_options)
  
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
  
  #INTERNAL function: Maximum Onset Principle phonological parse function--different from the one in map_PG/map_OR, etc. because it has the remapped biphone representations (e.g., "ju" = œ)
  parse_syllables_PWSPELL <- function(target,word_index) {
    
    vowels <- c("5", "O", "8", "je", "j3r", "ju", "jU", "o", "2", 
                "@", "a", "e", "E", "3r", "i", "1", "c", "u", "U", "^",
                "œ","∑","®","†") #this line has the special cases for PW SPELL
    
    consonants <- c("G", "gz", "kS", "nj", "C", "T", "D", "Z", "N",
                    "b", "d", "f", "g", "h", "j", "k", "l", "m", "n", "p", 
                    "r", "s", "S", "t", "v", "w", "z",
                    "¥","ø","π","å","ß") #this line has the special cases for PW SPELL
    
    
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
  
  #INTERNAL FUNCTION for creating different grain sizes:
  insertSpacesMatching <- function(originalString, matchString) {
    originalString <- gsub("\\s", "", originalString)  # Remove any existing spaces from the original string
    spaces <- gregexpr(" ", matchString)[[1]] - 1  # Find the positions of spaces in the match string and adjust for indexing
    
    for (pos in spaces) {
      originalString <- paste0(substr(originalString, 1, pos), " ", substr(originalString, pos + 1, nchar(originalString)))
    }
    
    return(originalString)
  }
  
  #INTERNAL function: if level = "OR", then prep_pword's output needs to be worked into onset/rime level (on its own it just does the phonographemic level)
  #note that the code will always run prep_pword and parse_syllables first, which return mylist2, and so mylist2 is what prep_pword_OR works with
  #just note that in the original function (in map_OR), what was hold[[1]][1,] was phonemes, so now thats mylist1 row2, and hold[[1]][3,] is now row1
  
  #list vowels per in-house code
  vowels <- c("5", "O", "8", "je", "j3", "ju", "jU", "o", "2", 
              "@", "a", "e", "E", "3", "i", "1", "c", "u", "U", "^")
  
  prep_pword_ONC <- function(mylist2) {
    
    hold <- mylist2
    
    for(i in 1:length(target)){
      
      hold[[i]] <- rbind(hold[[i]],hold[[i]][2,],hold[[i]][2,])
      
      tryCatch(
        {if(which(is.na(hold[[i]][2,])) > 0)
          hold[[i]] <- hold[[i]][,-which(is.na(hold[[i]][2,]))] #remove excess columns (comes from remapping of diphthongs, X, etc.)
        }, error=function(e) NA)
      
      hold[[i]][4,][hold[[i]][2,] %in% vowels] <- "V"
      hold[[i]][4,][!hold[[i]][2,] %in% vowels] <- "C"
      
      #label columns with alphabetic indices for later cbinding
      colnames(hold[[i]]) <- c(letters,LETTERS)[1:ncol(hold[[i]])]
      
      #get complete CV string
      CVstring <- paste0(hold[[i]][4,],collapse="")
      
      #get complete position string
      posstring <- paste0(hold[[i]][1,],collapse="")
      
      #get column indices for eventual pasting
      colstring <- paste0(c(letters,LETTERS)[1:nchar(CVstring)],collapse="")
      
      #find the positions of 'V' characters
      split_positions1 <- gregexpr("V", CVstring)[[1]]
      
      #insert spaces before AND AFTER 'V' characters -- whatever you do HERE is what determines ONC, OC, or OR
      CVstring <- gsub("V", " V ", CVstring)
      
      #make the posstring and colstring match the regrouped CVstring
      posstring <- insertSpacesMatching(posstring,CVstring)
      colstring <- insertSpacesMatching(colstring,CVstring)
      
      #find the positions of '2' characters
      split_positions2 <- gregexpr("2", posstring)[[1]]
      
      #insert spaces before '2' characters
      posstring <- gsub("2", " 2", posstring)
      
      #make the CVstring and colstring match the regroups posstring
      CVstring <- insertSpacesMatching(CVstring,posstring)
      colstring <- insertSpacesMatching(colstring,posstring)
      
      #vowel-initial words will have whitespace at the start, strip that off:
      CVstring <- trimws(CVstring,"left")
      colstring <- trimws(colstring,"left")
      posstring <- trimws(posstring,"left")
      
      #convert colstring to matrix of column indices to paste together:
      colstring <- as.matrix(read.table(text=colstring))
      
      #make a matrix to hold the remapping
      result <- matrix(NA,ncol=length(colstring),nrow=4)
      
      #now loop through each to-be-combined element of colstring to fill in result matrix
      for (j in 1:length(colstring)) {
        cols_to_paste <- unlist(strsplit(colstring[j], ""))
        result[, j] <- apply(hold[[i]][, cols_to_paste, drop = FALSE], 1, paste, collapse = "")
      }
      result <- as.data.frame(result)
      
      #format the new OR matrix including removing medial 3's from the position info, keeping only the first remaining character (e.g., a 14 rhyme like in AB-STAIN should be just 1) and dropping the CV markers
      hold[[i]] <- result
      
      
      #run through each position code in row 3--since nucleii are always only one phoneme (in English anyway...), it is the length>1 mappings that need adjusting:
      #if the minimum is <3 keep just that (e.g., 13 becomes 1), if the maximum>3 keep just that (e.g., 35 becomes 5)
      for (j in 1:ncol(hold[[i]])){
        if(nchar(hold[[i]][4,j]) > 1) #only need a change if there is a cluster
          if(as.numeric(substr(hold[[i]][1,j],1,1)) < 3) { #if it starts 1 or 2
            hold[[i]][1,j] <- substr(hold[[i]][1,j],1,1)} else { #keep the first thing (the 1 or 2)
              hold[[i]][1,j] <- substr(hold[[i]][1,j],nchar(hold[[i]][1,j]),nchar(hold[[i]][1,j])) #otherwise keep the last thing (it'll be 4 or 5)
            }}
      
      hold[[i]] <- data.frame(hold[[i]][-4,]) #remove VC structural
    }
    return(hold)
  }
  prep_pword_OC <- function(mylist2) {
    
    hold <- mylist2
    
    for(i in 1:length(target)){
      
      hold[[i]] <- rbind(hold[[i]],hold[[i]][2,],hold[[i]][2,])
      
      tryCatch(
        {if(which(is.na(hold[[i]][2,])) > 0)
          hold[[i]] <- hold[[i]][,-which(is.na(hold[[i]][2,]))] #remove excess columns (comes from remapping of diphthongs, X, etc.)
        }, error=function(e) NA)
      
      hold[[i]][4,][hold[[i]][2,] %in% vowels] <- "V"
      hold[[i]][4,][!hold[[i]][2,] %in% vowels] <- "C"
      
      #label columns with alphabetic indices for later cbinding
      colnames(hold[[i]]) <- c(letters,LETTERS)[1:ncol(hold[[i]])]
      
      #get complete CV string
      CVstring <- paste0(hold[[i]][4,],collapse="")
      
      #get complete position string
      posstring <- paste0(hold[[i]][1,],collapse="")
      
      #get column indices for eventual pasting
      colstring <- paste0(c(letters,LETTERS)[1:nchar(CVstring)],collapse="")
      
      #find the positions of 'V' characters
      split_positions1 <- gregexpr("V", CVstring)[[1]]
      
      #insert spaces AFTER 'V' characters -- whatever you do HERE is what determines ONC, OC, or OR
      CVstring <- gsub("V", "V ", CVstring)
      
      #make the posstring and colstring match the regrouped CVstring
      posstring <- insertSpacesMatching(posstring,CVstring)
      colstring <- insertSpacesMatching(colstring,CVstring)
      
      #find the positions of '2' characters
      split_positions2 <- gregexpr("2", posstring)[[1]]
      
      #insert spaces before '2' characters
      posstring <- gsub("2", " 2", posstring)
      
      #make the CVstring and colstring match the regroups posstring
      CVstring <- insertSpacesMatching(CVstring,posstring)
      colstring <- insertSpacesMatching(colstring,posstring)
      
      #vowel-final words will have whitespace at the end, strip that off:
      CVstring <- trimws(CVstring,"right")
      colstring <- trimws(colstring,"right")
      posstring <- trimws(posstring,"right")
      
      #and some words will have duplicated internal whitespaces, strip those off:
      CVstring <- gsub("\\s+", " ", CVstring)
      colstring <- gsub("\\s+", " ", colstring)
      posstring <- gsub("\\s+", " ", posstring)
      
      #convert colstring to matrix of column indices to paste together:
      colstring <- as.matrix(read.table(text=colstring))
      
      #make a matrix to hold the remapping
      result <- matrix(NA,ncol=length(colstring),nrow=4)
      
      #now loop through each to-be-combined element of colstring to fill in result matrix
      for (j in 1:length(colstring)) {
        cols_to_paste <- unlist(strsplit(colstring[j], ""))
        result[, j] <- apply(hold[[i]][, cols_to_paste, drop = FALSE], 1, paste, collapse = "")
      }
      result <- as.data.frame(result)
      
      #format the new OR matrix including removing medial 3's from the position info, keeping only the first remaining character (e.g., a 14 rhyme like in AB-STAIN should be just 1) and dropping the CV markers
      hold[[i]] <- result
      
      
      #run through each position code in row 3--rimes may look like 3(3)4 or 3(3)5 and should become just 3 or 5...onsets can be like 13(3) or 23(3) and should become just 1 or 2
      #if the minimum is <3 keep just that (e.g., 13 becomes 1), if the maximum>3 keep just that (e.g., 35 becomes 5)
      #REWRITE: if the min = 1, make it 1...if the max = 5, make it five. if neither of those conditions are true, than min<3 = min, if max>3 =max.
      for (j in 1:ncol(hold[[i]])){
        if(nchar(hold[[i]][4,j]) > 1) #only need a change if there is a cluster
          if(as.numeric(substr(hold[[i]][1,j],1,1)) == 1){ #new line
            hold[[i]][1,j] <- 1 #new line
          } else { #new line
            if(as.numeric(substr(hold[[i]][1,j],nchar(hold[[i]][1,j]),nchar(hold[[i]][1,j]))) == 5){ #new line
              hold[[i]][1,j] <- 5 #new line
            } else { #new line, but the following are from before:
              if(as.numeric(substr(hold[[i]][1,j],1,1)) < 3) { #if it starts 1 or 2
                hold[[i]][1,j] <- substr(hold[[i]][1,j],1,1)} else { #keep the first thing (the 1 or 2) #NEW: Except now that would only be from a 2, since a 1 was already made one
                  hold[[i]][1,j] <- substr(hold[[i]][1,j],nchar(hold[[i]][1,j]),nchar(hold[[i]][1,j])) #otherwise keep the last thing (it'll be 4 or 5) #NEW: Except now that will only be for a 4, since a 5 was already made five
                }}}} #new: added two more }}
      
      hold[[i]] <- data.frame(hold[[i]][-4,]) #remove VC structural
    }
    return(hold)
  }
  prep_pword_OR <- function(mylist2) {
    
    hold <- mylist2
    
    for(i in 1:length(target)){
      
      hold[[i]] <- rbind(hold[[i]],hold[[i]][2,],hold[[i]][2,])
      
      tryCatch(
        {if(which(is.na(hold[[i]][2,])) > 0)
          hold[[i]] <- hold[[i]][,-which(is.na(hold[[i]][2,]))] #remove excess columns (comes from remapping of diphthongs, X, etc.)
        }, error=function(e) NA)
      
      hold[[i]][4,][hold[[i]][2,] %in% vowels] <- "V"
      hold[[i]][4,][!hold[[i]][2,] %in% vowels] <- "C"
      
      #label columns with alphabetic indices for later cbinding
      colnames(hold[[i]]) <- c(letters,LETTERS)[1:ncol(hold[[i]])]
      
      #get complete CV string
      CVstring <- paste0(hold[[i]][4,],collapse="")
      
      #get complete position string
      posstring <- paste0(hold[[i]][1,],collapse="")
      
      #get column indices for eventual pasting
      colstring <- paste0(c(letters,LETTERS)[1:nchar(CVstring)],collapse="")
      
      #find the positions of 'V' characters
      split_positions1 <- gregexpr("V", CVstring)[[1]]
      
      #insert spaces BEFORE 'V' characters -- whatever you do HERE is what determines ONC, OC, or OR
      CVstring <- gsub("V", " V", CVstring)
      
      #make the posstring and colstring match the regrouped CVstring
      posstring <- insertSpacesMatching(posstring,CVstring)
      colstring <- insertSpacesMatching(colstring,CVstring)
      
      #find the positions of '2' characters
      split_positions2 <- gregexpr("2", posstring)[[1]]
      
      #insert spaces before '2' characters
      posstring <- gsub("2", " 2", posstring)
      
      #make the CVstring and colstring match the regroups posstring
      CVstring <- insertSpacesMatching(CVstring,posstring)
      colstring <- insertSpacesMatching(colstring,posstring)
      
      #vowel-final words will have whitespace at the beginning, strip that off:
      CVstring <- trimws(CVstring,"left")
      colstring <- trimws(colstring,"left")
      posstring <- trimws(posstring,"left")
      
      #and some words will have duplicated internal whitespaces, strip those off:
      CVstring <- gsub("\\s+", " ", CVstring)
      colstring <- gsub("\\s+", " ", colstring)
      posstring <- gsub("\\s+", " ", posstring)
      
      #convert colstring to matrix of column indices to paste together:
      colstring <- as.matrix(read.table(text=colstring))
      
      #make a matrix to hold the remapping
      result <- matrix(NA,ncol=length(colstring),nrow=4)
      
      #now loop through each to-be-combined element of colstring to fill in result matrix
      for (j in 1:length(colstring)) {
        cols_to_paste <- unlist(strsplit(colstring[j], ""))
        result[, j] <- apply(hold[[i]][, cols_to_paste, drop = FALSE], 1, paste, collapse = "")
      }
      result <- as.data.frame(result)
      
      #format the new OR matrix including removing medial 3's from the position info, keeping only the first remaining character (e.g., a 14 rhyme like in AB-STAIN should be just 1) and dropping the CV markers
      hold[[i]] <- result
      
      
      #run through each position code in row 3--rimes may look like 3(3)4 or 3(3)5 and should become just 3 or 5...onsets can be like 13(3) or 23(3) and should become just 1 or 2
      #if the minimum is <3 keep just that (e.g., 13 becomes 1), if the maximum>3 keep just that (e.g., 35 becomes 5)
      #REWRITE: FOR RIMES, if the min = 1, make it 1 UNLESS if the max = 5, make it five (always make it 5 if there's a 5). if neither of those conditions are true, than min<3 = min, if max>3 =max.
      for (j in 1:ncol(hold[[i]])){
        if(nchar(hold[[i]][4,j]) > 1) #only need a change if there is a cluster
          if(as.numeric(substr(hold[[i]][1,j],nchar(hold[[i]][1,j]),nchar(hold[[i]][1,j]))) == 5){ #new line
            hold[[i]][1,j] <- 5 #new line
          } else { #new line
            if(as.numeric(substr(hold[[i]][1,j],1,1)) == 1){ #new line
              hold[[i]][1,j] <- 1 #new line
            } else { #new line, but the following are from before:
              if(as.numeric(substr(hold[[i]][1,j],1,1)) < 3) { #if it starts 1 or 2
                hold[[i]][1,j] <- substr(hold[[i]][1,j],1,1)} else { #keep the first thing (the 1 or 2) #NEW: Except now that would only be from a 2, since a 1 was already made one
                  hold[[i]][1,j] <- substr(hold[[i]][1,j],nchar(hold[[i]][1,j]),nchar(hold[[i]][1,j])) #otherwise keep the last thing (it'll be 4 or 5) #NEW: Except now that will only be for a 4, since a 5 was already made five
                }}}} #new: added two more }}
      
      hold[[i]] <- data.frame(hold[[i]][-4,]) #remove VC structural
    }
    return(hold)
  }
  
  #INTERNAL function: Recursive function to generate combinations (when looking for more than just the maximum probability spelling)
  combine_vectors <- function(lists, current = character()) {
    # Base case: if all matrices are processed, return the current combination
    if (length(lists) == 0) {
      return(paste0(current, collapse = ""))
    }
    
    first <- lists[[1]]
    first_vals <- if (is.matrix(first) || is.data.frame(first)) {
      as.character(first[, 1])
    } else {
      as.character(first)
    }
    first_vals <- first_vals[!is.na(first_vals)]
    
    if (length(first_vals) == 0) {
      return(character(0))
    }
    
    # Recursive case: for each option in the first slot, append and recurse
    result <- character(0)
    for (val in first_vals) {
      result <- c(result, combine_vectors(lists[-1], c(current, val)))
    }
    return(result)
  }
  
  #correspondences for biphones
  correspondences <- c("ju"	= "œ","je" = "∑", "jU" = "®", "j3" = "†", "ks" = "¥", "kS" = "ø", "nj" = "π", "gz" = "å", "gZ" = "ß")
  correspondences_reverse <- c("œ" = "ju", "∑" = "je",  "®" = "jU","†" =  "j3", "¥" = "ks", "ø" = "kS", "π" = "nj", "å" = "gz", "ß"= "gZ")
  
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
  
  #begin real function here
  
  mylist1 <- list()
  mylist2 <- list()
  mymat <- list()
  
  position_mapping <- c("1" = "wi", "2" = "si", "3" = "sm", "4" = "sf", "5" = "wf")
  
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
  
  #now run prep_pword_OR if level == "OR", and its output should replace mylist2--or any other grain size!
  ifelse(level=="OR", mylist2 <- prep_pword_OR(mylist2),
         ifelse(level=="OC", mylist2 <- prep_pword_OC(mylist2), mylist2 <- mylist2))
  
  generate_spellings_from_matrix <- function(word_mat, threshold, use_max = FALSE) {
    mytemp <- list()
    suppressWarnings({
      if (use_max) {
        for (j in 1:ncol(word_mat)) {
          this_rows <- tables[["pg"]][tables[["pg"]]$phoneme == word_mat[2, j], ]
          this_vals <- as.numeric(as.matrix(this_rows[, as.numeric(word_mat[1, j]) + 2, drop = FALSE]))
          this_graph <- as.character(this_rows[[2]])
          if (length(this_vals) == 0 || all(!is.finite(this_vals))) {
            mytemp[[j]] <- character(0)
          } else {
            max_val <- max(this_vals[is.finite(this_vals)])
            mytemp[[j]] <- this_graph[which(this_vals == max_val)]
          }
        }
      } else {
        for (j in 1:ncol(word_mat)) {
          this_rows <- tables[["pg"]][tables[["pg"]]$phoneme == word_mat[2, j], ]
          this_vals <- as.numeric(as.matrix(this_rows[, as.numeric(word_mat[1, j]) + 2, drop = FALSE]))
          this_graph <- as.character(this_rows[[2]])
          if (length(this_vals) == 0) {
            mytemp[[j]] <- character(0)
          } else {
            mytemp[[j]] <- this_graph[which(is.finite(this_vals) & this_vals > threshold)]
          }
        }
      }
    })
    out <- unique(combine_vectors(mytemp))
    out[!grepl("NA|character\\(0\\)", out)]
  }
  
  get_word_matrix <- function(i, use_biphone = FALSE) {
    if (!use_biphone) {
      mat <- mylist2[[i]]
      if (level == "PG") mat <- rbind(mat, mat[2, ])
      return(mat)
    }
    
    original_target <- target[i]
    on.exit({ target[i] <<- original_target }, add = TRUE)
    target[i] <<- replace_patterns(target[i], correspondences)
    
    alt1 <- prep_pword(i)
    alt2 <- matrix(strsplit(alt1[1, ], "")[[1]], ncol = length(strsplit(alt1[1, ], "")[[1]]))
    alt2 <- rbind(alt2, matrix(strsplit(alt1[2, ], "")[[1]], ncol = length(strsplit(alt1[2, ], "")[[1]])))
    alt2 <- reverse_replace_patterns(alt2, correspondences_reverse)
    
    if (level == "PG") {
      alt2 <- rbind(alt2, alt2[2, ])
      return(alt2)
    }
    
    temp_list <- mylist2
    temp_list[[i]] <- alt2
    if (level == "OR") {
      temp_list <- prep_pword_OR(temp_list)
    } else if (level == "OC") {
      temp_list <- prep_pword_OC(temp_list)
    }
    temp_list[[i]]
  }
  
  #now do the mappings!
  for(i in 1:length(target)){
    base_mat <- get_word_matrix(i, use_biphone = FALSE)
    use_max_mode <- (min_map == 1)
    base_spellings <- generate_spellings_from_matrix(base_mat, min_map, use_max = use_max_mode)
    
    alt_spellings <- character(0)
    if (check_biphones[i] == TRUE) {
      alt_mat <- get_word_matrix(i, use_biphone = TRUE)
      alt_spellings <- generate_spellings_from_matrix(alt_mat, min_map, use_max = use_max_mode)
    }
    
    combined_spellings <- unique(c(base_spellings, alt_spellings))
    
    # throttle candidate explosion by temporarily raising min_map per target
    if (length(combined_spellings) > max_options) {
      temp_min_map <- .05
      while (length(combined_spellings) > max_options && temp_min_map < .51) {
        base_spellings <- generate_spellings_from_matrix(base_mat, temp_min_map, use_max = FALSE)
        if (check_biphones[i] == TRUE) {
          alt_mat <- get_word_matrix(i, use_biphone = TRUE)
          alt_spellings <- generate_spellings_from_matrix(alt_mat, temp_min_map, use_max = FALSE)
        } else {
          alt_spellings <- character(0)
        }
        combined_spellings <- unique(c(base_spellings, alt_spellings))
        temp_min_map <- min(temp_min_map + .05, .5)
      }
    }
    
    mymat[[i]] <- combined_spellings
  }
  n_per_pron <- vapply(mymat, length, integer(1))
  all_spellings <- unlist(mymat, use.names = FALSE)
  
  if (length(all_spellings) == 0) {
    spellings_df <- data.frame(
      spelling = character(0),
      pronunciation = character(0),
      stringsAsFactors = FALSE
    )
    source_idx <- integer(0)
    source_prons <- character(0)
  } else {
    source_idx <- rep(seq_along(mymat), times = n_per_pron)
    source_prons <- target[source_idx]
    spellings_df <- data.frame(
      spelling = all_spellings,
      pronunciation = source_prons,
      stringsAsFactors = FALSE
    )
  }
  
  if (score == TRUE) {
    required_globals <- c("map_value", "summarize_words")
    missing_globals <- required_globals[!vapply(required_globals, exists, logical(1), inherits = TRUE)]
    if (length(missing_globals) > 0) {
      stop("Missing required objects in the global environment: ",
           paste(missing_globals, collapse = ", "))
    }
    
    if (length(all_spellings) == 0) {
      score_df <- data.frame(
        spelling = character(0),
        pronunciation = character(0),
        mean = numeric(0),
        input_index = integer(0),
        input_target = character(0),
        stringsAsFactors = FALSE
      )
      return(setNames(list(score_df), c(paste0("scores_", tolower(selected_param)))))
    }
    
    if (score_row %in% c("pp", "pp_count", "pp2", "pp2_count")) {
      if (is.null(parsed_corpus)) {
        stop("parsed_corpus must be provided when requesting parsing scores from pw_spell().")
      }
      if (score_row != "pp") {
        required_expanded_columns <- c("pp_count", "pp2_role", "index5", "index6", "pp2", "pp2_freq", "pp2_count")
        missing_expanded_columns <- if (length(parsed_corpus) < 3L) {
          required_expanded_columns
        } else {
          setdiff(required_expanded_columns, names(parsed_corpus[[3]]))
        }
        if (length(missing_expanded_columns) > 0L) {
          stop(
            "The supplied parsed_corpus predates the requested parsing measure. ",
            "Rebuild it with the current parse_corpus(); missing columns: ",
            paste(missing_expanded_columns, collapse = ", ")
          )
        }
      }
      if (is.null(PG_table)) {
        stop("PG_table must be provided when a parsing parameter is requested in pw_spell().")
      }
      pp_tables <- PG_table
      mapped_scores <- map_value(
        spelling = all_spellings,
        pronunciation = source_prons,
        level = "PG",
        tables = pp_tables,
        progress = FALSE,
        parsed_corpus = parsed_corpus
      )
      score_df <- summarize_words(mapped_scores, score_row, mode = "default")
      score_df <- score_df[is.finite(score_df$mean), , drop = FALSE]
    } else {
      mapped_scores <- map_value(
        spelling = all_spellings,
        pronunciation = source_prons,
        level = level,
        tables = tables,
        progress = FALSE
      )
      
      score_df <- summarize_words(mapped_scores, score_row, mode = summary_mode)
      if ("mean" %in% names(score_df)) {
        score_df <- score_df[!is.na(score_df$mean), , drop = FALSE]
      } else {
        keep_rows <- apply(score_df, 1, function(r) any(is.finite(suppressWarnings(as.numeric(r)))))
        score_df <- score_df[keep_rows, , drop = FALSE]
      }
    }
    
    if (nrow(score_df) > 0) {
      score_df$input_index <- source_idx[seq_len(nrow(score_df))]
      score_df$input_target <- source_prons[seq_len(nrow(score_df))]
    } else {
      score_df$input_index <- integer(0)
      score_df$input_target <- character(0)
    }
    
    if (nrow(score_df) > 0 && mean_score == 1 && "mean" %in% names(score_df)) {
      best_score <- max(score_df$mean, na.rm = TRUE)
      tie_tolerance <- sqrt(.Machine$double.eps) * max(1, abs(best_score))
      score_df <- score_df[abs(score_df$mean - best_score) <= tie_tolerance, , drop = FALSE]
    }
    
    if (nrow(score_df) > 0 && mean_score < 1 && "mean" %in% names(score_df)) {
      score_df <- score_df[score_df$mean >= mean_score, , drop = FALSE]
    }
    
    return(setNames(list(score_df), c(paste0("scores_", tolower(selected_param)))))
  }
  
  return(spellings_df)
}

#read pseudowords
pw_read <- function(target, level = "PG", parsed_corpus = parsed_corpus, PG_table, OC_table = NULL, OR_table = NULL, score = TRUE, min_pp = 1, min_map = 1, mean_score = 0, max_options = 1000, param = "GP", summary_mode = c("default", "ONC"), return_parse_pp_only = FALSE, target_graphemes = NULL, pp_param = "pp"){
  
  ##pw_read()
  require(stringr)
  require(data.tree)
  require(readxl)
  
  target <- as.character(target)
  bad_target <- !grepl("^[A-Za-z]+$", target)
  if (any(bad_target)) {
    stop(
      "pw_read target must contain alphabetic characters only (A-Z). Invalid target(s): ",
      paste(unique(target[bad_target]), collapse = ", ")
    )
  }
  
  if (length(target) == 0) {
    return(list())
  }
  if (isTRUE(return_parse_pp_only) && length(target) != 1L) {
    stop("return_parse_pp_only requires exactly one target spelling.")
  }
  
  ## 1. Check that globals exist
  required_globals <- c(
    "word_initial_mappings",
    "word_final_mappings",
    "syllable_medial_mappings",
    "syllable_final_mappings",
    "syllable_initial_mappings",
    ".add_pp2_context",
    "map_value",
    "summarize_words"
  )
  
  missing_globals <- required_globals[!vapply(required_globals, exists, logical(1), inherits = TRUE)]
  if (length(missing_globals) > 0) {
    stop("Missing required mapping objects in the global environment: ",
         paste(missing_globals, collapse = ", "))
  }
  
  level <- toupper(level)
  
  # checks based on "level"
  if (level == "PG") {
    if (is.null(PG_table)) {
      stop("PG_table must be provided when level = 'PG'.")
    }
  } else if (level == "ALL") {
    if (is.null(PG_table) || is.null(OC_table) || is.null(OR_table)) {
      stop("When level = 'all', PG_table, OC_table, and OR_table must all be provided.")
    }
  } else {
    stop("Unknown level: ", level, ". Use 'PG' or 'all'.")
  }
  
  selected_param <- toupper(param)
  if (!selected_param %in% c("PG", "GP", "PG_FREQ", "P_FREQ", "G_FREQ", "PP", "PP_COUNT", "PP2", "PP2_COUNT")) {
    stop("param must be one of: PG, GP, PG_freq, P_freq, G_freq, pp, pp_count, pp2, or pp2_count.")
  }
  summary_mode <- match.arg(summary_mode)
  if (summary_mode == "ONC" && mean_score != 0) {
    stop("mean_score thresholding is only available when summary_mode = 'default'.")
  }
  
  score_row <- switch(
    selected_param,
    "PG" = "PG",
    "GP" = "GP",
    "PG_FREQ" = "PG_freq",
    "P_FREQ" = "P_freq",
    "G_FREQ" = "G_freq",
    "PP" = "pp",
    "PP_COUNT" = "pp_count",
    "PP2" = "pp2",
    "PP2_COUNT" = "pp2_count"
  )
  pg_score_col <- paste0("PG_", score_row)

  pp_param <- tolower(as.character(pp_param)[1])
  allowed_pp_params <- c("pp", "pp_count", "pp2", "pp2_count")
  if (!pp_param %in% allowed_pp_params) {
    stop("pp_param must be one of: pp, pp_count, pp2, or pp2_count.")
  }
  
  # keep all ties for best-score and max-options truncation
  filter_with_ties <- function(df, metric_col, min_cut = NULL, mean_score = 0, max_options = 1000, apply_mean = TRUE, apply_min_cut = FALSE) {
    if (is.null(df) || nrow(df) == 0 || !(metric_col %in% names(df))) return(df)
    vals <- suppressWarnings(as.numeric(df[[metric_col]]))
    keep <- is.finite(vals)
    df <- df[keep, , drop = FALSE]
    vals <- vals[keep]
    if (length(vals) == 0) return(df)
    
    if (apply_min_cut && !is.null(min_cut)) {
      if (isTRUE(all.equal(min_cut, 1))) {
        parsing_group_col <- if ("parse_score" %in% names(df)) "parse_score" else if ("pp" %in% names(df)) "pp" else NULL
        if (!is.null(parsing_group_col)) {
          parsing_vals <- suppressWarnings(as.numeric(df[[parsing_group_col]]))
          has_parsing_value <- is.finite(parsing_vals)
          keep <- rep(FALSE, length(vals))
          
          if (any(has_parsing_value)) {
            parsing_key <- formatC(parsing_vals[has_parsing_value], digits = 15, format = "fg", flag = "#")
            idx_split <- split(which(has_parsing_value), parsing_key)
            for (idx in idx_split) {
              best_g <- max(vals[idx])
              keep[idx] <- vals[idx] == best_g
            }
          }
          
          if (any(!has_parsing_value)) {
            best_np <- max(vals[!has_parsing_value])
            keep[!has_parsing_value] <- vals[!has_parsing_value] == best_np
          }
        } else {
          best <- max(vals)
          keep <- vals == best
        }
        df <- df[keep, , drop = FALSE]
        vals <- vals[keep]
      } else {
        keep <- vals >= min_cut
        df <- df[keep, , drop = FALSE]
        vals <- vals[keep]
      }
    }
    if (nrow(df) == 0) return(df)
    
    if (apply_mean && mean_score == 1) {
      best <- max(vals)
      keep <- vals == best
      df <- df[keep, , drop = FALSE]
      vals <- vals[keep]
    }
    if (apply_mean && mean_score < 1) {
      keep <- vals >= mean_score
      df <- df[keep, , drop = FALSE]
      vals <- vals[keep]
    }
    if (nrow(df) == 0) return(df)
    
    if (!is.null(max_options) && is.finite(max_options) && max_options >= 1 && nrow(df) > max_options) {
      ord <- order(vals, decreasing = TRUE)
      cutoff <- vals[ord[min(max_options, length(ord))]]
      df <- df[vals >= cutoff, , drop = FALSE]
    }
    df
  }
  
  # ONC-mode selector: use average of available ONC means to apply
  # min_map/mean_score/max_options without changing reported columns.
  filter_with_onc_metric <- function(df, min_cut, mean_score, max_options) {
    if (is.null(df) || nrow(df) == 0) return(df)
    onc_cols <- intersect(c("mean_onset", "mean_nucleus", "mean_coda"), names(df))
    if (length(onc_cols) == 0) return(df)
    
    mat <- as.data.frame(lapply(df[, onc_cols, drop = FALSE], function(x) suppressWarnings(as.numeric(x))))
    metric <- rowMeans(mat, na.rm = TRUE)
    metric[!is.finite(metric)] <- NA_real_
    df[["..onc_metric"]] <- metric
    
    df <- filter_with_ties(
      df,
      "..onc_metric",
      min_cut = min_cut,
      mean_score = mean_score,
      max_options = max_options,
      apply_mean = TRUE,
      apply_min_cut = TRUE
    )
    df[["..onc_metric"]] <- NULL
    df
  }
  
  # Drop rows where all ONC means are 0 (or non-finite after coercion).
  drop_all_zero_onc <- function(df) {
    required <- c("mean_onset", "mean_nucleus", "mean_coda")
    if (is.null(df) || nrow(df) == 0 || !all(required %in% names(df))) return(df)
    
    m <- as.data.frame(lapply(df[, required, drop = FALSE], function(x) suppressWarnings(as.numeric(x))))
    m[!is.finite(as.matrix(m))] <- 0
    keep <- rowSums(m > 0, na.rm = TRUE) > 0
    df[keep, , drop = FALSE]
  }
  
  # phonotactic filter for candidate pronunciations
  # drop candidates that:
  # 1) have no vowel unit
  # 2) have only schwa-bearing vowel unit(s): "e" and "je"
  # 3) contain bare "3" not immediately followed by "r"
  # 4) contain "rr"
  filter_pron_candidates <- function(prons) {
    pr <- as.character(prons)
    if (length(pr) == 0) return(logical(0))
    
    escape_regex_local <- function(x) gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x, perl = TRUE)
    vowels_filter <- c("5","O","8","je","j3r","ju","jU","o","2",
                       "@","a","e","E","3","3r","i","1","c","u","U","^")
    v_esc <- escape_regex_local(vowels_filter)
    v_esc <- v_esc[order(nchar(v_esc), decreasing = TRUE)]
    vowel_pat <- paste0("(", paste(v_esc, collapse = "|"), ")")
    
    vowel_tokens <- regmatches(pr, gregexpr(vowel_pat, pr, perl = TRUE))
    n_vowels <- lengths(vowel_tokens)
    has_vowel <- n_vowels >= 1
    schwa_tokens <- c("e", "je")
    only_schwa_e <- has_vowel & vapply(vowel_tokens, function(v) all(v %in% schwa_tokens), logical(1))
    bad_bare3 <- grepl("3(?!r)", pr, perl = TRUE)
    bad_rr <- grepl("rr", pr, fixed = TRUE)
    
    has_vowel & !only_schwa_e & !bad_bare3 & !bad_rr
  }
  
  if (length(target) > 1) {
    out <- lapply(seq_along(target), function(i) {
      pw_read(
        target = target[i],
        parsed_corpus = parsed_corpus,
        PG_table = PG_table,
        OC_table = OC_table,
        OR_table = OR_table,
        level = level,
        score = score,
        min_pp = min_pp,
        min_map = min_map,
        mean_score = mean_score,
        max_options = max_options,
        param = param,
        summary_mode = summary_mode,
        pp_param = pp_param
      )
    })
    
    if (level == "ALL" && score) {
      score_name <- paste0("scores_", tolower(selected_param))
      level_names <- c("PG", "OC", "OR")
      out_by_level <- setNames(lapply(level_names, function(lvl) {
        score_dfs <- lapply(seq_along(out), function(i) {
          this_df <- out[[i]][[lvl]][[score_name]]
          this_df$input_index <- i
          this_df$input_target <- target[i]
          this_df
        })
        scores_df <- if (length(score_dfs) > 0) do.call(rbind, score_dfs) else data.frame()
        setNames(list(scores_df), c(score_name))
      }), level_names)
      return(out_by_level)
    }
    
    if (level == "ALL" && !score) {
      level_names <- c("PG", "OC", "OR")
      out_by_level <- setNames(lapply(level_names, function(lvl) {
        pron_dfs <- lapply(out, function(x) x[[lvl]][["pronunciations"]])
        pronunciations_df <- if (length(pron_dfs) > 0) do.call(rbind, pron_dfs) else data.frame(
          spelling = character(0),
          pronunciation = character(0),
          stringsAsFactors = FALSE
        )
        list(pronunciations = pronunciations_df)
      }), level_names)
      return(out_by_level)
    }
    
    if (score) {
      score_name <- paste0("scores_", tolower(selected_param))
      score_dfs <- lapply(seq_along(out), function(i) {
        this_df <- out[[i]][[score_name]]
        if (!("input_index" %in% names(this_df))) this_df$input_index <- i
        if (!("input_target" %in% names(this_df))) this_df$input_target <- target[i]
        this_df$input_index <- i
        this_df$input_target <- target[i]
        this_df
      })
      scores_df <- if (length(score_dfs) > 0) do.call(rbind, score_dfs) else data.frame()
      return(setNames(list(scores_df), c(score_name)))
    }
    
    pron_dfs <- out
    pronunciations_df <- if (length(pron_dfs) > 0) do.call(rbind, pron_dfs) else data.frame(
      spelling = character(0),
      pronunciation = character(0),
      stringsAsFactors = FALSE
    )
    return(pronunciations_df)
  }
  
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
  required_new_columns <- c("pp_count", "pp2_role", "index5", "index6", "pp2", "pp2_freq", "pp2_count")
  missing_new_columns <- setdiff(required_new_columns, names(corpus_parsed_table))
  if (length(missing_new_columns) > 0L && pp_param != "pp") {
    stop(
      "The supplied parsed_corpus predates the requested parsing measure. ",
      "Rebuild it with the current parse_corpus(); missing columns: ",
      paste(missing_new_columns, collapse = ", ")
    )
  }
  
  #make sure PG table is numeric format
  PG_table$gp <- PG_table$gp |>
    dplyr::mutate(
      wi = as.numeric(wi),
      si = as.numeric(si),
      sm = as.numeric(sm),
      sf = as.numeric(sf),
      wf = as.numeric(wf)
    )
  
  #create general 'mid-word' probabilities--because, before phonemes are mapped, internal syllabic structure is ambiguous!
  #the MAX across any position si, sm, sf is used--so that a min_map threshhold isn't too conservative
  PG_table$gp <- PG_table$gp |>
    dplyr::mutate(
      p_mid = pmax(
        ifelse(is.na(si), 0, si),
        ifelse(is.na(sm), 0, sm),
        ifelse(is.na(sf), 0, sf)
      )
    )
  
  
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
  
  #this generates the options for mappings
  make_options <- function(allbranches_df,PG_table,top_parse,min_map) {
    
    temp <- add_grapheme_index(allbranches_df)
    allbranches_df$index2 <- paste0(temp$grapheme_index, "_", temp$Position)
    
    top_graphemes <- cbind(allbranches_df[allbranches_df$parse_no==top_parse,]$Graphemes,allbranches_df[allbranches_df$parse_no==top_parse,]$Position,allbranches_df[allbranches_df$parse_no==top_parse,]$index2)
    top_graphemes <- top_graphemes[!duplicated(top_graphemes), , drop = FALSE] #will remove most duplicates, e.g., [UI] now appears once, UNLESS it actually occurs multiple times in the word
    
    gtop_options <- list()
    num_graph <- nrow(top_graphemes)
    
    for (j in seq_len(num_graph)) {
      gtop_options[[j]] <- PG_table$gp[PG_table$gp$grapheme == tolower(top_graphemes[j, 1]), ]
      
    }
    
    # Word-initial
    if (num_graph >= 1L && nrow(gtop_options[[1]]) > 0L) {
      gtop_options[[1]] <- gtop_options[[1]][gtop_options[[1]]$wi >= min(min_map,max(gtop_options[[1]]$wi)) , ,drop = FALSE]
    }
    
    # Word-final
    if (num_graph >= 1L && nrow(gtop_options[[num_graph]]) > 0L) {
      gtop_options[[num_graph]] <- gtop_options[[num_graph]][
        gtop_options[[num_graph]]$wf >= min(min_map,max(gtop_options[[num_graph]]$wf)),
        ,
        drop = FALSE
      ]
    }
    
    # Mid-word positions: use p_mid
    if (num_graph > 2L) {
      for (j in 2:(num_graph - 1L)) {
        if (nrow(gtop_options[[j]]) > 0L) {
          gtop_options[[j]] <- gtop_options[[j]][
            gtop_options[[j]]$p_mid >= min(min_map,max(gtop_options[[j]]$p_mid)),
            ,
            drop = FALSE
          ]
        }
      }
    }
    
    #pull out the phoneme options:
    phon_options <- lapply(gtop_options, function(x) x$phoneme)
    #generate all possible combos:
    phon_options <- expand.grid(phon_options, stringsAsFactors = FALSE)
    #turn them into strings
    phon_options <- apply(phon_options, 1, paste, collapse = "")
    return(phon_options)
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
  allbranches_df$index1 <- paste0(allbranches_df$Letter_Occurrence,allbranches_df$Graphemes,"_",allbranches_df$Position)
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

  pp2_context <- .add_pp2_context(allbranches_df, target)
  allbranches_df <- merge(
    allbranches_df,
    pp2_context,
    by = "Letter_Occurrence",
    all.x = TRUE
  )
  
  # Add the legacy PP indices and the expanded-context PP2 indices.
  allbranches_df$index3 <- paste0(allbranches_df$Letter,"_in_",allbranches_df$index2,"_and_",allbranches_df$reduced,"_SC_",allbranches_df$suffix_class)
  allbranches_df$index4 <- paste0(allbranches_df$Letter,"_in_",allbranches_df$reduced,"_SC_",allbranches_df$suffix_class)
  allbranches_df$index5 <- paste0(allbranches_df$Letter,"_role_",allbranches_df$pp2_role,"_in_",allbranches_df$index2,"_and_CTX_",allbranches_df$pp2_signature,"_SC_",allbranches_df$suffix_class)
  allbranches_df$index6 <- paste0(allbranches_df$Letter,"_role_",allbranches_df$pp2_role,"_in_CTX_",allbranches_df$pp2_signature,"_SC_",allbranches_df$suffix_class)

  pp_lookup <- parsing_table[, setdiff(names(parsing_table), "index4"), drop = FALSE]
  allbranches_df <- merge(allbranches_df,pp_lookup,by="index3",all.x=T) #note that all.x=T because impossible parses need to be kept

  if (all(c("index5", "pp2", "pp2_freq", "pp2_count") %in% names(corpus_parsed_table))) {
    pp2_lookup <- unique(corpus_parsed_table[, c("index5", "pp2", "pp2_freq", "pp2_count"), drop = FALSE])
    allbranches_df <- merge(allbranches_df,pp2_lookup,by="index5",all.x=T)
  } else {
    allbranches_df$pp2 <- NA_real_
    allbranches_df$pp2_freq <- NA_real_
    allbranches_df$pp2_count <- NA_real_
  }
  allbranches_df <- allbranches_df[order(allbranches_df$order),] #re-order into original parses
  allbranches_df$order <- NULL #now drop the order variable
  allbranches_df$pp[is.na(allbranches_df$pp)] <- 0 #replace NAs, which means the parse is NEVER used in the corpus, with 0's
  allbranches_df$pp_freq[is.na(allbranches_df$pp_freq)] <- 0 #replace NAs, which means the parse is NEVER used in the corpus, with 0's
  if (!("pp_count" %in% names(allbranches_df))) allbranches_df$pp_count <- 0
  allbranches_df$pp_count[is.na(allbranches_df$pp_count)] <- 0
  allbranches_df$pp2[is.na(allbranches_df$pp2)] <- 0
  allbranches_df$pp2_freq[is.na(allbranches_df$pp2_freq)] <- 0
  allbranches_df$pp2_count[is.na(allbranches_df$pp2_count)] <- 0
  
  # Mean values of all selectable parsing measures for each candidate parse.
  if (nrow(allbranches_df) == 0) {
    parse_metrics <- data.frame(
      parse_no = integer(0),
      pp = numeric(0),
      pp_count = numeric(0),
      pp2 = numeric(0),
      pp2_count = numeric(0),
      stringsAsFactors = FALSE
    )
  } else {
    parse_metrics <- aggregate(
      allbranches_df[, c("pp", "pp_count", "pp2", "pp2_count"), drop = FALSE],
      by = list(parse_no = allbranches_df$parse_no),
      FUN = mean
    )
  }
  parse_metrics$parse_score <- parse_metrics[[pp_param]]
  
  # Return spelling-side parse probabilities before generating pronunciation options.
  if (isTRUE(return_parse_pp_only)) {
    parse_pp_out <- parse_metrics
    
    if (!is.null(target_graphemes) && nrow(parse_pp_out) > 0L) {
      target_graphemes_norm <- unname(toupper(as.character(target_graphemes)))
      parse_nos <- sort(unique(allbranches_df$parse_no))
      parse_matches <- vapply(parse_nos, function(parse_no) {
        parse_rows <- allbranches_df[
          allbranches_df$parse_no == parse_no,
          ,
          drop = FALSE
        ]
        if (nrow(parse_rows) == 0L) return(FALSE)
        
        # add_grapheme_index expects parse numbers to start at one.
        parse_rows$parse_no <- 1L
        parse_rows <- add_grapheme_index(parse_rows)
        parse_graphemes <- as.character(
          parse_rows$Graphemes[!duplicated(parse_rows$grapheme_index)]
        )
        identical(unname(toupper(parse_graphemes)), target_graphemes_norm)
      }, logical(1))
      parse_pp_out <- parse_pp_out[
        parse_pp_out$parse_no %in% parse_nos[parse_matches],
        ,
        drop = FALSE
      ]
      
      if (nrow(parse_pp_out) > 0L) {
        best_parse <- parse_pp_out$parse_no[which.max(parse_pp_out$parse_score)]
        parse_rows <- allbranches_df[
          allbranches_df$parse_no == best_parse,
          ,
          drop = FALSE
        ]
        parse_pp_out <- data.frame(
          spelling = rep(target, nrow(parse_rows)),
          parse_no = rep(best_parse, nrow(parse_rows)),
          letter_index = seq_len(nrow(parse_rows)),
          letter = as.character(parse_rows$Letter),
          pp = suppressWarnings(as.numeric(parse_rows$pp)),
          pp_count = suppressWarnings(as.numeric(parse_rows$pp_count)),
          pp2 = suppressWarnings(as.numeric(parse_rows$pp2)),
          pp2_count = suppressWarnings(as.numeric(parse_rows$pp2_count)),
          stringsAsFactors = FALSE
        )
        return(parse_pp_out)
      }
    }
    
    if (nrow(parse_pp_out) == 0L) {
      return(data.frame(
        spelling = character(0),
        parse_no = integer(0),
        pp = numeric(0),
        pp_count = numeric(0),
        pp2 = numeric(0),
        pp2_count = numeric(0),
        parse_score = numeric(0),
        stringsAsFactors = FALSE
      ))
    }
    
    parse_pp_out$spelling <- target
    return(parse_pp_out[, c("spelling", "parse_no", "pp", "pp_count", "pp2", "pp2_count", "parse_score"), drop = FALSE])
  }
  
  
  # With scoring, generate a broad candidate pool first.
  # Final thresholding is then applied on the summarized score metric.
  gen_min_map <- if (score == TRUE) 0 else min_map
  
  if (nrow(parse_metrics) == 0) {
    top_parses <- integer(0)
  } else if (isTRUE(all.equal(min_pp, 1))) {
    # min_pp = 1 is the legacy sentinel for the best parse. Retain every
    # numerically tied maximum rather than only the first one.
    best_score <- max(parse_metrics$parse_score)
    tie_tolerance <- sqrt(.Machine$double.eps) * max(1, abs(best_score))
    top_parses <- parse_metrics$parse_no[
      abs(parse_metrics$parse_score - best_score) <= tie_tolerance
    ]
  } else {
    top_parses <- parse_metrics$parse_no[parse_metrics$parse_score >= min_pp]
  }

  phon_options <- vector("list", length(top_parses))
  for (k in seq_along(top_parses)) {
    top_parse <- top_parses[k]
    phon_options[[k]] <- make_options(allbranches_df,PG_table,top_parse,gen_min_map)

    ##THIS IS THE THROTTLE IF MAX_OPTIONS IS EXCEEDED:
    check_options <- length(phon_options[[k]])
    if(check_options > max_options && score == TRUE){
      temp_min_map <- .05
      while(check_options > max_options && temp_min_map < .51){
        phon_options[[k]] <- make_options(allbranches_df,PG_table,top_parse,temp_min_map)
        check_options <- length(phon_options[[k]])
        temp_min_map <- min(temp_min_map+.05,.5)
      }}
  }

  parse_metrics <- parse_metrics[
    match(top_parses, parse_metrics$parse_no, nomatch = 0L),
    ,
    drop = FALSE
  ]

  all_prons_raw <- unlist(phon_options, use.names = FALSE)
  n_per_parse <- vapply(phon_options, length, integer(1))
  parsing_measure_names <- c("pp", "pp_count", "pp2", "pp2_count")
  parsing_aligned_raw <- setNames(
    lapply(parsing_measure_names, function(metric) {
      rep(parse_metrics[[metric]], times = n_per_parse)
    }),
    parsing_measure_names
  )
  
  keep_prons <- filter_pron_candidates(all_prons_raw)
  all_prons <- all_prons_raw[keep_prons]
  parsing_aligned <- lapply(parsing_aligned_raw, function(x) x[keep_prons])
  pp_aligned <- parsing_aligned$pp
  pp_param_aligned <- parsing_aligned[[pp_param]]
  
  pronunciations_df <- data.frame(
    spelling = rep(target, length(all_prons)),
    pronunciation = all_prons,
    stringsAsFactors = FALSE
  )

  is_parsing_score <- score_row %in% parsing_measure_names
  
  if (score == TRUE){ 
    if (is_parsing_score) {
      selected_score_aligned <- parsing_aligned[[score_row]]
      gp_scores <- data.frame(
        spelling = rep(target, length(all_prons)),
        pronunciation = all_prons,
        mean = selected_score_aligned,
        pp = pp_aligned,
        parse_score = pp_param_aligned,
        stringsAsFactors = FALSE
      )
      if (score_row != "pp") gp_scores[[score_row]] <- selected_score_aligned
    } else {
      if (length(all_prons) == 0) {
        gp_scores <- data.frame()
      } else {
        gp_scores <- do.call(
          rbind,
          lapply(all_prons, function(p) {
            map_value(
              spelling      = target,
              pronunciation = p,
              level         = "PG",
              tables        = PG_table
            )
          })
        )
        gp_scores <- summarize_words(gp_scores, score_row, mode = summary_mode)
        if ("mean" %in% names(gp_scores)) {
          gp_scores <- gp_scores[!is.na(gp_scores$mean), , drop = FALSE]
        }
      }
      
      if (length(all_prons) == 0) {
        pp_by_pron <- data.frame(
          pronunciation = character(0),
          pp = numeric(0),
          parse_score = numeric(0),
          stringsAsFactors = FALSE
        )
      } else {
        pp_by_pron <- aggregate(
          cbind(pp, parse_score) ~ pronunciation,
          data = data.frame(
            pronunciation = all_prons,
            pp = pp_aligned,
            parse_score = pp_param_aligned,
            stringsAsFactors = FALSE
          ),
          FUN = max
        )
      }
      
      if (nrow(gp_scores) == 0L) {
        if (summary_mode == "default") {
          gp_scores <- data.frame(
            spelling = character(0),
            pronunciation = character(0),
            mean = numeric(0),
            median = numeric(0),
            max = numeric(0),
            min = numeric(0),
            sd = numeric(0),
            pp = numeric(0),
            parse_score = numeric(0),
            stringsAsFactors = FALSE
          )
        } else {
          gp_scores <- data.frame(
            spelling = character(0),
            pronunciation = character(0),
            mean_onset = numeric(0),
            median_onset = numeric(0),
            max_onset = numeric(0),
            min_onset = numeric(0),
            sd_onset = numeric(0),
            mean_nucleus = numeric(0),
            median_nucleus = numeric(0),
            max_nucleus = numeric(0),
            min_nucleus = numeric(0),
            sd_nucleus = numeric(0),
            mean_coda = numeric(0),
            median_coda = numeric(0),
            max_coda = numeric(0),
            min_coda = numeric(0),
            sd_coda = numeric(0),
            pp = numeric(0),
            parse_score = numeric(0),
            stringsAsFactors = FALSE
          )
        }
      } else {
        if (summary_mode == "default") {
          if (!("mean" %in% names(gp_scores))) {
            gp_scores$mean <- NA_real_
          }
          if (!("median" %in% names(gp_scores))) gp_scores$median <- NA_real_
          if (!("max" %in% names(gp_scores))) gp_scores$max <- NA_real_
          if (!("min" %in% names(gp_scores))) gp_scores$min <- NA_real_
          if (!("sd" %in% names(gp_scores))) gp_scores$sd <- NA_real_
          
          gp_scores <- merge(gp_scores[,c("spelling","pronunciation","mean","median","max","min","sd"), drop = FALSE],
                             pp_by_pron, by = "pronunciation", all.x = TRUE)
          gp_scores$pp[is.na(gp_scores$pp)] <- 0
          gp_scores$parse_score[is.na(gp_scores$parse_score)] <- 0
          gp_scores <- gp_scores[,c("spelling","pronunciation","mean","median","max","min","sd","pp","parse_score"), drop = FALSE]
        } else {
          gp_scores <- merge(gp_scores, pp_by_pron, by = "pronunciation", all.x = TRUE)
          gp_scores$pp[is.na(gp_scores$pp)] <- 0
          gp_scores$parse_score[is.na(gp_scores$parse_score)] <- 0
        }
      }
    }
  }
  
  if(score == TRUE && level == "ALL" && !is_parsing_score){
    onc_stat_cols <- c(
      "mean_onset","median_onset","max_onset","min_onset","sd_onset",
      "mean_nucleus","median_nucleus","max_nucleus","min_nucleus","sd_nucleus",
      "mean_coda","median_coda","max_coda","min_coda","sd_coda"
    )
    
    ## ---------- OC ----------
    if (length(all_prons) == 0) {
      OC_scores <- data.frame()
    } else {
      OC_scores <- do.call(
        rbind,
        lapply(all_prons, function(p) {
          map_value(
            spelling      = target,
            pronunciation = p,
            level         = "OC",
            tables        = OC_table
          )
        })
      )
      OC_scores <- summarize_words(OC_scores, score_row, mode = summary_mode)
    }
    
    if (summary_mode == "default") {
      if (nrow(OC_scores) == 0L || !("mean" %in% names(OC_scores))) {
        OC_scores <- data.frame(
          spelling      = rep(target, length(all_prons)),
          pronunciation = all_prons,
          mean          = 0,
          median        = 0,
          max           = 0,
          min           = 0,
          sd            = 0,
          stringsAsFactors = FALSE
        )
      } else {
        OC_scores[is.na(OC_scores$mean), "mean"] <- 0
        for (nm in c("median","max","min","sd")) {
          if (nm %in% names(OC_scores)) OC_scores[is.na(OC_scores[[nm]]), nm] <- 0
        }
      }
    } else {
      if (nrow(OC_scores) == 0L) {
        OC_scores <- data.frame(
          spelling = rep(target, length(all_prons)),
          pronunciation = all_prons,
          stringsAsFactors = FALSE
        )
        for (nm in onc_stat_cols) OC_scores[[nm]] <- 0
      } else {
        for (nm in onc_stat_cols) {
          if (!(nm %in% names(OC_scores))) OC_scores[[nm]] <- 0
          OC_scores[[nm]][is.na(OC_scores[[nm]])] <- 0
        }
      }
      OC_scores <- OC_scores[, c("spelling", "pronunciation", onc_stat_cols), drop = FALSE]
    }
    
    ## ---------- OR ----------
    if (length(all_prons) == 0) {
      OR_scores <- data.frame()
    } else {
      OR_scores <- do.call(
        rbind,
        lapply(all_prons, function(p) {
          map_value(
            spelling      = target,
            pronunciation = p,
            level         = "OR",
            tables        = OR_table
          )
        })
      )
      
      OR_scores <- summarize_words(OR_scores, score_row, mode = summary_mode)
    }
    
    if (summary_mode == "default") {
      if (nrow(OR_scores) == 0L || !("mean" %in% names(OR_scores))) {
        OR_scores <- data.frame(
          spelling      = rep(target, length(all_prons)),
          pronunciation = all_prons,
          mean          = 0,
          median        = 0,
          max           = 0,
          min           = 0,
          sd            = 0,
          stringsAsFactors = FALSE
        )
      } else {
        OR_scores[is.na(OR_scores$mean), "mean"] <- 0
        for (nm in c("median","max","min","sd")) {
          if (nm %in% names(OR_scores)) OR_scores[is.na(OR_scores[[nm]]), nm] <- 0
        }
      }
    } else {
      if (nrow(OR_scores) == 0L) {
        OR_scores <- data.frame(
          spelling = rep(target, length(all_prons)),
          pronunciation = all_prons,
          stringsAsFactors = FALSE
        )
        for (nm in onc_stat_cols) OR_scores[[nm]] <- 0
      } else {
        for (nm in onc_stat_cols) {
          if (!(nm %in% names(OR_scores))) OR_scores[[nm]] <- 0
          OR_scores[[nm]][is.na(OR_scores[[nm]])] <- 0
        }
      }
      OR_scores <- OR_scores[, c("spelling", "pronunciation", onc_stat_cols), drop = FALSE]
    }
    
    if (summary_mode == "ONC") {
      gp_keep <- c("spelling","pronunciation",onc_stat_cols,"pp")
      gp_keep <- gp_keep[gp_keep %in% names(gp_scores)]
      pg_scores <- gp_scores[, gp_keep, drop = FALSE]
    } else {
      pg_scores <- gp_scores
    }
    
    oc_had_pp <- "pp" %in% names(OC_scores)
    or_had_pp <- "pp" %in% names(OR_scores)
    if (exists("pp_by_pron", inherits = FALSE) && nrow(pp_by_pron) > 0) {
      if (nrow(OC_scores) > 0 && !oc_had_pp && "pronunciation" %in% names(OC_scores)) {
        OC_scores$pp <- pp_by_pron$pp[match(OC_scores$pronunciation, pp_by_pron$pronunciation)]
        OC_scores$pp[is.na(OC_scores$pp)] <- 0
      }
      if (nrow(OR_scores) > 0 && !or_had_pp && "pronunciation" %in% names(OR_scores)) {
        OR_scores$pp <- pp_by_pron$pp[match(OR_scores$pronunciation, pp_by_pron$pronunciation)]
        OR_scores$pp[is.na(OR_scores$pp)] <- 0
      }
    }
    
    if (summary_mode == "default") {
      # independent final selection per level
      pg_scores <- filter_with_ties(pg_scores, "mean", min_cut = min_map, mean_score = mean_score, max_options = max_options, apply_mean = TRUE, apply_min_cut = TRUE)
      OC_scores <- filter_with_ties(OC_scores, "mean", min_cut = min_map, mean_score = mean_score, max_options = max_options, apply_mean = TRUE, apply_min_cut = TRUE)
      OR_scores <- filter_with_ties(OR_scores, "mean", min_cut = min_map, mean_score = mean_score, max_options = max_options, apply_mean = TRUE, apply_min_cut = TRUE)
      
      # match pw_spell-style behavior: keep only candidates with positive mean
      if ("mean" %in% names(pg_scores) && nrow(pg_scores) > 0) {
        pg_scores <- pg_scores[is.finite(suppressWarnings(as.numeric(pg_scores$mean))) &
                                 suppressWarnings(as.numeric(pg_scores$mean)) > 0, , drop = FALSE]
      }
      if ("mean" %in% names(OC_scores) && nrow(OC_scores) > 0) {
        OC_scores <- OC_scores[is.finite(suppressWarnings(as.numeric(OC_scores$mean))) &
                                 suppressWarnings(as.numeric(OC_scores$mean)) > 0, , drop = FALSE]
      }
      if ("mean" %in% names(OR_scores) && nrow(OR_scores) > 0) {
        OR_scores <- OR_scores[is.finite(suppressWarnings(as.numeric(OR_scores$mean))) &
                                 suppressWarnings(as.numeric(OR_scores$mean)) > 0, , drop = FALSE]
      }
    } else {
      # ONC mode: apply independent selection using ONC composite metric
      pg_scores <- filter_with_onc_metric(pg_scores, min_cut = min_map, mean_score = mean_score, max_options = max_options)
      OC_scores <- filter_with_onc_metric(OC_scores, min_cut = min_map, mean_score = mean_score, max_options = max_options)
      OR_scores <- filter_with_onc_metric(OR_scores, min_cut = min_map, mean_score = mean_score, max_options = max_options)
      
      pg_scores <- drop_all_zero_onc(pg_scores)
      OC_scores <- drop_all_zero_onc(OC_scores)
      OR_scores <- drop_all_zero_onc(OR_scores)
    }
    
    if (!oc_had_pp && "pp" %in% names(OC_scores)) OC_scores$pp <- NULL
    if (!or_had_pp && "pp" %in% names(OR_scores)) OR_scores$pp <- NULL
    
    score_name <- paste0("scores_", tolower(selected_param))
    add_input_meta <- function(df, idx, tgt) {
      n <- nrow(df)
      df$input_index <- if (n > 0) rep.int(idx, n) else integer(0)
      df$input_target <- if (n > 0) rep.int(tgt, n) else character(0)
      df
    }
    pg_scores <- add_input_meta(pg_scores, 1L, target)
    OC_scores <- add_input_meta(OC_scores, 1L, target)
    OR_scores <- add_input_meta(OR_scores, 1L, target)
    
    return(list(
      PG = setNames(list(pg_scores), c(score_name)),
      OC = setNames(list(OC_scores), c(score_name)),
      OR = setNames(list(OR_scores), c(score_name))
    ))
  }
  
  if (score == TRUE && summary_mode == "default") {
    gp_scores <- filter_with_ties(gp_scores, "mean", min_cut = min_map, mean_score = mean_score, max_options = max_options, apply_mean = TRUE, apply_min_cut = TRUE)
    if ("mean" %in% names(gp_scores) && nrow(gp_scores) > 0) {
      gp_scores <- gp_scores[is.finite(suppressWarnings(as.numeric(gp_scores$mean))) &
                               suppressWarnings(as.numeric(gp_scores$mean)) > 0, , drop = FALSE]
    }
  }
  if (score == TRUE && summary_mode == "ONC") {
    gp_scores <- filter_with_onc_metric(gp_scores, min_cut = min_map, mean_score = mean_score, max_options = max_options)
    gp_scores <- drop_all_zero_onc(gp_scores)
  }
  
  if(score == TRUE){
    n <- nrow(gp_scores)
    gp_scores$input_index <- if (n > 0) rep.int(1L, n) else integer(0)
    gp_scores$input_target <- if (n > 0) rep.int(target, n) else character(0)
    return(setNames(list(gp_scores), c(paste0("scores_", tolower(selected_param)))))
  } else {
    if (level == "ALL") {
      return(list(
        PG = list(pronunciations = pronunciations_df),
        OC = list(pronunciations = pronunciations_df),
        OR = list(pronunciations = pronunciations_df)
      ))
    }
    return(pronunciations_df)
  }
}



#fourth: strip off unnecessary accent marks then map all the words at all grain sizes then map sound-spelling correspondence at all levels
wordlist_v2_1_merged$spelling <- replace_accented_vowels(wordlist_v2_1_merged$spelling)

#compute all sound-spelling mappings
all_words_PG <- map_PG(spelling=wordlist_v2_1_merged$spelling,pronunciation = wordlist_v2_1_merged$pronunciation, map_progress = TRUE)
# map_PG() now includes internal caching in v2.1, so these calls reuse the
# prior full-corpus PG mapping when inputs match.
all_words_ONC <- map_ONC(spelling=wordlist_v2_1_merged$spelling,pronunciation = wordlist_v2_1_merged$pronunciation, map_progress = TRUE)
all_words_OC <- map_OC(spelling=wordlist_v2_1_merged$spelling,pronunciation = wordlist_v2_1_merged$pronunciation, map_progress = TRUE)
all_words_OR <- map_OR(spelling=wordlist_v2_1_merged$spelling,pronunciation = wordlist_v2_1_merged$pronunciation, map_progress = TRUE)


#fifth: create tables of sublexical consistent and frequency, given the words mapped in step four

#English-only
all_tables_PG <- make_tables(all_words_PG)
all_tables_ONC <- make_tables(all_words_ONC)
all_tables_OC <- make_tables(all_words_OC)
all_tables_OR <- make_tables(all_words_OR)



#sixth: compute parsing probabilities given sound-spelling correspondence tables

#parse the corpus:
parsed_corpus <- parse_corpus(wordlist_v2_1_merged,all_words_PG)

####WHAT THE FILES ARE:
head(parsed_corpus[[1]]) ##[[1]] pp_table: legacy pp/pp_freq plus pp_count for each letter-grapheme combination
head(parsed_corpus[[2]]) ##[[2]] pp_reduced: this is needed for reading pseudowords
head(parsed_corpus[[3]]) ##[[3]] corpus_parsed: letter assignments plus pp, pp_freq, pp_count, pp2, pp2_freq, and pp2_count
head(parsed_corpus[[4]]) ##[[4]] corpus_pp: word-level means and minima for all parsing measures-MOST USEFUL TO INSPECT!


#seventh: score all the words, given the mappings and tables

#English-only
#only include the parsing probabilities (i.e., supply parsed_corpus) in the PG-level scores, as it takes a lot of extra time to do so (and does not depend on the larger grain sizes)
scored_words_PG <- map_value(spelling = wordlist_v2_1_merged$spelling, pronunciation = wordlist_v2_1_merged$pronunciation, level = "PG", tables = all_tables_PG, parsed_corpus = parsed_corpus)
scored_words_ONC <- map_value(spelling = wordlist_v2_1_merged$spelling, pronunciation = wordlist_v2_1_merged$pronunciation, level = "ONC", tables = all_tables_ONC)
scored_words_OC <- map_value(spelling = wordlist_v2_1_merged$spelling, pronunciation = wordlist_v2_1_merged$pronunciation, level = "OC", tables = all_tables_OC)
scored_words_OR <- map_value(spelling = wordlist_v2_1_merged$spelling, pronunciation = wordlist_v2_1_merged$pronunciation, level = "OR", tables = all_tables_OR)



### USAGE ###
#Examples of extracting information

#phonographeme spelling consistency for specific words
my_word <- "penguin"
summarize_words(mapped_words = scored_words_PG[which(wordlist_v2_1_merged$spelling==my_word)], parameter = "PG") #unweighted
summarize_words(mapped_words = scored_words_PG[which(wordlist_v2_1_merged$spelling==my_word)], parameter = "PG") #frequency-weighted

#phonographeme spelling consistency for all words in the corpus
all_words_PG_probability <- summarize_words(mapped_words = scored_words_PG, parameter = "PG")
head(all_words_PG_probability)

#onset-rime reading consistency for all words in the corpus
all_words_OR_GP_probability <- summarize_words(mapped_words = scored_words_OR, parameter = "GP")
head(all_words_OR_GP_probability)

#parsing measures for specific words
my_word <- "yacht"
parsed_corpus[[4]][parsed_corpus[[4]]$spelling==my_word,] #mean
parsed_corpus[[3]][parsed_corpus[[3]]$spelling==my_word,] #by letter (sorted alphabetically)



#find words beginning with the grapheme [HO] pronounced in any way
word_pattern(scored_words_PG,"any","ho","wi")

#find words ending with the orthography rime -[ANE] pronounced in any way
word_pattern(scored_words_OR,"any","a_en","wf")

#examples of generating spellings with pw_spell
#all spellings for /sib/ at PG level with at least 1% Spelling probability:
pw_spell(target = "sib", level = "PG", PG_table = all_tables_PG, min_map = 0.01, score = FALSE) 
#most likely spellings for /sib/ at all levels
pw_spell(target = "sib", level = "all", PG_table = all_tables_PG, OC_table = all_tables_OC, OR_table = all_tables_OR, min_map = 1) 


#examples of generating pronunciations with pw_read
#most likely pronunciation(s) for [sebe] at PG level under legacy pp:
pw_read(target = "sebe", level = "PG", parsed_corpus = parsed_corpus, PG_table = all_tables_PG, min_map = 1) 
#select parses with pp2 or pp2_count instead; min_pp = 1 retains all tied maxima:
pw_read(target = "bluise", level = "PG", parsed_corpus = parsed_corpus, PG_table = all_tables_PG, min_map = 1, min_pp = 1, pp_param = "pp2")
pw_read(target = "bluise", level = "PG", parsed_corpus = parsed_corpus, PG_table = all_tables_PG, min_map = 1, min_pp = 1, pp_param = "pp2_count")
#most likely pronunciation(s) for [sebe] at each level, per retained parse
pw_read(target = "sebe", level = "all", parsed_corpus = parsed_corpus, PG_table = all_tables_PG, OC_table = all_tables_OC, OR_table = all_tables_OR, min_map = 1, min_pp = 0, score = TRUE) 


#visualize parse trees
visualize_parse_tree("sebe")
visualize_parse_tree("resume", "rezum", show_plot = TRUE)
visualize_parse_tree("resume", "rEzum8", show_plot = TRUE)
