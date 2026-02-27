# Sublexical Toolkit v2.1 - Processing Scripts
# Extracted from Toolkit_v2.1.RData on 2026-02-26 
# Functions for phonographemic mapping, frequency/consistency analysis,
# pseudoword generation, and parse tree visualization.

# ============================================================ 
# replace_accented_vowels 
# ============================================================ 
replace_accented_vowels <- function (text) 
{
    accented_vowels <- c("á", "é", "ó", "ú", "à", "è", 
        "ì", "ò", "ù", "ä", "ë", "ï", "ö")
    unaccented_vowels <- c("a", "e", "o", "u", "a", "e", "i", 
        "o", "u", "a", "e", "i", "o")
    for (i in seq_along(accented_vowels)) {
        text <- gsub(accented_vowels[i], unaccented_vowels[i], 
            text, fixed = TRUE)
    }
    return(text)
} 

# ============================================================ 
# (closure helper for map_PG)
# map_PG_uncached 
# ============================================================ 
map_PG_uncached <- function (spelling, pronunciation, map_progress = FALSE) 
{
    require(stringr)
    require(stringi)
    vowels <- c("5", "O", "8", "je", "j3r", "ju", "jU", "o", 
        "2", "@", "a", "e", "E", "3r", "i", "1", "c", "u", "U", 
        "^")
    consonants <- c("G", "gz", "kS", "nj", "C", "T", "D", "Z", 
        "N", "b", "d", "f", "g", "h", "j", "k", "l", "m", "n", 
        "p", "r", "R", "s", "S", "t", "v", "w", "z")
    prep_word <- function(word_index) {
        hold0 <- as.data.frame(as.vector(sapply(pronunciation[word_index], 
            parse_syllables)))
        hold0 <- hold0[hold0 != "", ]
        parsed_syll <- matrix(strsplit(hold0, "   ")[[1]])
        letter_str <- tolower(spelling[word_index])
        syll_count <- parsed_syll
        tryCatch({
            syll_count[syll_count == "", ] <- NA
        }, error = function(e) {
            syll_count <<- 1
        })
        syll_count <- length(na.omit(syll_count))
        syll_struc <- parsed_syll
        tryCatch({
            syll_struc[1, ] <- sub(".", "1", syll_struc[1, ])
        }, error = function(e) {
            syll_struc <<- sub(".", "1", syll_struc)
        })
        for (i in 2:10) {
            tryCatch(syll_struc[i, ] <- {
                sub(".", "2", syll_struc[i, ])
            }, error = function(e) NA)
        }
        my.index <- matrix(NA, nrow = 10)
        for (i in 1:10) {
            tryCatch({
                my.index[i] <- nchar(syll_struc[i, ])
            }, error = function(e) NA)
        }
        my.index[1, ] <- nchar(syll_struc[1])
        for (i in 1:10) {
            tryCatch({
                substring(syll_struc[i, ], 2, my.index[i] - 1) <- paste(rep("3", 
                  my.index[i] - 2), collapse = "")
            }, error = function(e) NA)
        }
        if (max(nchar(syll_struc) == 1 & syll_count == 1)) {
            syll_struc <- 1
        }
        else {
            ifelse(syll_count == 1, substring(syll_struc[1], 
                2, my.index[1, ] - 1) <- paste(rep("3", my.index[1, 
                ] - 2), collapse = ""), syll_struc <- syll_struc)
        }
        for (i in 1:10) {
            ifelse(my.index[i] > 1, my.index[i] <- my.index[i], 
                my.index[i] <- 0)
        }
        for (i in 1:10) {
            tryCatch({
                substring(syll_struc[i, ], my.index[i], my.index[i]) <- "4"
            }, error = function(e) NA)
        }
        monophonemic <- FALSE
        ifelse(nchar(syll_struc[1]) == 1 & syll_count == 1, monophonemic <- TRUE, 
            monophonemic <- FALSE)
        ifelse(monophonemic == FALSE & syll_count > 1, str_sub(syll_struc[syll_count, 
            ], -1, -1) <- "5", syll_struc <- syll_struc)
        ifelse(monophonemic == FALSE & syll_count == 1, str_sub(syll_struc[syll_count], 
            -1, -1) <- "5", syll_struc <- syll_struc)
        map_j <- FALSE
        map_j_wi <- FALSE
        syll_struc0 <- paste(syll_struc, collapse = "")
        parsed_syll0 <- paste(parsed_syll, collapse = "")
        letter_str0 <- letter_str
        return(matrix(c(syll_struc0, parsed_syll0, letter_str0)))
    }
    parse_syllables <- function(pronunciation, word_index = 1) {
        library(stringr)
        has_bad_3 <- function(x) {
            grepl("3(?!.*r)", x, perl = TRUE)
        }
        current_string <- if (length(pronunciation) == 1L) 
            pronunciation
        else pronunciation[word_index]
        if (has_bad_3(current_string)) {
            stop(structure(list(message = paste0("__PARSE_FAIL__:", 
                current_string), input = current_string, output = NA_character_, 
                recon = NA_character_), class = c("parse_fail", 
                "error", "condition")))
        }
        fail_parse <- function(input, output) {
            recon <- gsub(" ", "", output, fixed = TRUE)
            if (!identical(recon, input)) {
                stop(structure(list(message = paste0("__PARSE_FAIL__:", 
                  input), input = input, output = output, recon = recon), 
                  class = c("parse_fail", "error", "condition")))
            }
            output
        }
        vowels <- c("5", "O", "8", "je", "j3r", "ju", "jU", "o", 
            "2", "@", "a", "e", "E", "3r", "i", "1", "c", "u", 
            "U", "^")
        consonants <- c("G", "gz", "kS", "nj", "C", "T", "D", 
            "Z", "N", "b", "d", "f", "g", "h", "j", "k", "l", 
            "m", "n", "p", "r", "R", "s", "S", "t", "v", "w", 
            "z")
        syll_init_cons <- c("flw", "spl", "spr", "str", "skr", 
            "skw", "skj", "njw", "bw", "Cw", "jw", "sw", "pl", 
            "pr", "Dj", "tr", "tw", "kl", "kr", "kw", "bl", "br", 
            "dr", "dw", "gl", "gr", "fl", "fr", "Tr", "Sr", "sl", 
            "st", "sp", "sk", "sm", "sn", "sf", "bj", "fj", "vj", 
            "mj", "kj", "hj", "nj", "pj", "tj", "Cj", "dj", "Tj", 
            "Gj", "gj", "Sj", "sj", "gw", "Dr", "hw", "G", "C")
        if (substring(current_string, 1, 2) == "gn") 
            syll_init_cons <- c("flw", "spl", "spr", "str", "skr", 
                "skw", "skj", "njw", "gn", "bw", "Cw", "jw", 
                "sw", "pl", "pr", "Dj", "tr", "tw", "kl", "kr", 
                "kw", "bl", "br", "dr", "dw", "gl", "gr", "fl", 
                "fr", "Tr", "Sr", "sl", "st", "sp", "sk", "sm", 
                "sn", "sf", "bj", "fj", "vj", "mj", "kj", "hj", 
                "nj", "pj", "tj", "Cj", "dj", "Tj", "Gj", "gj", 
                "Sj", "sj", "gw", "Dr", "hw", "G", "C")
        all_phonemes <- c(vowels, syll_init_cons, consonants)
        phonemes <- c()
        remaining <- current_string
        vowel_count <- 0
        while (nchar(remaining) > 0) {
            matched <- FALSE
            if (length(phonemes) == 0 || (length(phonemes) > 
                0 && phonemes[length(phonemes)] %in% vowels)) {
                for (p in syll_init_cons) {
                  if (startsWith(remaining, p)) {
                    phonemes <- c(phonemes, p)
                    remaining <- substring(remaining, nchar(p) + 
                      1)
                    matched <- TRUE
                    break
                  }
                }
            }
            if (!matched) {
                for (p in all_phonemes) {
                  if (startsWith(remaining, p)) {
                    phonemes <- c(phonemes, p)
                    remaining <- substring(remaining, nchar(p) + 
                      1)
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
        pattern <- paste(ifelse(phonemes %in% vowels, "V", "C"), 
            collapse = "")
        if (vowel_count < 2) {
            return(current_string)
        }
        rules <- list(CCCVCC = 5, CCVCC = 4, CVCCC = 4, CCCVC = 4, 
            CVCC = 3, CCVC = 3, CVC = 2, CVV = 2, VCC = 2, VCV = 1, 
            VV = 1)
        split_index <- 0
        for (rule in names(rules)) {
            if (startsWith(pattern, rule)) {
                split_index <- rules[[rule]]
                break
            }
        }
        if (split_index >= 1) {
            left <- paste(phonemes[1:split_index], collapse = "")
            right <- paste(phonemes[(split_index + 1):length(phonemes)], 
                collapse = "")
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
    map_wf <- function(p_wf, syll_struc0, parsed_syll0, letter_str0) {
        if (p_wf == "u") 
            tryCatch({
                ifelse(map_j == TRUE & my.index == flag.index_j, 
                  if (str_sub(parsed_syll0, -2, -1) == "ju") 
                    p_wf <- str_sub(parsed_syll0, -2, -1)
                  else if (str_sub(parsed_syll0, -3, -2) == "ju") 
                    p_wf <- str_sub(parsed_syll0, -3, -2), p_wf <- p_wf)
            }, error = function(e) NA)
        p_wf_prior <- str_sub(parsed_syll0, -2, -2)
        no_z_es <- FALSE
        no_d_ed <- FALSE
        no_s_es <- FALSE
        if (p_wf_prior %in% vowels & p_wf == "d" | p_wf_prior %in% 
            vowels & p_wf == "z" | p_wf_prior %in% vowels & p_wf == 
            "s") {
            no_z_es <- TRUE
            no_d_ed <- TRUE
            no_s_es <- TRUE
        }
        else {
            no_z_es <- no_z_es
            no_s_es <- no_s_es
            no_d_ed <- no_d_ed
        }
        no_aeiou_haeiou <- FALSE
        ifelse(p_wf_prior == "C" & p_wf %in% c("a", "i", "E", 
            "o", "u") & stri_sub(letter_str0, -3, -3) == "c", 
            no_aeiou_haeiou <- TRUE, no_aeiou_haeiou <- no_aeiou_haeiou)
        no_aeiou_haeiou <- no_aeiou_haeiou
        ifelse(p_wf_prior == "h" & p_wf %in% c("a", "i", "E", 
            "o", "u") & stri_sub(letter_str0, -2, -2) == "h", 
            no_aeiou_haeiou <- TRUE, no_aeiou_haeiou <- no_aeiou_haeiou)
        no_z_ds <- FALSE
        if (p_wf_prior == "d" & p_wf == "z") 
            no_z_ds <- TRUE
        no_d_ld <- FALSE
        if (p_wf_prior == "l" & p_wf == "d") 
            no_d_ld <- TRUE
        no_t_ct <- FALSE
        no_t_bt <- FALSE
        if (p_wf_prior == "k" & p_wf == "t") {
            no_t_ct <- TRUE
        }
        if (p_wf_prior == "b" & p_wf == "t") {
            no_t_bt <- TRUE
        }
        no_e_ia <- FALSE
        no_e_ea <- FALSE
        if (p_wf_prior == "j" & p_wf == "e" | p_wf_prior == "i" & 
            p_wf == "e" | p_wf_prior == "2" & p_wf == "e" | p_wf_prior %in% 
            c("S", "Z") & p_wf == "e") {
            no_e_ia <- TRUE
        }
        if (p_wf_prior == "j" & p_wf == "e" | p_wf_prior == "i" & 
            p_wf == "e" | p_wf_prior == "2" & p_wf == "e" | p_wf_prior %in% 
            c("S") & p_wf == "e") {
            no_e_ea <- TRUE
        }
        no_k_lk <- FALSE
        if (p_wf_prior == "l" & p_wf == "k") {
            no_k_lk <- TRUE
        }
        no_n_ln <- FALSE
        if (p_wf_prior == "l" & p_wf == "n") {
            no_n_ln <- TRUE
        }
        no_f_lf <- FALSE
        if (p_wf_prior == "l" & p_wf == "f") {
            no_f_lf <- TRUE
        }
        no_m_lm <- FALSE
        if (p_wf_prior == "l" & p_wf == "m") {
            no_m_lm <- TRUE
        }
        no_s_tz <- FALSE
        if (p_wf_prior == "t" & p_wf == "s") {
            no_s_tz <- TRUE
        }
        no_v_lv <- FALSE
        if (p_wf_prior == "l" & p_wf == "v") {
            no_v_lv <- TRUE
        }
        no_ieia_schwa <- FALSE
        if (p_wf == "e" & p_wf_prior == "S" & stri_sub(letter_str0, 
            -3, -3) == "t") {
            no_ieia_schwa <- TRUE
        }
        no_C_tch <- FALSE
        if (p_wf_prior == "t" & p_wf == "C") {
            no_C_tch <- TRUE
        }
        p_wf_2prior <- str_sub(parsed_syll0, -3, -2)
        if (p_wf_2prior == "3r") {
            no_z_es <- TRUE
            no_d_ed <- TRUE
        }
        if (p_wf_2prior == "se") {
            no_l_sl <- TRUE
        }
        else {
            no_l_sl <- FALSE
        }
        ifelse(p_wf_2prior == "^n" & p_wf == "z", no_z_es <- TRUE, 
            no_z_es <- no_z_es)
        p_wf_anteprior <- str_sub(parsed_syll0, -3, -3)
        if (p_wf_anteprior == "e" & p_wf_prior == "l") {
            no_z_es <- TRUE
            no_d_ed <- TRUE
        }
        else {
            no_z_es <- no_z_es
            no_d_ed <- no_d_ed
        }
        p_wf_jointprior <- str_sub(parsed_syll0, -2, -1)
        wf_x <- str_sub(letter_str0, -1, -1)
        map_x = FALSE
        ifelse(wf_x == "x", ifelse(p_wf_jointprior == "ks", map_x <- TRUE, 
            map_x <- FALSE), map_x <- FALSE)
        wf_xe <- str_sub(letter_str0, -2, -1)
        map_xe = FALSE
        ifelse(wf_xe == "xe", ifelse(p_wf_jointprior == "ks", 
            map_xe <- TRUE, map_xe <- FALSE), map_xe <- FALSE)
        g_opt <- word_final_mappings[word_final_mappings$phoneme == 
            p_wf, ]
        if (nrow(g_opt) == 0) {
            g_opt <- word_final_mappings[word_final_mappings$phoneme == 
                "s", ][1, ]
            g_opt[1, 1] <- "6"
            g_opt[1, 2] <- "6"
        }
        ifelse(p_wf_prior == "p" & p_wf == "t", g_opt <- g_opt[!g_opt$grapheme == 
            "pt", ], g_opt <- g_opt)
        ifelse(p_wf_prior == "b" & p_wf == "t", g_opt <- g_opt[!g_opt$grapheme == 
            "bt", ], g_opt <- g_opt)
        ifelse(no_z_es == TRUE, g_opt <- g_opt[!g_opt$grapheme == 
            "es", ], g_opt <- g_opt)
        ifelse(no_s_es == TRUE, g_opt <- g_opt[!g_opt$grapheme == 
            "es", ], g_opt <- g_opt)
        ifelse(no_z_ds == TRUE, g_opt <- g_opt[!g_opt$grapheme == 
            "ds", ], g_opt <- g_opt)
        ifelse(no_d_ed == TRUE, g_opt <- g_opt[!g_opt$grapheme == 
            "ed", ], g_opt <- g_opt)
        ifelse(no_d_ld == TRUE, g_opt <- g_opt[!g_opt$grapheme == 
            "ld", ], g_opt <- g_opt)
        ifelse(no_e_ia == TRUE, g_opt <- g_opt[!g_opt$grapheme == 
            "ia", ], g_opt <- g_opt)
        ifelse(no_e_ea == TRUE, g_opt <- g_opt[!g_opt$grapheme == 
            "ea", ], g_opt <- g_opt)
        ifelse(no_t_ct == TRUE, g_opt <- g_opt[!g_opt$grapheme == 
            "ct", ], g_opt <- g_opt)
        ifelse(no_t_bt == TRUE, g_opt <- g_opt[!g_opt$grapheme == 
            "bt", ], g_opt <- g_opt)
        ifelse(no_k_lk == TRUE, g_opt <- g_opt[!g_opt$grapheme == 
            "lk", ], g_opt <- g_opt)
        ifelse(no_n_ln == TRUE, g_opt <- g_opt[!g_opt$grapheme == 
            "ln", ], g_opt <- g_opt)
        ifelse(no_f_lf == TRUE, g_opt <- g_opt[!g_opt$grapheme == 
            "lf", ], g_opt <- g_opt)
        ifelse(no_m_lm == TRUE, g_opt <- g_opt[!g_opt$grapheme == 
            "lm", ], g_opt <- g_opt)
        ifelse(no_s_tz == TRUE, g_opt <- g_opt[!g_opt$grapheme == 
            "tz", ], g_opt <- g_opt)
        ifelse(no_v_lv == TRUE, g_opt <- g_opt[!g_opt$grapheme == 
            "lv", ], g_opt <- g_opt)
        ifelse(no_l_sl == TRUE, g_opt <- g_opt[!g_opt$grapheme == 
            "sl", ], g_opt <- g_opt)
        ifelse(no_C_tch == TRUE, g_opt <- g_opt[!g_opt$grapheme == 
            "tch", ], g_opt <- g_opt)
        ifelse(no_ieia_schwa == TRUE, g_opt <- g_opt[!g_opt$grapheme %in% 
            c("ie", "ia"), ], g_opt <- g_opt)
        ifelse(no_aeiou_haeiou == TRUE, g_opt <- g_opt[!g_opt$grapheme %in% 
            c("ha", "he", "hi", "ho", "hu"), ], g_opt <- g_opt)
        ifelse(map_x, g_opt <- word_final_mappings[word_final_mappings$grapheme == 
            "x", ], g_opt <- g_opt)
        ifelse(map_xe, g_opt <- word_final_mappings[word_final_mappings$grapheme == 
            "x", ], g_opt <- g_opt)
        n_opt <- length(g_opt$grapheme)
        search_lists <- list()
        for (j in (1:n_opt)) {
            search_lists[[j]] <- matrix(NA, nrow = 1, ncol = 2)
            search_lists[[j]][1] <- max(unlist(gregexpr(g_opt$grapheme[j], 
                letter_str0))) + nchar(g_opt$grapheme[j]) - 1
            ifelse(gregexpr(g_opt$grapheme[j], letter_str0)[[1]][1] == 
                -1, search_lists[[j]][1] <- -1, search_lists[[j]][1] <- search_lists[[j]][1])
            search_lists[[j]][2] <- nchar(g_opt$grapheme[j])
        }
        select_g <- t(matrix(unlist(search_lists), ncol = n_opt, 
            nrow = 2))
        g_wf <- which(select_g[, 1] == max(select_g[, 1]))[which.max(select_g[which(select_g[, 
            1] == max(select_g[, 1])), 2])]
        g_wf <- g_opt$grapheme[g_wf]
        ifelse(max(select_g[, 1]) < 1, g_wf <- "FAILED", g_wf <- g_wf)
        letter_str.latest <- stri_reverse(str_replace(stri_reverse(letter_str0), 
            stri_reverse(g_wf), "_"))
        if (str_sub(letter_str.latest, -2, -2) == "_") {
            if (str_sub(letter_str.latest, -1, -1) != "e") {
                if (p_wf_prior == "j") {
                  flag.restart <<- TRUE
                  flag.index_j <<- my.index
                  map_j <<- TRUE
                }
                else {
                  flag.restart <<- TRUE
                  flag.index <<- my.index
                }
            }
        }
        if (tail(unlist(gregexpr("_", letter_str.latest)), 1) == 
            nchar(letter_str.latest) - 1 | tail(unlist(gregexpr("_", 
            letter_str.latest)), 1) == nchar(letter_str.latest) | 
            tail(unlist(gregexpr("_", letter_str.latest)), 1) == 
                -1) {
            flag.restart <- flag.restart
        }
        else {
            if (p_wf_prior == "j") {
                flag.restart <<- TRUE
                map_j <<- TRUE
                flag.index_j <<- my.index
            }
            else {
                flag.restart <<- TRUE
                flag.index <<- my.index
            }
        }
        ifelse(str_sub(letter_str.latest, -1, -1) == "e", letter_str.latest <- letter_str.latest, 
            letter_str.latest <- str_sub(letter_str.latest, end = -2))
        parsed_syll.latest <- str_sub(parsed_syll0, 1, -(nchar(p_wf) + 
            1))
        ifelse(map_x, parsed_syll.latest <- str_sub(parsed_syll0, 
            1, -3), parsed_syll.latest <- parsed_syll.latest)
        ifelse(map_xe, parsed_syll.latest <- str_sub(parsed_syll0, 
            1, -3), parsed_syll.latest <- parsed_syll.latest)
        ifelse(map_x, p_wf <- p_wf_jointprior, p_wf <- p_wf)
        ifelse(map_xe, p_wf <- p_wf_jointprior, p_wf <- p_wf)
        syll_struc.latest <- str_sub(syll_struc0, 1, -(nchar(p_wf) + 
            1))
        PGlist <- list()
        PGlist <- append(PGlist, matrix(c(p_wf, g_wf, "5", syll_struc0, 
            syll_struc.latest, parsed_syll.latest, letter_str.latest)))
        return(PGlist)
    }
    map_m <- function(p_m, syll_struc.latest, parsed_syll.latest, 
        letter_str.latest) {
        if (p_m == "u") 
            tryCatch({
                ifelse(map_j == TRUE & my.index == flag.index_j, 
                  if (str_sub(parsed_syll.latest, -2, -1) == 
                    "ju") 
                    p_m <- str_sub(parsed_syll.latest, -2, -1)
                  else if (str_sub(parsed_syll.latest, -3, -2) == 
                    "ju") 
                    p_m <- str_sub(parsed_syll.latest, -3, -2), 
                  p_m <- p_m)
            }, error = function(e) NA)
        if (p_m == "e") 
            tryCatch({
                ifelse(map_j == TRUE & my.index == flag.index_j, 
                  if (str_sub(parsed_syll.latest, -2, -1) == 
                    "je") 
                    p_m <- str_sub(parsed_syll.latest, -2, -1)
                  else if (str_sub(parsed_syll.latest, -3, -2) == 
                    "je") 
                    p_m <- str_sub(parsed_syll.latest, -3, -2), 
                  p_m <- p_m)
            }, error = function(e) NA)
        if (p_m == "U") 
            tryCatch({
                ifelse(map_j == TRUE & my.index == flag.index_j, 
                  if (str_sub(parsed_syll.latest, -2, -1) == 
                    "jU") 
                    p_m <- str_sub(parsed_syll.latest, -2, -1)
                  else if (str_sub(parsed_syll.latest, -3, -2) == 
                    "jU") 
                    p_m <- str_sub(parsed_syll.latest, -3, -2), 
                  p_m <- p_m)
            }, error = function(e) NA)
        if (p_m == "3") 
            tryCatch({
                ifelse(map_j == TRUE & my.index == flag.index_j, 
                  if (str_sub(parsed_syll.latest, -2, -1) == 
                    "j3") 
                    p_m <- str_sub(parsed_syll.latest, -2, -1)
                  else if (str_sub(parsed_syll.latest, -3, -2) == 
                    "j3") 
                    p_m <- str_sub(parsed_syll.latest, -3, -2), 
                  p_m <- p_m)
            }, error = function(e) NA)
        p_m_prior <- str_sub(parsed_syll.latest, -2, -2)
        no_z_es <- FALSE
        no_r_or <- FALSE
        no_d_ed <- FALSE
        no_s_tz <- FALSE
        no_t_pt <- FALSE
        no_a_aha <- FALSE
        no_v_lv <- FALSE
        no_u_wo <- FALSE
        no_k_lk <- FALSE
        no_n_gn <- FALSE
        ifelse(p_m_prior %in% vowels & p_m == "z", no_z_es <- TRUE, 
            no_z_es <- no_z_es)
        ifelse(p_m_prior %in% vowels & p_m == "d", no_d_ed <- TRUE, 
            no_z_es <- no_d_ed)
        ifelse(p_m_prior == "c" & p_m == "r" | p_m_prior == "3" & 
            p_m == "r", no_r_or <- TRUE, no_r_or <- no_r_or)
        ifelse(p_m_prior == "t" & p_m == "s", no_s_tz <- TRUE, 
            no_s_tz <- no_s_tz)
        ifelse(p_m_prior == "p" & p_m == "t", no_t_pt <- TRUE, 
            no_t_pt <- no_t_pt)
        ifelse(p_m_prior == "h" & p_m == "@", no_a_aha <- TRUE, 
            no_a_aha <- no_a_aha)
        ifelse(p_m_prior == "l" & p_m == "v", no_v_lv <- TRUE, 
            no_v_lv <- no_v_lv)
        ifelse(p_m_prior == "w" & p_m == "u", no_u_wo <- TRUE, 
            no_u_wo <- no_u_wo)
        ifelse(p_m_prior == "l" & p_m == "k", no_k_lk <- TRUE, 
            no_k_lk <- no_k_lk)
        ifelse(p_m_prior == "g" & p_m == "n", no_n_gn <- TRUE, 
            no_n_gn <- no_n_gn)
        no_w_hu <- FALSE
        ifelse(p_m_prior == "C" & p_m == "w", no_w_hu <- TRUE, 
            no_w_hu <- no_w_hu)
        no_aeiou_haeiou <- FALSE
        ifelse(p_m_prior %in% c("C", "S") & p_m %in% c("a", "i", 
            "E", "o", "u") & stri_sub(letter_str.latest, -3, 
            -3) == "c" || p_m_prior %in% c("h", "T", "D", "f") & 
            p_m %in% c("a", "i", "E", "o", "u") & stri_sub(letter_str.latest, 
            -2, -2) == "h" || p_m_prior %in% c("g", "w") & p_m %in% 
            c("a", "i", "E", "o", "u") & stri_sub(letter_str.latest, 
            -3, -3) %in% c("g", "w"), no_aeiou_haeiou <- TRUE, 
            no_aeiou_haeiou <- no_aeiou_haeiou)
        no_j_hi <- FALSE
        ifelse(p_m == "j" & p_m_prior == "C", no_j_hi <- TRUE, 
            no_j_hi <- no_j_hi)
        ifelse(p_m_prior == "o" & p_m == "r", no_r_or <- TRUE, 
            no_r_or <- no_r_or)
        check_for_enye <- FALSE
        if (stri_sub(letter_str.latest, -1, -1) == "ñ") 
            check_for_enye <- TRUE
        if (p_m == "j" & p_m_prior == "n" & check_for_enye == 
            TRUE) 
            p_m <- "nj"
        check_for_gn <- FALSE
        if (stri_sub(letter_str.latest, -2, -1) == "gn" & p_m == 
            "j" & p_m_prior == "n") 
            check_for_gn <- TRUE
        if (check_for_gn == TRUE) 
            p_m <- "nj"
        no_e_ia <- FALSE
        no_e_ea <- FALSE
        no_e_ie <- FALSE
        no_e_ia_e <- FALSE
        if (p_m_prior == "j" & p_m == "e" | p_m_prior == "i" & 
            p_m == "e") {
            no_e_ia <- TRUE
            no_e_ea <- TRUE
            no_e_ie <- TRUE
            no_e_ia_e <- TRUE
        }
        if (p_m_prior == "S" & p_m == "e" & stri_sub(letter_str.latest, 
            -3, -3) == "t") {
            no_e_ia <- TRUE
            no_e_ie <- TRUE
        }
        no_E_ie <- FALSE
        no_E_aye <- FALSE
        if (p_m_prior == "j" & p_m == "E") {
            no_E_ie <- TRUE
            no_E_aye <- TRUE
        }
        no_e_ui <- FALSE
        if (p_m_prior == "w" & p_m == "e") {
            no_e_ui <- TRUE
        }
        no_o_e <- FALSE
        no_1_ui <- FALSE
        ifelse(p_m_prior == "w" & p_m == "^", no_o_e <- TRUE, 
            no_o_e <- no_o_e)
        ifelse(p_m_prior == "w" & p_m == "1" & stri_sub(letter_str.latest, 
            -3, -3) != "w", no_1_ui <- TRUE, no_1_ui <- no_1_ui)
        no_3_he <- FALSE
        ifelse(p_m_prior != "p" & p_m == "3", no_3_he <- TRUE, 
            no_3_he <- no_3_he)
        no_m_lm <- FALSE
        ifelse(p_m_prior == "l" & p_m == "m", no_m_lm <- TRUE, 
            no_m_lm <- no_m_lm)
        p_m_jointprior <- str_sub(parsed_syll.latest, -2, -1)
        m_x <- str_sub(letter_str.latest, -1, -1)
        map_x = FALSE
        ifelse(m_x == "x", ifelse(p_m_jointprior == "ks", map_x <- TRUE, 
            map_x <- FALSE), map_x <- FALSE)
        m_xe <- str_sub(letter_str.latest, -2, -1)
        map_xe = FALSE
        ifelse(m_xe == "xe", ifelse(p_m_jointprior == "ks", map_xe <- TRUE, 
            map_xe <- FALSE), map_xe <- FALSE)
        p_m_2prior <- str_sub(parsed_syll.latest, -3, -2)
        if (p_m_2prior == "3r") {
            ifelse(p_m == "z", no_z_es <- TRUE, no_z_es <- no_z_es)
            ifelse(p_m == "d", no_d_ed <- TRUE, no_d_ed <- no_d_ed)
        }
        g_opt <- syllable_medial_mappings[syllable_medial_mappings$phoneme == 
            p_m, ]
        if (nrow(g_opt) == 0) {
            g_opt <- syllable_medial_mappings[syllable_medial_mappings$phoneme == 
                "s", ][1, ]
            g_opt[1, 1] <- "6"
            g_opt[1, 2] <- "6"
        }
        if (flag.index == my.index) {
            if (p_m == "3") {
                g_opt <- g_opt[-which(g_opt$grapheme == "he"), 
                  ]
            }
        }
        if (flag.index == my.index) {
            if (grepl("x", letter_str.latest) & grepl("gz", parsed_syll.latest)) {
                stri_sub(syll_struc0, unlist(gregexpr("gz", parsed_syll.latest)) + 
                  1, unlist(gregexpr("gz", parsed_syll.latest)) + 
                  1) <- "4"
                if (stri_sub(syll_struc0, unlist(gregexpr("gz", 
                  parsed_syll.latest)) + 2, unlist(gregexpr("gz", 
                  parsed_syll.latest)) + 2) == "3") {
                  stri_sub(syll_struc0, unlist(gregexpr("gz", 
                    parsed_syll.latest)) + 2, unlist(gregexpr("gz", 
                    parsed_syll.latest)) + 2) <- "2"
                }
            }
            syll_struc0 <<- syll_struc0
            flag.restart <<- TRUE
            flag.index <<- 0
        }
        if (flag.index == my.index) {
            if (grepl("x", letter_str.latest) & grepl("ks", parsed_syll.latest)) {
                stri_sub(syll_struc0, unlist(gregexpr("ks", parsed_syll.latest)) + 
                  1, unlist(gregexpr("ks", parsed_syll.latest)) + 
                  1) <- "4"
                if (stri_sub(syll_struc0, unlist(gregexpr("ks", 
                  parsed_syll.latest)) + 2, unlist(gregexpr("ks", 
                  parsed_syll.latest)) + 2) == "3") {
                  stri_sub(syll_struc0, unlist(gregexpr("ks", 
                    parsed_syll.latest)) + 2, unlist(gregexpr("ks", 
                    parsed_syll.latest)) + 2) <- "2"
                }
            }
            syll_struc0 <<- syll_struc0
            flag.restart <<- TRUE
            flag.index <<- 0
        }
        if (flag.index == my.index) {
            if (grepl("x", letter_str.latest) & grepl("kS", parsed_syll.latest)) {
                stri_sub(syll_struc0, unlist(gregexpr("kS", parsed_syll.latest)) + 
                  1, unlist(gregexpr("kS", parsed_syll.latest)) + 
                  1) <- "4"
                if (stri_sub(syll_struc0, unlist(gregexpr("kS", 
                  parsed_syll.latest)) + 2, unlist(gregexpr("kS", 
                  parsed_syll.latest)) + 2) == "3") {
                  stri_sub(syll_struc0, unlist(gregexpr("kS", 
                    parsed_syll.latest)) + 2, unlist(gregexpr("kS", 
                    parsed_syll.latest)) + 2) <- "2"
                }
            }
            syll_struc0 <<- syll_struc0
            flag.restart <<- TRUE
            flag.index <<- 0
        }
        ifelse(no_z_es == TRUE, g_opt <- g_opt[!g_opt$grapheme == 
            "es", ], g_opt <- g_opt)
        ifelse(no_d_ed == TRUE, g_opt <- g_opt[!g_opt$grapheme == 
            "ed", ], g_opt <- g_opt)
        ifelse(no_r_or == TRUE, g_opt <- g_opt[!g_opt$grapheme == 
            "or", ], g_opt <- g_opt)
        ifelse(no_o_e == TRUE, g_opt <- g_opt[!g_opt$grapheme == 
            "o_e", ], g_opt <- g_opt)
        ifelse(no_3_he == TRUE, g_opt <- g_opt[!g_opt$grapheme == 
            "he", ], g_opt <- g_opt)
        ifelse(no_e_ie == TRUE, g_opt <- g_opt[!g_opt$grapheme == 
            "ie", ], g_opt <- g_opt)
        ifelse(no_e_ia_e == TRUE, g_opt <- g_opt[!g_opt$grapheme == 
            "ia_e", ], g_opt <- g_opt)
        ifelse(no_e_ea == TRUE, g_opt <- g_opt[!g_opt$grapheme == 
            "ea", ], g_opt <- g_opt)
        ifelse(no_e_ia == TRUE, g_opt <- g_opt[!g_opt$grapheme == 
            "ia", ], g_opt <- g_opt)
        ifelse(no_1_ui == TRUE, g_opt <- g_opt[!g_opt$grapheme == 
            "ui", ], g_opt <- g_opt)
        ifelse(no_m_lm == TRUE, g_opt <- g_opt[!g_opt$grapheme == 
            "lm", ], g_opt <- g_opt)
        ifelse(no_e_ui == TRUE, g_opt <- g_opt[!g_opt$grapheme == 
            "ui", ], g_opt <- g_opt)
        ifelse(no_s_tz == TRUE, g_opt <- g_opt[!g_opt$grapheme == 
            "tz", ], g_opt <- g_opt)
        ifelse(no_t_pt == TRUE, g_opt <- g_opt[!g_opt$grapheme == 
            "pt", ], g_opt <- g_opt)
        ifelse(no_a_aha == TRUE, g_opt <- g_opt[!g_opt$grapheme == 
            "aha", ], g_opt <- g_opt)
        ifelse(no_v_lv == TRUE, g_opt <- g_opt[!g_opt$grapheme == 
            "lv", ], g_opt <- g_opt)
        ifelse(no_u_wo == TRUE, g_opt <- g_opt[!g_opt$grapheme == 
            "wo", ], g_opt <- g_opt)
        ifelse(no_k_lk == TRUE, g_opt <- g_opt[!g_opt$grapheme == 
            "lk", ], g_opt <- g_opt)
        ifelse(no_E_ie == TRUE, g_opt <- g_opt[!g_opt$grapheme == 
            "ie", ], g_opt <- g_opt)
        ifelse(no_E_aye == TRUE, g_opt <- g_opt[!g_opt$grapheme == 
            "aye", ], g_opt <- g_opt)
        ifelse(no_w_hu == TRUE, g_opt <- g_opt[!g_opt$grapheme == 
            "hu", ], g_opt <- g_opt)
        ifelse(no_n_gn == TRUE, g_opt <- g_opt[!g_opt$grapheme == 
            "gn", ], g_opt <- g_opt)
        ifelse(no_aeiou_haeiou == TRUE, g_opt <- g_opt[!g_opt$grapheme %in% 
            c("ha", "he", "hi", "ho", "hu"), ], g_opt <- g_opt)
        ifelse(no_j_hi == TRUE, g_opt <- g_opt[!g_opt$grapheme == 
            "hi", ], g_opt <- g_opt)
        ifelse(map_x, g_opt <- syllable_medial_mappings[syllable_medial_mappings$grapheme == 
            "x", ], g_opt <- g_opt)
        ifelse(map_xe, g_opt <- syllable_medial_mappings[syllable_medial_mappings$grapheme == 
            "x", ], g_opt <- g_opt)
        n_opt <- length(g_opt$grapheme)
        search_lists <- list()
        for (j in (1:n_opt)) {
            search_lists[[j]] <- matrix(NA, nrow = 1, ncol = 2)
            search_lists[[j]][1] <- max(unlist(gregexpr(g_opt$grapheme[j], 
                letter_str.latest))) + nchar(g_opt$grapheme[j]) - 
                1
            ifelse(gregexpr(g_opt$grapheme[j], letter_str.latest)[[1]][1] == 
                -1, search_lists[[j]][1] <- -1, search_lists[[j]][1] <- search_lists[[j]][1])
            search_lists[[j]][2] <- nchar(g_opt$grapheme[j])
        }
        select_g <- t(matrix(unlist(search_lists), ncol = n_opt, 
            nrow = 2))
        g_m <- which(select_g[, 1] == max(select_g[, 1]))[which.max(select_g[which(select_g[, 
            1] == max(select_g[, 1])), 2])]
        g_m <- g_opt$grapheme[g_m]
        true_syllabic_position <- "3"
        if (check_for_enye == TRUE & g_m == "ñ" & p_m == "nj") 
            true_syllabic_position <- as.character(stri_sub(syll_struc0, 
                unlist(gregexpr("nj", parsed_syll.latest)), unlist(gregexpr("nj", 
                  parsed_syll.latest))))
        if (check_for_gn == TRUE & g_m == "gn" & p_m == "nj") 
            true_syllabic_position <- as.character(stri_sub(syll_struc0, 
                unlist(gregexpr("nj", parsed_syll.latest)), unlist(gregexpr("nj", 
                  parsed_syll.latest))))
        ifelse(max(select_g[, 1]) < 1, g_m <- "FAILED", g_m <- g_m)
        p_m_jointpost <- stri_sub(parsed_syll0, -my.index, -my.index + 
            1)
        map_j = FALSE
        ifelse(p_m_jointpost == "ju" & g_m == "FAILED", map_j <- TRUE, 
            map_j <- map_j)
        ifelse(p_m_jointpost == "je" & g_m == "FAILED", map_j <- TRUE, 
            map_j <- map_j)
        ifelse(p_m_jointpost == "jU" & g_m == "FAILED", map_j <- TRUE, 
            map_j <- map_j)
        ifelse(p_m_jointpost == "j3" & g_m == "FAILED", map_j <- TRUE, 
            map_j <- map_j)
        if (p_m_jointprior == "nj" & g_m == "FAILED") {
            map_j <- TRUE
        }
        else {
            map_j <- map_j
        }
        if (map_j == TRUE) {
            get_mapped <- unlist(strsplit(syll_struc0, split = syll_struc.latest, 
                fixed = TRUE))[2]
            if ("n" %in% c(stri_sub(letter_str.latest, -1, -1), 
                stri_sub(letter_str.latest, -3, -3)) & !p_m_jointpost %in% 
                c("je", "j3", "ju") || "ñ" %in% c(stri_sub(letter_str.latest, 
                -1, -1), stri_sub(letter_str.latest, -3, -3)) & 
                p_m_prior == "n") {
                flag.nj <<- TRUE
                get_tomap_position <- stri_sub(syll_struc0, -my.index - 
                  1, -my.index - 1)
                stri_sub(syll_struc.latest, -1, -1) <- get_tomap_position
                syll_struc0 <<- paste0(syll_struc.latest, get_mapped)
            }
            else {
                get_mapped_position <- stri_sub(syll_struc0, 
                  -my.index + 1, -my.index + 1)
                if (get_mapped_position == 5) {
                  get_mapped_position <- 3
                }
                stri_sub(syll_struc.latest, -1, -1) <- get_mapped_position
                syll_struc0 <<- paste0(syll_struc.latest, get_mapped)
            }
        }
        if (map_j == TRUE) {
            map_j <<- TRUE
            if (flag.nj == TRUE) {
                flag.index_j <<- my.index
            }
            else {
                flag.index_j <<- my.index - 1
            }
            flag.restart <<- TRUE
        }
        letter_str.latest <- stri_reverse(str_replace(stri_reverse(letter_str.latest), 
            stri_reverse(g_m), "_"))
        if (str_sub(letter_str.latest, -2, -2) == "_") {
            if (str_sub(letter_str.latest, -1, -1) != "e") {
                flag.restart <<- TRUE
                if (p_m == "j") {
                  flag.index_j <<- my.index - 1
                  map_j <<- TRUE
                }
                else {
                  flag.index <<- my.index - 1
                }
            }
        }
        if (tail(unlist(gregexpr("_", letter_str.latest)), 1) == 
            nchar(letter_str.latest) - 1 | tail(unlist(gregexpr("_", 
            letter_str.latest)), 1) == nchar(letter_str.latest) | 
            tail(unlist(gregexpr("_", letter_str.latest)), 1) == 
                -1) {
            flag.restart <- flag.restart
        }
        else {
            flag.restart <<- TRUE
            if (p_m_jointprior == "nj" & p_m_jointpost != "ju") {
                flag.index_j <<- my.index
                map_j <<- TRUE
                flag.nj <<- TRUE
                stri_sub(syll_struc0, -flag.index_j, -flag.index_j) <- "2"
                syll_struc0 <<- syll_struc0
            }
            else {
                if (p_m == "j") {
                  flag.index_j <<- my.index - 1
                  map_j <<- TRUE
                }
                flag.index <<- my.index - 1
            }
        }
        if (p_m_jointprior == "nj" & p_m_jointpost != "ju" & 
            stri_sub(letter_str.latest, -1, -1) == "ñ") {
            flag.restart <<- TRUE
            flag.index_j <<- my.index
            map_j <<- TRUE
            flag.nj <<- TRUE
            stri_sub(syll_struc0, -flag.index_j, -flag.index_j) <- "2"
            syll_struc0 <<- syll_struc0
        }
        ifelse(str_sub(letter_str.latest, -2, -1) == "_e", letter_str.latest <- letter_str.latest, 
            letter_str.latest <- str_sub(letter_str.latest, end = -2))
        letter_str.latest <- gsub("__", "_", letter_str.latest)
        ifelse(map_x, p_m <- p_m_jointprior, p_m <- p_m)
        ifelse(map_xe, p_m <- p_m_jointprior, p_m <- p_m)
        parsed_syll.latest <- str_sub(parsed_syll.latest, 1, 
            -(nchar(p_m) + 1))
        syll_struc.latest <- str_sub(syll_struc.latest, 1, -(nchar(p_m) + 
            1))
        PGlist <- list()
        PGlist <- append(PGlist, matrix(c(p_m, g_m, true_syllabic_position, 
            syll_struc.latest, parsed_syll.latest, letter_str.latest)))
        return(PGlist)
        print(PGlist)
    }
    map_si <- function(p_si, syll_struc.latest, parsed_syll.latest, 
        letter_str.latest) {
        if (map_j == TRUE & flag.nj == TRUE) {
            if (str_sub(parsed_syll.latest, -2, -1) == "nj") {
                flag.nj <<- FALSE
                map_j <<- FALSE
                p_si <- str_sub(parsed_syll.latest, -2, -1)
            }
        }
        p_si_prior <- str_sub(parsed_syll.latest, -(nchar(p_si) + 
            1), -(nchar(p_si) + 1))
        no_z_es <- FALSE
        no_t_bt <- FALSE
        no_t_ct <- FALSE
        no_e_ia <- FALSE
        no_e_he <- FALSE
        no_m_lm <- FALSE
        no_m_thm <- FALSE
        no_s_tz <- FALSE
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
        ifelse(p_si_prior %in% vowels & p_si == "e", no_e_ia <- TRUE, 
            no_e_ia <- no_e_ia)
        ifelse(p_si_prior %in% vowels & p_si == "e" & stri_sub(letter_str.latest, 
            -3, -3) == "i", no_e_ia <- FALSE, no_e_ia <- no_e_ia)
        ifelse(p_si_prior == "b" & p_si == "t", no_t_bt <- TRUE, 
            no_t_bt <- no_t_bt)
        ifelse(p_si_prior == "k" & p_si == "t", no_t_ct <- TRUE, 
            no_t_ct <- no_t_ct)
        ifelse(p_si_prior == "l" & p_si == "m", no_m_lm <- TRUE, 
            no_m_lm <- no_m_lm)
        ifelse(p_si_prior == "T" & p_si == "m" | p_si_prior == 
            "D" & p_si == "m", no_m_thm <- TRUE, no_m_thm <- no_m_thm)
        ifelse(p_si_prior == "t" & p_si == "s", no_s_tz <- TRUE, 
            no_s_tz <- no_s_tz)
        ifelse(p_si_prior == "g" & p_si == "n", no_n_gn <- TRUE, 
            no_n_gn <- no_n_gn)
        ifelse(p_si_prior == "k" & p_si == "n", no_n_kn <- TRUE, 
            no_n_kn <- no_n_kn)
        ifelse(p_si_prior == "n" & p_si == "n", no_n_nn <- TRUE, 
            no_n_nn <- no_n_nn)
        ifelse(p_si_prior == "d" & p_si == "d", no_d_dd <- TRUE, 
            no_d_dd <- no_d_dd)
        ifelse(p_si_prior == "t" & p_si == "t", no_t_tt <- TRUE, 
            no_t_tt <- no_t_tt)
        ifelse(p_si_prior == "l" & p_si == "k", no_k_lk <- TRUE, 
            no_k_lk <- no_k_lk)
        ifelse(p_si_prior == "k" & p_si == "k", no_k_kk <- TRUE, 
            no_k_kk <- no_k_kk)
        ifelse(p_si_prior == "i" & p_si == "j", no_j_ill <- TRUE, 
            no_j_ill <- no_j_ill)
        ifelse(p_si_prior == "l" & p_si == "j", no_j_ll <- TRUE, 
            no_j_ll <- no_j_ll)
        ifelse(p_si_prior == "s" & p_si == "l" | p_si_prior == 
            "z" & p_si == "l", no_l_sl <- TRUE, no_l_sl <- no_l_sl)
        ifelse(p_si_prior == "r" & p_si == "r", no_r_rr <- TRUE, 
            no_r_rr <- no_r_rr)
        ifelse(p_si_prior == "t" & p_si == "C", no_C_tch <- TRUE, 
            no_C_tch <- no_C_tch)
        ifelse(p_si_prior == "t" & p_si == "T", no_T_tth <- TRUE, 
            no_T_tth <- no_T_tth)
        ifelse(p_si_prior == "p" & p_si == "b", no_b_pb <- TRUE, 
            no_b_pb <- no_b_pb)
        ifelse(p_si_prior == "8" & p_si == "1" | p_si_prior == 
            "5" & p_si == "1", no_1_hi <- TRUE, no_1_hi <- no_1_hi)
        ifelse(p_si_prior == "z" & p_si == "s", no_s_ss <- TRUE, 
            no_s_ss <- no_s_ss)
        ifelse(p_si_prior == "l" & p_si == "l", no_l_ll <- TRUE, 
            no_l_ll <- no_l_ll)
        ifelse(p_si_prior == "m" & p_si == "m", no_m_mm <- TRUE, 
            no_m_mm <- no_m_mm)
        ifelse(p_si_prior == "k" & p_si == "s", no_s_cs <- TRUE, 
            no_s_cs <- no_s_cs)
        ifelse(p_si_prior != "i" & p_si == "e", no_e_he <- TRUE, 
            no_e_he <- no_e_he)
        no_w_ju <- FALSE
        ifelse(p_si_prior == "h" & p_si == "w", no_w_ju <- TRUE, 
            no_w_ju <- no_w_ju)
        p_si_jointprior <- str_sub(parsed_syll.latest, -2, -1)
        si_x <- str_sub(letter_str.latest, -1, -1)
        if (map_x == FALSE) {
            if (si_x == "x" | si_x == "xh") {
                if (p_si_jointprior == "ks" | p_si_jointprior == 
                  "gz" | p_si_jointprior == "kS") {
                  map_x <<- TRUE
                  flag.index <<- my.index
                }
            }
        }
        si_xe <- str_sub(letter_str.latest, -2, -1)
        if (map_xe == FALSE) {
            if (si_xe == "xe") {
                if (p_si_jointprior == "ks" | p_si_jointprior == 
                  "gz" | p_si_jointprior == "kS") {
                  map_xe <<- TRUE
                  flag.index <<- my.index
                }
            }
        }
        si_xi <- str_sub(letter_str.latest, -2, -1)
        if (map_xi == FALSE) {
            if (si_xi == "xi") {
                if (p_si_jointprior == "ks" | p_si_jointprior == 
                  "gz" | p_si_jointprior == "kS") {
                  map_xi <<- TRUE
                  flag.index <<- my.index
                }
            }
        }
        if (flag.index_j == my.index & map_j == TRUE & p_si_jointprior == 
            "ju" | flag.index_j == my.index & map_j == TRUE & 
            p_si_jointprior == "jU" | flag.index_j == my.index & 
            map_j == TRUE & p_si_jointprior == "j3" | flag.index_j == 
            my.index & map_j == TRUE & p_si_jointprior == "je") 
            p_si <- p_si_jointprior
        g_opt <- syllable_initial_mappings[syllable_initial_mappings$phoneme == 
            p_si, ]
        if (nrow(g_opt) == 0) {
            g_opt <- syllable_initial_mappings[syllable_initial_mappings$phoneme == 
                "s", ][1, ]
            g_opt[1, 1] <- "6"
            g_opt[1, 2] <- "6"
        }
        ifelse(no_z_es == TRUE, g_opt <- g_opt[!g_opt$grapheme == 
            "es", ], g_opt <- g_opt)
        ifelse(no_e_ia == TRUE, g_opt <- g_opt[!g_opt$grapheme == 
            "ia", ], g_opt <- g_opt)
        if (no_t_ct == TRUE) {
            g_opt <- g_opt[!g_opt$grapheme == "ct", ]
        }
        if (no_t_bt == TRUE) {
            g_opt <- g_opt[!g_opt$grapheme == "bt", ]
        }
        if (no_m_lm == TRUE) {
            g_opt <- g_opt[!g_opt$grapheme == "lm", ]
        }
        if (no_m_thm == TRUE) {
            g_opt <- g_opt[!g_opt$grapheme == "thm", ]
        }
        if (no_s_tz == TRUE) {
            g_opt <- g_opt[!g_opt$grapheme == "tz", ]
        }
        if (no_n_gn == TRUE) {
            g_opt <- g_opt[!g_opt$grapheme == "gn", ]
        }
        if (no_n_kn == TRUE) {
            g_opt <- g_opt[!g_opt$grapheme == "kn", ]
        }
        if (no_n_nn == TRUE) {
            g_opt <- g_opt[!g_opt$grapheme == "nn", ]
        }
        if (no_d_dd == TRUE) {
            g_opt <- g_opt[!g_opt$grapheme == "dd", ]
        }
        if (no_t_tt == TRUE) {
            g_opt <- g_opt[!g_opt$grapheme == "tt", ]
        }
        if (no_k_lk == TRUE) {
            g_opt <- g_opt[!g_opt$grapheme == "lk", ]
        }
        if (no_k_kk == TRUE) {
            g_opt <- g_opt[!g_opt$grapheme == "kk", ]
        }
        if (no_j_ill == TRUE) {
            g_opt <- g_opt[!g_opt$grapheme == "ill", ]
        }
        if (no_j_ll == TRUE) {
            g_opt <- g_opt[!g_opt$grapheme == "ll", ]
        }
        if (no_l_sl == TRUE) {
            g_opt <- g_opt[!g_opt$grapheme == "sl", ]
        }
        if (no_r_rr == TRUE) {
            g_opt <- g_opt[!g_opt$grapheme == "rr", ]
        }
        if (no_C_tch == TRUE) {
            g_opt <- g_opt[!g_opt$grapheme == "tch", ]
        }
        if (no_T_tth == TRUE) {
            g_opt <- g_opt[!g_opt$grapheme == "tth", ]
        }
        if (no_b_pb == TRUE) {
            g_opt <- g_opt[!g_opt$grapheme == "pb", ]
        }
        if (no_e_he == TRUE) {
            g_opt <- g_opt[!g_opt$grapheme == "he", ]
        }
        if (no_1_hi == TRUE) {
            g_opt <- g_opt[!g_opt$grapheme == "hi", ]
        }
        if (no_s_ss == TRUE) {
            g_opt <- g_opt[!g_opt$grapheme == "ss", ]
        }
        if (no_l_ll == TRUE) {
            g_opt <- g_opt[!g_opt$grapheme == "ll", ]
        }
        if (no_m_mm == TRUE) {
            g_opt <- g_opt[!g_opt$grapheme == "mm", ]
        }
        if (no_s_cs == TRUE) {
            g_opt <- g_opt[!g_opt$grapheme == "cs", ]
        }
        if (no_w_ju == TRUE) {
            g_opt <- g_opt[!g_opt$grapheme == "ju", ]
        }
        if (flag.index == my.index) {
            if (p_si == "l") {
                g_opt <- g_opt[-which(g_opt$grapheme == "sl"), 
                  ]
            }
        }
        if (map_x == FALSE & map_xe == FALSE & map_xi == FALSE) {
            if (flag.index == my.index) {
                if (grepl("x", letter_str.latest) & grepl("gz", 
                  parsed_syll.latest)) {
                  stri_sub(syll_struc0, unlist(gregexpr("gz", 
                    parsed_syll.latest)) + 1, unlist(gregexpr("gz", 
                    parsed_syll.latest)) + 1) <- "4"
                  if (stri_sub(syll_struc0, unlist(gregexpr("gz", 
                    parsed_syll.latest)) + 2, unlist(gregexpr("gz", 
                    parsed_syll.latest)) + 2) == "3") {
                    stri_sub(syll_struc0, unlist(gregexpr("gz", 
                      parsed_syll.latest)) + 2, unlist(gregexpr("gz", 
                      parsed_syll.latest)) + 2) <- "2"
                  }
                }
                syll_struc0 <<- syll_struc0
                flag.restart <<- TRUE
                flag.index <<- 0
            }
        }
        if (map_x == FALSE & map_xe == FALSE & map_xi == FALSE) {
            if (flag.index == my.index) {
                if (grepl("x", letter_str.latest) & grepl("ks", 
                  parsed_syll.latest)) {
                  stri_sub(syll_struc0, unlist(gregexpr("ks", 
                    parsed_syll.latest)) + 1, unlist(gregexpr("ks", 
                    parsed_syll.latest)) + 1) <- "4"
                  if (stri_sub(syll_struc0, unlist(gregexpr("ks", 
                    parsed_syll.latest)) + 2, unlist(gregexpr("ks", 
                    parsed_syll.latest)) + 2) == "3") {
                    stri_sub(syll_struc0, unlist(gregexpr("ks", 
                      parsed_syll.latest)) + 2, unlist(gregexpr("ks", 
                      parsed_syll.latest)) + 2) <- "2"
                  }
                }
                syll_struc0 <<- syll_struc0
                flag.restart <<- TRUE
                flag.index <<- 0
            }
        }
        if (map_x == FALSE & map_xe == FALSE & map_xi == FALSE) {
            if (flag.index == my.index) {
                if (grepl("x", letter_str.latest) & grepl("kS", 
                  parsed_syll.latest)) {
                  stri_sub(syll_struc0, unlist(gregexpr("kS", 
                    parsed_syll.latest)) + 1, unlist(gregexpr("kS", 
                    parsed_syll.latest)) + 1) <- "4"
                  if (stri_sub(syll_struc0, unlist(gregexpr("kS", 
                    parsed_syll.latest)) + 2, unlist(gregexpr("kS", 
                    parsed_syll.latest)) + 2) == "3") {
                    stri_sub(syll_struc0, unlist(gregexpr("kS", 
                      parsed_syll.latest)) + 2, unlist(gregexpr("kS", 
                      parsed_syll.latest)) + 2) <- "2"
                  }
                }
                syll_struc0 <<- syll_struc0
                flag.restart <<- TRUE
                flag.index <<- 0
            }
        }
        if (map_x == TRUE | map_xe == TRUE | map_xi == TRUE) {
            if (flag.index == my.index) {
                get_mapped <- unlist(strsplit(syll_struc0, split = syll_struc.latest, 
                  fixed = TRUE))[2]
                ifelse(get_mapped != "5", stri_sub(get_mapped, 
                  1, 1) <- "2", get_mapped <- get_mapped)
                stri_sub(syll_struc.latest, -1, -1) <- "4"
                syll_struc0 <<- paste0(syll_struc.latest, get_mapped)
                flag.restart <<- TRUE
            }
        }
        n_opt <- length(g_opt$grapheme)
        search_lists <- list()
        for (j in (1:n_opt)) {
            search_lists[[j]] <- matrix(NA, nrow = 1, ncol = 2)
            search_lists[[j]][1] <- max(unlist(gregexpr(g_opt$grapheme[j], 
                letter_str.latest))) + nchar(g_opt$grapheme[j]) - 
                1
            ifelse(gregexpr(g_opt$grapheme[j], letter_str.latest)[[1]][1] == 
                -1, search_lists[[j]][1] <- -1, search_lists[[j]][1] <- search_lists[[j]][1])
            search_lists[[j]][2] <- nchar(g_opt$grapheme[j])
        }
        select_g <- t(matrix(unlist(search_lists), ncol = n_opt, 
            nrow = 2))
        g_si <- which(select_g[, 1] == max(select_g[, 1]))[which.max(select_g[which(select_g[, 
            1] == max(select_g[, 1])), 2])]
        g_si <- g_opt$grapheme[g_si]
        ifelse(max(select_g[, 1]) < 1, g_si <- "FAILED", g_si <- g_si)
        p_si_jointpost <- stri_sub(parsed_syll0, -my.index, -my.index + 
            1)
        if (p_si_jointpost == "ju" & g_si == "FAILED" | p_si_jointpost == 
            "jU" & g_si == "FAILED" | p_si_jointpost == "j3" & 
            g_si == "FAILED" | p_si_jointpost == "je" & g_si == 
            "FAILED") {
            map_j <<- TRUE
            flag.index_j <<- my.index - 1
            if (stri_sub(syll_struc0, nchar(syll_struc.latest) + 
                1, nchar(syll_struc.latest) + 1) != "5") {
                stri_sub(syll_struc0, nchar(syll_struc.latest) + 
                  1, nchar(syll_struc.latest) + 1) <- stri_sub(syll_struc0, 
                  nchar(syll_struc.latest), nchar(syll_struc.latest))
            }
            else {
            }
            flag.restart <<- TRUE
        }
        syll_struc0 <<- syll_struc0
        if (g_si == "FAILED") {
            if (map_j == FALSE) 
                if (p_si_jointprior == "ks" | p_si_jointprior == 
                  "kS" | p_si_jointprior == "gz") {
                  get_mapped <- unlist(strsplit(syll_struc0, 
                    split = syll_struc.latest, fixed = TRUE))[2]
                  ifelse(get_mapped != "5", stri_sub(get_mapped, 
                    1, 1) <- "2", get_mapped <- get_mapped)
                  stri_sub(syll_struc.latest, -1, -1) <- "4"
                  syll_struc0 <<- paste0(syll_struc.latest, get_mapped)
                  flag.restart <<- TRUE
                }
                else {
                  flag.index <<- my.index - 1
                  flag.restart <<- TRUE
                }
            else {
                flag.restart <<- TRUE
            }
        }
        letter_str.latest <- stri_reverse(str_replace(stri_reverse(letter_str.latest), 
            stri_reverse(g_si), "_"))
        if (str_sub(letter_str.latest, -1, -1) == "_") {
            letter_str.latest <- str_sub(letter_str.latest, end = -2)
        }
        if (tail(unlist(gregexpr("_", letter_str.latest)), n = 1) != 
            -1) {
            if (stri_sub(letter_str.latest, tail(unlist(gregexpr("_", 
                letter_str.latest)), n = 1) + 1, tail(unlist(gregexpr("_", 
                letter_str.latest)), n = 1) + 1) != "e") {
                if (p_si_jointpost %in% c("ju", "jU", "j3", "je")) {
                  flag.restart <<- TRUE
                  map_j <<- TRUE
                  flag.index_j <<- my.index - 1
                }
                else {
                  flag.restart <<- TRUE
                  flag.index <<- my.index
                }
            }
        }
        parsed_syll.latest <- str_sub(parsed_syll.latest, 1, 
            -(nchar(p_si) + 1))
        syll_struc.latest <- str_sub(syll_struc.latest, 1, -(nchar(p_si) + 
            1))
        PGlist <- list()
        PGlist <- append(PGlist, matrix(c(p_si, g_si, "2", syll_struc.latest, 
            parsed_syll.latest, letter_str.latest)))
        return(PGlist)
        print(PGlist)
    }
    map_sf <- function(p_sf, syll_struc.latest, parsed_syll.latest, 
        letter_str.latest) {
        if (str_sub(letter_str.latest, -2, -1) == "_e") {
            to_map <- "342"
            stri_sub(syll_struc0, nchar(syll_struc.latest), -(nchar(syll_struc0) - 
                nchar(syll_struc.latest) - 1)) <- to_map
            syll_struc0 <<- syll_struc0
            flag.restart <<- TRUE
        }
        p_sf_prior <- str_sub(parsed_syll.latest, -2, -2)
        no_z_es <- FALSE
        no_d_ed <- FALSE
        ifelse(p_sf_prior %in% vowels & p_sf == "z", no_z_es <- TRUE, 
            no_z_es <- no_z_es)
        ifelse(p_sf_prior %in% vowels & p_sf == "d", no_d_ed <- TRUE, 
            no_d_ed <- no_d_ed)
        no_silent_H <- FALSE
        ifelse(p_sf_prior %in% c("h", "C", "T", "S", "g", "w", 
            "k") & p_sf %in% vowels | p_sf_prior == "D" & p_sf %in% 
            vowels & str_sub(letter_str.latest, -3, -3) %in% 
            c("t"), no_silent_H <- TRUE, no_silent_H <- no_silent_H)
        no_u_hu <- FALSE
        ifelse(p_sf_prior == "j" & p_sf == "u" & stri_sub(letter_str.latest, 
            -2, -2) == "h", no_u_hu <- TRUE, no_u_hu <- no_u_hu)
        no_r_ro <- FALSE
        ifelse(p_sf_prior == "3" & p_sf == "r", no_r_ro <- TRUE, 
            no_r_ro <- no_r_ro)
        no_m_lm <- FALSE
        ifelse(p_sf_prior == "l" & p_sf == "m", no_m_lm <- TRUE, 
            no_m_lm <- no_m_lm)
        no_t_ct <- FALSE
        ifelse(p_sf_prior == "k" & p_sf == "t", no_t_ct <- TRUE, 
            no_t_ct <- no_t_ct)
        no_1_ui <- FALSE
        ifelse(p_sf_prior == "w" & p_sf == "1", no_1_ui <- TRUE, 
            no_1_ui <- no_1_ui)
        no_1_hi <- FALSE
        ifelse(p_sf_prior == "S" & p_sf == "1", no_1_hi <- TRUE, 
            no_1_hi <- no_1_hi)
        ifelse(p_sf_prior == "f" & p_sf %in% vowels & stri_sub(letter_str.latest, 
            -2, -2) == "h", no_silent_H <- TRUE, no_silent_H <- no_silent_H)
        no_e_ua <- FALSE
        ifelse(p_sf_prior == "w" & p_sf == "e", no_e_ua <- TRUE, 
            no_e_ua <- no_e_ua)
        no_ho <- FALSE
        ifelse(p_sf_prior %in% c("w", "C", "S", "k", "t"), no_ho <- TRUE, 
            no_ho <- no_ho)
        no_e_iaie <- FALSE
        if (p_sf == "e" & p_sf_prior == "S" & stri_sub(letter_str.latest, 
            -3, -3) %in% c("c", "t")) {
            no_e_iaie <- TRUE
        }
        p_sf_jointprior <- str_sub(parsed_syll.latest, -2, -1)
        sf_x <- str_sub(letter_str.latest, -1, -1)
        map_x = FALSE
        ifelse(sf_x == "x", ifelse(p_sf_jointprior == "ks" | 
            p_sf_jointprior == "kS" | p_sf_jointprior == "gz" | 
            p_sf_jointprior == "gZ", map_x <- TRUE, map_x <- FALSE), 
            map_x <- FALSE)
        sf_xe <- str_sub(letter_str.latest, -2, -1)
        map_xe = FALSE
        ifelse(sf_xe == "xe", ifelse(p_sf_jointprior == "ks" | 
            p_sf_jointprior == "kS" | p_sf_jointprior == "gz" | 
            p_sf_jointprior == "gZ", map_xe <- TRUE, map_xe <- FALSE), 
            map_xe <- FALSE)
        sf_xi <- str_sub(letter_str.latest, -2, -1)
        map_xi = FALSE
        ifelse(sf_xi == "xi", ifelse(p_sf_jointprior == "ks" | 
            p_sf_jointprior == "kS" | p_sf_jointprior == "gz" | 
            p_sf_jointprior == "gZ", map_xi <- TRUE, map_xi <- FALSE), 
            map_xi <- FALSE)
        p_sf_jointprior2 <- str_sub(parsed_syll.latest, -3, -1)
        if (p_sf_jointprior2 == "3rd") 
            no_d_ed <- TRUE
        no_ju_eu <- FALSE
        if (p_sf_jointprior2 == "iju" & p_sf == "ju" | p_sf_jointprior2 == 
            "iju" & p_sf == "u") 
            no_ju_eu <- TRUE
        if (p_sf == "u") 
            tryCatch({
                ifelse(map_j == TRUE & my.index == flag.index_j, 
                  if (str_sub(parsed_syll.latest, -2, -1) == 
                    "ju") {
                    p_sf <- str_sub(parsed_syll.latest, -2, -1)
                    map_j <- FALSE
                    flag.index_j <- 0
                  }
                  else if (str_sub(parsed_syll.latest, -3, -2) == 
                    "ju") {
                    p_sf <- str_sub(parsed_syll.latest, -3, -2)
                    map_j <- FALSE
                    flag.index_j <- 0
                  }, p_sf <- p_sf)
            }, error = function(e) NA)
        if (p_sf == "e") 
            tryCatch({
                ifelse(map_j == TRUE & my.index == flag.index_j, 
                  if (str_sub(parsed_syll.latest, -2, -1) == 
                    "je") {
                    p_sf <- str_sub(parsed_syll.latest, -2, -1)
                    map_j <- FALSE
                    flag.index_j <- 0
                  }
                  else if (str_sub(parsed_syll.latest, -3, -2) == 
                    "je") {
                    p_sf <- str_sub(parsed_syll.latest, -3, -2)
                    map_j <- FALSE
                    flag.index_j <- 0
                  }, p_sf <- p_sf)
            }, error = function(e) NA)
        if (p_sf == "U") 
            tryCatch({
                ifelse(map_j == TRUE & my.index == flag.index_j, 
                  if (str_sub(parsed_syll.latest, -2, -1) == 
                    "jU") {
                    p_sf <- str_sub(parsed_syll.latest, -2, -1)
                    map_j <- FALSE
                    flag.index_j <- 0
                  }
                  else if (str_sub(parsed_syll.latest, -3, -2) == 
                    "jU") {
                    p_sf <- str_sub(parsed_syll.latest, -3, -2)
                    map_j <- FALSE
                    flag.index_j <- 0
                  }, p_sf <- p_sf)
            }, error = function(e) NA)
        if (flag.index == my.index & flag.ts == TRUE) {
            p_sf <- "ts"
        }
        g_opt <- syllable_final_mappings[syllable_final_mappings$phoneme == 
            p_sf, ]
        if (nrow(g_opt) == 0) {
            if (p_sf_jointprior == "nj" && nrow(syllable_final_mappings[syllable_final_mappings$phoneme == 
                p_sf, ]) > 0) {
                p_sf <- "nj"
                g_opt <- syllable_final_mappings[syllable_final_mappings$phoneme == 
                  p_sf, ]
            }
            else {
                g_opt <- syllable_final_mappings[syllable_final_mappings$phoneme == 
                  "s", ][1, ]
                g_opt[1, 1] <- "6"
                g_opt[1, 2] <- "6"
            }
        }
        ifelse(no_z_es == TRUE, g_opt <- g_opt[!g_opt$grapheme == 
            "es", ], g_opt <- g_opt)
        ifelse(no_d_ed == TRUE, g_opt <- g_opt[!g_opt$grapheme == 
            "ed", ], g_opt <- g_opt)
        ifelse(no_silent_H, g_opt <- g_opt[str_sub(g_opt$grapheme, 
            1, 1) != "h", ], g_opt <- g_opt)
        ifelse(no_e_ua, g_opt <- g_opt[g_opt$grapheme != "ua", 
            ], g_opt <- g_opt)
        ifelse(no_r_ro, g_opt <- g_opt[g_opt$grapheme != "ro", 
            ], g_opt <- g_opt)
        ifelse(no_m_lm, g_opt <- g_opt[g_opt$grapheme != "lm", 
            ], g_opt <- g_opt)
        ifelse(no_t_ct, g_opt <- g_opt[g_opt$grapheme != "ct", 
            ], g_opt <- g_opt)
        ifelse(no_1_ui, g_opt <- g_opt[g_opt$grapheme != "ui", 
            ], g_opt <- g_opt)
        ifelse(no_1_hi, g_opt <- g_opt[g_opt$grapheme != "hi", 
            ], g_opt <- g_opt)
        ifelse(no_ju_eu, g_opt <- g_opt[g_opt$grapheme != "eu", 
            ], g_opt <- g_opt)
        ifelse(no_ho, g_opt <- g_opt[g_opt$grapheme != "ho", 
            ], g_opt <- g_opt)
        ifelse(no_u_hu, g_opt <- g_opt[g_opt$grapheme != "hu", 
            ], g_opt <- g_opt)
        ifelse(no_e_iaie, g_opt <- g_opt[!g_opt$grapheme %in% 
            c("ie", "ia"), ], g_opt <- g_opt)
        ifelse(map_x, g_opt <- syllable_final_mappings[syllable_final_mappings$grapheme == 
            "x", ], g_opt <- g_opt)
        if (map_x == TRUE & p_sf_jointprior == "ks" | map_x == 
            TRUE & p_sf_jointprior == "kS" | map_x == TRUE & 
            p_sf_jointprior == "gz" | map_x == TRUE & p_sf_jointprior == 
            "gZ") {
            p_sf <- p_sf_jointprior
        }
        else {
            p_sf <- p_sf
        }
        ifelse(map_xe, g_opt <- syllable_final_mappings[syllable_final_mappings$grapheme == 
            "x", ], g_opt <- g_opt)
        if (map_xe == TRUE & p_sf_jointprior == "ks" | map_xe == 
            TRUE & p_sf_jointprior == "kS" | map_xe == TRUE & 
            p_sf_jointprior == "gz" | map_xe == TRUE & p_sf_jointprior == 
            "gZ") {
            p_sf <- p_sf_jointprior
        }
        else {
            p_sf <- p_sf
        }
        ifelse(map_xi, g_opt <- syllable_final_mappings[syllable_final_mappings$grapheme == 
            "xi", ], g_opt <- g_opt)
        if (map_xi == TRUE & p_sf_jointprior == "ks" | map_xi == 
            TRUE & p_sf_jointprior == "kS" | map_xi == TRUE & 
            p_sf_jointprior == "gz" | map_xi == TRUE & p_sf_jointprior == 
            "gZ") {
            p_sf <- p_sf_jointprior
        }
        else {
            p_sf <- p_sf
        }
        if (map_x != TRUE & map_xe != TRUE & map_xi != TRUE) {
            if (flag.index == my.index) {
                if (grepl("x", letter_str.latest) & grepl("gz", 
                  parsed_syll.latest)) {
                  stri_sub(syll_struc0, unlist(gregexpr("gz", 
                    parsed_syll.latest)) + 1, unlist(gregexpr("gz", 
                    parsed_syll.latest)) + 1) <- "4"
                  if (stri_sub(syll_struc0, unlist(gregexpr("gz", 
                    parsed_syll.latest)) + 2, unlist(gregexpr("gz", 
                    parsed_syll.latest)) + 2) == "4") {
                    stri_sub(syll_struc0, unlist(gregexpr("gz", 
                      parsed_syll.latest)) + 2, unlist(gregexpr("gz", 
                      parsed_syll.latest)) + 2) <- "2"
                  }
                  if (stri_sub(syll_struc0, unlist(gregexpr("gz", 
                    parsed_syll.latest)) + 3, unlist(gregexpr("gz", 
                    parsed_syll.latest)) + 3) == "3") {
                    stri_sub(syll_struc0, unlist(gregexpr("gz", 
                      parsed_syll.latest)) + 3, unlist(gregexpr("gz", 
                      parsed_syll.latest)) + 3) <- "2"
                  }
                  syll_struc0 <<- syll_struc0
                  flag.restart <<- TRUE
                  flag.index <<- 0
                }
            }
        }
        if (map_x != TRUE & map_xe != TRUE & map_xi != TRUE) {
            if (flag.index == my.index) {
                if (grepl("x", letter_str.latest) & grepl("gZ", 
                  parsed_syll.latest)) {
                  stri_sub(syll_struc0, unlist(gregexpr("gZ", 
                    parsed_syll.latest)) + 1, unlist(gregexpr("gZ", 
                    parsed_syll.latest)) + 1) <- "4"
                  if (stri_sub(syll_struc0, unlist(gregexpr("gZ", 
                    parsed_syll.latest)) + 2, unlist(gregexpr("gZ", 
                    parsed_syll.latest)) + 2) == "4") {
                    stri_sub(syll_struc0, unlist(gregexpr("gZ", 
                      parsed_syll.latest)) + 2, unlist(gregexpr("gZ", 
                      parsed_syll.latest)) + 2) <- "2"
                  }
                  if (stri_sub(syll_struc0, unlist(gregexpr("gZ", 
                    parsed_syll.latest)) + 3, unlist(gregexpr("gZ", 
                    parsed_syll.latest)) + 3) == "3") {
                    stri_sub(syll_struc0, unlist(gregexpr("gZ", 
                      parsed_syll.latest)) + 3, unlist(gregexpr("gZ", 
                      parsed_syll.latest)) + 3) <- "2"
                  }
                  syll_struc0 <<- syll_struc0
                  flag.restart <<- TRUE
                  flag.index <<- 0
                }
            }
        }
        if (map_x != TRUE & map_xe != TRUE & map_xi != TRUE) {
            if (flag.index == my.index) {
                if (grepl("x", letter_str.latest) & grepl("kS", 
                  parsed_syll.latest)) {
                  stri_sub(syll_struc0, unlist(gregexpr("kS", 
                    parsed_syll.latest)) + 1, unlist(gregexpr("kS", 
                    parsed_syll.latest)) + 1) <- "4"
                  if (stri_sub(syll_struc0, unlist(gregexpr("kS", 
                    parsed_syll.latest)) + 2, unlist(gregexpr("kS", 
                    parsed_syll.latest)) + 2) == "4") {
                    stri_sub(syll_struc0, unlist(gregexpr("kS", 
                      parsed_syll.latest)) + 2, unlist(gregexpr("kS", 
                      parsed_syll.latest)) + 2) <- "2"
                  }
                  if (stri_sub(syll_struc0, unlist(gregexpr("kS", 
                    parsed_syll.latest)) + 3, unlist(gregexpr("kS", 
                    parsed_syll.latest)) + 3) == "3") {
                    stri_sub(syll_struc0, unlist(gregexpr("kS", 
                      parsed_syll.latest)) + 3, unlist(gregexpr("kS", 
                      parsed_syll.latest)) + 3) <- "2"
                  }
                  syll_struc0 <<- syll_struc0
                  flag.restart <<- TRUE
                  flag.index <<- 0
                }
            }
        }
        if (map_x != TRUE & map_xe != TRUE & map_xi != TRUE) {
            if (flag.index == my.index) {
                if (grepl("x", letter_str.latest) & grepl("ks", 
                  parsed_syll.latest)) {
                  stri_sub(syll_struc0, unlist(gregexpr("ks", 
                    parsed_syll.latest)) + 1, unlist(gregexpr("ks", 
                    parsed_syll.latest)) + 1) <- "4"
                  if (stri_sub(syll_struc0, unlist(gregexpr("ks", 
                    parsed_syll.latest)) + 2, unlist(gregexpr("ks", 
                    parsed_syll.latest)) + 2) == "4") {
                    stri_sub(syll_struc0, unlist(gregexpr("ks", 
                      parsed_syll.latest)) + 2, unlist(gregexpr("ks", 
                      parsed_syll.latest)) + 2) <- "2"
                  }
                  if (stri_sub(syll_struc0, unlist(gregexpr("ks", 
                    parsed_syll.latest)) + 3, unlist(gregexpr("ks", 
                    parsed_syll.latest)) + 3) == "3") {
                    stri_sub(syll_struc0, unlist(gregexpr("ks", 
                      parsed_syll.latest)) + 3, unlist(gregexpr("ks", 
                      parsed_syll.latest)) + 3) <- "2"
                  }
                  syll_struc0 <<- syll_struc0
                  flag.restart <<- TRUE
                  flag.index <<- 0
                }
            }
        }
        if (flag.index == my.index & p_sf == "3") {
            g_opt <- g_opt[!stri_sub(g_opt$grapheme, 1, 1) == 
                "h", ]
        }
        if (flag.index == my.index & p_sf == "e") {
            g_opt <- g_opt[!stri_sub(g_opt$grapheme, 1, 1) == 
                "h", ]
        }
        n_opt <- length(g_opt$grapheme)
        search_lists <- list()
        for (j in (1:n_opt)) {
            search_lists[[j]] <- matrix(NA, nrow = 1, ncol = 2)
            search_lists[[j]][1] <- max(unlist(gregexpr(g_opt$grapheme[j], 
                letter_str.latest))) + nchar(g_opt$grapheme[j]) - 
                1
            ifelse(gregexpr(g_opt$grapheme[j], letter_str.latest)[[1]][1] == 
                -1, search_lists[[j]][1] <- -1, search_lists[[j]][1] <- search_lists[[j]][1])
            search_lists[[j]][2] <- nchar(g_opt$grapheme[j])
        }
        select_g <- t(matrix(unlist(search_lists), ncol = n_opt, 
            nrow = 2))
        g_sf <- which(select_g[, 1] == max(select_g[, 1]))[which.max(select_g[which(select_g[, 
            1] == max(select_g[, 1])), 2])]
        g_sf <- g_opt$grapheme[g_sf]
        ifelse(max(select_g[, 1]) < 1, g_sf <- "FAILED", g_sf <- g_sf)
        if (g_sf == "FAILED" & p_sf_jointprior == "nj" && nrow(syllable_final_mappings[syllable_final_mappings$phoneme == 
            p_sf, ]) > 0) {
            p_sf <- p_sf_jointprior
            g_opt <- syllable_final_mappings[syllable_final_mappings$phoneme == 
                p_sf, ]
            n_opt <- length(g_opt$grapheme)
            search_lists <- list()
            for (j in (1:n_opt)) {
                search_lists[[j]] <- matrix(NA, nrow = 1, ncol = 2)
                search_lists[[j]][1] <- max(unlist(gregexpr(g_opt$grapheme[j], 
                  letter_str.latest))) + nchar(g_opt$grapheme[j]) - 
                  1
                ifelse(gregexpr(g_opt$grapheme[j], letter_str.latest)[[1]][1] == 
                  -1, search_lists[[j]][1] <- -1, search_lists[[j]][1] <- search_lists[[j]][1])
                search_lists[[j]][2] <- nchar(g_opt$grapheme[j])
            }
            select_g <- t(matrix(unlist(search_lists), ncol = n_opt, 
                nrow = 2))
            g_sf <- which(select_g[, 1] == max(select_g[, 1]))[which.max(select_g[which(select_g[, 
                1] == max(select_g[, 1])), 2])]
            g_sf <- g_opt$grapheme[g_sf]
            ifelse(max(select_g[, 1]) < 1, g_sf <- "FAILED", 
                g_sf <- g_sf)
        }
        if (g_sf == "FAILED") {
            if (paste0(p_sf, PGlist[[length(PGlist) - 5]]) == 
                "ts") {
                stri_sub(syll_struc0, nchar(syll_struc.latest) + 
                  1, nchar(syll_struc.latest) + 1) <- "4"
                syll_struc0 <<- syll_struc0
                flag.index <<- my.index - 1
                flag.ts <<- TRUE
                flag.restart <<- TRUE
            }
            else {
                flag.index <<- my.index - 1
                flag.restart <<- TRUE
            }
        }
        letter_str.latest <- stri_reverse(str_replace(stri_reverse(letter_str.latest), 
            stri_reverse(g_sf), "_"))
        ifelse(str_sub(letter_str.latest, -1, -1) == "e", letter_str.latest <- letter_str.latest, 
            letter_str.latest <- str_sub(letter_str.latest, end = -2))
        parsed_syll.latest <- str_sub(parsed_syll.latest, 1, 
            -(nchar(p_sf) + 1))
        ifelse(map_x, str_sub(syll_struc.latest, -2, -1) <- "44", 
            syll_struc.latest <- syll_struc.latest)
        ifelse(map_xe, str_sub(syll_struc.latest, -2, -1) <- "44", 
            syll_struc.latest <- syll_struc.latest)
        syll_struc.latest <- str_sub(syll_struc.latest, 1, -(nchar(p_sf) + 
            1))
        PGlist <- list()
        PGlist <- append(PGlist, matrix(c(p_sf, g_sf, "4", syll_struc.latest, 
            parsed_syll.latest, letter_str.latest)))
        return(PGlist)
        print(PGlist)
    }
    map_wi <- function(p_wi, syll_struc.latest, parsed_syll.latest, 
        letter_str.latest) {
        tryCatch({
            ifelse(map_j_wi == TRUE, p_wi <- str_sub(parsed_syll0, 
                1, 2), p_wi <- p_wi)
        }, error = function(e) NA)
        g_opt <- word_initial_mappings[word_initial_mappings$phoneme == 
            p_wi, ]
        if (nrow(g_opt) == 0) {
            g_opt <- word_initial_mappings[word_initial_mappings$phoneme == 
                "s", ][1, ]
            g_opt[1, 1] <- "6"
            g_opt[1, 2] <- "6"
        }
        n_opt <- length(g_opt$grapheme)
        search_lists <- list()
        for (j in (1:n_opt)) {
            search_lists[[j]] <- matrix(NA, nrow = 1, ncol = 2)
            search_lists[[j]][1] <- max(unlist(gregexpr(g_opt$grapheme[j], 
                letter_str.latest))) + nchar(g_opt$grapheme[j]) - 
                1
            ifelse(gregexpr(g_opt$grapheme[j], letter_str.latest)[[1]][1] == 
                -1, search_lists[[j]][1] <- -1, search_lists[[j]][1] <- search_lists[[j]][1])
            search_lists[[j]][2] <- nchar(g_opt$grapheme[j])
        }
        select_g <- t(matrix(unlist(search_lists), ncol = n_opt, 
            nrow = 2))
        g_wi <- which(select_g[, 1] == max(select_g[, 1]))[which.max(select_g[which(select_g[, 
            1] == max(select_g[, 1])), 2])]
        g_wi <- g_opt$grapheme[g_wi]
        ifelse(max(select_g[, 1]) < 1, g_wi <- "FAILED", g_wi <- g_wi)
        map_j_wi = FALSE
        p_wi_jointpost <- paste0(p_wi, stri_sub(parsed_syll0, 
            -(my.index - 1), -(my.index - 1)))
        ifelse(p_wi_jointpost == "ju" & g_wi == "FAILED" | p_wi_jointpost == 
            "je" & g_wi == "FAILED" | p_wi_jointpost == "jU" & 
            g_wi == "FAILED", map_j_wi <- TRUE, map_j_wi <- FALSE)
        if (map_j_wi == TRUE) {
            flag.restart <<- TRUE
            map_j_wi <<- map_j_wi
        }
        else {
            flag.restart <<- FALSE
        }
        letter_str.latest <- stri_reverse(str_replace(stri_reverse(letter_str.latest), 
            stri_reverse(g_wi), "_"))
        ifelse(str_sub(letter_str.latest, -1, -1) == "e", letter_str.latest <- letter_str.latest, 
            letter_str.latest <- str_sub(letter_str.latest, end = -2))
        ifelse(letter_str.latest != "", g_wi <- "FAILED", g_wi <- g_wi)
        parsed_syll.latest <- str_sub(parsed_syll.latest, 1, 
            -(nchar(p_wi) + 1))
        syll_struc.latest <- str_sub(syll_struc.latest, 1, -2)
        PGlist <- list()
        PGlist <- append(PGlist, matrix(c(p_wi, g_wi, "1", syll_struc.latest, 
            parsed_syll.latest, letter_str.latest)))
        ifelse(flag.restart == TRUE, mapped_wi <<- FALSE, mapped_wi <<- TRUE)
        return(PGlist)
        print(PGlist)
    }
    generate_custom_sequence <- function(length) {
        sequence <- c()
        count <- 1
        skip <- 0
        while (length(sequence) < length) {
            if (skip < 3) {
                sequence <- c(sequence, count)
                count <- count + 1
            }
            else {
                count <- count + 3
                skip <- -1
            }
            skip <- skip + 1
        }
        return(sequence)
    }
    count_failed <- function(x) sum(unlist(x) == "FAILED", na.rm = TRUE)
    all_words <- list()
    cleared <- matrix(NA, nrow = length(spelling))
    if (map_progress == TRUE) 
        pb = txtProgressBar(min = 0, max = length(spelling), 
            initial = 0)
    for (i in 1:length(spelling)) {
        word_index <- i
        prepped_word <- tryCatch(prep_word(word_index), error = function(e) {
            msg <- conditionMessage(e)
            if (inherits(e, "parse_fail") || grepl("^__PARSE_FAIL__:", 
                msg)) {
                pronunciation[word_index] <<- "a"
                return(prep_word(word_index))
            }
            stop(e)
        })
        syll_struc0 <- prepped_word[1, ]
        parsed_syll0 <- prepped_word[2, ]
        letter_str0 <- prepped_word[3, ]
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
        counter.FAILED <- 0
        failed_prev <- 0
        while (mapped_wi == FALSE & count.restarts < 10) {
            flag.restart <- FALSE
            if (map_j_wi == TRUE) {
                str_sub(syll_struc0, 1, 2) <- "1"
            }
            else {
                syll_struc0 <- syll_struc0
            }
            to_map <- matrix(NA, nrow = 1, ncol = 2)
            to_map[1, 1] <- str_sub(parsed_syll0, -1, -1)
            to_map[1, 2] <- str_sub(syll_struc0, -1, -1)
            if (nchar(syll_struc0) == 1) {
                p_wi <- to_map[1, 1]
                my.index <- 1
                PGlist <- matrix(map_wi(p_wi = p_wi, syll_struc.latest = syll_struc0, 
                  parsed_syll.latest = parsed_syll0, letter_str.latest = letter_str0))
            }
            else {
                if (to_map[1, 2] == 5) {
                  p_wf <- to_map[1, 1]
                  my.index <- 1
                  PGlist <- map_wf(p_wf = p_wf, syll_struc0 = syll_struc0, 
                    parsed_syll0 = parsed_syll0, letter_str0 = letter_str0)
                }
            }
            failed_now <- count_failed(PGlist)
            counter.FAILED <- counter.FAILED + max(0, failed_now - 
                failed_prev)
            failed_prev <- failed_now
            if (counter.FAILED >= 3) {
                flag.restart <- TRUE
            }
            if (PGlist[[2]] == "FAILED") {
                cleared[i] <- FALSE
                my.format <- generate_custom_sequence(nchar(syll_struc0) * 
                  3)
                all_words[[i]] <- rev(data.frame(matrix(unlist(PGlist)[my.format], 
                  nrow = 3)))
                all_words[[i]] <- all_words[[i]][, !is.na(all_words[[i]][1, 
                  ])]
                all_words[[i]] <- data.frame(all_words[[i]])
                count.restarts = 10
            }
            else {
                while (PGlist[[length(PGlist)]] != "") {
                  if (flag.restart == TRUE) {
                    count.restarts <- count.restarts + 1
                    break
                  }
                  to_map[1, 1] <- str_sub(PGlist[[length(PGlist) - 
                    1]], -1, -1)
                  to_map[1, 2] <- str_sub(PGlist[[length(PGlist) - 
                    2]], -1, -1)
                  syll_struc.latest <- PGlist[[length(PGlist) - 
                    2]]
                  parsed_syll.latest <- PGlist[[length(PGlist) - 
                    1]]
                  letter_str.latest <- PGlist[[length(PGlist)]]
                  if (to_map[1, 2] == 3) {
                    p_m <- to_map[1, 1]
                    my.index <- my.index + 1
                    PGlist <- append(PGlist, map_m(p_m = p_m, 
                      syll_struc.latest = syll_struc.latest, 
                      parsed_syll.latest = parsed_syll.latest, 
                      letter_str.latest = letter_str.latest))
                  }
                  failed_now <- count_failed(PGlist)
                  counter.FAILED <- counter.FAILED + max(0, failed_now - 
                    failed_prev)
                  failed_prev <- failed_now
                  if (counter.FAILED >= 3) {
                    flag.restart <- TRUE
                  }
                  if (flag.restart == TRUE) {
                    count.restarts <- count.restarts + 1
                    break
                  }
                  to_map[1, 1] <- str_sub(PGlist[[length(PGlist) - 
                    1]], -1, -1)
                  to_map[1, 2] <- str_sub(PGlist[[length(PGlist) - 
                    2]], -1, -1)
                  syll_struc.latest <- PGlist[[length(PGlist) - 
                    2]]
                  parsed_syll.latest <- PGlist[[length(PGlist) - 
                    1]]
                  letter_str.latest <- PGlist[[length(PGlist)]]
                  if (to_map[1, 2] == "") {
                    mapped_wi <- TRUE
                    PGlist[[length(PGlist) - 3]] <- 1
                  }
                  if (to_map[1, 2] == 2) {
                    p_si <- to_map[1, 1]
                    my.index <- my.index + 1
                    PGlist <- append(PGlist, map_si(p_si = p_si, 
                      syll_struc.latest = syll_struc.latest, 
                      parsed_syll.latest = parsed_syll.latest, 
                      letter_str.latest = letter_str.latest))
                  }
                  failed_now <- count_failed(PGlist)
                  counter.FAILED <- counter.FAILED + max(0, failed_now - 
                    failed_prev)
                  failed_prev <- failed_now
                  if (counter.FAILED >= 3) {
                    flag.restart <- TRUE
                  }
                  if (flag.restart == TRUE) {
                    count.restarts <- count.restarts + 1
                    break
                  }
                  to_map[1, 1] <- str_sub(PGlist[[length(PGlist) - 
                    1]], -1, -1)
                  to_map[1, 2] <- str_sub(PGlist[[length(PGlist) - 
                    2]], -1, -1)
                  syll_struc.latest <- PGlist[[length(PGlist) - 
                    2]]
                  parsed_syll.latest <- PGlist[[length(PGlist) - 
                    1]]
                  letter_str.latest <- PGlist[[length(PGlist)]]
                  if (to_map[1, 2] == 4) {
                    p_sf <- to_map[1, 1]
                    my.index <- my.index + 1
                    PGlist <- append(PGlist, map_sf(p_sf = p_sf, 
                      syll_struc.latest = syll_struc.latest, 
                      parsed_syll.latest = parsed_syll.latest, 
                      letter_str.latest = letter_str.latest))
                  }
                  failed_now <- count_failed(PGlist)
                  counter.FAILED <- counter.FAILED + max(0, failed_now - 
                    failed_prev)
                  failed_prev <- failed_now
                  if (counter.FAILED >= 3) {
                    flag.restart <- TRUE
                  }
                  if (flag.restart == TRUE) {
                    count.restarts <- count.restarts + 1
                    break
                  }
                  to_map[1, 1] <- str_sub(PGlist[[length(PGlist) - 
                    1]], -1, -1)
                  to_map[1, 2] <- str_sub(PGlist[[length(PGlist) - 
                    2]], -1, -1)
                  syll_struc.latest <- PGlist[[length(PGlist) - 
                    2]]
                  parsed_syll.latest <- PGlist[[length(PGlist) - 
                    1]]
                  letter_str.latest <- PGlist[[length(PGlist)]]
                  if (to_map[1, 2] == 1) {
                    p_wi <- to_map[1, 1]
                    my.index <- my.index + 1
                    PGlist <- append(PGlist, map_wi(p_wi = p_wi, 
                      syll_struc.latest = syll_struc.latest, 
                      parsed_syll.latest = parsed_syll.latest, 
                      letter_str.latest = letter_str.latest))
                  }
                  failed_now <- count_failed(PGlist)
                  counter.FAILED <- counter.FAILED + max(0, failed_now - 
                    failed_prev)
                  failed_prev <- failed_now
                  if (counter.FAILED >= 3) {
                    flag.restart <- TRUE
                  }
                  if (flag.restart == TRUE) {
                    count.restarts <- count.restarts + 1
                    break
                  }
                  if (PGlist[[length(PGlist)]] == "" && PGlist[[length(PGlist) - 
                    1]] != "") {
                    failed_now <- count_failed(PGlist) + 1
                    counter.FAILED <- counter.FAILED + max(0, 
                      failed_now - failed_prev)
                    failed_prev <- failed_now
                  }
                  if (counter.FAILED >= 3) {
                    flag.restart <- TRUE
                  }
                  if (mapped_wi == TRUE) 
                    break
                }
                my.format <- generate_custom_sequence(nchar(syll_struc0) * 
                  3)
                my.format <- my.format + 1
                my.format[1:3] <- my.format[1:3] - 1
                all_words[[i]] <- rev(data.frame(matrix(unlist(PGlist)[my.format], 
                  nrow = 3)))
                all_words[[i]] <- all_words[[i]][, !is.na(all_words[[i]][1, 
                  ])]
                all_words[[i]] <- data.frame(all_words[[i]])
                cleared[i] <- ifelse(mapped_wi == TRUE & !"FAILED" %in% 
                  (all_words[[i]][2, ]) == TRUE, TRUE, FALSE)
            }
            if (map_progress == TRUE) 
                setTxtProgressBar(pb, i)
        }
    }
    return(list(all_words, cleared))
} 


# ============================================================ 
# map_PG 
# ============================================================ 
map_PG <- function (spelling, pronunciation, map_progress = FALSE) 
{
    spelling_chr <- as.character(spelling)
    pronunciation_chr <- as.character(pronunciation)
    if (!is.null(.last_spelling) && identical(spelling_chr, .last_spelling) && 
        identical(pronunciation_chr, .last_pronunciation)) {
        return(.last_result)
    }
    out <- map_PG_uncached(spelling = spelling, pronunciation = pronunciation, 
        map_progress = map_progress)
    .last_spelling <<- spelling_chr
    .last_pronunciation <<- pronunciation_chr
    .last_result <<- out
    out
} 

# ============================================================ 
# map_ONC 
# ============================================================ 
map_ONC <- function (spelling, pronunciation, map_progress = FALSE) 
{
    library(stringi)
    vowels <- c("5", "O", "8", "je", "j3", "ju", "jU", "o", "2", 
        "@", "a", "e", "E", "3", "i", "1", "c", "u", "U", "^")
    hold <- map_PG(spelling, pronunciation, map_progress = map_progress)
    if (map_progress == TRUE) 
        pb = txtProgressBar(min = 0, max = length(spelling), 
            initial = 0)
    insertSpacesMatching <- function(originalString, matchString) {
        originalString <- gsub("\\s", "", originalString)
        spaces <- gregexpr(" ", matchString)[[1]] - 1
        for (pos in spaces) {
            originalString <- paste0(substr(originalString, 1, 
                pos), " ", substr(originalString, pos + 1, nchar(originalString)))
        }
        return(originalString)
    }
    for (i in 1:length(spelling)) {
        tryCatch({
            if (which(is.na(hold[[1]][[i]][1, ])) > 0) 
                hold[[1]][[i]] <- hold[[1]][[i]][, -which(is.na(hold[[1]][[i]][1, 
                  ]))]
        }, error = function(e) NA)
        hold[[1]][[i]][4, ] <- hold[[1]][[i]][1, ]
        hold[[1]][[i]][4, ][hold[[1]][[i]][1, ] %in% vowels] <- "V"
        hold[[1]][[i]][4, ][!hold[[1]][[i]][1, ] %in% vowels] <- "C"
        colnames(hold[[1]][[i]]) <- c(letters, LETTERS)[1:ncol(hold[[1]][[i]])]
        CVstring <- paste0(hold[[1]][[i]][4, ], collapse = "")
        posstring <- paste0(hold[[1]][[i]][3, ], collapse = "")
        colstring <- paste0(c(letters, LETTERS)[1:nchar(CVstring)], 
            collapse = "")
        split_positions1 <- gregexpr("V", CVstring)[[1]]
        CVstring <- gsub("V", " V ", CVstring)
        posstring <- insertSpacesMatching(posstring, CVstring)
        colstring <- insertSpacesMatching(colstring, CVstring)
        split_positions2 <- gregexpr("2", posstring)[[1]]
        posstring <- gsub("2", " 2", posstring)
        CVstring <- insertSpacesMatching(CVstring, posstring)
        colstring <- insertSpacesMatching(colstring, posstring)
        CVstring <- trimws(CVstring, "left")
        colstring <- trimws(colstring, "left")
        posstring <- trimws(posstring, "left")
        colstring <- as.matrix(read.table(text = colstring))
        result <- matrix(NA, ncol = length(colstring), nrow = 4)
        for (j in 1:length(colstring)) {
            cols_to_paste <- unlist(strsplit(colstring[j], ""))
            result[, j] <- apply(hold[[1]][[i]][, cols_to_paste, 
                drop = FALSE], 1, paste, collapse = "")
        }
        result <- as.data.frame(result)
        hold[[1]][[i]] <- result
        for (j in 1:ncol(hold[[1]][[i]])) {
            if (nchar(hold[[1]][[i]][4, j]) > 1) 
                if (as.numeric(substr(hold[[1]][[i]][3, j], 1, 
                  1)) < 3) {
                  hold[[1]][[i]][3, j] <- substr(hold[[1]][[i]][3, 
                    j], 1, 1)
                }
                else {
                  hold[[1]][[i]][3, j] <- substr(hold[[1]][[i]][3, 
                    j], nchar(hold[[1]][[i]][3, j]), nchar(hold[[1]][[i]][3, 
                    j]))
                }
        }
        hold[[1]][[i]] <- data.frame(hold[[1]][[i]][-4, ])
        if (map_progress == TRUE) 
            setTxtProgressBar(pb, i)
    }
    return(hold)
} 

# ============================================================ 
# map_OC 
# ============================================================ 
map_OC <- function (spelling, pronunciation, map_progress = FALSE) 
{
    library(stringi)
    vowels <- c("5", "O", "8", "je", "j3", "ju", "jU", "o", "2", 
        "@", "a", "e", "E", "3", "i", "1", "c", "u", "U", "^")
    hold <- map_PG(spelling, pronunciation, map_progress = map_progress)
    if (map_progress == TRUE) 
        pb = txtProgressBar(min = 0, max = length(spelling), 
            initial = 0)
    insertSpacesMatching <- function(originalString, matchString) {
        originalString <- gsub("\\s", "", originalString)
        spaces <- gregexpr(" ", matchString)[[1]] - 1
        for (pos in spaces) {
            originalString <- paste0(substr(originalString, 1, 
                pos), " ", substr(originalString, pos + 1, nchar(originalString)))
        }
        return(originalString)
    }
    for (i in 1:length(spelling)) {
        tryCatch({
            if (which(is.na(hold[[1]][[i]][1, ])) > 0) 
                hold[[1]][[i]] <- hold[[1]][[i]][, -which(is.na(hold[[1]][[i]][1, 
                  ]))]
        }, error = function(e) NA)
        hold[[1]][[i]][4, ] <- hold[[1]][[i]][1, ]
        hold[[1]][[i]][4, ][hold[[1]][[i]][1, ] %in% vowels] <- "V"
        hold[[1]][[i]][4, ][!hold[[1]][[i]][1, ] %in% vowels] <- "C"
        colnames(hold[[1]][[i]]) <- c(letters, LETTERS)[1:ncol(hold[[1]][[i]])]
        CVstring <- paste0(hold[[1]][[i]][4, ], collapse = "")
        posstring <- paste0(hold[[1]][[i]][3, ], collapse = "")
        colstring <- paste0(c(letters, LETTERS)[1:nchar(CVstring)], 
            collapse = "")
        split_positions1 <- gregexpr("V", CVstring)[[1]]
        CVstring <- gsub("V", "V ", CVstring)
        posstring <- insertSpacesMatching(posstring, CVstring)
        colstring <- insertSpacesMatching(colstring, CVstring)
        split_positions2 <- gregexpr("2", posstring)[[1]]
        posstring <- gsub("2", " 2", posstring)
        CVstring <- insertSpacesMatching(CVstring, posstring)
        colstring <- insertSpacesMatching(colstring, posstring)
        CVstring <- trimws(CVstring, "right")
        colstring <- trimws(colstring, "right")
        posstring <- trimws(posstring, "right")
        CVstring <- gsub("\\s+", " ", CVstring)
        colstring <- gsub("\\s+", " ", colstring)
        posstring <- gsub("\\s+", " ", posstring)
        colstring <- as.matrix(read.table(text = colstring))
        result <- matrix(NA, ncol = length(colstring), nrow = 4)
        for (j in 1:length(colstring)) {
            cols_to_paste <- unlist(strsplit(colstring[j], ""))
            result[, j] <- apply(hold[[1]][[i]][, cols_to_paste, 
                drop = FALSE], 1, paste, collapse = "")
        }
        result <- as.data.frame(result)
        hold[[1]][[i]] <- result
        for (j in 1:ncol(hold[[1]][[i]])) {
            if (nchar(hold[[1]][[i]][4, j]) > 1) {
                pos_str <- hold[[1]][[i]][3, j]
                if (is.na(pos_str) || pos_str == "") 
                  next
                first_char <- substr(pos_str, 1, 1)
                last_char <- substr(pos_str, nchar(pos_str), 
                  nchar(pos_str))
                first_num <- suppressWarnings(as.numeric(first_char))
                last_num <- suppressWarnings(as.numeric(last_char))
                if (is.na(first_num) || is.na(last_num)) 
                  next
                if (first_num == 1) {
                  hold[[1]][[i]][3, j] <- 1
                }
                else if (last_num == 5) {
                  hold[[1]][[i]][3, j] <- 5
                }
                else if (first_num < 3) {
                  hold[[1]][[i]][3, j] <- first_num
                }
                else {
                  hold[[1]][[i]][3, j] <- last_num
                }
            }
        }
        hold[[1]][[i]] <- data.frame(hold[[1]][[i]][-4, ])
        if (map_progress == TRUE) 
            setTxtProgressBar(pb, i)
    }
    return(hold)
} 

# ============================================================ 
# map_OR 
# ============================================================ 
map_OR <- function (spelling, pronunciation, map_progress = FALSE) 
{
    library(stringi)
    vowels <- c("5", "O", "8", "je", "j3", "ju", "jU", "o", "2", 
        "@", "a", "e", "E", "3", "i", "1", "c", "u", "U", "^")
    hold <- map_PG(spelling, pronunciation, map_progress = map_progress)
    if (map_progress == TRUE) 
        pb = txtProgressBar(min = 0, max = length(spelling), 
            initial = 0)
    insertSpacesMatching <- function(originalString, matchString) {
        originalString <- gsub("\\s", "", originalString)
        spaces <- gregexpr(" ", matchString)[[1]] - 1
        for (pos in spaces) {
            originalString <- paste0(substr(originalString, 1, 
                pos), " ", substr(originalString, pos + 1, nchar(originalString)))
        }
        return(originalString)
    }
    for (i in 1:length(spelling)) {
        tryCatch({
            if (which(is.na(hold[[1]][[i]][1, ])) > 0) 
                hold[[1]][[i]] <- hold[[1]][[i]][, -which(is.na(hold[[1]][[i]][1, 
                  ]))]
        }, error = function(e) NA)
        hold[[1]][[i]][4, ] <- hold[[1]][[i]][1, ]
        hold[[1]][[i]][4, ][hold[[1]][[i]][1, ] %in% vowels] <- "V"
        hold[[1]][[i]][4, ][!hold[[1]][[i]][1, ] %in% vowels] <- "C"
        colnames(hold[[1]][[i]]) <- c(letters, LETTERS)[1:ncol(hold[[1]][[i]])]
        CVstring <- paste0(hold[[1]][[i]][4, ], collapse = "")
        posstring <- paste0(hold[[1]][[i]][3, ], collapse = "")
        colstring <- paste0(c(letters, LETTERS)[1:nchar(CVstring)], 
            collapse = "")
        split_positions1 <- gregexpr("V", CVstring)[[1]]
        CVstring <- gsub("V", " V", CVstring)
        posstring <- insertSpacesMatching(posstring, CVstring)
        colstring <- insertSpacesMatching(colstring, CVstring)
        split_positions2 <- gregexpr("2", posstring)[[1]]
        posstring <- gsub("2", " 2", posstring)
        CVstring <- insertSpacesMatching(CVstring, posstring)
        colstring <- insertSpacesMatching(colstring, posstring)
        CVstring <- trimws(CVstring, "left")
        colstring <- trimws(colstring, "left")
        posstring <- trimws(posstring, "left")
        CVstring <- gsub("\\s+", " ", CVstring)
        colstring <- gsub("\\s+", " ", colstring)
        posstring <- gsub("\\s+", " ", posstring)
        colstring <- as.matrix(read.table(text = colstring))
        result <- matrix(NA, ncol = length(colstring), nrow = 4)
        for (j in 1:length(colstring)) {
            cols_to_paste <- unlist(strsplit(colstring[j], ""))
            result[, j] <- apply(hold[[1]][[i]][, cols_to_paste, 
                drop = FALSE], 1, paste, collapse = "")
        }
        result <- as.data.frame(result)
        hold[[1]][[i]] <- result
        for (j in 1:ncol(hold[[1]][[i]])) {
            if (nchar(hold[[1]][[i]][4, j]) > 1) 
                if (as.numeric(substr(hold[[1]][[i]][3, j], nchar(hold[[1]][[i]][3, 
                  j]), nchar(hold[[1]][[i]][3, j]))) == 5) {
                  hold[[1]][[i]][3, j] <- 5
                }
                else {
                  if (as.numeric(substr(hold[[1]][[i]][3, j], 
                    1, 1)) == 1) {
                    hold[[1]][[i]][3, j] <- 1
                  }
                  else {
                    if (as.numeric(substr(hold[[1]][[i]][3, j], 
                      1, 1)) < 3) {
                      hold[[1]][[i]][3, j] <- substr(hold[[1]][[i]][3, 
                        j], 1, 1)
                    }
                    else {
                      hold[[1]][[i]][3, j] <- substr(hold[[1]][[i]][3, 
                        j], nchar(hold[[1]][[i]][3, j]), nchar(hold[[1]][[i]][3, 
                        j]))
                    }
                  }
                }
        }
        hold[[1]][[i]] <- data.frame(hold[[1]][[i]][-4, ])
        if (map_progress == TRUE) 
            setTxtProgressBar(pb, i)
    }
    return(hold)
} 

# ============================================================ 
# make_tables 
# ============================================================ 
make_tables <- function (mapped_words, weight = FALSE, positional = TRUE) 
{
    library(dplyr)
    library(tidyr)
    dummy_word <- map_PG("hardy", "hardi")
    dummy_word[[1]][[1]][, 1] <- c(7, 7, 1)
    dummy_word[[1]][[1]][, 2] <- c(7, 7, 3)
    dummy_word[[1]][[1]][, 3] <- c(7, 7, 4)
    dummy_word[[1]][[1]][, 4] <- c(7, 7, 2)
    dummy_word[[1]][[1]][, 5] <- c(7, 7, 5)
    if (length(weight) > 1) {
        dummy_word[[1]][[1]][4, ] <- 1
    }
    convert_matrix_nofreq <- function(mat) {
        df <- as.data.frame(t(mat))
        colnames(df) <- c("phoneme", "grapheme", "position")
        separator <- data.frame(phoneme = "----------", grapheme = "----------", 
            position = "----------")
        return(rbind(df, separator))
    }
    convert_matrix_freq <- function(mat) {
        df <- as.data.frame(t(mat))
        colnames(df) <- c("phoneme", "grapheme", "position", 
            "weight")
        separator <- data.frame(phoneme = "----------", grapheme = "----------", 
            position = "----------", weight = "----------")
        return(rbind(df, separator))
    }
    append_freq <- function(mapped_words, weight) {
        for (i in 1:length(weight)) {
            mapped_words[[1]][[i]][4, ] <- weight[i]
        }
        return(mapped_words)
    }
    if (length(weight) > 1) {
        mapped_words <- append_freq(mapped_words, weight)
    }
    if (length(weight) == 1) {
        all_words_df <- do.call(rbind, lapply(mapped_words[[1]], 
            convert_matrix_nofreq))
        all_words_df <- rbind(all_words_df, convert_matrix_nofreq(dummy_word[[1]][[1]]))
        all_words_df <- all_words_df[1:(nrow(all_words_df) - 
            1), ]
        rownames(all_words_df) <- NULL
    }
    else {
        all_words_df <- do.call(rbind, lapply(mapped_words[[1]], 
            convert_matrix_freq))
        all_words_df <- rbind(all_words_df, convert_matrix_freq(dummy_word[[1]][[1]]))
        all_words_df <- all_words_df[1:(nrow(all_words_df) - 
            1), ]
        rownames(all_words_df) <- NULL
    }
    filtered_df <- all_words_df %>% filter(phoneme != "----------" & 
        grapheme != "----------")
    if (positional == FALSE) {
        filtered_df$position <- 1
    }
    if (length(weight) > 1) {
        phoneme_grapheme_frequency <- filtered_df %>% group_by(position, 
            phoneme, grapheme) %>% summarize(freq = sum(as.numeric(weight))) %>% 
            ungroup()
    }
    else {
        phoneme_grapheme_frequency <- filtered_df %>% group_by(position, 
            phoneme, grapheme) %>% summarize(freq = n()) %>% 
            ungroup()
    }
    result_df <- phoneme_grapheme_frequency %>% spread(key = position, 
        value = freq, fill = 0)
    if (positional == FALSE) {
        result_df$`2` <- result_df$`1`
        result_df$`3` <- result_df$`1`
        result_df$`4` <- result_df$`1`
        result_df$`5` <- result_df$`1`
    }
    pg <- result_df %>% group_by(phoneme) %>% mutate(across(`1`:`5`, 
        ~round((./sum(.)), 4), .names = "prob_{.col}")) %>% ungroup() %>% 
        transmute(phoneme = paste(phoneme), grapheme = paste(grapheme), 
            wi = prob_1, si = prob_2, sm = prob_3, sf = prob_4, 
            wf = prob_5) %>% mutate(across(c(wi, si, sm, sf, 
        wf), ~as.character(ifelse(is.na(.), "0", .))))
    gp <- result_df %>% group_by(grapheme) %>% mutate(across(`1`:`5`, 
        ~round((./sum(.)), 4), .names = "prob_{.col}")) %>% ungroup() %>% 
        transmute(phoneme = paste(phoneme), grapheme = paste(grapheme), 
            wi = prob_1, si = prob_2, sm = prob_3, sf = prob_4, 
            wf = prob_5) %>% mutate(across(c(wi, si, sm, sf, 
        wf), ~as.character(ifelse(is.na(.), "0", .))))
    result_df <- result_df %>% rename(wi = `1`, si = `2`, sm = `3`, 
        sf = `4`, wf = `5`)
    phoneme_freq <- result_df %>% group_by(phoneme) %>% summarise(wi = round(log10(sum(wi) + 
        1), 4), si = round(log10(sum(si) + 1), 4), sm = round(log10(sum(sm) + 
        1), 4), sf = round(log10(sum(sf) + 1), 4), wf = round(log10(sum(wf) + 
        1), 4)) %>% ungroup()
    grapheme_freq <- result_df %>% group_by(grapheme) %>% summarise(wi = round(log10(sum(wi) + 
        1), 4), si = round(log10(sum(si) + 1), 4), sm = round(log10(sum(sm) + 
        1), 4), sf = round(log10(sum(sf) + 1), 4), wf = round(log10(sum(wf) + 
        1), 4)) %>% ungroup()
    phoneme_grapheme_freq <- result_df %>% group_by(phoneme, 
        grapheme) %>% summarise(wi = round(log10(sum(wi) + 1), 
        4), si = round(log10(sum(si) + 1), 4), sm = round(log10(sum(sm) + 
        1), 4), sf = round(log10(sum(sf) + 1), 4), wf = round(log10(sum(wf) + 
        1), 4)) %>% ungroup()
    pg <- pg[pg$phoneme != "7", ]
    gp <- gp[gp$grapheme != "7", ]
    phoneme_freq <- phoneme_freq[phoneme_freq$phoneme != "7", 
        ]
    grapheme_freq <- grapheme_freq[grapheme_freq$grapheme != 
        "7", ]
    phoneme_grapheme_freq <- phoneme_grapheme_freq[phoneme_grapheme_freq$grapheme != 
        "7", ]
    return(list(pg = pg, gp = gp, p_freq = phoneme_freq, g_freq = grapheme_freq, 
        pg_freq = phoneme_grapheme_freq))
} 

# ============================================================ 
# parse_corpus 
# ============================================================ 
parse_corpus <- function (corpus, mapped_corpus) 
{
    library(progress)
    library(readxl)
    library(data.tree)
    required_globals <- c("word_initial_mappings", "word_final_mappings", 
        "syllable_medial_mappings", "syllable_final_mappings", 
        "syllable_initial_mappings")
    missing_globals <- required_globals[!vapply(required_globals, 
        exists, logical(1), inherits = TRUE)]
    if (length(missing_globals) > 0) {
        stop("Missing required mapping objects in the global environment: ", 
            paste(missing_globals, collapse = ", "))
    }
    internal_mappings <- rbind(syllable_initial_mappings, syllable_medial_mappings, 
        syllable_final_mappings)
    parse_corpus_word_initial_mappings <- word_initial_mappings
    parse_corpus_word_final_mappings <- word_final_mappings
    parse_corpus_word_final_mappings[nrow(parse_corpus_word_final_mappings) + 
        1, ] <- parse_corpus_word_final_mappings[nrow(parse_corpus_word_final_mappings), 
        ]
    parse_corpus_word_final_mappings[nrow(parse_corpus_word_final_mappings), 
        ]$grapheme <- "h"
    parse_corpus_word_final_mappings[nrow(parse_corpus_word_final_mappings), 
        ]$phoneme <- "h"
    parse_corpus_word_final_mappings[nrow(parse_corpus_word_final_mappings) + 
        1, ] <- parse_corpus_word_final_mappings[nrow(parse_corpus_word_final_mappings), 
        ]
    parse_corpus_word_final_mappings[nrow(parse_corpus_word_final_mappings), 
        ]$grapheme <- "w"
    parse_corpus_word_final_mappings[nrow(parse_corpus_word_final_mappings), 
        ]$phoneme <- "w"
    parse_corpus_word_initial_mappings$grapheme <- toupper(parse_corpus_word_initial_mappings$grapheme)
    internal_mappings$grapheme <- toupper(internal_mappings$grapheme)
    parse_corpus_word_final_mappings$grapheme <- toupper(parse_corpus_word_final_mappings$grapheme)
    parse_string <- function(startword) {
        consonants <- c("b", "c", "d", "f", "g", "h", "j", "k", 
            "l", "m", "n", "p", "q", "r", "s", "t", "v", "x", 
            "z")
        vowels <- c("a|e|i|o|u|y")
        remove_consonants <- c(B = "_", C = "_", D = "_", F = "_", 
            G = "_", H = "_", J = "_", K = "_", L = "_", M = "_", 
            N = "_", P = "_", Q = "_", R = "_", S = "_", T = "_", 
            V = "_", X = "_", Z = "_")
        remove_first <- function(word, char) {
            sub(char, "", word, fixed = TRUE)
        }
        process_string <- function(string, word) {
            before_underscore <- sub("_(.*)", "", string)
            remaining_word <- sub(paste0("^", before_underscore), 
                "", word)
            after_e <- sub("^.*?E", "", remaining_word)
            return(after_e)
        }
        prune_tree <- function(node) {
            children_to_check <- node$children
            for (child in children_to_check) {
                if (child$isLeaf) {
                  if (child$name == "W" | child$name == "H") {
                    node$RemoveChild(child$name)
                  }
                }
                else {
                  prune_tree(child)
                  if (length(child$children) == 0) {
                    node$RemoveChild(child$name)
                  }
                }
            }
        }
        has_unfinished_leaves <- function(tree) {
            lv <- tree$leaves
            if (length(lv) == 0L) 
                return(FALSE)
            for (ii in seq_along(lv)) {
                if (!identical(lv[[ii]]$name, "9")) 
                  return(TRUE)
            }
            FALSE
        }
        myword <- toupper(startword)
        options_1_left <- myword
        options_2_left <- myword
        options_3_left <- myword
        options_4_left <- myword
        options_1e_left <- myword
        options_2e_left <- myword
        options_ue_left <- myword
        options_base_left <- myword
        myletters <- data.frame(str_split_fixed(myword, "", max(nchar(myword))))
        skeleton <- gsub("_+", "_", str_replace_all(myword, remove_consonants))
        myskeleton <- data.frame(str_split_fixed(skeleton, "", 
            max(nchar(skeleton))))
        options_1 <- unique(subset(parse_corpus_word_initial_mappings, 
            grapheme == paste0(myletters[, 1]))$grapheme)
        if (nchar(myword) > 1) {
            options_2 <- unique(subset(parse_corpus_word_initial_mappings, 
                grapheme == paste0(myletters[, 1], myletters[, 
                  2]))$grapheme)
        }
        else {
            options_2 <- NULL
        }
        if (nchar(myword) > 2) {
            options_3 <- unique(subset(parse_corpus_word_initial_mappings, 
                grapheme == paste0(myletters[, 1], myletters[, 
                  2], myletters[, 3]))$grapheme)
        }
        else {
            options_3 <- NULL
        }
        if (nchar(myword) > 3) {
            options_4 <- unique(subset(parse_corpus_word_initial_mappings, 
                grapheme == paste0(myletters[, 1], myletters[, 
                  2], myletters[, 3], myletters[, 4]))$grapheme)
        }
        else {
            options_4 <- NULL
        }
        if (ncol(myskeleton) > 2) {
            options_1e <- unique(subset(parse_corpus_word_initial_mappings, 
                grapheme == paste0(myskeleton[, 1], myskeleton[, 
                  2], myskeleton[, 3]))$grapheme)
        }
        else {
            options_1e <- NULL
        }
        if (length(options_1e) && grepl("_", options_1e)) {
            options_1e <- options_1e
        }
        else {
            options_1e <- NULL
        }
        if (ncol(myskeleton) > 3) {
            ifelse(ncol(myskeleton) > 3, options_2e <- unique(subset(parse_corpus_word_initial_mappings, 
                grapheme == paste0(myskeleton[, 1], myskeleton[, 
                  2], myskeleton[, 3], myskeleton[, 4]))$grapheme), 
                options_2e <- character(0))
        }
        else {
            options_2e <- NULL
        }
        if (length(options_2e) && grepl("_", options_2e)) {
            options_2e <- options_2e
        }
        else {
            options_2e <- NULL
        }
        if (ncol(myskeleton) > 3) {
            ifelse(ncol(myskeleton) > 3 & myskeleton[, 3] == 
                "U", options_ue <- unique(subset(parse_corpus_word_initial_mappings, 
                grapheme == paste0(myskeleton[, 1], myskeleton[, 
                  2], myskeleton[, 4]))$grapheme), options_ue <- character(0))
        }
        else {
            options_ue <- NULL
        }
        if (length(options_ue) && grepl("_", options_ue)) {
            options_ue <- options_ue
        }
        else {
            options_ue <- NULL
        }
        if (length(options_1)) 
            options_1_left <- sub(options_1, "", myword)
        else options_1_left <- NULL
        if (length(options_2)) 
            options_2_left <- sub(options_2, "", myword)
        else options_2_left <- NULL
        if (length(options_3)) 
            options_3_left <- sub(options_3, "", myword)
        else options_3_left <- NULL
        if (length(options_4)) 
            options_4_left <- sub(options_4, "", myword)
        else options_4_left <- NULL
        if (length(options_1e)) {
            pattern_before_underscore <- strsplit(sub("_.*", 
                "", options_1e), NULL)[[1]]
            for (char in pattern_before_underscore) {
                options_1e_left <- sub(char, "", options_1e_left, 
                  fixed = TRUE)
            }
            options_1e_left <- sub("E", "_", options_1e_left)
        }
        if (!grepl(vowels, options_1e_left, ignore.case = TRUE)) {
            if (process_string(options_1e, options_base_left) == 
                "") {
                options_1e_left <- options_1e_left
            }
            else {
                options_1e_left <- options_base_left
                options_1e <- character(0)
            }
        }
        else {
            options_1e_left <- options_1e_left
        }
        if (length(options_2e)) {
            pattern_before_underscore <- strsplit(sub("_.*", 
                "", options_2e), NULL)[[1]]
            for (char in pattern_before_underscore) {
                options_2e_left <- sub(char, "", options_2e_left, 
                  fixed = TRUE)
            }
            options_2e_left <- sub("E", "_", options_2e_left)
        }
        if (!grepl(vowels, options_2e_left, ignore.case = TRUE)) {
            if (process_string(options_2e, options_base_left) == 
                "") {
                options_2e_left <- options_2e_left
            }
            else {
                options_2e_left <- options_base_left
                options_2e <- character(0)
            }
        }
        else {
            options_2e_left <- options_2e_left
        }
        if (length(options_ue)) {
            pattern_before_underscore <- strsplit(sub("_.*", 
                "", options_ue), NULL)[[1]]
            for (char in pattern_before_underscore) {
                options_ue_left <- sub(char, "", options_ue_left, 
                  fixed = TRUE)
            }
            options_ue_left <- sub("E", "_", options_ue_left)
        }
        if (!grepl(vowels, options_ue_left, ignore.case = TRUE)) {
            if (process_string(options_ue, options_base_left) == 
                "") {
                options_ue_left <- options_ue_left
            }
            else {
                options_ue_left <- options_base_left
                options_ue <- character(0)
            }
        }
        else {
            options_ue_left <- options_ue_left
        }
        mytree <- Node$new(myword)
        wi1 <- mytree$AddChild(options_1)
        if (length(options_2) > 0) {
            wi2 <- mytree$AddChild(options_2)
        }
        if (length(options_3) > 0) {
            wi3 <- mytree$AddChild(options_3)
        }
        if (length(options_4) > 0) {
            wi4 <- mytree$AddChild(options_4)
        }
        if (length(options_1e) > 0) {
            wi1e <- mytree$AddChild(options_1e)
        }
        if (length(options_2e) > 0) {
            wi2e <- mytree$AddChild(options_2e)
        }
        if (length(options_ue) > 0) {
            wiue <- mytree$AddChild(options_ue)
        }
        lefttree <- Node$new(myword)
        if (options_1_left == "") {
            wi1 <- lefttree$AddChild("9")
        }
        else {
            wi1 <- lefttree$AddChild(options_1_left)
        }
        if (length(options_2) > 0) {
            if (options_2_left == "") {
                wi2 <- lefttree$AddChild("9")
            }
            else {
                wi2 <- lefttree$AddChild(options_2_left)
            }
        }
        if (length(options_3) > 0) {
            if (options_3_left == "") {
                wi3 <- lefttree$AddChild("9")
            }
            else {
                wi3 <- lefttree$AddChild(options_3_left)
            }
        }
        if (length(options_4) > 0) {
            if (options_4_left == "") {
                wi4 <- lefttree$AddChild("9")
            }
            else {
                wi4 <- lefttree$AddChild(options_4_left)
            }
        }
        if (length(options_1e) > 0) {
            if (options_1e_left == "") {
                wi1e <- lefttree$AddChild("9")
            }
            else {
                wi1e <- lefttree$AddChild(options_1e_left)
            }
        }
        if (length(options_2e) > 0) {
            if (options_2e_left == "") {
                wi2e <- lefttree$AddChild("9")
            }
            else {
                wi2e <- lefttree$AddChild(options_2e_left)
            }
        }
        if (length(options_ue) > 0) {
            if (options_ue_left == "") {
                wiue <- lefttree$AddChild("9")
            }
            else {
                wiue <- lefttree$AddChild(options_ue_left)
            }
        }
        counter <- 1
        while (has_unfinished_leaves(lefttree)) {
            if (counter > length(mytree$leaves)) {
                counter <- 1
            }
            while (lefttree$leaves[[counter]]$name == "9") {
                counter <- counter + 1
            }
            my_leaf <- lefttree$leaves[[counter]]$name
            my_leaf <- sub("_$", "", my_leaf)
            mytree_path <- mytree$leaves[[counter]]$path[-1]
            lefttree_path <- lefttree$leaves[[counter]]$path[-1]
            if (my_leaf != "9") {
                internal_options_1_left <- my_leaf
                internal_options_2_left <- my_leaf
                internal_options_3_left <- my_leaf
                internal_options_4_left <- my_leaf
                internal_options_5_left <- my_leaf
                internal_options_1e_left <- my_leaf
                internal_options_2e_left <- my_leaf
                internal_options_ue_left <- my_leaf
                internal_options_2e2_left <- my_leaf
                internal_options_base_left <- my_leaf
                myletters_left <- data.frame(str_split_fixed(my_leaf, 
                  "", max(nchar(my_leaf))))
                skeleton_left <- gsub("_+", "_", str_replace_all(my_leaf, 
                  remove_consonants))
                myskeleton_left <- data.frame(str_split_fixed(skeleton_left, 
                  "", max(nchar(skeleton_left))))
                if (ncol(myletters_left) - as.numeric("_" %in% 
                  myletters_left) == 1) {
                  check_mappings <- parse_corpus_word_final_mappings
                }
                else {
                  check_mappings <- internal_mappings
                }
                if (length(myletters_left) > 0) {
                  internal_options_1 <- unique(subset(check_mappings, 
                    grapheme == paste0(myletters_left[, 1]))$grapheme)
                }
                else {
                  internal_options_1 <- NULL
                }
                if (ncol(myletters_left) - as.numeric("_" %in% 
                  myletters_left) == 2) {
                  check_mappings <- parse_corpus_word_final_mappings
                }
                else {
                  check_mappings <- internal_mappings
                }
                if (length(myletters_left) > 1) {
                  internal_options_2 <- unique(subset(check_mappings, 
                    grapheme == paste0(myletters_left[, 1], myletters_left[, 
                      2]))$grapheme)
                }
                else {
                  internal_options_2 <- NULL
                }
                if (ncol(myletters_left) - as.numeric("_" %in% 
                  myletters_left) == 3) {
                  check_mappings <- parse_corpus_word_final_mappings
                }
                else {
                  check_mappings <- internal_mappings
                }
                if (length(myletters_left) > 2) {
                  internal_options_3 <- unique(subset(check_mappings, 
                    grapheme == paste0(myletters_left[, 1], myletters_left[, 
                      2], myletters_left[, 3]))$grapheme)
                }
                else {
                  internal_options_3 <- NULL
                }
                if (ncol(myletters_left) - as.numeric("_" %in% 
                  myletters_left) == 4) {
                  check_mappings <- parse_corpus_word_final_mappings
                }
                else {
                  check_mappings <- internal_mappings
                }
                if (length(myletters_left) > 3) {
                  internal_options_4 <- unique(subset(check_mappings, 
                    grapheme == paste0(myletters_left[, 1], myletters_left[, 
                      2], myletters_left[, 3], myletters_left[, 
                      4]))$grapheme)
                }
                else {
                  internal_options_4 <- NULL
                }
                if (ncol(myletters_left) - as.numeric("_" %in% 
                  myletters_left) == 5) {
                  check_mappings <- parse_corpus_word_final_mappings
                }
                else {
                  check_mappings <- internal_mappings
                }
                if (length(myletters_left) > 4) {
                  internal_options_5 <- unique(subset(check_mappings, 
                    grapheme == paste0(myletters_left[, 1], myletters_left[, 
                      2], myletters_left[, 3], myletters_left[, 
                      4], myletters_left[, 5]))$grapheme)
                }
                else {
                  internal_options_5 <- NULL
                }
                my_leaf <- gsub("_", "", my_leaf)
                ifelse(ncol(myskeleton_left) > 2, internal_options_1e <- unique(subset(internal_mappings, 
                  grapheme == paste0(myskeleton_left[, 1], myskeleton_left[, 
                    2], myskeleton_left[, 3]))$grapheme), internal_options_1e <- character(0))
                if (length(internal_options_1e) && grepl("_", 
                  internal_options_1e)) {
                  internal_options_1e <- internal_options_1e
                }
                else {
                  internal_options_1e <- character(0)
                }
                ifelse(ncol(myskeleton_left) > 3, internal_options_2e <- unique(subset(internal_mappings, 
                  grapheme == paste0(myskeleton_left[, 1], myskeleton_left[, 
                    2], myskeleton_left[, 3], myskeleton_left[, 
                    4]))$grapheme), internal_options_2e <- character(0))
                if (length(internal_options_2e) && grepl("_", 
                  internal_options_2e)) {
                  internal_options_2e <- internal_options_2e
                }
                else {
                  internal_options_2e <- character(0)
                }
                ifelse(ncol(myskeleton_left) > 3, ifelse(myskeleton_left[, 
                  3] == "U", internal_options_ue <- unique(subset(internal_mappings, 
                  grapheme == paste0(myskeleton_left[, 1], myskeleton_left[, 
                    2], myskeleton_left[, 4]))$grapheme), internal_options_ue <- character(0)), 
                  internal_options_ue <- character(0))
                if (length(internal_options_ue) && grepl("_", 
                  internal_options_ue)) {
                  internal_options_ue <- internal_options_ue
                }
                else {
                  internal_options_ue <- character(0)
                }
                if (ncol(myskeleton_left) > 3) {
                  if (myskeleton_left[, 4] == "U" && ncol(myskeleton_left) > 
                    3) {
                    ifelse(ncol(myskeleton_left) > 4, internal_options_2e2 <- unique(subset(internal_mappings, 
                      grapheme == paste0(myskeleton_left[, 1], 
                        myskeleton_left[, 2], myskeleton_left[, 
                          3], myskeleton_left[, 5]))$grapheme), 
                      internal_options_2e2 <- character(0))
                    if (length(internal_options_2e2) && grepl("_", 
                      internal_options_2e2)) {
                      internal_options_2e2 <- internal_options_2e2
                    }
                    else {
                      internal_options_2e2 <- character(0)
                    }
                  }
                  else {
                    internal_options_2e2 <- character(0)
                  }
                }
                else {
                  internal_options_2e2 <- character(0)
                }
                if (length(internal_options_1)) 
                  internal_options_1_left <- sub(internal_options_1, 
                    "", my_leaf)
                else internal_options_1_left <- internal_options_1_left
                if (length(internal_options_2)) 
                  internal_options_2_left <- sub(internal_options_2, 
                    "", my_leaf)
                else internal_options_2_left <- internal_options_2_left
                if (length(internal_options_3)) 
                  internal_options_3_left <- sub(internal_options_3, 
                    "", my_leaf)
                else internal_options_3_left <- internal_options_3_left
                if (length(internal_options_4)) 
                  internal_options_4_left <- sub(internal_options_4, 
                    "", my_leaf)
                else internal_options_4_left <- internal_options_4_left
                if (length(internal_options_5)) 
                  internal_options_5_left <- sub(internal_options_5, 
                    "", my_leaf)
                else internal_options_5_left <- internal_options_5_left
                if (length(internal_options_1e)) {
                  pattern_before_underscore <- strsplit(sub("_.*", 
                    "", internal_options_1e), NULL)[[1]]
                  for (char in pattern_before_underscore) {
                    internal_options_1e_left <- sub(char, "", 
                      internal_options_1e_left, fixed = TRUE)
                  }
                  internal_options_1e_left <- sub("E", "_", internal_options_1e_left)
                }
                if (length(internal_options_1e) > 0) {
                  if (!grepl("_E$", internal_options_1e)) {
                    internal_options_1e <- character(0)
                  }
                }
                if (!grepl(vowels, internal_options_1e_left, 
                  ignore.case = TRUE)) {
                  if (process_string(internal_options_1e, internal_options_base_left) == 
                    "") {
                    internal_options_1e_left <- internal_options_1e_left
                  }
                  else {
                    internal_options_1e_left <- internal_options_base_left
                    internal_options_1e <- character(0)
                  }
                }
                else {
                  internal_options_1e_left <- internal_options_1e_left
                }
                if (length(internal_options_2e)) {
                  pattern_before_underscore <- strsplit(sub("_.*", 
                    "", internal_options_2e), NULL)[[1]]
                  for (char in pattern_before_underscore) {
                    internal_options_2e_left <- sub(char, "", 
                      internal_options_2e_left, fixed = TRUE)
                  }
                  internal_options_2e_left <- sub("E", "_", internal_options_2e_left)
                }
                if (length(internal_options_2e) > 0) {
                  if (!grepl("_E$", internal_options_2e)) {
                    internal_options_2e <- character(0)
                  }
                }
                if (!grepl(vowels, internal_options_2e_left, 
                  ignore.case = TRUE)) {
                  if (process_string(internal_options_2e, internal_options_base_left) == 
                    "") {
                    internal_options_2e_left <- internal_options_2e_left
                  }
                  else {
                    internal_options_2e_left <- internal_options_base_left
                    internal_options_2e <- character(0)
                  }
                }
                else {
                  internal_options_2e_left <- internal_options_2e_left
                }
                if (length(internal_options_ue)) {
                  pattern_before_underscore <- strsplit(sub("_.*", 
                    "", internal_options_ue), NULL)[[1]]
                  for (char in pattern_before_underscore) {
                    internal_options_ue_left <- sub(char, "", 
                      internal_options_ue_left, fixed = TRUE)
                  }
                  internal_options_ue_left <- sub("E", "_", internal_options_ue_left)
                }
                if (length(internal_options_ue) > 0) {
                  if (!grepl("_E$", internal_options_ue)) {
                    internal_options_ue <- character(0)
                  }
                }
                if (!grepl(vowels, internal_options_ue_left, 
                  ignore.case = TRUE)) {
                  if (process_string(internal_options_ue, internal_options_base_left) == 
                    "") {
                    internal_options_ue_left <- internal_options_ue_left
                  }
                  else {
                    internal_options_ue_left <- internal_options_base_left
                    internal_options_ue <- character(0)
                  }
                }
                else {
                  internal_options_ue_left <- internal_options_ue_left
                }
                if (length(internal_options_2e2)) {
                  pattern_before_underscore <- strsplit(sub("_.*", 
                    "", internal_options_2e2), NULL)[[1]]
                  for (char in pattern_before_underscore) {
                    internal_options_2e2_left <- sub(char, "", 
                      internal_options_2e2_left, fixed = TRUE)
                  }
                  internal_options_2e2_left <- sub("E", "_", 
                    internal_options_2e2_left)
                }
                if (length(internal_options_2e2) > 0) {
                  if (!grepl("_E$", internal_options_2e2)) {
                    internal_options_2e2 <- character(0)
                  }
                }
                if (!grepl(vowels, internal_options_2e2_left, 
                  ignore.case = TRUE)) {
                  if (process_string(internal_options_2e2, internal_options_base_left) == 
                    "") {
                    internal_options_2e2_left <- internal_options_2e2_left
                  }
                  else {
                    internal_options_2e2_left <- internal_options_base_left
                    internal_options_2e2 <- character(0)
                  }
                }
                else {
                  internal_options_2e2_left <- internal_options_2e2_left
                }
                if (length(internal_options_1) > 0) {
                  internal <- mytree$Climb(mytree_path)$AddChild(internal_options_1)
                }
                if (length(internal_options_2) > 0) {
                  internal <- mytree$Climb(mytree_path)$AddChild(internal_options_2)
                }
                if (length(internal_options_3) > 0) {
                  internal <- mytree$Climb(mytree_path)$AddChild(internal_options_3)
                }
                if (length(internal_options_4) > 0) {
                  internal <- mytree$Climb(mytree_path)$AddChild(internal_options_4)
                }
                if (length(internal_options_5) > 0) {
                  internal <- mytree$Climb(mytree_path)$AddChild(internal_options_5)
                }
                if (length(internal_options_1e) > 0) {
                  internal <- mytree$Climb(mytree_path)$AddChild(internal_options_1e)
                }
                if (length(internal_options_2e) > 0) {
                  internal <- mytree$Climb(mytree_path)$AddChild(internal_options_2e)
                }
                if (length(internal_options_ue) > 0) {
                  internal <- mytree$Climb(mytree_path)$AddChild(internal_options_ue)
                }
                if (length(internal_options_2e2) > 0) {
                  internal <- mytree$Climb(mytree_path)$AddChild(internal_options_2e2)
                }
                if (length(internal_options_1) > 0) {
                  if (ncol(myletters_left) == 1) {
                    internal <- lefttree$Climb(lefttree_path)$AddChild("9")
                  }
                  else {
                    internal <- lefttree$Climb(lefttree_path)$AddChild(internal_options_1_left)
                  }
                }
                if (length(internal_options_2) > 0) {
                  if (ncol(myletters_left) == 2) {
                    internal <- lefttree$Climb(lefttree_path)$AddChild("9")
                  }
                  else {
                    internal <- lefttree$Climb(lefttree_path)$AddChild(internal_options_2_left)
                  }
                }
                if (length(internal_options_3) > 0) {
                  if (ncol(myletters_left) == 3) {
                    internal <- lefttree$Climb(lefttree_path)$AddChild("9")
                  }
                  else {
                    internal <- lefttree$Climb(lefttree_path)$AddChild(internal_options_3_left)
                  }
                }
                if (length(internal_options_4) > 0) {
                  if (ncol(myletters_left) == 4) {
                    internal <- lefttree$Climb(lefttree_path)$AddChild("9")
                  }
                  else {
                    internal <- lefttree$Climb(lefttree_path)$AddChild(internal_options_4_left)
                  }
                }
                if (length(internal_options_5) > 0) {
                  if (ncol(myletters_left) == 5) {
                    internal <- lefttree$Climb(lefttree_path)$AddChild("9")
                  }
                  else {
                    internal <- lefttree$Climb(lefttree_path)$AddChild(internal_options_5_left)
                  }
                }
                if (length(internal_options_1e) > 0) {
                  internal <- lefttree$Climb(lefttree_path)$AddChild(internal_options_1e_left)
                }
                if (length(internal_options_2e) > 0) {
                  internal <- lefttree$Climb(lefttree_path)$AddChild(internal_options_2e_left)
                }
                if (length(internal_options_ue) > 0) {
                  internal <- lefttree$Climb(lefttree_path)$AddChild(internal_options_ue_left)
                }
                if (length(internal_options_2e2) > 0) {
                  internal <- lefttree$Climb(lefttree_path)$AddChild(internal_options_2e2_left)
                }
            }
            if (lefttree$leaves[[counter]]$name == "9") {
                counter <- counter + 1
            }
        }
        prune_tree(mytree)
        return(mytree)
    }
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
    extract_branches <- function(node, path = c()) {
        path <- c(path, node$name)
        if (node$isLeaf) {
            branch <- paste(path, collapse = "-")
            original_word <- sub("-.*", "", branch)
            graphemes <- unlist(strsplit(sub(paste0(original_word, 
                "-"), "", branch), "-"))
            modified_branches <- list(branch)
            le_indices <- which(graphemes == "L" & c(graphemes[-1], 
                "") == "E")
            if (length(le_indices) > 0 && le_indices[1] == 1) {
                le_indices <- le_indices[-1]
            }
            if (length(le_indices) > 0) {
                for (i in seq_along(le_indices)) {
                  modified_graphemes <- graphemes
                  modified_graphemes[le_indices[i]] <- "_E"
                  modified_graphemes[le_indices[i] + 1] <- "L"
                  modified_branch <- paste(c(original_word, modified_graphemes), 
                    collapse = "-")
                  modified_branches <- c(modified_branches, modified_branch)
                }
            }
            re_indices <- which(graphemes == "R" & c(graphemes[-1], 
                "") == "E")
            if (length(re_indices) > 0 && re_indices[1] == 1) {
                re_indices <- re_indices[-1]
            }
            if (length(re_indices) > 0) {
                for (i in seq_along(re_indices)) {
                  modified_graphemes <- graphemes
                  modified_graphemes[re_indices[i]] <- "_E"
                  modified_graphemes[re_indices[i] + 1] <- "R"
                  modified_branch <- paste(c(original_word, modified_graphemes), 
                    collapse = "-")
                  modified_branches <- c(modified_branches, modified_branch)
                }
            }
            one_indices <- suppressWarnings(which(graphemes == 
                "O" & c(graphemes[-1], "") == "N" & c(graphemes[-(1:2)], 
                "") == "E"))
            if (length(one_indices) > 0) {
                for (i in seq_along(one_indices)) {
                  modified_graphemes <- graphemes
                  modified_graphemes[one_indices[i]] <- "O"
                  modified_graphemes[one_indices[i] + 1] <- "_E"
                  modified_graphemes[one_indices[i] + 2] <- "N"
                  modified_branch <- paste(c(original_word, modified_graphemes), 
                    collapse = "-")
                  modified_branches <- c(modified_branches, modified_branch)
                }
            }
            once_indices <- suppressWarnings(which(graphemes == 
                "O" & c(graphemes[-1], "") == "N" & c(graphemes[-(1:2)], 
                "") == "C" & c(graphemes[-(1:3)], "") == "E"))
            if (length(once_indices) > 0) {
                for (i in seq_along(once_indices)) {
                  modified_graphemes <- graphemes
                  modified_graphemes[once_indices[i]] <- "O"
                  modified_graphemes[once_indices[i] + 1] <- "_E"
                  modified_graphemes[once_indices[i] + 2] <- "N"
                  modified_graphemes[once_indices[i] + 3] <- "C"
                  modified_branch <- paste(c(original_word, modified_graphemes), 
                    collapse = "-")
                  modified_branches <- c(modified_branches, modified_branch)
                }
            }
            return(modified_branches)
        }
        else {
            branches <- list()
            for (child in node$children) {
                branches <- c(branches, extract_branches(child, 
                  path))
            }
            return(branches)
        }
    }
    count_letters_and_extract_graphemes <- function(branches) {
        original_word <- sub("-.*", "", branches[[1]])
        letter_counts <- table(strsplit(original_word, "")[[1]])
        grapheme_branches <- lapply(branches, function(x) {
            graphemes <- unlist(strsplit(sub("^[^-]*-", "", x), 
                "-"))
            return(graphemes)
        })
        return(list(letter_counts = letter_counts, grapheme_branches = grapheme_branches))
    }
    extract_graphemes_for_letters <- function(branches) {
        results <- count_letters_and_extract_graphemes(branches)
        grapheme_branches <- results$grapheme_branches
        letter_occurrences <- list()
        letter_total_counts <- as.list(results$letter_counts)
        results_df <- data.frame(Letter_Occurrence = character(), 
            Graphemes = character(), Position = character(), 
            stringsAsFactors = FALSE)
        for (branch in grapheme_branches) {
            branch_letter_counts <- setNames(as.list(rep(0, length(letter_total_counts))), 
                names(letter_total_counts))
            for (i in seq_along(branch)) {
                grapheme <- branch[i]
                position <- if (i == 1) {
                  "wi"
                }
                else if (i == length(branch)) {
                  "wf"
                }
                else {
                  "wm"
                }
                grapheme_letters <- unlist(strsplit(grapheme, 
                  ""))
                grapheme_letters <- grapheme_letters[grapheme_letters != 
                  "_"]
                for (letter in grapheme_letters) {
                  branch_letter_counts[[letter]] <- branch_letter_counts[[letter]] + 
                    1
                  occurrence_key <- paste0(letter, branch_letter_counts[[letter]])
                  results_df <- rbind(results_df, data.frame(Letter_Occurrence = occurrence_key, 
                    Graphemes = grapheme, Position = position, 
                    stringsAsFactors = FALSE))
                }
            }
        }
        return(results_df)
    }
    parsed_words <- list()
    pb <- NULL
    for (j in 1:length(corpus$spelling)) {
        myword <- corpus$spelling[j]
        branches <- get_branches_cached(myword)
        if (grepl("es$", myword)) {
            myword2 <- substr(myword, 1, nchar(myword) - 1)
            branches2 <- get_branches_cached(myword2)
            branches2 <- lapply(branches2, function(branch) {
                if (grepl("-", branch)) {
                  branch <- sub("-", "S-", branch)
                }
                paste(branch, "-S", sep = "")
            })
            branches <- unique(c(branches, branches2))
        }
        if (grepl("ed$", myword)) {
            myword2 <- substr(myword, 1, nchar(myword) - 1)
            branches2 <- get_branches_cached(myword2)
            branches2 <- lapply(branches2, function(branch) {
                if (grepl("-", branch)) {
                  branch <- sub("-", "D-", branch)
                }
                paste(branch, "-D", sep = "")
            })
            branches <- unique(c(branches, branches2))
        }
        allbranches_df <- extract_graphemes_for_letters(branches)
        mycorrect <- paste(toupper(mapped_corpus[[1]][[j]][2, 
            ]), collapse = "-")
        correctbranch <- which(sub("^[^-]*-", "", branches) == 
            mycorrect)
        correctbranch_df <- extract_graphemes_for_letters(branches[correctbranch])
        correctbranch_df$index1 <- paste0(correctbranch_df$Letter_Occurrence, 
            correctbranch_df$Graphemes, "_", correctbranch_df$Position)
        allbranches_df$index1 <- paste0(allbranches_df$Letter_Occurrence, 
            allbranches_df$Graphemes, "_", allbranches_df$Position)
        allbranches_df$correct <- 0
        allbranches_df[allbranches_df$index1 %in% correctbranch_df$index1, 
            ]$correct <- 1
        allbranches_df$index2 <- paste0(allbranches_df$Graphemes, 
            "_", allbranches_df$Position)
        allbranches_df$choices <- allbranches_df$index2
        for (i in 1:length(allbranches_df$Letter_Occurrence)) {
            allbranches_df$choices[i] <- paste0(sort(unique(allbranches_df[allbranches_df$Letter_Occurrence == 
                allbranches_df[i, ]$Letter_Occurrence, ]$index2)), 
                collapse = ",")
        }
        parsed_words[[j]] <- allbranches_df
        if (!is.null(pb)) 
            pb$tick()
    }
    for (i in 1:length(parsed_words)) {
        parsed_words[[i]]$spelling <- corpus$spelling[i]
        parsed_words[[i]]$frequency <- corpus$freq[i]
    }
    parsed_words_nodups <- parsed_words[!duplicated(parsed_words)]
    mystats <- list()
    for (i in 1:length(parsed_words_nodups)) {
        temp <- parsed_words_nodups[[i]][parsed_words_nodups[[i]]$correct == 
            1, ][!duplicated(parsed_words_nodups[[i]][parsed_words_nodups[[i]]$correct == 
            1, ]), ]
        mystats[[i]] <- temp
    }
    mystats <- do.call(rbind, mystats)
    mystats$reduced <- mystats$choices
    for (i in 1:length(unique(mystats$choices))) {
        mystats[mystats$choices == unique(mystats$choices)[i], 
            ]$reduced <- paste0(sort(unique(mystats[mystats$choices == 
            unique(mystats$choices)[i], ]$index2)), collapse = ",")
    }
    mystats$Letter <- substring(mystats$Letter_Occurrence, 1, 
        1)
    mystats$suffix_class <- ifelse(grepl("ed$", mystats$spelling, 
        ignore.case = TRUE), "EDERES", ifelse(grepl("er$", mystats$spelling, 
        ignore.case = TRUE), "EDERES", ifelse(grepl("es$", mystats$spelling, 
        ignore.case = TRUE), "EDERES", "OTHER")))
    mystats$index3 <- paste0(mystats$Letter, "_in_", mystats$index2, 
        "_and_", mystats$reduced, "_SC_", mystats$suffix_class)
    mystats$index4 <- paste0(mystats$Letter, "_in_", mystats$reduced, 
        "_SC_", mystats$suffix_class)
    pp_table <- aggregate(data = mystats, FUN = length, spelling ~ 
        index3 * index4)
    colnames(pp_table)[3] <- "numer"
    temp <- aggregate(data = mystats, FUN = length, spelling ~ 
        index4)
    colnames(temp)[2] <- "denom"
    pp_table <- merge(pp_table, temp, by = "index4")
    pp_table$pp <- pp_table$numer/pp_table$denom
    temp_freq1 <- aggregate(data = mystats, FUN = sum, log10(frequency + 
        1) ~ index3 * index4)
    colnames(temp_freq1)[3] <- "numer"
    temp_freq2 <- aggregate(data = mystats, FUN = sum, log10(frequency + 
        1) ~ index4)
    colnames(temp_freq2)[2] <- "denom"
    temp_freq1 <- merge(temp_freq1, temp_freq2, by = "index4")
    temp_freq1$pp_freq <- temp_freq1$numer/temp_freq1$denom
    pp_table <- merge(pp_table, temp_freq1[, c(2, 5)], by = "index3")
    pp_table$numer <- NULL
    pp_table$denom <- NULL
    pp_table <- pp_table[order(pp_table$index4), ]
    corpus_parsed <- merge(mystats, pp_table[, c(1, 3, 4)], by = "index3")
    corpus_pp <- aggregate(data = corpus_parsed, FUN = mean, 
        pp ~ spelling)
    colnames(corpus_pp)[2] <- "pp_mean"
    corpus_pp$pp_min <- aggregate(data = corpus_parsed, FUN = min, 
        pp ~ spelling)[, 2]
    corpus_pp$pp_freq_mean <- aggregate(data = corpus_parsed, 
        FUN = mean, pp_freq ~ spelling)[, 2]
    corpus_pp$pp_freq_min <- aggregate(data = corpus_parsed, 
        FUN = min, pp_freq ~ spelling)[, 2]
    pp_reduced <- aggregate(data = mystats, FUN = length, spelling ~ 
        reduced * choices)[, c(1, 2)]
    return(list(pp_table, pp_reduced, corpus_parsed, corpus_pp))
} 

# ============================================================ 
# map_value 
# ============================================================ 
map_value <- function (spelling, pronunciation, level, tables, progress = FALSE, 
    parsed_corpus = NULL) 
{
    if (progress == TRUE) {
        pb = txtProgressBar(min = 0, max = length(spelling), 
            initial = 0)
    }
    mylist <- list()
    pp_cache <- list()
    position_mapping <- c(`1` = "wi", `2` = "si", `3` = "sm", 
        `4` = "sf", `5` = "wf")
    if (!is.null(parsed_corpus)) {
        stopifnot(is.list(parsed_corpus), length(parsed_corpus) >= 
            2)
        stopifnot(exists("pw_read"))
        get_pair_pp <- function(spell, pron) {
            if (is.null(pp_cache[[spell]])) {
                pp_lookup <- tryCatch({
                  pp_out <- pw_read(target = spell, parsed_corpus = parsed_corpus, 
                    PG_table = tables, level = "PG", score = FALSE, 
                    min_pp = 0, min_map = 0, max_options = 1e+06)
                  phon_options <- pp_out[[1]]
                  pp_means <- pp_out[[2]]
                  if (!is.list(phon_options) || length(phon_options) == 
                    0 || nrow(pp_means) == 0) {
                    data.frame(pronunciation = character(0), 
                      pp = numeric(0), stringsAsFactors = FALSE)
                  }
                  else {
                    all_prons <- unlist(phon_options, use.names = FALSE)
                    n_per_parse <- vapply(phon_options, length, 
                      integer(1))
                    pp_vals <- rep(pp_means$pp, times = n_per_parse)
                    temp <- data.frame(pronunciation = all_prons, 
                      pp = pp_vals, stringsAsFactors = FALSE)
                    aggregate(pp ~ pronunciation, data = temp, 
                      FUN = max)
                  }
                }, error = function(e) {
                  data.frame(pronunciation = character(0), pp = numeric(0), 
                    stringsAsFactors = FALSE)
                })
                pp_cache[[spell]] <<- pp_lookup
            }
            idx <- which(pp_cache[[spell]]$pronunciation == pron)
            if (length(idx) == 0) 
                return(0)
            max(pp_cache[[spell]]$pp[idx], na.rm = TRUE)
        }
    }
    for (i in 1:length(spelling)) {
        ifelse(level == "ONC", value <- map_ONC(spelling[i], 
            pronunciation[i], map_progress = FALSE), ifelse(level == 
            "OC", value <- map_OC(spelling[i], pronunciation[i], 
            map_progress = FALSE), ifelse(level == "OR", value <- map_OR(spelling[i], 
            pronunciation[i], map_progress = FALSE), value <- map_PG(spelling[i], 
            pronunciation[i], map_progress = FALSE))))
        df <- as.data.frame(value[[1]][[1]])
        while (all(is.na(df[, 1]))) {
            df <- df[, -1]
        }
        rownames(df) <- c("phoneme", "grapheme", "position")
        df["position", ] <- position_mapping[unlist(df["position", 
            ])]
        df[c("PG", "GP", "PG_freq", "P_freq", "G_freq"), ] <- NA
        tryCatch({
            for (col in colnames(df)) {
                phoneme_val <- df["phoneme", col]
                grapheme_val <- df["grapheme", col]
                position_val <- df["position", col]
                df["PG", col] <- subset(tables$pg, phoneme == 
                  phoneme_val & grapheme == grapheme_val)[, position_val]
                df["GP", col] <- subset(tables$gp, phoneme == 
                  phoneme_val & grapheme == grapheme_val)[, position_val]
                df["PG_freq", col] <- subset(tables$pg_freq, 
                  phoneme == phoneme_val & grapheme == grapheme_val)[, 
                  position_val]
                df["P_freq", col] <- subset(tables$p_freq, phoneme == 
                  phoneme_val)[, position_val]
                df["G_freq", col] <- subset(tables$g_freq, grapheme == 
                  grapheme_val)[, position_val]
            }
        }, error = function(e) {
            NA
        })
        df["spelling", 1] <- spelling[i]
        df["spelling", -1] <- ""
        df["pronunciation", 1] <- pronunciation[i]
        df["pronunciation", -1] <- ""
        df["PG_accuracy", 1] <- value[[2]]
        df["PG_accuracy", -1] <- ""
        if (!is.null(parsed_corpus)) {
            df["pp", 1] <- get_pair_pp(spelling[i], pronunciation[i])
            df["pp", -1] <- ""
        }
        mylist[[i]] <- df
        names(mylist[[i]]) <- spelling[i]
        if (progress == TRUE) {
            setTxtProgressBar(pb, i)
        }
    }
    return(mylist)
} 

# ============================================================ 
# word_pattern 
# ============================================================ 
word_pattern <- function (mapped_words, phoneme, grapheme, position) 
{
    scenario <- ifelse(phoneme == "any", 1, ifelse(grapheme == 
        "any", 2, ifelse(position == "any", 3, 4)))
    matching_words <- c()
    for (i in seq_along(mapped_words)) {
        df <- mapped_words[[i]]
        if (!("spelling" %in% rownames(df))) {
            next
        }
        word_values <- as.character(df["spelling", ])
        word <- word_values[which.max(nchar(word_values))]
        if (nchar(word) == 0) 
            next
        if (scenario == 4) 
            for (j in 1:ncol(df)) {
                if (df["phoneme", j] == phoneme && df["grapheme", 
                  j] == grapheme && df["position", j] == position) {
                  matching_words <- c(matching_words, word)
                  break
                }
            }
        if (scenario == 3) 
            for (j in 1:ncol(df)) {
                if (df["phoneme", j] == phoneme && df["grapheme", 
                  j] == grapheme) {
                  matching_words <- c(matching_words, word)
                  break
                }
            }
        if (scenario == 2) 
            for (j in 1:ncol(df)) {
                if (df["phoneme", j] == phoneme && df["position", 
                  j] == position) {
                  matching_words <- c(matching_words, word)
                  break
                }
            }
        if (scenario == 1) 
            for (j in 1:ncol(df)) {
                if (df["grapheme", j] == grapheme && df["position", 
                  j] == position) {
                  matching_words <- c(matching_words, word)
                  break
                }
            }
    }
    return(matrix(matching_words))
} 

# ============================================================ 
# summarize_words 
# ============================================================ 
summarize_words <- function (mapped_words, parameter, mode = c("default", "ONC")) 
{
    mode <- match.arg(mode)
    vowels <- c("5", "O", "8", "je", "j3r", "ju", "jU", "o", 
        "2", "@", "a", "e", "E", "3", "3r", "i", "1", "c", "u", 
        "U", "^")
    classify_onc_cols <- function(mw) {
        ph <- as.character(mw["phoneme", ])
        pos <- as.character(mw["position", ])
        if (length(ph) > 0 && ph[1] == mw["spelling", 1]) {
            ph <- ph[-1]
            pos <- pos[-1]
            offset <- 1L
        }
        else {
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
                  idx_O <- c(idx_O, seg_idx[!is_vowel[seg_idx] & 
                    seg_pos %in% c("wi", "si", "sm")])
                  idx_C <- c(idx_C, seg_idx[!is_vowel[seg_idx] & 
                    seg_pos %in% c("sf", "wf")])
                }
                else {
                  first_v <- min(seg_v_idx)
                  last_v <- max(seg_v_idx)
                  cons_idx <- seg_idx[!is_vowel[seg_idx]]
                  idx_N <- c(idx_N, seg_v_idx)
                  idx_O <- c(idx_O, cons_idx[cons_idx < first_v])
                  idx_C <- c(idx_C, cons_idx[cons_idx > last_v])
                }
            }
            return(list(O = sort(unique(idx_O + offset)), N = sort(unique(idx_N + 
                offset)), C = sort(unique(idx_C + offset))))
        }
        is_onset_pos <- pos_norm %in% c("wi", "si")
        is_coda_pos <- pos_norm %in% c("sm", "sf", "wf")
        idx_N <- integer(0)
        idx_O <- which(!is_vowel & is_onset_pos) + offset
        idx_C <- which(!is_vowel & is_coda_pos) + offset
        list(O = idx_O, N = idx_N, C = idx_C)
    }
    stats_vec <- function(x) {
        x_raw <- suppressWarnings(as.numeric(x))
        has_bad_component <- any(!is.finite(x_raw) | x_raw == 
            0)
        x <- x_raw[is.finite(x_raw)]
        if (length(x) == 0) 
            return(c(0, NA_real_, NA_real_, NA_real_, NA_real_))
        c(if (has_bad_component) 0 else mean(x), median(x), max(x), 
            min(x), stats::sd(x))
    }
    if (mode == "default") {
        mystats <- matrix(nrow = length(mapped_words), ncol = 7)
        for (i in seq_along(mapped_words)) {
            mw <- mapped_words[[i]]
            mystats[i, 1] <- mw["spelling", 1]
            mystats[i, 2] <- mw["pronunciation", 1]
            if (mw["PG_accuracy", ][1] == TRUE) {
                data <- as.numeric(mw[parameter, ])
                mystats[i, 3:7] <- stats_vec(data)
            }
            else {
                mystats[i, 3:7] <- NA
            }
        }
        mystats <- as.data.frame(mystats, stringsAsFactors = FALSE)
        colnames(mystats) <- c("spelling", "pronunciation", "mean", 
            "median", "max", "min", "sd")
        for (nm in c("mean", "median", "max", "min", "sd")) mystats[[nm]] <- as.numeric(mystats[[nm]])
        return(mystats)
    }
    mystats <- matrix(nrow = length(mapped_words), ncol = 17)
    for (i in seq_along(mapped_words)) {
        mw <- mapped_words[[i]]
        mystats[i, 1] <- mw["spelling", 1]
        mystats[i, 2] <- mw["pronunciation", 1]
        if (mw["PG_accuracy", ][1] == TRUE) {
            data <- as.numeric(mw[parameter, ])
            idx <- classify_onc_cols(mw)
            mystats[i, 3:7] <- stats_vec(data[idx$O])
            mystats[i, 8:12] <- stats_vec(data[idx$N])
            mystats[i, 13:17] <- stats_vec(data[idx$C])
        }
        else {
            mystats[i, 3:17] <- NA
        }
    }
    mystats <- as.data.frame(mystats, stringsAsFactors = FALSE)
    colnames(mystats) <- c("spelling", "pronunciation", "mean_onset", 
        "median_onset", "max_onset", "min_onset", "sd_onset", 
        "mean_nucleus", "median_nucleus", "max_nucleus", "min_nucleus", 
        "sd_nucleus", "mean_coda", "median_coda", "max_coda", 
        "min_coda", "sd_coda")
    for (nm in names(mystats)[-(1:2)]) mystats[[nm]] <- as.numeric(mystats[[nm]])
    return(mystats)
} 

# ============================================================ 
# pw_spell 
# ============================================================ 
pw_spell <- function (target, level = "PG", score = TRUE, PG_table = NULL, 
    OC_table = NULL, OR_table = NULL, min_map = 1, max_options = 1000, 
    param = "pg", mean_score = 0, parsed_corpus = NULL, summary_mode = c("default", 
        "ONC")) 
{
    library(readr)
    library(stringr)
    phonemes_to_check <- paste(c("ju", "je", "jU", "j3", "ks", 
        "kS", "nj", "gz", "gZ"), collapse = "|")
    level <- toupper(level)
    if (!level %in% c("PG", "OC", "OR", "ALL")) {
        stop("Unknown level: ", level, ". Use 'PG', 'OC', 'OR', or 'all'.")
    }
    if (level == "ALL") {
        if (is.null(PG_table) || is.null(OC_table) || is.null(OR_table)) {
            stop("When level = 'all', PG_table, OC_table, and OR_table must all be provided.")
        }
        return(list(PG = pw_spell(target = target, level = "PG", 
            PG_table = PG_table, OC_table = OC_table, OR_table = OR_table, 
            min_map = min_map, max_options = max_options, param = param, 
            score = score, mean_score = mean_score, parsed_corpus = parsed_corpus, 
            summary_mode = summary_mode), OC = pw_spell(target = target, 
            level = "OC", PG_table = PG_table, OC_table = OC_table, 
            OR_table = OR_table, min_map = min_map, max_options = max_options, 
            param = param, score = score, mean_score = mean_score, 
            parsed_corpus = parsed_corpus, summary_mode = summary_mode), 
            OR = pw_spell(target = target, level = "OR", PG_table = PG_table, 
                OC_table = OC_table, OR_table = OR_table, min_map = min_map, 
                max_options = max_options, param = param, score = score, 
                mean_score = mean_score, parsed_corpus = parsed_corpus, 
                summary_mode = summary_mode)))
    }
    if (level == "PG") {
        tables <- PG_table
    }
    else if (level == "OC") {
        tables <- OC_table
    }
    else {
        tables <- OR_table
    }
    if (is.null(tables)) {
        stop(level, "_table must be provided when level = '", 
            level, "'.")
    }
    selected_param <- toupper(param)
    if (!selected_param %in% c("PG", "GP", "PG_FREQ", "P_FREQ", 
        "G_FREQ", "PP")) {
        stop("param must be one of: PG, GP, PG_freq, P_freq, G_freq, or pp.")
    }
    summary_mode <- match.arg(summary_mode)
    if (summary_mode == "ONC" && mean_score != 0) {
        stop("mean_score thresholding is only available when summary_mode = 'default'.")
    }
    score_row <- switch(selected_param, PG = "PG", GP = "GP", 
        PG_FREQ = "PG_freq", P_FREQ = "P_freq", G_FREQ = "G_freq", 
        PP = "pp")
    if (!is.numeric(min_map) || length(min_map) != 1 || is.na(min_map)) {
        stop("min_map must be a single numeric value.")
    }
    if (!is.numeric(max_options) || length(max_options) != 1 || 
        is.na(max_options) || max_options < 1) {
        stop("max_options must be a single numeric value >= 1.")
    }
    max_options <- as.integer(max_options)
    prep_pword <- function(word_index) {
        hold0 <- as.data.frame(as.vector(sapply(target[word_index], 
            parse_syllables_PWSPELL)))
        hold0 <- hold0[hold0 != "", ]
        parsed_syll <- matrix(strsplit(hold0, "   ")[[1]])
        syll_count <- parsed_syll
        tryCatch({
            syll_count[syll_count == "", ] <- NA
        }, error = function(e) {
            syll_count <<- 1
        })
        syll_count <- length(na.omit(syll_count))
        syll_struc <- parsed_syll
        tryCatch({
            syll_struc[1, ] <- sub(".", "1", syll_struc[1, ])
        }, error = function(e) {
            syll_struc <<- sub(".", "1", syll_struc)
        })
        for (i in 2:10) {
            tryCatch(syll_struc[i, ] <- {
                sub(".", "2", syll_struc[i, ])
            }, error = function(e) NA)
        }
        my.index <- matrix(NA, nrow = 10)
        for (i in 1:10) {
            tryCatch({
                my.index[i] <- nchar(syll_struc[i, ])
            }, error = function(e) NA)
        }
        my.index[1, ] <- nchar(syll_struc[1])
        for (i in 1:10) {
            tryCatch({
                substring(syll_struc[i, ], 2, my.index[i] - 1) <- paste(rep("3", 
                  my.index[i] - 2), collapse = "")
            }, error = function(e) NA)
        }
        if (max(nchar(syll_struc) == 1 & syll_count == 1)) {
            syll_struc <- 1
        }
        else {
            ifelse(syll_count == 1, substring(syll_struc[1], 
                2, my.index[1, ] - 1) <- paste(rep("3", my.index[1, 
                ] - 2), collapse = ""), syll_struc <- syll_struc)
        }
        for (i in 1:10) {
            ifelse(my.index[i] > 1, my.index[i] <- my.index[i], 
                my.index[i] <- 0)
        }
        for (i in 1:10) {
            tryCatch({
                substring(syll_struc[i, ], my.index[i], my.index[i]) <- "4"
            }, error = function(e) NA)
        }
        monophonemic <- FALSE
        ifelse(nchar(syll_struc[1]) == 1 & syll_count == 1, monophonemic <- TRUE, 
            monophonemic <- FALSE)
        ifelse(monophonemic == FALSE & syll_count > 1, str_sub(syll_struc[syll_count, 
            ], -1, -1) <- "5", syll_struc <- syll_struc)
        ifelse(monophonemic == FALSE & syll_count == 1, str_sub(syll_struc[syll_count], 
            -1, -1) <- "5", syll_struc <- syll_struc)
        map_j <- FALSE
        map_j_wi <- FALSE
        syll_struc0 <- paste(syll_struc, collapse = "")
        parsed_syll0 <- paste(parsed_syll, collapse = "")
        return(matrix(c(syll_struc0, parsed_syll0)))
    }
    parse_syllables_PWSPELL <- function(target, word_index) {
        vowels <- c("5", "O", "8", "je", "j3r", "ju", "jU", "o", 
            "2", "@", "a", "e", "E", "3r", "i", "1", "c", "u", 
            "U", "^", "œ", "∑", "®", "†")
        consonants <- c("G", "gz", "kS", "nj", "C", "T", "D", 
            "Z", "N", "b", "d", "f", "g", "h", "j", "k", "l", 
            "m", "n", "p", "r", "s", "S", "t", "v", "w", "z", 
            "¥", "ø", "π", "å", "ß")
        syll_init_cons <- c("spl", "spr", "str", "skr", "skw", 
            "pl", "pr", "G", "tr", "C", "tw", "kl", "kr", "kw", 
            "bl", "br", "dr", "dw", "gl", "gr", "fl", "fr", "Tr", 
            "Sr", "sl", "st", "sp", "sk", "sm", "sn", "sf", "bj", 
            "fj", "vj", "mj", "kj", "hj", "nj", "pj", "tj", "Cj", 
            "dj", "Tj", "Gj", "gj", "Sj", "sj", "gw")
        all_phonemes <- c(vowels, syll_init_cons, consonants)
        phonemes <- c()
        remaining <- target[word_index]
        vowel_count <- 0
        while (nchar(remaining) > 0) {
            matched <- FALSE
            if (length(phonemes) == 0 || (length(phonemes) > 
                0 && phonemes[length(phonemes)] %in% vowels)) {
                for (p in syll_init_cons) {
                  if (startsWith(remaining, p)) {
                    phonemes <- c(phonemes, p)
                    remaining <- substring(remaining, nchar(p) + 
                      1)
                    matched <- TRUE
                    break
                  }
                }
            }
            if (!matched) {
                for (p in all_phonemes) {
                  if (startsWith(remaining, p)) {
                    phonemes <- c(phonemes, p)
                    remaining <- substring(remaining, nchar(p) + 
                      1)
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
        pattern <- paste(ifelse(phonemes %in% vowels, "V", "C"), 
            collapse = "")
        if (vowel_count < 2) {
            return(target[word_index])
        }
        rules <- list(CCCVCC = 5, CCVCC = 4, CVCCC = 4, CCCVC = 4, 
            CVCC = 3, CCVC = 3, CVC = 2, CVV = 2, VCC = 2, VCV = 1, 
            VV = 1)
        split_index <- 0
        for (rule in names(rules)) {
            if (startsWith(pattern, rule)) {
                split_index <- rules[[rule]]
                break
            }
        }
        if (split_index >= 1) {
            left <- paste(phonemes[1:split_index], collapse = "")
            right <- paste(phonemes[(split_index + 1):length(phonemes)], 
                collapse = "")
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
    insertSpacesMatching <- function(originalString, matchString) {
        originalString <- gsub("\\s", "", originalString)
        spaces <- gregexpr(" ", matchString)[[1]] - 1
        for (pos in spaces) {
            originalString <- paste0(substr(originalString, 1, 
                pos), " ", substr(originalString, pos + 1, nchar(originalString)))
        }
        return(originalString)
    }
    vowels <- c("5", "O", "8", "je", "j3", "ju", "jU", "o", "2", 
        "@", "a", "e", "E", "3", "i", "1", "c", "u", "U", "^")
    prep_pword_ONC <- function(mylist2) {
        hold <- mylist2
        for (i in 1:length(target)) {
            hold[[i]] <- rbind(hold[[i]], hold[[i]][2, ], hold[[i]][2, 
                ])
            tryCatch({
                if (which(is.na(hold[[i]][2, ])) > 0) 
                  hold[[i]] <- hold[[i]][, -which(is.na(hold[[i]][2, 
                    ]))]
            }, error = function(e) NA)
            hold[[i]][4, ][hold[[i]][2, ] %in% vowels] <- "V"
            hold[[i]][4, ][!hold[[i]][2, ] %in% vowels] <- "C"
            colnames(hold[[i]]) <- c(letters, LETTERS)[1:ncol(hold[[i]])]
            CVstring <- paste0(hold[[i]][4, ], collapse = "")
            posstring <- paste0(hold[[i]][1, ], collapse = "")
            colstring <- paste0(c(letters, LETTERS)[1:nchar(CVstring)], 
                collapse = "")
            split_positions1 <- gregexpr("V", CVstring)[[1]]
            CVstring <- gsub("V", " V ", CVstring)
            posstring <- insertSpacesMatching(posstring, CVstring)
            colstring <- insertSpacesMatching(colstring, CVstring)
            split_positions2 <- gregexpr("2", posstring)[[1]]
            posstring <- gsub("2", " 2", posstring)
            CVstring <- insertSpacesMatching(CVstring, posstring)
            colstring <- insertSpacesMatching(colstring, posstring)
            CVstring <- trimws(CVstring, "left")
            colstring <- trimws(colstring, "left")
            posstring <- trimws(posstring, "left")
            colstring <- as.matrix(read.table(text = colstring))
            result <- matrix(NA, ncol = length(colstring), nrow = 4)
            for (j in 1:length(colstring)) {
                cols_to_paste <- unlist(strsplit(colstring[j], 
                  ""))
                result[, j] <- apply(hold[[i]][, cols_to_paste, 
                  drop = FALSE], 1, paste, collapse = "")
            }
            result <- as.data.frame(result)
            hold[[i]] <- result
            for (j in 1:ncol(hold[[i]])) {
                if (nchar(hold[[i]][4, j]) > 1) 
                  if (as.numeric(substr(hold[[i]][1, j], 1, 1)) < 
                    3) {
                    hold[[i]][1, j] <- substr(hold[[i]][1, j], 
                      1, 1)
                  }
                  else {
                    hold[[i]][1, j] <- substr(hold[[i]][1, j], 
                      nchar(hold[[i]][1, j]), nchar(hold[[i]][1, 
                        j]))
                  }
            }
            hold[[i]] <- data.frame(hold[[i]][-4, ])
        }
        return(hold)
    }
    prep_pword_OC <- function(mylist2) {
        hold <- mylist2
        for (i in 1:length(target)) {
            hold[[i]] <- rbind(hold[[i]], hold[[i]][2, ], hold[[i]][2, 
                ])
            tryCatch({
                if (which(is.na(hold[[i]][2, ])) > 0) 
                  hold[[i]] <- hold[[i]][, -which(is.na(hold[[i]][2, 
                    ]))]
            }, error = function(e) NA)
            hold[[i]][4, ][hold[[i]][2, ] %in% vowels] <- "V"
            hold[[i]][4, ][!hold[[i]][2, ] %in% vowels] <- "C"
            colnames(hold[[i]]) <- c(letters, LETTERS)[1:ncol(hold[[i]])]
            CVstring <- paste0(hold[[i]][4, ], collapse = "")
            posstring <- paste0(hold[[i]][1, ], collapse = "")
            colstring <- paste0(c(letters, LETTERS)[1:nchar(CVstring)], 
                collapse = "")
            split_positions1 <- gregexpr("V", CVstring)[[1]]
            CVstring <- gsub("V", "V ", CVstring)
            posstring <- insertSpacesMatching(posstring, CVstring)
            colstring <- insertSpacesMatching(colstring, CVstring)
            split_positions2 <- gregexpr("2", posstring)[[1]]
            posstring <- gsub("2", " 2", posstring)
            CVstring <- insertSpacesMatching(CVstring, posstring)
            colstring <- insertSpacesMatching(colstring, posstring)
            CVstring <- trimws(CVstring, "right")
            colstring <- trimws(colstring, "right")
            posstring <- trimws(posstring, "right")
            CVstring <- gsub("\\s+", " ", CVstring)
            colstring <- gsub("\\s+", " ", colstring)
            posstring <- gsub("\\s+", " ", posstring)
            colstring <- as.matrix(read.table(text = colstring))
            result <- matrix(NA, ncol = length(colstring), nrow = 4)
            for (j in 1:length(colstring)) {
                cols_to_paste <- unlist(strsplit(colstring[j], 
                  ""))
                result[, j] <- apply(hold[[i]][, cols_to_paste, 
                  drop = FALSE], 1, paste, collapse = "")
            }
            result <- as.data.frame(result)
            hold[[i]] <- result
            for (j in 1:ncol(hold[[i]])) {
                if (nchar(hold[[i]][4, j]) > 1) 
                  if (as.numeric(substr(hold[[i]][1, j], 1, 1)) == 
                    1) {
                    hold[[i]][1, j] <- 1
                  }
                  else {
                    if (as.numeric(substr(hold[[i]][1, j], nchar(hold[[i]][1, 
                      j]), nchar(hold[[i]][1, j]))) == 5) {
                      hold[[i]][1, j] <- 5
                    }
                    else {
                      if (as.numeric(substr(hold[[i]][1, j], 
                        1, 1)) < 3) {
                        hold[[i]][1, j] <- substr(hold[[i]][1, 
                          j], 1, 1)
                      }
                      else {
                        hold[[i]][1, j] <- substr(hold[[i]][1, 
                          j], nchar(hold[[i]][1, j]), nchar(hold[[i]][1, 
                          j]))
                      }
                    }
                  }
            }
            hold[[i]] <- data.frame(hold[[i]][-4, ])
        }
        return(hold)
    }
    prep_pword_OR <- function(mylist2) {
        hold <- mylist2
        for (i in 1:length(target)) {
            hold[[i]] <- rbind(hold[[i]], hold[[i]][2, ], hold[[i]][2, 
                ])
            tryCatch({
                if (which(is.na(hold[[i]][2, ])) > 0) 
                  hold[[i]] <- hold[[i]][, -which(is.na(hold[[i]][2, 
                    ]))]
            }, error = function(e) NA)
            hold[[i]][4, ][hold[[i]][2, ] %in% vowels] <- "V"
            hold[[i]][4, ][!hold[[i]][2, ] %in% vowels] <- "C"
            colnames(hold[[i]]) <- c(letters, LETTERS)[1:ncol(hold[[i]])]
            CVstring <- paste0(hold[[i]][4, ], collapse = "")
            posstring <- paste0(hold[[i]][1, ], collapse = "")
            colstring <- paste0(c(letters, LETTERS)[1:nchar(CVstring)], 
                collapse = "")
            split_positions1 <- gregexpr("V", CVstring)[[1]]
            CVstring <- gsub("V", " V", CVstring)
            posstring <- insertSpacesMatching(posstring, CVstring)
            colstring <- insertSpacesMatching(colstring, CVstring)
            split_positions2 <- gregexpr("2", posstring)[[1]]
            posstring <- gsub("2", " 2", posstring)
            CVstring <- insertSpacesMatching(CVstring, posstring)
            colstring <- insertSpacesMatching(colstring, posstring)
            CVstring <- trimws(CVstring, "left")
            colstring <- trimws(colstring, "left")
            posstring <- trimws(posstring, "left")
            CVstring <- gsub("\\s+", " ", CVstring)
            colstring <- gsub("\\s+", " ", colstring)
            posstring <- gsub("\\s+", " ", posstring)
            colstring <- as.matrix(read.table(text = colstring))
            result <- matrix(NA, ncol = length(colstring), nrow = 4)
            for (j in 1:length(colstring)) {
                cols_to_paste <- unlist(strsplit(colstring[j], 
                  ""))
                result[, j] <- apply(hold[[i]][, cols_to_paste, 
                  drop = FALSE], 1, paste, collapse = "")
            }
            result <- as.data.frame(result)
            hold[[i]] <- result
            for (j in 1:ncol(hold[[i]])) {
                if (nchar(hold[[i]][4, j]) > 1) 
                  if (as.numeric(substr(hold[[i]][1, j], nchar(hold[[i]][1, 
                    j]), nchar(hold[[i]][1, j]))) == 5) {
                    hold[[i]][1, j] <- 5
                  }
                  else {
                    if (as.numeric(substr(hold[[i]][1, j], 1, 
                      1)) == 1) {
                      hold[[i]][1, j] <- 1
                    }
                    else {
                      if (as.numeric(substr(hold[[i]][1, j], 
                        1, 1)) < 3) {
                        hold[[i]][1, j] <- substr(hold[[i]][1, 
                          j], 1, 1)
                      }
                      else {
                        hold[[i]][1, j] <- substr(hold[[i]][1, 
                          j], nchar(hold[[i]][1, j]), nchar(hold[[i]][1, 
                          j]))
                      }
                    }
                  }
            }
            hold[[i]] <- data.frame(hold[[i]][-4, ])
        }
        return(hold)
    }
    combine_vectors <- function(lists, current = character()) {
        if (length(lists) == 0) {
            return(paste0(current, collapse = ""))
        }
        first <- lists[[1]]
        first_vals <- if (is.matrix(first) || is.data.frame(first)) {
            as.character(first[, 1])
        }
        else {
            as.character(first)
        }
        first_vals <- first_vals[!is.na(first_vals)]
        if (length(first_vals) == 0) {
            return(character(0))
        }
        result <- character(0)
        for (val in first_vals) {
            result <- c(result, combine_vectors(lists[-1], c(current, 
                val)))
        }
        return(result)
    }
    correspondences <- c(ju = "œ", je = "∑", jU = "®", j3 = "†", 
        ks = "¥", kS = "ø", nj = "π", gz = "å", gZ = "ß")
    correspondences_reverse <- c(œ = "ju", `∑` = "je", `®` = "jU", 
        `†` = "j3", `¥` = "ks", ø = "kS", π = "nj", å = "gz", 
        ß = "gZ")
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
    mymat <- list()
    position_mapping <- c(`1` = "wi", `2` = "si", `3` = "sm", 
        `4` = "sf", `5` = "wf")
    check_biphones <- matrix(NA, nrow = length(target))
    for (i in 1:length(target)) {
        check_biphones[i] <- grepl(phonemes_to_check, target[i], 
            ignore.case = FALSE)
    }
    for (i in 1:length(target)) {
        mylist1[[i]] <- prep_pword(i)
    }
    for (i in 1:length(target)) {
        mylist2[[i]] <- matrix(strsplit(mylist1[[i]][1, ], "")[[1]], 
            ncol = length(strsplit(mylist1[[i]][1, ], "")[[1]]))
        mylist2[[i]] <- rbind(mylist2[[i]], matrix(strsplit(mylist1[[i]][2, 
            ], "")[[1]], ncol = length(strsplit(mylist1[[i]][2, 
            ], "")[[1]])))
    }
    ifelse(level == "OR", mylist2 <- prep_pword_OR(mylist2), 
        ifelse(level == "OC", mylist2 <- prep_pword_OC(mylist2), 
            mylist2 <- mylist2))
    generate_spellings_from_matrix <- function(word_mat, threshold, 
        use_max = FALSE) {
        mytemp <- list()
        suppressWarnings({
            if (use_max) {
                for (j in 1:ncol(word_mat)) {
                  this_rows <- tables[["pg"]][tables[["pg"]]$phoneme == 
                    word_mat[2, j], ]
                  this_vals <- as.numeric(as.matrix(this_rows[, 
                    as.numeric(word_mat[1, j]) + 2, drop = FALSE]))
                  this_graph <- as.character(this_rows[[2]])
                  if (length(this_vals) == 0 || all(!is.finite(this_vals))) {
                    mytemp[[j]] <- character(0)
                  }
                  else {
                    max_val <- max(this_vals[is.finite(this_vals)])
                    mytemp[[j]] <- this_graph[which(this_vals == 
                      max_val)]
                  }
                }
            }
            else {
                for (j in 1:ncol(word_mat)) {
                  this_rows <- tables[["pg"]][tables[["pg"]]$phoneme == 
                    word_mat[2, j], ]
                  this_vals <- as.numeric(as.matrix(this_rows[, 
                    as.numeric(word_mat[1, j]) + 2, drop = FALSE]))
                  this_graph <- as.character(this_rows[[2]])
                  if (length(this_vals) == 0) {
                    mytemp[[j]] <- character(0)
                  }
                  else {
                    mytemp[[j]] <- this_graph[which(is.finite(this_vals) & 
                      this_vals > threshold)]
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
            if (level == "PG") 
                mat <- rbind(mat, mat[2, ])
            return(mat)
        }
        original_target <- target[i]
        on.exit({
            target[i] <<- original_target
        }, add = TRUE)
        target[i] <<- replace_patterns(target[i], correspondences)
        alt1 <- prep_pword(i)
        alt2 <- matrix(strsplit(alt1[1, ], "")[[1]], ncol = length(strsplit(alt1[1, 
            ], "")[[1]]))
        alt2 <- rbind(alt2, matrix(strsplit(alt1[2, ], "")[[1]], 
            ncol = length(strsplit(alt1[2, ], "")[[1]])))
        alt2 <- reverse_replace_patterns(alt2, correspondences_reverse)
        if (level == "PG") {
            alt2 <- rbind(alt2, alt2[2, ])
            return(alt2)
        }
        temp_list <- mylist2
        temp_list[[i]] <- alt2
        if (level == "OR") {
            temp_list <- prep_pword_OR(temp_list)
        }
        else if (level == "OC") {
            temp_list <- prep_pword_OC(temp_list)
        }
        temp_list[[i]]
    }
    for (i in 1:length(target)) {
        base_mat <- get_word_matrix(i, use_biphone = FALSE)
        use_max_mode <- (min_map == 1)
        base_spellings <- generate_spellings_from_matrix(base_mat, 
            min_map, use_max = use_max_mode)
        alt_spellings <- character(0)
        if (check_biphones[i] == TRUE) {
            alt_mat <- get_word_matrix(i, use_biphone = TRUE)
            alt_spellings <- generate_spellings_from_matrix(alt_mat, 
                min_map, use_max = use_max_mode)
        }
        combined_spellings <- unique(c(base_spellings, alt_spellings))
        if (length(combined_spellings) > max_options) {
            temp_min_map <- 0.05
            while (length(combined_spellings) > max_options && 
                temp_min_map < 0.51) {
                base_spellings <- generate_spellings_from_matrix(base_mat, 
                  temp_min_map, use_max = FALSE)
                if (check_biphones[i] == TRUE) {
                  alt_mat <- get_word_matrix(i, use_biphone = TRUE)
                  alt_spellings <- generate_spellings_from_matrix(alt_mat, 
                    temp_min_map, use_max = FALSE)
                }
                else {
                  alt_spellings <- character(0)
                }
                combined_spellings <- unique(c(base_spellings, 
                  alt_spellings))
                temp_min_map <- min(temp_min_map + 0.05, 0.5)
            }
        }
        mymat[[i]] <- combined_spellings
    }
    n_per_pron <- vapply(mymat, length, integer(1))
    all_spellings <- unlist(mymat, use.names = FALSE)
    if (length(all_spellings) == 0) {
        spellings_df <- data.frame(spelling = character(0), pronunciation = character(0), 
            stringsAsFactors = FALSE)
        source_idx <- integer(0)
        source_prons <- character(0)
    }
    else {
        source_idx <- rep(seq_along(mymat), times = n_per_pron)
        source_prons <- target[source_idx]
        spellings_df <- data.frame(spelling = all_spellings, 
            pronunciation = source_prons, stringsAsFactors = FALSE)
    }
    if (score == TRUE) {
        required_globals <- c("map_value", "summarize_words")
        missing_globals <- required_globals[!vapply(required_globals, 
            exists, logical(1), inherits = TRUE)]
        if (length(missing_globals) > 0) {
            stop("Missing required objects in the global environment: ", 
                paste(missing_globals, collapse = ", "))
        }
        if (length(all_spellings) == 0) {
            score_df <- data.frame(spelling = character(0), pronunciation = character(0), 
                mean = numeric(0), input_index = integer(0), 
                input_target = character(0), stringsAsFactors = FALSE)
            return(setNames(list(score_df), c(paste0("scores_", 
                tolower(selected_param)))))
        }
        if (score_row == "pp") {
            if (is.null(parsed_corpus)) {
                stop("parsed_corpus must be provided when requesting PP scores from pw_spell().")
            }
            if (is.null(PG_table)) {
                stop("PG_table must be provided when param = 'pp' in pw_spell().")
            }
            pp_tables <- PG_table
            mapped_scores <- map_value(spelling = all_spellings, 
                pronunciation = source_prons, level = "PG", tables = pp_tables, 
                progress = FALSE, parsed_corpus = parsed_corpus)
            score_df <- data.frame(spelling = vapply(mapped_scores, 
                function(x) as.character(x["spelling", 1]), character(1)), 
                pronunciation = vapply(mapped_scores, function(x) as.character(x["pronunciation", 
                  1]), character(1)), mean = suppressWarnings(as.numeric(vapply(mapped_scores, 
                  function(x) as.character(x["pp", 1]), character(1)))), 
                stringsAsFactors = FALSE)
            score_df <- score_df[is.finite(score_df$mean), , 
                drop = FALSE]
        }
        else {
            mapped_scores <- map_value(spelling = all_spellings, 
                pronunciation = source_prons, level = level, 
                tables = tables, progress = FALSE)
            score_df <- summarize_words(mapped_scores, score_row, 
                mode = summary_mode)
            if ("mean" %in% names(score_df)) {
                score_df <- score_df[!is.na(score_df$mean), , 
                  drop = FALSE]
            }
            else {
                keep_rows <- apply(score_df, 1, function(r) any(is.finite(suppressWarnings(as.numeric(r)))))
                score_df <- score_df[keep_rows, , drop = FALSE]
            }
        }
        if (nrow(score_df) > 0) {
            score_df$input_index <- source_idx[seq_len(nrow(score_df))]
            score_df$input_target <- source_prons[seq_len(nrow(score_df))]
        }
        else {
            score_df$input_index <- integer(0)
            score_df$input_target <- character(0)
        }
        if (nrow(score_df) > 0 && mean_score == 1 && "mean" %in% 
            names(score_df)) {
            score_df <- score_df[which.max(score_df$mean), , 
                drop = FALSE]
        }
        if (nrow(score_df) > 0 && mean_score < 1 && "mean" %in% 
            names(score_df)) {
            score_df <- score_df[score_df$mean >= mean_score, 
                , drop = FALSE]
        }
        return(setNames(list(score_df), c(paste0("scores_", tolower(selected_param)))))
    }
    return(spellings_df)
} 

# ============================================================ 
# pw_read 
# ============================================================ 
pw_read <- function (target, parsed_corpus, PG_table, OC_table = NULL, OR_table = NULL, 
    level = "PG", score = TRUE, min_pp = 1, min_map = 0.01, mean_score = 0, 
    max_options = 1000, param = "GP", summary_mode = c("default", 
        "ONC")) 
{
    require(stringr)
    require(data.tree)
    require(readxl)
    target <- as.character(target)
    bad_target <- !grepl("^[A-Za-z]+$", target)
    if (any(bad_target)) {
        stop("pw_read target must contain alphabetic characters only (A-Z). Invalid target(s): ", 
            paste(unique(target[bad_target]), collapse = ", "))
    }
    if (length(target) == 0) {
        return(list())
    }
    required_globals <- c("word_initial_mappings", "word_final_mappings", 
        "syllable_medial_mappings", "syllable_final_mappings", 
        "syllable_initial_mappings", "map_value", "summarize_words")
    missing_globals <- required_globals[!vapply(required_globals, 
        exists, logical(1), inherits = TRUE)]
    if (length(missing_globals) > 0) {
        stop("Missing required mapping objects in the global environment: ", 
            paste(missing_globals, collapse = ", "))
    }
    level <- toupper(level)
    if (level == "PG") {
        if (is.null(PG_table)) {
            stop("PG_table must be provided when level = 'PG'.")
        }
    }
    else if (level == "ALL") {
        if (is.null(PG_table) || is.null(OC_table) || is.null(OR_table)) {
            stop("When level = 'all', PG_table, OC_table, and OR_table must all be provided.")
        }
    }
    else {
        stop("Unknown level: ", level, ". Use 'PG' or 'all'.")
    }
    selected_param <- toupper(param)
    if (!selected_param %in% c("PG", "GP", "PG_FREQ", "P_FREQ", 
        "G_FREQ", "PP")) {
        stop("param must be one of: PG, GP, PG_freq, P_freq, G_freq, or pp.")
    }
    summary_mode <- match.arg(summary_mode)
    if (summary_mode == "ONC" && mean_score != 0) {
        stop("mean_score thresholding is only available when summary_mode = 'default'.")
    }
    score_row <- switch(selected_param, PG = "PG", GP = "GP", 
        PG_FREQ = "PG_freq", P_FREQ = "P_freq", G_FREQ = "G_freq", 
        PP = "pp")
    pg_score_col <- paste0("PG_", score_row)
    filter_with_ties <- function(df, metric_col, min_cut = NULL, 
        mean_score = 0, max_options = 1000, apply_mean = TRUE, 
        apply_min_cut = FALSE) {
        if (is.null(df) || nrow(df) == 0 || !(metric_col %in% 
            names(df))) 
            return(df)
        vals <- suppressWarnings(as.numeric(df[[metric_col]]))
        keep <- is.finite(vals)
        df <- df[keep, , drop = FALSE]
        vals <- vals[keep]
        if (length(vals) == 0) 
            return(df)
        if (apply_min_cut && !is.null(min_cut)) {
            if (isTRUE(all.equal(min_cut, 1))) {
                if ("pp" %in% names(df)) {
                  pp_vals <- suppressWarnings(as.numeric(df[["pp"]]))
                  has_pp <- is.finite(pp_vals)
                  keep <- rep(FALSE, length(vals))
                  if (any(has_pp)) {
                    pp_key <- formatC(pp_vals[has_pp], digits = 15, 
                      format = "fg", flag = "#")
                    idx_split <- split(which(has_pp), pp_key)
                    for (idx in idx_split) {
                      best_g <- max(vals[idx])
                      keep[idx] <- vals[idx] == best_g
                    }
                  }
                  if (any(!has_pp)) {
                    best_np <- max(vals[!has_pp])
                    keep[!has_pp] <- vals[!has_pp] == best_np
                  }
                }
                else {
                  best <- max(vals)
                  keep <- vals == best
                }
                df <- df[keep, , drop = FALSE]
                vals <- vals[keep]
            }
            else {
                keep <- vals >= min_cut
                df <- df[keep, , drop = FALSE]
                vals <- vals[keep]
            }
        }
        if (nrow(df) == 0) 
            return(df)
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
        if (nrow(df) == 0) 
            return(df)
        if (!is.null(max_options) && is.finite(max_options) && 
            max_options >= 1 && nrow(df) > max_options) {
            ord <- order(vals, decreasing = TRUE)
            cutoff <- vals[ord[min(max_options, length(ord))]]
            df <- df[vals >= cutoff, , drop = FALSE]
        }
        df
    }
    filter_with_onc_metric <- function(df, min_cut, mean_score, 
        max_options) {
        if (is.null(df) || nrow(df) == 0) 
            return(df)
        onc_cols <- intersect(c("mean_onset", "mean_nucleus", 
            "mean_coda"), names(df))
        if (length(onc_cols) == 0) 
            return(df)
        mat <- as.data.frame(lapply(df[, onc_cols, drop = FALSE], 
            function(x) suppressWarnings(as.numeric(x))))
        metric <- rowMeans(mat, na.rm = TRUE)
        metric[!is.finite(metric)] <- NA_real_
        df[["..onc_metric"]] <- metric
        df <- filter_with_ties(df, "..onc_metric", min_cut = min_cut, 
            mean_score = mean_score, max_options = max_options, 
            apply_mean = TRUE, apply_min_cut = TRUE)
        df[["..onc_metric"]] <- NULL
        df
    }
    filter_pron_candidates <- function(prons) {
        pr <- as.character(prons)
        if (length(pr) == 0) 
            return(logical(0))
        escape_regex_local <- function(x) gsub("([][{}()+*^$|\\\\?.])", 
            "\\\\\\1", x, perl = TRUE)
        vowels_filter <- c("5", "O", "8", "je", "j3r", "ju", 
            "jU", "o", "2", "@", "a", "e", "E", "3", "3r", "i", 
            "1", "c", "u", "U", "^")
        v_esc <- escape_regex_local(vowels_filter)
        v_esc <- v_esc[order(nchar(v_esc), decreasing = TRUE)]
        vowel_pat <- paste0("(", paste(v_esc, collapse = "|"), 
            ")")
        vowel_tokens <- regmatches(pr, gregexpr(vowel_pat, pr, 
            perl = TRUE))
        n_vowels <- lengths(vowel_tokens)
        has_vowel <- n_vowels >= 1
        only_schwa_e <- has_vowel & vapply(vowel_tokens, function(v) all(v == 
            "e"), logical(1))
        bad_bare3 <- grepl("3(?!r)", pr, perl = TRUE)
        bad_rr <- grepl("rr", pr, fixed = TRUE)
        has_vowel & !only_schwa_e & !bad_bare3 & !bad_rr
    }
    if (length(target) > 1) {
        out <- lapply(seq_along(target), function(i) {
            pw_read(target = target[i], parsed_corpus = parsed_corpus, 
                PG_table = PG_table, OC_table = OC_table, OR_table = OR_table, 
                level = level, score = score, min_pp = min_pp, 
                min_map = min_map, mean_score = mean_score, max_options = max_options, 
                param = param, summary_mode = summary_mode)
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
                scores_df <- if (length(score_dfs) > 0) 
                  do.call(rbind, score_dfs)
                else data.frame()
                setNames(list(scores_df), c(score_name))
            }), level_names)
            return(out_by_level)
        }
        if (level == "ALL" && !score) {
            level_names <- c("PG", "OC", "OR")
            out_by_level <- setNames(lapply(level_names, function(lvl) {
                pron_dfs <- lapply(out, function(x) x[[lvl]][["pronunciations"]])
                pronunciations_df <- if (length(pron_dfs) > 0) 
                  do.call(rbind, pron_dfs)
                else data.frame(spelling = character(0), pronunciation = character(0), 
                  stringsAsFactors = FALSE)
                list(pronunciations = pronunciations_df)
            }), level_names)
            return(out_by_level)
        }
        if (score) {
            score_name <- paste0("scores_", tolower(selected_param))
            score_dfs <- lapply(seq_along(out), function(i) {
                this_df <- out[[i]][[score_name]]
                if (!("input_index" %in% names(this_df))) 
                  this_df$input_index <- i
                if (!("input_target" %in% names(this_df))) 
                  this_df$input_target <- target[i]
                this_df$input_index <- i
                this_df$input_target <- target[i]
                this_df
            })
            scores_df <- if (length(score_dfs) > 0) 
                do.call(rbind, score_dfs)
            else data.frame()
            return(setNames(list(scores_df), c(score_name)))
        }
        pron_dfs <- out
        pronunciations_df <- if (length(pron_dfs) > 0) 
            do.call(rbind, pron_dfs)
        else data.frame(spelling = character(0), pronunciation = character(0), 
            stringsAsFactors = FALSE)
        return(pronunciations_df)
    }
    internal_mappings <- rbind(syllable_initial_mappings, syllable_medial_mappings, 
        syllable_final_mappings)
    parse_corpus_word_initial_mappings <- word_initial_mappings
    parse_corpus_word_final_mappings <- word_final_mappings
    parse_corpus_word_final_mappings[nrow(parse_corpus_word_final_mappings) + 
        1, ] <- parse_corpus_word_final_mappings[nrow(parse_corpus_word_final_mappings), 
        ]
    parse_corpus_word_final_mappings[nrow(parse_corpus_word_final_mappings), 
        ]$grapheme <- "h"
    parse_corpus_word_final_mappings[nrow(parse_corpus_word_final_mappings), 
        ]$phoneme <- "h"
    parse_corpus_word_final_mappings[nrow(parse_corpus_word_final_mappings) + 
        1, ] <- parse_corpus_word_final_mappings[nrow(parse_corpus_word_final_mappings), 
        ]
    parse_corpus_word_final_mappings[nrow(parse_corpus_word_final_mappings), 
        ]$grapheme <- "w"
    parse_corpus_word_final_mappings[nrow(parse_corpus_word_final_mappings), 
        ]$phoneme <- "w"
    parse_corpus_word_initial_mappings$grapheme <- toupper(parse_corpus_word_initial_mappings$grapheme)
    internal_mappings$grapheme <- toupper(internal_mappings$grapheme)
    parse_corpus_word_final_mappings$grapheme <- toupper(parse_corpus_word_final_mappings$grapheme)
    parsing_table <- parsed_corpus[[1]]
    reduced_table <- parsed_corpus[[2]]
    PG_table$gp <- dplyr::mutate(PG_table$gp, wi = as.numeric(wi), 
        si = as.numeric(si), sm = as.numeric(sm), sf = as.numeric(sf), 
        wf = as.numeric(wf))
    PG_table$gp <- dplyr::mutate(PG_table$gp, p_mid = pmax(ifelse(is.na(si), 
        0, si), ifelse(is.na(sm), 0, sm), ifelse(is.na(sf), 0, 
        sf)))
    parse_string <- function(startword) {
        consonants <- c("b", "c", "d", "f", "g", "h", "j", "k", 
            "l", "m", "n", "p", "q", "r", "s", "t", "v", "x", 
            "z")
        vowels <- c("a|e|i|o|u|y")
        remove_consonants <- c(B = "_", C = "_", D = "_", F = "_", 
            G = "_", H = "_", J = "_", K = "_", L = "_", M = "_", 
            N = "_", P = "_", Q = "_", R = "_", S = "_", T = "_", 
            V = "_", X = "_", Z = "_")
        remove_first <- function(word, char) {
            sub(char, "", word, fixed = TRUE)
        }
        process_string <- function(string, word) {
            before_underscore <- sub("_(.*)", "", string)
            remaining_word <- sub(paste0("^", before_underscore), 
                "", word)
            after_e <- sub("^.*?E", "", remaining_word)
            return(after_e)
        }
        prune_tree <- function(node) {
            children_to_check <- node$children
            for (child in children_to_check) {
                if (child$isLeaf) {
                  if (child$name == "W" | child$name == "H") {
                    node$RemoveChild(child$name)
                  }
                }
                else {
                  prune_tree(child)
                  if (length(child$children) == 0) {
                    node$RemoveChild(child$name)
                  }
                }
            }
        }
        myword <- toupper(startword)
        options_1_left <- myword
        options_2_left <- myword
        options_3_left <- myword
        options_4_left <- myword
        options_1e_left <- myword
        options_2e_left <- myword
        options_ue_left <- myword
        options_base_left <- myword
        myletters <- data.frame(str_split_fixed(myword, "", max(nchar(myword))))
        skeleton <- gsub("_+", "_", str_replace_all(myword, remove_consonants))
        myskeleton <- data.frame(str_split_fixed(skeleton, "", 
            max(nchar(skeleton))))
        options_1 <- unique(subset(parse_corpus_word_initial_mappings, 
            grapheme == paste0(myletters[, 1]))$grapheme)
        if (nchar(myword) > 1) {
            options_2 <- unique(subset(parse_corpus_word_initial_mappings, 
                grapheme == paste0(myletters[, 1], myletters[, 
                  2]))$grapheme)
        }
        else {
            options_2 <- NULL
        }
        if (nchar(myword) > 2) {
            options_3 <- unique(subset(parse_corpus_word_initial_mappings, 
                grapheme == paste0(myletters[, 1], myletters[, 
                  2], myletters[, 3]))$grapheme)
        }
        else {
            options_3 <- NULL
        }
        if (nchar(myword) > 3) {
            options_4 <- unique(subset(parse_corpus_word_initial_mappings, 
                grapheme == paste0(myletters[, 1], myletters[, 
                  2], myletters[, 3], myletters[, 4]))$grapheme)
        }
        else {
            options_4 <- NULL
        }
        if (ncol(myskeleton) > 2) {
            options_1e <- unique(subset(parse_corpus_word_initial_mappings, 
                grapheme == paste0(myskeleton[, 1], myskeleton[, 
                  2], myskeleton[, 3]))$grapheme)
        }
        else {
            options_1e <- NULL
        }
        if (length(options_1e) && grepl("_", options_1e)) {
            options_1e <- options_1e
        }
        else {
            options_1e <- NULL
        }
        if (ncol(myskeleton) > 3) {
            ifelse(ncol(myskeleton) > 3, options_2e <- unique(subset(parse_corpus_word_initial_mappings, 
                grapheme == paste0(myskeleton[, 1], myskeleton[, 
                  2], myskeleton[, 3], myskeleton[, 4]))$grapheme), 
                options_2e <- character(0))
        }
        else {
            options_2e <- NULL
        }
        if (length(options_2e) && grepl("_", options_2e)) {
            options_2e <- options_2e
        }
        else {
            options_2e <- NULL
        }
        if (ncol(myskeleton) > 3) {
            ifelse(ncol(myskeleton) > 3 & myskeleton[, 3] == 
                "U", options_ue <- unique(subset(parse_corpus_word_initial_mappings, 
                grapheme == paste0(myskeleton[, 1], myskeleton[, 
                  2], myskeleton[, 4]))$grapheme), options_ue <- character(0))
        }
        else {
            options_ue <- NULL
        }
        if (length(options_ue) && grepl("_", options_ue)) {
            options_ue <- options_ue
        }
        else {
            options_ue <- NULL
        }
        if (length(options_1)) 
            options_1_left <- sub(options_1, "", myword)
        else options_1_left <- NULL
        if (length(options_2)) 
            options_2_left <- sub(options_2, "", myword)
        else options_2_left <- NULL
        if (length(options_3)) 
            options_3_left <- sub(options_3, "", myword)
        else options_3_left <- NULL
        if (length(options_4)) 
            options_4_left <- sub(options_4, "", myword)
        else options_4_left <- NULL
        if (length(options_1e)) {
            pattern_before_underscore <- strsplit(sub("_.*", 
                "", options_1e), NULL)[[1]]
            for (char in pattern_before_underscore) {
                options_1e_left <- sub(char, "", options_1e_left, 
                  fixed = TRUE)
            }
            options_1e_left <- sub("E", "_", options_1e_left)
        }
        if (!grepl(vowels, options_1e_left, ignore.case = TRUE)) {
            if (process_string(options_1e, options_base_left) == 
                "") {
                options_1e_left <- options_1e_left
            }
            else {
                options_1e_left <- options_base_left
                options_1e <- character(0)
            }
        }
        else {
            options_1e_left <- options_1e_left
        }
        if (length(options_2e)) {
            pattern_before_underscore <- strsplit(sub("_.*", 
                "", options_2e), NULL)[[1]]
            for (char in pattern_before_underscore) {
                options_2e_left <- sub(char, "", options_2e_left, 
                  fixed = TRUE)
            }
            options_2e_left <- sub("E", "_", options_2e_left)
        }
        if (!grepl(vowels, options_2e_left, ignore.case = TRUE)) {
            if (process_string(options_2e, options_base_left) == 
                "") {
                options_2e_left <- options_2e_left
            }
            else {
                options_2e_left <- options_base_left
                options_2e <- character(0)
            }
        }
        else {
            options_2e_left <- options_2e_left
        }
        if (length(options_ue)) {
            pattern_before_underscore <- strsplit(sub("_.*", 
                "", options_ue), NULL)[[1]]
            for (char in pattern_before_underscore) {
                options_ue_left <- sub(char, "", options_ue_left, 
                  fixed = TRUE)
            }
            options_ue_left <- sub("E", "_", options_ue_left)
        }
        if (!grepl(vowels, options_ue_left, ignore.case = TRUE)) {
            if (process_string(options_ue, options_base_left) == 
                "") {
                options_ue_left <- options_ue_left
            }
            else {
                options_ue_left <- options_base_left
                options_ue <- character(0)
            }
        }
        else {
            options_ue_left <- options_ue_left
        }
        mytree <- Node$new(myword)
        wi1 <- mytree$AddChild(options_1)
        if (length(options_2) > 0) {
            wi2 <- mytree$AddChild(options_2)
        }
        if (length(options_3) > 0) {
            wi3 <- mytree$AddChild(options_3)
        }
        if (length(options_4) > 0) {
            wi4 <- mytree$AddChild(options_4)
        }
        if (length(options_1e) > 0) {
            wi1e <- mytree$AddChild(options_1e)
        }
        if (length(options_2e) > 0) {
            wi2e <- mytree$AddChild(options_2e)
        }
        if (length(options_ue) > 0) {
            wiue <- mytree$AddChild(options_ue)
        }
        lefttree <- Node$new(myword)
        if (options_1_left == "") {
            wi1 <- lefttree$AddChild("9")
        }
        else {
            wi1 <- lefttree$AddChild(options_1_left)
        }
        if (length(options_2) > 0) {
            if (options_2_left == "") {
                wi2 <- lefttree$AddChild("9")
            }
            else {
                wi2 <- lefttree$AddChild(options_2_left)
            }
        }
        if (length(options_3) > 0) {
            if (options_3_left == "") {
                wi3 <- lefttree$AddChild("9")
            }
            else {
                wi3 <- lefttree$AddChild(options_3_left)
            }
        }
        if (length(options_4) > 0) {
            if (options_4_left == "") {
                wi4 <- lefttree$AddChild("9")
            }
            else {
                wi4 <- lefttree$AddChild(options_4_left)
            }
        }
        if (length(options_1e) > 0) {
            if (options_1e_left == "") {
                wi1e <- lefttree$AddChild("9")
            }
            else {
                wi1e <- lefttree$AddChild(options_1e_left)
            }
        }
        if (length(options_2e) > 0) {
            if (options_2e_left == "") {
                wi2e <- lefttree$AddChild("9")
            }
            else {
                wi2e <- lefttree$AddChild(options_2e_left)
            }
        }
        if (length(options_ue) > 0) {
            if (options_ue_left == "") {
                wiue <- lefttree$AddChild("9")
            }
            else {
                wiue <- lefttree$AddChild(options_ue_left)
            }
        }
        counter <- 1
        while (any(as.matrix(as.data.frame(lefttree$leaves)) != 
            "9")) {
            if (counter > length(mytree$leaves)) {
                counter <- 1
            }
            while (lefttree$leaves[[counter]]$name == "9") {
                counter <- counter + 1
            }
            my_leaf <- lefttree$leaves[[counter]]$name
            my_leaf <- sub("_$", "", my_leaf)
            mytree_path <- mytree$leaves[[counter]]$path[-1]
            lefttree_path <- lefttree$leaves[[counter]]$path[-1]
            if (my_leaf != "9") {
                internal_options_1_left <- my_leaf
                internal_options_2_left <- my_leaf
                internal_options_3_left <- my_leaf
                internal_options_4_left <- my_leaf
                internal_options_5_left <- my_leaf
                internal_options_1e_left <- my_leaf
                internal_options_2e_left <- my_leaf
                internal_options_ue_left <- my_leaf
                internal_options_2e2_left <- my_leaf
                internal_options_base_left <- my_leaf
                myletters_left <- data.frame(str_split_fixed(my_leaf, 
                  "", max(nchar(my_leaf))))
                skeleton_left <- gsub("_+", "_", str_replace_all(my_leaf, 
                  remove_consonants))
                myskeleton_left <- data.frame(str_split_fixed(skeleton_left, 
                  "", max(nchar(skeleton_left))))
                if (ncol(myletters_left) - as.numeric("_" %in% 
                  myletters_left) == 1) {
                  check_mappings <- parse_corpus_word_final_mappings
                }
                else {
                  check_mappings <- internal_mappings
                }
                if (length(myletters_left) > 0) {
                  internal_options_1 <- unique(subset(check_mappings, 
                    grapheme == paste0(myletters_left[, 1]))$grapheme)
                }
                else {
                  internal_options_1 <- NULL
                }
                if (ncol(myletters_left) - as.numeric("_" %in% 
                  myletters_left) == 2) {
                  check_mappings <- parse_corpus_word_final_mappings
                }
                else {
                  check_mappings <- internal_mappings
                }
                if (length(myletters_left) > 1) {
                  internal_options_2 <- unique(subset(check_mappings, 
                    grapheme == paste0(myletters_left[, 1], myletters_left[, 
                      2]))$grapheme)
                }
                else {
                  internal_options_2 <- NULL
                }
                if (ncol(myletters_left) - as.numeric("_" %in% 
                  myletters_left) == 3) {
                  check_mappings <- parse_corpus_word_final_mappings
                }
                else {
                  check_mappings <- internal_mappings
                }
                if (length(myletters_left) > 2) {
                  internal_options_3 <- unique(subset(check_mappings, 
                    grapheme == paste0(myletters_left[, 1], myletters_left[, 
                      2], myletters_left[, 3]))$grapheme)
                }
                else {
                  internal_options_3 <- NULL
                }
                if (ncol(myletters_left) - as.numeric("_" %in% 
                  myletters_left) == 4) {
                  check_mappings <- parse_corpus_word_final_mappings
                }
                else {
                  check_mappings <- internal_mappings
                }
                if (length(myletters_left) > 3) {
                  internal_options_4 <- unique(subset(check_mappings, 
                    grapheme == paste0(myletters_left[, 1], myletters_left[, 
                      2], myletters_left[, 3], myletters_left[, 
                      4]))$grapheme)
                }
                else {
                  internal_options_4 <- NULL
                }
                if (ncol(myletters_left) - as.numeric("_" %in% 
                  myletters_left) == 5) {
                  check_mappings <- parse_corpus_word_final_mappings
                }
                else {
                  check_mappings <- internal_mappings
                }
                if (length(myletters_left) > 4) {
                  internal_options_5 <- unique(subset(check_mappings, 
                    grapheme == paste0(myletters_left[, 1], myletters_left[, 
                      2], myletters_left[, 3], myletters_left[, 
                      4], myletters_left[, 5]))$grapheme)
                }
                else {
                  internal_options_5 <- NULL
                }
                my_leaf <- gsub("_", "", my_leaf)
                ifelse(ncol(myskeleton_left) > 2, internal_options_1e <- unique(subset(internal_mappings, 
                  grapheme == paste0(myskeleton_left[, 1], myskeleton_left[, 
                    2], myskeleton_left[, 3]))$grapheme), internal_options_1e <- character(0))
                if (length(internal_options_1e) && grepl("_", 
                  internal_options_1e)) {
                  internal_options_1e <- internal_options_1e
                }
                else {
                  internal_options_1e <- character(0)
                }
                ifelse(ncol(myskeleton_left) > 3, internal_options_2e <- unique(subset(internal_mappings, 
                  grapheme == paste0(myskeleton_left[, 1], myskeleton_left[, 
                    2], myskeleton_left[, 3], myskeleton_left[, 
                    4]))$grapheme), internal_options_2e <- character(0))
                if (length(internal_options_2e) && grepl("_", 
                  internal_options_2e)) {
                  internal_options_2e <- internal_options_2e
                }
                else {
                  internal_options_2e <- character(0)
                }
                ifelse(ncol(myskeleton_left) > 3, ifelse(myskeleton_left[, 
                  3] == "U", internal_options_ue <- unique(subset(internal_mappings, 
                  grapheme == paste0(myskeleton_left[, 1], myskeleton_left[, 
                    2], myskeleton_left[, 4]))$grapheme), internal_options_ue <- character(0)), 
                  internal_options_ue <- character(0))
                if (length(internal_options_ue) && grepl("_", 
                  internal_options_ue)) {
                  internal_options_ue <- internal_options_ue
                }
                else {
                  internal_options_ue <- character(0)
                }
                if (ncol(myskeleton_left) > 3) {
                  if (myskeleton_left[, 4] == "U" && ncol(myskeleton_left) > 
                    3) {
                    ifelse(ncol(myskeleton_left) > 4, internal_options_2e2 <- unique(subset(internal_mappings, 
                      grapheme == paste0(myskeleton_left[, 1], 
                        myskeleton_left[, 2], myskeleton_left[, 
                          3], myskeleton_left[, 5]))$grapheme), 
                      internal_options_2e2 <- character(0))
                    if (length(internal_options_2e2) && grepl("_", 
                      internal_options_2e2)) {
                      internal_options_2e2 <- internal_options_2e2
                    }
                    else {
                      internal_options_2e2 <- character(0)
                    }
                  }
                  else {
                    internal_options_2e2 <- character(0)
                  }
                }
                else {
                  internal_options_2e2 <- character(0)
                }
                if (length(internal_options_1)) 
                  internal_options_1_left <- sub(internal_options_1, 
                    "", my_leaf)
                else internal_options_1_left <- internal_options_1_left
                if (length(internal_options_2)) 
                  internal_options_2_left <- sub(internal_options_2, 
                    "", my_leaf)
                else internal_options_2_left <- internal_options_2_left
                if (length(internal_options_3)) 
                  internal_options_3_left <- sub(internal_options_3, 
                    "", my_leaf)
                else internal_options_3_left <- internal_options_3_left
                if (length(internal_options_4)) 
                  internal_options_4_left <- sub(internal_options_4, 
                    "", my_leaf)
                else internal_options_4_left <- internal_options_4_left
                if (length(internal_options_5)) 
                  internal_options_5_left <- sub(internal_options_5, 
                    "", my_leaf)
                else internal_options_5_left <- internal_options_5_left
                if (length(internal_options_1e)) {
                  pattern_before_underscore <- strsplit(sub("_.*", 
                    "", internal_options_1e), NULL)[[1]]
                  for (char in pattern_before_underscore) {
                    internal_options_1e_left <- sub(char, "", 
                      internal_options_1e_left, fixed = TRUE)
                  }
                  internal_options_1e_left <- sub("E", "_", internal_options_1e_left)
                }
                if (length(internal_options_1e) > 0) {
                  if (!grepl("_E$", internal_options_1e)) {
                    internal_options_1e <- character(0)
                  }
                }
                if (!grepl(vowels, internal_options_1e_left, 
                  ignore.case = TRUE)) {
                  if (process_string(internal_options_1e, internal_options_base_left) == 
                    "") {
                    internal_options_1e_left <- internal_options_1e_left
                  }
                  else {
                    internal_options_1e_left <- internal_options_base_left
                    internal_options_1e <- character(0)
                  }
                }
                else {
                  internal_options_1e_left <- internal_options_1e_left
                }
                if (length(internal_options_2e)) {
                  pattern_before_underscore <- strsplit(sub("_.*", 
                    "", internal_options_2e), NULL)[[1]]
                  for (char in pattern_before_underscore) {
                    internal_options_2e_left <- sub(char, "", 
                      internal_options_2e_left, fixed = TRUE)
                  }
                  internal_options_2e_left <- sub("E", "_", internal_options_2e_left)
                }
                if (length(internal_options_2e) > 0) {
                  if (!grepl("_E$", internal_options_2e)) {
                    internal_options_2e <- character(0)
                  }
                }
                if (!grepl(vowels, internal_options_2e_left, 
                  ignore.case = TRUE)) {
                  if (process_string(internal_options_2e, internal_options_base_left) == 
                    "") {
                    internal_options_2e_left <- internal_options_2e_left
                  }
                  else {
                    internal_options_2e_left <- internal_options_base_left
                    internal_options_2e <- character(0)
                  }
                }
                else {
                  internal_options_2e_left <- internal_options_2e_left
                }
                if (length(internal_options_ue)) {
                  pattern_before_underscore <- strsplit(sub("_.*", 
                    "", internal_options_ue), NULL)[[1]]
                  for (char in pattern_before_underscore) {
                    internal_options_ue_left <- sub(char, "", 
                      internal_options_ue_left, fixed = TRUE)
                  }
                  internal_options_ue_left <- sub("E", "_", internal_options_ue_left)
                }
                if (length(internal_options_ue) > 0) {
                  if (!grepl("_E$", internal_options_ue)) {
                    internal_options_ue <- character(0)
                  }
                }
                if (!grepl(vowels, internal_options_ue_left, 
                  ignore.case = TRUE)) {
                  if (process_string(internal_options_ue, internal_options_base_left) == 
                    "") {
                    internal_options_ue_left <- internal_options_ue_left
                  }
                  else {
                    internal_options_ue_left <- internal_options_base_left
                    internal_options_ue <- character(0)
                  }
                }
                else {
                  internal_options_ue_left <- internal_options_ue_left
                }
                if (length(internal_options_2e2)) {
                  pattern_before_underscore <- strsplit(sub("_.*", 
                    "", internal_options_2e2), NULL)[[1]]
                  for (char in pattern_before_underscore) {
                    internal_options_2e2_left <- sub(char, "", 
                      internal_options_2e2_left, fixed = TRUE)
                  }
                  internal_options_2e2_left <- sub("E", "_", 
                    internal_options_2e2_left)
                }
                if (length(internal_options_2e2) > 0) {
                  if (!grepl("_E$", internal_options_2e2)) {
                    internal_options_2e2 <- character(0)
                  }
                }
                if (!grepl(vowels, internal_options_2e2_left, 
                  ignore.case = TRUE)) {
                  if (process_string(internal_options_2e2, internal_options_base_left) == 
                    "") {
                    internal_options_2e2_left <- internal_options_2e2_left
                  }
                  else {
                    internal_options_2e2_left <- internal_options_base_left
                    internal_options_2e2 <- character(0)
                  }
                }
                else {
                  internal_options_2e2_left <- internal_options_2e2_left
                }
                if (length(internal_options_1) > 0) {
                  internal <- mytree$Climb(mytree_path)$AddChild(internal_options_1)
                }
                if (length(internal_options_2) > 0) {
                  internal <- mytree$Climb(mytree_path)$AddChild(internal_options_2)
                }
                if (length(internal_options_3) > 0) {
                  internal <- mytree$Climb(mytree_path)$AddChild(internal_options_3)
                }
                if (length(internal_options_4) > 0) {
                  internal <- mytree$Climb(mytree_path)$AddChild(internal_options_4)
                }
                if (length(internal_options_5) > 0) {
                  internal <- mytree$Climb(mytree_path)$AddChild(internal_options_5)
                }
                if (length(internal_options_1e) > 0) {
                  internal <- mytree$Climb(mytree_path)$AddChild(internal_options_1e)
                }
                if (length(internal_options_2e) > 0) {
                  internal <- mytree$Climb(mytree_path)$AddChild(internal_options_2e)
                }
                if (length(internal_options_ue) > 0) {
                  internal <- mytree$Climb(mytree_path)$AddChild(internal_options_ue)
                }
                if (length(internal_options_2e2) > 0) {
                  internal <- mytree$Climb(mytree_path)$AddChild(internal_options_2e2)
                }
                if (length(internal_options_1) > 0) {
                  if (ncol(myletters_left) == 1) {
                    internal <- lefttree$Climb(lefttree_path)$AddChild("9")
                  }
                  else {
                    internal <- lefttree$Climb(lefttree_path)$AddChild(internal_options_1_left)
                  }
                }
                if (length(internal_options_2) > 0) {
                  if (ncol(myletters_left) == 2) {
                    internal <- lefttree$Climb(lefttree_path)$AddChild("9")
                  }
                  else {
                    internal <- lefttree$Climb(lefttree_path)$AddChild(internal_options_2_left)
                  }
                }
                if (length(internal_options_3) > 0) {
                  if (ncol(myletters_left) == 3) {
                    internal <- lefttree$Climb(lefttree_path)$AddChild("9")
                  }
                  else {
                    internal <- lefttree$Climb(lefttree_path)$AddChild(internal_options_3_left)
                  }
                }
                if (length(internal_options_4) > 0) {
                  if (ncol(myletters_left) == 4) {
                    internal <- lefttree$Climb(lefttree_path)$AddChild("9")
                  }
                  else {
                    internal <- lefttree$Climb(lefttree_path)$AddChild(internal_options_4_left)
                  }
                }
                if (length(internal_options_5) > 0) {
                  if (ncol(myletters_left) == 5) {
                    internal <- lefttree$Climb(lefttree_path)$AddChild("9")
                  }
                  else {
                    internal <- lefttree$Climb(lefttree_path)$AddChild(internal_options_5_left)
                  }
                }
                if (length(internal_options_1e) > 0) {
                  internal <- lefttree$Climb(lefttree_path)$AddChild(internal_options_1e_left)
                }
                if (length(internal_options_2e) > 0) {
                  internal <- lefttree$Climb(lefttree_path)$AddChild(internal_options_2e_left)
                }
                if (length(internal_options_ue) > 0) {
                  internal <- lefttree$Climb(lefttree_path)$AddChild(internal_options_ue_left)
                }
                if (length(internal_options_2e2) > 0) {
                  internal <- lefttree$Climb(lefttree_path)$AddChild(internal_options_2e2_left)
                }
            }
            if (lefttree$leaves[[counter]]$name == "9") {
                counter <- counter + 1
            }
        }
        prune_tree(mytree)
        return(mytree)
    }
    extract_branches <- function(node, path = c()) {
        path <- c(path, node$name)
        if (node$isLeaf) {
            branch <- paste(path, collapse = "-")
            original_word <- sub("-.*", "", branch)
            graphemes <- unlist(strsplit(sub(paste0(original_word, 
                "-"), "", branch), "-"))
            modified_branches <- list(branch)
            le_indices <- which(graphemes == "L" & c(graphemes[-1], 
                "") == "E")
            if (length(le_indices) > 0 && le_indices[1] == 1) {
                le_indices <- le_indices[-1]
            }
            if (length(le_indices) > 0) {
                for (i in seq_along(le_indices)) {
                  modified_graphemes <- graphemes
                  modified_graphemes[le_indices[i]] <- "_E"
                  modified_graphemes[le_indices[i] + 1] <- "L"
                  modified_branch <- paste(c(original_word, modified_graphemes), 
                    collapse = "-")
                  modified_branches <- c(modified_branches, modified_branch)
                }
            }
            re_indices <- which(graphemes == "R" & c(graphemes[-1], 
                "") == "E")
            if (length(re_indices) > 0 && re_indices[1] == 1) {
                re_indices <- re_indices[-1]
            }
            if (length(re_indices) > 0) {
                for (i in seq_along(re_indices)) {
                  modified_graphemes <- graphemes
                  modified_graphemes[re_indices[i]] <- "_E"
                  modified_graphemes[re_indices[i] + 1] <- "R"
                  modified_branch <- paste(c(original_word, modified_graphemes), 
                    collapse = "-")
                  modified_branches <- c(modified_branches, modified_branch)
                }
            }
            one_indices <- suppressWarnings(which(graphemes == 
                "O" & c(graphemes[-1], "") == "N" & c(graphemes[-(1:2)], 
                "") == "E"))
            if (length(one_indices) > 0) {
                for (i in seq_along(one_indices)) {
                  modified_graphemes <- graphemes
                  modified_graphemes[one_indices[i]] <- "O"
                  modified_graphemes[one_indices[i] + 1] <- "_E"
                  modified_graphemes[one_indices[i] + 2] <- "N"
                  modified_branch <- paste(c(original_word, modified_graphemes), 
                    collapse = "-")
                  modified_branches <- c(modified_branches, modified_branch)
                }
            }
            once_indices <- suppressWarnings(which(graphemes == 
                "O" & c(graphemes[-1], "") == "N" & c(graphemes[-(1:2)], 
                "") == "C" & c(graphemes[-(1:3)], "") == "E"))
            if (length(once_indices) > 0) {
                for (i in seq_along(once_indices)) {
                  modified_graphemes <- graphemes
                  modified_graphemes[once_indices[i]] <- "O"
                  modified_graphemes[once_indices[i] + 1] <- "_E"
                  modified_graphemes[once_indices[i] + 2] <- "N"
                  modified_graphemes[once_indices[i] + 3] <- "C"
                  modified_branch <- paste(c(original_word, modified_graphemes), 
                    collapse = "-")
                  modified_branches <- c(modified_branches, modified_branch)
                }
            }
            return(modified_branches)
        }
        else {
            branches <- list()
            for (child in node$children) {
                branches <- c(branches, extract_branches(child, 
                  path))
            }
            return(branches)
        }
    }
    count_letters_and_extract_graphemes <- function(branches) {
        original_word <- sub("-.*", "", branches[[1]])
        letter_counts <- table(strsplit(original_word, "")[[1]])
        grapheme_branches <- lapply(branches, function(x) {
            graphemes <- unlist(strsplit(sub("^[^-]*-", "", x), 
                "-"))
            return(graphemes)
        })
        return(list(letter_counts = letter_counts, grapheme_branches = grapheme_branches))
    }
    extract_graphemes_for_letters <- function(branches) {
        results <- count_letters_and_extract_graphemes(branches)
        grapheme_branches <- results$grapheme_branches
        letter_occurrences <- list()
        letter_total_counts <- as.list(results$letter_counts)
        results_df <- data.frame(Letter_Occurrence = character(), 
            Graphemes = character(), Position = character(), 
            stringsAsFactors = FALSE)
        for (branch in grapheme_branches) {
            branch_letter_counts <- setNames(as.list(rep(0, length(letter_total_counts))), 
                names(letter_total_counts))
            for (i in seq_along(branch)) {
                grapheme <- branch[i]
                position <- if (i == 1) {
                  "wi"
                }
                else if (i == length(branch)) {
                  "wf"
                }
                else {
                  "wm"
                }
                grapheme_letters <- unlist(strsplit(grapheme, 
                  ""))
                grapheme_letters <- grapheme_letters[grapheme_letters != 
                  "_"]
                for (letter in grapheme_letters) {
                  branch_letter_counts[[letter]] <- branch_letter_counts[[letter]] + 
                    1
                  occurrence_key <- paste0(letter, branch_letter_counts[[letter]])
                  results_df <- rbind(results_df, data.frame(Letter_Occurrence = occurrence_key, 
                    Graphemes = grapheme, Position = position, 
                    stringsAsFactors = FALSE))
                }
            }
        }
        return(results_df)
    }
    make_options <- function(allbranches_df, PG_table, top_parse, 
        min_map) {
        temp <- add_grapheme_index(allbranches_df)
        allbranches_df$index2 <- paste0(temp$grapheme_index, 
            "_", temp$Position)
        top_graphemes <- cbind(allbranches_df[allbranches_df$parse_no == 
            top_parse, ]$Graphemes, allbranches_df[allbranches_df$parse_no == 
            top_parse, ]$Position, allbranches_df[allbranches_df$parse_no == 
            top_parse, ]$index2)
        top_graphemes <- top_graphemes[!duplicated(top_graphemes), 
            , drop = FALSE]
        gtop_options <- list()
        num_graph <- nrow(top_graphemes)
        for (j in seq_len(num_graph)) {
            gtop_options[[j]] <- PG_table$gp[PG_table$gp$grapheme == 
                tolower(top_graphemes[j, 1]), ]
        }
        if (num_graph >= 1L && nrow(gtop_options[[1]]) > 0L) {
            gtop_options[[1]] <- gtop_options[[1]][gtop_options[[1]]$wi >= 
                min(min_map, max(gtop_options[[1]]$wi)), , drop = FALSE]
        }
        if (num_graph >= 1L && nrow(gtop_options[[num_graph]]) > 
            0L) {
            gtop_options[[num_graph]] <- gtop_options[[num_graph]][gtop_options[[num_graph]]$wf >= 
                min(min_map, max(gtop_options[[num_graph]]$wf)), 
                , drop = FALSE]
        }
        if (num_graph > 2L) {
            for (j in 2:(num_graph - 1L)) {
                if (nrow(gtop_options[[j]]) > 0L) {
                  gtop_options[[j]] <- gtop_options[[j]][gtop_options[[j]]$p_mid >= 
                    min(min_map, max(gtop_options[[j]]$p_mid)), 
                    , drop = FALSE]
                }
            }
        }
        phon_options <- lapply(gtop_options, function(x) x$phoneme)
        phon_options <- expand.grid(phon_options, stringsAsFactors = FALSE)
        phon_options <- apply(phon_options, 1, paste, collapse = "")
        return(phon_options)
    }
    add_parse_number <- function(output_df, string_length) {
        total_rows <- nrow(output_df)
        parse_no <- rep(1:(total_rows/string_length), each = string_length)
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
            current_counter <- 1
            accumulated_letters <- ""
            accumulated_graphemes <- ""
            for (i in min(rows):max(rows)) {
                accumulated_letters <- paste0(accumulated_letters, 
                  substr(df$Letter_Occurrence[i], 1, 1))
                if (set_flag == FALSE) {
                  accumulated_graphemes <- paste0(accumulated_graphemes, 
                    gsub("_", "", df$Graphemes[i]))
                }
                df$grapheme_index[i] <- paste0(current_counter, 
                  df$Graphemes[i])
                if (accumulated_letters == accumulated_graphemes) {
                  current_counter <- current_counter + 1
                  set_flag <- FALSE
                }
                else {
                  set_flag <- TRUE
                }
            }
        }
        return(df)
    }
    branches <- extract_branches(parse_string(target))
    if (grepl("es$", target)) {
        target2 <- substr(target, 1, nchar(target) - 1)
        branches2 <- extract_branches(parse_string(target2))
        branches2 <- lapply(branches2, function(branch) {
            if (grepl("-", branch)) {
                branch <- sub("-", "S-", branch)
            }
            paste(branch, "-S", sep = "")
        })
        branches <- unique(c(branches, branches2))
    }
    if (grepl("ed$", target)) {
        target2 <- substr(target, 1, nchar(target) - 1)
        branches2 <- extract_branches(parse_string(target2))
        branches2 <- lapply(branches2, function(branch) {
            if (grepl("-", branch)) {
                branch <- sub("-", "D-", branch)
            }
            paste(branch, "-D", sep = "")
        })
        branches <- unique(c(branches, branches2))
    }
    allbranches_df <- extract_graphemes_for_letters(branches)
    allbranches_df <- add_parse_number(allbranches_df, nchar(target))
    allbranches_df$index1 <- paste0(allbranches_df$Letter_Occurrence, 
        allbranches_df$Graphemes, "_", allbranches_df$Position)
    allbranches_df$index2 <- paste0(allbranches_df$Graphemes, 
        "_", allbranches_df$Position)
    allbranches_df$choices <- allbranches_df$index2
    for (j in seq_len(length(allbranches_df$Letter_Occurrence))) {
        allbranches_df$choices[j] <- paste0(sort(unique(allbranches_df[allbranches_df$Letter_Occurrence == 
            allbranches_df[j, ]$Letter_Occurrence, ]$index2)), 
            collapse = ",")
    }
    allbranches_df$suffix_class <- ifelse(grepl("ed$", target, 
        ignore.case = TRUE), "EDERES", ifelse(grepl("er$", target, 
        ignore.case = TRUE), "EDERES", ifelse(grepl("es$", target, 
        ignore.case = TRUE), "EDERES", "OTHER")))
    allbranches_df$order <- seq_len(nrow(allbranches_df))
    allbranches_df <- merge(allbranches_df, reduced_table, by = "choices", 
        all.x = T)
    allbranches_df$Letter <- substring(allbranches_df$Letter_Occurrence, 
        1, 1)
    allbranches_df$index3 <- paste0(allbranches_df$Letter, "_in_", 
        allbranches_df$index2, "_and_", allbranches_df$reduced, 
        "_SC_", allbranches_df$suffix_class)
    allbranches_df <- merge(allbranches_df, parsing_table[, -2], 
        by = "index3", all.x = T)
    allbranches_df <- allbranches_df[order(allbranches_df$order), 
        ]
    allbranches_df$order <- NULL
    allbranches_df$pp[is.na(allbranches_df$pp)] <- 0
    allbranches_df$pp_freq[is.na(allbranches_df$pp_freq)] <- 0
    if (nrow(allbranches_df) == 0) {
        pp_means <- data.frame(parse_no = integer(0), pp = numeric(0), 
            stringsAsFactors = FALSE)
    }
    else {
        pp_means <- aggregate(data = allbranches_df, FUN = mean, 
            pp ~ parse_no)
    }
    gen_min_map <- if (score == TRUE) 
        0
    else min_map
    if (nrow(pp_means) == 0) {
        phon_options <- character(0)
    }
    else if (min_pp == 1) {
        top_parse <- which.max(pp_means$pp)
        phon_options <- make_options(allbranches_df, PG_table, 
            top_parse, gen_min_map)
        check_options <- length(phon_options)
        if (check_options > max_options && score == TRUE) {
            temp_min_map <- 0.05
            while (check_options > max_options && temp_min_map < 
                0.51) {
                phon_options <- make_options(allbranches_df, 
                  PG_table, top_parse, temp_min_map)
                check_options <- length(phon_options)
                temp_min_map <- min(temp_min_map + 0.05, 0.5)
            }
        }
        pp_means <- pp_means[which.max(pp_means$pp), ]
    }
    else {
        top_parses <- which(pp_means$pp >= min_pp)
        temp <- add_grapheme_index(allbranches_df)
        allbranches_df$index2 <- paste0(temp$grapheme_index, 
            "_", temp$Position)
        phon_options <- vector("list", length(top_parses))
        for (k in seq_len(length(top_parses))) {
            top_parse <- top_parses[k]
            phon_options[[k]] <- make_options(allbranches_df, 
                PG_table, top_parse, gen_min_map)
            check_options <- length(phon_options[[k]])
            if (check_options > max_options && score == TRUE) {
                temp_min_map <- 0.05
                while (check_options > max_options && temp_min_map < 
                  0.51) {
                  phon_options[[k]] <- make_options(allbranches_df, 
                    PG_table, top_parse, temp_min_map)
                  check_options <- length(phon_options[[k]])
                  temp_min_map <- min(temp_min_map + 0.05, 0.5)
                }
            }
        }
        pp_means <- pp_means[pp_means$parse_no %in% top_parses, 
            , drop = FALSE]
    }
    all_prons_raw <- if (min_pp == 1) 
        phon_options
    else unlist(phon_options, use.names = FALSE)
    n_per_parse <- if (min_pp == 1) 
        length(phon_options)
    else vapply(phon_options, length, integer(1))
    pp_aligned_raw <- rep(pp_means$pp, times = n_per_parse)
    keep_prons <- filter_pron_candidates(all_prons_raw)
    all_prons <- all_prons_raw[keep_prons]
    pp_aligned <- pp_aligned_raw[keep_prons]
    pronunciations_df <- data.frame(spelling = rep(target, length(all_prons)), 
        pronunciation = all_prons, stringsAsFactors = FALSE)
    if (score == TRUE) {
        if (score_row == "pp") {
            gp_scores <- data.frame(spelling = rep(target, length(all_prons)), 
                pronunciation = all_prons, mean = pp_aligned, 
                pp = pp_aligned, stringsAsFactors = FALSE)
        }
        else {
            if (length(all_prons) == 0) {
                gp_scores <- data.frame()
            }
            else {
                gp_scores <- do.call(rbind, lapply(all_prons, 
                  function(p) {
                    map_value(spelling = target, pronunciation = p, 
                      level = "PG", tables = PG_table)
                  }))
                gp_scores <- summarize_words(gp_scores, score_row, 
                  mode = summary_mode)
                if ("mean" %in% names(gp_scores)) {
                  gp_scores <- gp_scores[!is.na(gp_scores$mean), 
                    , drop = FALSE]
                }
            }
            if (length(all_prons) == 0) {
                pp_by_pron <- data.frame(pronunciation = character(0), 
                  pp = numeric(0), stringsAsFactors = FALSE)
            }
            else {
                pp_by_pron <- aggregate(pp ~ pronunciation, data = data.frame(pronunciation = all_prons, 
                  pp = pp_aligned, stringsAsFactors = FALSE), 
                  FUN = max)
            }
            if (nrow(gp_scores) == 0L) {
                if (summary_mode == "default") {
                  gp_scores <- data.frame(spelling = character(0), 
                    pronunciation = character(0), mean = numeric(0), 
                    median = numeric(0), max = numeric(0), min = numeric(0), 
                    sd = numeric(0), pp = numeric(0), stringsAsFactors = FALSE)
                }
                else {
                  gp_scores <- data.frame(spelling = character(0), 
                    pronunciation = character(0), mean_onset = numeric(0), 
                    median_onset = numeric(0), max_onset = numeric(0), 
                    min_onset = numeric(0), sd_onset = numeric(0), 
                    mean_nucleus = numeric(0), median_nucleus = numeric(0), 
                    max_nucleus = numeric(0), min_nucleus = numeric(0), 
                    sd_nucleus = numeric(0), mean_coda = numeric(0), 
                    median_coda = numeric(0), max_coda = numeric(0), 
                    min_coda = numeric(0), sd_coda = numeric(0), 
                    pp = numeric(0), stringsAsFactors = FALSE)
                }
            }
            else {
                if (summary_mode == "default") {
                  if (!("mean" %in% names(gp_scores))) {
                    gp_scores$mean <- NA_real_
                  }
                  if (!("median" %in% names(gp_scores))) 
                    gp_scores$median <- NA_real_
                  if (!("max" %in% names(gp_scores))) 
                    gp_scores$max <- NA_real_
                  if (!("min" %in% names(gp_scores))) 
                    gp_scores$min <- NA_real_
                  if (!("sd" %in% names(gp_scores))) 
                    gp_scores$sd <- NA_real_
                  gp_scores <- merge(gp_scores[, c("spelling", 
                    "pronunciation", "mean", "median", "max", 
                    "min", "sd"), drop = FALSE], pp_by_pron, 
                    by = "pronunciation", all.x = TRUE)
                  gp_scores$pp[is.na(gp_scores$pp)] <- 0
                  gp_scores <- gp_scores[, c("spelling", "pronunciation", 
                    "mean", "median", "max", "min", "sd", "pp"), 
                    drop = FALSE]
                }
                else {
                  gp_scores <- merge(gp_scores, pp_by_pron, by = "pronunciation", 
                    all.x = TRUE)
                  gp_scores$pp[is.na(gp_scores$pp)] <- 0
                }
            }
        }
    }
    if (score == TRUE && level == "ALL" && score_row != "pp") {
        onc_stat_cols <- c("mean_onset", "median_onset", "max_onset", 
            "min_onset", "sd_onset", "mean_nucleus", "median_nucleus", 
            "max_nucleus", "min_nucleus", "sd_nucleus", "mean_coda", 
            "median_coda", "max_coda", "min_coda", "sd_coda")
        if (length(all_prons) == 0) {
            OC_scores <- data.frame()
        }
        else {
            OC_scores <- do.call(rbind, lapply(all_prons, function(p) {
                map_value(spelling = target, pronunciation = p, 
                  level = "OC", tables = OC_table)
            }))
            OC_scores <- summarize_words(OC_scores, score_row, 
                mode = summary_mode)
        }
        if (summary_mode == "default") {
            if (nrow(OC_scores) == 0L || !("mean" %in% names(OC_scores))) {
                OC_scores <- data.frame(spelling = rep(target, 
                  length(all_prons)), pronunciation = all_prons, 
                  mean = 0, median = 0, max = 0, min = 0, sd = 0, 
                  stringsAsFactors = FALSE)
            }
            else {
                OC_scores[is.na(OC_scores$mean), "mean"] <- 0
                for (nm in c("median", "max", "min", "sd")) {
                  if (nm %in% names(OC_scores)) 
                    OC_scores[is.na(OC_scores[[nm]]), nm] <- 0
                }
            }
        }
        else {
            if (nrow(OC_scores) == 0L) {
                OC_scores <- data.frame(spelling = rep(target, 
                  length(all_prons)), pronunciation = all_prons, 
                  stringsAsFactors = FALSE)
                for (nm in onc_stat_cols) OC_scores[[nm]] <- 0
            }
            else {
                for (nm in onc_stat_cols) {
                  if (!(nm %in% names(OC_scores))) 
                    OC_scores[[nm]] <- 0
                  OC_scores[[nm]][is.na(OC_scores[[nm]])] <- 0
                }
            }
            OC_scores <- OC_scores[, c("spelling", "pronunciation", 
                onc_stat_cols), drop = FALSE]
        }
        if (length(all_prons) == 0) {
            OR_scores <- data.frame()
        }
        else {
            OR_scores <- do.call(rbind, lapply(all_prons, function(p) {
                map_value(spelling = target, pronunciation = p, 
                  level = "OR", tables = OR_table)
            }))
            OR_scores <- summarize_words(OR_scores, score_row, 
                mode = summary_mode)
        }
        if (summary_mode == "default") {
            if (nrow(OR_scores) == 0L || !("mean" %in% names(OR_scores))) {
                OR_scores <- data.frame(spelling = rep(target, 
                  length(all_prons)), pronunciation = all_prons, 
                  mean = 0, median = 0, max = 0, min = 0, sd = 0, 
                  stringsAsFactors = FALSE)
            }
            else {
                OR_scores[is.na(OR_scores$mean), "mean"] <- 0
                for (nm in c("median", "max", "min", "sd")) {
                  if (nm %in% names(OR_scores)) 
                    OR_scores[is.na(OR_scores[[nm]]), nm] <- 0
                }
            }
        }
        else {
            if (nrow(OR_scores) == 0L) {
                OR_scores <- data.frame(spelling = rep(target, 
                  length(all_prons)), pronunciation = all_prons, 
                  stringsAsFactors = FALSE)
                for (nm in onc_stat_cols) OR_scores[[nm]] <- 0
            }
            else {
                for (nm in onc_stat_cols) {
                  if (!(nm %in% names(OR_scores))) 
                    OR_scores[[nm]] <- 0
                  OR_scores[[nm]][is.na(OR_scores[[nm]])] <- 0
                }
            }
            OR_scores <- OR_scores[, c("spelling", "pronunciation", 
                onc_stat_cols), drop = FALSE]
        }
        if (summary_mode == "ONC") {
            gp_keep <- c("spelling", "pronunciation", onc_stat_cols, 
                "pp")
            gp_keep <- gp_keep[gp_keep %in% names(gp_scores)]
            pg_scores <- gp_scores[, gp_keep, drop = FALSE]
        }
        else {
            pg_scores <- gp_scores
        }
        oc_had_pp <- "pp" %in% names(OC_scores)
        or_had_pp <- "pp" %in% names(OR_scores)
        if (exists("pp_by_pron", inherits = FALSE) && nrow(pp_by_pron) > 
            0) {
            if (nrow(OC_scores) > 0 && !oc_had_pp && "pronunciation" %in% 
                names(OC_scores)) {
                OC_scores$pp <- pp_by_pron$pp[match(OC_scores$pronunciation, 
                  pp_by_pron$pronunciation)]
                OC_scores$pp[is.na(OC_scores$pp)] <- 0
            }
            if (nrow(OR_scores) > 0 && !or_had_pp && "pronunciation" %in% 
                names(OR_scores)) {
                OR_scores$pp <- pp_by_pron$pp[match(OR_scores$pronunciation, 
                  pp_by_pron$pronunciation)]
                OR_scores$pp[is.na(OR_scores$pp)] <- 0
            }
        }
        if (summary_mode == "default") {
            pg_scores <- filter_with_ties(pg_scores, "mean", 
                min_cut = min_map, mean_score = mean_score, max_options = max_options, 
                apply_mean = TRUE, apply_min_cut = TRUE)
            OC_scores <- filter_with_ties(OC_scores, "mean", 
                min_cut = min_map, mean_score = mean_score, max_options = max_options, 
                apply_mean = TRUE, apply_min_cut = TRUE)
            OR_scores <- filter_with_ties(OR_scores, "mean", 
                min_cut = min_map, mean_score = mean_score, max_options = max_options, 
                apply_mean = TRUE, apply_min_cut = TRUE)
        }
        else {
            pg_scores <- filter_with_onc_metric(pg_scores, min_cut = min_map, 
                mean_score = mean_score, max_options = max_options)
            OC_scores <- filter_with_onc_metric(OC_scores, min_cut = min_map, 
                mean_score = mean_score, max_options = max_options)
            OR_scores <- filter_with_onc_metric(OR_scores, min_cut = min_map, 
                mean_score = mean_score, max_options = max_options)
        }
        if (!oc_had_pp && "pp" %in% names(OC_scores)) 
            OC_scores$pp <- NULL
        if (!or_had_pp && "pp" %in% names(OR_scores)) 
            OR_scores$pp <- NULL
        oc_note <- NULL
        or_note <- NULL
        if (summary_mode == "default") {
            if (nrow(OC_scores) > 0 && "mean" %in% names(OC_scores)) {
                oc_vals <- suppressWarnings(as.numeric(OC_scores$mean))
                if (length(oc_vals) > 0 && all(!is.finite(oc_vals) | 
                  oc_vals == 0)) {
                  OC_scores <- OC_scores[0, , drop = FALSE]
                  oc_note <- "No OC candidates: all candidate means are 0."
                }
            }
            if (nrow(OR_scores) > 0 && "mean" %in% names(OR_scores)) {
                or_vals <- suppressWarnings(as.numeric(OR_scores$mean))
                if (length(or_vals) > 0 && all(!is.finite(or_vals) | 
                  or_vals == 0)) {
                  OR_scores <- OR_scores[0, , drop = FALSE]
                  or_note <- "No OR candidates: all candidate means are 0."
                }
            }
        }
        score_name <- paste0("scores_", tolower(selected_param))
        add_input_meta <- function(df, idx, tgt) {
            n <- nrow(df)
            df$input_index <- if (n > 0) 
                rep.int(idx, n)
            else integer(0)
            df$input_target <- if (n > 0) 
                rep.int(tgt, n)
            else character(0)
            df
        }
        pg_scores <- add_input_meta(pg_scores, 1L, target)
        OC_scores <- add_input_meta(OC_scores, 1L, target)
        OR_scores <- add_input_meta(OR_scores, 1L, target)
        OC_out <- setNames(list(OC_scores), c(score_name))
        OR_out <- setNames(list(OR_scores), c(score_name))
        if (!is.null(oc_note)) 
            OC_out$note <- oc_note
        if (!is.null(or_note)) 
            OR_out$note <- or_note
        return(list(PG = setNames(list(pg_scores), c(score_name)), 
            OC = OC_out, OR = OR_out))
    }
    if (score == TRUE && summary_mode == "default") {
        gp_scores <- filter_with_ties(gp_scores, "mean", min_cut = min_map, 
            mean_score = mean_score, max_options = max_options, 
            apply_mean = TRUE, apply_min_cut = TRUE)
    }
    if (score == TRUE && summary_mode == "ONC") {
        gp_scores <- filter_with_onc_metric(gp_scores, min_cut = min_map, 
            mean_score = mean_score, max_options = max_options)
    }
    if (score == TRUE) {
        n <- nrow(gp_scores)
        gp_scores$input_index <- if (n > 0) 
            rep.int(1L, n)
        else integer(0)
        gp_scores$input_target <- if (n > 0) 
            rep.int(target, n)
        else character(0)
        return(setNames(list(gp_scores), c(paste0("scores_", 
            tolower(selected_param)))))
    }
    else {
        if (level == "ALL") {
            return(list(PG = list(pronunciations = pronunciations_df), 
                OC = list(pronunciations = pronunciations_df), 
                OR = list(pronunciations = pronunciations_df)))
        }
        return(pronunciations_df)
    }
} 

# ============================================================ 
# visualize_parse_tree 
# ============================================================ 
visualize_parse_tree <- function (spelling, pronunciation = NULL, show_plot = FALSE, 
    include_suffix_variants = TRUE) 
{
    stopifnot(is.character(spelling), length(spelling) == 1L)
    if (!is.null(pronunciation)) {
        stopifnot(is.character(pronunciation), length(pronunciation) == 
            1L)
    }
    library(data.tree)
    library(stringr)
    required_globals <- c("word_initial_mappings", "word_final_mappings", 
        "syllable_medial_mappings", "syllable_final_mappings", 
        "syllable_initial_mappings")
    missing_globals <- required_globals[!vapply(required_globals, 
        exists, logical(1), inherits = TRUE)]
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
            is.call(e) && identical(e[[1]], as.name("<-")) && 
                identical(e[[2]], as.name(name))
        }, logical(1)))
        if (length(idx) == 0L) {
            stop("Could not locate assignment for ", name, " inside parse_corpus.")
        }
        if (name == "parse_string" && length(idx) > 1L) {
            return(exprs[[idx[1]]])
        }
        if (length(idx) != 1L) {
            stop("Could not uniquely locate assignment for ", 
                name, " inside parse_corpus.")
        }
        exprs[[idx]]
    }
    parse_env <- new.env(parent = parent.frame())
    parse_env$word_initial_mappings <- get("word_initial_mappings", 
        inherits = TRUE)
    parse_env$word_final_mappings <- get("word_final_mappings", 
        inherits = TRUE)
    parse_env$syllable_medial_mappings <- get("syllable_medial_mappings", 
        inherits = TRUE)
    parse_env$syllable_final_mappings <- get("syllable_final_mappings", 
        inherits = TRUE)
    parse_env$syllable_initial_mappings <- get("syllable_initial_mappings", 
        inherits = TRUE)
    parse_env$internal_mappings <- rbind(parse_env$syllable_initial_mappings, 
        parse_env$syllable_medial_mappings, parse_env$syllable_final_mappings)
    parse_env$parse_corpus_word_initial_mappings <- parse_env$word_initial_mappings
    parse_env$parse_corpus_word_final_mappings <- parse_env$word_final_mappings
    parse_env$parse_corpus_word_final_mappings[nrow(parse_env$parse_corpus_word_final_mappings) + 
        1, ] <- parse_env$parse_corpus_word_final_mappings[nrow(parse_env$parse_corpus_word_final_mappings), 
        ]
    parse_env$parse_corpus_word_final_mappings[nrow(parse_env$parse_corpus_word_final_mappings), 
        ]$grapheme <- "h"
    parse_env$parse_corpus_word_final_mappings[nrow(parse_env$parse_corpus_word_final_mappings), 
        ]$phoneme <- "h"
    parse_env$parse_corpus_word_final_mappings[nrow(parse_env$parse_corpus_word_final_mappings) + 
        1, ] <- parse_env$parse_corpus_word_final_mappings[nrow(parse_env$parse_corpus_word_final_mappings), 
        ]
    parse_env$parse_corpus_word_final_mappings[nrow(parse_env$parse_corpus_word_final_mappings), 
        ]$grapheme <- "w"
    parse_env$parse_corpus_word_final_mappings[nrow(parse_env$parse_corpus_word_final_mappings), 
        ]$phoneme <- "w"
    parse_env$parse_corpus_word_initial_mappings$grapheme <- toupper(parse_env$parse_corpus_word_initial_mappings$grapheme)
    parse_env$internal_mappings$grapheme <- toupper(parse_env$internal_mappings$grapheme)
    parse_env$parse_corpus_word_final_mappings$grapheme <- toupper(parse_env$parse_corpus_word_final_mappings$grapheme)
    eval(pick_assign("parse_string"), envir = parse_env)
    eval(pick_assign("extract_branches"), envir = parse_env)
    parse_string <- get("parse_string", envir = parse_env, inherits = FALSE)
    extract_branches <- get("extract_branches", envir = parse_env, 
        inherits = FALSE)
    tree <- parse_string(spelling)
    branches <- extract_branches(tree)
    if (include_suffix_variants && grepl("es$", spelling, ignore.case = TRUE)) {
        spelling2 <- substr(spelling, 1, nchar(spelling) - 1)
        branches2 <- extract_branches(parse_string(spelling2))
        branches2 <- lapply(branches2, function(branch) {
            if (grepl("-", branch)) 
                branch <- sub("-", "S-", branch)
            paste(branch, "-S", sep = "")
        })
        branches <- unique(c(branches, branches2))
    }
    if (include_suffix_variants && grepl("ed$", spelling, ignore.case = TRUE)) {
        spelling2 <- substr(spelling, 1, nchar(spelling) - 1)
        branches2 <- extract_branches(parse_string(spelling2))
        branches2 <- lapply(branches2, function(branch) {
            if (grepl("-", branch)) 
                branch <- sub("-", "D-", branch)
            paste(branch, "-D", sep = "")
        })
        branches <- unique(c(branches, branches2))
    }
    branches <- unique(as.character(unlist(branches, use.names = FALSE)))
    correct_branches <- character(0)
    if (!is.null(pronunciation)) {
        mapped <- map_PG(spelling, pronunciation, map_progress = FALSE)
        target_parse <- paste(toupper(mapped[[1]][[1]][2, ]), 
            collapse = "-")
        keep <- sub("^[^-]*-", "", branches) == target_parse
        correct_branches <- branches[keep]
    }
    if (show_plot) {
        if (requireNamespace("igraph", quietly = TRUE) && length(branches) > 
            0L) {
            branch_tokens <- strsplit(branches, "-", fixed = TRUE)
            node_labels <- list()
            edge_from <- character(0)
            edge_to <- character(0)
            for (tok in branch_tokens) {
                if (length(tok) < 1L) 
                  next
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
            edges_df <- unique(data.frame(from = edge_from, to = edge_to, 
                stringsAsFactors = FALSE))
            node_ids <- unique(c(edges_df$from, edges_df$to))
            vertices_df <- data.frame(name = node_ids, label = vapply(node_ids, 
                function(id) node_labels[[id]], character(1)), 
                stringsAsFactors = FALSE)
            highlight_nodes <- character(0)
            highlight_edges <- character(0)
            if (length(correct_branches) > 0L) {
                hit_tokens <- strsplit(correct_branches, "-", 
                  fixed = TRUE)
                for (tok in hit_tokens) {
                  ids <- character(length(tok))
                  ids[1] <- tok[1]
                  highlight_nodes <- c(highlight_nodes, ids[1])
                  if (length(tok) > 1L) {
                    for (ii in 2:length(tok)) {
                      ids[ii] <- paste(ids[ii - 1], tok[ii], 
                        sep = "/")
                      highlight_nodes <- c(highlight_nodes, ids[ii])
                      highlight_edges <- c(highlight_edges, paste(ids[ii - 
                        1], ids[ii], sep = "->"))
                    }
                  }
                }
            }
            highlight_nodes <- unique(highlight_nodes)
            highlight_edges <- unique(highlight_edges)
            g <- igraph::graph_from_data_frame(edges_df, directed = TRUE, 
                vertices = vertices_df)
            edge_df <- igraph::as_data_frame(g, what = "edges")
            edge_keys <- paste(edge_df$from, edge_df$to, sep = "->")
            edge_col <- ifelse(edge_keys %in% highlight_edges, 
                "firebrick3", "gray70")
            edge_wd <- ifelse(edge_keys %in% highlight_edges, 
                3, 1)
            v_col <- ifelse(igraph::V(g)$name %in% highlight_nodes, 
                "gold", "lightblue")
            v_frame <- ifelse(igraph::V(g)$name %in% highlight_nodes, 
                "firebrick3", "gray40")
            root_id <- branch_tokens[[1]][1]
            root_idx <- which(igraph::V(g)$name == root_id)
            lo <- igraph::layout_as_tree(g, root = root_idx[1], 
                circular = FALSE)
            old_par <- par(no.readonly = TRUE)
            on.exit(par(old_par), add = TRUE)
            par(mar = c(1, 1, 1, 1))
            plot(g, layout = lo, vertex.label = igraph::V(g)$label, 
                vertex.color = v_col, vertex.frame.color = v_frame, 
                vertex.size = 22, vertex.label.cex = 0.9, edge.color = edge_col, 
                edge.width = edge_wd, edge.arrow.size = 0.25)
        }
        else {
            plot_obj <- plot(tree)
            if (!is.null(plot_obj)) {
                print(plot_obj)
            }
        }
    }
    else {
        print(tree, "level")
    }
    if (length(branches) > 0L) {
        cat("\nCandidate branches (", length(branches), "):\n", 
            sep = "")
        cat(paste0("  ", branches), sep = "\n")
        cat("\n")
    }
    if (!is.null(pronunciation)) {
        cat("\nPronunciation target: ", pronunciation, "\n", 
            sep = "")
        if (length(correct_branches) > 0L) {
            cat("Matching branch(es):\n")
            cat(paste0("  * ", correct_branches), sep = "\n")
            cat("\n")
        }
        else {
            cat("No branch matched the map_PG-derived parse for this pronunciation.\n")
        }
    }
    invisible(list(tree = tree, branches = branches, pronunciation = pronunciation, 
        matching_branches = correct_branches))
} 

