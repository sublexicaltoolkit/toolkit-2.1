SB <- "<s>"     # word-start indicator
SE <- "</s>"    # word-end indicator

# ===========================================================================
# PAIRS IN THE LEXICON
# ===========================================================================

# Build a dataset of all pairs of units in the lexicon. The SB and SE units defined
# above are used to include units that are present in the word-initial and 
# word-final positions. This reduces the number of special cases we need to 
# handle in the sliding window algorithm because the wi/wf validity of options
#  can be checked the same way as all other unit pairs.
# row: "phoneme" or "grapheme", based on which set of units to pair from words
# scored_words: scored_words_PG dataset
attested_pairs <- function(row, scored_words = scored_words_PG) {
  units <- lapply(scored_words, function(d) as.character(unlist(d[row, ])))
  if (row == "grapheme") units <- lapply(units, tolower)
  unique(unlist(lapply(units, function(v) {
    v <- c(SB, v, SE)
    paste(v[-length(v)], v[-1], sep = "\r")
  }), use.names = FALSE))
}

# ===========================================================================
# LEGALITY RULES FROM pw_read
# ===========================================================================

# conditionals
# These are the filters from pw_read:
# 1) The "3" phoneme must be followed by "r"
# 2) A word must contain at least one vowel
# 3) Schwa cannot be the only value
# 4) Condense two "r" phonemes in a row to a single "r"
bare3 <- function(a, b) endsWith(a, "3") & !startsWith(b, "r")

nonschwa <- function(p) grepl("[5O8o2@aE3i1cuU^]", p)

# Removes leading "r" phonemes from b if the segment a ends in an "r"
rr_emit <- function(a, b) ifelse(endsWith(a, "r"), sub("^r+", "", b), b)

# ===========================================================================
# LEXICON EXCEPTIONS FOR SOME REAL WORDS
# ===========================================================================

# Returns the pronunciations for a given word that do not pass the vowel/nonschwa
# checks. This is specifically used when computing over the lexicon for the words
# a, the, hm, and shh
lex_exceptions <- function(entries) {
  e <- entries[!nonschwa(entries$pronunciation), ]
  lapply(split(e$pronunciation, e$word), unique)
}

# Checks if the trellis graph(s) for a given word contain any of the exception
# pronunciations provided, in order to determine which of those exceptions if any
# come from the parses for that word as represented by these trellises
reachable_exceptions <- function(trellises, exceptions) {
  if (!length(exceptions)) return(character(0))
  spelled <- unlist(lapply(trellises, function(tr) {
    if (any(lengths(tr$dom) == 0L)) return(NULL)
    tr$through <- .sw_sweep(tr$adj, lapply(tr$dom, function(d) rep(TRUE, length(d))))$through
    .sw_enum(tr, emit_read)
  }))
  intersect(exceptions, spelled)
}

# ===========================================================================
# SLIDING WINDOW ALGORITHM
# ===========================================================================

# A: matrix of allowed pairs, such that for a position i, A[[i]] has one row
#   per option at position i and one column per option at position i + 1, with
#   values at A[[i]][r, c] TRUE/FALSE depending on whether or not c can follow
#   r based on the pairs in the lexicon
# v: matrix describing which options are non-schwa values. For position i,
#   v[[i]][k] is TRUE if option k at position i is a non-schwa vowel, which
#   is used in finding paths containing non-schwa vowels
# Outputs a tuple containing the following information: 
# through: the values corresponding to each option in the trellis, with numbers
#   for each option based on the number of partial paths passing through that
#   option
# count: the number of complete routes that pass the pw_read filters
# paths: the number of complete routes (ignoring filters)
.sw_sweep <- function(A, v) {
  n <- length(v)
  # left to right: partial routes ending at each option
  f0 <- f1 <- vector("list", n); f0[[1]] <- 1; f1[[1]] <- 0
  for (i in 2:n) {
    in0 <- drop(f0[[i - 1]] %*% A[[i - 1]]); in1 <- drop(f1[[i - 1]] %*% A[[i - 1]])
    f1[[i]] <- ifelse(v[[i]], in0 + in1, in1); f0[[i]] <- ifelse(v[[i]], 0, in0)
  }
  # right to left: completions after each option
  ball <- bneed <- vector("list", n); ball[[n]] <- 1; bneed[[n]] <- 0
  for (i in (n - 1):1) {
    ball[[i]]  <- drop(A[[i]] %*% ball[[i + 1]])
    bneed[[i]] <- drop(A[[i]] %*% ifelse(v[[i + 1]], ball[[i + 1]], bneed[[i + 1]]))
  }
  list(through = lapply(seq_len(n), function(i) f1[[i]] * ball[[i]] + f0[[i]] * bneed[[i]]),
       count   = sum(f1[[n]]),
       paths   = sum(f0[[n]] + f1[[n]]))
}

# Lists all of the strings that can be produced from the trellis of a word.
# This should only be used on small words with small graphs and is not feasible
# to run on words with large graphs.
.sw_enum <- function(tr, emit) {
  dom <- tr$dom; A <- tr$adj; n <- length(dom)
  if (any(lengths(dom) == 0L) || sum(tr$through[[1]]) == 0) return(character(0))
  live <- lapply(tr$through, `>`, 0)
  s <- ""; last <- 1L; held <- ""
  for (i in seq_len(n - 1)) {
    nxt <- lapply(last, function(a) which(A[[i]][a, ] & live[[i + 1]]))
    L <- lengths(nxt); b <- unlist(nxt, use.names = FALSE)
    a <- rep(last, L); held <- rep(held, L)
    if (is.list(emit)) {
      st <- emit$step(dom[[i + 1]][b], tr$pos[i + 1], held, tr$ph[i + 1])
      s <- paste0(rep(s, L), st$out); held <- st$held
    } else s <- paste0(rep(s, L), emit(dom[[i]][a], dom[[i + 1]][b]))
    last <- b
  }
  s
}

# opts: number of phoneme options per position
# pairs: dataset built from attested_pairs() containing pairs present in the lexicon
# enum_limit: if the final count of pronunciations is below this limit, they
#   will be enumerated in prons in the output
# Output (all of the following exist by name in the output vector):
# count: number of pronunciations that pass all filters
# paths: raw count of the number of pronunciations ignoring pw_read filters
# prons: vector of the count output pronunciations (empty if count > enum_limit)
# dom: positions and their associated options
# adj: matrices of the format of A in .sw_sweep describing the allowed pairs
# through: number partial routes through each option
sw_read <- function(opts, pairs, enum_limit = 1e5) {
  dom <- c(list(SB), opts, list(SE))
  n   <- length(dom)

  # handle positions with no options
  if (any(lengths(dom) == 0L))
    return(list(count = 0, paths = 0, prons = character(0), dom = dom,
                through = lapply(dom, function(d) numeric(length(d)))))

  # A[[i]][a, b]: may option a at position i be followed by option b at position i + 1?
  # Must be in the lexicon and not break the 3r rule
  A <- lapply(seq_len(n - 1), function(i) {
    a <- rep(dom[[i]], times = length(dom[[i + 1]]))
    b <- rep(dom[[i + 1]], each = length(dom[[i]]))
    matrix(paste(a, b, sep = "\r") %in% pairs & !bare3(a, b),
           length(dom[[i]]), length(dom[[i + 1]]))
  })

  sw <- .sw_sweep(A, lapply(dom, nonschwa))
  r  <- list(count = sw$count, paths = sw$paths, prons = character(0), dom = dom, adj = A,
             through = sw$through)
  if (r$count > 0 && r$count <= enum_limit) {
    s <- .sw_enum(r, emit_read)
    r$prons <- s[nonschwa(s)]
  }
  r
}

# ===========================================================================
# DETERMINING THE NUMBER OF UNIQUE STRINGS
# ===========================================================================

# Handles pronunciation de-duplication over different parses for a final
# deduplicated pronunciation count

# Builds a partial string by appending b to a and applying the "rr" rule in 
# case a ends in "r" and b starts with "r"
emit_read <- function(a, b) ifelse(b == SE, "", rr_emit(a, b))

# We combine all of the trellises into an NFA, where individual nodes are
# individual characters in a pronunciation, based on the options available in
# all positions across all parses.
# This NFA is built from only those options through which a final pronunciation
# is constructed.
.nfa <- function(trellises, emit) {
  carry <- is.list(emit)
  ns <- 0L
  fr <- list(); ch <- list(); to <- list(); ef <- list(); et <- list()
  start <- integer(0); accept <- integer(0)
  for (p in seq_along(trellises)) {
    tr <- trellises[[p]]; dom <- tr$dom; A <- tr$adj; n <- length(dom)
    live <- lapply(tr$through, function(t) t > 0)
    if (!length(live[[1]]) || !isTRUE(live[[1]][1])) next   # nothing survives
    if (carry && (is.null(tr$pos) || is.null(tr$ph))) stop(".nfa: a carrying emitter needs tr$pos and tr$ph")
    ns <- ns + 1L
    cur <- list(node = 1L, held = "", id = ns)
    start <- c(start, ns)
    for (i in seq_len(n - 1)) {
      e <- which(A[[i]] & outer(live[[i]], live[[i + 1]], "&"), arr.ind = TRUE)
      if (!length(e) || !nrow(e)) { cur <- NULL; break }
      by_a <- split(seq_len(nrow(e)), factor(e[, 1], levels = seq_along(dom[[i]])))
      cnt  <- lengths(by_a)[cur$node]
      rr   <- rep(seq_along(cur$node), cnt)
      ee   <- unlist(by_a[cur$node], use.names = FALSE)
      if (!length(ee)) { cur <- NULL; break }
      av <- e[ee, 1]; bv <- e[ee, 2]; fa <- cur$id[rr]
      if (carry) {
        st <- emit$step(dom[[i + 1]][bv], tr$pos[i + 1], cur$held[rr], tr$ph[i + 1])
        em <- st$out; h2 <- st$held
      } else {
        em <- emit(dom[[i]][av], dom[[i + 1]][bv]); h2 <- rep("", length(bv))
      }
      key <- paste(bv, h2, sep = "\r"); uk <- unique(key); mk <- match(key, uk)
      ids <- ns + seq_along(uk); ns <- ns + length(uk)
      tb  <- ids[mk]
      first <- !duplicated(mk)
      cur <- list(node = bv[first][order(mk[first])], held = h2[first][order(mk[first])], id = ids)
      L <- nchar(em); z <- L == 0L
      if (any(z)) { ef[[length(ef) + 1L]] <- fa[z]; et[[length(et) + 1L]] <- tb[z] }
      if (any(!z)) {
        fz <- fa[!z]; tz <- tb[!z]; ez <- em[!z]; Lz <- L[!z]
        # chain: fa -c1-> m1 -c2-> ... -cL-> tb, with L-1 fresh middle states
        mid0 <- ns + c(0L, cumsum(Lz - 1L))[seq_along(Lz)]
        ns   <- ns + sum(Lz - 1L)
        k    <- sequence(Lz)
        eidx <- rep(seq_along(Lz), Lz)
        fr[[length(fr) + 1L]] <- ifelse(k == 1L, fz[eidx], mid0[eidx] + k - 1L)
        ch[[length(ch) + 1L]] <- substr(rep(ez, Lz), k, k)
        to[[length(to) + 1L]] <- ifelse(k == Lz[eidx], tz[eidx], mid0[eidx] + k)
      }
    }
    if (!is.null(cur)) accept <- c(accept, cur$id)
  }
  list(n = ns, start = unique(start), accept = unique(accept),
       from = unlist(fr, use.names = FALSE), char = unlist(ch, use.names = FALSE),
       to = unlist(to, use.names = FALSE),
       efrom = unlist(ef, use.names = FALSE), eto = unlist(et, use.names = FALSE))
}

# Describes the number of possible states the NFA representation could be in
# after consuming some number of characters based on free edges
.closures <- function(M) {
  cl <- as.list(seq_len(M$n))
  if (!length(M$efrom)) return(cl)
  succ <- split(M$eto, factor(M$efrom, levels = seq_len(M$n)))
  has  <- which(lengths(succ) > 0)
  repeat {
    changed <- FALSE
    for (s in has) {
      v <- sort(unique(c(s, unlist(cl[succ[[s]]], use.names = FALSE))))
      if (length(v) != length(cl[[s]])) { cl[[s]] <- v; changed <- TRUE }
    }
    if (!changed) break
  }
  cl
}

# Computes the number of distinct strings across all provided trellises, for
# the spelling or reading direction.
# trellises: outputs from sw_read() or sw_spell()
# emit: emit_read() or emit_spell() passed in as a function based on the direction
# need_vowel: TRUE if this is reading, FALSE if spelling
sw_unique <- function(trellises, emit, need_vowel) {
  M <- .nfa(trellises, emit)
  if (!length(M$start)) return(0)
  cl  <- .closures(M)
  acc <- logical(M$n); acc[M$accept] <- TRUE
  o    <- order(M$from)
  tfrom <- M$from[o]; tch <- M$char[o]; tto <- M$to[o]
  tlen <- tabulate(tfrom, nbins = M$n)
  tbeg <- cumsum(c(1L, tlen))[seq_len(M$n)]
  alpha <- sort(unique(tch)); tcode <- match(tch, alpha)
  vowel_ch <- if (need_vowel) nonschwa(alpha) else rep(TRUE, length(alpha))

  cur_sets <- list(sort(unique(unlist(cl[M$start], use.names = FALSE))))
  cur_bit  <- !need_vowel
  cur_cnt  <- 1
  total <- 0
  repeat {
    sid <- rep(seq_along(cur_sets), lengths(cur_sets))
    st  <- unlist(cur_sets, use.names = FALSE)
    L   <- tlen[st]
    if (!sum(L)) break
    k    <- rep(tbeg[st], L) + sequence(L) - 1L
    sid2 <- rep(sid, L)
    cc   <- tcode[k]
    nx   <- tto[k]
    # one move per (current state set, character)
    grp  <- (sid2 - 1L) * length(alpha) + cc
    clen <- lengths(cl[nx])
    gg   <- rep(grp, clen)
    tt   <- unlist(cl[nx], use.names = FALSE)
    keep <- !duplicated(as.numeric(gg) * (M$n + 1) + tt)
    gg <- gg[keep]; tt <- tt[keep]
    oo <- order(gg, tt); gg <- gg[oo]; tt <- tt[oo]
    ug <- unique(gg)
    gsid <- (ug - 1L) %/% length(alpha) + 1L
    gch  <- (ug - 1L) %% length(alpha) + 1L
    gbit <- cur_bit[gsid] | vowel_ch[gch]
    sets <- split(tt, factor(gg, levels = ug))
    key  <- paste0(as.integer(gbit), ":", vapply(sets, paste, "", collapse = ","))
    cnt  <- cur_cnt[gsid]
    # merge moves that land in the same state set
    uk   <- unique(key); m <- match(key, uk)
    ncnt <- as.numeric(rowsum(cnt, m, reorder = TRUE)[, 1])
    first <- !duplicated(m)
    nsets <- unname(sets[first][order(m[first])])
    nbit  <- gbit[first][order(m[first])]
    isacc <- vapply(nsets, function(s) any(acc[s]), logical(1))
    total <- total + sum(ncnt[isacc & nbit])
    cur_sets <- nsets; cur_bit <- nbit; cur_cnt <- ncnt
  }
  total
}

# For a list of parsing probability scores per parse, returns a vector of
# equal length with TRUE for the parse(s) that tied for the highest pp value
# and FALSE for the rest
sw_top_flags <- function(score) {
  best <- max(score)
  abs(score - best) <= sqrt(.Machine$double.eps) * max(1, abs(best))
}

# Computes the unique pronunciation count for the reading direction
# using a set of trellises, parsing probability scores, and exceptions
# Outputs:
# per_parse: a list corresponding to each parse of the number of distinct
#   pronunciations produced by that parse
# total: the number of distinct pronunciations across all parses
# top: A list of four entries (pp, pp2, pp_freq, pp2_freq), where each is the
#   number of distinct pronunciations produced by the parse(s) that had the
#   highest of each of those parsing probability scores
sw_read_unique <- function(trs, scores, exceptions = NULL) {
  one <- function(set) sw_unique(trs[set], emit_read, need_vowel = TRUE) +
    length(reachable_exceptions(trs[set], exceptions))
  per <- vapply(seq_along(trs), one, 0)
  uni <- function(set) if (length(set) == 1L) per[set] else one(set)
  list(per_parse = per, total = uni(seq_along(trs)),
       top = lapply(scores, function(s) uni(which(sw_top_flags(s)))))
}

# ===========================================================================
# One word, every parse
# ===========================================================================

# Contains one entry for each of the words in the Toolkit lexicon in order,
# with
# word_index: the index of that word in the Toolkit wordlist
# word: the grapheme string for the word
# pronunciation: the phoneme string for the word as it exists in the Toolkit
# actual_parse: the parse of the grapheme string for its entry in the Toolkit
lex_entries <- function(scored_words = scored_words_PG) {
  data.frame(
    word_index    = seq_along(scored_words),
    word          = tolower(vapply(scored_words, function(d) as.character(d["spelling", 1]), "")),
    pronunciation = vapply(scored_words, function(d) as.character(d["pronunciation", 1]), ""),
    actual_parse  = vapply(scored_words, function(d)
                      paste(tolower(as.character(unlist(d["grapheme", ]))), collapse = "/"), ""),
    stringsAsFactors = FALSE)
}

# This is the primary function that should be called to run the sliding window
# algorithm on a word in the reading direction.
# Inputs: 
# word: a spelling (grapheme string)
# pairs: dataset of unit pairs in the lexicon, output of attested_pairs()
# exmap: output of lex_exceptions(entries)
# entries: output of lex_entries() (input of a specific format for the lexicon)
# enum_limit: upper bound on the number of pronunciations for which to allow
#   enumeration in the output
# Outputs: 
# t1: A data frame with one row per parse of the word, containing statistics
#   including the number of distinct prons for each parse, pp values, etc.
# t2: Data frame with a single row for this word with aggregate metrics
# prons: vector of pronunciations for this word, if the number of distinct
#   pronunciations is less than enum_limit
# NOTE: t1 and t2 can be respectively vertically stacked with outputs from calls
# to this function for other words, to build a final dataset across, for
# instance, all entries in the lexicon
sw_read_word <- function(word, pairs, exmap = list(), entries = NULL, enum_limit = 0) {
  word <- tolower(word)
  cps <- sw_parse(word)
  if (is.null(cps)) return(NULL)
  sc  <- attr(cps, "scores")
  top <- lapply(sc, sw_top_flags)
  trs <- lapply(cps, function(x) sw_read(x$o, pairs, enum_limit))
  u   <- sw_read_unique(trs, sc, exmap[[word]])
  parse <- vapply(cps, function(x) paste(x$g, collapse = "/"), "")

  e <- if (is.null(entries)) NULL else entries[entries$word == word, ]
  if (is.null(e) || !nrow(e)) e <- data.frame(word_index = NA_integer_, actual_parse = NA_character_)
  np <- length(cps)
  ei <- rep(seq_len(nrow(e)), each = np)       # one block of parse rows per entry
  pj <- rep(seq_len(np), nrow(e))
  t1 <- data.frame(
    word_index = e$word_index[ei], word = word, n_letters = nchar(word), n_parses = np,
    parse_no = vapply(cps, `[[`, 0, "parse_no")[pj], parse = parse[pj],
    units = vapply(cps, function(x) length(x$g), 0L)[pj],
    pp = sc$pp[pj], pp2 = sc$pp2[pj], p_count = u$per_parse[pj],
    is_top_pp = top$pp[pj], is_top_pp2 = top$pp2[pj],
    is_actual_parse = parse[pj] == e$actual_parse[ei],
    pp_freq = sc$pp_freq[pj], pp2_freq = sc$pp2_freq[pj],
    is_top_pp_freq = top$pp_freq[pj], is_top_pp2_freq = top$pp2_freq[pj],
    stringsAsFactors = FALSE)

  act <- t1[which(t1$is_actual_parse), ]
  ai  <- match(e$word_index, act$word_index)
  t2 <- data.frame(
    word_index = e$word_index, word = word, n_letters = nchar(word), n_parses = np,
    p_count_top_pp = u$top$pp, p_count_top_pp2 = u$top$pp2,
    p_count_actual_parse = act$p_count[ai], total_p_count = u$total,
    actual_is_top_pp = act$is_top_pp[ai], actual_is_top_pp2 = act$is_top_pp2[ai],
    p_count_top_pp_freq = u$top$pp_freq, p_count_top_pp2_freq = u$top$pp2_freq,
    actual_is_top_pp_freq = act$is_top_pp_freq[ai],
    actual_is_top_pp2_freq = act$is_top_pp2_freq[ai],
    stringsAsFactors = FALSE)
  prons <- if (enum_limit > 0 && all(vapply(trs, `[[`, 0, "count") <= enum_limit))
    sort(as.character(unique(c(unlist(lapply(trs, `[[`, "prons")),
                               reachable_exceptions(trs, exmap[[word]])))))
  else character(0)
  list(t1 = t1, t2 = t2, prons = prons)
}

# ===========================================================================
# SPELLING DIRECTION
# ===========================================================================

# Spelling-direction version of sw_read
sw_spell <- function(ph, opts, pos, pairs) {
  dom <- c(list(SB), lapply(opts, function(o) unique(o[!is.na(o)])), list(SE))
  n   <- length(dom)
  # handle positions with no options
  if (any(lengths(dom) == 0L))
    return(list(count = 0, paths = 0, dom = dom, pos = c("", pos, ""), ph = c(SB, ph, SE),
                through = lapply(dom, function(d) numeric(length(d)))))
  # A[[i]][a, b]: may option a at position i be followed by option b at position i + 1?
  A <- lapply(seq_len(n - 1), function(i) {
    a <- rep(dom[[i]], times = length(dom[[i + 1]]))
    b <- rep(dom[[i + 1]], each = length(dom[[i]]))
    matrix(paste(a, b, sep = "\r") %in% pairs, length(dom[[i]]), length(dom[[i + 1]]))
  })
  sw <- .sw_sweep(A, lapply(dom, function(d) rep(TRUE, length(d))))
  list(count = sw$count, paths = sw$paths, dom = dom, adj = A, through = sw$through,
       pos = c("", pos, ""), ph = c(SB, ph, SE))
}

# ===========================================================================
# SPLIT DIGRAPH LETTER REPRESENTATION
# ===========================================================================

# Helpers for split digraph cases for spelling
# 1) The second component always goes at the end of its syllable
# 2) Exception to 1 when a word-final -s is read as /z/ or /d/
split_pre <- function(x) ifelse(x == SE, "", sub("_.*$", "", x))
split_suf <- function(x) ifelse(grepl("_", x, fixed = TRUE) & x != SB, sub("^[^_]*_", "", x), "")
SYLL_FINAL <- c("sf", "wf", "4", "5") # corpus labels from pw_spell
WORD_FINAL <- c("wf", "5")
FRESH <- "\001" # mark when a Y has not yet been carried

# Split digraph spelling: wait to append the second half of a split digraph's
# letters until the syllable-final position is reached while adding letters
# between
spell_step <- function(b, pos, held, ph = "") {
  aged  <- nzchar(held) & !startsWith(held, FRESH)
  hv    <- sub(FRESH, "", held, fixed = TRUE)
  own   <- split_suf(b)
  fl    <- pos %in% SYLL_FINAL | b == SE
  infl  <- fl & aged & pos %in% WORD_FINAL & ((b == "s" & ph == "z") | (b == "d" & ph == "d"))
  out   <- ifelse(infl, paste0(hv, split_pre(b), own),
           ifelse(fl,   paste0(split_pre(b), hv, own), split_pre(b)))
  keep  <- ifelse(nzchar(own), paste0(FRESH, hv, own), hv)   # carried one more unit
  list(out = out, held = ifelse(fl, "", keep))
}

emit_spell <- list(step = spell_step)

# This is the primary function that should be called to run the sliding window
# algorithm on a word in the spelling direction.
# Inputs: 
# word: a pronunciation (phoneme string)
# pairs: dataset of unit pairs in the lexicon, output of attested_pairs()
# entries: output of lex_entries() (input of a specific format for the lexicon)
#   in the spelling direction
# enum_limit: upper bound on the number of spellings for which to allow
#   enumeration in the output
# Outputs: 
# t1: A data frame with one row per phoneme parse of the word, containing statistics
#   including the number of distinct spellings for each parse, etc.
#   really, these "parsings" are groupings of phonemes (i.e., /j/u/ or /ju/)
# t2: Data frame with a single row for this word with aggregate metrics
# prons: vector of spellings for this word, if the number of distinct
#   spellings is less than enum_limit
# NOTE: t1 and t2 can be respectively vertically stacked with outputs from calls
# to this function for other words, to build a final dataset across, for
# instance, all entries in the lexicon
sw_spell_word <- function(pron, pairs, entries = NULL, enum_limit = 0) {
  cp <- sw_spell_parse(pron)
  if (is.null(cp)) return(NULL)
  trs <- lapply(cp, function(x) sw_spell(x$ph, x$o, x$pos, pairs))
  per <- vapply(trs, function(t) sw_unique(list(t), emit_spell, need_vowel = FALSE), 0)
  all <- if (length(trs) == 1L) per else sw_unique(trs, emit_spell, need_vowel = FALSE)

  e <- if (is.null(entries)) NULL else entries[entries$pronunciation == pron, ]
  if (is.null(e) || !nrow(e)) e <- data.frame(word_index = NA_integer_, word = NA_character_)
  np <- length(cp)
  ei <- rep(seq_len(nrow(e)), each = np)       # one block of parse rows per entry
  pj <- rep(seq_len(np), nrow(e))
  t3 <- data.frame(
    word_index = e$word_index[ei], pronunciation = pron, word = e$word[ei], n_parses = np,
    parse_no = vapply(cp, `[[`, 0L, "parse_no")[pj],
    parse = vapply(cp, function(x) paste(x$ph, collapse = "/"), "")[pj],
    units = vapply(cp, function(x) length(x$ph), 0L)[pj],
    s_count = per[pj],
    stringsAsFactors = FALSE)
  t4 <- data.frame(word_index = e$word_index, pronunciation = pron, word = e$word,
                   n_parses = np, total_s_count = all, stringsAsFactors = FALSE)
  spellings <- if (enum_limit > 0 && all(vapply(trs, `[[`, 0, "count") <= enum_limit))
    sort(as.character(unique(unlist(lapply(trs, .sw_enum, emit = emit_spell)))))
  else character(0)
  list(t3 = t3, t4 = t4, spellings = spellings)
}
