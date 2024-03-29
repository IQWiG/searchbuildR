
prepare_freq_table <- function(corpus_ref, devSet = FALSE){

  # remove all special characters
  # see also:    https://de.wikipedia.org/wiki/Liste_der_Unicode-Eigenschaften
  # punctuation: remove all characters in the Unicode "Punctuation" [P] class⁠
  # symbols:     all characters in the Unicode "Symbol" ⁠[S]⁠ class
  # hyphens:     if FALSE, do not split words that are connected by hyphenation and hyphenation-like characters in between words, e.g. "self-aware" becomes c("self", "-", "aware")
  # numbers:     remove tokens that consist only of numbers, but not words that start with digits, e.g. ⁠2day⁠

  corpus_ref <- stringr::str_replace_all(corpus_ref, "\\d+|[\\[\\].#@'_:]|NO_TITLE|NO_ABSTRACT", " ") # this line used to be in function create_corpus, before version 0.0.12, remove characters which have shown to be problematic for the quanteda package in the frequency analysis
  dfm_ref <- quanteda::dfm(quanteda::tokens(corpus_ref, remove_punct = T, split_hyphens = T, remove_symbols = T, remove_numbers = T))
  freq_ref <- quanteda.textstats::textstat_frequency(dfm_ref)
  freq_ref <- freq_ref %>%
    mutate(n = sum(.data$frequency,na.rm=T),
           coverage = .data$docfreq/length(corpus_ref)*100)
  return(freq_ref)
}
