#' Find adjacent tokens in a corpus
#'
#' `adjacency` extracts adjacent tokens for a user definded skip-gram from
#'  a text corpus and calculates their frequency using the quanteda package. Skip-grams are adjacent words, where a word in between is skipped.
#'  Example: "quality life" is a skip-1-gram of "quality of life".
#'
#' @param corpus_data a corpus object from created with quanteda::corpus()
#' @param skip integer, the number of tokens to skip to calculate adjacency
#'
#' @returns a data frame showing the skip-grams of the input corpus with columns "feature" "frequency" and "gram"
#'
#' @examples
#' \dontrun{
#' adjacency(corpus)
#' }
#'
adjacency <- function(corpus_data, skip = 0) {
  corpus_data |>
    quanteda::tokens(split_hyphens = TRUE, remove_punct = T, remove_symbols = T, remove_numbers = T, padding = F) |>
    quanteda::tokens_remove(quanteda::stopwords("en"), padding = T) |>
    quanteda::tokens_ngrams(n = 2, skip = skip, concatenator = " ") |>
    quanteda::dfm() |>
    quanteda.textstats::textstat_frequency()|>
    dplyr::select("feature", "frequency") |>
    dplyr::mutate(gram = skip + 2)
#  return(skip)
}

#' A wrapper function to summarise adjacency() for multiple skip-grams
#'
#' @inheritParams adjacency
#' @param ngrams integer, defines the window of tokens (ngram) in which to identify skip-grams
#'
#' @returns a data frame showing the skip-grams of the input corpus with columns "feature" "frequency"
#' and a column "Skip-x" for the frequency of each skip-gram.
#'
#' @examples
#' \dontrun{
#' summarise_adjacency(corpus)
#' }

summarise_adjacency <- function(corpus_data, ngrams = 4){
  result_loop <- vector("list", ngrams - 1)

  for (i in seq.int(from = 2, to = ngrams)){
  skip <- i - 2
    result_loop[[i-1]] <- adjacency(corpus_data, skip = skip)
  }
  result <- result_loop |>
    bind_rows() |>
    group_by(.data$feature) |>
    dplyr::reframe(skip_gram_frequency = .data$frequency,
            skip = .data$gram - 2,
            frequency = sum(.data$frequency)) |> # avoid dplyr message about group tibble
    arrange(.data$skip) |>
    tidyr::pivot_wider(names_from = "skip",
                       names_prefix = "Skip-",
                       values_from = "skip_gram_frequency",
                       values_fn = as.character, # otherwise values_fill throws an error
                       values_fill = "-") |>
    arrange(desc(frequency))
  return(result)
}

