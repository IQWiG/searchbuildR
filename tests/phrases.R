#'Extract all tokens (2-5?)
#'Count frequency
#'select those occuring in 80% as skip-gram
#'repeat with left-over 20% until no term occurs in more than 80% or if only 3 references leftover
#'return left-over references
#'
library(testthat)
normset <- revtools::read_bibliography("C:/Users/kapp/Desktop/Population Set 2022/populationSet2022.ris")
normset$text <- paste(normset$title, normset$abstract, sep = "\n ")
corpus_normset <- quanteda::corpus(normset, docid_field = "label", text_field ="text")

gram_2 <- corpus_normset |>
  quanteda::tokens(split_hyphens = TRUE, remove_punct = T, remove_symbols = T, remove_numbers = T, padding = F) |>
  quanteda::tokens_remove(quanteda::stopwords("en"), padding = T) |>
  quanteda::tokens_ngrams(n = c(2,3,4), skip = 0, concatenator = " ") |>
  quanteda::dfm() |>
  quanteda.textstats::textstat_frequency() |>
  dplyr::mutate(n = sum(.data$frequency,na.rm=T),
         coverage = .data$docfreq/length(corpus_normset)*100) |>
  dplyr::rename(Norm.frequency = "frequency",
         Norm.docfreq = "docfreq",
         Norm.rank = "rank",
         N = "n") |>
  dplyr::mutate(p = .data$Norm.frequency/.data$N)

testset_corpus <- readRDS(testthat::test_path("fixtures","testset_ref.rds"))|>
  tibble::as_tibble() |>
  tidyr::unite(text, c("title", "abstract"), sep = "\n ") |>
  quanteda::corpus(docid_field = "label", text_field = "text")

  testset_gram_2 <- testset_corpus |>
    quanteda::tokens(split_hyphens = TRUE, remove_punct = T, remove_symbols = T, remove_numbers = T, padding = F) |>
    quanteda::tokens_remove(quanteda::stopwords("en"), padding = T) |>
    quanteda::tokens_ngrams(n = c(2,3,4), skip = 0, concatenator = " ") |>
    quanteda::dfm() |>
    quanteda.textstats::textstat_frequency() |>
    dplyr::mutate(n = sum(.data$frequency,na.rm=T),
                  coverage = .data$docfreq/length(testset_corpus)*100)

two_gram <- testset_gram_2 |>
  dplyr::left_join(gram_2, by = "feature") |>
  dplyr::mutate(#n = sum(frequency, na.rm = T),
    E = .data$n*.data$p,
    var = .data$E*(1-.data$p),
    z = (.data$frequency - .data$E)/sqrt(.data$var),
    approx_criteria = .data$var >= 9)  |>
  dplyr::arrange(desc(.data$z)) |>
  dplyr::select(feature, z, frequency, docfreq)

two_gram |>
  dplyr::arrange(desc(docfreq)) |>
    View()

two_gram |>
  dplyr::filter(docfreq >= length(testset_corpus)*0.80) |>
  dplyr::pull(feature)

testset_corpus
