#' Create a quanteda text corpus from bibliographic data
#'
#' `create_corpus` accepts an list object of class bibliography and
#'  extracts the titles and abstracts for a text analysis.
#'  Empty titles and abstracts are marked with "NO_TITLE" or "NO_ABSTRACT" in the corpus.
#'  Author names are normalized and problems with missing first authors are handled by substituting empty first author fields with "Anonymous".
#'
#' @param reference_set a list object created with read_bibliography()
#'
#' @returns a quanteda text corpus with titles and abstracts as the text corpus

#' @examples
#' \dontrun{
#' create_corpus(references)
#' }

create_corpus <- function(reference_set){
  references <- as.data.frame (reference_set) |>
    select(tidyselect::any_of(c("author", "year", "title", "abstract", "ID", "accession")))

  # Remove empty titles and abstracts (previously "NA" was interpreted as string by quanteda) and special characters
  references <-references |>
    tidyr::replace_na(list(title = "NO_TITLE", abstract = "NO_ABSTRACT"))

  references$text <- paste(references$title, references$abstract, sep = "\n ")

  # name documents with algorithm "author_year"
  if (any(names(references) == "author")) {
 references$author <- references$author |>
   replace_na("Anonymous")
  } else {
    references$author <- "Anonymous"
 }

  # create a quanteda text corpus
  references <- quanteda::corpus(references, text_field = "text")

  authors <- strsplit(references$author, split = ",")
  first_authors <- lapply(authors,"[[",1)
  Docnames <- make.unique(paste(first_authors, references$year, sep = "_"))
  quanteda::docnames(references) <- Docnames

  return(references)
}
