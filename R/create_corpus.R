
create_corpus <- function(reference_set){
  references <- as.data.frame (reference_set) %>%
    select(tidyselect::any_of(c("author", "year", "title", "abstract", "ID", "accession")))

  # Remove empty titles and abstracts (previously "NA" was interpreted as string by quanteda) and special characters
  references <-references %>%
    tidyr::replace_na(list(title = "NO_TITLE", abstract = "NO_ABSTRACT"))

  references$text <- paste(references$title, references$abstract, sep = "\n ")

  # name documents with algorithm "author_year"
  if (any(names(references) == "author")) {
 references$author <- references$author %>% replace_na("Anonymous")
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
