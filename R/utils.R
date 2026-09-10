#' Extract the Accession Numbers from a bibliography class object
#'
#' @param testset_ref a list of class bibliography
#' @param random_references a vector of indices of testset_ref which will be the development set
#' @param validation_set should the accession numbers of the validation set be returned as well
#'
#' @returns A list with all accession numbers `testset` and optionally the accession numbers for `development_set` and `validation_set`
#'
#' @examples
#' \dontrun{
#' references <- read_bibliography(ris, return_df = FALSE)
#' return_pmids(references)
#' }
return_pmids <- function(testset_ref, random_references = NULL, validation_set = FALSE){
  if(any(grepl("accession", testset_ref))){

  testset_pmids<- testset_ref %>%
    map(pluck, "accession") %>%
    flatten_chr

  if(validation_set){
    development_set_pmids <- testset_pmids[random_references]
    validation_set_pmids <- testset_pmids[setdiff(seq(testset_ref),random_references)]

    result <- list("testset" = testset_pmids,
                   "development_set"= development_set_pmids,
                   "validation_set" = validation_set_pmids)
  }else{
    result <- list("testset" = testset_pmids)
  }
  return(result)
  }else {
    return("No accession numbers provided")
  }
}

#' Calculate the z-scores as a binomial distribution
#'
#' @param testset a data frame containing a quanteda.textstats document feature matrix
#' @param popset_norms a data frame containing a quanteda.textstats document feature matrix which is to be assumed the basic population
#' @param key_testset the column name containing the terms which should be analyzed
#' @param key_popset the corresponding column name in the popset_norms
#'
#' @returns a data frame containing the z-scores
#' @examples
#' \dontrun{
#' calculate_z_scores(testset[["freetext"]],
#' popset[["freetext"]],
#' key_testset = "feature",
#' key_popset = "feature")
#' }

calculate_z_scores <- function (testset, popset_norms, key_testset = "MeSH", key_popset) {
  key <- eval(key_popset)
  names(key) <- eval(key_testset)

  z_table <- testset %>%
    left_join(popset_norms, by = {{ key }}) %>%
    mutate(#n = sum(frequency, na.rm = T),
      E = .data$n*.data$p,
      var = .data$E*(1-.data$p),
      z = (.data$frequency - .data$E)/sqrt(.data$var),
      approx_criteria = .data$var >= 9) %>%
    arrange(desc(.data$z))

  #change z-Score for Values, that are not in population Set to 10000
  z_table$z <-z_table$z %>% replace_na(10000)

  return(z_table)
}

#' Parse a RIS or PubMed format text file into a list
#'
#' @param x path to a RIS or PubMed format file
#'
#' @returns a list with each reference as a character vector
#' @export
#'
#' @examples
#' ris <- c("TY  - JOUR",
#'         "AU  - Kapp",
#'         "TI  - Titles",
#'         "PY  - 2023",
#'         "JOUR  - IQWiG Journal",
#'         "KW  - Systematic Reviews as Topic",
#'         "ER  -")
#'
#'tmp <- tempfile(fileext = ".txt")
#'writeLines(ris, tmp)
#'raw_ris_data(tmp)
#'
raw_ris_data <- function(x){
  temp <- readLines(x)
  temp <- temp[temp != ""]
  if ( any(grepl ("^(PMID- )", temp))) {
    start_ref <- grep("PMID- ", temp)
    result <- split(temp, cumsum(seq_along(temp) %in% start_ref[-1])+1)
  }else if(any(grepl("ER  -", temp))){
    start_ref <- grep("ER  -", temp)+1
    result <- split(temp, cumsum(seq_along(temp) %in% start_ref)+1)
  }else{
    stop("Wrong file format. Raw data cannot be processed to return subset of original data.")
  }
  return(result)
}

