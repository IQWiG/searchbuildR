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

# create tokens
