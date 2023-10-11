
#' Fetch random sample of pubmed references
#'
#' @param size approximate number of references to be sampled
#' @param maxUID biggest UID to be considered for random sample
#'
#' @returns a list with two elements `seed´ (the random seed applied) and ´result´ (retrieved references in PubMed format)
#' @export
#'
#' @examples
#' random_pubmed_fetch(size = 2, maxUID = 37000000)
random_pubmed_fetch <- function(size, maxUID){
  random_uids <- sample_uids(size = size, maxUID = maxUID)
  # Efetch request to entrez direct (Pubmed API) and write retrieved references into a text-file in Pubmed-Format
  recs <-efetch(random_uids$random_result)
  # Some PubMed entries contain formatting errors
  recs <- clean_efetch_result(recs)
  return(list(seed  = random_uids$seed, result = recs))
}

sample_uids <- function (size, maxUID) {
  stopifnot("maxUID must me above 1001" = maxUID > 1000)
  seed <- sample.int(.Machine$integer.max, 1L) # set fixed random engine for reproducibility
  random_PMIDS_to8d3 <- withr::with_seed(seed, sample(x = 1000:maxUID, size = size)) # sample random PMIDs
  random_PMIDS_from8d3 <- withr::with_seed(seed,sample(x = 30000000:maxUID, size = size*0.06)) # sample appropriate proportion of highest PMIDs in order to have a sample size of ~400 of the latest 4 years (since 2019)
  random_result <- unique(c(random_PMIDS_to8d3, random_PMIDS_from8d3)) # append all retrieved PMIDs into one vector
return(list (seed = seed, random_result = random_result))
}

efetch <- function(uid) {
resp <- httr2::request("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi") %>%# add if statement for longer than 200 IDs (required by NLM)
    httr2::req_body_form(tool = "searchbuildR",
                         email = "recherche@iqwig.de",
                         db = "pubmed",
                         id = paste(format(uid, scientific = FALSE, trim = TRUE), collapse = ","),
                         rettype = "medline") %>%
    httr2::req_perform()
if (length(resp$body) == 0){
  return("The request retrieved no results.")
} else {
  resp <- resp %>%
    httr2::resp_body_string() %>%
    stringr::str_split_1(pattern = "\n")
  return(resp)
}
}


clean_efetch_result <- function(recs){

  recs <- gsub("The following PMID is not available","", recs) # legacy: delete unavailable PMIDS
  if(recs[1] == ""){ # the response usually starts with an empty line which should be trimmed
    recs <- recs[-1]
  }
  # Some PubMed references contain false carriage returns in the MeSH terms which creates errors, when parsing MeSH terms
  before_white_lines <- grep("^\\s{6}", recs) - 1 # grep lines with not tag and return line above
  MH_lines <- grep("^MH\\s", recs)
  incomplete_lines <-  intersect(MH_lines, before_white_lines)

  if(length(incomplete_lines) > 0){
    recs[incomplete_lines] <- paste0(recs[incomplete_lines], trimws(recs[incomplete_lines + 1], whitespace = "^\\s{6}"))
    recs <- recs[-(incomplete_lines +1)]
  }
  return(recs)
}
#
## plot PMID disribution
#options(scipen=999)
#plot(density(random_PMIDS_to8d3))
#plot(density(random_PMIDS_from8d3))
#
