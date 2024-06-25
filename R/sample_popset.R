#' Sample a random set of PubMed references via E-Utilities API
#' @param sample_size positive integer value defining the size of references to be retrieved
#' @param filters a character vector of either "none" default,"RCT", "nonRCT"
#' @param year_range a character vector, the range of publication years to be considered as
#' @param max_pmid the highest PMID to be considered
#'
#' @returns an object with a list of random PubMed
#'  references, a histogram and
#'  table showing the distribution of publication years of the references
#'
#' @export
#' @example

sample_popset <- function(sample_size, filters = "none",
                          year_range, max_pmid = NULL){

  filterSyntax <- switch(filters,
         none = NULL,
         RCT = "therapy/broad[filter]",
         sensNonRCT = paste0("(", filter[["sensNonRCT"]],")"),
         specNonRCT = paste0("(", filter[["specNonRCT"]],")"),
         nonRCT = paste0(paste0("(", filter, ")"), collapse = " OR ")
  )

  initialSearchTerm <- paste0(filterSyntax, year_range,"[pdat]", collapse = " AND ")

  # initial search
  initialSearch <- esearch(term = initialSearchTerm)  |>  httr2::req_perform() |> httr2::resp_body_json()
  #get webenv
  webEnv_init <- initialSearch$esearchresult$webenv
  queryKey_init <- initialSearch$esearchresult$querykey

  #loop queries until finished   #check number of hits and compare to sample_size #repeat
  UIDSearchTermList <- create_UID_esearch_terms( sample_size = sample_size) # maximum search query length is 200000 characters for one request

  index <- 1
  counter <- 0
  queryKeys <- character(length = 0)

  while (as.numeric(counter) < sample_size) {
    if(index > length(UIDSearchTermList)) {
      break
    }
    countHits <- esearch(term = paste0("#1 AND (", UIDSearchTermList[[index]],")"), #
                         WebEnv = webEnv_init) |>
      httr2::req_perform() |>
      httr2::resp_body_json()
    counter <- counter + as.numeric(countHits$esearchresult$count)
    index <- index + 1
    queryKeys <- append(queryKeys, countHits$esearchresult$querykey)
    Sys.sleep(1)
  }
  last_batch_size <- as.numeric(countHits$esearchresult$count) - (counter - sample_size)
  if(counter < sample_size){
    stop("repeat sampling process: not enough references identified")
  }  #loop efetch
  last_hits <- esearch(term = paste0("#",tail(queryKeys,1)),
                       WebEnv = webEnv_init,
                       retmax = last_batch_size) |>
    httr2::req_perform() |>
    httr2::resp_body_json()|>
    purrr::chuck("esearchresult", "idlist")

#fetch MEDLINE format records
  result_all_but_last <- purrr::map(head(queryKeys,-1), \(x) {efetch_with_queryKey(queryKey = as.numeric(x), webEnv = webEnv_init)}) |>
  purrr::flatten_chr()
  result_last_hits <- efetch_with_idlist(last_hits)

# return results
  return(c(result_all_but_last, result_last_hits))
}

esearch <- function(term, tool = "searchbuildR", email= "claudia.kapp@iqwig.de", rettype = NULL, WebEnv = NULL, retmax = NULL){ # tool und email ändern
  #  if(maxdate == "now"){
  #    maxdate <- lubridate::today() |> lubridate::year() |> as.character()
  #  }
  httr2::request("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi") |>
    httr2::req_url_query (tool = tool,
                          email = email,
                          db = "pubmed",
                          retmode = "json",
                          rettype = rettype,
                          retmax = retmax,
                          usehistory = "y",
                          WebEnv = WebEnv) |>
    httr2::req_body_form(term = term) # for very long search queries NCBI suggest to use the POST method
  }

efetch_with_queryKey <- function(queryKey, webEnv) {
  resp <- httr2::request("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi") |>
    httr2::req_url_query(tool = "searchbuildR",
                         email = "claudia.kapp@iqwig.de",
                         db = "pubmed",
                         rettype = "medline",
                         WebEnv = webEnv,
                         query_key = queryKey) |>
    httr2::req_perform()|>
    httr2::resp_body_string() |>
    stringr::str_split_1(pattern = "\n")
}

efetch_with_idlist <- function(idlist) {
  resp <- httr2::request("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi") |>
    httr2::req_url_query(tool = "searchbuildR",
                         email = "claudia.kapp@iqwig.de",
                         db = "pubmed",
                         rettype = "medline") |>
    httr2::req_body_form( id = paste(idlist, collapse =",")) |>
    httr2::req_perform()|>
    httr2::resp_body_string() |>
    stringr::str_split_1(pattern = "\n")
}

create_UID_esearch_terms <- function(sample_size = 20000, seed = NULL) {
  if(is.null(seed)){
  seed <- sample.int(.Machine$integer.max, 1L)
  }
sampleUID <- withr::with_seed(seed, sample(1000:40000000, sample_size*6))
sampleUID <- paste0(sampleUID,"[uid] OR ")

countChars <- nchar(sampleUID) |> cumsum()/20000 # 20000 experimentally determined acceptable search query length by number of characters

termList <- split(sampleUID, round(countChars, digits = 0))
purrr::map(termList, \(x) {paste0(x, collapse = "") |> stringr::str_sub(end = -5)}) # delete the trailing " OR " of the search query
}
