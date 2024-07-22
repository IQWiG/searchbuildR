#' Sample a random set of PubMed references via E-Utilities API
#'
#' @param sample_size positive integer value < 100,000 defining the size of references to be retrieved
#' @param filters a character vector defining a study filter to restrict the retrieved references. Default is "none" or "RCT".
#' The option "RCT" uses the latest PubMed "Therapy/Broad"\[filter\]
#' @param year_range a character vector defining the range of publication years to be considered
#' in the format "YYYY:YYYY" (e.g. "2000:2024")
#' @param max_pmid the highest PMID to be considered
#' @param email a valid e-mail address, which will be sent with the API request to identify the API user (required by NLM)
#' @param seed an integer value to control the random seed used for sampling
#'
#' @returns an object with a list of random PubMed
#'  references. A histogram is produced showing the distribution of publication years
#'
#' @export
#' @examplesIf interactive()
#' sample_popset(sample_size = 10, year_range = "1900:2024")
#'

sample_popset <- function(sample_size, filters = "none",
                          year_range, max_pmid = 40000000,
                          email,
                          seed = NULL){
  stopifnot("Please provide an e-mail address for the API request to PubMed" = grepl("^([\\w\\-\\.])+@([\\w\\-]+\\.)+[\\w\\-]{2,4}$", email, perl = TRUE))
  stopifnot("Choose a sample_size below 100,000" = sample_size < 100000)

  filterSyntax <- switch(filters,
         none = NULL,
         RCT = "therapy/broad[filter]" #,
        # sensNonRCT = paste0("(", filter[["sensNonRCT"]],")"),
        # specNonRCT = paste0("(", filter[["specNonRCT"]],")"),
        # nonRCT = paste0(paste0("(", filter, ")"), collapse = " OR ")
  )

  initialSearchTerm <- ifelse(is.null(filterSyntax), paste0(year_range,"[pdat]"), paste(filterSyntax, paste0(year_range,"[pdat]"), sep = " AND "))

  # initial search
  initialSearch <- esearch_post_req(term = initialSearchTerm, email = email)  |>  httr2::req_perform() |> httr2::resp_body_json()
  #get webenv
  webEnv_init <- initialSearch$esearchresult$webenv
  queryKey_init <- initialSearch$esearchresult$querykey

  if(is.null(seed)){
    seed <- sample.int(.Machine$integer.max, 1L)
  }
  UIDSearchTermList <- create_UID_esearch_terms( sample_size = sample_size, seed = seed, max_pmid = max_pmid) # maximum search query length is 200000 characters for one request

  retrievedPMIDs <- post_random_PMIDs(UIDSearchTermList, sample_size = sample_size, webEnv = webEnv_init,
                                          email = email)

  if(retrievedPMIDs$counter < sample_size){
    if(retrievedPMIDs$counter == 0){
      stop("Error: request retrieved no results,
           please check your search filter input.", .call = FALSE)
    }
    message(paste("Sampling process retrieved only",
                  retrievedPMIDs$counter,
                  "instead of requested",
                  sample_size,
                  "random references. Consider repeating the request."))
  }  #loop efetch

  last_batch_size <- as.numeric(retrievedPMIDs$countHits$esearchresult$count) - (retrievedPMIDs$counter - sample_size)

  last_hits <- esearch_post_req(term = paste0("#",tail(retrievedPMIDs$queryKeys,1)),
                       webEnv = webEnv_init,
                       retmax = last_batch_size,
                       email = email) |>
    httr2::req_perform() |>
    httr2::resp_body_json()|>
    purrr::chuck("esearchresult", "idlist")

  #fetch MEDLINE format records
  result_all_but_last <- purrr::map(head(retrievedPMIDs$queryKeys,-1),
                                    \(x) {efetch_with_queryKey(queryKey = as.numeric(x),
                                                               webEnv = webEnv_init,
                                                               email = email)}) |>
    purrr::flatten_chr()
  result_last_hits <- efetch_with_idlist(last_hits, email = email)
  result <- c(result_all_but_last, result_last_hits)
  plot_data(result, retrievedPMIDs$counter, seed)


  message(paste(stringr::str_count(result, "PMID- ") |> sum(),
            "random PubMed references retrieved"))

# return results
  return(c(result_all_but_last, result_last_hits))
}

#' Creating an esearch POST request via E-Utilities using the history server
#'
#' @inheritParams sample_popset
#' @param term a character vector with a valid Pubmed query (see also https://pubmed.ncbi.nlm.nih.gov/help/#how-do-i-search-pubmed)
#' @param tool a character vector providing information about the tool that sends the request (defaults to package name "searchbuildR")
#' @param retmax the maximum number of references to be retrieved from the total search result
#' @param webEnv a character vector providing a Web environment string returned from a previous ESearch (see also https://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.ESearch)
#'
#' @returns an HTTP POST request which can be sent to the E-Utilities API
#'
#' @examples
#' searchbuildR:::esearch_post_req(term="20204[pdat]",email="xx@xx.xxxx")
#' #|>  httr2::req_perform()
#'
esearch_post_req <- function(term, tool = "searchbuildR", email, retmax = NULL, webEnv = NULL){
  #  if(maxdate == "now"){
  #    maxdate <- lubridate::today() |> lubridate::year() |> as.character()
  #  }
  httr2::request("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi") |>
    httr2::req_url_query (tool = tool,
                          email = email,
                          db = "pubmed",
                          retmode = "json",
                          retmax = retmax,
                          usehistory = "y",
                          WebEnv = webEnv) |>
    httr2::req_body_form(term = term) # for very long search queries NCBI suggest to use the POST method
  }

#' Creating an Efetch GET request via E-Utilities using the history server
#'
#' @inheritParams esearch_post_req
#' @param queryKey Integer or character value containing an integer query key returned by a previous ESearch
#'
#' @returns a character vector containing the requested PubMed reference metadata in MEDLINE format
#'
#' @examples
#' \dontrun{
#' efetch_with_queryKey(queryKey = 1, webEnv = "JFm5NJfsp6rHJGKsdjg576GU" , email = "xx@xx.xxxxx")
#' }
#'
efetch_with_queryKey <- function(queryKey, webEnv, email) {
  resp <- httr2::request("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi") |>
    httr2::req_url_query(tool = "searchbuildR",
                         email = email,
                         db = "pubmed",
                         rettype = "medline",
                         WebEnv = webEnv,
                         query_key = queryKey) |>
    httr2::req_perform()|>
    httr2::resp_body_string() |>
    stringr::str_split_1(pattern = "\n")
}

efetch_with_idlist <- function(idlist, email) {
  resp <- httr2::request("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi") |>
    httr2::req_url_query(tool = "searchbuildR",
                         email = email,
                         db = "pubmed",
                         rettype = "medline") |>
    httr2::req_body_form( id = paste(idlist, collapse =",")) |>
    httr2::req_perform()|>
    httr2::resp_body_string() |>
    stringr::str_split_1(pattern = "\n")
}

#' Create a valid PubMed search query using random integer numbers as PMIDs
#' @inheritParams sample_popset
#' @param seed an integer value to control the random seed used for sampling
#'
#' @returns A list of character vectors each containing a valid PubMed search query of a maximum length of 20000 character symbols.
#' Each list value can be passed on to the "term" argument of esearch_post_req() to create a POST request.
#' @export
#'
#' @examples
#' create_UID_esearch_terms(sample_size = 4, max_pmid = 30000000)
#'
create_UID_esearch_terms <- function(sample_size = 20000, max_pmid = 40000000, seed = NULL) {
  if(is.null(seed)){
  seed <- sample.int(.Machine$integer.max, 1L)
  }
  # randomNumber_size makes sure, that enough random numbers are generated for very small requested samples
  randomNumber_size <- ifelse(sample_size*6 > 10000, sample_size*6, 10000)
sampleUID <- withr::with_seed(seed, sample(1000:max_pmid, randomNumber_size))
sampleUID <- paste0(sampleUID,"[uid] OR ")

countChars <- nchar(sampleUID) |> cumsum()/20000 # 20000 experimentally determined acceptable search query length by number of characters

termList <- split(sampleUID, round(countChars, digits = 0))
purrr::map(termList, \(x) {paste0(x, collapse = "") |> stringr::str_sub(end = -5)}) # delete the trailing " OR " of the search query
}

post_random_PMIDs <- function (UIDSearchTermList, sample_size, webEnv, email){
  index <- 1
  counter <- 0
  queryKeys <- character(length = 0)

  while (as.numeric(counter) < sample_size) {
    if(index > length(UIDSearchTermList)) {
      break
    }
    countHits <- esearch_post_req(term = paste0("#1 AND (", UIDSearchTermList[[index]],")"), #
                                  webEnv = webEnv,
                                  email = email) |>
      httr2::req_perform() |>
      httr2::resp_body_json()
    counter <- counter + as.numeric(countHits$esearchresult$count)
    index <- index + 1
    queryKeys <- append(queryKeys, countHits$esearchresult$querykey)
    Sys.sleep(0.3)
  }
  return(list(
    countHits = countHits,
    queryKeys = queryKeys,
    counter = counter
  ))
}

clean_efetch_result <- function(recs){
  # Some PubMed references contain false carriage returns in the MeSH terms which creates errors, when parsing MeSH terms
  before_white_lines <- grep("^\\s{6}", recs) - 1 # grep lines with no tag and return line above
  MH_lines <- grep("^MH\\s", recs)
  incomplete_lines <-  intersect(MH_lines, before_white_lines)

  if(length(incomplete_lines) > 0){
    recs[incomplete_lines] <- paste0(recs[incomplete_lines], trimws(recs[incomplete_lines + 1], whitespace = "^\\s{6}"))
    recs <- recs[-(incomplete_lines +1)]
  }
  return(recs)
}

plot_data <- function(result, counter, seed){

  plotData <- stringr::str_extract_all(result, "(?<=DP\\s\\s\\-\\s)(\\d{4})") |>
    as.numeric()

  hist(plotData,
       freq = FALSE,
       breaks = length(unique(plotData)),
       col = "blue",
       main = paste("Distribution of Publication Years for", counter ,"Retrieved References"),
       xlab = "Year",
       sub = paste("created ", format(Sys.Date(), "%d/%m/%y"), "with random seed:", seed)
  )
}
