#' Sample a random set of PubMed references via EUtils API
#'
#' `sample_popset` generates a random sequence of numbers to be sent as Unique Identifiers (PMIDs)
#' as a request to the EUtils API provided by NLM.
#'
#' @param sample_size positive integer value < 100,000 defining the size of references to be retrieved
#' @param filters a character vector defining a study filter to restrict the retrieved references. Default is "none" or "RCT".
#' The option "RCT" uses the latest PubMed "Therapy/Broad"\[filter\]
#' @param year_range a character vector defining the range of publication years to be considered
#' in the format "YYYY:YYYY" (e.g. "2000:2024")
#' @param max_pmid the highest PMID to be considered, defaults to 40000000.
#' @param email a valid e-mail address, which will be sent with the API request to identify the API user (required by NLM)
#' @param seed an integer value to control the random seed used for sampling
#'
#' @returns an object with a list of random PubMed
#'  references in MEDLINE Format. A histogram is outputted showing the distribution of publication years.
#'
#' @export
#' @examplesIf interactive()
#' sample_popset(sample_size = 10, year_range = "1900:2024")
#'

sample_popset <- function(sample_size,
                          filters = "none",
                          year_range,
                          max_pmid = 40000000,
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
message("Sending initial search request...")
  initialSearchTerm <- ifelse(is.null(filterSyntax), paste0(year_range,"[pdat]"), paste(filterSyntax, paste0(year_range,"[pdat]"), sep = " AND "))

  # initial search
  initialSearch <- esearch_post_req(term = initialSearchTerm, email = email)  |>  httr2::req_perform() |> httr2::resp_body_json()
  #get webenv
  webEnv_init <- initialSearch$esearchresult$webenv
  queryKey_init <- initialSearch$esearchresult$querykey
message("Sampling random PMIDs...")
  if(is.null(seed)){
    seed <- sample.int(.Machine$integer.max, 1L)
  }
  UIDSearchTermList <- create_UID_esearch_terms( sample_size = sample_size, seed = seed, max_pmid = max_pmid) # maximum search query length is 200000 characters for one request
message("Posting random PMIDs to PubMed...")
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
message("Fetching Titles and abstracts...")
  #fetch MEDLINE format records
  result_all_but_last <- purrr::map(head(retrievedPMIDs$queryKeys,-1),
                                    \(x) {efetch_with_queryKey(queryKey = as.numeric(x),
                                                               webEnv = webEnv_init,
                                                               email = email)}) |>
    purrr::flatten_chr()
  result_last_hits <- efetch_with_idlist(last_hits, email = email)
  result <- c(result_all_but_last, result_last_hits)
  plot_popset_data(result, seed)


  message(paste(stringr::str_count(result, "PMID- ") |> sum(),
            "random PubMed references retrieved"))

# return results
  return(c(result_all_but_last, result_last_hits))
}

#' Creating an esearch POST request for the EUtils API using the history server
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
esearch_post_req <- function(term,
                             tool = "searchbuildR",
                             email,
                             retmax = NULL,
                             webEnv = NULL){
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

#' Creating an Efetch GET request via E-Utils API from data on the history server
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
efetch_with_queryKey <- function(queryKey,
                                 webEnv,
                                 email) {
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

#' Creating an Efetch POST request for PubMed via EUtils API from a list of UIDs (PMIDs)
#'
#' @inheritParams sample_popset
#' @param idlist list of PMIDs
#'
#' @returns a character vector containing the requested PubMed reference metadata in MEDLINE format
#' @examplesIf interactive()
#' validEmail <- "ENTER_VALID_EMAIL"
#' efetch_with_idlist(idlist = list( 22587829), email = validEmail)
#'
efetch_with_idlist <- function(idlist,
                               email) {
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
create_UID_esearch_terms <- function(sample_size = 20000,
                                     max_pmid = 40000000,
                                     seed = NULL) {
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

#' Send a POST request with mulitple a search lines to PubMed via Eutils API.
#'
#' `post_random_PMIDs` takes a list of search queries and combines each with (#1 AND), POSTs it to the PubMed history server
#' and retrieves the search result count. The first search (#1) in the current search history (webEnv) thus
#' will be used as a filter.
#'
#' @inheritParams esearch_post_req
#' @param UIDSearchTermList a list of character strings which are valid PubMed search queries.
#' @param sample_size the number of references to be retrieved
#'
#' @returns a list containing countHits(the last search result), "queryKey"(all search line indices) and "counter" (total search size result size)
#' @examplesIf interactive()
#' searchTermList <- list("1", "2")
#' validwebEnv <-"VALID_WEB_ENV"
#' validEmail <- "ENTER_VALID_EMAIL"
#' post_random_PMIDs(searchTermList, sample_size = 2, webEnv = validwebEnv, email = validEmail)
#'
post_random_PMIDs <- function (UIDSearchTermList,
                               sample_size,
                               webEnv,
                               email){
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

#' Clean up untidy MeSH data in MEDLINE format efetch results
#' `clean_efectch_result` handles PubMed references that contain false carriage returns
#' in the MeSH terms which creates errors, when parsing MeSH terms.
#'
#' @param recs a character vector with a MEDLINE format search result
#'
#' @returns the input search result as a cleaned up character vector
#' @examplesIf interactive()
#' ref <- c("MH  - Information Storage and Retrieval","      / methods*")
#' clean_efetch_result(ref)

clean_efetch_result <- function(recs){

  before_white_lines <- grep("^\\s{6}", recs) - 1 # grep lines with no tag and return line above
  MH_lines <- grep("^MH\\s", recs)
  incomplete_lines <-  intersect(MH_lines, before_white_lines)

  if(length(incomplete_lines) > 0){
    recs[incomplete_lines] <- paste0(recs[incomplete_lines], trimws(recs[incomplete_lines + 1], whitespace = "^\\s{6}"))
    recs <- recs[-(incomplete_lines +1)]
  }
  return(recs)
}

#' Plot the distribution of publication years for a search result
#'
#' @param result character vector, MEDLINE format search result
#' @param seed a random seed used to sample random PMIDs
#'
#' @returns a histogram
#'
#' @examplesIf interactive()
#' result <- c("PMID- 1",
#' "DP  - 2021",
#' "PMID- 2",
#' "DP  - 2023",
#' "PMID- 3",
#' "DP  - 2022")
#' plot_popset_data(result = result, seed = 1)
#'
plot_popset_data <- function(result,
                      seed){

  plotData <- stringr::str_extract_all(result, "(?<=DP\\s\\s\\-\\s)(\\d{4})") |>
    as.numeric()
  counter <- stringr::str_count(result, "PMID- ") |>
    sum()

  hist(plotData,
       freq = FALSE,
       breaks = length(unique(plotData)),
       col = "blue",
       main = paste("Distribution of Publication Years for", counter ,"Retrieved References"),
       xlab = "Year",
       sub = paste("created ", format(Sys.Date(), "%d/%m/%y"), "with random seed:", seed)
  )
}
