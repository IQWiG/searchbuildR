#' Retrieve MeSH entry terms for MeSH Headings
#'
#' @param meshTerm a character vector with MeSH headings
#' @param email a valid e-mail address, which will be sent with the API request to identify the API user (required by NLM)
#'
#' @returns a list of entry terms
#' @export
#'
#' @examplesIf interactive()
#' get_mesh_entry_terms(c("Breast Neoplasms", "Gingivitis"), email = "xx@xx.xxxx")
get_mesh_entry_terms <- function(meshTerm, email){
  uids <- mesh_esearch(meshTerm, email)
  resp <- mesh_esummary(uids, email)
    resp <- resp %>%
      purrr::pluck("result")
    entryTerms <- resp %>%
      purrr::discard_at("uids") %>%
      purrr::map(function(x) {list("entryTerms" = purrr::pluck(x, "ds_meshterms"))})
    parentsUID <- resp  %>%
      purrr::discard_at("uids") %>%
      map(pluck, "ds_idxlinks") %>%
      map(pluck, 1, "parent") ###tbc https://tidyr.tidyverse.org/articles/rectangle.html

    parentsTerms <- mesh_esummary(unname(unlist(parentsUID)), email) %>%
      purrr::pluck("result") %>%
      purrr::discard_at("uids") %>%
      map(pluck, "ds_meshterms", 1)

    parentsMesh <- map(parentsUID, function(x) {list("parentsTerm" = x <- parentsTerms[x == names(parentsTerms)][[1]])})

    return(list("entryTerms" = entryTerms, "parents" = parentsMesh))
}


#' Search MeSH Terms in MeSH tree
#'
#' @param meshTerm a character vector with MeSH headings
#' @param email a valid e-mail address, which will be sent with the API request to identify the API user (required by NLM)
#'
#' @returns a character vector with MeSH unique identifiers
#' @export
#'
#' @examplesIf interactive()
#' mesh_esearch(c("Breast Neoplasms", "Gingivitis"), email = "xx@xx.xxxxx")
mesh_esearch <- function(meshTerm, email){
  resp <- httr2::request("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi") %>%
    httr2::req_url_query(tool = "searchbuildR",
                         email = email,
                         db = "mesh",
                         retmode = "json",
                         term = paste(paste0(meshTerm,"[MESH]"), collapse = " OR ")) %>%
    httr2::req_perform()
  if (length(resp$body) == 0){
    return("The request retrieved no results.")
  } else {
    resp <- resp %>%
      httr2::resp_body_json()
    resp <- resp %>%
      purrr::pluck("esearchresult", "idlist") %>%
      unlist()
    return(resp)
  }
}

#' Get MeSH entry terms from MeSH tree
#'
#' @param uids a vector with unique MeSH identifiers
#' @param email a valid e-mail address, which will be sent with the API request to identify the API user (required by NLM)
#'
#' @returns  a list of entry terms and MeSH parent subheadings
#' @export
#'
#' @examplesIf interactive()
#' mesh_esummary(c("68005891","68001943"), email = "xx@xx.xxxxx")
mesh_esummary <- function(uids, email){
  resp <- httr2::request("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi") %>%
    httr2::req_url_query(tool = "searchbuildR",
                         email = email,
                         db = "mesh",
                         retmode = "json",
                         id = paste(uids, collapse = ",")) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()
  stopifnot("At least one MeSH identifier does not exist" = !(any(grepl(pattern = "cannot get document summary", resp))))
  return(resp)
}

# retrieve information on MeSH entrez database
#mesh <- httr2::request("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/einfo.fcgi") %>%
#   httr2::req_url_query(db = "mesh",
#                      retmode = "json") %>%
#  req_perform() %>%
#  httr2::resp_body_json()
#str(mesh)
#
#mesh_fields <- mesh %>%
#  pluck("einforesult", "dbinfo", 1, "fieldlist") %>%
#  map(pluck, "name")
#
#mesh_description <- mesh %>%
#  pluck("einforesult", "dbinfo", 1, "fieldlist") %>%
#  map(pluck, "description")
#
