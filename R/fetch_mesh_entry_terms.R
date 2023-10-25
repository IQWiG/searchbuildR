#' Retrieve MeSH entry terms for MeSH Headings
#'
#' @param meshTerm a character vector with MeSH headings
#'
#' @returns a list of entry terms
#' @export
#'
#' @examplesIf interactive()
#' get_mesh_entry_terms(c("Breast Neoplasms", "Gingivitis"))
get_mesh_entry_terms <- function(meshTerm){
  uids <- mesh_esearch(meshTerm)
  resp <- mesh_esummary(uids)
    resp <- resp %>%
      purrr::pluck("result")
    entryTerms <- resp %>%
      purrr::discard_at("uids") %>%
      purrr::map(function(x) {list("entryTerms" = purrr::pluck(x, "ds_meshterms"))})
    parentsUID <- resp  %>%
      purrr::discard_at("uids") %>%
      map(pluck, "ds_idxlinks") %>%
      map(pluck, 1, "parent") ###tbc https://tidyr.tidyverse.org/articles/rectangle.html

    parentsTerms <- mesh_esummary(unname(unlist(parentsUID))) %>%
      purrr::pluck("result") %>%
      purrr::discard_at("uids") %>%
      map(pluck, "ds_meshterms", 1)

    parentsMesh <- map(parentsUID, function(x) {list("parentsTerm" = x <- parentsTerms[x == names(parentsTerms)][[1]])})

    return(list("entryTerms" = entryTerms, "parents" = parentsMesh))
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
