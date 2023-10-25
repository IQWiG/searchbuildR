#' Search MeSH Terms in MeSH tree
#'
#' @param meshTerm a character vector with MeSH headings
#'
#' @returns a character vector with MeSH unique identifiers
#' @export
#'
#' @examplesIf interactive()
#' mesh_esearch(c("Breast Neoplasms", "Gingivitis"))
mesh_esearch <- function(meshTerm){
  resp <- httr2::request("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi") %>%
    httr2::req_url_query(tool = "searchbuildR",
                         email = "recherche@iqwig.de",
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
