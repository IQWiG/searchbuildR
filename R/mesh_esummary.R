#' Get MeSH entry terms from MeSH tree
#'
#' @param uids a vector with unique MeSH identifiers
#'
#' @returns  a list of entry terms and MeSH parent subheadings
#' @export
#'
#' @examplesIf interactive()
#' mesh_esummary(c("68005891","68001943"))
mesh_esummary <- function(uids){
  resp <- httr2::request("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi") %>%
    httr2::req_url_query(tool = "searchbuildR",
                         email = "recherche@iqwig.de",
                         db = "mesh",
                         retmode = "json",
                         id = paste(uids, collapse = ",")) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()
  stopifnot("At least one MeSH identifier does not exist" = !(any(grepl(pattern = "cannot get document summary", resp))))
  return(resp)
}
