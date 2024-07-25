#' Process raw Endnote ris file with PubMed data into population norm set
#'
#' @inheritParams create_testset
#' @param mesh_xml optional argument, character vector with the path to an xml file containing MeSH descriptors in the NLM format
#' @param qual_xml optional argument, character vector with the path to an xml file containing MeSH descriptors in the NLM format
#'
#'
#' @returns popset a list of objects
#' popset
#' ...$freetext
#' ...$MeSH.terms
#' ...$qualifier
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom rlang set_names
#'
#' @export
#' @examples
#' ris <- c("TY  - JOUR",
#'          "AU  - Kapp",
#'          "TI  - Titles",
#'          "PY  - 2023",
#'          "JOUR  - IQWiG Journal",
#'          "KW  - Systematic Reviews as Topic",
#'          "ER  -")
#'
#' tmp <- tempfile(fileext = ".txt")
#' writeLines(ris, tmp)
#' create_popset(tmp)


create_popset <- function(risfile, mesh_xml =  NULL, qual_xml = NULL){
  popset_ref <- read_bibliography(risfile, return_df = F)
  message("Population set imported.")
  popset_df <- create_corpus(popset_ref)
  message("Titles and abstracts corpus created.")
  popset_df <- prepare_freq_table(popset_df)
  popset_df <- popset_df  |>
    select(!("group")) |>
    rename(Norm.frequency = "frequency",
           Norm.docfreq = "docfreq",
           Norm.rank = "rank",
           N = "n",
           Norm.coverage = "coverage") |>
    mutate(p = .data$Norm.frequency/.data$N)
  message("Term probabilities calculated.")
  popset_MeSH <- create_MeSH_norm_frequencies(popset_ref, mesh_xml, qual_xml)
  newMeSH <- popset_MeSH$newMeSH
  newQual <- popset_MeSH$newQual

  message("MeSH probabilities calculated.")
  result <- list(
    "popset" = list("freetext" = popset_df, "MeSH.Terms" = popset_MeSH[["headings"]], "qualifier" = popset_MeSH[["qualifier"]]),
    "newMeSH" = newMeSH,
    "newQual" = newQual)

  return(result)
}
