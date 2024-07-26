#' Create z-scores for every token in the testset provided
#'
#' See `vignette("z_scores", package = "searchbuildR")` for more details.
#'
#' @inheritParams create_testset
#' @param references an object created with create_testset() to be used fo analysis instead of a raw ris file
#' @param custom_popset a population set created with create_popset()
#'
#' @returns a list
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
#' z_scores(tmp)
#'
z_scores <- function(risfile = NULL, references = NULL, custom_popset = NULL, dev_set = FALSE, seed = NULL){

  #load population set
  if(!is.null(custom_popset)){
    popset <- custom_popset
  }
  if(!is.null(risfile)){
  # calculate frequency table for testset
  testset <- create_testset(risfile, dev_set = dev_set, seed = seed)
  }else if (!is.null(references)){
  testset <- references
  }else {
    stop("Please provide a valid set of test references as risfile or created with create_testset()")
  }
  testset <- create_MeSH_qualifier_lists(testset)


  zscore_freetext <- calculate_z_scores(testset[["freetext"]], popset[["freetext"]], key_testset = "feature", key_popset = "feature")
  zscore_MeSH <- calculate_z_scores(testset[["MeSH.Terms"]][["MeSH"]], popset[["MeSH.Terms"]], key_popset = "MeSH")
  zscore_qualifier <- calculate_z_scores(testset[["MeSH.Terms"]][["qualifier"]], popset[["qualifier"]], key_popset = "MeSH")

  result <- list("freetext" = zscore_freetext,
                 "MeSH" = zscore_MeSH,
                 "qualifier" = zscore_qualifier,
                 "all_keywords" = testset$MeSH.Terms$MeSH_with_qualifier,
                 "leftover_keywords" = testset$MeSH.Terms$Not_MeSH)
  return(result)
}


#' Utilities function to merge testset and population set data.
#'
#' The function extracts the MeSH terms and qualifiers from the testset data and counts their frequency.
#'
#' @param testset an object created with create_testset() to be used fo analysis instead of a raw ris file
#'
#' @returns the input object with processed data
#'
#' @examples
#' \dontrun{
#' create_MeSH_qualifier_lists(testset)
#' }

create_MeSH_qualifier_lists <- function(testset){
  testset[["MeSH.Terms"]][["MeSH"]] <-  testset[["MeSH.Terms"]][["all_keywords"]] |>
    filter(.data$MeSH %in% .env$popset[["MeSH.Terms"]][["MeSH"]]) |>
    mutate(n = sum(frequency, na.rm = T))
  testset[["MeSH.Terms"]][["qualifier"]]<- testset[["MeSH.Terms"]][["all_keywords"]] |>
    filter(.data$MeSH %in% .env$popset[["qualifier"]][["MeSH"]])|>
    mutate(n = sum(frequency, na.rm = T))

  return(testset)
}
