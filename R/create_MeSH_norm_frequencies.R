#' Calculate new probabilities for MeSH and qualifier for a given population set
#'
#' @inheritParams create_popset
#' @param reference_set a reference_set in RIS or PubMed format created with read_bibliography()
#' @returns a list of 2 data frames with MeSH ("headings") and "qualifier" norms and two list with raw MeSH headings and MeSH qualifiers
#' @export
#'
#' @examples
#'ris <- c("TY  - JOUR",
#'         "AU  - Kapp",
#'         "TI  - Titles",
#'         "PY  - 2023",
#'         "JOUR  - IQWiG Journal",
#'         "KW  - Systematic Reviews as Topic",
#'         "ER  -")
#'
#' tmp <- tempfile(fileext = ".txt")
#' writeLines(ris, tmp)
#' reference_set <- read_bibliography(tmp, return_df = FALSE)
#' create_MeSH_norm_frequencies(reference_set)

create_MeSH_norm_frequencies <- function (reference_set, mesh_xml = NULL, qual_xml = NULL) {
  populationMeSHTable <- prepare_MeSH_table(reference_set)
  message("Population set prepared for creating MeSH norms.")

  if(!is.null(mesh_xml)){
    newMeSH <- update_mesh(mesh_xml)
    currentMeSHDict <- newMeSH$Terms
    message("MeSH updated")
  } else {
    currentMeSHDict <- MeSH_Dictionary
    newMeSH <- NA
    message("Using default MeSH dictionary")
  }
  if(!is.null(qual_xml)){
    newQual <- update_mesh(qual_xml)
    currentQualDict <- newQual$Terms
    message("Qualifier updated")
  } else{
    currentQualDict <- Qualifier_Dictionary
    newQual <- NA
    message("Using default qualifier dictionary")
  }
  MeSHNorms <- calculate_mesh_norms(currentMeSHDict, populationMeSHTable)
  QualNorms <- calculate_mesh_norms(currentQualDict, populationMeSHTable)
  result <- list("headings" = MeSHNorms, "qualifier" = QualNorms, "newMeSH" = newMeSH, "newQual" = newQual)
  message("New norms have been calculated.")
  return(result)
}

update_mesh <- function(mesh_xml){
  MeSHTibble <-  xml2::read_xml(mesh_xml) |>
    xml2::as_list() |>
    tibble::as_tibble()

  MeSHType <- names(MeSHTibble)
  ui_name <- paste0(stringr::str_extract(MeSHType, pattern= ".*(?=RecordSet)"),
                  c("UI", "Name"))
  lookup_name <- c(MeSH = ui_name[2])
  message(paste("New",MeSHType ,"xml imported."))

  MeSHTerms <- MeSHTibble |>
    tidyr::unnest_wider(tidyselect::all_of(MeSHType))|>
    dplyr::select(tidyselect::all_of(ui_name)) |>
    dplyr::mutate(across(everything(), unlist)) |>
    dplyr::rename(tidyselect::all_of(lookup_name))
message("Creating MeSH Tree...")
  MeSHTree <- MeSHTibble |>
    tidyr::hoist(tidyselect::all_of(MeSHType),
                 "UI" = list(ui_name[1], 1L),
                 "TreeNumberList",
                 .remove = FALSE) |>
    tidyr::unnest_longer("TreeNumberList", indices_include = FALSE) |>
    tidyr::hoist("TreeNumberList",
                 TreeNumber = list(1)) |>
    dplyr::select(!all_of(MeSHType))

  result <- list("Terms" = MeSHTerms,
                 "Tree" = MeSHTree)
  return(result)
}

calculate_mesh_norms <- function(dictionary, population_mesh) {
  dictionary |>
  left_join(population_mesh[["all_keywords"]], by = "MeSH") |>
    rename(Norm.frequency = "frequency",
           Norm.docfreq = "docfreq") |>
    mutate(N = sum(.data$Norm.frequency, na.rm = T),
           p = .data$Norm.frequency/.data$N)
}
