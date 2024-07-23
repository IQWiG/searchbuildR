create_MeSH_norm_frequencies <- function (reference_set, update_mesh = FALSE, mesh_xml = NULL) {
  if(update_mesh){
    newMeSH <- update_mesh(mesh_xml)
    currentMeSHDictionary <- newMeSH$MeSHTerm
  } else{
      # currentMeSHDictionary <- searchbuildR::MeSH_Dictionary
    }

  population_MeSH <- prepare_MeSH_table(reference_set)

  population_headings <- searchbuildR::MeSH_Dictionary |>
    left_join(population_MeSH[["all_keywords"]], by = "MeSH") |>
    rename(Norm.frequency = "frequency",
           Norm.docfreq = "docfreq") |>
    mutate(N = sum(.data$Norm.frequency, na.rm = T),
           p = .data$Norm.frequency/.data$N)

  population_qualifier <- searchbuildR::Qualifier_Dictionary |>
    left_join(population_MeSH[["all_keywords"]], by = c("qualifier" = "MeSH")) |>
    rename(Norm.frequency = "frequency",
           Norm.docfreq = "docfreq") |>
    mutate(N = sum(.data$Norm.frequency, na.rm = T),
           p = .data$Norm.frequency/.data$N)

  result <- list("headings" = population_headings, "qualifier" = population_qualifier)

  return(result)
}

update_mesh <- function(mesh_xml){
  MeSHXML <-  xml2::read_xml(mesh_xml) |>
    xml2::as_list()
  MeSHTibble <- tibble::tibble(mesh = MeSHXML$DescriptorRecordSet)

  MeSHTerms <- MeSHTibble |>
    tidyr::unnest_wider(mesh)|>
    dplyr::select("DescriptorUI", "DescriptorName") |>
    dplyr::mutate(across(everything(), unlist)) |>
    dplyr::rename(MeSH = DescriptorName)

  MeSHTree <- MeSHTibble |>
    tidyr::unnest_wider(mesh)|>
    dplyr::select("DescriptorUI", "TreeNumberList") |>
    dplyr::mutate("DescriptorUI" = unlist(DescriptorUI)) |>
    tidyr::unnest_longer("TreeNumberList", indices_include = FALSE) |>
    tidyr::hoist(TreeNumberList,
                 TreeNumber = list(1))
  result <- list("MeSHTerms" = MeSHTerms,
                 "MeSHTree" = MeSHTree)

  return(result)
}
