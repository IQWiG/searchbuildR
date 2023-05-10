#' Title
#'
#' @param risfile a ri-format file to be imported
#' @param dev_set should a random development set (2/3 imported references) be created
#' @param project_name should the project name be guessed from the file name
#' @param seed option to set a random seed, if dev_set = TRUE
#'
#' @returns a list in the format
#'
#' testset
#' ...$freetext
#' ...$MeSH.terms / ...$all_keywords / ...$MeSH_with_Qualifier
#' ...$PMIDS / ...$testset | ...$devevolpment_set /...$validation_set
#' ...$reference.list
#' ...$text_corpus
#' if (dev_set = TRUE) ...$seed
#'
#'#' @examples
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
#' create_testset(tmp)
#'
#' @export
create_testset <- function(risfile, dev_set = FALSE, project_name = TRUE, seed = NULL) {
 all_ref <- read_bibliography(risfile, return_df = F)

  if (dev_set){
    #### create a development set of two thirds of the testset
    raw_ris <- raw_ris_data(risfile)

    if(is.null(seed)){
      seed <- sample.int(.Machine$integer.max, 1L)
    }
    random_references <- with_seed(seed, sort(sample(length(all_ref),
                                                     round((length(all_ref)*2)/3))))

    validation_references <- setdiff(seq(all_ref), random_references)
    testset_ref <- all_ref[random_references]
    validation_set <- TRUE
    # add raw ris files
    developmentset_raw <- raw_ris[random_references]
    validationset_raw <- raw_ris[validation_references]


  }else{
    testset_ref <- all_ref
    random_references <- NULL
    validation_set <- FALSE
  }

  pmids <- return_pmids(all_ref,
                        random_references = random_references,
                        validation_set = validation_set)

  testset_corpus <- create_corpus(testset_ref)
  testset_df <- prepare_freq_table(testset_corpus)
  testset_MeSH <- prepare_MeSH_table(testset_ref)


  # attach project name per row
  if (project_name){
    project_names <- gregexpr("(?<=/|\\\\)(\\w|-)+(?=\\.)", risfile, perl = T)
    project_names <- regmatches(risfile,project_names)
    testset_df$project <- unlist(project_names)
    testset_MeSH[["all_keywords"]]$project <- unlist(project_names)
  }

  if(dev_set){
    testset <- list ("freetext" = testset_df,
                     "MeSH.Terms" = testset_MeSH,
                     "PMIDS" = pmids,
                     "reference.list" = all_ref, # return list of all references not only development set
                     "development_set" = developmentset_raw,
                     "validation_set" = validationset_raw,
                     "text_corpus" = testset_corpus,
                     "seed" = seed) #

  }else{
    testset <- list ("freetext" = testset_df,
                     "MeSH.Terms" = testset_MeSH,
                     "PMIDS" = pmids,
                     "reference.list" = all_ref,
                     "text_corpus" = testset_corpus)
  }
  return(testset)
}
