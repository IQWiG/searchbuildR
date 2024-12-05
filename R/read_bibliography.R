#' import a ris file as class bibliography
#'
#' @param filename a risfile to import
#' @param return_df if TRUE a dataframe will be returned, if FALSE a list will be returned
#' @param refChars a character vector of a RIS or PubMed format input
#'
#' @return an object of class bibliography from the revtools package
#' @export
#'
#' @examples
#' ris <- c("TY  - JOUR",
#'         "AU  - Kapp",
#'         "TI  - Titles",
#'         "PY  - 2023",
#'         "JOUR  - IQWiG Journal",
#'         "KW  - Systematic Reviews as Topic",
#'         "ER  -")
#'
#'tmp <- tempfile(fileext = ".txt")
#'writeLines(ris, tmp)
#'read_bibliography(tmp)
read_bibliography<- function (filename = NULL, return_df = TRUE,refChars = NULL)
{
  if(!is.null(filename)){
  z <- tryCatch({
    scan(filename, sep = "\t", what = "character", quote = "",
         quiet = TRUE, blank.lines.skip = FALSE)
  }, warning = function(w) {
    stop("file import failed: data type not recognized by read_bibliography",
         call. = FALSE)
  }, error = function(e) {
    stop("file import failed: data type not recognized by read_bibliography",
         call. = FALSE)
  })
  Encoding(z) <- "UTF-8"
  } else if(!is.null(refChars)){
    z <- refChars
  } else{
    stop("Please provide a valid file path 'filename' or character vector 'refChars' to be converted to a bibliography.")
  }
  nrows <- min(c(200, length(z)))
  zsub <- z[seq_len(nrows)]
  tag_type <- "ris"
  z_dframe <- prep_ris(z, detect_delimiter(zsub))
  if (any(z_dframe$ris == "PMID")) {
    result <- read_medline(z_dframe)
  }
  else {
    result <- read_ris(z_dframe, tag_type)
  }
  if (return_df) {
    result <- as.data.frame(result)
  }

  return(result)
}
