% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sample_popset.R
\name{clean_efetch_result}
\alias{clean_efetch_result}
\title{Clean up untidy MeSH data in MEDLINE format efetch results
\code{clean_efectch_result} handles PubMed references that contain false carriage returns
in the MeSH terms which creates errors, when parsing MeSH terms.}
\usage{
clean_efetch_result(recs)
}
\arguments{
\item{recs}{a character vector with a MEDLINE format search result}
}
\value{
the input search result as a cleaned up character vector
}
\description{
Clean up untidy MeSH data in MEDLINE format efetch results
\code{clean_efectch_result} handles PubMed references that contain false carriage returns
in the MeSH terms which creates errors, when parsing MeSH terms.
}
\examples{
\dontshow{if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
ref <- c("MH  - Information Storage and Retrieval","      / methods*")
clean_efetch_result(ref)
\dontshow{\}) # examplesIf}
}
