% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sample_popset.R
\name{sample_popset}
\alias{sample_popset}
\title{Sample a random set of PubMed references via EUtils API}
\usage{
sample_popset(
  sample_size,
  filters = "none",
  year_range,
  max_pmid = 4e+07,
  email,
  seed = NULL
)
}
\arguments{
\item{sample_size}{positive integer value < 100,000 defining the size of references to be retrieved}

\item{filters}{a character vector defining a study filter to restrict the retrieved references. Default is "none" or "RCT".
The option "RCT" uses the latest PubMed "Therapy/Broad"[filter]}

\item{year_range}{a character vector defining the range of publication years to be considered
in the format "YYYY:YYYY" (e.g. "2000:2024")}

\item{max_pmid}{the highest PMID to be considered, defaults to 40000000.}

\item{email}{a valid e-mail address, which will be sent with the API request to identify the API user (required by NLM)}

\item{seed}{an integer value to control the random seed used for sampling}
}
\value{
an object with a list of random PubMed
references in MEDLINE Format. A histogram is outputted showing the distribution of publication years.
}
\description{
\code{sample_popset} generates a random sequence of numbers to be sent as Unique Identifiers (PMIDs)
as a request to the EUtils API provided by NLM.
}
\examples{
\dontshow{if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
sample_popset(sample_size = 10, year_range = "1900:2024")
\dontshow{\}) # examplesIf}
}
