% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{calculate_z_scores}
\alias{calculate_z_scores}
\title{Calculate the z-scores as a binomial distribution}
\usage{
calculate_z_scores(testset, popset_norms, key_testset = "MeSH", key_popset)
}
\arguments{
\item{testset}{a data frame containing a quanteda.textstats document feature matrix}

\item{popset_norms}{a data frame containing a quanteda.textstats document feature matrix which is to be assumed the basic population}

\item{key_testset}{the column name containing the terms which should be analyzed}

\item{key_popset}{the corresponding column name in the popset_norms}
}
\value{
a data frame containing the z-scores
}
\description{
Calculate the z-scores as a binomial distribution
}
\examples{
\dontrun{
calculate_z_scores(testset[["freetext"]],
popset[["freetext"]],
key_testset = "feature",
key_popset = "feature")
}
}
