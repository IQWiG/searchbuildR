% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{testset}
\alias{testset}
\title{Testset example}
\format{
\subsection{\code{testset}}{

a list of 5 objects
\describe{
freetext  S3: ‘frequency’, ‘textstat’ and 'data.frame':	172 rows of  16 columns:
\describe{
\item{feature}{character, single word token/term}
\item{frequency}{double, observed frequency of the single word token/term}
\item{group}{"all"}
\item{n}{sum of all terms included in analysis}
\item{project}{name of the import file}
\item{rank}{integer, rank of the term according to observed frequency}
\item{Norm.frequency}{numeric, absolute frequency of each token/term in population set}
\item{Norm.rank}{numeric, rank of token/term in population set}
\item{Norm.docfreq}{numeric, absolute document frequency of each token/term in population set}
\item{N}{numeric, total frequency of all token/terms in population set}
\item{p}{estimated proportional frequency based on Norm.frequency}
\item{E}{expected value}
\item{var}{estimated variance}
\item{z}{z-score of bionomial test}
\item{approx_criteria}{}
}
MeSH.Terms:S3:tbl_df, tbl, data.frame :	8 rows and 13 columns:
\describe{
\item{MeSH}{character, MeSH heading title style case}
\item{frequency}{integer, observed frequency of MeSH term}
\item{docfreq}{integer, observed frequency of the MeSH term}
\item{project}{name of the import file}
\item{n}{sum of all MeSH-terms included in analysis}
\item{Norm.docfreq}{numeric, absolute document frequency of each MeSH heading in population set}
\item{Norm.frequency}{numeric, absolute frequency of each MeSH heading in population set}
\item{N}{numeric, total frequency of all MeSH headings in population set}
\item{p}{numeric, p-value of binomial test of Norm.frequency}
\item{E}{expected value}
\item{var}{estimated variance}
\item{z}{z-score of bionomial test}
\item{approx_criteria}{}
}
PMIDS : a list with one character vector
\describe{
\item{testset}{character, 3 PMIDS}
}
reference.list : an S3 object of class bibliography
\describe{
\item{Hausner_2015_JCE}{list, reference entry}
\item{Hausner_2016_JCE}{list, reference entry}
\item{Hausner_2022_JCE}{list, reference entry}
}
test_corpus : a corpus object with 3 entries
\describe{
\item{Hausner_2015}{corpus object}
\item{Hausner_2016}{corpus object}
\item{Hausner_2022}{corpus object}
}
}
}
}
\usage{
testset
}
\description{
example result of function "create_testset"
}
\keyword{datasets}
