% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{popset}
\alias{popset}
\title{PubMed random population set}
\format{
\subsection{\code{popset}}{

A list of 3 data frames
\describe{
freetext  :Classes ‘frequency’, ‘textstat’ and 'data.frame':	66014 obs. of  6 variables:
\describe{
\item{feature}{character, single word token/term}
\item{Norm.frequency}{numeric, absolute frequency of each token/term in population set}
\item{Norm.rank}{numeric, rank of token/term in population set}
\item{Norm.docfreq}{numeric, absolute document frequency of each token/term in population set}
\item{N}{numeric, total frequency of all token/terms in population set}
\item{p}{p-value of binomial test of Norm.frequency}
}
MeSH.Terms:'data.frame':	30194 obs. of  5 variables:
\describe{
\item{MeSH}{character, MeSH heading title style case}
\item{Norm.docfreq}{numeric, absolute document frequency of each MeSH heading in population set}
\item{Norm.frequency}{numeric, absolute frequency of each MeSH heading in population set}
\item{N}{numeric, total frequency of all MeSH headings in population set}
\item{p}{numeric, p-value of binomial test of Norm.frequency}
}
qualifier :'data.frame':	76 obs. of  5 variables:
\describe{
\item{qualifier}{character, MeSH qualifier all lower case}
\item{Norm.docfreq}{numeric, absolute document frequency of each MeSH qualifier in population set}
\item{Norm.frequency}{numeric, absolute frequency of each MeSH qualifier in population set}
\item{N}{numeric, total frequency of all MeSH qualifiers in population set}
\item{p}{numeric, p-value of binomial test of Norm.frequency}
}
}
}
}
\source{
\url{https://www.pubmed.com}
}
\usage{
popset
}
\description{
A random sample of 19383 PubMed references drawn from random PMIDs (range 1000 - 36000000),
search date: April 1st 2022. Source: National Library of Medicine.
}
\keyword{datasets}
