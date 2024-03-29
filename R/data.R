#' PubMed MeSH Headings 2022
#'
#' A comprehensive list of the MeSH headings as data.frame
#'
#'
#' @format ## `MeSH_Dictionary`
#' A data frame with 3094 rows and 1 column:
#' \describe{
#'   \item{MeSH}{MeSH Heading}
#' }
#' @source <https://www.nlm.nih.gov/databases/download/mesh.html>
"MeSH_Dictionary"

#' PubMed random population set
#'
#' A random sample of 19383 PubMed references drawn from random PMIDs (range 1000 - 36000000),
#' search date: April 1st 2022. Source: National Library of Medicine.
#'
#'
#' @format ## `popset`
#' A list of 3 data frames
#' \describe{
#'   freetext  :Classes ‘frequency’, ‘textstat’ and 'data.frame':	66014 obs. of  6 variables:
#'   \describe{
#'     \item{feature}{character, single word token/term}
#'     \item{Norm.frequency}{numeric, absolute frequency of each token/term in population set}
#'     \item{Norm.rank}{numeric, rank of token/term in population set}
#'     \item{Norm.docfreq}{numeric, absolute document frequency of each token/term in population set}
#'     \item{N}{numeric, total frequency of all token/terms in population set}
#'     \item{p}{p-value of binomial test of Norm.frequency}
#'     }
#'   MeSH.Terms:'data.frame':	30194 obs. of  5 variables:
#'   \describe{
#'     \item{MeSH}{character, MeSH heading title style case}
#'     \item{Norm.docfreq}{numeric, absolute document frequency of each MeSH heading in population set}
#'     \item{Norm.frequency}{numeric, absolute frequency of each MeSH heading in population set}
#'     \item{N}{numeric, total frequency of all MeSH headings in population set}
#'     \item{p}{numeric, p-value of binomial test of Norm.frequency}
#'     }
#'   qualifier :'data.frame':	76 obs. of  5 variables:
#'   \describe{
#'     \item{qualifier}{character, MeSH qualifier all lower case}
#'     \item{Norm.docfreq}{numeric, absolute document frequency of each MeSH qualifier in population set}
#'     \item{Norm.frequency}{numeric, absolute frequency of each MeSH qualifier in population set}
#'     \item{N}{numeric, total frequency of all MeSH qualifiers in population set}
#'     \item{p}{numeric, p-value of binomial test of Norm.frequency}
#'     }
#' }
#' @source <https://www.pubmed.com>
"popset"

#' PubMed Qualifier 2022
#'
#' A comprehensive list of the MeSH qualifier as data.frame
#'
#'
#' @format ## `Qualifier_Dictionary`
#' A data frame with 76 rows and 1 column:
#' \describe{
#'   \item{qualifier}{MeSH qualifier}
#' }
#' @source <https://www.nlm.nih.gov/databases/download/mesh.html>
"Qualifier_Dictionary"

#' Testset example
#'
#' example result of function "z_scores"
#'
#'
#' @format ## `testset`
#' a list of 4 data frames
#'  \describe{
#'   freetext  S3: ‘frequency’, ‘textstat’ and 'data.frame':	172 rows of  16 columns:
#'   \describe{
#'     \item{feature}{character, single word token/term}
#'     \item{frequency}{double, observed frequency of the single word token/term}
#'     \item{group}{"all"}
#'     \item{n}{sum of all terms included in analysis}
#'     \item{project}{name of the import file}
#'     \item{rank}{integer, rank of the term according to observed frequency}
#'     \item{Norm.frequency}{numeric, absolute frequency of each token/term in population set}
#'     \item{Norm.rank}{numeric, rank of token/term in population set}
#'     \item{Norm.docfreq}{numeric, absolute document frequency of each token/term in population set}
#'     \item{N}{numeric, total frequency of all token/terms in population set}
#'     \item{p}{estimated proportional frequency based on Norm.frequency}
#'     \item{E}{expected value}
#'     \item{var}{estimated variance}
#'     \item{z}{z-score of bionomial test}
#'     \item{approx_criteria}{}
#'     }
#'   MeSH.Terms:S3:tbl_df, tbl, data.frame :	8 rows and 13 columns:
#'   \describe{
#'     \item{MeSH}{character, MeSH heading title style case}
#'     \item{frequency}{integer, observed frequency of MeSH term}
#'     \item{docfreq}{integer, observed frequency of the MeSH term}
#'     \item{project}{name of the import file}
#'     \item{n}{sum of all MeSH-terms included in analysis}
#'     \item{Norm.docfreq}{numeric, absolute document frequency of each MeSH heading in population set}
#'     \item{Norm.frequency}{numeric, absolute frequency of each MeSH heading in population set}
#'     \item{N}{numeric, total frequency of all MeSH headings in population set}
#'     \item{p}{numeric, p-value of binomial test of Norm.frequency}
#'     \item{E}{expected value}
#'     \item{var}{estimated variance}
#'     \item{z}{z-score of bionomial test}
#'     \item{approx_criteria}{}
#'          }
#'   qualifier :S3:tbl_df, tbl, data.frame :	4 rows and 13 columns:
#'   \describe{
#'     \item{qualifier}{character, MeSH qualifier all lower case}
#'     \item{frequency}{integer, observed frequency of MeSH term}
#'     \item{docfreq}{integer, observed frequency of the MeSH term}
#'     \item{project}{name of the import file}
#'     \item{n}{sum of all MeSH-terms included in analysis}
#'     \item{Norm.docfreq}{numeric, absolute document frequency of each MeSH heading in population set}
#'     \item{Norm.frequency}{numeric, absolute frequency of each MeSH heading in population set}
#'     \item{N}{numeric, total frequency of all MeSH headings in population set}
#'     \item{p}{numeric, p-value of binomial test of Norm.frequency}
#'     \item{E}{expected value}
#'     \item{var}{estimated variance}
#'     \item{z}{z-score of bionomial test}
#'     \item{approx_criteria}{}
#'          }
#'    \describe{
#'    all_keywords:S3:tbl_df, tbl, data.frame :	12 rows and 4 columns:
#'     \item{MeSH}{character, MeSH heading title style case}
#'     \item{frequency}{integer, observed frequency of MeSH term}
#'     \item{docfreq}{integer, observed frequency of the MeSH term}
#'     \item{project}{name of the import file}
#'     }
#' \describe{
#'    leftover_keywords: an empty list
#'    }
#' }
"testset"
