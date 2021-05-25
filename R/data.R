#' Sample GloVe
#'
#' A subset of Stanford GloVe embeddings.
#'
#' @format A double object with 70 rows and 300 columns:
#' \describe{
#'   \item{row}{each row corresponds to a word}
#'   \item{column}{each column corresponds to a dimension in the embedding space}
#'   ...
#' }
#' @source \url{https://nlp.stanford.edu/projects/glove/}
"sample_glove"


#' Khodak et al's transformation matrix for GloVe
#'
#' A square matrix corresponding to the transformation matrix computed by Khodak et al.
#'
#' @format A 300 by 300 matrix.
#' @source \url{https://github.com/NLPrinceton/ALaCarte}
"khodakA"


#' Sample corpus
#'
#' A sample from the congressional record corpus
#' 1000 speeches from each of the two major parties, sessions 111th - 114th
#'
#'
#' @format A data frame with 2000 rows and 2 variables:
#' \describe{
#'   \item{speech}{words spoken by a given representative}
#'   \item{party}{party membership of said representative}
#'   ...
#' }
#' @source \url{https://data.stanford.edu/congress_text}
"sample_corpus"

