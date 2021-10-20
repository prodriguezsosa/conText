#' GloVe subset
#'
#' A subset of a GloVe embeddings model trained on the top 5000 features
#' in the Congressional Record Record corpus covering the 111th - 114th Congresses,
#' and limited to speeches by Democrat and Republican representatives.
#'
#' @format A matrix with 500 rows and 300 columns:
#' \describe{
#'   \item{row}{each row corresponds to a word}
#'   \item{column}{each column corresponds to a dimension in the embedding space}
#'   ...
#' }
#' @source \url{https://www.dropbox.com/s/p84wzv8bdmziog8/cr_glove.R?dl=0}
"cr_glove_subset"


#' Transformation matrix
#'
#' A square matrix corresponding to the transformation matrix computed
#' using the cr_glove_subset embeddings and corresponding corpus.
#'
#' @format A 300 by 300 matrix.
#' @source \url{https://www.dropbox.com/s/p84wzv8bdmziog8/cr_glove.R?dl=0}
"cr_transform"

#' Congressional Record sample corpus
#'
#' A (quanteda) corpus containing a sample of the United States Congressional Record
#' (daily transcripts) covering the 111th to 114th Congresses.
#' The raw corpus is first subset to speeches
#' containing the regular expression "immig*". Then 100 docs from each party-gender pair
#' is randomly sampled. For full data and pre-processing file, see:
#' https://www.dropbox.com/sh/jsyrag7opfo7l7i/AAB1z7tumLuKihGu2-FDmhmKa?dl=0
#'
#' @format A quanteda corpus with 200 documents and 3 docvars:
#' \describe{
#'   \item{party}{party of speaker, (D)emocrat or (R)epublican}
#'   \item{gender}{gender of speaker, (F)emale or (M)ale}
#'   \item{session_id}{id of Congress session in which speech was given}
#'   ...
#' }
#' @source \url{https://data.stanford.edu/congress_text}
"cr_sample_corpus"
