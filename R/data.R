#' GloVe subset
#'
#' A subset of Stanford GloVe embeddings containing the top terms in the sample corpora.
#'
#' @format A matrix with 393 rows and 300 columns:
#' \describe{
#'   \item{row}{each row corresponds to a word}
#'   \item{column}{each column corresponds to a dimension in the embedding space}
#'   ...
#' }
#' @source \url{https://nlp.stanford.edu/projects/glove/}
"glove_subset"


#' Khodak et al's transformation matrix for GloVe 300
#'
#' A square matrix corresponding to the transformation matrix computed by Khodak et al. for 300-dimensional GloVe
#'
#' @format A 300 by 300 matrix.
#' @source \url{https://github.com/NLPrinceton/ALaCarte/tree/master/transform}
"khodakA"

#' Congressional Record sample corpus
#'
#' A (quanteda) corpus containing a sample of the United States Congressional Record
#' (daily transcripts) covering the 111th to 114th Congresses.
#' The raw corpus is first subset to speeches
#' containing the word "immigration". Then 25 docs from each party-session pair
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


#' ANES 2016 sample corpus
#'
#' A (quanteda) corpus containing a sample of the ANES 2016 open-ended
#' responses to the question:
#' "what is the most important problem facing the country?"
#' 300 responses for each ideology - generation pair is sampled.
#' For full data and pre-processing file, see:
#' https://www.dropbox.com/sh/jsyrag7opfo7l7i/AAB1z7tumLuKihGu2-FDmhmKa?dl=0
#'
#' @format A quanteda corpus with 3600 documents and 4 docvars:
#' \describe{
#'   \item{respondent_id}{respondent id (do not correspond to the original ids)}
#'   \item{ideology}{ideology of respondent, Liberal, Moderate, Conservative}
#'   \item{yob}{year of birth}
#'   \item{generation}{generation defined by yob, see pre-processing file}
#'   ...
#' }
#' @source \url{https://electionstudies.org/data-center/2016-time-series-study/}
"anes2016_sample_corpus"
NULL
