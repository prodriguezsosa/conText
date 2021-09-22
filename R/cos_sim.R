#' Given a set of embeddings and a set of features, calculate cosine similarities.
#'
#' @param x a [dem-class] or [fem-class] object
#' @param pre_trained a F x D matrix of numeric values corresponding to pretrained embeddings
#' F = number of features and D = embedding dimensions.
#' rownames(pre_trained) = set of features for which there is a pre-trained embedding
#' @param features (character) features of interest
#'
#' @return a `data.frame` with following columns:
#'  \item{`target`}{ (character) vector with the rownames of the dfm,
#'  either defining the groups or the target terms}.
#'  \item{`feature`}{(character) vector of feature terms, one
#'  instance for each target.}
#'  \item{`value`}{(numeric) cosine similarity between target
#'  and feature.}
#'
#' @export
#' @rdname cos_sim
#' @keywords cos_sim
#' @examples
#'
#' library(quanteda)
#'
#' # build corpus of contexts around immigration
#' immig_corpus <- corpus_context(x = cr_sample_corpus,
#' pattern = "immigration", window = 6L, verbose = TRUE)
#'
#' # tokenize text
#' immig_toks <- tokens(immig_corpus)
#'
#' # construct document-feature-matrix
#' immig_dfm <- dfm(immig_toks)
#'
#' # construct document-embedding-matrix
#' immig_dem <- dem(immig_dfm, pre_trained = glove_subset,
#' transform = TRUE, transform_matrix = khodakA, verbose = FALSE)
#'
#' # group document-embedding-matrix
#' immig_dem_party <- dem_group(immig_dem,
#' groups = immig_dem@docvars$party)
#'
#' # find nearest neighbors
#' cos_sim(x = immig_dem_party,
#' pre_trained = glove_subset, features = c('reform', 'enforce'))
cos_sim <- function(x, pre_trained, features = NULL){

  # check features are in pre-trained embeddings
  feature_check <- features %in% rownames(pre_trained)
  if(!all(feature_check)) stop('the following features do not appear to have an embedding in the set of pre-trained embeddings provided: ', paste(features[which(!feature_check)], collapse = ', '))

  # compute cosine similarity
  cos_sim <- text2vec::sim2(x, matrix(pre_trained[features,], ncol = ncol(pre_trained), dimnames = list(features)), method = 'cosine', norm = 'l2')

  # convert to dataframe
  result <- reshape2::melt(as.matrix(cos_sim)) %>% setNames(c('target', 'feature', 'value'))

  return(result)
}
