#' Compute the cosine similarity between one or more ALC embeddings and a set of features.
#'
#' @param x a (quanteda) `dem-class` or `fem-class` object.
#' @param pre_trained (numeric) a F x D matrix corresponding to pretrained embeddings.
#' F = number of features and D = embedding dimensions.
#' rownames(pre_trained) = set of features for which there is a pre-trained embedding.
#' @param features (character) features of interest.
#' @param as_list (logical) if FALSE all results are combined into a single data.frame
#' If TRUE, a list of data.frames is returned with one data.frame per feature.
#'
#' @return a `data.frame` or list of data.frames (one for each target)
#' with the following columns:
#' \describe{
#'  \item{`target`}{ (character) rownames of `x`,
#'  the labels of the ALC embeddings.}
#'  \item{`feature`}{(character) feature terms defined in
#'  the `features` argument.}
#'  \item{`value`}{(numeric) cosine similarity between `x`
#'  and feature.}
#'  }
#'
#' @export
#' @rdname cos_sim
#' @keywords cos_sim
#' @examples
#'
#' library(quanteda)
#'
#' # tokenize corpus
#' toks <- tokens(cr_sample_corpus)
#'
#' # build a tokenized corpus of contexts sorrounding a target term
#' immig_toks <- tokens_context(x = toks, pattern = "immigr*", window = 6L)
#'
#' # build document-feature matrix
#' immig_dfm <- dfm(immig_toks)
#'
#' # construct document-embedding-matrix
#' immig_dem <- dem(immig_dfm, pre_trained = cr_glove_subset,
#' transform = TRUE, transform_matrix = cr_transform, verbose = FALSE)
#'
#' # to get group-specific embeddings, average within party
#' immig_wv_party <- dem_group(immig_dem, groups = immig_dem@docvars$party)
#'
#' # compute the cosine similarity between each party's embedding and a specific set of features
#' cos_sim(immig_wv_party, pre_trained = cr_glove_subset,
#' features = c('reform', 'enforcement'), as_list = FALSE)
cos_sim <- function(x, pre_trained, features = NULL, as_list = TRUE){

  # check features are in pre-trained embeddings
  feature_check <- features %in% rownames(pre_trained)
  if(!all(feature_check)) stop('the following features do not appear to have an embedding in the set of pre-trained embeddings provided: ', paste(features[which(!feature_check)], collapse = ', '))

  # compute cosine similarity
  cos_sim <- text2vec::sim2(x, matrix(pre_trained[features,], ncol = ncol(pre_trained), dimnames = list(features)), method = 'cosine', norm = 'l2')

  # convert to dataframe
  result <- reshape2::melt(as.matrix(cos_sim)) %>% setNames(c('target', 'feature', 'value'))

  # if !as_list return a list object with an item for each feature data.frame
  if(as_list) result <- lapply(unique(result$feature), function(i) result[result$feature == i,] %>% dplyr::mutate(feature = as.character(feature))) %>% setNames(unique(result$feature))

  return(result)
}
