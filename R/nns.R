#' Given a set of embeddings and a set of candidate neighbors, find the top N nearest
#' neighbors.
#'
#' @param x a `dem-class` or `fem-class` object.
#' @param N (numeric) number of nearest neighbors to return
#' @param candidates (character) vector of features to consider as candidates to be nearest neighbor
#' You may for example want to only consider features that meet a certain count threshold
#' or exclude stop words etc. To do so you can simply identify the set of features you
#' want to consider and supply these as a character vector in the `candidates` argument.
#' @param pre_trained (numeric) a F x D matrix corresponding to pretrained embeddings.
#' F = number of features and D = embedding dimensions.
#' rownames(pre_trained) = set of features for which there is a pre-trained embedding.
#' @param stem (logical) - whether to stem candidates when evaluating nns. Default is FALSE.
#' If TRUE, candidate stems are ranked by their average cosine similarity to the target.
#' We recommend you remove misspelled words from candidate set `candidates` as these can
#' significantly influence the average.
#' @param as_list (logical) if FALSE all results are combined into a single data.frame
#' If TRUE, a list of data.frames is returned with one data.frame per group.
#'
#' @return a `data.frame` or list of data.frames (one for each target)
#' with the following columns:
#' \describe{
#'  \item{`target`}{ (character) rownames of `x`,
#'  the labels of the ALC embeddings. `NA` if `is.null(rownames(x))`.}
#'  \item{`feature`}{(character) features identified as nearest neighbors.}
#'  \item{`rank`}{(character) rank of feature in terms of similarity with `x`.}
#'  \item{`value`}{(numeric) cosine similarity between `x` and feature.}
#'  }
#'
#' @export
#' @rdname nns
#' @keywords nns
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
#' # find nearest neighbors by party
#' # setting as_list = FALSE combines each group's
#' # results into a single tibble (useful for joint plotting)
#' immig_nns <- nns(immig_wv_party, pre_trained = cr_glove_subset,
#' N = 5, candidates = immig_wv_party@features, stem = TRUE, as_list = TRUE)
nns <- function(x, N = 10, candidates = character(0), pre_trained, stem = FALSE, as_list = TRUE){

  # for single numeric vectors
  if(is.null(dim(x)) && length(x) == dim(pre_trained)[2]) x <- matrix(x, nrow = 1)

  # subset candidates to features present in pre-trained embeddings provided
  if(length(candidates) > 0) candidates <- intersect(candidates, rownames(pre_trained))

  ## compute cosine similarity
  # if no candidates are provided, use full set of features
  if(length(candidates) == 0) cos_sim <- text2vec::sim2(x = as.matrix(pre_trained), y = as.matrix(x), method = "cosine", norm = "l2") %>% data.frame()

  # if candidates are provided, subset pre-trained
  if(length(candidates) > 0) cos_sim <- text2vec::sim2(x = as.matrix(pre_trained)[candidates,], y = as.matrix(x), method = "cosine", norm = "l2") %>% data.frame()

  # name columns
  if(!is.null(rownames(x))) colnames(cos_sim) <- rownames(x)
  if(is.null(rownames(x))) colnames(cos_sim) <- 'target'

  # add a column with feature names
  cos_sim <- cos_sim %>% dplyr::mutate(feature = rownames(cos_sim))

  # stemming
  if(stem){
    if (requireNamespace("SnowballC", quietly = TRUE)) cos_sim <- cos_sim %>% dplyr::mutate(feature = SnowballC::wordStem(feature)) %>% dplyr::group_by(feature) %>% dplyr::summarise(dplyr::across(where(is.numeric), mean)) %>% dplyr::ungroup()
    else warning('"SnowballC (>= 0.7.0)" package must be installed to use stemmming option. Will proceed without stemming.', call. = FALSE)
  }

  # reshape data
  result <- reshape2::melt(cos_sim, id.vars = 'feature') %>%
    setNames(c('feature', 'target', 'value')) %>%
    dplyr::group_by(target) %>%
    dplyr::slice_max(order_by = value, n = N) %>%
    dplyr::mutate(rank = 1:dplyr::n()) %>%
    dplyr::arrange(dplyr::desc(value)) %>%
    dplyr::ungroup() %>%
    dplyr::select('target', 'feature', 'rank', 'value')

  # if rowname is missing, replace with NA
  if(is.null(rownames(x))) result$target <- NA

  # if !as_list return a list object with an item for each target data.frame
  if(as_list && !is.null(rownames(x))) result <- lapply(unique(result$target), function(i) result[result$target == i,] %>% dplyr::mutate(target = as.character(target))) %>% setNames(unique(result$target))
  if(as_list && is.null(rownames(x))) message("although as_list = TRUE, will return a single tibble as there is only one word vector")

  return(result)
}
