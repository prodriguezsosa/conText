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
#' @inheritParams SnowballC::wordStem
#' @param as_list (logical) if FALSE all results are combined into a single data.frame
#' If TRUE, a list of data.frames is returned with one data.frame per group.
#' @param show_language (logical) if TRUE print out message with language used for stemming.
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
#' N = 5, candidates = immig_wv_party@features, stem = FALSE, as_list = TRUE)
nns <- function(x, N = 10, candidates = character(0), pre_trained, stem = FALSE, language = 'porter', as_list = TRUE, show_language = TRUE){

  # for single numeric vectors
  if(is.null(dim(x)) && length(x) == dim(pre_trained)[2]) x <- matrix(x, nrow = 1)

  # stem pre_trained embeddings if TRUE
  if(stem){
    if (requireNamespace("SnowballC", quietly = TRUE)) {
      if(show_language) cat('Using', language, 'for stemming. To check available languages run "SnowballC::getStemLanguages()"', '\n')
      pre_trained_feats <- SnowballC::wordStem(rownames(pre_trained))
    } else stop('"SnowballC (>= 0.7.0)" package must be installed to use stemmming option.')
  } else pre_trained_feats <- rownames(pre_trained)

  # rename features in pre_trained (only changes if stem)
  rownames(pre_trained) <- pre_trained_feats

  # if candidates are provided
  if(length(candidates) > 0) {
    if(stem) candidates <- SnowballC::wordStem(candidates) # stem

    # check which, if any candidates, are present
    candidate_check <- candidates %in% pre_trained_feats
    if(!any(candidate_check)) stop('none of canidates appear to have an embedding in the set of pre-trained embeddings provided, please select other candidates', call. = FALSE)
    if(!all(candidate_check)) warning('the following canidates do not appear to have an embedding in the set of pre-trained embeddings provided: ', paste(candidates[which(!candidate_check)], collapse = ', '))
    candidates_present <- candidates[candidate_check]

    # subset pre-trained embeddings to candidates of interest
    pre_trained <- pre_trained[rownames(pre_trained) %in% candidates_present,]
  }

  ## compute cosine similarity
  cos_sim <- text2vec::sim2(x = as.matrix(pre_trained), y = as.matrix(x), method = "cosine", norm = "l2") %>% t() %>% as.data.frame() %>% t()

  # name columns
  if(!is.null(rownames(x))) colnames(cos_sim) <- rownames(x)
  if(is.null(rownames(x))) colnames(cos_sim) <- 'target'

  # add a column with feature names
  cos_sim <- cos_sim %>% as.data.frame() %>% dplyr::mutate(feature = rownames(cos_sim))
  rownames(cos_sim) <- NULL

  # if stem, average over stems
  if(stem) cos_sim <- cos_sim %>% dplyr::group_by(feature) %>% dplyr::summarise(dplyr::across(where(is.numeric), mean)) %>% dplyr::ungroup()

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
