#' Given a set of embeddings and a set of candidate neighbors, find the top N nearest
#' neighbors.
#'
#' @param x a [dem-class] or [fem-class] object
#' @param N (numeric) number of nearest neighbors to return
#' @param candidates (character) vector of features to consider as candidates to be nearest neighbor
#' You may for example want to only consider features that meet a certian count threshold
#' or exclude stop words etc. To do so you can simply identify the set of features you
#' want to consider and supply these as a character vector in the `candidates` argument.
#' @param pre_trained a F x D matrix of numeric values corresponding to pretrained embeddings
#' F = number of features and D = embedding dimensions.
#' rownames(pre_trained) = set of features for which there is a pre-trained embedding
#' @param as_list (logical) if FALSE all results are combined into a single data.frame
#' If TRUE, a list of data.frames is returned with one data.frame per target.
#'
#' @return a `data.frame` or list of data.frames (one for each target)
#' with the following columns:
#'  \item{`target`}{ (character) vector with the rownames of the dfm,
#'  either defining the groups or the target terms}.
#'  \item{`feature`}{(character) vector of features from the candidate set,
#'  one instance for each target.}
#'  \item{`value`}{(numeric) cosine similarity between target
#'  and candidate.}
#'
#' @export
#' @rdname nns
#' @keywords nns
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
#' nns(x = immig_dem_party,
#' pre_trained = glove_subset, candidates = NULL, N = 10, as_list = FALSE)
nns <- function(x, N = 10, candidates = character(0), pre_trained, as_list = TRUE){

  # for single numeric vectors
  if(is.null(dim(x)) & length(x) == dim(pre_trained)[2]) x <- matrix(x, nrow = 1)

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
