#' Given a set of embeddings and a set of candidate neighbors, find the top N nearest
#' neighbors.
#'
#' @param x a [dem-class] or [fem-class] object
#' @param N (numeric) number of nearest neighbors to return
#' @param candidates (character) vector of features to consider as candidates to be nearest neighbor
#' @param pre_trained a F x D matrix of numeric values corresponding to pretrained embeddings
#' F = number of features and D = embedding dimensions.
#' rownames(pre_trained) = set of features for which there is a pre-trained embedding
#'
#' @return a `data.frame` with following columns:
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
#' # tokenize text
#' anes2016_toks <- tokens(anes2016_sample_corpus)
#'
#' # construct document-feature-matrix
#' anes2016_dfm <- dfm(anes2016_toks)
#'
#' # construct document-embedding-matrix
#' anes2016_dem <- dem(anes2016_dfm, pre_trained = glove_subset,
#' transform = TRUE, transform_matrix = khodakA, verbose = TRUE)
#'
#' # group document-embedding-matrix
#' anes2016_dem_ideology <- dem_group(anes2016_dem,
#' groups = anes2016_dem@docvars$ideology)
#'
#' # find nearest neighbors
#' nns(x = anes2016_dem_ideology,
#' pre_trained = glove_subset, candidates = NULL, N = 10)
nns <- function(x, N = 10, candidates = character(0), pre_trained){

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
    dplyr::ungroup() %>%
    dplyr::select('target', 'feature', 'rank', 'value')

  # if rowname is missing, replace with NA
  if(is.null(rownames(x))) result$target <- NA

  return(result)
}
