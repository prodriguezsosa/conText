#' Computes the ratio of cosine similarities for two embeddings over
#' the union of their respective top N nearest neighbors.
#'
#' @param x a [dem-class] or [fem-class] object or in general a matrix of embeddings
#' @param N number of nearest neighbors to consider
#' @param numerator (character) defines which group is the nuemerator in the ratio
#' Note: if nearest neighbors overlap, the resulting number of features
#' will be fewer than 2*N.
#' @param candidates character vector delimiting the features to consider as nearest neighbor candidates
#' @param pre_trained a F x D matrix of numeric values corresponding to pretrained embeddings
#' F = number of features and D = embedding dimensions.
#' rownames(pre_trained) = set of features for which there is a pre-trained embedding
#' @param verbose provide information on which group is the numerator
#'
#' @return a `data.frame` with following columns:
#'  \item{`feature`}{(character) vector of features from the candidate set,
#'  one instance for each target.}
#'  \item{`value`}{(numeric) ratio of cosine similarities}.
#'
#' @export
#' @rdname nns_ratio
#' @keywords nns_ratio
#' @examples
#'
#' library(quanteda)
#'
#' # tokenize corpus
#' cr_toks <- tokens(cr_sample_corpus)
#'
#' # get tokens around immigration
#' immig_toks <- tokens_context(x = cr_toks,
#' pattern = "immigration", window = 6L, hard_cut = FALSE, verbose = TRUE)
#'
#' # construct document-feature-matrix
#' immig_dfm <- dfm(immig_toks)
#'
#' # construct document-embedding-matrix
#' immig_dem <- dem(immig_dfm,
#' pre_trained =
#' cr_glove_subset,
#' transform = TRUE,
#' transform_matrix = cr_transform,
#' verbose = FALSE)
#'
#' # group document-embedding-matrix
#' immig_dem_party <- dem_group(immig_dem, groups = immig_dem@docvars$party)
#'
#' # find nearest neighbors
#' nns_ratio(x = immig_dem_party, N = 10,
#' numerator = "R",
#' candidates = character(0),
#' pre_trained = cr_glove_subset,
#' verbose = TRUE)
nns_ratio <- function(x, N = 10, numerator = NULL, candidates = character(0), pre_trained, verbose = TRUE){

  # check
  if(nrow(x)!=2) stop('nns_ratio can only be applied to a pair of embeddings i.e. nrow(x) must equal 2')

  # re-arrange if numerator is defined
  if(!is.null(numerator) && rownames(x)[1]!=numerator) x <- x[c(rownames(x)[2],rownames(x)[1]),]

  # subset candidates to features present in pre-trained embeddings provided
  if(length(candidates) > 0) candidates <- intersect(candidates, rownames(pre_trained))

  # get nns
  nnsdf1 <- nns(x = x[1,], N = Inf, candidates = candidates, pre_trained = pre_trained, as_list = FALSE)
  nnsdf2 <- nns(x = x[2,], N = Inf, candidates = candidates, pre_trained = pre_trained, as_list = FALSE)

  # get union of top N nns (if N is NULL, use all features)
  if(is.null(N)){union_nns <- union(nnsdf1$feature, nnsdf2$feature)}else{union_nns <- union(nnsdf1$feature[1:N], nnsdf2$feature[1:N])}

  # compute ratio for union_nns
  nnsdf1 <- nnsdf1 %>% dplyr::filter(feature %in% union_nns) %>% dplyr::arrange(feature)
  nnsdf2 <- nnsdf2 %>% dplyr::filter(feature %in% union_nns) %>% dplyr::arrange(feature)
  result <- data.frame(feature = nnsdf1$feature, value = nnsdf1$value/nnsdf2$value) %>% dplyr::arrange(-value)

  # add an attribute specifying which group is the numerator and communicated this to user
  attr(result, "numerator") <- rownames(x)[1]
  if(verbose) cat("NOTE: values refer to the ratio", paste0(rownames(x)[1], "/", rownames(x)[2], "."))

  return(result)
}
