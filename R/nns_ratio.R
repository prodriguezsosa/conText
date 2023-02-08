#' Computes the ratio of cosine similarities for two embeddings over
#' the union of their respective top N nearest neighbors.
#'
#' Computes the ratio of cosine similarities between group embeddings and features
#' --that is, for any given feature it first computes the similarity between that feature
#' and each group embedding, and then takes the ratio of these two similarities.
#' This ratio captures how "discriminant" a feature is of a given group.
#' Values larger (smaller) than 1 mean the feature is more (less)
#' discriminant of the group in the numerator (denominator).
#'
#' @param x a (quanteda) `dem-class` or `fem-class` object.
#' @param N (numeric) number of nearest neighbors to return. Nearest neighbors
#' consist of the union of the top N nearest neighbors of the embeddings in `x`.
#' If these overlap, then resulting N will be smaller than 2*N.
#' @param numerator (character) defines which group is the nuemerator in the ratio
#' @param candidates (character) vector of features to consider as candidates to be nearest neighbor
#' You may for example want to only consider features that meet a certian count threshold
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
#' @param verbose report which group is the numerator and which group is the denominator.
#' @param show_language (logical) if TRUE print out message with language used for stemming.
#'
#' @return a `data.frame` with following columns:
#' \describe{
#'  \item{`feature`}{(character) features in `candidates`
#'  (or all features if `candidates` not defined), one instance for each embedding in `x`.}
#'  \item{`value`}{(numeric) ratio of cosine similarities.}
#'  }
#'
#' @export
#' @rdname nns_ratio
#' @keywords nns_ratio
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
#' # compute the cosine similarity between each party's
#' # embedding and a specific set of features
#' nns_ratio(x = immig_wv_party, N = 10, numerator = "R",
#' candidates = immig_wv_party@features,
#' pre_trained = cr_glove_subset, verbose = FALSE)
#'
#' # with stemming
#' nns_ratio(x = immig_wv_party, N = 10, numerator = "R",
#' candidates = immig_wv_party@features,
#' pre_trained = cr_glove_subset, stem = TRUE, verbose = FALSE)
nns_ratio <- function(x, N = 10, numerator = NULL, candidates = character(0), pre_trained, stem = FALSE, language = 'porter', verbose = TRUE, show_language = TRUE){

  # check
  if(nrow(x)!=2) stop('nns_ratio can only be applied to a pair of embeddings i.e. nrow(x) must equal 2')

  # re-arrange if numerator is defined
  if(!is.null(numerator) && rownames(x)[1]!=numerator) x <- x[c(rownames(x)[2],rownames(x)[1]),]

  # subset candidates to features present in pre-trained embeddings provided
  if(length(candidates) > 0) candidates <- intersect(candidates, rownames(pre_trained))

  # get nns
  nnsdf1 <- nns(x = x[1,], N = Inf, candidates = candidates, pre_trained = pre_trained, stem = stem, language = language, as_list = FALSE, show_language = show_language)
  nnsdf2 <- nns(x = x[2,], N = Inf, candidates = candidates, pre_trained = pre_trained, stem = stem, language = language, as_list = FALSE, show_language = FALSE)

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
