#' Create an feature-embedding matrix
#'
#' Given a featureco-occurrence matrix for each feature,
#' multiply its feature counts (columns) with their
#' corresponding pre-trained embeddings and average
#' (usually referred to as averaged or additive embeddings).
#' If specified and a transformation matrix is provided,
#' multiply the feature embeddings by the transformation matrix
#' to obtain the corresponding `a la carte` embeddings.
#' (see eq 2: https://arxiv.org/pdf/1805.05388.pdf)
#'
#' @param x a `fcm-class` object
#' @param pre_trained a F x D matrix of numeric values corresponding to pretrained embeddings
#' F = number of features and D = embedding dimensions.
#' rownames(pre_trained) = set of features for which there is a pre-trained embedding
#' @param transform (logical) if TRUE (default) apply the `a la carte` transformation, if FALSE ouput untransformed averaged embedding
#' @param transform_matrix a D x D matrix of numeric values corresponding to the `a la carte`` transformation matrix
#' @param verbose logical - report the documents/features that had no overlap with the provided pre-trained embeddings
#'
#' @return a `fem-class` object
#' @export
#' @rdname fem
#' @keywords fem
#' @examples
#'
#' library(quanteda)
#'
#' ## embed all features
#'
#' #' # tokenize text
#' cr_toks <- tokens(cr_sample_corpus)
#'
#' # construct document-feature-matrix
#' cr_fcm <- fcm(cr_toks, context = "window", window = 6,
#' count = "weighted", weights = 1 / (1:6), tri = FALSE)
#' # set tri = FALSE to work with fem
#'
#' # construct feature-embedding-matrix
#' cr_fem <- fem(cr_fcm, pre_trained = cr_glove_subset,
#' transform = TRUE, transform_matrix = cr_transform, verbose = FALSE)
#' dim(cr_fem)
#'
#' ## embed specific target features
#'
#' feats <- c('immigration', 'immigrants')
#'
#' # create feature co-occurrence matrix for features of interest
#' cr_fcm <- tokens_select(cr_toks, pattern = feats, padding = TRUE) %>%
#' # set padding = TRUE here to avoid making non-adjecnt words adjacent
#' fcm(context = "window", window = 6,
#' count = "weighted", weights = 1 / (1:6), tri = FALSE)
#' # set tri = FALSE to work with fem and dem
#'
#' # construct feature-embedding-matrix
#' cr_fem <- fem(cr_fcm, pre_trained = cr_glove_subset,
#' transform = TRUE, transform_matrix = cr_transform, verbose = FALSE)
#' dim(cr_fem)
fem <- function(x, pre_trained, transform = TRUE, transform_matrix, verbose = TRUE){

  # checks
  if(transform){
    if(!exists('transform_matrix'))stop("when transform = TRUE, a transform_matrix must be provided")
    if(!(ncol(pre_trained) == nrow(transform_matrix)))stop("transform_matrix must be a square matrix with dimensions ncol(pre_trained) x ncol(pre_trained)")
  }

  # subset to features overlapping with pre-trained embeddings
  overlapping_features <- intersect(colnames(x), rownames(pre_trained)) # identify overlapping context features
  if(length(overlapping_features) == 0)stop("no overlapping features with the pre-trained embeddings.")
  feature_matrix <- x[,overlapping_features] # subset to overlapping context features
  pre_trained <- pre_trained[colnames(feature_matrix),] # subset and re-order to match feature_matrix
  N <- Matrix::rowSums(feature_matrix) # number of context features

  # identify those contexts that have no features in the pre-trained embeddings
  missing = which(N==0)
  included = which(N>0)
  if(verbose) if(length(missing) >0) cat('the following features could not be embedded due lack of overlap with pre-trained embeddings provided: \n', rownames(feature_matrix)[missing], '\n')

  # remove contexts with no context features in the pre-trained embeddings to avoid NaNs (i.e. dividing by 0)
  feature_matrix <- feature_matrix[N!=0,]
  N <- Matrix::rowSums(feature_matrix)

  # context embeddings by instance
  context_embedding <- Matrix::crossprod(t(feature_matrix), pre_trained) # crossprod faster than %*%

  # adjust by number of context features
  # for fcm: (1/|Cf|)*(1/|c|) ~ 1/N
  result <- sweep(context_embedding, 1, 1/N, '*')

  # apply transformation
  if(transform) result <- transform_matrix %*% t(result) %>% t

  # create `fem` class object
  result <- build_fem(Class = 'fem',
                     x_fem = result,
                     features = overlapping_features,
                     counts = x@meta$object$margin[overlapping_features],
                     Dimnames = list(
                       rows = rownames(x)[included],
                       columns = NULL))

  return(result)
}
