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
#' @param x a quanteda (`fcm-class`) feature-co-occurrence-matrix
#' @param pre_trained (numeric) a F x D matrix corresponding to pretrained embeddings.
#' F = number of features and D = embedding dimensions.
#' rownames(pre_trained) = set of features for which there is a pre-trained embedding.
#' @param transform (logical) if TRUE (default) apply the 'a la carte' transformation,
#' if FALSE ouput untransformed averaged embeddings.
#' @param transform_matrix (numeric) a D x D 'a la carte' transformation matrix.
#' D = dimensions of pretrained embeddings.
#' @param verbose (logical) - if TRUE, report the features that had
#' no overlapping (co-occurring) features with the pretrained embeddings provided.
#'
#' @return a `fem-class` object
#' @export
#' @rdname fem
#' @keywords fem
#' @examples
#'
#' library(quanteda)
#'
#' # tokenize corpus
#' toks <- tokens(cr_sample_corpus)
#'
#' # create feature co-occurrence matrix (set tri = FALSE to work with fem)
#' toks_fcm <- fcm(toks, context = "window", window = 6,
#' count = "frequency", tri = FALSE)
#'
#' # compute feature-embedding matrix
#' toks_fem <- fem(toks_fcm, pre_trained = cr_glove_subset,
#' transform = TRUE, transform_matrix = cr_transform, verbose = FALSE)
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
