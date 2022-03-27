#' Build a document-embedding matrix
#'
#' Given a document-feature-matrix, for each document,
#' multiply its feature counts (columns) with their
#' corresponding pretrained word embeddings and average
#' (usually referred to as averaged or additive document embeddings).
#' If specified and a transformation matrix is provided,
#' multiply the document embeddings by the transformation matrix
#' to obtain the corresponding `a la carte` document embeddings.
#' (see eq 2: https://arxiv.org/pdf/1805.05388.pdf)
#'
#' @param x a quanteda (`dfm-class`) document-feature-matrix
#' @param pre_trained (numeric) a F x D matrix corresponding to pretrained embeddings.
#' F = number of features and D = embedding dimensions.
#' rownames(pre_trained) = set of features for which there is a pre-trained embedding.
#' @param transform (logical) if TRUE (default) apply the 'a la carte' transformation,
#' if FALSE ouput untransformed averaged embeddings.
#' @param transform_matrix (numeric) a D x D 'a la carte' transformation matrix.
#' D = dimensions of pretrained embeddings.
#' @param verbose (logical) - if TRUE, report the documents that had
#' no overlapping features with the pretrained embeddings provided.
#'
#' @return a N x D (`dem-class`) document-embedding-matrix corresponding to the ALC embeddings for each document.
#' N = number of documents (that could be embedded), D = dimensions of pretrained embeddings. This object
#' inherits the document variables in `x`, the dfm used. These can be accessed calling the attribute: `@docvars`.
#' Note, documents with no overlapping features with the pretrained embeddings provided are automatically
#' dropped. For a list of the documents that were embedded call the attribute: `@Dimnames$docs`.
#'
#' @export
#' @rdname dem
#' @keywords dem
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
#' # construct document-feature-matrix
#' immig_dfm <- dfm(immig_toks)
#'
#' # construct document-embedding-matrix
#' immig_dem <- dem(immig_dfm, pre_trained = cr_glove_subset,
#' transform = TRUE, transform_matrix = cr_transform, verbose = FALSE)
dem <- function(x, pre_trained, transform = TRUE, transform_matrix, verbose = TRUE){

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
  if(verbose) if(length(missing) >0) cat('the following documents could not be embedded due lack of overlap with pre-trained embeddings provided: \n', rownames(feature_matrix)[missing], '\n')

  # remove contexts with no context features in the pre-trained embeddings to avoid NaNs (i.e. dividing by 0)
  feature_matrix <- feature_matrix[N!=0,]
  N <- Matrix::rowSums(feature_matrix)

  # context embeddings by instance
  context_embedding <- Matrix::crossprod(t(feature_matrix), pre_trained) # crossprod faster than %*%

  # adjust by number of context features
  result <- sweep(context_embedding, 1, 1/N, '*')

  # apply alc transformation
  if(transform) result <- transform_matrix %*% t(result) %>% t

  # create `dem` class object
  result <- build_dem(Class = 'dem',
                     x_dem = result,
                     docvars = quanteda::docvars(x)[included,,drop=FALSE],
                     features = overlapping_features,
                     Dimnames = list(
                       docs = as.character(quanteda::docid(x)[included]),
                       columns = NULL))


  return(result)
}
