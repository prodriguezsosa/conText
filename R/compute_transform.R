#' compute transformation matrix A
#'
#' @param x a `fcm`
#' @param pre_trained a `dgTMatrix-class` matrix of word vectors
#' @param weighting 1 = no weighting; log = weight by the log of the frequency count; a numeric = threshold based weighting (= 1 if token count meets threshold, 0 ow); use log for small corpora, numeric threshold for larger corpora
#'
#' @return a `dgTMatrix-class` D x D (D = word vector dimensions)
#' @export
#' @rdname compute_transform
#' @keywords compute_transform
#' @examples
#'
#' library(quanteda)
#'
#' # note, cr_sample_corpus is too small to produce sensical word vectors
#'
#' # tokenize
#' cr_toks <- tokens(cr_sample_corpus)
#'
#' # construct feature-co-occurrence matrix
#' cr_fcm <- fcm(cr_toks, context = "window", window = 6,
#' count = "weighted", weights = 1 / (1:6), tri = FALSE)
#'
#' # you will generally want to estimate a new (corpus-specific)
#' # GloVe model, we will use cr_glove_subset instead
#' # see the Quick Start Guide to see a full example.
#'
#' # estimate transform
#' cr_subset_transform <- compute_transform(x = cr_fcm,
#' pre_trained = cr_glove_subset, weighting = 'log')
compute_transform <- function(x, pre_trained, weighting = 500){

  # compute un-transformed additive embedding
  context_embeddings <- fem(x = x, pre_trained = pre_trained, transform_matrix, transform = FALSE, verbose = FALSE)

  # extract feature frequency from fcm object
  feature_frequency <- x@meta$object$margin
  feature_frequency <- feature_frequency[intersect(names(feature_frequency), attr(context_embeddings, 'features'))]
  if(weighting == 'log') feature_frequency <- feature_frequency[feature_frequency >= 1] # avoig negatives when taking logs

  # apply weighting if given
  if(is.numeric(weighting)) feature_frequency <- feature_frequency[feature_frequency >= weighting]

  # make sure featuers are in the same order
  context_embeddings <- context_embeddings[names(feature_frequency),]
  pre_trained <- pre_trained[names(feature_frequency),]

  # weighting function
  alpha <- Matrix::Matrix(nrow = nrow(context_embeddings), ncol = nrow(context_embeddings), data=0, sparse=T) # initialize weight matrix to be modified
  if(is.numeric(weighting)) diag(alpha) <- 1 # threshold is applied above, hence can simply multiply by 1
  if(weighting == 'log') diag(alpha) <- log(feature_frequency) # weight by log of token count

  # solve for transformation matrix (just a weighted regression)
  # following lm, we use qr decomposition, faster and more stable
  # useful discussion: https://www.theissaclee.com/post/linearqrandchol/
  wx = sqrt(alpha)%*%context_embeddings
  wy = sqrt(alpha)%*%pre_trained
  transform_matrix <- qr.solve(wx, wy)

  return(t(transform_matrix))
}
