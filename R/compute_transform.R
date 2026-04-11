#' Compute transformation matrix A
#'
#' Computes a transformation matrix, given a feature-co-occurrence
#' matrix and corresponding pre-trained embeddings.
#'
#' @param x a (quanteda) `fcm-class` object.
#' @param pre_trained (numeric) a F x D matrix corresponding to pretrained embeddings,
#' usually trained on the same corpus as that used for `x`.
#' F = number of features and D = embedding dimensions.
#' rownames(pre_trained) = set of features for which there is a pre-trained embedding
#' @param weighting (character or numeric) weighting options:
#' \describe{
#'   \item{`1`}{no weighting.}
#'   \item{`"log"`}{weight by the log of the frequency count.}
#'   \item{`numeric`}{threshold based weighting (= 1 if token count meets threshold, 0 ow).}
#'   }
#' Recommended: use `log` for small corpora, a numeric threshold for larger corpora.
#'
#' @return a `dgTMatrix-class` D x D non-symmetrical matrix
#' (D = dimensions of pre-trained embedding space) corresponding
#' to an 'a la carte' transformation matrix. This matrix is optimized
#' for the corpus and pre-trained embeddings employed.
#'
#' @export
#' @rdname compute_transform
#' @keywords compute_transform
#' @examples
#'
#' \dontrun{
#' # example exceeds CRAN CPU time to elapsed time limit
#' library(quanteda)
#' # note, cr_sample_corpus is too small to produce sensical word vectors
#'
#' # tokenize
#' toks <- tokens(cr_sample_corpus)
#'
#' # construct feature-co-occurrence matrix
#' toks_fcm <- fcm(toks, context = "window", window = 6,
#' count = "weighted", weights = 1 / (1:6), tri = FALSE)
#'
#' # you will generally want to estimate a new (corpus-specific)
#' # GloVe model, we will use cr_glove_subset instead
#' # see the Quick Start Guide to see a full example.
#'
#' # estimate transform
#' local_transform <- compute_transform(x = toks_fcm,
#' pre_trained = cr_glove_subset, weighting = 'log')
#' }
compute_transform <- function(x, pre_trained, weighting = 500){

  # compute un-transformed additive embedding
  context_embeddings <- fem(x = x, pre_trained = pre_trained, transform_matrix, transform = FALSE, verbose = FALSE)

  # extract feature frequency from fcm object
  feature_frequency <- x@meta$object$margin
  feature_frequency <- feature_frequency[intersect(names(feature_frequency), rownames(context_embeddings))]
  feature_frequency <- feature_frequency[intersect(names(feature_frequency), rownames(pre_trained))] # only use overlap btw pretrained and context embeddings (when pretrained embeddings are not trained on local corpus)
  if(weighting == 'log') feature_frequency <- feature_frequency[feature_frequency >= 1] # avoid negatives when taking logs

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
