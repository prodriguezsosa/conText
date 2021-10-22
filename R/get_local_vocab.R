#' Identify words common to a collection of texts and a set of pretrained embeddings.
#'
#' Local vocab consists of the intersect between the set of pretrained embeddings
#' and the collection of texts.
#'
#' @param context (character) vector of contexts (usually `context` in `get_context()` output)
#' @param pre_trained (numeric) a F x D matrix corresponding to pretrained embeddings.
#' F = number of features and D = embedding dimensions.
#' rownames(pre_trained) = set of features for which there is a pre-trained embedding.
#'
#' @return  (character) vector of words common to the texts and pretrained embeddings.
#'
#' @export
#' @rdname get_local_vocab
#' @keywords get_local_vocab
#' @examples
#' # find local vocab (use it to define the candidate of nearest neighbors)
#' local_vocab <- get_local_vocab(cr_sample_corpus, pre_trained = cr_glove_subset)

get_local_vocab <- function(context, pre_trained){

  # build context term-feature matrix
  context_tfm <- quanteda::dfm(quanteda::tokens(context))

  # common vocab between pretrained and contexts
  local_vocab <- intersect(colnames(context_tfm), rownames(pre_trained))

  return(local_vocab)
}
