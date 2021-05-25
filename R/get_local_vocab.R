#' Get local vocab
#'
#' Local vocab consists of the intersect between the set of pre-trained embeddings
#' and the local contexts.
#'
#' @param context the text to be used to compute localized embeddings
#' @param pre_trained the pre-trained embeddings to be used to compute localized embeddings
#'
#' @return chr vector - vector of words in vocabulary
#' @examples
#' library(conText)
#' library(dplyr)
#'
#' # load data
#' corpus <- sample_corpus
#' pre_trained <- sample_glove
#' transform_matrix <- khodakA
#'
#' # find contexts of immigration
#' context_immigration <- get_context(x = corpus$speech, target = 'immigration',
#'                         window = 6, valuetype = "fixed", case_insensitive = TRUE,
#'                         hard_cut = FALSE, verbose = FALSE)
#'
#' # find local vocab (use it to define the candidate of nearest neighbors)
#' local_vocab <- get_local_vocab(context_immigration$context, pre_trained)
#' @export
get_local_vocab <- function(context, pre_trained){

  # build context term-feature matrix
  context_tfm <- quanteda::dfm(quanteda::tokens(context))

  # common vocab between pretrained and contexts
  local_vocab <- intersect(colnames(context_tfm), rownames(pre_trained))

  return(local_vocab)
}
