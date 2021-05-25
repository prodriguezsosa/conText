#' Return nearest neighbors based on cosine similarity
#'
#' @param target_embedding matrix of numeric values
#' @param pre_trained matrix of numeric values - pretrained embeddings
#' @param N number of nearest neighbors to return
#' @param candidates character vector defining vocabulary to subset conmparison to
#' @param norm character = c("l2", "none") - how to scale input matrices. If they are already scaled - use "none" (see ?sim2)
#' @return a character vector of nearest neighbors to target
#' @examples
#' library(conText)
#' library(dplyr)
#'
#' # load data
#' corpus <- sample_corpus
#' pre_trained <- sample_glove
#' transform_matrix <- khodakA
#'
#' # find contexts for term immigration
#' context_immigration <- get_context(x = corpus$speech, target = 'immigration',
#'                                    window = 6, valuetype = "fixed", case_insensitive = TRUE,
#'                                    hard_cut = FALSE, verbose = FALSE)
#'
#' # compute alc embedding
#' contexts_vectors <- embed_target(context = context_immigration$context, pre_trained,
#'                                  transform_matrix, transform = TRUE,
#'                                  aggregate = TRUE, verbose = FALSE)
#'
#' # find local vocab
#' local_vocab <- get_local_vocab(context_immigration$context, pre_trained)
#'
# find nearest neighbors
#' find_nns(target_embedding = contexts_vectors$target_embedding,
#'          pre_trained, N = 5, candidates = local_vocab, norm = "l2")
#' @export
find_nns <- function(target_embedding, pre_trained, N = 5, candidates = NULL, norm = "l2"){
  if(is.null(candidates)) cos_sim <- text2vec::sim2(x = pre_trained, y = matrix(target_embedding, nrow = 1), method = "cosine", norm = norm)
  if(!is.null(candidates)) cos_sim <- text2vec::sim2(x = pre_trained[candidates,], y = matrix(target_embedding, nrow = 1), method = "cosine", norm = norm)
  nn <- cos_sim[order(-cos_sim),]
  return(names(nn)[1:N])
}
