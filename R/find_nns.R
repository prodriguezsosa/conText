#' Return nearest neighbors based on cosine similarity
#'
#' @param target_embedding (numeric) 1 x D matrix. D = dimensions of pretrained embeddings.
#' @param pre_trained (numeric) a F x D matrix corresponding to pretrained embeddings.
#' F = number of features and D = embedding dimensions.
#' rownames(pre_trained) = set of features for which there is a pre-trained embedding.
#' @param N (numeric) number of nearest neighbors to return.
#' @param candidates (character) vector of candidate features for nearest neighbors
#' @param norm (character) - how to compute similarity (see ?text2vec::sim2):
#' \describe{
#'   \item{`"l2"`}{cosine similarity}
#'   \item{`"none"`}{inner product}
#'   }
#'
#' @return (character) vector of nearest neighbors to target
#' @export
#' @rdname find_nns
#' @keywords find_nns
#' @examples
# find nearest neighbors
#' find_nns(target_embedding = cr_glove_subset['immigration',],
#'          pre_trained = cr_glove_subset, N = 5,
#'          candidates = NULL, norm = "l2")
find_nns <- function(target_embedding, pre_trained, N = 5, candidates = NULL, norm = "l2"){
  if(is.null(candidates)) cos_sim <- text2vec::sim2(x = pre_trained, y = matrix(target_embedding, nrow = 1), method = "cosine", norm = norm)
  if(!is.null(candidates)) cos_sim <- text2vec::sim2(x = pre_trained[candidates,], y = matrix(target_embedding, nrow = 1), method = "cosine", norm = norm)
  nn <- cos_sim[order(-cos_sim),]
  return(names(nn)[1:N])
}
