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
#' @param stem (logical) - whether to stem candidates when evaluating nns. Default is FALSE.
#' If TRUE, candidate stems are ranked by their average cosine similarity to the target.
#' We recommend you remove misspelled words from candidate set `candidates` as these can
#' significantly influence the average.
#' @inheritParams SnowballC::wordStem
#'
#' @return (character) vector of nearest neighbors to target
#' @export
#' @rdname find_nns
#' @keywords find_nns
#' @examples
# find nearest neighbors
#' find_nns(target_embedding = cr_glove_subset['immigration',],
#'          pre_trained = cr_glove_subset, N = 5,
#'          candidates = NULL, norm = "l2", stem = FALSE)
find_nns <- function(target_embedding, pre_trained, N = 5, candidates = NULL, norm = "l2", stem = FALSE, language = 'porter'){
  if(is.null(candidates)) cos_sim <- text2vec::sim2(x = pre_trained, y = matrix(target_embedding, nrow = 1), method = "cosine", norm = norm)[,1]
  if(!is.null(candidates)) cos_sim <- text2vec::sim2(x = pre_trained[candidates,], y = matrix(target_embedding, nrow = 1), method = "cosine", norm = norm)[,1]
  nn_df <- data.frame(token = names(cos_sim), value = unname(cos_sim)) %>% dplyr::arrange(-value)
  # stemming
  if(stem){
    if (requireNamespace("SnowballC", quietly = TRUE)) {
      cat('Using', language, 'for stemming. To check available languages run "SnowballC::getStemLanguages()"', '\n')
      nn_df <- nn_df %>% dplyr::mutate(token = SnowballC::wordStem(token, language = language)) %>% dplyr::group_by(token) %>% dplyr::summarize(value = mean(value)) %>% dplyr::arrange(-value) %>% dplyr::ungroup()
    } else warning('"SnowballC (>= 0.7.0)" package must be installed to use stemmming option. Will proceed without stemming.', call. = FALSE)
  }
  return(nn_df$token[1:N])
}
