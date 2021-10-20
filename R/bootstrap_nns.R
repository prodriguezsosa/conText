#' Bootstrap nearest neighbors
#'
#' @param context vector of texts - `context`` variable in get_context output
#' @param pre_trained a V x D matrix of numeric values - pretrained embeddings with V = size of vocabulary and D = embedding dimensions
#' @param transform_matrix a D x D transformation matrix
#' @param transform logical - if TRUE (default) apply the a la carte transformation, if FALSE ouput untransformed averaged embedding
#' @param candidates character vector defining the candidates for nearest neighbors - e.g. output from `get_local_vocab`
#' @param bootstrap logical - if TRUE, bootstrap cosine similarity - required to get standard errors cosine similarity of nearest neighbors
#' @param num_bootstraps numeric - number of bootstraps to use
#' @param N number of nearest neighbors to return
#' @param norm character = c("l2", "none") - set to 'l2' for cosine similarity and to 'none' for inner product (see ?sim2 in text2vec)
#'
#' @return data.frame with nearest neighbor tokens, similarity (numeric value) and std.error (given bootstrap)
#' @export
#' @rdname bootstrap_nns
#' @keywords bootstrap_nns
#' @examples
#'
#' # find contexts of immigration
#' context_immigration <- get_context(x = cr_sample_corpus,
#'                                    target = 'immigration',
#'                                    window = 6,
#'                                    valuetype = "fixed",
#'                                    case_insensitive = TRUE,
#'                                    hard_cut = FALSE, verbose = FALSE)
#'
#' # find local vocab (use it to define the candidate of nearest neighbors)
#' local_vocab <- get_local_vocab(context_immigration$context, pre_trained = glove_subset)
#'
#' set.seed(42L)
#' nns_immigration <- bootstrap_nns(context = context_immigration$context,
#'                                  pre_trained = glove_subset,
#'                                  transform_matrix = khodakA,
#'                                  transform = TRUE,
#'                                  candidates = local_vocab,
#'                                  bootstrap = TRUE,
#'                                  num_bootstraps = 20, N = 50,
#'                                  norm = "l2")
#'
#' @export
bootstrap_nns <- function(context = NULL, pre_trained = NULL, transform = TRUE, transform_matrix = NULL, candidates = NULL, bootstrap = TRUE, num_bootstraps = 20, N = 50, norm = "l2"){

  # IF BOOTSTRAP
  if(bootstrap){

    # compute single instance embeddings
    embeds_out <- embed_target(context = context, pre_trained, transform_matrix, transform = transform, aggregate = FALSE, verbose = FALSE)

    # bootstrapped similarity
    bootstrap_out <- replicate(num_bootstraps, bootstrap_similarity(target_embeddings = embeds_out$target_embedding, pre_trained, candidates = candidates), simplify = FALSE)

    # mean similarity
    cos_out <- do.call(rbind,bootstrap_out)
    mean_cos <- apply(cos_out, 2, mean)
    stderror_cos <- 1/sqrt(nrow(cos_out)) * apply(cos_out, 2, sd)
    nns <- dplyr::tibble(Term = names(mean_cos), Estimate = unname(mean_cos), Std.Error = unname(stderror_cos)) %>% dplyr::arrange(-Estimate)}else{

      # ELSE

      # compute aggregate embedding
      embeds_out <- embed_target(context = context, pre_trained, transform_matrix, transform = transform, aggregate = TRUE, verbose = FALSE)

      # compute similarity
      if(!is.null(candidates)) cos_sim <- text2vec::sim2(embeds_out$target_embedding, pre_trained[candidates,], method = 'cosine', norm = norm)else{
        cos_sim <- text2vec::sim2(embeds_out$target_embedding, pre_trained, method = 'cosine', norm = norm)
      }

      nns <- dplyr::tibble(Term = colnames(cos_sim), Estimate = cos_sim[1,]) %>% dplyr::arrange(-Estimate)

    }

  # subset if N is not null
  if(!is.null(N))  nns <- nns[1:N,]

  # output
  return(nns)
}

# -----------------------------
#
# SUB-FUNCTIONS
#
# -----------------------------

#' Boostrap similarity vector
#'
#' @param target_embeddings the target embeddings (embeddings of context)
#' @param pre_trained a V x D matrix of numeric values - pretrained embeddings with V = size of vocabulary and D = embedding dimensions
#' @param candidates character vector defining the candidates for nearest neighbors - e.g. output from `get_local_vocab`
#' @param norm character = c("l2", "none") - set to 'l2' for cosine similarity and to 'none' for inner product (see ?sim2 in text2vec)
#'
#' @return vector(s) of cosine similarities between alc embedding and nearest neighbor candidates
#'
bootstrap_similarity <- function(target_embeddings = NULL, pre_trained = NULL, candidates = NULL, norm = NULL){

  # sample
  bs_target_embeddings <- target_embeddings[sample(1:nrow(target_embeddings), replace = TRUE),]

  # compute similarities
  bs_out <- compute_similarity(target_embeddings = bs_target_embeddings, pre_trained = pre_trained, candidates = candidates, norm = norm)

  # output
  return(bs_out)

}

#' Compute similarity vector (sub-function of bootstrap_similarity)
#'
#' @param target_embeddings the target embeddings (embeddings of context)
#' @param pre_trained a V x D matrix of numeric values - pretrained embeddings with V = size of vocabulary and D = embedding dimensions
#' @param candidates character vector defining the candidates for nearest neighbors - e.g. output from `get_local_vocab`
#' @param norm character = c("l2", "none") - set to 'l2' for cosine similarity and to 'none' for inner product (see ?sim2 in text2vec)
#'
#' @return vector of cosine similarities between alc embedding and nearest neighbor candidates
#'
compute_similarity <- function(target_embeddings = NULL, pre_trained = NULL, candidates = NULL, norm = NULL){

  # ALC embeddings
  alc_embedding <- matrix(Matrix::colMeans(target_embeddings), nrow = 1)

  # cosine similarities
  if(!is.null(candidates)){cos_sim <- text2vec::sim2(alc_embedding, pre_trained[candidates,], method = 'cosine', norm = norm)}else{
    cos_sim <- text2vec::sim2(alc_embedding, pre_trained, method = 'cosine', norm = norm)
  }

  # output
  return(cos_sim)

}
