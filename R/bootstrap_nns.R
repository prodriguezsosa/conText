#' Bootstrap nearest neighbors
#'
#' Uses bootstrapping --sampling of of texts with replacement--
#' to identify the top N nearest neighbors based on cosine or inner product
#' similarity.
#'
#' @param context (character) vector of texts - `context` variable in get_context output
#' @param pre_trained (numeric) a F x D matrix corresponding to pretrained embeddings.
#' F = number of features and D = embedding dimensions.
#' rownames(pre_trained) = set of features for which there is a pre-trained embedding.
#' @param transform (logical) - if TRUE (default) apply the a la carte transformation, if FALSE ouput untransformed averaged embedding.
#' @param transform_matrix (numeric) a D x D 'a la carte' transformation matrix.
#' D = dimensions of pretrained embeddings.
#' @param candidates (character) vector defining the candidates for nearest neighbors - e.g. output from `get_local_vocab`.
#' @param bootstrap (logical) if TRUE, bootstrap similarity values - sample from texts with replacement.
#' Required to get std. errors.
#' @param num_bootstraps (numeric) - number of bootstraps to use.
#' @param N (numeric) number of nearest neighbors to return.
#' @param norm (character) - how to compute the similarity (see ?text2vec::sim2):
#' \describe{
#'   \item{`"l2"`}{cosine similarity}
#'   \item{`"none"`}{inner product}
#'   }
#'
#' @return a `data.frame` with the following columns:
#' \describe{
#'  \item{`feature`}{(character)  vector of feature terms corresponding to the nearest neighbors.}
#'  \item{`value`}{(numeric) cosine/inner product similarity between
#'  texts and feature. Average over bootstrapped samples if bootstrap = TRUE.}
#'  \item{`std.error`}{(numeric) std. error of the similarity value. Column is dropped if bootstrap = FALSE.}
#'  }
#'
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
#' local_vocab <- get_local_vocab(context_immigration$context, pre_trained = cr_glove_subset)
#'
#' set.seed(42L)
#' nns_immigration <- bootstrap_nns(context = context_immigration$context,
#'                                  pre_trained = cr_glove_subset,
#'                                  transform_matrix = cr_transform,
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
    nns <- dplyr::tibble(feature = names(mean_cos), value = unname(mean_cos), std.error = unname(stderror_cos)) %>% dplyr::arrange(-value)}else{

      # ELSE

      # compute aggregate embedding
      embeds_out <- embed_target(context = context, pre_trained, transform_matrix, transform = transform, aggregate = TRUE, verbose = FALSE)

      # compute similarity
      if(!is.null(candidates)) cos_sim <- text2vec::sim2(embeds_out$target_embedding, pre_trained[candidates,], method = 'cosine', norm = norm)else{
        cos_sim <- text2vec::sim2(embeds_out$target_embedding, pre_trained, method = 'cosine', norm = norm)
      }

      nns <- dplyr::tibble(feature = colnames(cos_sim), value = cos_sim[1,]) %>% dplyr::arrange(-value)

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
