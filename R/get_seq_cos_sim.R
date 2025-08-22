#' Calculate cosine similarities between target word and candidates words over
#' sequenced variable using ALC embedding approach
#'
#' @param x (character) vector - this is the set of documents (corpus) of interest
#' @param seqvar ordered variable such as list of dates or ordered iseology scores
#' @param target (character) vector - target word
#' @param candidates (character) vector of features of interest
#' @param pre_trained (numeric) a F x D matrix corresponding to pretrained embeddings.
#' F = number of features and D = embedding dimensions.
#' rownames(pre_trained) = set of features for which there is a pre-trained embedding.
#' @param transform_matrix (numeric) a D x D 'a la carte' transformation matrix.
#' D = dimensions of pretrained embeddings.
#' @inheritParams get_context
#'
#' @return a data.frame with one column for
#' each candidate term with corresponding cosine similarity values
#' and one column for seqvar.
#'
#' @export
#'
#' @examples
#'
#' library(quanteda)
#' # gen sequence var (here: year)
#' docvars(cr_sample_corpus, 'year') <- rep(2011:2014, each = 50)
#' cos_simsdf <- get_seq_cos_sim(x = cr_sample_corpus,
#' seqvar = docvars(cr_sample_corpus, 'year'),
#' target = "equal",
#' candidates = c("immigration", "immigrants"),
#' pre_trained = cr_glove_subset,
#' transform_matrix = cr_transform)
get_seq_cos_sim <- function(x,
                           seqvar,
                           target,
                           candidates,
                           pre_trained,
                           transform_matrix,
                           window = 6,
                           valuetype = "fixed",
                           case_insensitive = TRUE,
                           hard_cut = FALSE,
                           verbose = TRUE) {

  veclist <- list()
  seqvals <- unique(seqvar)

  for (i in seq_along(seqvals)) {
    seqval = seqvals[[i]]

    # get context words for target within sequence var
    contextftu <-
      get_context(
        x = x[which(seqvar==seqval)], #subset corpus to observations for unit in sequence
        target = target,
        window = window,
        valuetype = valuetype,
        case_insensitive = case_insensitive,
        hard_cut = hard_cut,
        verbose = verbose
      )

    # embed each instance using a la carte
    # try catch errors for when zero instances: replaced with NULL
    error <-
      tryCatch(
        veclist[[i]] <-
          embed_target(
            context = contextftu$context,
            pre_trained,
            transform_matrix,
            transform = TRUE,
            aggregate = TRUE,
            verbose = verbose
          ),
        error = function(e)
          e
      )
    if (inherits(error, 'error')) {
      warning("No instances of target found in corpus; replacing with NULL")
      veclist[[i]] <- list(
        target_embedding = NULL,
        local_vocab = NULL,
        obs_included = NULL
      )
    }
  }

  # get cosine similarities

  cos_simsdf <- data.frame()
  cos_sims <- vector()

  for (i in seq_along(veclist)) {
    target_embedding = veclist[[i]][["target_embedding"]]

    # replace cosine similarities for candidate words with NA when target embedding NULL
    if(is.null(target_embedding)){
      cos_sim <- as.vector(rep(NA, length(candidates)))
    }else{
      cos_sim <- find_cos_sim(
        target_embedding = target_embedding,
        pre_trained,
        candidates = candidates,
        norm = "l2")
    }

    cos_sim <- as.vector(cos_sim)
    cos_sims <- rbind(cos_sims, cos_sim)
    cos_simsdf <- as.data.frame(cos_sims, row.names = F)

  }

  for (i in seq_along(candidates)) {
    cname = candidates[[i]]
    names(cos_simsdf)[i] <- paste0(cname)
  }

  cos_simsdf <- cbind(cos_simsdf, seqvals)
  names(cos_simsdf)[names(cos_simsdf) == "seqvals"] <- "seqvar"

  return(cos_simsdf)
}


#' Find cosine similarities between target and candidate words
#'
#' @param target_embedding matrix of numeric values
#' @param pre_trained matrix of numeric values - pretrained embeddings
#' @param candidates character vector defining vocabulary to subset comparison to
#' @param norm character = c("l2", "none") - how to scale input matrices. If they are already scaled - use "none" (see ?sim2)
#'
#' @return a vector of cosine similarities of length candidates
#'
find_cos_sim <- function(target_embedding,
                         pre_trained,
                         candidates,
                         norm = "l2")
{
  if (length(candidates) == 1){
    error <- tryCatch(
      cos_sim <- text2vec::sim2(
        x = matrix(pre_trained[candidates, ], nrow = 1),
        y = matrix(target_embedding, nrow = 1),
        method = "cosine",
        norm = norm
      ),

      error = function(e)
        e
    )
    if (inherits(error, 'error')) {
      warning("No instances of candidate: '",
              candidates,
              "' found in pre-trained embeddings")
      cos_sim <- as.vector(rep(NA, 1))
      return(cos_sim)
    }
    row.names(cos_sim) <- candidates
    return(cos_sim)}else{

      if (length(candidates) > 1)
        cos_sim <- vector()
      for (i in seq_along(candidates)) {
        candidate = candidates[[i]]
        error <-
          tryCatch(
            cos_sim_i <- text2vec::sim2(
              x = matrix(pre_trained[candidate, ], nrow = 1),
              y = matrix(target_embedding, nrow = 1),
              method = "cosine",
              norm = norm
            ),
            error = function(e)
              e
          )
        if (inherits(error, 'error')) {
          warning("No instances of candidate: '",
                  candidate,
                  "' found in pre-trained embeddings")
          cos_sim_i <- as.vector(rep(NA, 1))
        }
        cos_sim <- rbind(cos_sim, cos_sim_i)
      }

      row.names(cos_sim) <- candidates
      return(cos_sim)}
}
