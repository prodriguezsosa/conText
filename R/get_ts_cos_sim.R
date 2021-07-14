#' Calculate cosine similarities between target word and candidates words over
#' time using ALC embedding approach
#'
#' @param x a character vector - this is the set of documents (corpus) of interest
#' @param timevar name of variable in corpus defining time unit within which to calculate similarities
#' @param textvar name of variable in corpus from which the text comes (e.g., "tweet" in dataset of tweets)
#' @param target a character vector - target word
#' @param candidates character vector defining vocabulary to subset conmparison to
#' @param pre_trained a V x D matrix of numeric values - pretrained embeddings with V = size of vocabulary and D = embedding dimensions
#' @param transform_matrix a D x D transformation matrix
#' @param window integer - defines the size of a context (words around the target)
#' @param valuetype the type of pattern matching: "glob" for "glob"-style wildcard expressions;
#' "regex" for regular expressions; or "fixed" for exact matching.
#' @param case_insensitive logical - if TRUE, ignore case when matching the target.
#' See quanteda's documentation for the kwic function.
#' @param hard_cut logical - if TRUE then the text must have window x 2 tokens,
#' if FALSE it can have window x 2 or fewer (e.g. if a doc begins with a target word,
#' then text will have window tokens rather than window x 2)
#' @param verbose logical - if TRUE, report the total number of target instances found, and
#' report the observations that had no overlap the provided pre-trained embeddings
#'
#' @return
#' @export
#'
#' @examples
#'library(conText)
#'library(dplyr)
#'
#' # load corpus
#' corpus <- sample_corpus
#' pre_trained <- sample_glove
#' transform_matrix <- khodakA
#'
#' # gen time var
#' corpus$year <- rep(2011:2014, each = 250)
#' cos_simsdf <- get_ts_cos_sim(x = corpus, timevar = "year", textvar = "speech", target = "equal", candidates = c("and", "the"), pre_trained = pre_trained, transform_matrix = transform_matrix)
get_ts_cos_sim <- function(x,
                           timevar,
                           textvar,
                           target,
                           candidates,
                           pre_trained,
                           transform_matrix,
                           window = 6,
                           valuetype = "fixed",
                           case_insensitive = TRUE,
                           hard_cut = FALSE,
                           verbose = TRUE) {

  timevec <- unique(x[[timevar]])
  timevec <- sort(timevec)

  veclist <- list()

  for (i in seq_along(timevec)) {
    timevecunit = timevec[[i]]

    # get context words for target
    contextftu <-
      get_context(
        x = subset(x[[textvar]], x[[timevar]] == timevecunit),
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
      veclist[[i]] <- list(
        target_embedding = NULL,
        local_vocab = NULL,
        obs_included = NULL
      )
      next
    }
  }

  cos_simsdf <- data.frame()
  cos_sims <- vector()

  for (i in seq_along(veclist)) {
    target_embedding = veclist[[i]][["target_embedding"]]

    # get cosine similarities
    # trycatch for errors when zero instances; replaced with NA
    error <-
      tryCatch(
        cos_sim <- find_cos_sim(
          target_embedding = target_embedding,
          pre_trained,
          candidates = candidates,
          norm = "l2"
        ),
        error = function(e)
          e
      )

    if (inherits(error, 'error')) {
      cos_sim <- as.vector(rep(NA, length(candidates)))
    }

    cos_sim <- as.vector(cos_sim)
    cos_sims <- rbind(cos_sims, cos_sim)
    cos_simsdf <- as.data.frame(cos_sims, row.names = F)

  }

  for (i in seq_along(candidates)) {
    cname = candidates[[i]]
    names(cos_simsdf)[i] <- paste0(cname)
  }

  cos_simsdf <- cbind(cos_simsdf, timevec)

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
  if (length(candidates) == 1)
    cos_sim <- text2vec::sim2(
      x = matrix(pre_trained[candidates,], nrow = 1),
      y = matrix(target_embedding, nrow = 1),
      method = "cosine",
      norm = norm
    )
  if (length(candidates) > 1)
    cos_sim <- text2vec::sim2(
      x = pre_trained[candidates,],
      y = matrix(target_embedding, nrow = 1),
      method = "cosine",
      norm = norm
    )
  return(cos_sim)
}
