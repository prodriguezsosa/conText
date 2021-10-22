#' Embed target using either: (a) a la carte OR (b) simple (untransformed) averaging of context embeddings
#'
#' For a vector of contexts (generally the context variable in get_context output), return
#' the transformed (or untransformed) additive embeddings, aggregated or by instance, along with
#' the local vocabulary. Keep track of which contexts were embedded and which were excluded.
#'
#' required packages: quanteda
#'
#' @param context (character) vector of texts - `context` variable in get_context output
#' @param pre_trained (numeric) a F x D matrix corresponding to pretrained embeddings.
#' F = number of features and D = embedding dimensions.
#' rownames(pre_trained) = set of features for which there is a pre-trained embedding.
#' @param transform (logical) if TRUE (default) apply the 'a la carte' transformation,
#' if FALSE ouput untransformed averaged embeddings.
#' @param transform_matrix (numeric) a D x D 'a la carte' transformation matrix.
#' D = dimensions of pretrained embeddings.
#' @param aggregate (logical) - if TRUE (default) output will return one embedding (i.e. averaged over all instances of target)
#' if FALSE output will return one embedding per instance
#' @param verbose (logical) - report the observations that had no overlap the provided pre-trained embeddings
#'
#' @return list with three elements:
#' \describe{
#' \item{`target_embedding`}{the target embedding(s). Values and dimensions will vary with the above settings.}
#' \item{`local_vocab`}{(character) vocabulary that appears in the set of contexts provided.}
#' \item{`obs_included`}{(integer) rows of the context vector that were included in the computation.
#' A row (context) is excluded when none of the words in the context are present in the pre-trained embeddings provided.}
#' }
#'
#' @export
#' @rdname embed_target
#' @keywords embed_target
#' @examples
#' # find contexts for term immigration
#' context_immigration <- get_context(x = cr_sample_corpus, target = 'immigration',
#'                         window = 6, valuetype = "fixed", case_insensitive = TRUE,
#'                         hard_cut = FALSE, verbose = FALSE)
#'
#' contexts_vectors <- embed_target(context = context_immigration$context,
#' pre_trained = cr_glove_subset,
#' transform = TRUE, transform_matrix = cr_transform,
#' aggregate = FALSE, verbose = FALSE)
embed_target <- function(context, pre_trained, transform = TRUE, transform_matrix, aggregate = TRUE, verbose = TRUE){

  # checks
  if(!(ncol(pre_trained) == nrow(transform_matrix)))stop("transformation matrix must have the same number of dimensions as the pre-trained embeddings")
  if(!(is.character(context)))stop("context must be a character vector.")

  # build context term-feature matrix
  context_tfm <- quanteda::dfm(quanteda::tokens(context))
  local_vocab <- intersect(colnames(context_tfm), rownames(pre_trained))  # common vocab between pretrained and contexts
  context_tfm <- quanteda::dfm_select(context_tfm, pattern = local_vocab)  # subset tfm to common vocab
  N <- Matrix::rowSums(context_tfm)

  # identify those contexts that have no words in the pre-trained embeddings
  missing = unname(which(N==0))
  included = unname(which(N>0))
  if(verbose) if(length(missing) >0) cat('the following observations had no overlap with the pre-trained embeddings provided:', missing, '\n')

  # remove contexts with no context words in the pre-trained embeddings to avoid NaNs (i.e. dividing by 0)
  context_tfm <- context_tfm[N!=0,]
  N <- Matrix::rowSums(context_tfm)

  # context embeddings by instance
  context_embedding <- context_tfm[,local_vocab] %*% pre_trained[local_vocab,] # subsets both tfm & pretrained embeddings to the intersect of their vocabs
  context_embedding <- sweep(context_embedding, 1, 1/N, '*') # divide by number of context words (not a constant if hard_cut == FALSE in get_context)

  # aggregate context embeddings (default)
  if(aggregate) context_embedding <- Matrix::colMeans(context_embedding)

  # apply transformation (default)
  context_embedding <- matrix(context_embedding, ncol = ncol(pre_trained)) # adjust format
  if(transform){target_embedding <- transform_matrix %*% t(context_embedding) %>% t}else{
    target_embedding <- context_embedding
  }

  # output
  return(list(target_embedding = target_embedding, local_vocab = local_vocab, obs_included = included))
}
