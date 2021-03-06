#' Contrast nearest neighbors
#'
#' @param context1 context vector for group 1 - `context`` variable in get_context output
#' @param context2 context vector for group 2 - `context`` variable in get_context output
#' @param pre_trained a V x D matrix of numeric values - pretrained embeddings with V = size of vocabulary and D = embedding dimensions
#' @param transform_matrix a D x D transformation matrix
#' @param transform logical - if TRUE (default) apply the a la carte transformation, if FALSE ouput untransformed averaged embedding
#' @param bootstrap logical - if TRUE, bootstrap cosine similarity - required to get standard errors cosine similarity of nearest neighbors
#' @param num_bootstraps numeric - number of bootstraps to use
#' @param permute logical - if TRUE, compute empirical p-values using permutation test
#' @param num_permutations numeric - number of permutations to use
#' @param candidates character vector defining the candidates for nearest neighbors - e.g. output from `get_local_vocab`
#' @param N number of nearest neighbors to return
#' @param norm character = c("l2", "none") - set to 'l2' for cosine similarity and to 'none' for inner product (see ?sim2 in text2vec)
#'
#' @return a list with three elements, nns for group 1, nns for group 2 and nns_ratio comparing with ratios of similarities between the two groups
#' @export
#'
contrast_nns <- function(context1 = NULL, context2 = NULL, pre_trained = NULL, transform_matrix = NULL, transform = TRUE, bootstrap = TRUE, num_bootstraps = 20, permute = TRUE, num_permutations = 100, candidates = NULL, N = 50, norm = "l2"){

  # compute single instance embeddings
  embeds_out1 <- embed_target(context = context1, pre_trained, transform_matrix, aggregate = FALSE, verbose = FALSE)
  embeds_out2 <- embed_target(context = context2, pre_trained, transform_matrix, aggregate = FALSE, verbose = FALSE)

  if(bootstrap){

    cat('starting bootstrapping \n')
    # bootstrap similarity
    bootstrap_out <- replicate(num_bootstraps, bootstrap_contrast(target_embeddings1 = embeds_out1$target_embedding, target_embeddings2 = embeds_out2$target_embedding, pre_trained, candidates = candidates, norm = norm), simplify = FALSE)

    # sim_out1
    bs_sim_out1 <- lapply(bootstrap_out, '[[', 'cos_sim1') %>% do.call(rbind,.)
    sim_out1 <- apply(bs_sim_out1, 2, mean)
    stderror_sim_out1 <- 1/sqrt(nrow(bs_sim_out1)) * apply(bs_sim_out1, 2, sd)
    nns1 <- dplyr::tibble(Term = names(sim_out1), Estimate = unname(sim_out1), Std.Error = unname(stderror_sim_out1))

    # sim_out2
    bs_sim_out2 <- lapply(bootstrap_out, '[[', 'cos_sim2') %>% do.call(rbind,.)
    sim_out2 <- apply(bs_sim_out2, 2, mean)
    stderror_sim_out2 <- 1/sqrt(nrow(bs_sim_out2)) * apply(bs_sim_out2, 2, sd)
    nns2 <- dplyr::tibble(Term = names(sim_out2), Estimate = unname(sim_out2), Std.Error = unname(stderror_sim_out2))

    # sim_ratio
    bs_sim_ratio <- lapply(bootstrap_out, '[[', 'sim_ratio') %>% do.call(rbind,.)
    sim_ratio <- apply(bs_sim_ratio, 2, mean)
    dev1 <- abs(sim_ratio - 1)
    stderror_sim_ratio <- 1/sqrt(nrow(bs_sim_ratio)) * apply(bs_sim_ratio, 2, sd)
    nns_ratio <- dplyr::tibble(Term = names(sim_ratio), Estimate = unname(sim_ratio), Std.Error = unname(stderror_sim_ratio))
    cat('done bootstrapping \n')

  }else{

    # ALC embeddings
    alc_embedding1 <- matrix(Matrix::colMeans(embeds_out1$target_embedding), nrow = 1)
    alc_embedding2 <- matrix(Matrix::colMeans(embeds_out2$target_embedding), nrow = 1)

    # sim_out1
    sim_out1 <- text2vec::sim2(alc_embedding1, pre_trained[candidates,], method = 'cosine', norm = norm)
    nns1 <- dplyr::tibble(Term = colnames(sim_out1), Estimate = sim_out1[1,])

    # sim_out2
    sim_out2 <- text2vec::sim2(alc_embedding2, pre_trained[candidates,], method = 'cosine', norm = norm)
    nns2 <- dplyr::tibble(Term = colnames(sim_out2), Estimate = sim_out2[1,])

    # ratio of cosine similarities
    sim_ratio <- sim_out1/sim_out2
    dev1 <- abs(sim_ratio - 1)
    nns_ratio <- dplyr::tibble(Term = colnames(sim_ratio), Estimate = sim_ratio[1,])
  }

  if(permute){

    # permute similarity
    cat('starting permutations \n')
    permute_out <- replicate(num_permutations, permute_contrast(target_embeddings1 = embeds_out1$target_embedding, target_embeddings2 = embeds_out2$target_embedding, pre_trained, candidates = candidates, norm = norm), simplify = FALSE)

    # pm_out1
    pm_sim_out1 <- lapply(permute_out, '[[', 'cos_sim1') %>% do.call(rbind,.)
    pm_comp1 <- apply(pm_sim_out1, 1, function(i) i >= sim_out1)
    pvalue1 <- apply(pm_comp1, 1, function(i) sum(i)/length(i))
    nns1 <- cbind(nns1, Empirical_Pvalue = unname(pvalue1))

    # pm_out2
    pm_sim_out2 <- lapply(permute_out, '[[', 'cos_sim2') %>% do.call(rbind,.)
    pm_comp2 <- apply(pm_sim_out2, 1, function(i) i >= sim_out2)
    pvalue2 <- apply(pm_comp2, 1, function(i) sum(i)/length(i))
    nns2 <- cbind(nns2, Empirical_Pvalue = unname(pvalue2))

    # pm_ratio
    pm_ratio_out <- lapply(permute_out, '[[', 'sim_ratio') %>% do.call(rbind,.)
    pm_dev1 <- abs(pm_ratio_out - 1)
    pm_dev1 <- apply(pm_dev1, 1, function(i) i >= dev1)
    pvalue_ratio <- apply(pm_dev1, 1, function(i) sum(i)/length(i))
    nns_ratio <- cbind(nns_ratio, Empirical_Pvalue = unname(pvalue_ratio))
    cat('done with permutations \n')
  }

  # arrange
  nns1 <- nns1 %>% dplyr::arrange(-Estimate)
  nns2 <- nns2 %>% dplyr::arrange(-Estimate)
  nns_ratio <- nns_ratio %>% dplyr::arrange(-Estimate)

  # output
  return(list(nns1 = nns1, nns2 = nns2, nns_ratio = nns_ratio))
}

# -----------------------------
#
# SUB-FUNCTIONS
#
# -----------------------------

#' Permute similarity and ratio computations
#'
#' @param target_embeddings1 ALC embeddings for group 1
#' @param target_embeddings2 ALC embeddings for group 2
#' @param pre_trained a V x D matrix of numeric values - pretrained embeddings with V = size of vocabulary and D = embedding dimensions
#' @param candidates character vector defining the candidates for nearest neighbors - e.g. output from `get_local_vocab`
#' @param norm character = c("l2", "none") - set to 'l2' for cosine similarity and to 'none' for inner product (see ?sim2 in text2vec)
#'
#' @return a list with three elements, nns for group 1, nns for group 2 and nns_ratio comparing with ratios of similarities between the two groups
#' @export
#'
# runs permutations
permute_contrast <- function(target_embeddings1 = NULL, target_embeddings2 = NULL, pre_trained = NULL, candidates = NULL, norm = NULL){

  # number of observations
  num_obs1 <- nrow(target_embeddings1)
  num_obs2 <- nrow(target_embeddings2)
  num_obs <- num_obs1 + num_obs2

  # randomly sample observations for each group
  obs1_perm <- sample(1:num_obs, num_obs1)
  obs2_perm <- setdiff(1:num_obs, obs1_perm)

  # sample embeddings
  target_embeddings <- rbind(target_embeddings1, target_embeddings2)
  target_embeddings <- target_embeddings[sample(1:nrow(target_embeddings)),] # shuffle
  target_embeddings1_perm <- matrix(Matrix::colMeans(target_embeddings[obs1_perm,]), nrow = 1)
  target_embeddings2_perm <- matrix(Matrix::colMeans(target_embeddings[obs2_perm,]), nrow = 1)

  # compute similarities
  pm_out <- compute_contrast(target_embeddings1_perm, target_embeddings2_perm, pre_trained = pre_trained, candidates = candidates, norm = norm)

  # output
  return(pm_out)

}

#' Bootstrap similarity and ratio computations
#'
#' @param target_embeddings1 ALC embeddings for group 1
#' @param target_embeddings2 ALC embeddings for group 2
#' @param pre_trained a V x D matrix of numeric values - pretrained embeddings with V = size of vocabulary and D = embedding dimensions
#' @param candidates character vector defining the candidates for nearest neighbors - e.g. output from `get_local_vocab`
#' @param norm character = c("l2", "none") - set to 'l2' for cosine similarity and to 'none' for inner product (see ?sim2 in text2vec)
#'
#' @return a list with three elements, nns for group 1, nns for group 2 and nns_ratio comparing with ratios of similarities between the two groups
#' @export
#'
# runs bootstraps
bootstrap_contrast <- function(target_embeddings1 = NULL, target_embeddings2 = NULL, pre_trained = NULL, candidates = NULL, norm = NULL){

  # sample
  bs_target_embeddings1 <- target_embeddings1[sample(1:nrow(target_embeddings1), replace = TRUE),]
  bs_target_embeddings2 <- target_embeddings2[sample(1:nrow(target_embeddings2), replace = TRUE),]

  # compute similarities
  bs_out <- compute_contrast(bs_target_embeddings1, bs_target_embeddings2, pre_trained = pre_trained, candidates = candidates, norm = norm)

  # output
  return(bs_out)

}

#' Compute similarity and similarity ratios
#'
#' @param target_embeddings1 ALC embeddings for group 1
#' @param target_embeddings2 ALC embeddings for group 2
#' @param pre_trained a V x D matrix of numeric values - pretrained embeddings with V = size of vocabulary and D = embedding dimensions
#' @param candidates character vector defining the candidates for nearest neighbors - e.g. output from `get_local_vocab`
#' @param norm character = c("l2", "none") - set to 'l2' for cosine similarity and to 'none' for inner product (see ?sim2 in text2vec)
#'
#' @return a list with three elements, nns for group 1, nns for group 2 and nns_ratio comparing with ratios of similarities between the two groups
#' @export
#'
# computes ratio of similarities
compute_contrast <- function(target_embeddings1 = NULL, target_embeddings2 = NULL, pre_trained = NULL, candidates = NULL, norm = NULL){

  # ALC embeddings
  alc_embedding1 <- matrix(Matrix::colMeans(target_embeddings1), nrow = 1)
  alc_embedding2 <- matrix(Matrix::colMeans(target_embeddings2), nrow = 1)

  # cosine similarities
  cos_sim1 <- text2vec::sim2(alc_embedding1, pre_trained[candidates,], method = 'cosine', norm = norm)
  cos_sim2 <- text2vec::sim2(alc_embedding2, pre_trained[candidates,], method = 'cosine', norm = norm)

  # ratio of cosine similarities
  sim_ratio <- cos_sim1/cos_sim2

  # output
  return(list(cos_sim1 = cos_sim1, cos_sim2 = cos_sim2, sim_ratio = sim_ratio))

}

