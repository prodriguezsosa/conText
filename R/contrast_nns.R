#' Contrast nearest neighbors
#'
#' Computes the ratio of cosine similarities between group embeddings and features
#' --that is, for any given feature it first computes the similarity between that feature
#' and each group embedding, and then takes the ratio of these two similarities.
#' This ratio captures how "discriminant" a feature is of a given group.
#'
#' @param x (quanteda) `tokens-class` object
#' @param groups (numeric, factor, character) a binary variable of the same length as `x`
#' @inheritParams dem
#' @param bootstrap (logical) if TRUE, use bootstrapping -- sample from texts with replacement and
#' re-estimate cosine ratios for each sample. Required to get std. errors.
#' @param num_bootstraps (numeric) - number of bootstraps to use
#' @param permute (logical) - if TRUE, compute empirical p-values using a permutation test
#' @param num_permutations (numeric) - number of permutations to use
#' @param candidates (character) vector of candidate features for nearest neighbors
#' @param N (numeric) - nearest neighbors are subset to the union of the N neighbors of each group (if NULL, ratio is computed for all features)
#'
#' @return a data.frame with following columns:
#' \describe{
#'  \item{`feature`}{(character) vector of feature terms corresponding to the nearest neighbors.}
#'  \item{`value`}{(numeric) ratio of cosine similarities. Average over bootstrapped samples if bootstrap = TRUE.}
#'  \item{`std.error`}{(numeric) std. error of the ratio of cosine similarties. Column is dropped if bootsrap = FALSE.}
#'  \item{`p.value`}{(numeric) empirical p-value. Column is dropped if permute = FALSE.}
#'  }
#'
#' @export
#' @rdname contrast_nns
#' @keywords contrast_nns
#' @examples
#'
#' library(quanteda)
#'
#' cr_toks <- tokens(cr_sample_corpus)
#'
#' immig_toks <- tokens_context(x = cr_toks,
#' pattern = "immigration", window = 6L, hard_cut = FALSE, verbose = TRUE)
#'
#' set.seed(42L)
#' party_nns <- contrast_nns(x = immig_toks,
#' groups = docvars(immig_toks, 'party'),
#' pre_trained = cr_glove_subset,
#' transform = TRUE, transform_matrix = cr_transform,
#' bootstrap = TRUE, num_bootstraps = 10,
#' permute = TRUE, num_permutations = 100,
#' candidates = NULL, N = 20,
#' verbose = FALSE)
contrast_nns <- function(x, groups = NULL, pre_trained = NULL, transform = TRUE, transform_matrix = NULL, bootstrap = TRUE, num_bootstraps = 20, permute = TRUE, num_permutations = 100, candidates = NULL, N = 20, verbose = TRUE){

  # checks
  if(class(x)[1] != "tokens") stop("data must be of class tokens")
  groupvals <- unique(groups)
  if(length(groupvals)!=2) stop("groups must be binary")

  # add grouping variable to docvars
  quanteda::docvars(x, "group") <- groups

  # construct document-feature-matrix
  toks_dfm <- quanteda::dfm(x, tolower = FALSE)

  # construct document-embedding-matrix
  toks_dem <- dem(x = toks_dfm, pre_trained = pre_trained, transform = transform, transform_matrix = transform_matrix, verbose = verbose)

  # aggregate dems by group var
  embeds_out1 <- toks_dem[which(toks_dem@docvars$group == groupvals[1]),]
  embeds_out2 <- toks_dem[which(toks_dem@docvars$group == groupvals[2]),]

  if(bootstrap){

    cat('starting bootstrapping \n')
    # bootstrap similarity
    bootstrap_out <- replicate(num_bootstraps, bootstrap_contrast(target_embeddings1 = embeds_out1, target_embeddings2 = embeds_out2, pre_trained, candidates = candidates, norm = 'l2'), simplify = FALSE)

    # sim_out1
    bs_sim_out1 <- lapply(bootstrap_out, '[[', 'cos_sim1') %>% do.call(rbind,.)
    sim_out1 <- apply(bs_sim_out1, 2, mean)
    stderror_sim_out1 <- apply(bs_sim_out1, 2, sd)
    nns1 <- dplyr::tibble(feature = names(sim_out1), value = unname(sim_out1), std.error = unname(stderror_sim_out1))

    # sim_out2
    bs_sim_out2 <- lapply(bootstrap_out, '[[', 'cos_sim2') %>% do.call(rbind,.)
    sim_out2 <- apply(bs_sim_out2, 2, mean)
    stderror_sim_out2 <- apply(bs_sim_out2, 2, sd)
    nns2 <- dplyr::tibble(feature = names(sim_out2), value = unname(sim_out2), std.error = unname(stderror_sim_out2))

    # sim_ratio
    bs_sim_ratio <- lapply(bootstrap_out, '[[', 'sim_ratio') %>% do.call(rbind,.)
    sim_ratio <- apply(bs_sim_ratio, 2, mean)
    dev1 <- abs(sim_ratio - 1)
    stderror_sim_ratio <- apply(bs_sim_ratio, 2, sd)
    nns_ratio <- dplyr::tibble(feature = names(sim_ratio), value = unname(sim_ratio), std.error = unname(stderror_sim_ratio))
    cat('done bootstrapping \n')

  }else{

    # ALC embeddings
    alc_embedding1 <- matrix(Matrix::colMeans(embeds_out1), nrow = 1)
    alc_embedding2 <- matrix(Matrix::colMeans(embeds_out2), nrow = 1)

    if(length(candidates) >0){
      # sim_out1
      sim_out1 <- text2vec::sim2(alc_embedding1, pre_trained[candidates,], method = 'cosine', norm = 'l2')

      # sim_out2
      sim_out2 <- text2vec::sim2(alc_embedding2, pre_trained[candidates,], method = 'cosine', norm = 'l2')
    }else{
      # sim_out1
      sim_out1 <- text2vec::sim2(alc_embedding1, pre_trained, method = 'cosine', norm = 'l2')

      # sim_out2
      sim_out2 <- text2vec::sim2(alc_embedding2, pre_trained, method = 'cosine', norm = 'l2')
    }

    # nearest neighbors
    nns1 <- dplyr::tibble(feature = colnames(sim_out1), value = sim_out1[1,])
    nns2 <- dplyr::tibble(feature = colnames(sim_out2), value = sim_out2[1,])

    # ratio of cosine similarities
    sim_ratio <- sim_out1/sim_out2
    dev1 <- abs(sim_ratio - 1)
    nns_ratio <- dplyr::tibble(feature = colnames(sim_ratio), value = sim_ratio[1,])
  }

  if(permute){

    # permute similarity
    cat('starting permutations \n')
    permute_out <- replicate(num_permutations, permute_contrast(target_embeddings1 = embeds_out1, target_embeddings2 = embeds_out2, pre_trained, candidates = candidates, norm = 'l2'), simplify = FALSE)

    # pm_out1
    pm_sim_out1 <- lapply(permute_out, '[[', 'cos_sim1') %>% do.call(rbind,.)
    pm_comp1 <- apply(pm_sim_out1, 1, function(i) i >= sim_out1)
    pvalue1 <- apply(pm_comp1, 1, function(i) sum(i)/length(i))
    nns1 <- cbind(nns1, p.value = unname(pvalue1))

    # pm_out2
    pm_sim_out2 <- lapply(permute_out, '[[', 'cos_sim2') %>% do.call(rbind,.)
    pm_comp2 <- apply(pm_sim_out2, 1, function(i) i >= sim_out2)
    pvalue2 <- apply(pm_comp2, 1, function(i) sum(i)/length(i))
    nns2 <- cbind(nns2, p.value = unname(pvalue2))

    # pm_ratio
    pm_ratio_out <- lapply(permute_out, '[[', 'sim_ratio') %>% do.call(rbind,.)
    pm_dev1 <- abs(pm_ratio_out - 1)
    pm_dev1 <- apply(pm_dev1, 1, function(i) i >= dev1)
    pvalue_ratio <- apply(pm_dev1, 1, function(i) sum(i)/length(i))
    nns_ratio <- cbind(nns_ratio, p.value = unname(pvalue_ratio))
    cat('done with permutations \n')
  }

  # arrange
  nns1 <- nns1 %>% dplyr::arrange(-value)
  nns2 <- nns2 %>% dplyr::arrange(-value)
  nns_ratio <- nns_ratio %>% dplyr::arrange(-value)

  # subset if to union of top N nearest neighbors if specified
  if(is.numeric(N)){

    # subset nnss
    nns1 <- nns1 %>% dplyr::slice(1:N)
    nns2 <- nns2 %>% dplyr::slice(1:N)

    # subset nns_ratio
    nns_ratio <- nns_ratio %>% dplyr::filter(feature %in% union(nns1$feature,nns2$feature))

  }


  # output
  return(nns_ratio)
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
  pm_out <- compute_contrast(target_embeddings1_perm, target_embeddings2_perm, pre_trained = pre_trained, candidates = candidates, norm = 'l2')

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
  bs_out <- compute_contrast(bs_target_embeddings1, bs_target_embeddings2, pre_trained = pre_trained, candidates = candidates, norm = 'l2')

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
  if(length(candidates) > 0){
    cos_sim1 <- text2vec::sim2(alc_embedding1, pre_trained[candidates,], method = 'cosine', norm = 'l2')
    cos_sim2 <- text2vec::sim2(alc_embedding2, pre_trained[candidates,], method = 'cosine', norm = 'l2')}else{
      cos_sim1 <- text2vec::sim2(alc_embedding1, pre_trained, method = 'cosine', norm = 'l2')
      cos_sim2 <- text2vec::sim2(alc_embedding2, pre_trained, method = 'cosine', norm = 'l2')
    }

  # ratio of cosine similarities
  sim_ratio <- cos_sim1/cos_sim2

  # output
  return(list(cos_sim1 = cos_sim1, cos_sim2 = cos_sim2, sim_ratio = sim_ratio))

}
