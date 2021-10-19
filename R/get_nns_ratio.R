#' Given a corpus and a binary grouping variable, computes the ratio of cosine similarities
#' over the union of their respective N nearest neighbors.
#'
#' @param x a (quanteda) tokens object
#' @inheritParams nns_ratio
#' @inheritParams dem
#' @inheritParams dem
#' @inheritParams dem_group
#' @param numerator (character) defines which group is the nuemerator in the ratio.
#' @param bootstrap (logical) if TRUE, bootstrap nns - sample from corpus with replacement;
#' if groups defined, sampling is automatically stratified; top nns are those with
#' the highest average over all bootstrap samples.
#' @param num_bootstraps (integer) number of bootstraps to use
#' @param permute (logical) if TRUE, compute empirical p-values using permutation test
#' @param num_permutations (numeric) number of permutations to use
#' @param verbose provide information on which group is the numerator
#'
#' @return a `data.frame` with following columns:
#'  \item{`feature`}{(character) vector of features from the candidate set,
#'  one instance for each target.}
#'  \item{`value`}{(numeric) ratio of cosine similarities
#'  (mean of boostraps if boostrap = TRUE).}
#'  \item{`std.error`}{(numeric) sd of bootstrapped ratio
#'  of cosine similarities if bootstrap = TRUE, if FALSE, column is dropped.}
#'  \item{`p.value`}{(numeric) empirical p-value of bootstrapped ratio
#'  of cosine similarities if permute = TRUE, if FALSE, column is dropped.}
#'
#' @export
#' @rdname get_nns_ratio
#' @keywords get_nns_ratio
#' @examples
#'
#' library(quanteda)
#'
#' # build corpus of contexts around immigration
#' immig_corpus <- corpus_context(x = cr_sample_corpus,
#' pattern = "immigration",
#' window = 6L,
#' verbose = TRUE)
#'
#' # identify features in local vocab that overlap with pre-trained embeddings
#' # these are used to define candidate nearest neighbors below (optional)
#' local_vocab <- get_local_vocab(as.character(immig_corpus), pre_trained = glove_subset)
#'
#' # tokenize
#' immig_toks <- tokens(immig_corpus)
#'
#' set.seed(42L)
#' temp <- get_nns_ratio(x = immig_toks,
#' N = 20,
#' groups = docvars(immig_toks, 'party'),
#' numerator = "R",
#' candidates = local_vocab,
#' pre_trained = glove_subset,
#' transform = TRUE,
#' transform_matrix = khodakA,
#' bootstrap = TRUE,
#' num_bootstraps = 10,
#' permute = TRUE,
#' num_permutations = 10,
#' verbose = TRUE)
get_nns_ratio <- function(x,
                          N = 10,
                          groups,
                          numerator = NULL,
                          candidates = character(0),
                          pre_trained,
                          transform = TRUE,
                          transform_matrix,
                          bootstrap = TRUE,
                          num_bootstraps = 10,
                          permute = TRUE,
                          num_permutations = 100,
                          verbose = TRUE){

  # checks
  group_vars <- unique(groups)
  if(is.null(group_vars) | length(group_vars)!=2) stop("a binary grouping variable must be provided")
  if(!is.null(numerator)){
    if(!(numerator %in% group_vars)) stop("numerator must refer to one of the two groups in the groups argument")
  }
  denominator <- setdiff(group_vars, numerator)

  # add grouping variable to docvars
  if(!is.null(groups)) docvars(x) <- NULL; docvars(x, "group") <- groups

  # create document-feature matrix
  x_dfm <- quanteda::dfm(x, tolower = FALSE)

  # compute document-embedding matrix
  x_dem <- dem(x = x_dfm, pre_trained = pre_trained, transform = transform, transform_matrix = transform_matrix, verbose = FALSE)

  # aggregate dems by group var
  wvs <- dem_group(x = x_dem, groups = x_dem@docvars$group)

  # get top N nns (if N is Inf or NULL, use all features)
  nnsdfs <- nns(x = wvs, N = Inf, candidates = candidates, pre_trained = pre_trained, as_list = TRUE)
  nnsdf1 <- if(is.null(N)) nnsdfs[[numerator]]$feature else nnsdfs[[numerator]]$feature[1:N]
  nnsdf2 <- if(is.null(N)) nnsdfs[[denominator]]$feature else nnsdfs[[denominator]]$feature[1:N]

  # get union of top N nns
  union_nns <- union(nnsdf1, nnsdf2)

  if(!bootstrap){

    # find nearest neighbors ratio
    result <- nns_ratio(x = wvs, N = N, numerator = numerator, candidates = union_nns, pre_trained = pre_trained)

  }else{

    cat('starting bootstraps \n')
    # bootstrap ratio
    nnsratiodf_bs <- replicate(num_bootstraps,
                               nns_ratio_boostrap(x = x,
                                       groups = groups,
                                       numerator = numerator,
                                       candidates = union_nns,
                                       pre_trained = pre_trained,
                                       transform = transform,
                                       transform_matrix = transform_matrix),
                          simplify = FALSE)
    result <- do.call(rbind, nnsratiodf_bs) %>%
      dplyr::group_by(feature) %>%
      dplyr::summarise(std.error = sd(value),
                value = mean(value),
                .groups = 'keep') %>%
      dplyr::ungroup() %>%
      dplyr::select('feature','value', 'std.error') %>%
      dplyr::arrange(-value)

    cat('done with bootstraps \n')

  }

  if(permute){


    # permute similarity
    cat('starting permutations \n')
    permute_out <- replicate(num_permutations, nns_ratio_permute(x,
                                                                 groups = groups,
                                                                 numerator = numerator,
                                                                 candidates = union_nns,
                                                                 pre_trained = pre_trained,
                                                                 transform = transform,
                                                                 transform_matrix = transform_matrix),
                             simplify = FALSE)

    # compute deviations of the observed ratios from 1
    dev1 <- result %>% dplyr::mutate(value = abs(value - 1)) %>% as.data.frame()
    dev1_perm <- lapply(permute_out, function(perm) perm[order(match(perm[,1],dev1[,1])),'value'])
    dev1_perm <- do.call(rbind, dev1_perm)
    dev1_perm <- abs(dev1_perm - 1)
    dev1_perm <- apply(dev1_perm, 1, function(i) i >= dev1$value)
    p.value <- apply(dev1_perm, 1, function(i) sum(i)/length(i))
    result <- result %>% dplyr::mutate(p.value = p.value)
    cat('done with permutations \n')
  }

  # add information on nns
  result <- result %>% dplyr::mutate(group = dplyr::case_when((feature %in% nnsdf1) & (feature %in% nnsdf2) ~ "shared",
                                                              (feature %in% nnsdf1) & !(feature %in% nnsdf2) ~ numerator,
                                                              !(feature %in% nnsdf1) & (feature %in% nnsdf2) ~ denominator))

  # add an attribute specifying which group is the numerator and communicated this to user
  attr(result, "numerator") <- numerator
  if(verbose) cat("NOTE: values refer to the ratio", paste0(numerator, "/", denominator, "."))

  return(result)
}

# sub-function
nns_ratio_boostrap <- function(x,
                         groups,
                         numerator = NULL,
                         candidates = character(0),
                         pre_trained = pre_trained,
                         transform = TRUE,
                         transform_matrix = transform_matrix){


  # sample tokens with replacement
  x <- quanteda::tokens_sample(x = x, size = table(groups), replace = TRUE, by = groups)

  # create document-feature matrix
  x_dfm <- quanteda::dfm(x, tolower = FALSE)

  # compute document-embedding matrix
  x_dem <- dem(x = x_dfm, pre_trained = pre_trained, transform = transform, transform_matrix = transform_matrix, verbose = FALSE)

  # aggregate dems by group
  wvs <- dem_group(x = x_dem, groups = x_dem@docvars$group)

  # find nearest neighbors
  result <- nns_ratio(x = wvs, N = NULL, numerator = numerator, candidates = candidates, pre_trained = pre_trained, verbose = FALSE)

  return(result)

}

# runs permutations
nns_ratio_permute <- function(x,
                              groups,
                              numerator = NULL,
                              candidates = character(0),
                              pre_trained,
                              transform = TRUE,
                              transform_matrix){

  # shuffle tokenized texts
  docvars(x, 'group') <- sample(groups)

  # create document-feature matrix
  x_dfm <- quanteda::dfm(x, tolower = FALSE)

  # compute document-embedding matrix
  x_dem <- dem(x = x_dfm, pre_trained = pre_trained, transform = transform, transform_matrix = transform_matrix, verbose = FALSE)

  # aggregate dems by group
  wvs <- dem_group(x = x_dem, groups = x_dem@docvars$group)

  # find nearest neighbors
  result <- nns_ratio(x = wvs, N = NULL, numerator = numerator, candidates = candidates, pre_trained = pre_trained, verbose = FALSE)

  return(result)
}

