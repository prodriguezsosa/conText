#' Given a corpus and a binary grouping variable, computes the ratio of cosine similarities
#' over the union of their respective N nearest neighbors.
#'
#' This is a wrapper function for `nns_ratio()` that allows users to go from a
#' tokenized corpus to results with the option to: (1) bootstrap cosine similarity ratios
#' and get the corresponding std. errors. (2) use a permutation test to get empirical
#' p-values for inference.
#'
#' @param x a (quanteda) tokens object
#' @inheritParams nns_ratio
#' @inheritParams dem
#' @inheritParams dem
#' @inheritParams dem_group
#' @param numerator (character) defines which group is the nuemerator in the ratio.
#' @param bootstrap (logical) if TRUE, use bootstrapping -- sample from texts with replacement and
#' re-estimate cosine similarity ratios for each sample. Required to get std. errors.
#' If `groups` defined, sampling is automatically stratified.
#' @param num_bootstraps (integer) number of bootstraps to use.
#' @param permute (logical) if TRUE, compute empirical p-values using permutation test
#' @param num_permutations (numeric) number of permutations to use.
#' @param stem (logical) - whether to stem candidates when evaluating nns. Default is FALSE.
#' If TRUE, candidate stems are ranked by their average cosine similarity to the target.
#' We recommend you remove misspelled words from candidate set `candidates` as these can
#' significantly influence the average.
#' @param verbose provide information on which group is the numerator
#'
#' @return a `data.frame` with following columns:
#' \describe{
#'  \item{`feature`}{(character) features in `candidates`
#'  (or all features if `candidates` not defined), one instance for each embedding in `x`.}
#'  \item{`value`}{(numeric) cosine similarity ratio between `x`
#'  and feature. Average over bootstrapped samples if bootstrap = TRUE.}
#'  \item{`std.error`}{(numeric) std. error of the similarity value.
#'  Column is dropped if bootstrap = FALSE.}
#'  \item{`p.value`}{(numeric) empirical p-value of bootstrapped ratio
#'  of cosine similarities if permute = TRUE, if FALSE, column is dropped.}
#'  \item{`group`}{(character) group in `groups` for which feature belongs
#'  to the top N nearest neighbors. If "shared", the feature appeared as
#'  top nearest neighbor for both groups.}
#'  }
#'
#' @export
#' @rdname get_nns_ratio
#' @keywords get_nns_ratio
#' @examples
#'
#' library(quanteda)
#'
#' # tokenize corpus
#' toks <- tokens(cr_sample_corpus)
#'
#' # build a tokenized corpus of contexts sorrounding a target term
#' immig_toks <- tokens_context(x = toks, pattern = "immigr*", window = 6L)
#'
#' # we limit candidates to features in our corpus
#' feats <- featnames(dfm(immig_toks))
#'
#' # compute ratio
#' set.seed(2021L)
#' immig_nns_ratio <- get_nns_ratio(x = immig_toks,
#'                                  N = 10,
#'                                  groups = docvars(immig_toks, 'party'),
#'                                  numerator = "R",
#'                                  candidates = feats,
#'                                  pre_trained = cr_glove_subset,
#'                                  transform = TRUE,
#'                                  transform_matrix = cr_transform,
#'                                  bootstrap = TRUE,
#'                                  num_bootstraps = 5,
#'                                  permute = TRUE,
#'                                  num_permutations = 5,
#'                                  verbose = FALSE)
#'
#' head(immig_nns_ratio)
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
                          stem = FALSE,
                          verbose = TRUE){

  # initial checks
  if(class(x)[1] != "tokens") stop("data must be of class tokens")

  # checks
  group_vars <- unique(groups)
  if(is.null(group_vars) | length(group_vars)!=2) stop("a binary grouping variable must be provided")
  if(!is.null(numerator)){
    if(!(numerator %in% group_vars)) stop("numerator must refer to one of the two groups in the groups argument")
  }
  denominator <- setdiff(group_vars, numerator)

  # add grouping variable to docvars
  if(!is.null(groups)) quanteda::docvars(x) <- NULL; quanteda::docvars(x, "group") <- groups

  # create document-feature matrix
  x_dfm <- quanteda::dfm(x, tolower = FALSE)

  # compute document-embedding matrix
  x_dem <- dem(x = x_dfm, pre_trained = pre_trained, transform = transform, transform_matrix = transform_matrix, verbose = FALSE)

  # aggregate dems by group var
  wvs <- dem_group(x = x_dem, groups = x_dem@docvars$group)

  # subset candidates to features present in pre-trained embeddings provided
  if(length(candidates) > 0) candidates <- intersect(candidates, rownames(pre_trained))

  # get top N nns (if N is Inf or NULL, use all features)
  nnsdfs <- nns(x = wvs, N = Inf, candidates = candidates, pre_trained = pre_trained, stem = stem, as_list = TRUE)
  nnsdf1 <- if(is.null(N)) nnsdfs[[numerator]]$feature else nnsdfs[[numerator]]$feature[1:N]
  nnsdf2 <- if(is.null(N)) nnsdfs[[denominator]]$feature else nnsdfs[[denominator]]$feature[1:N]

  # get union of top N nns
  union_nns <- union(nnsdf1, nnsdf2)

  if(!bootstrap){

    # find nearest neighbors ratio
    result <- nns_ratio(x = wvs, N = N, numerator = numerator, candidates = candidates, pre_trained = pre_trained, stem = stem) %>% dplyr::filter(feature %in% union_nns)

  }else{

    cat('starting bootstraps \n')
    # bootstrap ratio
    nnsratiodf_bs <- replicate(num_bootstraps,
                               nns_ratio_boostrap(x = x,
                                       groups = groups,
                                       numerator = numerator,
                                       candidates = candidates,
                                       pre_trained = pre_trained,
                                       transform = transform,
                                       transform_matrix = transform_matrix,
                                       stem = stem),
                          simplify = FALSE)
    result <- do.call(rbind, nnsratiodf_bs) %>%
      dplyr::group_by(feature) %>%
      dplyr::summarise(std.error = sd(value),
                value = mean(value),
                .groups = 'keep') %>%
      dplyr::ungroup() %>%
      dplyr::select('feature','value', 'std.error') %>%
      dplyr::filter(feature %in% union_nns) %>%
      dplyr::arrange(-value)

    cat('done with bootstraps \n')

  }

  if(permute){


    # permute similarity
    cat('starting permutations \n')
    permute_out <- replicate(num_permutations, nns_ratio_permute(x,
                                                                 groups = groups,
                                                                 numerator = numerator,
                                                                 candidates = candidates,
                                                                 pre_trained = pre_trained,
                                                                 transform = transform,
                                                                 transform_matrix = transform_matrix,
                                                                 stem = stem),
                             simplify = FALSE)

    # compute deviations of the observed ratios from 1
    dev1 <- result %>% dplyr::mutate(value = abs(value - 1)) %>% as.data.frame()
    permute_out <- lapply(permute_out, function(perm) perm %>% dplyr::filter(feature %in% union_nns))
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
  if(verbose) cat("NOTE: values refer to the ratio", paste0(numerator, "/", denominator, "."), "\n")

  return(result)
}

# sub-function
nns_ratio_boostrap <- function(x,
                         groups,
                         numerator = NULL,
                         candidates = character(0),
                         pre_trained = pre_trained,
                         transform = TRUE,
                         transform_matrix = transform_matrix,
                         stem = stem){


  # sample tokens with replacement
  x <- quanteda::tokens_sample(x = x, size = table(groups), replace = TRUE, by = groups)

  # create document-feature matrix
  x_dfm <- quanteda::dfm(x, tolower = FALSE)

  # compute document-embedding matrix
  x_dem <- dem(x = x_dfm, pre_trained = pre_trained, transform = transform, transform_matrix = transform_matrix, verbose = FALSE)

  # aggregate dems by group
  wvs <- dem_group(x = x_dem, groups = x_dem@docvars$group)

  # find nearest neighbors
  result <- nns_ratio(x = wvs, N = NULL, numerator = numerator, candidates = candidates, pre_trained = pre_trained, stem = stem, verbose = FALSE)

  return(result)

}

# runs permutations
nns_ratio_permute <- function(x,
                              groups,
                              numerator = NULL,
                              candidates = character(0),
                              pre_trained,
                              transform = TRUE,
                              transform_matrix,
                              stem = stem){

  # shuffle tokenized texts
  quanteda::docvars(x, 'group') <- sample(groups)

  # create document-feature matrix
  x_dfm <- quanteda::dfm(x, tolower = FALSE)

  # compute document-embedding matrix
  x_dem <- dem(x = x_dfm, pre_trained = pre_trained, transform = transform, transform_matrix = transform_matrix, verbose = FALSE)

  # aggregate dems by group
  wvs <- dem_group(x = x_dem, groups = x_dem@docvars$group)

  # find nearest neighbors
  result <- nns_ratio(x = wvs, N = NULL, numerator = numerator, candidates = candidates, pre_trained = pre_trained, stem = stem, verbose = FALSE)

  return(result)
}

