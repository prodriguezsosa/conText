#' Given a corpus and a binary grouping variable, computes the ratio of cosine similarities
#' over the union of their respective N nearest neighbors.
#'
#' @param x a (quanteda) corpus or character vector
#' @inheritParams nns_ratio
#' @inheritParams dem
#' @inheritParams dem
#' @inheritParams dem_group
#' @param numerator (character)
#' @param bootstrap (logical) if TRUE, bootstrap nns - sample from corpus with replacement;
#' if groups defined, sampling is automatically stratified; top nns are those with
#' the highest average over all bootstrap samples.
#' @param num_bootstraps (integer) number of bootstraps to use
#' @param permute (logical) if TRUE, compute empirical p-values using permutation test
#' @param num_permutations (numeric) number of permutations to use
#' @param what character; which quanteda tokenizer to use. You will rarely want to change this.
#' For Chinese texts you may want to set what = 'fastestword'.
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
#' local_vocab <- get_local_vocab(as.character(immig_corpus), pre_trained = glove_subset)
#'
#' set.seed(42L)
#' get_nns_ratio(x = immig_corpus,
#' N = 20,
#' groups = docvars(immig_corpus, 'party'),
#' numerator = "R",
#' candidates = local_vocab,
#' pre_trained = glove_subset,
#' transform = TRUE,
#' transform_matrix = khodakA,
#' bootstrap = TRUE,
#' num_bootstraps = 10,
#' permute = TRUE,
#' num_permutations = 10)
#'
get_nns_ratio <- function(x,
                          N = 10,
                          groups = NULL,
                          numerator = NULL,
                          candidates = character(0),
                          pre_trained,
                          transform = TRUE,
                          transform_matrix,
                          bootstrap = TRUE,
                          num_bootstraps = 10,
                          permute = TRUE,
                          num_permutations = 100,
                          what = 'word'){

  # checks
  group_order <- unique(groups)
  if(is.null(group_order)) stop("a binary grouping variable must be provided")
  if(length(group_order)!=2) stop("groups must be binary")
  if(!is.null(numerator)){
    if(!(numerator %in% group_order)){
    stop("numerator must refer to one of the two groups in the groups argument")}else{
    group_order <- c(numerator, setdiff(group_order, numerator))}
  }

  # create a new corpus
  x <- quanteda::corpus(as.character(x), docvars = data.frame('group' = groups))

  # tokenize texts
  corpus_toks <- quanteda::tokens(x, what = what)

  # create document-feature matrix
  corpus_dfm <- quanteda::dfm(corpus_toks, tolower = FALSE)

  # compute document-embedding matrix
  corpus_dem <- dem(x = corpus_dfm, pre_trained = pre_trained, transform = transform, transform_matrix = transform_matrix, verbose = FALSE)

  # aggregate dems by group var
  corpus_dem <- dem_group(x = corpus_dem, groups = corpus_dem@docvars$group)
  corpus_dem <- corpus_dem[group_order,] # re-arrange groups

  # get top N nns (if N is Inf or NULL, use all features)
  nnsdfs <- nns(x = corpus_dem, N = Inf, candidates = candidates, pre_trained = pre_trained, as_list = TRUE)
  nnsdf1 <- if(is.null(N)) nnsdfs[[1]]$feature else nnsdfs[[1]]$feature[1:N]
  nnsdf2 <- if(is.null(N)) nnsdfs[[2]]$feature else nnsdfs[[2]]$feature[1:N]

  # get union of top N nns
  union_nns <- union(nnsdf1, nnsdf2)

  if(!bootstrap){

    # find nearest neighbors ratio
    result <- nns_ratio(x = corpus_dem, N = N, candidates = union_nns, pre_trained = pre_trained)

  }else{

    cat('starting bootstraps \n')
    # bootstrap ratio
    nnsratiodf_bs <- replicate(num_bootstraps,
                               nns_ratio_boostrap(x = x,
                                       groups = quanteda::docvars(x, 'group'),
                                       group_order = group_order,
                                       candidates = union_nns,
                                       pre_trained = pre_trained,
                                       transform = transform,
                                       transform_matrix = transform_matrix,
                                       what = what),
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
                                                                 groups = quanteda::docvars(x, 'group'),
                                                                 group_order = group_order,
                                                                 candidates = union_nns,
                                                                 pre_trained = pre_trained,
                                                                 transform = transform,
                                                                 transform_matrix = transform_matrix,
                                                                 what = what),
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
                                                              (feature %in% nnsdf1) & !(feature %in% nnsdf2) ~ group_order[1],
                                                              !(feature %in% nnsdf1) & (feature %in% nnsdf2) ~ group_order[2]))

  # add an attribute specifying which group is the numerator and communicated this to user
  attr(result, "numerator") <- group_order[1]
  cat("NOTE: values refer to the ratio", paste0(group_order[1], "/", group_order[2], "."))

  return(result)
}


# sub-function
nns_ratio_boostrap <- function(x,
                         groups,
                         group_order,
                         candidates = character(0),
                         pre_trained = pre_trained,
                         transform = TRUE,
                         transform_matrix = transform_matrix,
                         what = what){

  # create a new corpus
  x <- quanteda::corpus_sample(x, size = quanteda::ndoc(x), replace = TRUE, by = groups)

  # tokenize texts
  corpus_toks <- quanteda::tokens(x, what = what)

  # create document-feature matrix
  corpus_dfm <- quanteda::dfm(corpus_toks, tolower = FALSE)

  # compute document-embedding matrix
  corpus_dem <- dem(x = corpus_dfm, pre_trained = pre_trained, transform = transform, transform_matrix = transform_matrix, verbose = FALSE)

  # aggregate dems by group var
  corpus_dem <- dem_group(x = corpus_dem, groups = corpus_dem@docvars$group)
  corpus_dem <- corpus_dem[group_order,] # re-arrange groups

  # find nearest neighbors
  result <- nns_ratio(x = corpus_dem, N = NULL, candidates = candidates, pre_trained = pre_trained)

  return(result)

}

# runs permutations
nns_ratio_permute <- function(x,
                              groups,
                              group_order,
                              candidates = character(0),
                              pre_trained,
                              transform = TRUE,
                              transform_matrix,
                              what = 'word'){

  # shuffle the texts
  x <- quanteda::corpus(sample(as.character(x)), docvars = data.frame('group' = groups))

  # tokenize texts
  corpus_toks <- quanteda::tokens(x, what = what)

  # create document-feature matrix
  corpus_dfm <- quanteda::dfm(corpus_toks, tolower = FALSE)

  # compute document-embedding matrix
  corpus_dem <- dem(x = corpus_dfm, pre_trained = pre_trained, transform = transform, transform_matrix = transform_matrix, verbose = FALSE)

  # aggregate dems by group var
  corpus_dem <- dem_group(x = corpus_dem, groups = corpus_dem@docvars$group)
  corpus_dem <- corpus_dem[group_order,] # re-arrange groups

  # find nearest neighbors
  result <- nns_ratio(x = corpus_dem, N = NULL, candidates = candidates, pre_trained = pre_trained)

  return(result)
}

