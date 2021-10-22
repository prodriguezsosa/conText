#' Given a corpus and a set of candidate neighbors, find the top N nearest
#' neighbors.
#'
#' This is a wrapper function for `nns()` that allows users to go from a
#' tokenized corpus to results with the option to bootstrap cosine similarities
#' and get the corresponding std. errors.
#'
#' @param x a (quanteda) `tokens-class` object
#' @inheritParams nns
#' @param groups (numeric, factor, character) a binary variable of the same length as `x`
#' @inheritParams dem
#' @inheritParams dem_group
#' @param bootstrap (logical) if TRUE, use bootstrapping -- sample from texts with replacement and
#' re-estimate cosine similarities for each sample. Required to get std. errors.
#' If `groups` defined, sampling is automatically stratified.
#' @param num_bootstraps (integer) number of bootstraps to use.
#'
#' @return a `data.frame` or list of data.frames (one for each target)
#' with the following columns:
#' \describe{
#'  \item{`target`}{ (character) rownames of `x`,
#'  the labels of the ALC embeddings.}
#'  \item{`feature`}{(character) features identified as nearest neighbors.}
#'  \item{`rank`}{(character) rank of feature in terms of similarity with `x`.}
#'  \item{`value`}{(numeric) cosine similarity between `x`
#'  and feature. Average over bootstrapped samples if bootstrap = TRUE.}
#'  \item{`std.error`}{(numeric) std. error of the similarity value.
#'  Column is dropped if bootstrap = FALSE.}
#'  }
#'
#' @export
#' @rdname get_nns
#' @keywords get_nns
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
#' # compare nearest neighbors between groups
#' set.seed(2021L)
#' immig_party_nns <- get_nns(x = immig_toks, N = 10,
#'                            groups = docvars(immig_toks, 'party'),
#'                            candidates = feats,
#'                            pre_trained = cr_glove_subset,
#'                            transform = TRUE,
#'                            transform_matrix = cr_transform,
#'                            bootstrap = TRUE,
#'                            num_bootstraps = 10,
#'                            as_list = TRUE)
#'
#' # nearest neighbors of "immigration" for Republican party
#' immig_party_nns[["R"]]
get_nns <- function(x,
                    N = 10,
                    groups = NULL,
                    candidates = character(0),
                    pre_trained,
                    transform = TRUE,
                    transform_matrix,
                    bootstrap = TRUE,
                    num_bootstraps = 10,
                    as_list = TRUE) {

  # initial checks
  if(class(x)[1] != "tokens") stop("data must be of class tokens")

  # add grouping variable to docvars
  if(!is.null(groups)) quanteda::docvars(x) <- NULL; quanteda::docvars(x, "group") <- groups

  # subset candidates to features present in pre-trained embeddings provided
  if(length(candidates) > 0) candidates <- intersect(candidates, rownames(pre_trained))

  # if bootstrap
  if(bootstrap){
    nnsdf_bs <- replicate(num_bootstraps,
              nns_boostrap(x = x,
                           groups = groups,
                           candidates = candidates,
                           pre_trained = pre_trained,
                           transform = transform,
                           transform_matrix = transform_matrix,
                           as_list = FALSE),
              simplify = FALSE)
    result <- do.call(rbind, nnsdf_bs) %>%
      dplyr::group_by(target, feature) %>%
      dplyr::summarise(std.error = sd(value),
                value = mean(value),
                .groups = 'keep') %>%
      dplyr::group_by(target) %>%
      dplyr::slice_max(order_by = value, n = N) %>%
      dplyr::mutate(rank = 1:dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::select('target', 'feature', 'rank', 'value', 'std.error')
  }else{

  # create document-feature matrix
  x_dfm <- quanteda::dfm(x, tolower = FALSE)

  # compute document-embedding matrix
  x_dem <- dem(x = x_dfm, pre_trained = pre_trained, transform = transform, transform_matrix = transform_matrix, verbose = FALSE)

  # aggregate dems by group var
  if(!is.null(groups)){
    wvs <- dem_group(x = x_dem, groups = x_dem@docvars$group)
  } else {
    wvs <- matrix(colMeans(x_dem), ncol = ncol(x_dem))
  }

  # find nearest neighbors
  result <- nns(x = wvs, N = N, candidates = candidates, pre_trained = pre_trained, as_list = FALSE)
  }

  # if !as_list return a list object with an item for each target data.frame
  if(as_list) result <- lapply(unique(result$target), function(i) result[result$target == i,] %>% dplyr::mutate(target = as.character(target))) %>% setNames(unique(result$target))

  return(result)
}


# sub-function
nns_boostrap <- function(x,
                         groups = NULL,
                         candidates = character(0),
                         pre_trained,
                         transform = TRUE,
                         transform_matrix,
                         as_list = FALSE){

  # sample tokens with replacement
  if(!is.null(groups)) {
    x <- quanteda::tokens_sample(x = x, size = table(groups), replace = TRUE, by = groups)
    } else {
      x <- quanteda::tokens_sample(x = x, size = quanteda::ndoc(x), replace = TRUE)
  }

  # create document-feature matrix
  x_dfm <- quanteda::dfm(x, tolower = FALSE)

  # compute document-embedding matrix
  x_dem <- dem(x = x_dfm, pre_trained = pre_trained, transform = transform, transform_matrix = transform_matrix, verbose = FALSE)

  # aggregate dems by group var if defined
  if(!is.null(groups)){
    wvs <- dem_group(x = x_dem, groups = x_dem@docvars$group)
  } else {
    wvs <- matrix(colMeans(x_dem), ncol = ncol(x_dem))
    }

  # find nearest neighbors
  result <- nns(x = wvs, N = Inf, candidates = candidates, pre_trained = pre_trained, as_list = FALSE)

  return(result)

}

