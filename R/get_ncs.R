#' Given a set of tokenized contexts, find the top N nearest
#' contexts.
#'
#' This is a wrapper function for `ncs()` that allows users to go from a
#' tokenized corpus to results with the option to bootstrap cosine similarities
#' and get the corresponding std. errors.
#'
#' @param x a (quanteda) `tokens-class` object
#' @inheritParams ncs
#' @inheritParams dem
#' @inheritParams dem_group
#' @param bootstrap (logical) if TRUE, use bootstrapping -- sample from `x` with replacement and
#' re-estimate cosine similarities for each sample. Required to get std. errors.
#' If `groups` defined, sampling is automatically stratified.
#' @param num_bootstraps (integer) number of bootstraps to use.
#'
#' @return a `data.frame` or list of data.frames (one for each target)
#' with the following columns:
#' \describe{
#'  \item{`target`}{ (character) rownames of `x`,
#'  the labels of the ALC embeddings. `NA` if `is.null(rownames(x))`.}
#'  \item{`context`}{(character) contexts collapsed into single documents (i.e. untokenized).}
#'  \item{`rank`}{(character) rank of context in terms of similarity with `x`.}
#'  \item{`value`}{(numeric) cosine similarity between `x` and context.}
#'  \item{`std.error`}{(numeric) std. error of the similarity value.
#'  Column is dropped if bootstrap = FALSE.}
#'  }
#'
#' @export
#' @rdname get_ncs
#' @keywords get_ncs
#' @examples
#'
#' library(quanteda)
#'
#' # tokenize corpus
#' toks <- tokens(cr_sample_corpus)
#'
#' # build a tokenized corpus of contexts sorrounding a target term
#' immig_toks <- tokens_context(x = toks, pattern = "immigr*",
#' window = 6L, rm_keyword = FALSE)
#'
#' # compare nearest contexts between groups
#' set.seed(2021L)
#' immig_party_ncs <- get_ncs(x = immig_toks,
#'                            N = 10,
#'                            groups = docvars(immig_toks, 'party'),
#'                            pre_trained = cr_glove_subset,
#'                            transform = TRUE,
#'                            transform_matrix = cr_transform,
#'                            bootstrap = TRUE,
#'                            num_bootstraps = 10,
#'                            as_list = TRUE)
#'
#' # nearest neighbors of "immigration" for Republican party
#' immig_party_ncs[["D"]]
get_ncs <- function(x,
                    N = 5,
                    groups = NULL,
                    pre_trained,
                    transform = TRUE,
                    transform_matrix,
                    bootstrap = TRUE,
                    num_bootstraps = 10,
                    as_list = TRUE) {

  # initial checks
  if(class(x)[1] != "tokens") stop("data must be of class tokens", call. = FALSE)

  # add grouping variable to docvars
  if(!is.null(groups)) quanteda::docvars(x) <- NULL; quanteda::docvars(x, "group") <- groups

  # if bootstrap
  if(bootstrap){
    ncsdf_bs <- replicate(num_bootstraps,
                          ncs_bootstrap(x = x,
                                        groups = groups,
                                        pre_trained = pre_trained,
                                        transform = transform,
                                        transform_matrix = transform_matrix,
                                        as_list = FALSE),
                          simplify = FALSE)
    result <- do.call(rbind, ncsdf_bs) %>%
      dplyr::group_by(target, context) %>%
      dplyr::summarise(std.error = sd(value),
                       value = mean(value),
                       .groups = 'keep') %>%
      dplyr::group_by(target) %>%
      dplyr::slice_max(order_by = value, n = N) %>%
      dplyr::mutate(rank = 1:dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::select('target', 'context', 'rank', 'value', 'std.error')
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

    # find nearest contexts
    result <- ncs(x = wvs, contexts_dem = contexts_dem, contexts = x, N = N, as_list = as_list)

  }

  # if !as_list return a list object with an item for each target data.frame
  if(as_list) result <- lapply(unique(result$target), function(i) result[result$target == i,] %>% dplyr::mutate(target = as.character(target))) %>% setNames(unique(result$target))

  return(result)
}

# sub-function
ncs_bootstrap <- function(x,
                          groups = NULL,
                          pre_trained,
                          transform = TRUE,
                          transform_matrix,
                          as_list = FALSE){

  # sample tokens with replacement
  if(!is.null(groups)) {
    x_sample <- quanteda::tokens_sample(x = x, size = table(groups), replace = TRUE, by = groups)
  } else {
    x_sample <- quanteda::tokens_sample(x = x, size = quanteda::ndoc(x), replace = TRUE)
  }

  # create document-feature matrix
  x_sample_dfm <- quanteda::dfm(x_sample, tolower = FALSE)

  # compute document-embedding matrix
  x_sample_dem <- dem(x = x_sample_dfm, pre_trained = pre_trained, transform = transform, transform_matrix = transform_matrix, verbose = FALSE)

  # aggregate dems by group var if defined
  if(!is.null(groups)){
    wvs <- dem_group(x = x_sample_dem, groups = x_sample_dem@docvars$group)
  } else {
    wvs <- matrix(colMeans(x_sample_dem), ncol = ncol(x_sample_dem))
  }

  # find nearest contexts
  result <- ncs(x = wvs, contexts_dem = x_sample_dem, contexts = x, N = Inf, as_list = FALSE)

  return(result)

}

