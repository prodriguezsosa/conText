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
#' @param confidence_level (numeric in (0,1)) confidence level e.g. 0.95
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
#'  \item{`lower.ci`}{(numeric) (if bootstrap = TRUE) lower bound of the confidence interval.}
#'  \item{`upper.ci`}{(numeric) (if bootstrap = TRUE) upper bound of the confidence interval.}
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
#' immig_toks <- tokens_context(x = toks, pattern = "immigration",
#' window = 6L, rm_keyword = FALSE)
#'
#' # sample 100 instances of the target term, stratifying by party (only for example purposes)
#' set.seed(2022L)
#' immig_toks <- tokens_sample(immig_toks, size = 100, by = docvars(immig_toks, 'party'))
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
#'                            num_bootstraps = 100,
#'                            confidence_level = 0.95,
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
                    num_bootstraps = 100,
                    confidence_level = 0.95,
                    as_list = TRUE) {

  # initial checks
  if(bootstrap && (confidence_level >= 1 || confidence_level<=0)) stop('"confidence_level" must be a numeric value between 0 and 1.', call. = FALSE) # check confidence level is between 0 and 1
  if(bootstrap && num_bootstraps < 100) warning('num_bootstraps must be at least 100') # check num_bootstraps >= 100
  if(class(x)[1] != "tokens") stop("data must be of class tokens", call. = FALSE)

  # add grouping variable to docvars
  if(!is.null(groups)) quanteda::docvars(x) <- NULL; quanteda::docvars(x, "group") <- groups

  # create document-feature matrix
  x_dfm <- quanteda::dfm(x, tolower = FALSE)

  # compute document-embedding matrix
  x_dem <- dem(x = x_dfm, pre_trained = pre_trained, transform = transform, transform_matrix = transform_matrix, verbose = FALSE)
  x_contexts <- tokens_subset(x, docid(x) %in% x_dem@Dimnames$docs)

  # if bootstrap
  if(bootstrap){
    cat('starting bootstraps \n')
    ncsdf_bs <- replicate(num_bootstraps,
                          ncs_bootstrap(x = x_dem,
                                        x_contexts = x_contexts,
                                        by = groups,
                                        as_list = FALSE),
                          simplify = FALSE)
    result <- do.call(rbind, ncsdf_bs) %>%
      dplyr::group_by(target, context) %>%
      dplyr::mutate(lower.ci = dplyr::nth(value, round((1-confidence_level)*num_bootstraps), order_by = value),
                    upper.ci = dplyr::nth(value, round(confidence_level*num_bootstraps), order_by = value)) %>%
      dplyr::summarise(std.error = sd(value),
                       value = mean(value),
                       lower.ci = mean(lower.ci),
                       upper.ci = mean(upper.ci),
                       .groups = 'keep') %>%
      dplyr::group_by(target) %>%
      dplyr::slice_max(order_by = value, n = N) %>%
      dplyr::mutate(rank = 1:dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::select('target', 'context', 'rank', 'value', 'std.error', 'lower.ci', 'upper.ci')

    cat('done with bootstraps \n')
  }else{

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
  if(as_list){
    if(is.null(groups)) cat("NOTE: as_list cannot be TRUE if groups is NULL, proceeding with as_list = FALSE")
    else result <- lapply(unique(result$target), function(i) result[result$target == i,] %>% dplyr::mutate(target = as.character(target))) %>% setNames(unique(result$target))
  }
  return(result)
}

# sub-function
ncs_bootstrap <- function(x = NULL,
                          x_contexts = NULL,
                          by = NULL,
                          as_list = FALSE){

  # sample dems with replacement
  x_sample_dem <- dem_sample(x = x, size = 1, replace = TRUE, by = by)

  # aggregate dems by group var if defined
  if(!is.null(by)){
    wvs <- dem_group(x = x_sample_dem, groups = x_sample_dem@docvars$group)
  } else {
    wvs <- matrix(colMeans(x_sample_dem), ncol = ncol(x_sample_dem))
  }

  # find nearest contexts
  result <- ncs(x = wvs, contexts_dem = x, contexts = x_contexts, N = Inf, as_list = as_list)

  return(result)

}

