#' Given a tokenized corpus, compute the cosine similarities
#' of the resulting ALC embeddings and a defined set of features.
#'
#' This is a wrapper function for `cos_sim()` that allows users to go from a
#' tokenized corpus to results with the option to bootstrap cosine similarities
#' and get the corresponding std. errors.
#'
#' @param x a (quanteda) `tokens-class` object
#' @param groups (numeric, factor, character) a binary variable of the same length as `x`
#' @param features (character) features of interest
#' @inheritParams dem
#' @inheritParams cos_sim
#' @inheritParams dem_group
#' @param bootstrap (logical) if TRUE, use bootstrapping -- sample from texts with replacement and
#' re-estimate cosine similarities for each sample. Required to get std. errors.
#' If `groups` defined, sampling is automatically stratified.
#' @param num_bootstraps (integer) number of bootstraps to use.
#' @param confidence_level (numeric in (0,1)) confidence level e.g. 0.95
#' @return a `data.frame` or list of data.frames (one for each target)
#' with the following columns:
#' \describe{
#'  \item{`target`}{ (character) rownames of `x`,
#'  the labels of the ALC embeddings.}
#'  \item{`feature`}{(character) feature terms defined in
#'  the `features` argument.}
#'  \item{`value`}{(numeric) cosine similarity between `x`
#'  and feature. Average over bootstrapped samples if bootstrap = TRUE.}
#'  \item{`std.error`}{(numeric) std. error of the similarity value.
#'  Column is dropped if bootstrap = FALSE.}
#'  \item{`lower.ci`}{(numeric) (if bootstrap = TRUE) lower bound of the confidence interval.}
#'  \item{`upper.ci`}{(numeric) (if bootstrap = TRUE) upper bound of the confidence interval.}
#'  }
#'
#' @export
#' @rdname get_cos_sim
#' @keywords get_cos_sim
#'
#' @examples
#'
#' library(quanteda)
#'
#' # tokenize corpus
#' toks <- tokens(cr_sample_corpus)
#'
#' # build a tokenized corpus of contexts sorrounding a target term
#' immig_toks <- tokens_context(x = toks, pattern = "immigration", window = 6L)
#'
#' # sample 100 instances of the target term, stratifying by party (only for example purposes)
#' set.seed(2022L)
#' immig_toks <- tokens_sample(immig_toks, size = 100, by = docvars(immig_toks, 'party'))
#'
#' # compute the cosine similarity between each group's embedding
#' # and a specific set of features
#' set.seed(2021L)
#' get_cos_sim(x = immig_toks,
#'             groups = docvars(immig_toks, 'party'),
#'             features = c("reform", "enforce"),
#'             pre_trained = cr_glove_subset,
#'             transform = TRUE,
#'             transform_matrix = cr_transform,
#'             bootstrap = TRUE,
#'             num_bootstraps = 100,
#'             confidence_level = 0.95,
#'             stem = TRUE,
#'             as_list = FALSE)
get_cos_sim <- function(x,
                        groups = NULL,
                        features = character(0),
                        pre_trained,
                        transform = TRUE,
                        transform_matrix,
                        bootstrap = TRUE,
                        num_bootstraps = 100,
                        confidence_level = 0.95,
                        stem = FALSE,
                        language = 'porter',
                        as_list = TRUE) {

  # initial checks
  if(bootstrap && (confidence_level >= 1 || confidence_level<=0)) stop('"confidence_level" must be a numeric value between 0 and 1.', call. = FALSE) # check confidence level is between 0 and 1
  if(bootstrap && num_bootstraps < 100) stop('num_bootstraps must be at least 100') # check num_bootstraps >= 100
  if(class(x)[1] != "tokens") stop("data must be of class tokens", call. = FALSE)

  # stemming check
  if(stem){
    if (requireNamespace("SnowballC", quietly = TRUE)) {
      cat('Using', language, 'for stemming. To check available languages run "SnowballC::getStemLanguages()"', '\n')
    } else stop('"SnowballC (>= 0.7.0)" package must be installed to use stemmming option.')
  }

  # add grouping variable to docvars
  if(!is.null(groups)) quanteda::docvars(x) <- NULL; quanteda::docvars(x, "group") <- groups

  # create document-feature matrix
  x_dfm <- quanteda::dfm(x, tolower = FALSE)

  # compute document-embedding matrix
  x_dem <- dem(x = x_dfm, pre_trained = pre_trained, transform = transform, transform_matrix = transform_matrix, verbose = FALSE)

  if(bootstrap){
    cat('starting bootstraps \n')
    cossimdf_bs <- replicate(num_bootstraps,
                          cos_sim_boostrap(x = x_dem,
                                           by = x_dem@docvars$group,
                                           features = features,
                                           pre_trained = pre_trained,
                                           stem = stem,
                                           language = language,
                                           as_list = FALSE,
                                           show_language = FALSE),
                          simplify = FALSE)
    result <- do.call(rbind, cossimdf_bs) %>%
      dplyr::group_by(target, feature) %>%
      dplyr::mutate(lower.ci = dplyr::nth(value, round((1-confidence_level)*num_bootstraps), order_by = value),
                    upper.ci = dplyr::nth(value, round(confidence_level*num_bootstraps), order_by = value)) %>%
      dplyr::summarise(std.error = sd(value),
                       value = mean(value),
                       lower.ci = mean(lower.ci),
                       upper.ci = mean(upper.ci),
                       .groups = 'keep') %>%
      dplyr::ungroup() %>%
      dplyr::select('target', 'feature', 'value', 'std.error', 'lower.ci', 'upper.ci')
    cat('done with bootstraps \n')
  }else{

    # aggregate dems by group var
    if(!is.null(groups)){
      wvs <- dem_group(x = x_dem, groups = x_dem@docvars$group)
    } else {
      wvs <- matrix(colMeans(x_dem), ncol = ncol(x_dem))
    }

    # compute cosine similarity
    result <- cos_sim(x = wvs, pre_trained = pre_trained, features = features, stem = stem, language = language, as_list = as_list, show_language = FALSE)
  }

  # if !as_list return a list object with an item for each target data.frame
  if(as_list){
    if(is.null(groups)) cat("NOTE: as_list cannot be TRUE if groups is NULL, proceeding with as_list = FALSE")
    else result <- lapply(unique(result$target), function(i) result[result$target == i,] %>% dplyr::mutate(target = as.character(target))) %>% setNames(unique(result$target))
  }
  return(result)
}

# sub-function
cos_sim_boostrap <- function(x,
                             by = NULL,
                             features = NULL,
                             pre_trained,
                             stem = FALSE,
                             language = 'porter',
                             as_list = FALSE,
                             show_language = FALSE){

  # sample dems with replacement
  x_sample_dem <- dem_sample(x = x, size = 1, replace = TRUE, by = by)

  # aggregate dems by group var if defined
  if(!is.null(by)){
    wvs <- dem_group(x = x_sample_dem, groups = x_sample_dem@docvars$group)
  } else {
    wvs <- matrix(colMeans(x_sample_dem), ncol = ncol(x_sample_dem))
  }

  # compute cosine similarity
  result <- cos_sim(x = wvs, pre_trained = pre_trained, features = features, stem = stem, language = language, as_list = as_list, show_language = show_language)

  return(result)

}
