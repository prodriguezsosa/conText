#' Given a corpus and a set of features, calculate cosine similarities over
#' a grouping variable.
#'
#' @param x a (quanteda) tokens object
#' @param groups a grouping variable
#' @param features (character) features of interest
#' @inheritParams dem
#' @inheritParams cos_sim
#' @inheritParams dem_group
#' @param bootstrap (logical) if TRUE, bootstrap nns - sample from corpus with replacement;
#' if groups defined, sampling is automatically stratified; top nns are those with
#' the highest average over all bootstrap samples.
#' @param num_bootstraps (integer) number of bootstraps to use
#'
#' @return a `data.frame` or list of data.frames (one for each target)
#' with the following columns:
#'  \item{`target`}{ (character) vector with the rownames of the dfm,
#'  either defining the groups or the target terms}.
#'  \item{`feature`}{(character) vector of feature terms, one
#'  instance for each target.}
#'  \item{`value`}{(numeric) cosine similarity between target
#'  and feature.}
#'  \item{`std.error`}{(numeric) sd of bootstrapped cosine similarities
#'  if bootstrap = TRUE, if FALSE, column is dropped.}
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
#' cr_toks <- tokens(cr_sample_corpus)
#'
#' # get tokens around immigration
#' immig_toks <- tokens_context(x = cr_toks,
#' pattern = "immigration", window = 6L, hard_cut = FALSE, verbose = TRUE)
#'
#' get_cos_sim(x = immig_toks,
#' groups = docvars(immig_toks, "party"),
#' features = c("reform", "enforce"),
#' pre_trained = cr_glove_subset,
#' transform = TRUE,
#' transform_matrix = cr_transform,
#' bootstrap = TRUE,
#' num_bootstraps = 10,
#' as_list = FALSE,
#' verbose = FALSE)
get_cos_sim <- function(x,
                        groups = NULL,
                        features = character(0),
                        pre_trained,
                        transform = TRUE,
                        transform_matrix,
                        bootstrap = TRUE,
                        num_bootstraps = 10,
                        as_list = TRUE,
                        verbose = TRUE) {

  # initial checks
  if(class(x)[1] != "tokens") stop("data must be of class tokens")

  # add grouping variable to docvars
  if(!is.null(groups)) quanteda::docvars(x) <- NULL; quanteda::docvars(x, "group") <- groups

  if(bootstrap){
    cossimdf_bs <- replicate(num_bootstraps,
                          cos_sim_boostrap(x = x,
                                           groups = groups,
                                           features = features,
                                           pre_trained = pre_trained,
                                           transform = transform,
                                           transform_matrix = transform_matrix,
                                           as_list = FALSE),
                          simplify = FALSE)
    result <- do.call(rbind, cossimdf_bs) %>%
      dplyr::group_by(target, feature) %>%
      dplyr::summarise(std.error = sd(value),
                       value = mean(value),
                       .groups = 'keep') %>%
      dplyr::ungroup() %>%
      dplyr::select('target', 'feature', 'value', 'std.error')
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

    # compute cosine similarity
    result <- cos_sim(x = wvs, pre_trained = pre_trained, features = features)
  }

  # if !as_list return a list object with an item for each feature data.frame
  if(as_list) result <- lapply(unique(result$feature), function(i) result[result$feature == i,] %>% dplyr::mutate(feature = as.character(feature))) %>% setNames(unique(result$feature))

  return(result)
}

# sub-function
cos_sim_boostrap <- function(x,
                             groups = NULL,
                             features = character(0),
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

  # compute cosine similarity
  result <- cos_sim(x = wvs, pre_trained = pre_trained, features = features, as_list = FALSE)

  return(result)

}
