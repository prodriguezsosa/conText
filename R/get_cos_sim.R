#' Given a coprus and a set of features, calculate cosine similarities over
#' a grouping variable.
#'
#' @param x a character vector - this is the set of documents (corpus) of interest
#' @param groups a grouping variable
#' @param features (character) features of interest
#' @inheritParams dem
#' @inheritParams dem_group
#' @param bootstrap (logical) if TRUE, bootstrap nns - sample from corpus with replacement;
#' if groups defined, sampling is automatically stratified; top nns are those with
#' the highest average over all bootstrap samples.
#' @param num_bootstraps (integer) number of bootstraps to use
#' @param what character; which quanteda tokenizer to use. You will rarely want to change this.
#' For Chinese texts you may want to set what = 'fastestword'.
#'
#' @return a `data.frame` with following columns:
#'  \item{`target`}{ (character) vector with the rownames of the dfm,
#'  either defining the groups or the target terms}.
#'  \item{`feature`}{(character) vector of feature terms, one
#'  instance for each target.}
#'  \item{`value`}{(numeric) cosine similarity between target
#'  and feature.}
#'
#' @export
#' @rdname get_cos_sim
#' @keywords get_cos_sim
#'
#' @examples
#'
#' library(quanteda)
#'
#' immig_corpus <- corpus_context(x = cr_sample_corpus,
#' pattern = "immigration", window = 6L, verbose = TRUE)
#'
#' temp1 <- get_cos_sim(x = immig_corpus,
#' groups = docvars(immig_corpus, 'party'),
#' features = c("reform", "enforce"),
#' pre_trained = glove_subset,
#' transform = TRUE,
#' transform_matrix = khodakA,
#' bootstrap = TRUE,
#' num_bootstraps = 10,
#' verbose = FALSE)
get_cos_sim <- function(x,
                        groups = NULL,
                        features = character(0),
                        pre_trained,
                        transform = TRUE,
                        transform_matrix,
                        bootstrap = TRUE,
                        num_bootstraps = 10,
                        what = 'word',
                        verbose = TRUE) {

  # create a new corpus
  x <- quanteda::corpus(as.character(x), docvars = data.frame('group' = groups))

  if(bootstrap){
    set.seed(42L)
    cossimdf_bs <- replicate(num_bootstraps,
                          cos_sim_boostrap(x = x,
                                           groups = quanteda::docvars(x, 'group'),
                                           features = features,
                                           pre_trained = pre_trained,
                                           transform = transform,
                                           transform_matrix = transform_matrix,
                                           what = what),
                          simplify = FALSE)
    result <- do.call(rbind, cossimdf_bs) %>%


      dplyr::group_by(target, feature) %>%
      dplyr::summarise(std.error = sd(value)/sqrt(dplyr::n()),
                       value = mean(value),
                       .groups = 'keep') %>%
      dplyr::ungroup() %>%
      dplyr::select('target', 'feature', 'value', 'std.error')
  }else{

    # create a new corpus
    x <- quanteda::corpus(as.character(x), docvars = data.frame('group' = groups))

    # tokenize texts
    corpus_toks <- quanteda::tokens(x, what = what)

    # create document-feature matrix
    corpus_dfm <- quanteda::dfm(corpus_toks, tolower = FALSE)

    # compute document-embedding matrix
    corpus_dem <- dem(x = corpus_dfm, pre_trained = pre_trained, transform_matrix = transform_matrix, transform = transform, verbose = verbose)

    # aggregate dems by group var
    if(!is.null(groups)) corpus_dem <- dem_group(x = corpus_dem, groups = corpus_dem@docvars$group)

    # compute cosine similarity
    result <- cos_sim(x = corpus_dem, pre_trained = pre_trained, features = features)
  }

  return(result)
}

# sub-function
cos_sim_boostrap <- function(x,
                             groups = NULL,
                             features = character(0),
                             pre_trained,
                             transform = TRUE,
                             transform_matrix,
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
  if(!is.null(groups)) corpus_dem <- dem_group(x = corpus_dem, groups = corpus_dem@docvars$group)

  # compute cosine similarity
  result <- cos_sim(x = corpus_dem, pre_trained = pre_trained, features = features)

  return(result)

}
