#' Given a corpus and a set of candidate neighbors, find the top N nearest
#' neighbors.
#'
#' @param x a character vector - this is the set of documents (corpus) of interest
#' @inheritParams nns
#' @param groups grouping variable equal in length to the number of documents
#' @inheritParams dem
#' @inheritParams dem_group
#' @param bootstrap (logical) if TRUE, bootstrap nns - sample from corpus with replacement;
#' if groups defined, sampling is automatically stratified; top nns are those with
#' the highest average over all bootstrap samples.
#' @param num_bootstraps (integer) number of bootstraps to use
#' @param what character; which quanteda tokenizer to use. You will rarely want to change this.
#' For Chinese texts you may want to set what = 'fastestword'.
#'
#' @return a `data.frame` or list of data.frames (one for each target)
#' with the following columns:
#'  \item{`target`}{ (character) vector with the rownames of the dfm,
#'  either defining the groups or the target terms}.
#'  \item{`feature`}{(character) vector of features from the candidate set,
#'  one instance for each target.}
#'  \item{`value`}{(numeric) cosine similarity between target
#'  and candidate (mean of boostraps if boostrap = TRUE).}
#'  \item{`std.error`}{(numeric) sd/srt(num_bootstrap) of bootstrapped
#'  cosine similarities if bootstrap = TRUE, if FALSE, column is dropped.}
#'
#' @export
#' @rdname get_nns
#' @keywords get_nns
#' @examples
#'
#' library(quanteda)
#'
#' immig_corpus <- corpus_context(x = cr_sample_corpus,
#' pattern = "immigration", window = 6L, verbose = TRUE)
#'
#' get_nns(x = immig_corpus, N = 10,
#' groups = docvars(immig_corpus, 'party'),
#' candidates = character(0),
#' pre_trained = glove_subset,
#' transform = TRUE,
#' transform_matrix = khodakA,
#' bootstrap = TRUE,
#' num_bootstraps = 10,
#' as_list = FALSE)
get_nns <- function(x,
                    N = 10,
                    groups = NULL,
                    candidates = character(0),
                    pre_trained,
                    transform = TRUE,
                    transform_matrix,
                    bootstrap = TRUE,
                    num_bootstraps = 10,
                    what = 'word',
                    as_list = TRUE) {

  # create a new corpus
  x <- quanteda::corpus(as.character(x), docvars = data.frame('group' = groups))

  if(bootstrap){
    set.seed(42L)
    nnsdf_bs <- replicate(num_bootstraps,
              nns_boostrap(x = x,
                           groups = quanteda::docvars(x, 'group'),
                           candidates = candidates,
                           pre_trained = pre_trained,
                           transform = transform,
                           transform_matrix = transform_matrix,
                           what = what,
                           as_list = FALSE),
              simplify = FALSE)
    result <- do.call(rbind, nnsdf_bs) %>%
      dplyr::group_by(target, feature) %>%
      dplyr::summarise(std.error = sd(value)/sqrt(dplyr::n()),
                value = mean(value),
                .groups = 'keep') %>%
      dplyr::group_by(target) %>%
      dplyr::slice_max(order_by = value, n = N) %>%
      dplyr::mutate(rank = 1:dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::select('target', 'feature', 'rank', 'value', 'std.error')
  }else{

  # tokenize texts
  corpus_toks <- quanteda::tokens(x, what = what)

  # create document-feature matrix
  corpus_dfm <- quanteda::dfm(corpus_toks, tolower = FALSE)

  # compute document-embedding matrix
  corpus_dem <- dem(x = corpus_dfm, pre_trained = pre_trained, transform = transform, transform_matrix = transform_matrix, verbose = FALSE)

  # aggregate dems by group var
  if(!is.null(groups)) corpus_dem <- dem_group(x = corpus_dem, groups = corpus_dem@docvars$group)

  # find nearest neighbors
  result <- nns(x = corpus_dem, N = N, candidates = candidates, pre_trained = pre_trained, as_list = FALSE)
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
                         what = what,
                         as_list = FALSE){

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

  # find nearest neighbors
  result <- nns(x = corpus_dem, N = Inf, candidates = candidates, pre_trained = pre_trained, as_list = FALSE)

  return(result)

}

