#' Given two feature-embedding-matrices, compute "parallel" cosine similarities
#' between overlapping features.
#'
#' Efficient way of comparing two corpora along many features simultaneously.
#'
#' @param x a (`fem-class`) feature embedding matrix.
#' @param y a (`fem-class`) feature embedding matrix.
#' @param features (character) vector of features for which to compute
#' similarity scores. If not defined then all overlapping features will be used.
#'
#' @return a `data.frame` with following columns:
#' \describe{
#'  \item{`feature`}{(character) overlapping features}
#'  \item{`value`}{(numeric) cosine similarity between overlapping features.}
#'  }
#'
#' @export
#' @rdname feature_sim
#' @keywords feature_sim
#' @examples
#' \dontrun{
#' # example exceeds CRAN CPU time to elapsed time limit
#'
#' library(quanteda)
#' # tokenize corpus
#' toks <- tokens(cr_sample_corpus)
#'
#' # create feature co-occurrence matrix for each party (set tri = FALSE to work with fem)
#' fcm_D <- fcm(toks[docvars(toks, 'party') == "D",],
#' context = "window", window = 6, count = "frequency", tri = FALSE)
#' fcm_R <- fcm(toks[docvars(toks, 'party') == "R",],
#' context = "window", window = 6, count = "frequency", tri = FALSE)
#'
#' # compute feature-embedding matrix
#' fem_D <- fem(fcm_D, pre_trained = cr_glove_subset,
#' transform = TRUE, transform_matrix = cr_transform, verbose = FALSE)
#' fem_R <- fem(fcm_R, pre_trained = cr_glove_subset,
#' transform = TRUE, transform_matrix = cr_transform, verbose = FALSE)
#'
#' # compare "horizontal" cosine similarity
#' feat_comp <- feature_sim(x = fem_R, y = fem_D)
#' }
feature_sim <- function(x, y, features = character(0)){

  # check if there are any overlapping tokens
  overlapping_features <- intersect(rownames(x), rownames(y))

  # check if features are defined
  if(length(features) > 0){
    missing_features <- setdiff(features, overlapping_features)
    if(length(missing_features)!=0) for(i in 1:length(missing_features)) cat("the following features are not part of the overlapping feature set: ", paste0(missing_features, collapse = ", "))
    overlapping_features <- intersect(features, overlapping_features)
    }

  # check if there are any overlapping features
  if(length(overlapping_features) == 0) stop("no overlapping features")

  # subset to overlapping features
  x <- x[overlapping_features,]
  y <- y[overlapping_features,]

  # compute rowwise similarity
  row_sim <- text2vec::psim2(x, y, method = "cosine", norm = "l2")

  # result
  result <- data.frame(feature = names(row_sim), value = unname(row_sim)) %>% dplyr::arrange(value)

  return(result)

}
