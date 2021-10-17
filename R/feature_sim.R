#' Given two feature embedding matrices, compute "parallel" cosine similarities
#' between overlapping features.
#'
#' @param x a feature embedding matrix (`fem`)
#' @param y a feature embedding matrix (`fem`)
#' @param features (character) vector of features for which to compute
#' similarity scores. If not defined then all features will be used.
#'
#' @return a `data.frame` with following columns:
#'  \item{`feature`}{(character) vector of features that are being compared}
#'  \item{`value`}{(numeric) cosine similarity between features in both `fem`s}
#'
#' @export
#' @rdname feature_sim
#' @keywords feature_sim
#' @examples
#'
#' library(quanteda)
#'
#' # tokenize texts
#' cr_tokens_R <- tokens(corpus_subset(cr_sample_corpus, party == "R"))
#' cr_tokens_D <- tokens(corpus_subset(cr_sample_corpus, party == "D"))
#'
#' # create feature co-occurrence matrix (set tri = FALSE to work with fem)
#' cr_fcm_R <- fcm(cr_tokens_R, context = "window", window = 6,
#' count = "frequency", tri = FALSE)
#'
#' cr_fcm_D <- fcm(cr_tokens_D, context = "window", window = 6,
#' count = "frequency", tri = FALSE)
#'
#' # compute feature-embedding matrix
#' cr_fem_R <- fem(cr_fcm_R, pre_trained = glove_subset,
#' transform = TRUE, transform_matrix = khodakA, verbose = FALSE)
#'
#' cr_fem_D <- fem(cr_fcm_D, pre_trained = glove_subset,
#' transform = TRUE, transform_matrix = khodakA, verbose = FALSE)
#'
#' # compare features
#' feature_sim(x = cr_fem_R, y = cr_fem_D)
#'
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
