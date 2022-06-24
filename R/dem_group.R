#' Average document-embeddings in a dem by a grouping variable
#'
#' Average embeddings in a dem by a grouping variable, by averaging over columns within groups
#' and creating new "documents" with the group labels.
#' Similar in essence to `dfm_group`.
#'
#' @param x a (`dem-class`) document-embedding-matrix
#' @param groups a character or factor variable equal in length to the number of documents
#'
#' @return a G x D (`dem-class`) document-embedding-matrix corresponding to the ALC embeddings for each group.
#' G = number of unique groups defined in the `groups` variable, D = dimensions of pretrained embeddings.
#'
#' @export
#' @rdname dem_group
#' @keywords dem_group
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
#' # build document-feature matrix
#' immig_dfm <- dfm(immig_toks)
#'
#' # construct document-embedding-matrix
#' immig_dem <- dem(immig_dfm, pre_trained = cr_glove_subset,
#' transform = TRUE, transform_matrix = cr_transform, verbose = FALSE)
#'
#' # to get group-specific embeddings, average within party
#' immig_wv_party <- dem_group(immig_dem,
#' groups = immig_dem@docvars$party)
dem_group <- function(x, groups = NULL){

  # check grouping variable is of appropriate length
  if(length(groups) != nrow(x)) stop("grouping variable must be of same length as the number of rows in the dem provided", call. = FALSE)

  # group counts
  groups_count = table(groups)

  # sum over groups
  result <- base::rowsum(as.matrix(x), group = groups)
  #all.equal(names(groups_count), rownames(result))

  # divide by group count to get average
  result <- sweep(result, 1, 1/groups_count, '*')

  # create `dem` class object
  result <- build_dem(Class = 'dem',
                      x_dem = result,
                      features = x@features,
                      Dimnames = list(
                        docs = rownames(result),
                        columns = NULL))

  return(result)
}



