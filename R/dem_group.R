#' Average document-embeddings in a dem by a grouping variable
#'
#' Average embeddings in a dem by a grouping variable, by averaging over columns within groups
#' and creating new "documents" with the group labels.
#' Similar in essence to `dfm_group`.
#'
#' @param x a `dem`
#' @param groups a character or factor variable equal in length to the number of documents
#'
#' @return a `dem-class` object
#' @export
#' @rdname dem_group
#' @keywords dem_group
#' @examples
#'
#' library(quanteda)
#'
#' # tokenize text
#' cr_toks <- tokens(cr_sample_corpus)
#'
#' # construct document-feature-matrix
#' cr_dfm <- dfm(cr_toks)
#'
#' # construct document-embedding-matrix
#' cr_dem <- dem(cr_dfm, pre_trained = cr_glove_subset,
#' transform = TRUE, transform_matrix = cr_transform, verbose = FALSE)
#'
#' # group document-embedding-matrix
#' cr_dem_party <- dem_group(cr_dem,
#' groups = cr_dem@docvars$party)
#'
dem_group <- function(x, groups = NULL){

  # check grouping variable is of appropriate length
  if(length(groups) != nrow(x)) stop("grouping variable must be of same length as the number of rows in the dem provided")

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



