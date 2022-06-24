#' Randomly sample documents from a dem
#'
#' Take a random sample of documents from a `dem` with/without replacement and
#' with the option to group by a variable in `dem@docvars`.
#'
#' @param x a (`dem-class`) document-embedding-matrix
#' @inheritParams dplyr::sample_n
#' @param weight (numeric) Sampling weights. Vector of non-negative numbers of length `nrow(x)`.
#' Weights are automatically standardised to sum to 1 (see `dplyr::sample_n`).
#' May not be applied when `by` is used.
#' @param by (character or factor vector) either of length 1 with the name of grouping variable for sampling.
#' Refer to the variable WITH QUOTATIONS e.g. `"party"`. Must be a variable in `dem@docvars`. OR of length
#' nrow(x).
#'
#' @return a `size` x D (`dem-class`) document-embedding-matrix corresponding to the sampled
#' ALC embeddings. Note, `@features` in the resulting object will correspond to the original `@features`,
#' that is, they are not subsetted to the sampled documents. For a list of the documents that were
#' sampled call the attribute: `@Dimnames$docs`.
#'
#' @export
#' @rdname dem_sample
#' @keywords dem_sample
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
#' # to get a random sample
#' immig_wv_party <- dem_sample(immig_dem, size = 10,
#' replace = TRUE, by = "party")
#'
#' # also works
#' immig_wv_party <- dem_sample(immig_dem, size = 10,
#' replace = TRUE, by = immig_dem@docvars$party)
dem_sample <- function(x, size = NULL, replace = FALSE, weight = NULL, by = NULL){

  # check if by is specified
  if(!is.null(by)){

    # check weight is not being applied
    if(!is.null(weight)) stop('weight may not be applied with by', call. = FALSE)

    # check grouping variable is of appropriate length
    if((length(by)==1 && !(by %in% colnames(x@docvars))) || (length(by)!=1 && length(by) != nrow(x))) stop("by must either be a character vector of length 1, referring to a variable present in @docvars, or a vector of length equal to nrow(x)", call. = FALSE)

    # if a vector of lenght > 1
    if(length(by) == nrow(x)){

      # add to docvars
      x@docvars["tmp_by"] <- by

      # sample
      sample_docs <- x@docvars %>%
        dplyr::mutate(docid = seq_len(nrow(.))) %>%
        dplyr::group_by(tmp_by) %>%
        dplyr::sample_n(size = size, replace = replace) %>%
        dplyr::pull(docid)

      # remove from docvars
      x@docvars <-  subset(x@docvars, select = -tmp_by)
    }else{

      # sample
      sample_docs <- x@docvars %>%
        dplyr::mutate(docid = seq_len(nrow(.))) %>%
        dplyr::group_by_at(by) %>%
        dplyr::sample_n(size = size, replace = replace) %>%
        dplyr::pull(docid)
    }

  } else {

    if(is.null(weight)){

      sample_docs <- data.frame(docid = seq_len(nrow(x))) %>% dplyr::sample_n(size = size, replace = replace) %>% dplyr::pull(docid)

    } else{

      sample_docs <- data.frame(docid = seq_len(nrow(x))) %>% dplyr::sample_n(size = size, replace = replace, weight = weight) %>% dplyr::pull(docid)

    }

  }

  # create `dem` class object
  result <- build_dem(Class = 'dem',
                      x_dem = x[sample_docs,],
                      docvars = x@docvars[sample_docs,,drop=FALSE],
                      features = x@features,
                      Dimnames = list(
                        docs = rownames(x)[sample_docs],
                        columns = NULL))

  return(result)
}
