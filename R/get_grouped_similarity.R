#' Get averaged similarity scores between target word(s) and one or two vectors of candidate words.
#'
#' Get similarity scores between a target word or words and a comparison vector
#' of one candidate word or words. When two vectors of candidate words are
#' provided (`second_vec` is not `NULL`), the function calculates the cosine
#' similarity between a composite index of the two vectors. This is
#' operationalized as the mean similarity of the target word to the first
#' vector of terms plus negative one multiplied by the mean similarity to the
#' second vector of terms.
#'
#' @param x a (quanteda) `corpus` object
#' @param target (character) vector of words
#' @param first_vec (character) vector of words
#' @param second_vec (character) vector of words
#' @param pre_trained  (numeric) a F x D matrix corresponding to pretrained embeddings,
#' usually trained on the same corpus as that used for `x`.
#' F = number of features and D = embedding dimensions.
#' rownames(pre_trained) = set of features for which there is a pre-trained embedding
#' @param transform_matrix (numeric) a D x D 'a la carte' transformation matrix.
#' D = dimensions of pretrained embeddings.
#' @param group_var (character) variable name in corpus object defining grouping variable
#' @param window (numeric) - defines the size of a context (words around the target)
#' @param norm (character) - "l2" for l2 normalized cosine similarity and "none" for dot product
#' @param remove_punct (logical) - if `TRUE` remove all characters in the Unicode
#'   "Punctuation" `[P]` class
#' @param remove_symbols (logical) - if `TRUE` remove all characters in the Unicode
#'   "Symbol" `[S]` class
#' @param remove_numbers (logical) - if `TRUE` remove tokens that consist only of
#'   numbers, but not words that start with digits, e.g. `2day`
#' @param remove_separators (logical) - if `TRUE` remove separators and separator
#'   characters (Unicode "Separator" `[Z]` and "Control" `[C]` categories)
#' @param valuetype the type of pattern matching: `"glob"` for "glob"-style
#'   wildcard expressions; `"regex"` for regular expressions; or `"fixed"` for
#'   exact matching
#' @param hard_cut (logical) - if TRUE then a context must have `window` x 2 tokens,
#' if FALSE it can have `window` x 2 or fewer (e.g. if a doc begins with a target word,
#' then context will have `window` tokens rather than `window` x 2)
#' @param case_insensitive (logical) - if `TRUE`, ignore case when matching a
#' target patter
#'
#' @return a `data.frame` with the following columns:
#' \describe{
#'  \item{`group`}{ the grouping variable specified for the analysis}
#'  \item{`val`}{(numeric) cosine similarity scores}
#'  }
#' @export
#'
#' @examples
#' quanteda::docvars(cr_sample_corpus, 'year') <- rep(2011:2014, each = 50)
#' cos_simsdf <- get_grouped_similarity(cr_sample_corpus,
#'                                     group_var = "year",
#'                                     target = "immigration",
#'                                     first_vec = c("left", "lefty"),
#'                                     second_vec = c("right", "rightwinger"),
#'                                     pre_trained = cr_glove_subset,
#'                                     transform_matrix = cr_transform,
#'                                     window = 12L,
#'                                     norm = "l2")

get_grouped_similarity <- function(x,
                                   target,
                                   first_vec,
                                   second_vec,
                                   pre_trained,
                                   transform_matrix,
                                   group_var,
                                   window = window,
                                   norm = "l2",
                                   remove_punct = FALSE,
                                   remove_symbols = FALSE,
                                   remove_numbers = FALSE,
                                   remove_separators = FALSE,
                                   valuetype = "fixed",
                                   hard_cut = FALSE,
                                   case_insensitive = TRUE) {

  # Tokenize corpus
  toks <- quanteda::tokens(x, remove_punct = remove_punct, remove_symbols = remove_symbols,
                 remove_numbers = remove_numbers, remove_separators = remove_separators)

  # Build tokenized corpus of contexts surrounding the target word
  target_toks <- tokens_context(x = toks, pattern = target,
                                valuetype = valuetype, window = window,
                                hard_cut = hard_cut, case_insensitive = case_insensitive)

  # Compute ALC embeddings
  target_dfm <- quanteda::dfm(target_toks)
  target_dem <- dem(x = target_dfm, pre_trained = pre_trained,
                    transform = TRUE, transform_matrix = transform_matrix,
                    verbose = TRUE)

  # Aggregate embeddings over the grouping variable
  target_dem_grouped <- dem_group(target_dem,
                                  groups = target_dem@docvars[[group_var]])

  # Find the intersection of first_vec words with pre_trained embeddings
  intersect_words = intersect(first_vec, rownames(pre_trained))
  # Check if any words are missing and print them
  missing_words = setdiff(first_vec, intersect_words)
  if (length(missing_words) > 0) {
    cat("Words in first_vec not found in pre-trained embeddings:", paste(missing_words, collapse=", "), "\n")
  }

  # Determine if transpose is needed based on the number of intersecting words
  if (length(intersect_words) > 1) {
    y_matrix = as.matrix(pre_trained[intersect_words,])
  } else {
    # Transpose if only one word or none
    y_matrix = t(as.matrix(pre_trained[intersect_words,]))
  }
    # Cosine similarity for first vector of terms
  group_first_val <- text2vec::sim2(target_dem_grouped,
                          y = y_matrix,
                          method = 'cosine', norm = norm)

  group_first_val <- Matrix::rowMeans(group_first_val)
  group_first_val <- dplyr::tibble(group = factor(names(group_first_val)),
                            first_val = unname(group_first_val))

  # Assign group_first_val to result if second_vec is NULL
  if (is.null(second_vec)) {
    result <- group_first_val
  } else {
    # Find the intersection of first_vec words with pre_trained embeddings
    intersect_words = intersect(second_vec, rownames(pre_trained))
    # Check if any words are missing and print them
    missing_words = setdiff(second_vec, intersect_words)
    if (length(missing_words) > 0) {
      cat("Words in first_vec not found in pre-trained embeddings:", paste(missing_words, collapse=", "), "\n")
    }

    # Determine if transpose is needed based on the number of intersecting words
    if (length(intersect_words) > 1) {
      y_matrix = as.matrix(pre_trained[intersect_words,])
    } else {
      # Transpose if only one word or none
      y_matrix = t(as.matrix(pre_trained[intersect_words,]))
    }


    group_sec_val <- text2vec::sim2(target_dem_grouped,
                          y = y_matrix,
                          method = 'cosine', norm = norm)

    group_sec_val <- Matrix::rowMeans(group_sec_val)
    group_sec_val <- dplyr::tibble(group = factor(names(group_sec_val)),
                            sec_val = unname(group_sec_val))

    result <- dplyr::left_join(group_first_val, group_sec_val, by = "group") %>%
      dplyr::mutate(val = first_val + (-1)*sec_val) %>%
      dplyr::select(group, val)
  }

  return(result)
}
