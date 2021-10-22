#' Get context words (words within a symmetric window around the target word/phrase)
#' sorrounding a user defined target.
#'
#' A wrapper function for quanteda's `kwic()` function that subsets documents to where
#' target is present before tokenizing to speed up processing, and concatenates
#' kwic's pre/post variables into a `context` column.
#'
#' @param x (character) vector - this is the set of documents (corpus) of interest.
#' @param target (character) vector - these are the target words whose contexts we want to evaluate
#' This vector may include a single token, a phrase or multiple tokens and/or phrases.
#' @param window (numeric) - defines the size of a context (words around the target).
#' @param hard_cut (logical) - if TRUE then a context must have `window` x 2 tokens,
#' if FALSE it can have `window` x 2 or fewer (e.g. if a doc begins with a target word,
#' then context will have `window` tokens rather than `window` x 2)
#' @inheritParams quanteda::kwic
#' @param what (character) defines which quanteda tokenizer to use. You will rarely want to change this.
#' For chinese text you may want to set `what = 'fastestword'`.
#' @param verbose (logical) - if TRUE, report the total number of target instances found.
#'
#' @return a `data.frame` with the following columns:
#' \describe{
#'  \item{`docname`}{ (character) document name to which instances belong to.}
#'  \item{`target`}{(character) targets.}
#'  \item{`context`}{(numeric) pre/post variables in `kwic()` output concatenated.}
#'  }
#'
#' @note `target` in the return data.frame is equivalent to `kwic()`'s `keyword` output variable,
#' so it may not match the user-defined target exactly if `valuetype` is not fixed.
#'
#' @rdname get_context
#' @keywords get_context
#' @examples
#' # get context words sorrounding the term immigration
#' context_immigration <- get_context(x = cr_sample_corpus, target = 'immigration',
#'                                    window = 6, valuetype = "fixed", case_insensitive = FALSE,
#'                                    hard_cut = FALSE, verbose = FALSE)
#' @export
get_context <- function(x, target, window = 6L, valuetype = "fixed", case_insensitive = TRUE, hard_cut = FALSE, what = 'word', verbose = TRUE){

  # subset to documents where pattern is present (speeds up tokens() significantly)
  pattern_present <- stringr::str_detect(x, paste(stringr::regex(target, ignore_case = case_insensitive), collapse = '|'))

  # stop if no instances are found
  if(!any(pattern_present))stop("no instances of pattern found in corpus")

  # subset corpus
  x <- x[pattern_present]

  # get kwic for each element in target
  kwic_i <- quanteda::kwic(quanteda::tokens(x, what = what), pattern = quanteda::phrase(target), window = window, valuetype = valuetype, case_insensitive = case_insensitive) # get kwic given a phrase

  # combine texts to the left and right of the target word
  # pre/post are part of kwic's output
  if(nrow(kwic_i) > 0){
    out <- as.data.frame(kwic_i) %>%
      dplyr::select(docname, keyword, pre, post) %>% # keep pre and post (see kwic documentation for info on values)
      dplyr::mutate(context = paste(pre, post, sep = " ")) %>% # combine pre and post into one variable named context
      dplyr::select(-c('pre', 'post')) %>% # drop pre & post
      dplyr::rename(target = keyword) # align variable names (we use target instead of keyword)

    # if hard_cut, keep only contexts with 2*window number of words
    if(hard_cut) out <- out %>% dplyr::mutate(num_tokens = quanteda::ntoken(context)) %>% dplyr::filter(num_tokens == window*2) %>% dplyr::select(-num_tokens)

    # if verbose print how many instances of the target word were found
    if(verbose) cat(nrow(out), "instances of target found.", "\n")

    return(out)}else{
      # if no instances were found and verbose, report this
      cat("no instances of target found.", "\n")
    }
}
