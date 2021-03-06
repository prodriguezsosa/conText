#' Get contexts sorrounding a user defined target
#'
#' For a vector of texts, return a dataframe with the contexts within which a user defined target word appears.
#' The underlying machinary is quanteda's kwic function.
#'
#' Required packages: quanteda, dplyr, tidyr
#'
#' @param x a character vector - this is the set of documents (corpus) of interest
#' @param target a character vector - these are the target words whose contexts we want to evaluate
#' This vector may include a single token, a phrase or multiple tokens and/or phrases.
#' @param window integer - defines the size of a context (words around the target)
#' @param hard_cut logical - if TRUE then the text must have window x 2 tokens,
#' if FALSE it can have window x 2 or fewer (e.g. if a doc begins with a target word,
#' then text will have window tokens rather than window x 2)
#' @param valuetype the type of pattern matching: "glob" for "glob"-style wildcard expressions;
#' "regex" for regular expressions; or "fixed" for exact matching.
#' See quanteda's documentation for the kwic function.
#' @param case_insensitive logical - if TRUE, ignore case when matching the target.
#' See quanteda's documentation for the kwic function.
#' @param verbose logical - if TRUE, report the total number of target instances found.
#' @return a `conText` classed data.frame with three columns: document name (`docname`),
#' target (`target`) and contexts (`context`).
#'
#' @note `context` in a `conText` classed data.frame excludes the target word,
#' it is the result of concatenating `pre` and `post` in the `kwic` function.
#' `target` in the return data.frame is equivalent to `kwic`'s keyword,
#' so it may not match the user-defined target exactly if `valuetype` is not fixed.
#'
#' @export
#'
get_context <- function(x, target, window = 6L, valuetype = "fixed", case_insensitive = TRUE, hard_cut = FALSE, verbose = TRUE){

  # get kwic for each element in target
  kwic_i <- lapply(target, function(i){
    if(grepl(' ', i)){ # check if the target is a phrase
      kwic_i <- quanteda::kwic(x, pattern = quanteda::phrase(i), window = window, valuetype = valuetype, case_insensitive = case_insensitive)}else{ # get kwic given a phrase
        kwic_i <- quanteda::kwic(x, pattern = i, window = window, valuetype = valuetype, case_insensitive = case_insensitive) # get kwic given a token
      }})

  # bind kwics
  kwic_i <- do.call(rbind, kwic_i)

  # combine texts to the left and right of the target word
  # pre/post are part of kwic's output
  if(nrow(kwic_i) > 0){
    out <- kwic_i %>%
      dplyr::select(docname, keyword, pre, post) %>% # keep pre and post (see kwic documentation for info on values)
      dplyr::mutate(context = paste(pre, post, sep = " ")) %>% # combine pre and post into one variable named context
      dplyr::select(-c('pre', 'post')) %>% # drop pre & post
      dplyr::rename(target = keyword) # align variable names (we use target instead of keyword)

    # assign conText class
    class(out) <- c("conText", "data.frame")

    # if hard_cut, keep only contexts with 2*window number of words
    if(hard_cut) out <- out %>% dplyr::mutate(num_tokens = quanteda::ntoken(context)) %>% dplyr::filter(num_tokens == window*2) %>% dplyr::select(-num_tokens)

    # if verbose print how many instances of the target word were found
    if(verbose) cat(nrow(out), "instances of target found.", "\n")

    return(out)}else{
      # if no instances were found and verbose, report this
      cat("no instances of target found.", "\n")
    }
}
