#' Get the tokens of contexts sorrounding user defined patterns
#'
#' This function uses quanteda's `kwic()` function to find the contexts
#' around user defined patterns (i.e. target words/phrases) and return a tokens object
#' with the tokenized contexts and corresponding document variables.
#'
#' @param x a (quanteda) `tokens-class` object
#' @inheritParams quanteda::kwic
#' @param hard_cut (logical) - if TRUE then a context must have `window` x 2 tokens,
#' if FALSE it can have `window` x 2 or fewer (e.g. if a doc begins with a target word,
#' then context will have `window` tokens rather than `window` x 2)
#' @param verbose (logical) if TRUE, report the total number of instances per pattern found
#'
#' @return a (quanteda) `tokens-class`. Each document in the output tokens object
#' inherits the document variables (`docvars`) of the document from whence it came,
#' along with a column registering corresponding the pattern used.
#' This information can be retrieved using `docvars()`.
#'
#' @export
#' @rdname tokens_context
#' @keywords tokens_context
#' @examples
#'
#' library(quanteda)
#'
#' # tokenize corpus
#' toks <- tokens(cr_sample_corpus)
#'
#' # build a tokenized corpus of contexts sorrounding a target term
#' immig_toks <- tokens_context(x = toks, pattern = "immigr*", window = 6L)
tokens_context <- function(x, pattern, window = 6L, valuetype = c("glob", "regex", "fixed"), case_insensitive = TRUE, hard_cut = FALSE, verbose = TRUE){

  # class check
  if(class(x)[1] != "tokens") stop("data must be of class tokens")

  # extract elements of tokens object
  x_docvars <- quanteda::docvars(x)
  x_docnames <- quanteda::docnames(x)

  # get kwic for each element in pattern
  kwic_x <- quanteda::kwic(x, pattern = quanteda::phrase(pattern), window = window, valuetype = valuetype, case_insensitive = case_insensitive) %>% data.frame()

  # stop if no instances are found
  if(nrow(kwic_x) == 0) stop("no instances of pattern found in corpus")

  # create context variable by concatenating pre/post
  kwic_x <- kwic_x %>% dplyr::mutate(context = paste(pre, post, sep = " ")) %>% dplyr::select(docname, context, pattern)

  # if hard_cut, keep only contexts with 2*window number of words
  if(hard_cut) kwic_x <- kwic_x %>% dplyr::mutate(num_tokens = quanteda::ntoken(context)) %>% dplyr::filter(num_tokens == window*2) %>% dplyr::select(-num_tokens)

  # merge with docvars
  kwic_x <- dplyr::left_join(kwic_x, cbind("docname" = quanteda::docnames(x), quanteda::docvars(x)), by = "docname")

  # tokenize contexts
  result <- quanteda::tokens(kwic_x$context, what =  attr(x, "meta")$object$what) # use same tokenizer as x
  quanteda::docvars(result) <- kwic_x[,setdiff(colnames(kwic_x), c('docname', 'context'))]

  # if verbose print how many instances of each pattern word were found
  if(verbose){
    pattern_tab <- table(kwic_x$pattern)
    for(i in 1:length(pattern_tab)) cat(paste0(unname(pattern_tab[i]), "\ instances of \"", names(pattern_tab[i]),"\" found.", "\n"))}

  return(result)
}
