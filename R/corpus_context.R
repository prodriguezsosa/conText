#' Get contexts sorrounding a user defined keyword along with typicality scores
#'
#' A wrapper for quanteda::kwic() with two added functionalities:
#' i. subsets documents to where keyword is present before tokenizing thereby speeding up processing
#' ii. compute typicality scores (average cosine similarity between a context and all other contexts)
#' Contexts with higher typicality scores are more representative.
#'
#' @param x a [corpus] object
#' @inheritParams quanteda::kwic
#' @param hard_cut logical - if TRUE then the text must have window x 2 tokens,
#' if FALSE it can have window x 2 or fewer (e.g. if a doc begins with a target word,
#' then text will have window tokens rather than window x 2)
#' @param exclude_pattern (logical) if TRUE, pattern/keyword is excluded from the text documents
#' @param what character; which quanteda tokenizer to use. You will rarely want to change this.
#' For Chinese texts you may want to set what = 'fastestword'.
#' @param verbose (logical) if TRUE, report the total number of target instances found.
#'
#' @return a data.frame with with the same columns as quanteda::kwic.
#' If typicality_score = TRUE, then an additional (numeric) column with
#' typicality scores is added and data.frame is sorted from
#' highest to lowest typicality score.
#' @export
#' @rdname corpus_context
#' @keywords corpus_context
#' @examples
#'
#' immig_corpus <- corpus_context(x = cr_sample_corpus,
#' pattern = "immigration", window = 6L, verbose = TRUE)
#'
corpus_context <- function(x, pattern, window = 6L, valuetype = c("glob", "regex", "fixed"), case_insensitive = TRUE, hard_cut = FALSE, exclude_pattern = TRUE, what = 'word', verbose = TRUE){

  # extract elements of corpus
  doc_text <- as.character(x)
  doc_vars <- quanteda::docvars(x)
  doc_names <- quanteda::docnames(x)

  # subset to documents where pattern is present (speeds up tokens() significantly)
  pattern_present <- stringr::str_detect(x, paste(stringr::regex(pattern, ignore_case = case_insensitive), collapse = '|'))

  # stop if no instances are found
  if(!any(pattern_present))stop("no instances of pattern found in corpus")

  # subset corpus
  x <- x[pattern_present]

  # get kwic for each element in pattern
  kwic_x <- quanteda::kwic(quanteda::tokens(x, what = what), pattern = quanteda::phrase(pattern), window = window, valuetype = valuetype, case_insensitive = case_insensitive) %>% data.frame()

  # if hard_cut, keep only contexts with 2*window number of words
  if(hard_cut) kwic_x <- kwic_x %>% dplyr::mutate(num_tokens = quanteda::ntoken(context)) %>% dplyr::filter(num_tokens == window*2) %>% dplyr::select(-num_tokens)

  # create context variable by concatenating pre/post
  if(exclude_pattern) kwic_x <- kwic_x %>% dplyr::mutate(context = paste(pre, post, sep = " ")) %>% dplyr::select(docname, context, keyword, pattern)
  if(!exclude_pattern) kwic_x <- kwic_x %>% dplyr::mutate(context = paste(pre, keyword, post, sep = " ")) %>% dplyr::select(docname, context, keyword, pattern)

  # if verbose print how many instances of each pattern word were found
  if(verbose){
    pattern_tab <- table(kwic_x$keyword)
    for(i in 1:length(pattern_tab)) cat(paste0(unname(pattern_tab[i]), "\ instances of \"", names(pattern_tab[i]),"\" found.", "\n"))}

  # merge with docvars
  kwic_x <- dplyr::left_join(kwic_x, cbind("docname" = quanteda::docnames(x), quanteda::docvars(x)), by = "docname")

  # create corpus
  result <- quanteda::corpus(kwic_x$context, docvars = kwic_x[,setdiff(colnames(kwic_x), c('docname', 'context'))])

  return(result)
}
