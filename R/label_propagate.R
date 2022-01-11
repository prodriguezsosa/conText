#' Generate and select additional semantically related words from topical seed word(s)
#'
#' @param seed_words (character) vector - seed word(s)
#' @param pre_trained (numeric) - a F x D matrix corresponding to pretrained embeddings.
#' F = number of features and D = embedding dimensions.
#' @param graphics (logical) - opens window for selection of words
#' @param N (numeric) - number of words to generate for selection
#' @param norm character = c("l2", "none") - how to scale input matrices. If they are already scaled - use "none" (see ?sim2)
#'
#' @return a character vector comprising selected words
#' @export
#'
#' @examples
#' nns_selected <- label_propagate(seed_words =  c("president", "executive"),
#'                                 pre_trained = cr_glove_subset, N = 10,
#'                                 graphics = T)

label_propagate <- function(seed_words,
                            pre_trained,
                            graphics = FALSE,
                            N = 10,
                            norm = "l2") {
  comb_vec = numeric()
  for (i in seq_along(seed_words)) {
    vec = matrix(pre_trained[seed_words[i],], nrow = 1)
    comb_vec = rbind(comb_vec, vec)
  }

  #take column average
  sw_vec = Matrix::colMeans(comb_vec)

  # find nearest neighbours
  nns <- find_nns(
    target_embedding = sw_vec,
    pre_trained = pre_trained,
    N = N,
    candidates = NULL,
    norm = "l2"
  )

  # open selector window if graphics TRUE
  if (graphics) {
    nns_selected <- select.list(nns,
                                multiple = TRUE,
                                title = 'select your words',
                                graphics = TRUE)
  } else{
    nns_selected <- select.list(nns,
                                multiple = TRUE,
                                title =
                                  'select your words',
                                graphics = FALSE)
  }

  return(nns_selected)

}
