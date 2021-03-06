#' compute transform matrix A
#'
#' @param context_fcm a quanteda symmetrical fcm
#' @param pre_trained matrix of numeric values - pretrained embeddings
#' @param vocab the output of text2vec's create_vocabulary (or an equivalent)
#' @param weighting none = all words are weighted by their frequency count; log = weight by the log of the frequency count; a numeric = threshold based weighting (= 1 if token count meets threshold, 0 ow); use log for small corpora, numeric threshold for larger corpora
#' @return matrix of numeric values - the D by D transformation matrix (D = number of dimensions of the embeddings space)
#' @export
#'
compute_transform <- function(context_fcm, pre_trained, vocab, weighting = 500){

  # INCLUDE ALL CHECKS HERE (data structure, dimensions etc.)

  # apply hard threshold if given
  if(is.numeric(weighting)) vocab <- vocab %>% filter(term_count >= weighting)  # subset vocab with hard threshold

  # find common vocab between context_fcm and pre-trained word vectors
  local_vocab <- intersect(colnames(context_fcm), rownames(pre_trained))

  # subset pre-trained word vectors to common vocab
  pre_trained <- pre_trained[local_vocab,]

  # subset context_fcm to tokens to be embedded (rows) and contexts to include (columns)
  context_fcm <- context_fcm[vocab$term, local_vocab]

  # multiply context counts by corresponding pre_trained embeddings
  if(!all.equal(rownames(pre_trained), colnames(context_fcm)))stop("check order")
  context_embeddings <- context_fcm%*%pre_trained

  # divide by number of contexts to get average additive embedding for each token of interest
  context_embeddings <- sweep(context_embeddings, MARGIN = 1, 1/vocab$term_count, '*')

  # weight function
  alpha <- matrix(0, nrow = nrow(context_embeddings), ncol = nrow(context_embeddings)) # create weight matrix to be modified
  if(weighting == 'none' | is.numeric(weighting)) diag(alpha) <- 1 # given weighting is numeric, the vocab is subset above hence can simply apply same function as `none` here
  if(weighting == 'log') diag(alpha) <- log(vocab$term_count) # weight by log of token count
  rownames(alpha) <- rownames(context_embeddings)

  # solve for transformation matrix (just a weighted regression)
  transform_matrix <- solve(t(context_embeddings)%*%alpha%*%context_embeddings)%*%t(context_embeddings)%*%alpha%*%pre_trained[rownames(context_embeddings),]

  return(as.matrix(t(transform_matrix)))
}
