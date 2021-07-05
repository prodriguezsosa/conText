#' Embedding regression
#'
#' Estimate an embedding regression model
#'
#' @param formula (from lm function) an object of class "formula" (or one that can be coerced to that class): a
#' symbolic description of the model to be fitted.
#' @param data a data.frame containing the variables in the model
#' @param text_var chr - name of variable with the text from which context will be extracted
#' @param pre_trained a V x D matrix of numeric values - pretrained embeddings with V = size of vocabulary and D = embedding dimensions
#' @param transform logical - if TRUE (default), apply ALC transformation, if FALSE simply average context embeddings. If transform = TRUE, you must provide a transform_matrix.
#' @param transform_matrix square numeric matrix corresponding to the transformation matrix
#' @param bootstrap logical - if TRUE, bootstrap regression - required to get standard errors for normed coefficients
#' @param num_bootstraps numeric - number of bootstraps to use
#' @param stratify_by chr vector - specifying variables to stratify by when bootstrapping
#' @param permute logical - if TRUE, compute empirical p-values using permutation test
#' @param num_permutations numeric - number of permutations to use
#' @param getcontexts logical - if TRUE, apply get_context function to get context around target word (Y in formula)
#' @param window integer - defines the size of a context (words around the target)
#' See conText documentation for get_context.
#' @param hard_cut logical - if TRUE then the text must have window x 2 tokens,
#' if FALSE it can have window x 2 or fewer (e.g. if a doc begins with a target word,
#' then text will have window tokens rather than window x 2).
#' See conText documentation for get_context.
#' @param valuetype the type of pattern matching: "glob" for "glob"-style wildcard expressions;
#' "regex" for regular expressions; or "fixed" for exact matching.
#' See quanteda's documentation for the kwic function.
#' See conText documentation for get_context.
#' @param case_insensitive logical - if TRUE, ignore case when matching the target.
#' if valuetype = 'fixed' then this argument is ignored
#' See quanteda's documentation for the kwic function.
#' See conText documentation for get_context.
#' @param verbose print comments
#'
#' @return list with two elements, `betas` = list of beta_cofficients (D dimensional vectors);
#' `normed_betas` = tibble with the norm of the non-intercept coefficients, std.errors (given boostrap), empirical pvalue (given permute)
#' @examples
#' library(conText)
#' library(dplyr)
#'
#' # load data
#' corpus <- sample_corpus
#' pre_trained <- sample_glove
#' transform_matrix <- khodakA
#'
#' # add party indicator variable
#' corpus <- corpus %>% mutate(Republican = if_else(party == 'R', 1, 0))
#'
#' # run conText
#' model1 <- conText(formula = immigration ~ Republican,
#'                   data = corpus,
#'                   text_var = 'speech',
#'                   pre_trained = pre_trained,
#'                   transform = TRUE, transform_matrix = transform_matrix,
#'                   bootstrap = TRUE, num_bootstraps = 10,
#'                   stratify_by = 'Republican',
#'                   permute = TRUE, num_permutations = 100,
#'                   getcontexts = TRUE, window = 6, valuetype = "fixed",
#'                   case_insensitive = FALSE,
#'                   hard_cut = FALSE, verbose = FALSE)
#'
#' # norm of coefficients
#' knitr::kable(model1$normed_betas)
#' @export
conText <- function(formula, data, text_var = 'text', pre_trained, transform = TRUE, transform_matrix, bootstrap = TRUE, num_bootstraps = 20, stratify_by = NULL, permute = TRUE, num_permutations = 100, getcontexts = TRUE, window = 6, valuetype = "fixed", case_insensitive = TRUE, hard_cut = FALSE, verbose = TRUE){

  # warnings
  if(!transform & !is.null(transform_matrix)) warning("Warning: transform = FALSE means transform_matrix argument was ignored. If that was not your intention, use transform = TRUE.")

  # extract target word
  target <- as.character(formula[[2]])

  # extract covariates
  covariates <- attr(terms(formula), which = "term.labels")

  # if getcontexts is specified, get context around target word
  if(getcontexts){

  # subset data to where target is present (this step helps speed up the get_context function)
  if(valuetype == 'fixed') target_present <- grep(target, dplyr::pull(data, text_var), fixed = TRUE)
  if(valuetype != 'fixed') target_present <- grep(target, dplyr::pull(data, text_var), ignore.case = case_insensitive)
  data <- data[target_present,]

  # add document id variable (used for merging back with covariates)
  data <- data %>% dplyr::mutate(docname = paste0("text", 1:nrow(.)))

  # apply get_context function (see get_context documentation)
  context <- get_context(x = dplyr::pull(data, text_var), target = target, window = window, valuetype = valuetype, case_insensitive = case_insensitive, hard_cut = hard_cut, verbose = verbose)

  # merge with metadata
  context <- dplyr::left_join(context, data[,c("docname", covariates)], by = "docname") %>% dplyr::mutate('(Intercept)' = 1)

  # otherwise take the full text as the context to be embedded
  }else{

    # add intercept
    context <- data %>% dplyr::mutate('(Intercept)' = 1) %>% dplyr::mutate(context = dplyr::pull(data, text_var))

  }

  # embed context to get dependent variable (note, aggregate is set to FALSE as we want an embedding for each instance)
  embeds_out <- embed_target(context$context, pre_trained = pre_trained, transform_matrix = transform_matrix, transform = transform, aggregate = FALSE, verbose)
  Y <- embeds_out$target_embedding
  if(verbose) cat('total observations included in regression:', nrow(Y), '\n')

  # regressors
  X <- context[embeds_out$obs_included,c('(Intercept)', covariates)]

  # run full sample ols
  full_sample_out <- run_ols(Y = Y, X = X)

  # outputs
  beta_cofficients <- full_sample_out$betas
  norm_tibble <- dplyr::tibble(Coefficient = names(full_sample_out$normed_betas), Normed_Estimate = unname(full_sample_out$normed_betas))

  # -------------------
  # bootstrapping
  # -------------------

  if(bootstrap){

    if(verbose) cat('starting bootstrapping \n')

    # bootstrapped ols
    bootstrap_out <- replicate(num_bootstraps, bootstrap_ols(Y = Y, X = X, stratify_by = stratify_by), simplify = FALSE)

    # average betas
    betas <- lapply(bootstrap_out, '[[', 'betas')
    bs_betas <- Reduce("+",betas)/length(betas)

    # summary statistics for normed betas
    normed_betas <- lapply(bootstrap_out, '[[', 'normed_betas') %>% do.call(rbind,.)
    mean_normed_betas <- apply(normed_betas, 2, mean)
    stderror_normed_betas <- 1/sqrt(num_bootstraps) * apply(normed_betas, 2, sd)
    bs_normed_betas <- dplyr::tibble(Coefficient = names(mean_normed_betas), Normed_Estimate = unname(mean_normed_betas), Std.Error = unname(stderror_normed_betas))

    # output
    beta_cofficients <- bs_betas
    norm_tibble <- bs_normed_betas

    # notice
    if(verbose) cat('done with bootstrapping \n')

  }

  # -------------------
  # permutation
  # -------------------

  if(permute){

    if(verbose) cat('starting permutations \n')

    # permuted ols
    permute_out <- replicate(num_permutations, permute_ols(Y = Y, X = X), simplify = FALSE)

    # compute empirical p-value
    permuted_normed_betas <- lapply(permute_out, '[[', 'normed_betas') %>% do.call(rbind,.)
    empirical_pvalue <- sweep(permuted_normed_betas, 2, 1/full_sample_out$normed_betas, '*')
    empirical_pvalue <- apply(empirical_pvalue, 2, function(x) sum(x>1)/length(x))

    # bind with results
    norm_tibble <- cbind(norm_tibble, Empirical_Pvalue = unname(empirical_pvalue))

    if(verbose) cat('done with permutations \n')

  }

  # -------------------
  # output
  # -------------------

  return(list(betas = beta_cofficients, normed_betas = norm_tibble))
}

# -----------------------------
#
# SUB-FUNCTIONS
#
# -----------------------------

#' Permute OLS
#'
#' Estimate empirical p-value using permutated regression
#'
#' @param Y vector of regression model's dependent variable (embedded context)
#' @param X data.frame of model independent variables (covariates)
#'
#' @return list with two elements, `betas` = list of beta_cofficients (D dimensional vectors);
#' `normed_betas` = tibble with the norm of the non-intercept coefficients
#'
permute_ols <- function(Y = NULL, X = NULL){

  # shuffle Y
  Y_permuted <- Y[sample(1:nrow(Y), replace = FALSE),]

  # run ols
  ols_out <- run_ols(Y = Y_permuted, X = X)

  # output
  return(ols_out)

}

#' Bootstrap OLS
#'
#' Bootstrap model coefficients and standard errors
#'
#' @param Y vector of regression model's dependent variable (embedded context)
#' @param X data.frame of model independent variables (covariates)
#' @param stratify_by covariates to stratify when bootstrapping
#'
#' @return list with two elements, `betas` = list of beta_cofficients (D dimensional vectors);
#' `normed_betas` = tibble with the norm of the non-intercept coefficients
#'
bootstrap_ols <- function(Y = NULL, X = NULL, stratify_by = NULL){

  # label instances
  X_bs <- cbind(obs = 1:nrow(X), X)

  # sample observations with replacement
  if(is.null(stratify_by)) X_bs <- dplyr::sample_n(X_bs, size = nrow(X_bs), replace = TRUE)else{
    X_bs <- X_bs %>% dplyr::group_by_at(stratify_by) %>% dplyr::sample_n(size = dplyr::n(), replace = TRUE) %>% dplyr::ungroup()
  }

  # subset Y to sampled observations
  Y_bs <- Y[X_bs$obs,]

  # remove observation label
  X_bs <- X_bs[,-1]

  # run ols
  ols_out <- run_ols(Y = Y_bs, X = X_bs)

  # output
  return(ols_out)
}

#' Run OLS
#'
#' Bootstrap model coefficients and standard errors
#'
#' @param Y vector of regression model's dependent variable (embedded context)
#' @param X data.frame of model independent variables (covariates)
#'
#' @return list with two elements, `betas` = list of beta_cofficients (D dimensional vectors);
#' `normed_betas` = tibble with the norm of the non-intercept coefficients
#'
run_ols <- function(Y = NULL, X = NULL){

  # convert X to a matrix
  X_mat <- as.matrix(X, ncol = ncol(X))

  # compute OLS bets hats
  betas <- solve(t(X_mat)%*%X_mat)%*%t(X_mat)%*%Y

  # normed betas
  vars <- setdiff(colnames(X), "(Intercept)") # identify non-intercept covariates (norm of intercept is not all that informative)
  normed_betas <- apply(matrix(betas[vars,], nrow = length(vars)), 1, function(x) norm(matrix(x, nrow = 1))) %>% setNames(vars)

  # output
  return(list('betas' = betas, 'normed_betas' = normed_betas))

}

