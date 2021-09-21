#' Embedding regression
#'
#' Estimate an embedding regression model
#'
#' @param formula a symbolic description of the model to be fitted
#' @param corpus a quanteda `corpus`
#' @inheritParams dem
#' @param bootstrap (logical) if TRUE, bootstrap regression - required to get standard errors for normed coefficients
#' @param num_bootstraps (numeric) number of bootstraps to use
#' @param stratify (logical) if TRUE, stratify by covariates when bootstrapping
#' @param permute (logical) if TRUE, compute empirical p-values using permutation test
#' @param num_permutations (numeric) number of permutations to use
#' @inheritParams corpus_context
#'
#' @return a [conText-class] object
#' @export
#' @rdname conText
#' @keywords conText
#' @examples
#'
#' ## given the target word "immigration"
#' model1 <- conText(formula = immigration ~ party + gender,
#'                   corpus = cr_sample_corpus,
#'                   pre_trained = glove_subset,
#'                   transform = TRUE, transform_matrix = khodakA,
#'                   bootstrap = TRUE, num_bootstraps = 10,
#'                   stratify = TRUE,
#'                   permute = TRUE, num_permutations = 100,
#'                   window = 6, valuetype = "fixed",
#'                   case_insensitive = TRUE,
#'                   verbose = TRUE)
#'
#' # notice, non-binary covariates are automatically "dummified"
#' rownames(model1)
#'
#' # the beta coefficient 'partyR' in this case corresponds to the alc embedding
#' # of "immigration" for Republican party speeches
#'
#' # (normed) coefficient table
#' model1@normed_cofficients
#'
#' ## embedding full documents, not just a target word
#'
#' model1 <- conText(formula = . ~ ideology,
#'                   corpus = anes2016_sample_corpus,
#'                   pre_trained = glove_subset,
#'                   transform = TRUE, transform_matrix = khodakA,
#'                   bootstrap = TRUE, num_bootstraps = 10,
#'                   stratify = TRUE,
#'                   permute = TRUE, num_permutations = 100,
#'                   window = 6, valuetype = "fixed",
#'                   case_insensitive = TRUE,
#'                   verbose = TRUE)
#'
conText <- function(formula, corpus, pre_trained, transform = TRUE, transform_matrix, bootstrap = TRUE, num_bootstraps = 20, stratify = TRUE, permute = TRUE, num_permutations = 100, window = 6L, valuetype = c("glob", "regex", "fixed"), case_insensitive = TRUE, hard_cut = FALSE, verbose = TRUE){

  # initial checks
  if(class(corpus)[1] != "corpus") stop("corpus must be of class corpus")
  if(!transform & !is.null(transform_matrix)) warning("Warning: transform = FALSE means transform_matrix argument was ignored. If that was not your intention, use transform = TRUE.")

  # extract dependent variable
  target <- as.character(formula[[2]])

  # mirror lm convention: if DV is "." then full text is embedded, ow find and embed the context around DV
  if(target != "."){

    # create a corpus of contexts
    context <- corpus_context(x = corpus, pattern = target, window = window, valuetype = valuetype, case_insensitive = case_insensitive, hard_cut = FALSE, exclude_pattern = TRUE, verbose = verbose)

  }else{
    context <- corpus
  }

  #----------------------
  # COVARIATES
  #----------------------

  # extract covariates names
  docvars <- quanteda::docvars(context)
  if(as.character(formula[[3]])[1] == "."){covariates <- setdiff(names(docvars),"session_id")}else{ # follows lm convention, if DV = ., regress on all variables in corpus
    covariates <- attr(terms(formula), which = "term.labels")
    if(any(!(covariates %in% names(docvars))))stop("one or more of the covariates could not be found in the data.")
  }

  # select covariates
  cov_vars <- docvars %>% dplyr::select(all_of(covariates))

  # check covariates are binary dummy variables
  indicator_check <- apply(cov_vars, 2, function(i) all((class(i) %in% c('integer', 'numeric')) & (length(setdiff(i, c(0,1))) == 0)))

  # if there are non-indicator variables
  if(!all(indicator_check)){
    non_indicator_vars <- names(which(indicator_check == FALSE))

    # check whether they can be "dummified" (i.e. must be character or factor variables)
    class_check <- sapply(dplyr::tibble(cov_vars[,non_indicator_vars]), function(i) is.character(i) | is.factor(i))

    # if they can be "dummified", do so (see: https://cran.r-project.org/web/packages/fastDummies/fastDummies.pdf)
    if(!all(class_check))stop("covariates must be either a binary indicator variable (0/1s), a character variable or a factor variable")
    cov_vars <- fastDummies::dummy_cols(cov_vars, select_columns = non_indicator_vars, remove_first_dummy = TRUE, remove_selected_columns = TRUE, ignore_na = TRUE)
  }

  # add intercept
  cov_vars <- cov_vars %>% dplyr::mutate('(Intercept)' = 1)

  # tokenize texts
  context_toks <- tokens(context)

  # create document-feature matrix
  context_dfm <- dfm(context_toks, tolower = FALSE)

  # subset features
  #if(length(features) > 0) context_dfm <- dfm_keep(context_dfm, pattern = features, selection = 'keep', valuetype = 'fixed', case_insensitive = FALSE)

  # embed context to get dependent variable
  context_dem <- dem(x = context_dfm, pre_trained = pre_trained, transform_matrix = transform_matrix, transform = transform, verbose = verbose)
  Y <- context_dem
  if(verbose) cat('total observations included in regression:', nrow(Y), '\n')

  # regressors
  X <- cov_vars

  # run full sample ols
  full_sample_out <- run_ols(Y = Y, X = X)

  # outputs
  beta_coefficients <- full_sample_out$betas
  norm_tibble <- dplyr::tibble(Coefficient = names(full_sample_out$normed_betas), Normed_Estimate = unname(full_sample_out$normed_betas))

  # -------------------
  # bootstrapping
  # -------------------

  if(bootstrap){

    if(verbose) cat('starting bootstrapping \n')

    # bootstrapped ols
    bootstrap_out <- replicate(num_bootstraps, bootstrap_ols(Y = Y, X = X, stratify = stratify), simplify = FALSE)

    # average betas
    betas <- lapply(bootstrap_out, '[[', 'betas')
    bs_betas <- Reduce("+",betas)/length(betas)

    # summary statistics for normed betas
    normed_betas <- lapply(bootstrap_out, '[[', 'normed_betas') %>% do.call(rbind,.)
    mean_normed_betas <- apply(normed_betas, 2, mean)
    stderror_normed_betas <- 1/sqrt(num_bootstraps) * apply(normed_betas, 2, sd)
    bs_normed_betas <- dplyr::tibble(Coefficient = names(mean_normed_betas), Normed_Estimate = unname(mean_normed_betas), Std.Error = unname(stderror_normed_betas))

    # output
    beta_coefficients <- bs_betas
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
  # build conText object
  # -------------------
  result <- build_conText(Class = 'conText',
                          x_conText = beta_coefficients,
                          normed_cofficients = norm_tibble,
                          features = context_dem@features,
                          Dimnames = list(
                            rows = rownames(beta_coefficients),
                            columns = NULL))

  # print the normed coefficient table
  print(norm_tibble)

  return(result)
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
#' @return list with two elements, `betas` = list of beta_coefficients (D dimensional vectors);
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
#' @param stratify covariates to stratify when bootstrapping
#'
#' @return list with two elements, `betas` = list of beta_coefficients (D dimensional vectors);
#' `normed_betas` = tibble with the norm of the non-intercept coefficients
#'
bootstrap_ols <- function(Y = NULL, X = NULL, stratify = NULL){

  # label instances
  X_bs <- cbind(obs = 1:nrow(X), X)

  # sample observations with replacement
  if(stratify) X_bs <- X_bs %>% dplyr::group_by_at(setdiff(names(X), "(Intercept)")) %>% dplyr::sample_n(size = dplyr::n(), replace = TRUE) %>% dplyr::ungroup() else{
    X_bs <- dplyr::sample_n(X_bs, size = nrow(X_bs), replace = TRUE)
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
#' @return list with two elements, `betas` = list of beta_coefficients (D dimensional vectors);
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

