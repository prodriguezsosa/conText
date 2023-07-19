#' Embedding regression
#'
#' Estimates an embedding regression model with jackknife debiasing
#'
#' @param formula a symbolic description of the model to be fitted with a target word as a DV e.g.
#' `immigrant ~ party + gender`. To use a phrase as a DV, place it in quotations e.g.
#' `"immigrant refugees" ~ party + gender`. To use all covariates included in the data,
#' you can use `.` on RHS, e.g.`immigrant ~ .`. If you wish to treat the full document as you DV, rather
#' than a single target word, use `.` on the LHS e.g. `. ~ party + gender`. If you wish to use all covariates
#' on the RHS use `immigrant ~ .`. Any `character` or `factor` covariates will automatically be converted
#' to a set of binary (`0/1`s) indicator variables for each group, leaving the first level out of the regression.
#' @param data a quanteda `tokens-class` object with the necessary document variables. Covariates must be
#' either binary indicator variables or "trasnformable" into binary indicator variables. conText will automatically
#' transform any non-indicator variables into binary indicator variables (multiple if more than 2 classes),
#' leaving out a "base" category.
#' @inheritParams dem
#' @jackknife (logical) if TRUE, jaccknife debiasing is implemented (required to get std. errors)
#' @param confidence_level (numeric in (0,1)) confidence level e.g. 0.95
#' @param permute (logical) if TRUE, compute empirical p-values using permutation test
#' @param num_permutations (numeric) number of permutations to use
#' @inheritParams tokens_context
#'
#' @return a `conText-class` object - a D x M matrix with D = dimensions
#' of the pre-trained feature embeddings provided and M = number of covariates
#' including the intercept. These represent the estimated regression coefficients.
#' These can be combined to compute ALC embeddings for different combinations of covariates.
#' The object also includes various informative attributes, importantly
#' a `data.frame` with the following columns:
#' \describe{
#'  \item{`coefficient`}{(character) name of (covariate) coefficient.}
#'  \item{`value`}{(numeric) debiased norm of the corresponding beta coefficient.}
#'  \item{`std.error`}{(numeric) std. error of the debiased norm of the beta coefficient.}
#'  \item{`lower.ci`}{(numeric) lower bound of the confidence interval of the debiased norm.}
#'  \item{`upper.ci`}{(numeric) upper bound of the confidence interval of the debiased norm.}
#'  \item{`p.value`}{(numeric) (if permute = TRUE) empirical p.value of the norm of the coefficient.}
#'  }
#'
#' @export
#' @rdname conText_jackknife
#' @keywords conText_jackknife
#' @examples
#'
#' library(quanteda)
#'
#' # tokenize corpus
#' toks <- tokens(cr_sample_corpus)
#'
#' ## given the target word "immigration"
#' set.seed(2021L)
#' model1 <- conText_jackknife(formula = immigration ~ party + gender,
#'                  data = toks,
#'                  pre_trained = cr_glove_subset,
#'                  transform = TRUE,
#'                  transform_matrix = cr_transform,
#'                  permute = FALSE,
#'                  num_permutations = 10,
#'                  confidence_level = 0.95,
#'                  window = 6,
#'                  case_insensitive = TRUE,
#'                  verbose = FALSE)
#'
#' # notice, character/factor covariates are automatically "dummified"
#' rownames(model1)
#'
#' # the beta coefficient 'partyR' in this case corresponds to the alc embedding
#' # of "immigration" for Republican party speeches
#'
#' # (normed) coefficient table
#' model1@normed_coefficients
#'
conText_jackknife <- function(formula, data, pre_trained, transform = TRUE, transform_matrix, confidence_level = 0.95, permute = TRUE, num_permutations = 100, window = 6L, valuetype = c("glob", "regex", "fixed"), case_insensitive = TRUE, hard_cut = FALSE, verbose = TRUE){

  # initial checks
  if(class(data)[1] != "tokens") stop("data must be of class tokens", call. = FALSE)
  if(!transform && !is.null(transform_matrix)) warning('Warning: transform = FALSE means transform_matrix argument was ignored. If that was not your intention, use transform = TRUE.', call. = FALSE)
  if(any(grepl("factor\\(", formula))) stop('It seems you are using factor() in "formula" to create a factor variable. \n Please create it directly in "data" and re-run conText.', call. = FALSE) # pre-empt users using lm type notation
  if(confidence_level >= 1 || confidence_level<=0) stop('"confidence_level" must be a numeric value between 0 and 1.', call. = FALSE) # check confidence level is between 0 and 1

  # extract dependent variable
  target <- as.character(formula[[2]])

  # mirror lm convention: if DV is "." then full text is embedded, ow find and embed the context around DV
  if(length(target) == 1 && target == "."){

    toks <- data
    docvars <- quanteda::docvars(toks)

  }else{

    if(length(target) > 1) target <- target[2:length(target)]

    # create a corpus of contexts
    toks <- tokens_context(x = data, pattern = target, window = window, valuetype = valuetype, case_insensitive = case_insensitive, hard_cut = hard_cut, verbose = verbose)
    docvars <- quanteda::docvars(toks) %>% dplyr::select(-pattern)

  }

  #----------------------
  # COVARIATES
  #----------------------

  # extract covariates names
  if(formula[[3]] == "."){covariates <- names(docvars)}else{ # follows lm convention, if DV = ., regress on all variables in data
    covariates <- setdiff(stringr::str_squish(unlist(strsplit(as.character(formula[[3]]), '+', fixed = TRUE))), '') # to allow for phrase DVs
    covs_not_in_data <- covariates[!(covariates %in% names(docvars))]
    if(length(covs_not_in_data) > 0) stop("the following covariates could not be found in the data: ", paste0(covs_not_in_data, collapse = ", "))
  }

  # select covariates
  cov_vars <- docvars %>% dplyr::select(dplyr::all_of(covariates))

  # check which covariates are binary dummy variables
  numeric_vars <- c(names(which(sapply(cov_vars, is.numeric))), names(which(sapply(cov_vars, is.integer))))
  non_numeric_vars <- setdiff(covariates, numeric_vars)

  # dummify non-numeric/integer variables
  # see: https://cran.r-project.org/web/packages/fastDummies/fastDummies.pdf
  if(length(non_numeric_vars)>0){
    cov_vars <- fastDummies::dummy_cols(cov_vars, select_columns = non_numeric_vars, remove_first_dummy = TRUE, remove_selected_columns = TRUE, ignore_na = TRUE)
  }

  # add intercept
  cov_vars <- cov_vars %>% dplyr::mutate('(Intercept)' = 1)

  # create new corpus
  quanteda::docvars(toks) <- cov_vars

  # create document-feature matrix
  toks_dfm <- quanteda::dfm(toks, tolower = FALSE)

  # embed toks to get dependent variable
  toks_dem <- dem(x = toks_dfm, pre_trained = pre_trained, transform_matrix = transform_matrix, transform = transform, verbose = verbose)
  Y <- toks_dem
  if(verbose) cat('total observations included in regression:', nrow(Y), '\n')

  # regressors
  X <- toks_dem@docvars

  # run full sample ols
  full_sample_out <- run_ols(Y = Y, X = X)

  # outputs
  beta_coefficients <- full_sample_out$betas
  norm_tibble <- dplyr::tibble(coefficient = names(full_sample_out$normed_betas), normed.estimate = unname(full_sample_out$normed_betas))

  # -------------------
  # jackknife
  # -------------------
  jack_out <- run_jack_ols(X = X, Y = Y, confidence_level = confidence_level)
  beta_coefficients <- jack_out[["beta_coefficients"]]
  norm_tibble <- jack_out[["norm_tibble"]]

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
    norm_tibble <- cbind(norm_tibble, p.value = unname(empirical_pvalue))

    if(verbose) cat('done with permutations \n')

  }

  # -------------------
  # build conText object
  # -------------------
  result <- build_conText(Class = 'conText',
                          x_conText = beta_coefficients,
                          normed_coefficients = norm_tibble,
                          features = toks_dem@features,
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
#' Estimate empirical p-value using permuted regression
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

#' Run OLS
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
  normed_betas <- apply(matrix(betas[vars,], nrow = length(vars)), 1, function(x) norm(matrix(x, nrow = 1), type = "f")) %>% setNames(vars)

  # output
  return(list('betas' = betas, 'normed_betas' = normed_betas))

}

csolver <- function(x) { chol2inv(chol(crossprod(x)))}

#' Run jackknife debiased OLS
#'
#' @param Y vector of regression model's dependent variable (embedded context)
#' @param X data.frame of model independent variables (covariates)
#' @param confidence_level (numeric in (0,1)) confidence level e.g. 0.95
#'
#' @return list with two elements, `betas` = list of beta_coefficients (D dimensional vectors);
#' `normed_betas` = tibble with the norm and CIs of the non-intercept coefficients
#'
run_jack_ols <- function(X,Y, confidence_level = 0.95) {
  coefficient_names <- names(X)
  X <- as.matrix(X)
  Y <- as.matrix(Y)
  XtXinv <- csolver(X)
  XtY <- crossprod(X,Y)
  beta <- XtXinv%*%XtY
  resid <- Y - X%*%beta
  # compute the hat values without the entire matrix
  H <- rowSums((X%*%XtXinv)*X)
  # calculate all but the residual in the numerator for dfbeta
  numX <- tcrossprod(XtXinv,X)
  # make a residual list
  rlist <- asplit(resid/(1-H),MARGIN=2)
  # jackknife
  jack <- vector(mode="list",length=length(rlist))
  debiased <- vector(mode="list",length=length(rlist))
  n <- nrow(Y)
  for(i in 1:length(rlist)) {
    # writing a loop here because we need to access and compute from multiple lists
    b <- beta[,i] - t(t(numX)*c(rlist[[i]]))
    jack[[i]] <- b
    debiased[[i]] <- rowMeans(n*beta[,i] - (n-1)*b)
  }
  # bind lists
  pseudo_betas <- t(do.call(rbind, debiased))

  # get coefficient names
  rownames(pseudo_betas) <- coefficient_names

  # indices to separate the pseudo values for each coefficient
  pseudo_indices <- lapply(1:ncol(X), function(i) seq(i,ncol(X)*ncol(Y), ncol(X)))
  jack_betas <- lapply(pseudo_indices, function(i) do.call(rbind, jack)[i,]) %>% setNames(coefficient_names)
  normed_jack_betas <- lapply(jack_betas, function(i) apply(i, 2, function(i) sqrt(sum(i^2)))) %>% do.call(rbind,.)
  normed_betas <- apply(matrix(beta, nrow = nrow(beta)), 1, function(x) norm(matrix(x, nrow = 1), type = "f")) %>% setNames(coefficient_names)
  pseudo_normed_betas <- lapply(1:nrow(Y), function(i) nrow(Y)*normed_betas - (nrow(Y)-1)*normed_jack_betas[,i]) %>% do.call(rbind,.)
  avg_pseudo_normed_betas <- colMeans(pseudo_normed_betas)

  # std. error and confidence intervals
  stderr_pseudo_normed_betas <- apply(pseudo_normed_betas, 2, sd)/sqrt(nrow(pseudo_normed_betas))
  lci_pseudo_normed_betas <- avg_pseudo_normed_betas - qt((1-confidence_level)/2,nrow(pseudo_normed_betas)-1, lower.tail = FALSE)*stderr_pseudo_normed_betas
  uci_pseudo_normed_betas <- avg_pseudo_normed_betas + qt((1-confidence_level)/2,nrow(pseudo_normed_betas)-1, lower.tail = FALSE)*stderr_pseudo_normed_betas

  return(list(
    "beta_coefficients" = pseudo_betas,
    "norm_tibble" = dplyr::tibble(coefficient = coefficient_names,
                                  normed.estimate = avg_pseudo_normed_betas,
                                  std.error = stderr_pseudo_normed_betas,
                                  lower.ci = lci_pseudo_normed_betas,
                                  upper.ci = uci_pseudo_normed_betas) %>% dplyr::filter(coefficient!="(Intercept)")
    )
  )
}


