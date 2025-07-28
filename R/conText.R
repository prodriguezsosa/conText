
#' Embedding regression
#'
#' Estimates an embedding regression model with options to use bootstrapping to estimate confidence
#' intervals and a permutation test for inference (see https://github.com/prodriguezsosa/conText for details.)
#'
#'
#' @param formula a symbolic description of the model to be fitted with a target word as a DV e.g.
#' `immigrant ~ party + gender`. To use a phrase as a DV, place it in quotations e.g.
#' `"immigrant refugees" ~ party + gender`. To use all covariates included in the data,
#' you can use `.` on RHS, e.g.`immigrant ~ .`. If you wish to treat the full document as you DV, rather
#' than a single target word, use `.` on the LHS e.g. `. ~ party + gender`. If you wish to use all covariates
#' on the RHS use `immigrant ~ .`. Any `character` or `factor` covariates will automatically be converted
#' to a set of binary (`0/1`s) indicator variables for each group, leaving the first level out of the regression.
#' @param data a quanteda `tokens-class` object with the necessary document variables. Covariates must be
#' either binary indicator variables or "transformable" into binary indicator variables. conText will automatically
#' transform any non-indicator variables into binary indicator variables (multiple if more than 2 classes),
#' leaving out a "base" category.
#' @inheritParams dem
#' @param jackknife (logical) if TRUE, use jackknife (leave one out) to estimate standard errors. Implies n resamples.
#' @param confidence_level (numeric in (0,1)) confidence level e.g. 0.95
#' @param jackknife_fraction (numeric) fraction of data to use in jackknife. Useful for large datasets.
#' @param parallel (logical) if TRUE, use parallel processing. Requires a registered parallel backend (see doParallel() for more information).
#' @param permute (logical) if TRUE, compute empirical p-values using permutation test
#' @param num_permutations (numeric) number of permutations to use
#' @param cluster_variable (character) name of the variable to use for clustering.
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
#'  \item{`normed.estimate.orig`}{(numeric) squared norm of the corresponding beta coefficient.}
#'  \item{`normed.estimate.deflated`}{(numeric) debiased squared norm of the corresponding beta coefficient.}
#'  \item{`normed.estimate.beta.error.null`}{(numeric) estimate of the bias term in the the original squared norm.}
#'  \item{`std.error`}{(numeric) (if jackknife = TRUE) std. error of the debiased squared norm of the beta coefficient.}
#'  \item{`lower.ci`}{(numeric) (if jackknife = TRUE) lower bound of the confidence interval.}
#'  \item{`upper.ci`}{(numeric) (if jackknife = TRUE) upper bound of the confidence interval.}
#'  \item{`p.value`}{(numeric) (if permute = TRUE) empirical p.value of the debiased squared norm of the coefficient.}
#'  }
#'
#'
#' @export
#' @rdname conText
#' @keywords conText
#' @examples
#'
#' library(quanteda)
#'
#' # tokenize corpus
#' toks <- tokens(cr_sample_corpus)
#' ## given the target word "immigration"
#' set.seed(2021L)
#' model1 <- conText(formula = immigration ~ party + gender,
#'                  data = toks,
#'                  pre_trained = cr_glove_subset,
#'                  transform = TRUE, transform_matrix = cr_transform,
#'                  jackknife = FALSE,
#'                  confidence_level = 0.95,
#'                  permute = TRUE, num_permutations = 100,
#'                  cluster_variable = NULL,
#'                  window = 6, case_insensitive = TRUE,
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

conText <- function(formula, data, pre_trained, transform = TRUE, transform_matrix, jackknife=TRUE, confidence_level = 0.95, jackknife_fraction = 1, parallel=FALSE, permute = TRUE, num_permutations = 100, cluster_variable=NULL, window = 6L, valuetype = c("glob", "regex", "fixed"), case_insensitive = TRUE, hard_cut = FALSE, verbose = TRUE){
  # initial checks

  if(class(data)[1] != "tokens") stop("data must be of class tokens", call. = FALSE)

  if(!transform && !is.null(transform_matrix)) warning('Warning: transform = FALSE means transform_matrix argument was ignored. If that was not your intention, use transform = TRUE.', call. = FALSE)

  if(any(grepl("factor\\(|character\\(|numeric\\(", formula))) stop('It seems you are using one of factor(), character(), numeric() in "formula" to modify a variable. \n Please modify it directly in "data" and re-run conText.', call. = FALSE) # pre-empt users using lm type notation
  if((confidence_level >= 1 || confidence_level<=0)) stop('"confidence_level" must be a numeric value between 0 and 1.', call. = FALSE) # check confidence level is between 0 and 1
  if((jackknife_fraction > 1 || jackknife_fraction<=0)) stop('"jackknife_fraction" must be a numeric value between 0 and 1.', call. = FALSE) # check jackknife_fraction is between 0 and 1

  if (parallel){
    if(!foreach::getDoParRegistered()){
      parallel = F
      warning('parallel = TRUE but no parallel backend registered. Use registerDoParalell(). Executing function sequentially.', call. = FALSE)
    }
  }

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
  if (!is.null(cluster_variable)) {
    cluster_variable <- as.character(cluster_variable)
    covariates <- c(covariates, cluster_variable)
  }
  cov_vars <- docvars %>% dplyr::select(dplyr::all_of(covariates))

  # check which covariates are binary dummy variables
  numeric_vars <- c(names(which(sapply(cov_vars, is.numeric))), names(which(sapply(cov_vars, is.integer))))
  non_numeric_vars <- setdiff(setdiff(covariates, numeric_vars), cluster_variable)

  # dummify non-numeric/integer variables
  # see: https://cran.r-project.org/web/packages/fastDummies/fastDummies.pdf
  if(length(non_numeric_vars)>0){
    cov_vars <- fastDummies::dummy_cols(cov_vars, select_columns = non_numeric_vars, remove_first_dummy = TRUE, remove_selected_columns = TRUE, ignore_na = TRUE)
  }

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
  if (!is.null(cluster_variable)) {
    ids <- X[,which(names(X)==cluster_variable)]
    ids = factor(ids)
    X <- X[,-which(names(X)==cluster_variable),drop=F]
  }else{
    ids = factor(1:nrow(X))
  }

  full_sample_out <- run_ols(Y = Y, X = X, ids=ids)
  # outputs
  beta_coefficients <- full_sample_out$betas

  norm_tibble <- dplyr::tibble(
    coefficient = names(full_sample_out$normed_betas_deflated),
    normed.estimate.orig = unname(full_sample_out$normed_betas_orig),
    normed.estimate.deflated = unname(full_sample_out$normed_betas_deflated),
    normed.estimate.beta.error.null = unname(full_sample_out$beta_error_null),
    n = if (is.null(cluster_variable)) { nrow(X) } else {length(unique(ids))},
    n_obs = nrow(X),
    ## sum_vars = sum(apply(Y, 2, var)),
    covariate_mean = c(colMeans(
      dplyr::tibble(X),#avoid conversion to vector
      na.rm=T
    ))## unname(full_sample_out$covariate_mean)
  )

  # -------------------
  # jackknife
  # -------------------

  if(jackknife){
    if(verbose) cat('starting jackknife \n')
    norm_tibble = cbind(norm_tibble,run_jackknife(norm_tibble$normed.estimate.deflated,
                                                  X,Y,ids,confidence_level,verbose,parallel))
    if(verbose) cat('done with jackknife \n')
  }


  # -------------------
  # permutation
  # -------------------

  if(permute){
    if(verbose) cat('starting permutations \n')

    if (!is.null(cluster_variable)) {
      weights <- 1/as.vector(table(ids)[ids])
      uncorrected_betas <- run_ols_uncorrected(X=X, Y=Y, weights=weights)
      permute_out <- replicate(num_permutations, permute_ols(X=X, Y=uncorrected_betas$resids, ids=ids, weights=weights), simplify = FALSE)
    } else {
      uncorrected_betas <- run_ols_uncorrected(X=X, Y=Y)
      permute_out <- replicate(num_permutations, permute_ols(X=X, Y=uncorrected_betas$resids), simplify = FALSE)
    }
    # compute empirical p-value
    permuted_normed_betas <- lapply(permute_out, '[[', 'normed_betas') %>% do.call(rbind,.)
    empirical_pvalue <- sweep(permuted_normed_betas, 2, 1/uncorrected_betas$normed_betas, '*')
    empirical_pvalue <- apply(empirical_pvalue, 2, function(x) sum(x>1)/length(x))

    # bind with results
    norm_tibble <- cbind(norm_tibble, p.value = unname(empirical_pvalue))

    if(verbose) cat('done with permutations \n')

  }



  # -------------------
  # build conText object
  # -------------------

  rownames(norm_tibble) = NULL
  result <- build_conText(Class = 'conText',
                                    x_conText = beta_coefficients,
                                    normed_coefficients = norm_tibble,
                                    features = toks_dem@features,
                                    Dimnames = list(
                                      rows = rownames(beta_coefficients),
                                      columns = NULL))

  # print the normed coefficient table
  if (verbose) print(norm_tibble)



  return(result)
}

# -----------------------------
#
# SUB-FUNCTIONS
#
# -----------------------------


#' Run OLS
#'
#' Bootstrap model coefficients and standard errors
#'
#' @param Y vector of regression model's dependent variable (embedded context)
#' @param X data.frame of model independent variables (covariates)
#' @param ids vector of cluster ids
#' @return list with two elements, `betas` = list of beta_coefficients (D dimensional vectors);
#' `normed_betas` = tibble with the norm of the non-intercept coefficients
#'

run_ols = function(Y = NULL, X = NULL, ids = NULL){
  weights <- 1/as.vector(table(ids)[ids])


  mod_list = estimatr::lm_robust(as.matrix(Y) ~ .,
                       data=X,
                       clusters=ids,
                       se_type="stata",
                       return_vcov=F, weights=weights) %>%
    broom::tidy()

  betas = mod_list %>%
    dplyr::select(term,estimate) %>%
    tidyr::pivot_wider(names_from=term, values_from=estimate,values_fn = list) %>%
    tidyr::unchop(tidyselect::everything()) %>%
    t() %>%
    as.matrix()

  coefs = mod_list %>%
    dplyr::select(
      term, estimate, std.error
    ) %>%
    dplyr::filter(term != "(Intercept)") %>%
    dplyr::group_by(term = factor(term, levels=unique(term))) %>% # fix/prevent reordering!!!!!
    dplyr::summarize(
      original_estimate = sum(estimate^2),
      adjusted_estimate = sum(estimate^2 - std.error^2),
      the_null = sum(std.error^2)
    )

  return(
    list(
      'betas' = betas,
      'normed_betas_orig' = coefs$original_estimate %>% setNames(coefs$term),
      'normed_betas_deflated' = coefs$adjusted_estimate %>% setNames(coefs$term),
      'beta_error_null' = coefs$the_null %>% setNames(coefs$term)## ,
    )
  )
}

# jackknife: https://bookdown.org/compfinezbook/introcompfinr/The-Jackknife.html
# clustered: https://users.ssc.wisc.edu/~bhansen/papers/tcauchy.pdf (page 6)

jackknife_obs_remove = function(X,Y,ids,i){
  idx = which(ids != i)
  curr_X = as.data.frame(X[idx,])
  curr_Y = Y[idx,]
  curr_ids = ids[idx]
  return(run_ols(Y = curr_Y, X = curr_X, ids=factor(curr_ids))$normed_betas_deflated)
}

jackknife_calculate_se = function(partials,theta,n,confidence_level,jackknife_fraction=1){
  jack.se = apply(partials,2,function(x) sqrt((sum((x-mean(x))^2)*(jackknife_fraction))*((n-1)/n)))
  alpha = 1 - confidence_level
  ci = qt(alpha/2,n-1,lower.tail = F)*jack.se
  upper.ci = theta + ci
  lower.ci = theta - ci
  jack_tibble = data.frame(std.error = jack.se, lower.ci = lower.ci, upper.ci = upper.ci)
}


run_jackknife = function(theta,X,Y,ids,confidence_level,verbose=F,parallel=F,jackknife_fraction=1){
  `%fun%` <- foreach::`%do%`
  if (parallel == TRUE){
    `%fun%` <- foreach::`%dopar%`
  }
  n = nrow(X)
  partials = data.frame()
  if(jackknife_fraction < 1){
    jackknife_sample = sample(1:length(ids),size=length(ids)*jackknife_fraction,replace=F)
    ids = ids[jackknife_sample]
    X = as.data.frame(X[jackknife_sample,])
    Y = Y[jackknife_sample,]
  }
  ids = as.numeric(factor(ids))
  if(verbose) {
    pb = utils::txtProgressBar(min = 0, max = max(ids),
                        initial = 0, char = "=", width = 50)
  }
  partials <- foreach::foreach(
    i = 1:max(ids),
    .combine = 'rbind'
  ) %fun% {
    if(verbose) {
      utils::setTxtProgressBar(pb, i)
    }
    jackknife_obs_remove(X,Y,ids,i)
  }
  if(verbose) close(pb)
  jack_tibble = jackknife_calculate_se(partials,theta,n,confidence_level,jackknife_fraction)
  return(jack_tibble)
}



permute_ols <- function(X, Y, ids=NULL, weights=NULL) {
  if (is.null(ids)) {
    Y <- Y[sample(1:nrow(Y)),]
    return(run_ols_uncorrected(X,Y,resids=F))
  } else {
    permuted_data <- dplyr::tibble(
      Y = as.matrix(Y),
      X = X,
      id = ids
    ) %>%
      dplyr::group_by(id, X) %>%
      tidyr::nest(Y=Y) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        Y = sample(Y)
      ) %>%
      tidyr::unnest(cols=c(Y))
    X <- permuted_data$X
    Y <- permuted_data$Y
    weights_permuted <- 1/as.vector(table(permuted_data$id)[permuted_data$id])
    ## compute OLS bets hats
    return(run_ols_uncorrected(X,Y,weights=weights_permuted,resids=F))
  }
}

run_ols_uncorrected <- function(X, Y, ids_numeric=NULL, weights=NULL,resids=T) {
  ## convert X to a matrix
  X_mat <- cbind(as.matrix(X, ncol = ncol(X)), 1)
  colnames(X_mat)[length(colnames(X_mat))] <- "(Intercept)"

  if (!is.null(weights)) {
    sqrtW <- sqrt(weights)
    # Weighted design matrix and response vector
    WX <- sweep(X_mat, MARGIN=1, STATS=sqrtW, FUN="*")
    Wy <- Y * sqrtW
    # QR decomposition of the weighted design matrix
    QR <- qr(WX)
    betas <- qr.solve(QR,Wy)
  } else {
    ## compute OLS bets hats
    QR <- qr(X_mat)
    betas <- qr.solve(QR,Y)
  }
  if(resids){
    residuals <- Y - (X_mat %*% betas)
  }
  ## normed betas
  vars <- setdiff(colnames(X), "(Intercept)")
  normed_betas <- apply(matrix(betas[vars,], nrow = length(vars)), 1,
                        function(x) norm(matrix(x, nrow = 1), type = "f")) %>% setNames(vars)

  ## output
  if(resids){
    return(list('betas' = betas, 'normed_betas' = normed_betas^2, 'resids' = residuals))}
  else{
    return(list('betas' = betas, 'normed_betas' = normed_betas^2))
  }
}



