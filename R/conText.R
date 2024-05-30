
#' Embedding regression
#'
#' Estimates an embedding regression model with options to use bootstrapping to estimate confidence
#' intervals and a permutation test for inference (see https://github.com/prodriguezsosa/conText for details.)
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
#' @param bootstrap (logical) if TRUE, use bootstrapping -- sample from texts with replacement and
#' re-run regression on each sample. Required to get std. errors.
#' @param num_bootstraps (numeric) number of bootstraps to use (at least 100)
#' @param confidence_level (numeric in (0,1)) confidence level e.g. 0.95
#' @param stratify (logical) if TRUE, stratify by discrete covariates when bootstrapping.
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
#'  \item{`value`}{(numeric) norm of the corresponding beta coefficient.}
#'  \item{`std.error`}{(numeric) (if bootstrap = TRUE) std. error of the norm of the beta coefficient.}
#'  \item{`lower.ci`}{(numeric) (if bootstrap = TRUE) lower bound of the confidence interval.}
#'  \item{`upper.ci`}{(numeric) (if bootstrap = TRUE) upper bound of the confidence interval.}
#'  \item{`p.value`}{(numeric) (if permute = TRUE) empirical p.value of the norm of the coefficient.}
#'  }
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
#'
#' ## given the target word "immigration"
#' set.seed(2021L)
#' model1 <- conText(formula = immigration ~ party + gender,
#'                  data = toks,
#'                  pre_trained = cr_glove_subset,
#'                  transform = TRUE, transform_matrix = cr_transform,
#'                  bootstrap = TRUE,
#'                  num_bootstraps = 100,
#'                  confidence_level = 0.95,
#'                  stratify = FALSE,
#'                  permute = TRUE, num_permutations = 10,
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

conText <- function(formula, data, pre_trained, transform = TRUE, transform_matrix, jackknife=TRUE, confidence_level = 0.95, stratify = FALSE, permute = TRUE, num_permutations = 100, window = 6L, valuetype = c("glob", "regex", "fixed"), case_insensitive = TRUE, hard_cut = FALSE, verbose = TRUE, cluster_variable = NULL,new_run_ols = F,testing=F,c=0.5){
  # initial checks
  if(class(data)[1] != "tokens") stop("data must be of class tokens", call. = FALSE)
  if(!transform && !is.null(transform_matrix)) warning('Warning: transform = FALSE means transform_matrix argument was ignored. If that was not your intention, use transform = TRUE.', call. = FALSE)
  if(any(grepl("factor\\(", formula))) stop('It seems you are using factor() in "formula" to create a factor variable. \n Please create it directly in "data" and re-run conText.', call. = FALSE) # pre-empt users using lm type notation
  if((confidence_level >= 1 || confidence_level<=0)) stop('"confidence_level" must be a numeric value between 0 and 1.', call. = FALSE) # check confidence level is between 0 and 1

  if(new_run_ols){
    run_ols = run_ols_2
  }else{
    run_ols = run_ols_1
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
  print(3)
  # dummify non-numeric/integer variables
  # see: https://cran.r-project.org/web/packages/fastDummies/fastDummies.pdf
  if(length(non_numeric_vars)>0){
    cov_vars <- fastDummies::dummy_cols(cov_vars, select_columns = non_numeric_vars, remove_first_dummy = TRUE, remove_selected_columns = TRUE, ignore_na = TRUE)
  }

  # create new corpus
  quanteda::docvars(toks) <- cov_vars

  # create document-feature matrix
  toks_dfm <- quanteda::dfm(toks, tolower = FALSE)
  print(4)
  # embed toks to get dependent variable
  toks_dem <- dem(x = toks_dfm, pre_trained = pre_trained, transform_matrix = transform_matrix, transform = transform, verbose = verbose)
  Y <- toks_dem
  if(verbose) cat('total observations included in regression:', nrow(Y), '\n')
  # regressors
  X <- toks_dem@docvars
  if (!is.null(cluster_variable)) {
    ids <- X[,which(names(X)==cluster_variable)]
    X <- X[,-which(names(X)==cluster_variable),drop=F]
  }else{
    ids = NULL
  }


  c = 0.5
  n_1 = 500
  n_2 = 500
  cov_mtx_1 = diag(rchisq(50,df=1,ncp=1))
  cov_mtx_2 = diag(rchisq(50,df=1,ncp=1))
  mu_1 = rep(0,50)
  mu_2 = c(rep(c,25),rep(0,25))
  g1 = mvrnorm(n_1,mu_1,cov_mtx_1)
  g2 = mvrnorm(n_2,mu_2,cov_mtx_2)
  X = data.frame(as.matrix(c(rep(1,500),rep(0,500))))
  Y = rbind(g1,g2)



  full_sample_out <- run_ols(Y = Y, X = X, ids=ids)
  print(5)
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
      tibble(X),#avoid conversion to vector
      na.rm=T
    ))## unname(full_sample_out$covariate_mean)
  )
  #return(list('X'=X,'Y'=Y))
  # -------------------
  # jackknife
  # -------------------

  if(jackknife){
    #return(list('X'=X,'Y'=Y))
    print('Getting standard errors...')
    print('Corrections went through')
    theta = norm_tibble$normed.estimate.deflated
    n = nrow(X)
    partials <- foreach(
      i = 1:n,
      .combine = 'rbind'
    ) %dopar% {
      curr_X = as.data.frame(X[-i,])
      curr_Y = Y[-i,]
      run_ols(Y = curr_Y, X = curr_X, ids=ids)$normed_betas_deflated
    }
    partials = t(partials)
    theta_n = n*theta
    partials = (n-1)*partials
    pseudos = sweep(partials*-1,1,theta_n,'+')
    jack.se <- apply(pseudos,1,function(x) sqrt(var(x)/n))
    alpha = 1 - confidence_level
    ci = qt(alpha/2,n-1,lower.tail = F)*jack.se
    upper.ci = theta + ci
    lower.ci = theta - ci
    norm_tibble = cbind(norm_tibble, std.error = jack.se, lower.ci = lower.ci, upper.ci = upper.ci)

  }


  # -------------------
  # permutation
  # -------------------

  if(permute){
    if (!is.null(ids)) {
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

  }



  # -------------------
  # build conText object
  # -------------------
  rownames(norm_tibble) = NULL
  result <- conText:::build_conText(Class = 'conText',
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
#'
#' @return list with two elements, `betas` = list of beta_coefficients (D dimensional vectors);
#' `normed_betas` = tibble with the norm of the non-intercept coefficients
#'
run_ols_1 <- function(Y = NULL, X = NULL, ids = NULL){

  # observations for the same ID sum up to 1
  # TO DO: Implement version that allows user to also specify weights.
  if(is.null(ids)){
    weights <- rep(1,nrow(Y))
  }else{
    weights <- 1/as.vector(table(ids)[ids])
  }

  mod_list <- 1:ncol(Y) %>%
    purrr::map(
      function(i) estimatr::lm_robust(
        Y[,i] ~ ., data = X, clusters=ids,
        se_type = "stata", # faster than default
        return_vcov = F,
        weights=weights
      ) %>%
        broom::tidy()
    )

  betas <- mod_list %>%
    bind_rows(.id="k") %>%
    tidyr::pivot_wider(id_cols=k, names_from=term, values_from=estimate) %>%
    tibble::column_to_rownames(var="k") %>%
    t() %>%
    as.matrix()

  coefs <- mod_list %>%
    bind_rows() %>%
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

run_ols_2 = function(Y = NULL, X = NULL, ids = NULL){

  # observations for the same ID sum up to 1
  # TO DO: Implement version that allows user to also specify weights.
  if(is.null(ids)){
    weights <- rep(1,nrow(Y))
  }else{
    weights <- 1/as.vector(table(ids)[ids])
  }


  mod_list = lm_robust(as.matrix(Y) ~ .,
                       data=X,
                       clusters=ids,
                       se_type="stata",
                       return_vcov=F, weights=weights) %>%
    broom::tidy()


  betas = mod_list %>%
    dplyr::select(term,estimate) %>%
    tidyr::pivot_wider(names_from=term, values_from=estimate,values_fn = list) %>%
    unchop(everything()) %>%
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



permute_ols <- function(X, Y, ids=NULL, weights=NULL) {
  if (is.null(ids)) {
    Y <- Y[sample(1:nrow(Y)),]
    return(run_ols_uncorrected(X,Y,resids=F))
  } else {
    permuted_data <- tibble(
      Y = as.matrix(Y),
      X = X,
      id = ids
    ) %>%
      group_by(id, X) %>%
      tidyr::nest(Y=Y) %>%
      ungroup() %>%
      mutate(
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



