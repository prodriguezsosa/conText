#' Virtual class "dem" for a document-embedding matrix
#'
#' The `dem class` is `dgCMatrix class` matrix with additional slots:
#'
#' @slot docvars document covariates, inherited from dfm and corpus
#' subset to embeddable documents
#' @slot features features used in computing the document embeddings
#' @seealso `dem`
#' @rdname dem-class
#' @importClassesFrom Matrix dgCMatrix
#' @keywords internal dem
setClass("dem",
         slots = c(
           docvars = "data.frame",
           features = "character"),
         prototype = list(Dim = integer(2),
                          Dimnames = list(docs = character(), columns = NULL),
                          docvars = data.frame(row.names = integer()),
                          features = character()),
         contains = "dgCMatrix")

#' Virtual class "fem" for a feature-embedding matrix
#'
#' The `fem class` is `dgCMatrix class` matrix with additional slots:
#'
#' @slot features features used in computing the document embeddings
#' @seealso `fem`
#' @rdname fem-class
#' @importClassesFrom Matrix dgCMatrix
#' @keywords internal fem
setClass("fem",
         slots = c(
           features = "character",
           counts = "numeric"),
         prototype = list(Dim = integer(2),
                          Dimnames = list(rows = character(), columns = NULL),
                          features = character(), counts = numeric()),
         contains = "dgCMatrix")

#' Virtual class "conText" for a conText regression output
#'
#' The `conText-class` is `dgCMatrix class` matrix corresponding to
#' the beta coefficients (embeddings) with additional slots:
#'
#' @slot normed_coefficients `normed_betas` a data.frame containing the following variables:
#' \describe{
#' \item{`Coefficient`}{(character) non-intercept coefficient names}
#' \item{`Normed_Estimate`}{(numeric) norm of non-intercept beta coefficients}
#' \item{`Std.Error`}{(numeric) std errors (given boostrap)}
#' \item{`Empirical_Pvalue`}{(numeric) empirical pvalue (given permute)}
#' }
#' @slot features features used in computing the document embeddings
#' @seealso `conText`
#' @rdname conText-class
#' @importClassesFrom Matrix dgCMatrix
#' @keywords internal conText
setClass("conText",
         slots = c(
           normed_coefficients = "data.frame",
           features = "character"),
         prototype = list(Dim = integer(2),
                          Dimnames = list(rows = character(), columns = NULL),
                          normed_coefficients = data.frame(row.names = integer()),
                          features = character()),
         contains = "dgCMatrix")

