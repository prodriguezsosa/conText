# documentation function -------

#' build a `dem-class` object
#'
#' @param Class defines tha class of this object (fixed)
#' @param x_dem a `dgCMatrix class` matrix
#' @param docvars document covariates, inherited from dfm and corpus,
#' subset to embeddable documents
#' @param features features used in computing the embeddings
#' @param Dimnames row (documents) and columns (NULL) names
#' @export
build_dem <- function(Class = 'em',
                     x_dem,
                     docvars = data.frame(),
                     features = character(),
                     Dimnames = list()){
  result <-  new(Class = Class,
                 as(x_dem, "dgCMatrix"),
                 docvars = docvars,
                 features = features,
                 Dimnames = Dimnames)

  return(result)
}

#' build a `fem-class` object
#'
#' @param Class defines the class of this object (fixed)
#' @param x_fem a `dgCMatrix class` matrix
#' @param features features used in computing the embeddings
#' @param Dimnames row (features) and columns (NULL) names
#' @export
build_fem <- function(Class = 'fem',
                      x_fem,
                      features = character(),
                      Dimnames = list()){
  result <-  new(Class = Class,
                 as(x_fem, "dgCMatrix"),
                 features = features,
                 Dimnames = Dimnames)

  return(result)
}


#' build a `conText-class` object
#'
#' @param Class defines the class of this object (fixed)
#' @param x_conText a `dgCMatrix class` matrix
#' @param normed_cofficients a data.frame withe the normed coefficients and
#' other statistics
#' @param features features used in computing the embeddings
#' @param Dimnames row (features) and columns (NULL) names
#' @export
build_conText <- function(Class = 'conText',
                      x_conText,
                      normed_cofficients = data.frame(),
                      features = character(),
                      Dimnames = list()){
  result <-  new(Class = Class,
                 as(x_conText, "dgCMatrix"),
                 normed_cofficients = normed_cofficients,
                 features = features,
                 Dimnames = Dimnames)

  return(result)
}