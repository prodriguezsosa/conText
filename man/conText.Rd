% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conText.R
\name{conText}
\alias{conText}
\title{Embedding regression}
\usage{
conText(
  formula,
  data,
  pre_trained,
  transform = TRUE,
  transform_matrix,
  jackknife = TRUE,
  confidence_level = 0.95,
  jackknife_fraction = 1,
  parallel = FALSE,
  permute = TRUE,
  num_permutations = 100,
  cluster_variable = NULL,
  window = 6L,
  valuetype = c("glob", "regex", "fixed"),
  case_insensitive = TRUE,
  hard_cut = FALSE,
  verbose = TRUE
)
}
\arguments{
\item{formula}{a symbolic description of the model to be fitted with a target word as a DV e.g.
\code{immigrant ~ party + gender}. To use a phrase as a DV, place it in quotations e.g.
\code{"immigrant refugees" ~ party + gender}. To use all covariates included in the data,
you can use \code{.} on RHS, e.g.\code{immigrant ~ .}. If you wish to treat the full document as you DV, rather
than a single target word, use \code{.} on the LHS e.g. \code{. ~ party + gender}. If you wish to use all covariates
on the RHS use \code{immigrant ~ .}. Any \code{character} or \code{factor} covariates will automatically be converted
to a set of binary (\code{0/1}s) indicator variables for each group, leaving the first level out of the regression.}

\item{data}{a quanteda \code{tokens-class} object with the necessary document variables. Covariates must be
either binary indicator variables or "transformable" into binary indicator variables. conText will automatically
transform any non-indicator variables into binary indicator variables (multiple if more than 2 classes),
leaving out a "base" category.}

\item{pre_trained}{(numeric) a F x D matrix corresponding to pretrained embeddings.
F = number of features and D = embedding dimensions.
rownames(pre_trained) = set of features for which there is a pre-trained embedding.}

\item{transform}{(logical) if TRUE (default) apply the 'a la carte' transformation,
if FALSE ouput untransformed averaged embeddings.}

\item{transform_matrix}{(numeric) a D x D 'a la carte' transformation matrix.
D = dimensions of pretrained embeddings.}

\item{jackknife}{(logical) if TRUE, use jackknife (leave one out) to estimate standard errors. Implies n resamples.}

\item{confidence_level}{(numeric in (0,1)) confidence level e.g. 0.95}

\item{jackknife_fraction}{(numeric) fraction of data to use in jackknife. Useful for large datasets.}

\item{parallel}{(logical) if TRUE, use parallel processing. Requires a registered parallel backend (see doParallel() for more information).}

\item{permute}{(logical) if TRUE, compute empirical p-values using permutation test}

\item{num_permutations}{(numeric) number of permutations to use}

\item{cluster_variable}{(character) name of the variable to use for clustering.}

\item{window}{the number of context words to be displayed around the keyword}

\item{valuetype}{the type of pattern matching: \code{"glob"} for "glob"-style
wildcard expressions; \code{"regex"} for regular expressions; or \code{"fixed"} for
exact matching. See \link[quanteda]{valuetype} for details.}

\item{case_insensitive}{logical; if \code{TRUE}, ignore case when matching a
\code{pattern} or \link[quanteda]{dictionary} values}

\item{hard_cut}{(logical) - if TRUE then a context must have \code{window} x 2 tokens,
if FALSE it can have \code{window} x 2 or fewer (e.g. if a doc begins with a target word,
then context will have \code{window} tokens rather than \code{window} x 2)}

\item{verbose}{(logical) - if TRUE, report the documents that had
no overlapping features with the pretrained embeddings provided.}
}
\value{
a \code{conText-class} object - a D x M matrix with D = dimensions
of the pre-trained feature embeddings provided and M = number of covariates
including the intercept. These represent the estimated regression coefficients.
These can be combined to compute ALC embeddings for different combinations of covariates.
The object also includes various informative attributes, importantly
a \code{data.frame} with the following columns:
\describe{
\item{\code{coefficient}}{(character) name of (covariate) coefficient.}
\item{\code{normed.estimate.orig}}{(numeric) squared norm of the corresponding beta coefficient.}
\item{\code{normed.estimate.deflated}}{(numeric) debiased squared norm of the corresponding beta coefficient.}
\item{\code{normed.estimate.beta.error.null}}{(numeric) estimate of the bias term in the the original squared norm.}
\item{\code{std.error}}{(numeric) (if jackknife = TRUE) std. error of the debiased squared norm of the beta coefficient.}
\item{\code{lower.ci}}{(numeric) (if jackknife = TRUE) lower bound of the confidence interval.}
\item{\code{upper.ci}}{(numeric) (if jackknife = TRUE) upper bound of the confidence interval.}
\item{\code{p.value}}{(numeric) (if permute = TRUE) empirical p.value of the debiased squared norm of the coefficient.}
}
}
\description{
Estimates an embedding regression model with options to use bootstrapping to estimate confidence
intervals and a permutation test for inference (see https://github.com/prodriguezsosa/conText for details.)
}
\examples{

library(quanteda)

# tokenize corpus
toks <- tokens(cr_sample_corpus)
## given the target word "immigration"
set.seed(2021L)
model1 <- conText(formula = immigration ~ party + gender,
                 data = toks,
                 pre_trained = cr_glove_subset,
                 transform = TRUE, transform_matrix = cr_transform,
                 jackknife = FALSE,
                 confidence_level = 0.95,
                 permute = TRUE, num_permutations = 100,
                 cluster_variable = NULL,
                 window = 6, case_insensitive = TRUE,
                 verbose = FALSE)

# notice, character/factor covariates are automatically "dummified"
rownames(model1)

# the beta coefficient 'partyR' in this case corresponds to the alc embedding
# of "immigration" for Republican party speeches

# (normed) coefficient table
model1@normed_coefficients

}
\keyword{conText}
