% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cos_sim.R
\name{cos_sim}
\alias{cos_sim}
\title{Compute the cosine similarity between one or more ALC embeddings and a set of features.}
\usage{
cos_sim(
  x,
  pre_trained,
  features = NULL,
  stem = FALSE,
  language = "porter",
  as_list = TRUE,
  show_language = TRUE
)
}
\arguments{
\item{x}{a (quanteda) \code{dem-class} or \code{fem-class} object.}

\item{pre_trained}{(numeric) a F x D matrix corresponding to pretrained embeddings.
F = number of features and D = embedding dimensions.
rownames(pre_trained) = set of features for which there is a pre-trained embedding.}

\item{features}{(character) features of interest.}

\item{stem}{(logical) - If TRUE, both \code{features} and \code{rownames(pre_trained)}
are stemmed and average cosine similarities are reported.
We recommend you remove misspelled words from  \code{pre_trained} as these can
significantly influence the average.}

\item{language}{the name of a recognized language, as returned by
     \code{\link[SnowballC]{getStemLanguages}}, or a two- or three-letter ISO-639
     code corresponding to one of these languages (see references for
     the list of codes).
  }

\item{as_list}{(logical) if FALSE all results are combined into a single data.frame
If TRUE, a list of data.frames is returned with one data.frame per feature.}

\item{show_language}{(logical) if TRUE print out message with language used for stemming.}
}
\value{
a \code{data.frame} or list of data.frames (one for each target)
with the following columns:
\describe{
\item{\code{target}}{ (character) rownames of \code{x},
the labels of the ALC embeddings.
NA if is.null(rownames(x)).}
\item{\code{feature}}{(character) feature terms defined in
the \code{features} argument.}
\item{\code{value}}{(numeric) cosine similarity between \code{x}
and feature.}
}
}
\description{
Compute the cosine similarity between one or more ALC embeddings and a set of features.
}
\examples{

library(quanteda)

# tokenize corpus
toks <- tokens(cr_sample_corpus)

# build a tokenized corpus of contexts sorrounding a target term
immig_toks <- tokens_context(x = toks, pattern = "immigr*", window = 6L)

# build document-feature matrix
immig_dfm <- dfm(immig_toks)

# construct document-embedding-matrix
immig_dem <- dem(immig_dfm, pre_trained = cr_glove_subset,
transform = TRUE, transform_matrix = cr_transform, verbose = FALSE)

# to get group-specific embeddings, average within party
immig_wv_party <- dem_group(immig_dem, groups = immig_dem@docvars$party)

# compute the cosine similarity between each party's embedding and a specific set of features
cos_sim(x = immig_wv_party, pre_trained = cr_glove_subset,
features = c('reform', 'enforcement'), as_list = FALSE)
}
\keyword{cos_sim}
