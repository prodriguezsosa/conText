% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/embed_target.R
\name{embed_target}
\alias{embed_target}
\title{Embed target using either: (a) a la carte OR (b) simple (untransformed) averaging of context embeddings}
\usage{
embed_target(
  context,
  pre_trained,
  transform = TRUE,
  transform_matrix,
  aggregate = TRUE,
  verbose = TRUE
)
}
\arguments{
\item{context}{(character) vector of texts - \code{context} variable in get_context output}

\item{pre_trained}{(numeric) a F x D matrix corresponding to pretrained embeddings.
F = number of features and D = embedding dimensions.
rownames(pre_trained) = set of features for which there is a pre-trained embedding.}

\item{transform}{(logical) if TRUE (default) apply the 'a la carte' transformation,
if FALSE ouput untransformed averaged embeddings.}

\item{transform_matrix}{(numeric) a D x D 'a la carte' transformation matrix.
D = dimensions of pretrained embeddings.}

\item{aggregate}{(logical) - if TRUE (default) output will return one embedding (i.e. averaged over all instances of target)
if FALSE output will return one embedding per instance}

\item{verbose}{(logical) - report the observations that had no overlap the provided pre-trained embeddings}
}
\value{
list with three elements:
\describe{
\item{\code{target_embedding}}{the target embedding(s). Values and dimensions will vary with the above settings.}
\item{\code{local_vocab}}{(character) vocabulary that appears in the set of contexts provided.}
\item{\code{obs_included}}{(integer) rows of the context vector that were included in the computation.
A row (context) is excluded when none of the words in the context are present in the pre-trained embeddings provided.}
}
}
\description{
For a vector of contexts (generally the context variable in get_context output), return
the transformed (or untransformed) additive embeddings, aggregated or by instance, along with
the local vocabulary. Keep track of which contexts were embedded and which were excluded.
}
\details{
required packages: quanteda
}
\examples{
# find contexts for term immigration
context_immigration <- get_context(x = cr_sample_corpus, target = 'immigration',
                        window = 6, valuetype = "fixed", case_insensitive = TRUE,
                        hard_cut = FALSE, verbose = FALSE)

contexts_vectors <- embed_target(context = context_immigration$context,
pre_trained = cr_glove_subset,
transform = TRUE, transform_matrix = cr_transform,
aggregate = FALSE, verbose = FALSE)
}
\keyword{embed_target}
