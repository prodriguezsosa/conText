Quick Start Guide
================

# Setup

## Installation

``` r
devtools::install_github("prodriguezsosa/conText")
```

## Load package

``` r
library(conText)
library(dplyr)
library(ggplot2)
```

# Datasets

To use conText you will need three datasets:

1.  A corpus with the text and corresponding metadata you want to
    evaluate.
2.  A set of pre-trained embeddings used to embed context words.
3.  A transformation matrix \(A\) specific to the pre-trained
    embeddings.

In [this Dropbox
folder](https://www.dropbox.com/sh/jsyrag7opfo7l7i/AAB1z7tumLuKihGu2-FDmhmKa?dl=0)
we have included the three datasets we use in this Quick Start Guide
along with their documentation. These include: first, `cr_corpus`, a
subset –sessions 111th - 114th– of the [Congressional Record
corpus](https://data.stanford.edu/congress_text), daily edition. We did
some minor pre-processing to the data, specifically: removing non-alpha
characters, removing one to two letter words (e.g. of), and lowercasing.
Second, `glove`, the 300-dimensional GloVe pre-trained embeddings
provided by [Pennington et
al.](https://nlp.stanford.edu/projects/glove/). Third, `khodakA`, the
transformation matrix \(A\) computed by Khodak et al (2018) specific to
the 300-dimensional GloVe embeddings. In this guide we will use these
three datasets to explore how Congressional Democrats and Republicans
differ (during that period) in their understanding of “immigration”.
Note, while the easiest way to use `conText` is to use the provided
GloVe embeddings (`glove`) and transformation matrix (`khodakA`), you
can use your own locally estimated alternatives.

``` r
# set path to where you stored the required datasets
path_to_data <- "~/Dropbox/GitHub/large_data/conText/data/"

# corpus
cr_corpus <- readRDS(paste0(path_to_data, "cr_corpus.rds"))

# (GloVe) pre-trained embeddings
congress_pretrained_mat <- readRDS(paste0(path_to_data, "glove.rds"))

# transformation matrix
congress_transform_mat <- readRDS(paste0(path_to_data, "khodakA.rds"))
```

# Single instance embeddings

We begin by embedding and plotting single instances of contexts around
“immigration” (e.g. “will try once again enact comprehensive
\[immigration\] reforms that have eluded the past”). To do so we first
use the function `get_context` to find all the contexts in which
immigration is mentioned by party. Note, the `target` argument may
include a single token (as in this example), a phrase or multiple tokens
(e.g. `c('immigration', 'immigrants')`) and/or phrases (e.g. `illegal
immigrants`).

``` r
#---------------------------------
# build context corpus
#---------------------------------

# find contexts for Republican speakers
contextR <- get_context(x = cr_corpus$speech[cr_corpus$party == "R"], target = "immigration", 
    window = 6, valuetype = "fixed", case_insensitive = TRUE, hard_cut = FALSE, verbose = FALSE)

# find contexts for Democrat speakers
contextD <- get_context(x = cr_corpus$speech[cr_corpus$party == "D"], target = "immigration", 
    window = 6, valuetype = "fixed", case_insensitive = TRUE, hard_cut = FALSE, verbose = FALSE)

# bind contexts
contexts_corpus <- rbind(cbind(contextR, party = "R"), cbind(contextD, party = "D"))
```

To make it easy to visualize, we randomly sample 100 contexts from each
party.

``` r
# sample 100 observations from each party (for visualization purposes)
set.seed(42L)
contexts_sample <- contexts_corpus %>% group_by(party) %>% sample_n(., size = 100, 
    replace = FALSE) %>% ungroup()
```

Next we use the function `embed_target` to embed each context seperately
using the a la carte (ALC) transformation. Notice in this case we’ve set
the `aggregate` argument to `FALSE`, this indicates that we want an
embedding for every instance rather than an aggregate averaged embedding
– the latter would be the equivalent of the a la carte embedding.

``` r
#---------------------------------
# embed each instance using a la carte
#---------------------------------
contexts_vectors <- embed_target(context = contexts_sample$context, pre_trained = congress_pretrained_mat, 
    transform_matrix = congress_transform_mat, transform = TRUE, aggregate = FALSE, 
    verbose = TRUE)
```

To plot this set of singl-instance 300 dimensional embeddings we use PCA
and the first two components. Each observation on this plot represents
an embedded context in which the term “immigration” was mentioned. While
Democrat and Republican instances do intermingle, we can already notice
some clustering by party. If we were to take the average of the set of
instances for each party, we’d get each parties’ a la carte embedding
for “immigration” – this is equivalente to setting the `aggregate`
argument to `TRUE` in the `embed_target` function.

``` r
# find principal components using pca
contexts_pca <- prcomp(contexts_vectors$target_embedding, scale = TRUE)

# first two pcs
ind_coord <- as_tibble(contexts_pca$x[, 1:2])

# tibble for plotting
plot_tibble <- tibble(x = ind_coord$PC1, y = ind_coord$PC2) %>% mutate(group = factor(contexts_sample$party, 
    levels = c("D", "R")))

#---------------------------------
# visualize
#---------------------------------
ggplot(plot_tibble, aes(x = x, y = y, color = group, shape = group)) + geom_point(size = 2) + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) + 
    geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.5) + 
    scale_colour_manual(labels = c("Democrat", "Republican"), values = c("blue", 
        "red")) + scale_shape_manual(labels = c("Democrat", "Republican"), values = c(19, 
    17)) + scale_x_continuous(name = "PC1", limits = c(-20, 20), breaks = c(-20, 
    -15, -10, -5, 0, 5, 10, 20)) + scale_y_continuous(name = "PC2", limits = c(-15, 
    15), breaks = c(-15, -10, -5, 0, 5, 10, 15)) + theme(panel.background = element_blank(), 
    axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), 
    axis.title.y = element_text(size = 16, margin = margin(t = 0, r = 15, b = 0, 
        l = 15)), axis.title.x = element_text(size = 16, margin = margin(t = 15, 
        r = 0, b = 15, l = 0)), legend.text = element_text(size = 16), legend.title = element_blank(), 
    legend.key = element_blank(), legend.position = "top", legend.spacing.x = unit(0.25, 
        "cm"))
```

<img src="/Users/pedrorodriguez/Dropbox/GitHub/repositories/conText/vignettes/single_instance.png" width="100%" />

# Embedding regression

We noted above that contexts around “immigration” seem to cluster by
party. We can more formally quantify this with embedding regression. In
quantifying the difference as a function of party, we will also control
for gender. First, we need to create indicator variables for our set of
covariates (the `conText` function currently only allows binary
independent variables).

``` r
# add party and gender indicator variables
cr_corpus <- cr_corpus %>% mutate(Republican = if_else(party == "R", 1L, 0L), male = if_else(gender == 
    "M", 1L, 0L))
```

To run an embedding regression we use the `conText` function. First we
specify the formula consisting of the target word of interest,
“immigration” and the set of covariates (again, these must be binary
indicator variables). Notice, the `conText` function calls the
`get_context` function automatically, so you do not need to create a
corpus of contexts before running `conText`, simply provide it with the
original (pre-processed) corpus and `conText` will find all instances of
the target word and their respective contexts. It’s important that you
specify which variable in your `data` contains the text using
`text_var`. In the case of our `cr_corpus` the text variable is labeled
“speech”. Alternatively, if you want to embedded each document in its
entirety (e.g. open-ended responses in a survey), then you must use the
label of the text variable as your dependent variable (e.g. `speech ~
Republican + male`) and set `getcontexts = FALSE`. This will tell
`conText` to skip the step of searching for contexts around a target
word and simply embed the full document. This can be useful if you want
to explore differences in open-ended responses (see paper for an
example) or if you want more complex contexts consisting of multiple
words and/or phrases, in which case you’d need to get those using
`get_context` first. For more information on each argument, we refer you
to the documentation.

``` r
# run conText regression
model1 <- conText(formula = immigration ~ Republican + male, data = cr_corpus, text_var = "speech", 
    pre_trained = congress_pretrained_mat, transform = TRUE, transform_matrix = congress_transform_mat, 
    bootstrap = TRUE, num_bootstraps = 10, stratify_by = c("Republican", "male"), 
    permute = TRUE, num_permutations = 100, getcontexts = TRUE, window = 6, valuetype = "fixed", 
    case_insensitive = TRUE, hard_cut = FALSE, verbose = FALSE)

knitr::kable(model1$normed_betas)
```

| Coefficient | Normed\_Estimate | Std.Error | Empirical\_Pvalue |
| :---------- | ---------------: | --------: | ----------------: |
| Republican  |        0.0330936 | 0.0002574 |                 0 |
| male        |        0.0151128 | 0.0003986 |                 0 |

`model1` is list with two elements: (1) `betas`, these are the estimated
beta coefficients. We will use these below to find the a la carte
embeddings for each group. (2) `normed_betas`, these are the norms of
the beta coefficients (excluding the intercept) along with their
standard errors (given `bootstrap = TRUE`) and empirical p-values (given
`permute = T`). We plot `normed_betas` below.

``` r
# coefficient plot
plot_tibble <- model1$normed_betas %>% mutate(Coefficient = c("Republican", "male")) %>% 
    mutate(Coefficient = factor(Coefficient, levels = Coefficient))
ggplot(plot_tibble, aes(x = Coefficient, y = Normed_Estimate)) + geom_bar(position = position_dodge(), 
    stat = "identity", width = 0.5) + geom_errorbar(aes(ymin = Normed_Estimate - 
    1.96 * Std.Error, ymax = Normed_Estimate + 1.96 * Std.Error), size = 0.75, width = 0.15, 
    position = position_dodge(0.9)) + ylim(0, 0.04) + ylab("Norm of beta hats") + 
    # the stars here are based on the Empirical_Pvalue
geom_text(aes(label = c("***", "***")), position = position_dodge(width = 0.9), hjust = 0.5, 
    vjust = -1, size = 8) + theme(panel.background = element_blank(), axis.text.x = element_text(size = 16, 
    vjust = 0.5, margin = margin(t = 15, r = 0, b = 15, l = 0)), axis.text.y = element_text(size = 16), 
    axis.title.y = element_text(size = 18, margin = margin(t = 0, r = 15, b = 0, 
        l = 15)), axis.title.x = element_blank(), plot.margin = unit(c(1, 1, 0, 0), 
        "cm"))
```

<img src="/Users/pedrorodriguez/Dropbox/GitHub/repositories/conText/vignettes/regression.png" width="100%" />

# Nearest neighbors

`conText` provides three ways of exploring nearest neighbors. For all we
will first limit the candidates to nearest neighbors to the vocabulary
present in our corpus. This usually results in cleaner, more sensible
nearest neighbors than when including the full \(400000\) tokens in the
GloVe pre-trained embeddings as candidates. To do this we first find the
local vocabulary using the function `get_local_vocab`.

``` r
#---------------------------------
# get local vocab (we'll use it to define the candidates for nns)
#---------------------------------
local_vocab <- get_local_vocab(c(contextR$context, contextD$context), pre_trained = congress_pretrained_mat)
```

## 1\. `find_nns`: use alc embeddings output by regression

Our first approach to exploring nearest neighbors is to use the
coefficients output by `conText`. Each groups’ ALC embedding can be
found by simply adding the relevant coefficients.

``` r
#---------------------------------
# a. find_nns: use alc embeddings output by regression
#---------------------------------
alcDF <- model1$betas["(Intercept)", ]  # Democrat - female
alcDM <- model1$betas["(Intercept)", ] + model1$betas["male", ]  # Democrat - male
alcRF <- model1$betas["(Intercept)", ] + model1$betas["Republican", ]  # Republican - female
alcRM <- model1$betas["(Intercept)", ] + model1$betas["Republican", ] + model1$betas["male", 
    ]  # Republican - male
```

Given each groups’ ALC embedding for “immigration” we can use the
function `find_nns` to output the top `N` neighbors.

``` r
# nns
nnsDF <- find_nns(target_embedding = alcDF, pre_trained = congress_pretrained_mat, 
    N = 10, candidates = local_vocab, norm = "l2")
nnsDM <- find_nns(target_embedding = alcDM, pre_trained = congress_pretrained_mat, 
    N = 10, candidates = local_vocab, norm = "l2")
nnsRF <- find_nns(target_embedding = alcRF, pre_trained = congress_pretrained_mat, 
    N = 10, candidates = local_vocab, norm = "l2")
nnsRM <- find_nns(target_embedding = alcRM, pre_trained = congress_pretrained_mat, 
    N = 10, candidates = local_vocab, norm = "l2")

knitr::kable(cbind(`Dem-female` = nnsDF, `Dem-male` = nnsDM, `Rep-female` = nnsRF, 
    `Rep-male` = nnsRM))
```

| Dem-female  | Dem-male    | Rep-female     | Rep-male     |
| :---------- | :---------- | :------------- | :----------- |
| legislation | reform      | immigration    | immigration  |
| enacting    | overhauling | laws           | laws         |
| enact       | legislation | enforcement    | enacting     |
| overhauling | overhaul    | enforcing      | enforcing    |
| immigration | enact       | naturalization | enacted      |
| reform      | enacting    | enforces       | legislation  |
| overhaul    | reforming   | enforce        | illegals     |
| enacted     | entitlement | enacted        | legislations |
| entitlement | revamp      | regulations    | enforcement  |
| reforming   | immigration | enacting       | enact        |

## 2\. `bootstrap_nns`: bootstrap nearest neighbors

The `bootstrap_nns` function requires you provide it with the set of
contexts to use for bootstrapping. We got these using the `get_context`
function above. Sampling with replacement from this set, `bootstrap_nns`
will compute multiple (defined by `num_bootstraps`) ALC embeddings and
for each compute its cosine similarity with the set of candidate nearest
neighbors (the embeddings for these are those found in the pre-trained
embeddings). The output will then include the set of nearest neighbors,
along with their averaged cosine similarity with the target word and
their standard errors. If `bootstrap` is set to `FALSE`, the output will
just include the set of nearest neighbors and their respective cosine
similarity with the target word (note, the difference in this case with
`find_nns` is that the latter does not report the cosine similarities,
just the nearest neighbors).

``` r
set.seed(42L)
nnsR <- bootstrap_nns(context = contextR$context, pre_trained = congress_pretrained_mat, 
    transform_matrix = congress_transform_mat, transform = TRUE, candidates = local_vocab, 
    bootstrap = TRUE, num_bootstraps = 20, N = 50, norm = "l2")
nnsD <- bootstrap_nns(context = contextD$context, pre_trained = congress_pretrained_mat, 
    transform_matrix = congress_transform_mat, transform = TRUE, candidates = local_vocab, 
    bootstrap = TRUE, num_bootstraps = 20, N = 50, norm = "l2")

# print output
knitr::kable(head(nnsD))
```

| Term        |  Estimate | Std.Error |
| :---------- | --------: | --------: |
| reform      | 0.5279948 | 0.0006338 |
| overhauling | 0.5228246 | 0.0005372 |
| legislation | 0.5175039 | 0.0006885 |
| overhaul    | 0.5027943 | 0.0005302 |
| enact       | 0.4951832 | 0.0004612 |
| enacting    | 0.4928773 | 0.0005154 |

``` r
knitr::kable(head(nnsR))
```

| Term        |  Estimate | Std.Error |
| :---------- | --------: | --------: |
| immigration | 0.5056083 | 0.0006459 |
| laws        | 0.4921165 | 0.0009925 |
| enacting    | 0.4585830 | 0.0007630 |
| enforcing   | 0.4473162 | 0.0009387 |
| enacted     | 0.4462357 | 0.0008392 |
| legislation | 0.4425685 | 0.0007689 |

## 3\. `contrast_nns`: contrast the nearest neighbors of two groups

The thrid approach to exploring nearest neighbors, `contrast_nns`, is
useful if we are comparing two groups and want to identify the terms
that are significantly different between the two. In this case we
provide the set of contexts, obtained with `get_context`, for each
group. `contrast_nns` will compute an ALC embedding for each group, and
compare ranking of nearest neighbors by taking the ratio of cosine
similarites. The ratio is informative of how much a given nearest
neighbor is characteristic of one group. A ratio of \(1\) for a given
nearest neighbor indicates no difference between the two groups, while
significant deviations from \(1\) indicate the nearest neighbor is more
characteristic of one of the two groups. If `bootstrap = TRUE`, this
process will be repeated `num_bootstraps` times, sampling with
replacement, to obtain standard erros around the cosine similarities for
each group along with the ratio of these. If `permute = TRUE` then
permutation will be used to identify ratios that significantly deviate
from \(1\). Note, the numerator in the ratio is defined by `context1`.

``` r
set.seed(42L)
N <- 30
contrast_target <- contrast_nns(context1 = contextR$context, context2 = contextD$context, 
    pre_trained = congress_pretrained_mat, transform_matrix = congress_transform_mat, 
    transform = TRUE, bootstrap = TRUE, num_bootstraps = 20, permute = TRUE, num_permutations = 100, 
    candidates = local_vocab, N = 20, norm = "l2")
```

    ## starting bootstrapping 
    ## done bootstrapping 
    ## starting permutations 
    ## done with permutations

This function outputs a list with three elements: `nns1` and `nns2` are
equivalent to the ouputs of `bootstrap_nns`. The third element,
`nns_ratio`, is a dataframe with the full set of candidate nearest
neighbors order by the ratio of cosine similarities, their standard
errors and empirical p-value.

``` r
knitr::kable(head(contrast_target$nns1))
```

| Term        |  Estimate | Std.Error | Empirical\_Pvalue |
| :---------- | --------: | --------: | ----------------: |
| immigration | 0.5055121 | 0.0005854 |                 0 |
| laws        | 0.4904284 | 0.0008031 |                 0 |
| enacting    | 0.4589327 | 0.0006880 |                 1 |
| enacted     | 0.4461946 | 0.0008479 |                 1 |
| enforcing   | 0.4461584 | 0.0008864 |                 0 |
| legislation | 0.4434541 | 0.0009074 |                 1 |

``` r
knitr::kable(head(contrast_target$nns2))
```

| Term        |  Estimate | Std.Error | Empirical\_Pvalue |
| :---------- | --------: | --------: | ----------------: |
| reform      | 0.5277022 | 0.0007101 |              0.00 |
| overhauling | 0.5233232 | 0.0004636 |              0.00 |
| legislation | 0.5168430 | 0.0007806 |              0.00 |
| overhaul    | 0.5029213 | 0.0005290 |              0.00 |
| enact       | 0.4951476 | 0.0004855 |              0.00 |
| enacting    | 0.4930879 | 0.0004256 |              0.17 |

``` r
knitr::kable(head(contrast_target$nns_ratio))
```

| Term      | Estimate | Std.Error | Empirical\_Pvalue |
| :-------- | -------: | --------: | ----------------: |
| alliance  | 926.7512 |  919.6025 |                 0 |
| orders    | 736.4114 |  685.8994 |                 0 |
| minor     | 353.0639 |  384.6441 |                 0 |
| false     | 343.2215 |  350.2073 |                 0 |
| insulting | 310.6654 |  311.3354 |                 0 |
| hands     | 300.0554 |  362.1287 |                 0 |

Notice, it is likely that the candidate nearest neighbors with the
highest ratios are not part of the set of top nearest neighbors for each
party, and instead will tend to be low-frequency words that by chance
are mentioned by one group and not the other, yet are not all that
informative. We suggest sub-setting the `nns_ratio` dataframe to the
union of the top \(N\) nearest neighbors for each party as
below.

``` r
# first get each party's nearest neighbors (output by the contrast_nns function)
nnsR <- contrast_target$nns1
nnsD <- contrast_target$nns2

# subset to the union of top N nearest neighbors for each party
top_nns <- union(nnsR$Term[1:N], nnsD$Term[1:N])

# identify which of these are shared
shared_nns <- intersect(nnsR$Term[1:N], nnsD$Term[1:N])

# subset nns_ratio (output by contrast_nns) to the union of the top nearest
# neighbors
nns_ratio <- contrast_target$nns_ratio %>% dplyr::filter(Term %in% top_nns) %>% mutate(group = case_when(Term %in% 
    nnsR$Term[1:N] & !(Term %in% nnsD$Term[1:N]) ~ "Republican", !(Term %in% nnsR$Term[1:N]) & 
    Term %in% nnsD$Term[1:N] ~ "Democrat", Term %in% shared_nns ~ "shared"), significant = if_else(Empirical_Pvalue < 
    0.01, "yes", "no"))

# order Terms by Estimate
nns_ratio <- nns_ratio %>% mutate(absdev = abs(1 - Estimate)) %>% arrange(-absdev) %>% 
    mutate(tokenID = 1:nrow(.)) %>% mutate(Term_Sig = if_else(significant == "yes", 
    paste0(Term, "*"), Term))
```

We can then plot these nearest neighbors, ranked on the \(y\) axis by
the magnitude of their deviation from \(1\) (in absolute terms) and on
the x-axis by the actual deviation. The more a nearest neighbor deviates
from \(1\), on either side, the more distinctive it is of one of the two
groups.

``` r
# plot
ggplot() + geom_point(aes(x = Estimate, y = tokenID, color = group, shape = group), 
    data = nns_ratio, size = 2) + geom_vline(xintercept = 1, colour = "black", linetype = "dashed", 
    size = 0.5) + geom_text(aes(x = Estimate, y = tokenID, label = Term_Sig), data = nns_ratio, 
    hjust = if_else(nns_ratio$Estimate > 1, -0.2, 1.2), vjust = 0.25, size = 5) + 
    annotate(geom = "text", x = c(0.5, 1.5), y = c(55, 55), label = c("more Democrat", 
        "more Republican"), size = 6) + scale_color_manual(values = c("black", "gray30", 
    "gray60")) + xlim(0, 2) + ylim(0, 60) + ylab("") + xlab("cosine similarity ratio \n (Republican/Democrat)") + 
    theme(panel.background = element_blank(), plot.title = element_text(size = 18, 
        hjust = 0.5, color = "blue"), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), 
        axis.title.y = element_text(size = 16, margin = margin(t = 0, r = 15, b = 0, 
            l = 15)), axis.title.x = element_text(size = 16, margin = margin(t = 15, 
            r = 0, b = 15, l = 0)), legend.text = element_text(size = 16), legend.title = element_blank(), 
        legend.key = element_blank(), legend.position = "top", legend.spacing.x = unit(0.25, 
            "cm"), plot.margin = unit(c(1, 1, 0, 0), "cm"))
```

<img src="/Users/pedrorodriguez/Dropbox/GitHub/repositories/conText/vignettes/nns1.png" width="100%" />

Below is alternative approach to visualizing these results.

``` r
# another way of visualizing NNS
nns_ratio$EstimateJitter <- jitter(nns_ratio$Estimate, amount = 0.5)
nns_ratio <- nns_ratio %>% arrange(Estimate) %>% mutate(EstimateJitter = Estimate + 
    seq(0.2, 0.2 * nrow(.), 0.2))

ggplot() + geom_point(aes(x = EstimateJitter, y = c(0), color = group, shape = group), 
    data = nns_ratio, size = 3) + scale_color_manual(values = c("black", "gray30", 
    "gray60")) + geom_text(aes(x = EstimateJitter, y = c(0), label = Term_Sig), data = nns_ratio, 
    hjust = rep(c(-0.2, 1.2), length(nns_ratio$EstimateJitter)/2), vjust = 0.5, size = 4, 
    angle = 90) + theme(panel.background = element_blank(), legend.text = element_text(size = 14), 
    legend.title = element_blank(), legend.key = element_blank(), legend.position = "top", 
    axis.text.y = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank(), 
    axis.title.x = element_blank(), axis.ticks.y = element_blank(), axis.ticks.x = element_blank(), 
    legend.spacing.x = unit(0.25, "cm"))
```

<img src="/Users/pedrorodriguez/Dropbox/GitHub/repositories/conText/vignettes/nns2.png" width="100%" />

# Prototypical contexts:

Lastly, we may be interested in identifying the most prototypical
contexts within which a given term is used by each group. To do this we
use the `prototypical_context`
function.

``` r
republican_pr <- prototypical_context(context = contextR$context, pre_trained = congress_pretrained_mat, 
    transform = TRUE, transform_matrix = congress_transform_mat, N = 3, norm = "l2")
democrat_pr <- prototypical_context(context = contextD$context, pre_trained = congress_pretrained_mat, 
    transform = TRUE, transform_matrix = congress_transform_mat, N = 3, norm = "l2")
```

    ## most prototypical contexts for Republican speakers

``` r
contextR$context[republican_pr$doc_id]
```

    ## [1] "has said well dont like these laws law requires our immigration authorities"                        
    ## [2] "immigration law would say that our laws are true and just and"                                      
    ## [3] "these immigration laws law requires our authorities iceimmigration and customs enforcementwhen they"

    ## most prototypical contexts for Democrat speakers

``` r
contextD$context[democrat_pr$doc_id]
```

    ## [1] "our broken immigration system need comprehensive reform and act congress which the"    
    ## [2] "republicans should work with pass comprehensive reform bill fix our broken immigration"
    ## [3] "those that want bring about comprehensive reform and reform our immigration system"
