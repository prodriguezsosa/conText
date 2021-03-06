Quick Start Guide - Local Transformation Matrix
================

If (a) you have a large enough corpus to train a full GloVe embeddings
model and (b) your corpus is distinctive in some way –e.g. a collection
of articles from scientific journals–, then you may want to consider
estimating your own set of embeddings and transformation matrix before
using `conText`’s embedding regression functionality. In this quick
guide we go through an example using the same Congressional Record
corpus we used in the [Quick Start
Guide](https://github.com/prodriguezsosa/conText/blob/master/vignettes/quickstart.md).

``` r
# load libraries
library(conText)
library(dplyr)
library(text2vec)
library(quanteda)

# set path to where you stored the required datasets
path_to_data <- "~/Dropbox/GitHub/large_data/conText/data/"

# load corpus
cr_corpus <- readRDS(paste0(path_to_data, "cr_corpus.rds"))
```

# 1\. Estimate embeddings

Before estimating a transformation matrix specific to your corpus, you
will first need to estimate a full GloVe embeddings model. To do this we
use the excellent `text2vec` package. We add our code below as an
example but for details on how to use `text2vec` to estimate a GloVe
embeddings model, do refer to their
[website](http://text2vec.org/glove.html). Note, we use a high number of
iterations (`ITERS`). It is likely your model will converge well before
this. You can also lower this number significantly to speed up the
estimation but keep in mind this may reduce the quality of the
embeddings.

``` r
library(text2vec)
library(dplyr)
library(stringr)

# ================================ choice parameters
# ================================
WINDOW_SIZE <- 6
DIM <- 300
ITERS <- 100
COUNT_MIN <- 10

# ================================ create vocab ================================
tokens <- space_tokenizer(cr_corpus$speech)
it <- itoken(tokens, progressbar = FALSE)
vocab <- create_vocabulary(it)
vocab_pruned <- prune_vocabulary(vocab, term_count_min = COUNT_MIN)  # keep only words that meet count threshold

# ================================ create term co-occurrence matrix
# ================================
vectorizer <- vocab_vectorizer(vocab_pruned)
tcm <- create_tcm(it, vectorizer, skip_grams_window = WINDOW_SIZE, skip_grams_window_context = "symmetric", 
    weights = rep(1, WINDOW_SIZE))

# ================================ set model parameters
# ================================
glove <- GlobalVectors$new(rank = DIM, x_max = 100, learning_rate = 0.05)

# ================================ fit model ================================
word_vectors_main <- glove$fit_transform(tcm, n_iter = ITERS, convergence_tol = 0.001, 
    n_threads = RcppParallel::defaultNumThreads())

# ================================ get output ================================
word_vectors_context <- glove$components
local_glove <- word_vectors_main + t(word_vectors_context)  # word vectors

# ================================ save ================================
saveRDS(local_glove, file = paste0(path_to_data, "local_glove.rds"))
```

# 2\. Compute feature co-occurrence matrix

Given our “local” embeddings, we next compute the feature co-occurrence
matrix for our corpus. We do this using `quanteda`’s `fcm` function.
Depending on the size of your corpus, this can take several minutes
(just below 2 mins on 2019 mac book pro for this corpus).

``` r
#---------------------------------
# compute feature co-occurrence matrix
#---------------------------------

# use quanteda's fcm to create an fcm matrix
fcm_cr <- tokens(cr_corpus$speech) %>% fcm(context = "window", count = "frequency", 
    window = WINDOW_SIZE, weights = rep(1, WINDOW_SIZE), tri = FALSE)

# subset fcm to the vocabulary included in the embeddings
fcm_cr <- fcm_select(fcm_cr, pattern = vocab_pruned$term, selection = "keep")
```

# 3\. Estimate local transformation matrix

Finally, given a feature co-occurrence matrix and a set of pre-trained
embeddings (i.e. our local embeddings), we can use `conText`’s
`compute_transform` function to estimate a corpus specific transform.

``` r
#---------------------------------
# compute local transform
#---------------------------------
# the higher the threshold specified in the weighting arg, the faster the code
# (see function for more details)
local_transform <- compute_transform(context_fcm = fcm_cr, pre_trained = local_glove, 
    vocab = vocab_pruned, weighting = 1000)
saveRDS(local_transform, paste0(path_to_data, "local_transform.rds"))
```

# 4\. Check

We can check that our local embeddings and transformation matrix produce
sensical results with an example. In this case we estimate the “a la
Carte” embedding for `immigration` and its corresponding nearest
neighbors.

``` r
#---------------------------------
# load data
#---------------------------------
local_transform <- readRDS(paste0(path_to_data, "local_transform.rds"))
local_glove <- readRDS(paste0(path_to_data, "local_glove.rds"))

#---------------------------------
# find contexts for immigration
#---------------------------------
immigration_contexts <- get_context(x = cr_corpus$speech, target = "immigration", 
    window = 6, valuetype = "fixed", case_insensitive = TRUE, hard_cut = FALSE, verbose = FALSE)

#---------------------------------
# embed each instance using a la carte
#---------------------------------
contexts_vectors <- embed_target(context = immigration_contexts$context, pre_trained = local_glove, 
    transform_matrix = local_transform, transform = TRUE, aggregate = TRUE, verbose = TRUE)

#---------------------------------
# find nearest neighbors
#---------------------------------
find_nns(target_embedding = contexts_vectors$target_embedding, pre_trained = local_glove, 
    N = 10, candidates = contexts_vectors$local_vocab, norm = "l2")
```

    ##  [1] "immigration"   "reform"        "broken"        "comprehensive"
    ##  [5] "system"        "laws"          "fix"           "illegal"      
    ##  [9] "law"           "enforcement"
