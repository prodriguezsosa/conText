Quick Start Guide
================

# Preliminaries

**conText** was designed to work closely with
[quanteda](https://quanteda.io), as such most functions will expect a
quanteda object such as a *corpus*, *tokens* or *dfm*/*fcm*. To make the
most of **conText**, we recommend that you familiarize yourself with
some of quanteda’s basic functionalities [Quick Start
Guide](https://quanteda.io/articles/quickstart.html).

# Setup

## Installing the package

Since **conText** is not *yet* available on CRAN you will need to
install it directly from GitHub.

``` r
devtools::install_github("prodriguezsosa/conText")
```

## Load package

``` r
library("conText")
```

# Data

To use **conText** you will need three datasets:

1.  A (quanteda) **corpus** with the documents and corresponding
    document variables (covariates) you want to evaluate.
2.  A set of (GloVe) **pre-trained embeddings**.
3.  A **transformation matrix** specific to the pre-trained embeddings.

The **conText** package includes two sample corpora `cr_sample_corpus`
–a sample of the U.S. Congressional Records (Sessions 111th - 114th)–
and `anes2016_sample_corpus` –a sample of the ANES 2016 open-ended
question on “most important isssues facing the country”–; a subset of
[Stanford NLP’s](https://nlp.stanford.edu/projects/glove/)
300-dimensional GloVe embeddings, `glove_subset`; and a transformation
matrix ,`khodakA`, computed by [Khodak et
al.](https://arxiv.org/abs/1805.05388) for the aforementioned GloVe
300-dimensional embeddings (see documentation for details). For the
following guide we will be using the full versions of these datasets
which we’ve made available
[here](https://www.dropbox.com/sh/6dfr3i6no6nzvm0/AADqk6HFTvZJGNyu3FuO62kGa?dl=0).

## Load data

``` r
# other libraries
library(quanteda)
library(dplyr)

# path to data set this path to wherever you stored the data files
path_to_data <- "~/Dropbox/GitHub/large_data/conText/data/"

## load data

# full GloVe 300
glove_wvs <- readRDS(paste0(path_to_data, "glove.rds"))

# transformation matrix fro GloVe 300
khodakA <- readRDS(paste0(path_to_data, "khodakA.rds"))

# U.S. Congressional Record Sessions 111th - 114th (Obama years)
cr <- readRDS(paste0(path_to_data, "congressional_record.rds"))
cr <- cr %>% distinct(speech, .keep_all = TRUE)  # remove duplicate speeches

# sample corpus
set.seed(2021L)
cr_sample <- cr %>% sample_n(size = 10000)

## build corpora
cr_corpus <- corpus(cr_sample$speech, docvars = cr_sample[, c("party", "gender")])
```

# The building blocks of `a la carte` embeddings

## 1\. Build a corpus of contexts

Suppose we are interested in the semantics sorrounding the word
*immigration* in the U.S. Congress during the Obama years (sessions
111th - 114th). To explore this we will use our Congressional Record
corpus, `cr_corpus`. We begin by identifying all instances of the
**target** term –*immigration*– in our corpus and, for each instance,
store it’s context– the N words (N = 6 in this example) preceding and
following the instance. We can do this in one step using the
`conText::corpus_context` function. Notice, both the input, `x`, and the
output are a quanteda `corpus-object`. Moreover, each document in
`immig_corpus` –a context arround an instance of *immigration*– inherits
the document variables (`docvars`) of the document from whence it came
and can be retrieved using `docvars`.

``` r
# build a corpus of contexts sorrounding the target term 'immigration'
immig_corpus <- corpus_context(x = cr_corpus, pattern = "immigration", window = 6L)
```

    ## 520 instances of immigration found.

``` r
summary(immig_corpus, n = 3)
```

    ## Corpus consisting of 520 documents, showing 3 documents:
    ## 
    ##   Text Types Tokens Sentences     keyword     pattern party gender
    ##  text1    11     12         1 immigration immigration     R      M
    ##  text2    12     12         1 immigration immigration     R      M
    ##  text3    12     12         1 immigration immigration     R      M

## 2\. Build a document-feature-matrix

Given a corpus of contexts, we next build it’s corresponding
document-feature-matrix, namely a matrix where each row represents a
given document’s vector of feature counts. We do this using quanteda’s
`dfm` function.

``` r
# tokenize texts
immig_toks <- tokens(immig_corpus)

# create document-feature matrix
immig_dfm <- dfm(immig_toks)
immig_dfm[1:3, 1:3]
```

    ## Document-feature matrix of: 3 documents, 3 features (66.67% sparse) and 4 docvars.
    ##        features
    ## docs    president obama recently
    ##   text1         1     1        1
    ##   text2         0     0        0
    ##   text3         0     0        0

## 3\. Build a document-embedding-matrix

Given a `dfm`, a set of pre-trained embeddings, and a corresponding
transformation matrix, we can proceed to embed each document (context)
`a la carte`. We embed any given document by multiplying each of it’s
feature counts with its corresponding pre-trained feature-embedding,
(column) summing the resulting vectors, and multiplying by the
transformation matrix. We do so using `conText::dem`–`dem` standing for
*document-embedding-matrix*. Keep in mind, if a given document has no
features that overlap with the pre-trained embeddings provided, said
document will be dropped.

``` r
# create document-embedding matrix using a la carte
immig_dem <- dem(x = immig_dfm, pre_trained = glove_wvs, transform = TRUE, transform_matrix = khodakA, 
    verbose = TRUE)

# you can see which documents where not embedded (in this example all documents
# are embedded) setdiff(docnames(immig_dfm), immig_dem@Dimnames$rows)

# to see the features that were used in creating the embeddings
head(immig_dem@features)
```

    ## [1] "president" "obama"     "recently"  "announced" "executive" "action"

## 4\. Average over document embeddings

We now have an ALC embedding for each context of `immigration` in our
Congressional Record corpus. To get a single embedding for immigration,
we can simply take the (column) average. However, we are often
interested in exploring semantic differences as a function of covariates
–e.g. party. To do so we simply average within a grouping variable
defined by the covariates. We do this using `conText::dem_group` (very
similar in flavor to quanteda’s `dfm_group`). Notice, a `dem-class`
object inherits all the document variables from the `dfm` used to
compute it.

``` r
# to get a single 'corpus-wide' embedding, take the column average
immig_wv <- colMeans(immig_dem)
length(immig_wv)
```

    ## [1] 300

``` r
# to get group-specific embeddings, average within party
immig_wv_party <- dem_group(immig_dem, groups = immig_dem@docvars$party)
dim(immig_wv_party)
```

    ## [1]   2 300

``` r
# the first 5 dimensions of each party's dense vector representation (embedding)
# of 'immigration'
immig_wv_party[, 1:5]
```

    ## 2 x 5 sparse Matrix of class "dgCMatrix"
    ##                                                              
    ## D -0.008220030 -0.015602337 -0.03313222 0.04601158 0.01922102
    ## R  0.008403035 -0.007154615 -0.02150319 0.04856440 0.00241099

## 5\. Comparing group embeddings

Given each party’s embeddings, we can explore their differences using.
We can evaluate differences in nearest neighbors –features with the
highest cosine-similarity a given embedding– using `conText::nns()` or
explore their cosine similarity with specific features of interest using
`conText::cos_sim()`. Notice, below we limit the set of candidate
nearest neighbors to the set of features in our corpus.

``` r
# find nearest neighbors for overall immigraiton embedding
nns(immig_wv, pre_trained = glove_wvs, N = 5, candidates = immig_dem@features)
```

    ## # A tibble: 5 × 4
    ##   target feature      rank value
    ##   <lgl>  <chr>       <int> <dbl>
    ## 1 NA     legislation     1 0.478
    ## 2 NA     enacting        2 0.469
    ## 3 NA     immigration     3 0.450
    ## 4 NA     reform          4 0.449
    ## 5 NA     enacted         5 0.442

``` r
# find nearest neighbors by party
nns(immig_wv_party, pre_trained = glove_wvs, N = 5, candidates = immig_dem@features)
```

    ## # A tibble: 10 × 4
    ##    target feature      rank value
    ##    <fct>  <chr>       <int> <dbl>
    ##  1 D      reform          1 0.501
    ##  2 D      legislation     2 0.476
    ##  3 D      reforming       3 0.460
    ##  4 D      enacting        4 0.457
    ##  5 D      reforms         5 0.421
    ##  6 R      immigration     1 0.476
    ##  7 R      enacted         2 0.456
    ##  8 R      laws            3 0.454
    ##  9 R      enacting        4 0.450
    ## 10 R      legislation     5 0.443

``` r
# compute the cosine similarity between each party's embedding and a specific set
# of features
cos_sim(immig_wv_party, pre_trained = glove_wvs, features = c("reform", "enforcement"))
```

    ##   target     feature     value
    ## 1      D      reform 0.5007252
    ## 2      R      reform 0.3381023
    ## 3      D enforcement 0.2767213
    ## 4      R enforcement 0.3876106

## Multiple Keywords

In the above example we had one keyword, `immigration`. However, we can
also explore the semantics of multiple keywords simultaneously,
including phrases\! We simply provide a vector of patterns in the
`pattern` argument.

``` r
# build a corpus of contexts sorrounding the target term 'immigration'
mkws_corpus <- corpus_context(x = cr_corpus, pattern = c("immigration", "welfare", 
    "immigration reform"), window = 6L)
```

    ## 520 instances of immigration found. 
    ## 104 instances of immigration reform found. 
    ## 191 instances of welfare found.

``` r
# tokenize texts
mkws_toks <- tokens(mkws_corpus)

# create document-feature matrix
mkws_dfm <- dfm(mkws_toks)

# create document-embedding matrix using a la carte
mkws_dem <- dem(x = mkws_dfm, pre_trained = glove_wvs, transform = TRUE, transform_matrix = khodakA, 
    verbose = TRUE)

# get keyword-specific embeddings
mkws_wvs <- dem_group(mkws_dem, groups = mkws_dem@docvars$pattern)

# find nearest neighbors for overall immigraiton embedding
nns(mkws_wvs, pre_trained = glove_wvs, N = 5, candidates = mkws_wvs@features)
```

    ## # A tibble: 15 × 4
    ##    target             feature        rank value
    ##    <fct>              <chr>         <int> <dbl>
    ##  1 immigration        legislation       1 0.478
    ##  2 immigration        enacting          2 0.469
    ##  3 immigration        immigration       3 0.450
    ##  4 immigration        reform            4 0.449
    ##  5 immigration        enacted           5 0.442
    ##  6 welfare            welfare           1 0.542
    ##  7 welfare            beneficiaries     2 0.408
    ##  8 welfare            medicare          3 0.397
    ##  9 welfare            wellbeing         4 0.385
    ## 10 welfare            benefits          5 0.381
    ## 11 immigration reform comprehensive     1 0.507
    ## 12 immigration reform bipartisan        2 0.492
    ## 13 immigration reform legislation       3 0.458
    ## 14 immigration reform enacting          4 0.424
    ## 15 immigration reform amendments        5 0.380

# Wrapper functions

The above functions give users a lot of flexibility. However, it can be
cumbersome to write each step separately every time we want to analyze a
given target term. To facilitate this process `conText` includes three
(for now) wrapper functions that allow you to do the above analysis and
others with one line of code. An added advantage of the wrapper
functions is that they make it easy to apply boostrapping –peform the
analysis over random samples (with replacement) of the corpus and
averaging. We being with `conText::get_nns`, a wrapper function to
compare nearest neighbors between groups. Again, we limit the candidates
to the set of features in our corpus.

``` r
# get local vocabulary (the intersect between features in the corpus and
# pre-trained embeddings)
local_vocab <- get_local_vocab(immig_corpus, pre_trained = glove_wvs)

# set seed to replicate sampling
set.seed(2021L)

# compare nearest neighbors between groups
get_nns(x = immig_corpus, N = 10, groups = docvars(immig_corpus, "party"), candidates = local_vocab, 
    pre_trained = glove_wvs, transform = TRUE, transform_matrix = khodakA, bootstrap = TRUE, 
    num_bootstraps = 10)
```

    ## # A tibble: 20 × 5
    ##    target feature         rank value std.error
    ##    <fct>  <chr>          <int> <dbl>     <dbl>
    ##  1 D      reform             1 0.499   0.00406
    ##  2 D      legislation        2 0.475   0.00397
    ##  3 D      reforming          3 0.460   0.00233
    ##  4 D      enacting           4 0.456   0.00261
    ##  5 D      reforms            5 0.418   0.00400
    ##  6 D      enacted            6 0.407   0.00396
    ##  7 D      immigration        7 0.405   0.00365
    ##  8 D      bipartisan         8 0.403   0.00477
    ##  9 D      priorities         9 0.366   0.00200
    ## 10 D      amendments        10 0.363   0.00437
    ## 11 R      immigration        1 0.471   0.00380
    ## 12 R      enacted            2 0.454   0.00337
    ## 13 R      laws               3 0.448   0.00417
    ## 14 R      enacting           4 0.445   0.00409
    ## 15 R      legislation        5 0.441   0.00457
    ## 16 R      naturalization     6 0.405   0.00411
    ## 17 R      undocumented       7 0.397   0.00316
    ## 18 R      enforcing          8 0.390   0.00323
    ## 19 R      regulations        9 0.389   0.00424
    ## 20 R      enforce           10 0.385   0.00314

Similarly, we can evaluate how similar each group’s embedding is to a
set of features of interest using `conText::get_cos_sim`.

``` r
# set seed to replicate sampling
set.seed(2021L)

# compute the cosine similarity between each group's embedding and a specific set
# of features
get_cos_sim(x = immig_corpus, groups = docvars(immig_corpus, "party"), features = c("reform", 
    "enforce"), pre_trained = glove_wvs, transform = TRUE, transform_matrix = khodakA, 
    verbose = FALSE)
```

    ##   target feature     value
    ## 1      D  reform 0.5007252
    ## 2      R  reform 0.3381023
    ## 3      D enforce 0.2853419
    ## 4      R enforce 0.3898058

The third wrapper function is `conText::contrast_nns`. This wrapper
function is useful if we are comparing two groups and want to identify
the terms that are significantly distinctive to each group.
`contrast_nns` will compute an ALC embedding for each group, identify
the union of the top N nearest neighbors and, for this set, compute the
ratio cosine similarities. This ratio is informative of how much a given
nearest neighbor is characteristic of one group. A ratio of \(1\) for a
given nearest neighbor indicates no difference between the two groups,
while significant deviations from \(1\) indicate the nearest neighbor is
more distinctive to one of the two groups. If `bootstrap = TRUE`, this
process will be repeated `num_bootstraps` times, sampling with
replacement, to obtain standard erros around the cosine similarities for
each group along with the ratio of these. If `permute = TRUE` then
permutation will be used to identify ratios that significantly deviate
from \(1\).

``` r
# set seed to replicate sampling
set.seed(2021L)

# compute ratio
cnns_df <- contrast_nns(x = immig_corpus, groups = docvars(immig_corpus, "party"), 
    pre_trained = glove_wvs, transform = TRUE, transform_matrix = khodakA, bootstrap = TRUE, 
    num_bootstraps = 20, permute = TRUE, num_permutations = 100, candidates = local_vocab, 
    N = 20, verbose = FALSE)
```

    ## starting bootstrapping 
    ## done bootstrapping 
    ## starting permutations 
    ## done with permutations

``` r
# output
head(cnns_df)
```

    ##          feature    value  std.error p.value
    ## 1    regulations 1.467040 0.02430242       0
    ## 2      enforcing 1.439571 0.02790152       0
    ## 3       enforces 1.422304 0.03173212       0
    ## 4   undocumented 1.414508 0.02159694       0
    ## 5    noncitizens 1.391774 0.01797173       0
    ## 6 naturalization 1.389030 0.02087267       0

``` r
# note: the numerator in the ratio is determined by whichever group label is
# first.
unique(docvars(immig_corpus, "party"))
```

    ## [1] "R" "D"

We plot the output of `contrast_nns` using `conText::plot_ratio()` – TO
BE ADDED.

# Embedding regression

The above framework allows us to explore semantic differences over a
given grouping variable. However, we may want to control for other
covariates and, perhap more importantly, speak to the uncertainty
sorrounding observed differences. In [Rodriguez, Spirling and Stewart
(2021)](https://github.com/prodriguezsosa/EmbeddingRegression) we show
how `a la carte` embeddings can be used within a regression-framework
that allows us to perform statistical inference using embeddings. To do
so we use the function `conText::conText()`. The data must be a quanteda
corpus with covariates stored as document variables (`docvars`).

First we specify the formula consisting of the target word of interest,
*immigration* and the set of covariates –this follows a similar syntax
as R’s `lm()` and `glm()` functions. To use all covariates in the
corpus’ docvars simply specify `immigration ~ .`. Similarly, if you
want to embed the full documents rather than a specific target word,
specify `. ~ party + gender`. Finally, the target argument can also be a
phrase, simply wrap it in quotations – e.g. `"immigration reform" ~
party + gender`. Covariates must either be binary indicator variables or
discrete variables that can be transformed into binary indicator
variables e.g. a character or factor variable with three states will be
automatically transformed by `conText()` into two seperate indicator
variables with one “baseline” state that is excluded from the
regression.

Keep in mind, `conText()` calls on `corpus_context()`, so you do not
need to create a corpus of contexts before running it, simply provide it
with the original corpus and `conText()` will find all instances of the
target word along with their respective
contexts.

``` r
model1 <- conText(formula = immigration ~ party + gender, data = cr_corpus, pre_trained = glove_wvs, 
    transform = TRUE, transform_matrix = khodakA, bootstrap = TRUE, num_bootstraps = 10, 
    stratify = TRUE, permute = TRUE, num_permutations = 100, window = 6, case_insensitive = TRUE, 
    verbose = TRUE)
```

    ## 520 instances of immigration found. 
    ## total observations included in regression: 520 
    ## starting bootstrapping 
    ## done with bootstrapping 
    ## starting permutations 
    ## done with permutations 
    ##   Coefficient Normed_Estimate   Std.Error Empirical_Pvalue
    ## 1     party_R      0.04081586 0.001388125             0.00
    ## 2    gender_M      0.03278679 0.001904842             0.03

`conText()` outputs a `conText-class` object which is simply a
`dgCMatrix class` matrix corresponding to the beta coefficients
(embeddings) with additional attributes including: a table with the
normed coefficients (excluding the intercept), their std. errors and
p-values, `@normed_coefficients` (see [Rodriguez, Spirling and Stewart
(2021)](https://github.com/prodriguezsosa/EmbeddingRegression) for
details), and the set of features used when creating the embeddings
`@features`.

Notice, we can combine the coefficients to compute the ALC embeddings
for the various combinations of the
covariates.

``` r
# D-dimensional beta coefficients the intercept in this case is the ALC embedding
# for female Democrats
model1[, 1:5]
```

    ## 3 x 5 sparse Matrix of class "dgCMatrix"
    ##                                                                            
    ## party_R      0.0179647134  0.007890508  0.01564005 0.001406862 -0.013316186
    ## gender_M    -0.0109535752  0.004144968 -0.01788107 0.004825488 -0.006383699
    ## (Intercept)  0.0003681533 -0.019493154 -0.01977436 0.041665085  0.023799931

``` r
# beta coefficients can be combined to get each group's ALC embedding
DF_wv <- model1["(Intercept)", ]  # (D)emocrat - (F)emale 
DM_wv <- model1["(Intercept)", ] + model1["gender_M", ]  # (D)emocrat - (M)ale 
RF_wv <- model1["(Intercept)", ] + model1["party_R", ]  # (R)epublican - (F)emale 
RM_wv <- model1["(Intercept)", ] + model1["party_R", ] + model1["gender_M", ]  # (R)epublican - (M)ale 

# nearest neighbors
nns(rbind(DF_wv, DM_wv), N = 10, pre_trained = glove_wvs, candidates = model1@features)
```

    ## # A tibble: 20 × 4
    ##    target feature          rank value
    ##    <fct>  <chr>           <int> <dbl>
    ##  1 DF_wv  enacting            1 0.489
    ##  2 DF_wv  reform              2 0.488
    ##  3 DF_wv  legislation         3 0.471
    ##  4 DF_wv  reforming           4 0.444
    ##  5 DF_wv  enacted             5 0.443
    ##  6 DF_wv  reforms             6 0.427
    ##  7 DF_wv  immigration         7 0.422
    ##  8 DF_wv  amendments          8 0.387
    ##  9 DF_wv  implement           9 0.382
    ## 10 DF_wv  bipartisan         10 0.378
    ## 11 DM_wv  reform              1 0.493
    ## 12 DM_wv  legislation         2 0.464
    ## 13 DM_wv  reforming           3 0.456
    ## 14 DM_wv  enacting            4 0.439
    ## 15 DM_wv  reforms             5 0.410
    ## 16 DM_wv  bipartisan          6 0.400
    ## 17 DM_wv  immigration         7 0.396
    ## 18 DM_wv  enacted             8 0.392
    ## 19 DM_wv  comprehensively     9 0.364
    ## 20 DM_wv  priorities         10 0.363

We can also access and plot –using `plot_conText()`– the normed
coefficients.

``` r
model1@normed_cofficients
```

    ##   Coefficient Normed_Estimate   Std.Error Empirical_Pvalue
    ## 1     party_R      0.04081586 0.001388125             0.00
    ## 2    gender_M      0.03278679 0.001904842             0.03

**PLOT HERE**

# Local GloVe and transformation matrix

If (a) you have a large enough corpus to train a full GloVe embeddings
model and (b) your corpus is distinctive in some way –e.g. a collection
of articles from scientific journals–, then you may want to consider
estimating your own set of embeddings and transformation matrix. The
first step is to estimate GloVe embeddings on the full corpus which you
will then use as your pre-trained embeddings.

## Estimate GloVe embeddings

This example is taken (with minor changes) from [this quanteda
vignette](https://quanteda.io/articles/pkgdown/replication/text2vec.html)
on computing GloVe embeddings using `quanteda` and `text2vec`.

``` r
library(text2vec)

#---------------------------------
# estimate transform
#---------------------------------

# tokenize
cr_toks <- tokens(cr_corpus)

# only use features that appear at least 5 times in the corpus
feats <- dfm(cr_toks, verbose = TRUE) %>% dfm_trim(min_termfreq = 5) %>% featnames()

# leave the pads so that non-adjacent words will not become adjacent
cr_toks <- tokens_select(cr_toks, feats, padding = TRUE)

# construct the feature co-occurrence matrix
cr_fcm <- fcm(cr_toks, context = "window", window = 6, count = "frequency", tri = FALSE)

# estimate glove model using text2vec
glove <- GlobalVectors$new(rank = 300, x_max = 10, learning_rate = 0.05)
wv_main <- glove$fit_transform(cr_fcm, n_iter = 10, convergence_tol = 0.001, n_threads = RcppParallel::defaultNumThreads())


wv_context <- glove$components
cr_glove_wvs <- wv_main + t(wv_context)  # word vectors

# qualitative check
local_vocab <- get_local_vocab(cr_corpus, pre_trained = cr_glove_wvs)
find_nns(cr_glove_wvs["immigration", ], pre_trained = cr_glove_wvs, N = 5, candidates = local_vocab)
```

## Estimating the transformation matrix

Given a corpus and it’s corresponding GloVe embeddings, we can compute a
corresponding transformation matrix using
`conText::compute_transform()`.

``` r
# compute transform
cr_transform <- compute_transform(x = cr_fcm, pre_trained = cr_glove_wvs, weighting = 100)
```

``` r
#---------------------------------
# check
#---------------------------------

# create document-embedding matrix using our locally trained GloVe embeddings and
# transformation matrix
immig_dem_local <- dem(x = immig_dfm, pre_trained = cr_glove_wvs, transform = TRUE, 
    transform_matrix = cr_transform, verbose = TRUE)

# take the column average to get a single 'corpus-wide' embedding
immig_wv_local <- colMeans(immig_dem_local)

# find nearest neighbors for overall immigraiton embedding
find_nns(immig_wv_local, pre_trained = cr_glove_wvs, N = 10, candidates = immig_dem_local@features)

# we can also compare to corresponding pre-trained embedding
sim2(x = matrix(immig_wv_local, nrow = 1), y = matrix(cr_glove_wvs["immigration", 
    ], nrow = 1), method = "cosine", norm = "l2")
```

# Other

## Feature embedding matrix

We can use a featur-co-occurrence matrix to simultaneously embed
multiple features.

``` r
# terms of interest
features <- c("immigration", "welfare")

# tokenize texts
cr_toks <- tokens(cr_corpus)

# create feature co-occurrence matrix (set tri = FALSE to work with fem)
cr_fcm <- fcm(cr_toks, context = "window", window = 6, count = "frequency", tri = FALSE) %>% 
    .[features, ]  # subset rows to features of interest

# compute feature-embedding matrix
cr_fem <- fem(cr_fcm, pre_trained = glove_wvs, transform = TRUE, transform_matrix = khodakA, 
    verbose = FALSE)

# find nearest neighbors
nns(x = cr_fem, pre_trained = glove_wvs, N = 10, candidates = NULL)
```

## Embedding full documents

In this example we use open-ended responses to ANES 2016 question: “what
are the most important issues facing the country?”. Instead of embedding
contexts around a given set of target words we embed each full
open-ended response and average over respondent ideology.

``` r
# ANES 2016 open-ends on most important issues facing country
anes2016 <- readRDS(paste0(path_to_data, "anes2016.rds")) %>% select(response, ideology, 
    gender) %>% na.omit()

## build corpus
anes2016_corpus <- corpus(anes2016$response, docvars = anes2016[, c("ideology", "gender")])

# tokenize texts
anes2016_toks <- tokens(anes2016_corpus)

# create document-feature matrix
anes2016_dfm <- dfm(anes2016_toks)

# create document-embedding matrix using a la carte
anes2016_dem <- dem(x = anes2016_dfm, pre_trained = glove_wvs, transform = TRUE, 
    transform_matrix = khodakA, verbose = TRUE)
```

    ## the following documents could not be embedded due lack of overlap with pre-trained embeddings provided: 
    ##  text493 text964 text3941 text8189

``` r
# group by ideology
anes2016_wv_ideology <- dem_group(x = anes2016_dem, groups = anes2016_dem@docvars$ideology)

# check nearest neighbors
nns(x = anes2016_wv_ideology, pre_trained = glove_wvs, N = 10, candidates = NULL)
```

    ## # A tibble: 30 × 4
    ##    target       feature       rank value
    ##    <fct>        <chr>        <int> <dbl>
    ##  1 Conservative entitlements     1 0.491
    ##  2 Conservative entitlement      2 0.490
    ##  3 Conservative welfare          3 0.481
    ##  4 Conservative unemployment     4 0.478
    ##  5 Conservative immigration      5 0.478
    ##  6 Conservative homelessness     6 0.473
    ##  7 Conservative ills             7 0.466
    ##  8 Conservative joblessness      8 0.464
    ##  9 Conservative insecurity       9 0.452
    ## 10 Conservative combatting      10 0.451
    ## # … with 20 more rows
