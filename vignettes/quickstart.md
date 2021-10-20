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

The **conText** package includes two sample corpora:
- `cr_sample_corpus`, a sample of the U.S. Congressional Records (Sessions 111th - 114th)
- `anes2016_sample_corpus`, a sample of the ANES 2016 open-ended question on “most important issues facing the country”; 
It also includes:
- a subset of [Stanford NLP’s](https://nlp.stanford.edu/projects/glove/)
300-dimensional GloVe embeddings, `glove_subset`
- a transformation matrix ,`khodakA`, computed by [Khodak et
al.](https://arxiv.org/abs/1805.05388) for the aforementioned GloVe
300-dimensional embeddings (see documentation for details). 

For the following guide we will be using the full versions of these datasets
which we’ve made available
[here](https://www.dropbox.com/sh/6dfr3i6no6nzvm0/AADqk6HFTvZJGNyu3FuO62kGa?dl=0).

## Load data

``` r
# other libraries
library(quanteda)
library(dplyr)
library(stringr)

# path to data
path_to_data <- "~/Dropbox/GitHub/large_data/conText/data/"  # set this path to wherever you stored the data files

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

# build corpus
cr_corpus <- corpus(cr_sample$speech, docvars = cr_sample[, c("party", "gender")])
```

## Pre-processing

``` r
# tokenize corpus
toks <- tokens(cr_corpus, remove_punct = T, remove_symbols = T, remove_numbers = T, 
    remove_separators = T)

# clean out stopwords and words with 2 or fewer characters
toks_nostop <- tokens_select(toks, pattern = stopwords("en"), selection = "remove", 
    min_nchar = 3)

# only use features that appear at least 5 times in the corpus
feats <- dfm(toks_nostop, tolower = T, verbose = TRUE) %>% dfm_trim(min_termfreq = 5) %>% 
    featnames()

# leave the pads so that non-adjacent words will not become adjacent
toks <- tokens_select(toks_nostop, feats, padding = TRUE)
```

# The building blocks of `a la carte` embeddings

## 1\. Identify the relevant contexts

Suppose we are interested in the semantics surrounding the word
*immigration* in the U.S. Congress during the Obama years (sessions
111th - 114th). We begin by identifying all instances of the **target**
term –*immigration*– in our corpus and, for each instance, store it’s
context– the N words (N = 6 in this example) preceding and following the
instance. We can do this in one step using the `conText::tokens_context`
function. Notice, both the input, `x`, and the output are a quanteda
`tokens-object`. Moreover, each document in `immig_toks` –tokens of a
context around an instance of *immigration*– inherits the document
variables (`docvars`) of the document from whence it came and can be
retrieved using `docvars`.

``` r
# build a corpus of contexts sorrounding the target term 'immigration'
immig_toks <- tokens_context(x = toks, pattern = "immigration", window = 6L)
```

    ## 520 instances of "immigration" found.

## 2\. Build a document-feature-matrix

Given a set of tokenized contexts, we next build it’s corresponding
document-feature-matrix, namely a matrix where each row represents a
given document’s vector of feature counts. We do this using quanteda’s
[`dfm`](https://quanteda.io/reference/dfm.html) function.

``` r
# create document-feature matrix
immig_dfm <- dfm(immig_toks)
immig_dfm[1:3, 1:3]
```

    ## Document-feature matrix of: 3 documents, 3 features (66.67% sparse) and 3 docvars.
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
column-averaging the resulting vectors, and multiplying by the
transformation matrix. This “transforms” a *sparse* V-dimensional vector
(a vector of feature counts, with V = number of features in the corpus)
into a *dense* D-dimensional vector (a D-dimensional embedding, with D =
dimensions of pre-trained embeddings). We do this using
`conText::dem`–`dem` standing for *document-embedding-matrix*. Keep in
mind, only those features that appear in the set of pre-trained
embeddings will be used in computing a document’s embedding. Documents
with no features overlapping with the pre-trained provided are dropped.

`conText::dem`–`dem` outputs a `dem-class` object which is similar in
many ways to `dfm-class` object. Importantly, a `dem-class` object
inherits all the `docvars` from the `dfm` used to compute it (except
those of documents that could not be embedded). Additionally, a
`dem-class` object will store other useful attributes, including the
names of the documents that were embedded and the vector of features
used.

``` r
# create document-embedding matrix using a la carte
immig_dem <- dem(x = immig_dfm, pre_trained = glove_wvs, transform = TRUE, transform_matrix = khodakA, 
    verbose = TRUE)

## each embedding document is associated with it corresponding docvars
## head(immig_dem@docvars)

## you can check which documents were not embedded due to lack of overlapping
## features (in this example all documents are embedded) note: `quanteda`
## functions like `docvars()` and `docnames()` don't work on `dem` objects, so you
## will have to call the attributes directly.  setdiff(docnames(immig_dfm),
## immig_dem@Dimnames$docs)

## vector of features used to create the embeddings head(immig_dem@features)
```

## 4\. Average over document embeddings

We now have an ALC embedding for each context of `immigration` in our
Congressional Record corpus. To get a single embedding for immigration,
we can simply take the column-average.

    ## [1]   1 300

However, we are usually interested in exploring semantic differences
across groups. To do so, we can average within a grouping variable
defined by one or a combination of the `docvars`. We do this using
`conText::dem_group` (very similar in flavor to quanteda’s
[`dfm_group`](https://tutorials.quanteda.io/basic-operations/dfm/dfm_group/)).
This results in an `a la carte` embedding of `immigration` for each
group.

``` r
# to get group-specific embeddings, average within party
immig_wv_party <- dem_group(immig_dem, groups = immig_dem@docvars$party)
dim(immig_wv_party)
```

    ## [1]   2 300

## 5\. Comparing group embeddings

### Nearest neighbors

Given each party’s embeddings, we can proceed to do some exploratory
analysis. First, we can evaluate differences in nearest neighbors
–features with the highest cosine-similarity with each group
embedding– using `conText::nns()`. We can use the `candidates`
argument to limit the set of features we want `nns` to consider as
candidate nearest neighbors. In this example we limit candidates to
those features that appear in our corpus by setting `candidates =
immig_wv_party@features` (otherwise, any word in the set of pre-trained
embeddings can be a
candidate).

``` r
# find nearest neighbors by party setting as_list = TRUE outputs a list with a
# separate tibble for each group
nns(immig_wv_party, pre_trained = glove_wvs, N = 5, candidates = immig_wv_party@features, 
    as_list = FALSE)
```

    ## # A tibble: 10 × 4
    ##    target feature      rank value
    ##    <fct>  <chr>       <int> <dbl>
    ##  1 D      legislation     1 0.559
    ##  2 R      immigration     1 0.553
    ##  3 R      legislation     2 0.536
    ##  4 D      reform          2 0.536
    ##  5 R      enacting        3 0.522
    ##  6 D      enacting        3 0.513
    ##  7 R      enacted         4 0.504
    ##  8 R      laws            5 0.496
    ##  9 D      immigration     4 0.481
    ## 10 D      bipartisan      5 0.477

### Cosine similarity

We may be interested in exploring the relationship between each group’s
embedding of `immigration` and a specific set of features. For example,
how does each party’s understanding of immigration compares on the
“reform” vs. “enforcement” dimension. To answer this, we use
`conText::cos_sim()`. Results (on this limited sample of the data)
suggest Democrats are more prone to speak of “reform” in the context of
“immigration” whereas Republicans are more likely to speak of
“enforcement”.

``` r
# compute the cosine similarity between each party's embedding and a specific set
# of features
cos_sim(immig_wv_party, pre_trained = glove_wvs, features = c("reform", "enforcement"), 
    as_list = FALSE)
```

    ##   target     feature     value
    ## 1      D      reform 0.5361718
    ## 2      R      reform 0.3865946
    ## 3      D enforcement 0.3242427
    ## 4      R enforcement 0.4590991

### Nearest neighbors cosine similarity ratio

A third exploratory function included in `conText` is `nns_ratio`. Given
two group embeddings (`x`) and a set of candidates nearest neighbors
(`candidates`), `nns_ratio` computes the ratio of cosine similarities
between group embeddings and features. If `N` is defined, this ratio is
computed for union of the top `N` nearest neighbors. This ratio is
useful to identify features that are most “discriminant” of each party.
You can use the `numerator` argument to define which group’s embedding
to set as the numerator in this ratio. Below we set `numerator = "R"`,
so features with values greater than \(1\) identify features that are
more “discriminant” of Republican contexts of immigration (i.e. the
cosine similarity between the those features and the Republican
embedding is greater than their cosine similarity with the Democrat
embedding). Similarly, features with values lower than \(1\) identify
features that are more “discriminant” of Democrat contexts of
immigration.

``` r
# compute the cosine similarity between each party's embedding and a specific set
# of features
nns_ratio(x = immig_wv_party, N = 10, numerator = "R", candidates = immig_wv_party@features, 
    pre_trained = glove_wvs, verbose = FALSE)
```

    ##           feature     value
    ## 1     regulations 1.5144344
    ## 2    undocumented 1.4452839
    ## 3     enforcement 1.4159120
    ## 4  naturalization 1.3825175
    ## 5        illegals 1.3589015
    ## 6            laws 1.3497194
    ## 7     immigration 1.1487697
    ## 8         enacted 1.0934001
    ## 9        enacting 1.0165315
    ## 10     amendments 0.9810503
    ## 11    legislation 0.9587812
    ## 12        welfare 0.9011913
    ## 13      reforming 0.7998898
    ## 14         reform 0.7210275
    ## 15        reforms 0.7196747
    ## 16     bipartisan 0.6367251

## Multiple Keywords

In the above example we had one keyword, `immigration`. However, we can
also explore the semantics of multiple keywords simultaneously,
including phrases\! We simply provide a vector of patterns in the
`pattern` argument of `tokens_context`.

``` r
# build a corpus of contexts sorrounding the target term 'immigration'
mkws_toks <- tokens_context(x = toks, pattern = c("immigration", "welfare", "immigration reform", 
    "economy"), window = 6L, verbose = FALSE)

# create document-feature matrix
mkws_dfm <- dfm(mkws_toks)

# create document-embedding matrix using a la carte
mkws_dem <- dem(x = mkws_dfm, pre_trained = glove_wvs, transform = TRUE, transform_matrix = khodakA, 
    verbose = FALSE)

# get embeddings for each pattern
mkws_wvs <- dem_group(mkws_dem, groups = mkws_dem@docvars$pattern)

# find nearest neighbors for each keyword
mkws_nns <- nns(mkws_wvs, pre_trained = glove_wvs, N = 5, candidates = mkws_wvs@features, 
    as_list = TRUE)

# to check results for a given pattern
mkws_nns[["immigration reform"]]
```

    ## # A tibble: 5 × 4
    ##   target             feature        rank value
    ##   <chr>              <chr>         <int> <dbl>
    ## 1 immigration reform bipartisan        1 0.539
    ## 2 immigration reform legislation       2 0.536
    ## 3 immigration reform enact             3 0.517
    ## 4 immigration reform comprehensive     4 0.512
    ## 5 immigration reform enacting          5 0.477

This can also be useful if we wanted to explore *group differences* in
the semantics of a given “topic” where we define a topic by a vector of
keywords. Note, these “topical” embeddings are computed off of the
collection of contexts around the set of pattern words provided. So, in
the example below, the contexts around each of `"immigration"`,
`"immigrant"` and `"immigration reform"` are treated as a single
collection of contexts for the purposes of computing each group’s
embedding.

``` r
# build a corpus of contexts sorrounding the immigration related words
topical_toks <- tokens_context(x = toks, pattern = c("immigration", "immigrant", 
    "immigration reform"), window = 6L, verbose = FALSE)

# create document-feature matrix
topical_dfm <- dfm(topical_toks)

# create document-embedding matrix using a la carte
topical_dem <- dem(x = topical_dfm, pre_trained = glove_wvs, transform = TRUE, transform_matrix = khodakA, 
    verbose = FALSE)

# get 'topical' embeddings for each party
topical_wvs <- dem_group(topical_dem, groups = topical_dem@docvars$party)

# find nearest neighbors for each keyword
nns(topical_wvs, pre_trained = glove_wvs, N = 5, candidates = topical_wvs@features, 
    as_list = FALSE)
```

    ## # A tibble: 10 × 4
    ##    target feature       rank value
    ##    <fct>  <chr>        <int> <dbl>
    ##  1 R      immigration      1 0.548
    ##  2 D      legislation      1 0.542
    ##  3 R      legislation      2 0.512
    ##  4 R      enacting         3 0.506
    ##  5 R      undocumented     4 0.497
    ##  6 D      enacting         2 0.495
    ##  7 D      reform           3 0.491
    ##  8 D      immigration      4 0.484
    ##  9 R      enacted          5 0.482
    ## 10 D      bipartisan       5 0.470

# Wrapper functions

The above functions give users a lot of flexibility, however, it can be
cumbersome to write each step every time one wants to do some
exploratory analysis on a corpus. In this section we’ll look into a
collection of wrapper functions that allow users to go straight from
`corpus` to results. Each of the three exploratory functions `nns`,
`cos_sim` and `nns_ratio` has its own wrapper function.

An added advantage of the wrapper functions is that they make it easy to
obtain standard errors around the sample statistics of interest via
*bootstrapping*. Bootstrapping works by sampling with replacement
contexts from our corpus of contexts (within group if `groups` is
specified) and going through the above steps for each of our exploratory
analysis functions (i.e. computing an `a la carte` embedding on each
bootstrapped sample, then computing the cosine similarities etc.). Let’s
start with `conText::get_nns`, a wrapper function to compare nearest
neighbors between groups.

### Nearest neighbors

We will use the same tokenized corpus of “immigration” contexts
–`immig_toks` as above and again limit candidates nearest neighbors to
the set of features in our corpus. To group by `party` we set `groups =
docvars(immig_corpus, 'party')`. If no grouping variable is provided,
the full set of (tokenized) contexts are aggregated into one single
embedding. To estimate standard errors for the cosine similarities
between each party’s embedding and their corresponding nearest neighbors
we set `bootstrap = TRUE` and define the desired number of bootstraps
with `num_bootstrap`. Notice the output now has an additional column
`std.error`. This is simply the standard deviation of the sampling
distribution of cosine similarities obtained via bootstrapping. Note,
values may differ slightly to the step-by-step process outlined above as
they represent averages over bootstrapped samples.

``` r
# we limit candidates to features in the corpus
feats <- featnames(dfm(immig_toks))

# compare nearest neighbors between groups
set.seed(2021L)
immig_party_nns <- get_nns(x = immig_toks, N = 10, groups = docvars(immig_toks, "party"), 
    candidates = feats, pre_trained = glove_wvs, transform = TRUE, transform_matrix = khodakA, 
    bootstrap = TRUE, num_bootstraps = 10, as_list = TRUE)

# nearest neighbors of 'immigration' for Republican party
immig_party_nns[["R"]]
```

    ## # A tibble: 10 × 5
    ##    target feature         rank value std.error
    ##    <chr>  <chr>          <int> <dbl>     <dbl>
    ##  1 R      immigration        1 0.551    0.0143
    ##  2 R      legislation        2 0.525    0.0241
    ##  3 R      enacting           3 0.511    0.0153
    ##  4 R      enacted            4 0.496    0.0228
    ##  5 R      laws               5 0.490    0.0248
    ##  6 R      undocumented       6 0.474    0.0233
    ##  7 R      enforcement        7 0.461    0.0278
    ##  8 R      illegals           8 0.436    0.0188
    ##  9 R      naturalization     9 0.430    0.0172
    ## 10 R      regulations       10 0.422    0.0227

### Cosine similarity

`conText::get_cos_sim` is a wrapper function for `cos_sim`, used to
evaluate how similar each group’s embedding is to a set of features of
interest. We use the same immigration corpus as above and again set
`bootstrap = TRUE` to obtain standard errors for the cosine
similarities.

``` r
# compute the cosine similarity between each group's embedding and a specific set
# of features
set.seed(2021L)
get_cos_sim(x = immig_toks, groups = docvars(immig_toks, "party"), features = c("reform", 
    "enforce"), pre_trained = glove_wvs, transform = TRUE, transform_matrix = khodakA, 
    bootstrap = TRUE, num_bootstraps = 10, as_list = FALSE)
```

    ## # A tibble: 4 × 4
    ##   target feature value std.error
    ##   <fct>  <fct>   <dbl>     <dbl>
    ## 1 D      reform  0.532   0.0112 
    ## 2 D      enforce 0.276   0.00887
    ## 3 R      reform  0.379   0.0247 
    ## 4 R      enforce 0.403   0.0285

### Nearest neighbors cosine similarity ratio

Finally, `conText::get_nns_ratio()` is a wrapper function for
`nns_ratio`, used to gauge how discriminant nearest neighbors are of
each group embedding by using the ratio of cosine similarities. Unlike
`get_nns` and `get_cos_sim`, a binary `groups` argument must be
provided. To control which group to use as the numerator in this ratio,
we use the `numerator` argument. As before, we limit the candidate
nearest neighbors to the set of local features that meet our desired
count threshold and set `bootstrap = TRUE` to get standard errors, in
this case of the cosine similarity ratios.

Finally, `get_nns_ratio` allows us to make inferences using a
permutation test, specifically around the absolute deviation of the
observed cosine similarity ratios from \(1\) (this captures how
discriminant a given nearest neighbor is). Specifically, for each
permutation, the grouping variable is randomly shuffled and the absolute
deviation of the cosine similarity ratios from \(1\) is computed. The
empirical p.value is then the proportion of these deviations that are
larger than the observed deviation.

The output of `get_nns_ratio` contains three additional columns
(relative to `nns_ratio`) if `bootstrap = TRUE` and `permute = TRUE`.
These are the standard errors around the cosine similarity ratios, the
corresponding (empirical) `p.value` and a `group` variable identifying
which group the nearest neighbor belonged to – `shared` means it
appeared in both groups’ top \(N\) nearest neighbors.

``` r
# compute ratio
set.seed(2021L)
immig_nns_ratio <- get_nns_ratio(x = immig_toks, N = 10, groups = docvars(immig_toks, 
    "party"), numerator = "R", candidates = feats, pre_trained = glove_wvs, transform = TRUE, 
    transform_matrix = khodakA, bootstrap = TRUE, num_bootstraps = 10, permute = TRUE, 
    num_permutations = 10, verbose = FALSE)
```

    ## starting bootstraps 
    ## done with bootstraps 
    ## starting permutations 
    ## done with permutations

``` r
head(immig_nns_ratio)
```

    ## # A tibble: 6 × 5
    ##   feature        value std.error p.value group
    ##   <chr>          <dbl>     <dbl>   <dbl> <chr>
    ## 1 regulations     1.54    0.0762       0 R    
    ## 2 undocumented    1.50    0.129        0 R    
    ## 3 enforcement     1.50    0.0738       0 R    
    ## 4 naturalization  1.46    0.105        0 R    
    ## 5 illegals        1.39    0.113        0 R    
    ## 6 laws            1.38    0.0918       0 R

**conText** also includes a plotting function specific to
`get_nns_ratio()`, providing a nice visualization of its output. `alpha`
defines the desired significance threshold to denote “significant”
results on the plot. Also, you can choose between two different
visualizations of the same results using the `horizontal`
argument.

``` r
plot_nns_ratio(x = immig_nns_ratio, alpha = 0.01, horizontal = TRUE)
```

<img src="https://github.com/prodriguezsosa/conText/blob/develop/vignettes/nns_ratio.png" width="100%" />

# Embedding regression

The above framework allows us to explore semantic differences over a
given grouping variable. However, we may want to control for other
covariates and, perhaps more importantly, speak to the uncertainty
surrounding observed differences. In [Rodriguez, Spirling and Stewart
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
automatically transformed by `conText()` into two separate indicator
variables with one “baseline” state that is excluded from the
regression.

Keep in mind, `conText()` calls on `corpus_context()`, so you do not
need to create a corpus of contexts before running it, simply provide it
with the original corpus and `conText()` will find all instances of the
target word along with their respective contexts.

``` r
set.seed(2021L)
model1 <- conText(formula = immigration ~ party + gender, data = toks, pre_trained = glove_wvs, 
    transform = TRUE, transform_matrix = khodakA, bootstrap = TRUE, num_bootstraps = 10, 
    stratify = TRUE, permute = TRUE, num_permutations = 100, window = 6, case_insensitive = TRUE, 
    verbose = FALSE)
```

    ##   Coefficient Normed_Estimate   Std.Error Empirical_Pvalue
    ## 1     party_R      0.04703982 0.003844448             0.00
    ## 2    gender_M      0.03852159 0.004149152             0.13

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
# for female Democrats beta coefficients can be combined to get each group's ALC
# embedding
DF_wv <- model1["(Intercept)", ]  # (D)emocrat - (F)emale 
DM_wv <- model1["(Intercept)", ] + model1["gender_M", ]  # (D)emocrat - (M)ale 
RF_wv <- model1["(Intercept)", ] + model1["party_R", ]  # (R)epublican - (F)emale 
RM_wv <- model1["(Intercept)", ] + model1["party_R", ] + model1["gender_M", ]  # (R)epublican - (M)ale 

# nearest neighbors
nns(rbind(DF_wv, DM_wv), N = 10, pre_trained = glove_wvs, candidates = model1@features)
```

    ## $DF_wv
    ## # A tibble: 10 × 4
    ##    target feature      rank value
    ##    <chr>  <chr>       <int> <dbl>
    ##  1 DF_wv  legislation     1 0.559
    ##  2 DF_wv  enacting        2 0.534
    ##  3 DF_wv  reform          3 0.531
    ##  4 DF_wv  enacted         4 0.487
    ##  5 DF_wv  immigration     5 0.482
    ##  6 DF_wv  bipartisan      6 0.456
    ##  7 DF_wv  reforms         7 0.453
    ##  8 DF_wv  reforming       8 0.445
    ##  9 DF_wv  amendments      9 0.444
    ## 10 DF_wv  welfare        10 0.432
    ## 
    ## $DM_wv
    ## # A tibble: 10 × 4
    ##    target feature      rank value
    ##    <chr>  <chr>       <int> <dbl>
    ##  1 DM_wv  legislation     1 0.543
    ##  2 DM_wv  reform          2 0.537
    ##  3 DM_wv  enacting        3 0.492
    ##  4 DM_wv  bipartisan      4 0.482
    ##  5 DM_wv  immigration     5 0.470
    ##  6 DM_wv  reforming       6 0.450
    ##  7 DM_wv  reforms         7 0.438
    ##  8 DM_wv  enacted         8 0.432
    ##  9 DM_wv  welfare         9 0.410
    ## 10 DM_wv  amendments     10 0.395

We can also access and plot –using `plot_conText()`– the normed
coefficients.

``` r
model1@normed_cofficients
```

    ##   Coefficient Normed_Estimate   Std.Error Empirical_Pvalue
    ## 1     party_R      0.04703982 0.003844448             0.00
    ## 2    gender_M      0.03852159 0.004149152             0.13

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
find_nns(cr_glove_wvs["immigration", ], pre_trained = cr_glove_wvs, N = 5, candidates = feats)
```

## Estimating the transformation matrix

Given a corpus and it’s corresponding GloVe embeddings, we can compute a
corresponding transformation matrix using
`conText::compute_transform()`.

``` r
# compute transform
cr_transform <- compute_transform(x = cr_fcm, pre_trained = cr_glove_wvs, weighting = 100)
```

Below we provide a couple of sanity checks to make sure the computed
transformation matrix produces sensical results.

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

We can use a feature-co-occurrence matrix to simultaneously embed
multiple features. This can be useful to (1) compare two corpora along
multiple features simultaneously and/or (2) estimate context-specific
feature embeddings that can subsequently be fed into a downstream
classification task. We’ll focus on (1) here. First, we remove
stop-words and low-frequency features from our tokenized corpus. Notice,
we set `padding = TRUE` here to avoid making non-adjacent words adjacent
prior to computing the embeddings. We then compute a separate `fcm` for
each group (`party` in our case).

``` r
# tokenize texts
cr_toks <- tokens(cr_corpus)

# clean out stopwords
cr_toks_nostop <- tokens_select(cr_toks, pattern = stopwords("en"), selection = "remove")

# only use features that appear at least 5 times in the corpus
feats <- dfm(cr_toks_nostop, tolower = T, verbose = TRUE) %>% dfm_trim(min_termfreq = 5) %>% 
    featnames()

# leave the pads so that non-adjacent words will not become adjacent
cr_toks <- tokens_select(cr_toks_nostop, feats, padding = TRUE)

# create feature co-occurrence matrix for each party (set tri = FALSE to work
# with fem)
cr_fcm_D <- fcm(cr_toks[docvars(cr_toks, "party") == "D", ], context = "window", 
    window = 6, count = "frequency", tri = FALSE)
cr_fcm_R <- fcm(cr_toks[docvars(cr_toks, "party") == "R", ], context = "window", 
    window = 6, count = "frequency", tri = FALSE)
```

Given an `fcm` for each group, we can proceed to compute a corresponding
“feature-embedding-matrix” for each group.

``` r
# compute feature-embedding matrix
cr_fem_D <- fem(cr_fcm_D, pre_trained = glove_wvs, transform = TRUE, transform_matrix = khodakA, 
    verbose = FALSE)
cr_fem_R <- fem(cr_fcm_R, pre_trained = glove_wvs, transform = TRUE, transform_matrix = khodakA, 
    verbose = FALSE)

# cr_fem will contain an embedding for each feature
cr_fem_D[1:5, 1:3]
```

    ## 5 x 3 sparse Matrix of class "dgCMatrix"
    ##                                                 
    ## support     -0.013577516 -0.03155166 -0.02576430
    ## commonsense  0.001280815 -0.03109154 -0.02514370
    ## amendment   -0.012642031 -0.04976195 -0.03497852
    ## urge        -0.018224679 -0.03366194 -0.02769026
    ## colleagues  -0.004263367 -0.02473565 -0.02011835

Finally, we can use the `conText::feature_sim` function to compute
“horizontal” cosine similarities between both `fem`s for the set of
overlapping features. The output of `feature_sim` is ranked from least
similar to most similar features.

``` r
feat_comp <- feature_sim(x = cr_fem_R, y = cr_fem_D)

# least similar features
head(feat_comp)
```

    ##        feature      value
    ## 1    robertson -0.1530734
    ## 2     allstate -0.1266684
    ## 3      millers -0.1203797
    ## 4         webb -0.1134516
    ## 5     laughlin -0.1081735
    ## 6 cheerleaders -0.1016590

``` r
# most similar features
tail(feat_comp)
```

    ##          feature     value
    ## 14802    support 0.9916075
    ## 14803    consent 0.9916744
    ## 14804 bipartisan 0.9920280
    ## 14805    ranking 0.9921178
    ## 14806 colleagues 0.9927276
    ## 14807  unanimous 0.9931366

## Embedding full documents

In all of examples above we’ve focused on contexts defined as “windows”
around a target word. In [Rodriguez et al.
(2021)](https://github.com/prodriguezsosa/EmbeddingRegression) we show
that the same procedure works well on contexts defined as the full
document, as long as the full document is relatively short. We discuss
the example of analyzing open-ended responses to ANES 2016 question:
“what are the most important issues facing the country?”. Instead of
embedding contexts around a given target word, we embed each full
open-ended response and average over respondent ideology.

``` r
# ANES 2016 open-ends on most important issues facing country
anes2016 <- readRDS(paste0(path_to_data, "anes2016.rds")) %>% select(response, ideology, 
    gender) %>% na.omit()

# build corpus
anes2016_corpus <- corpus(anes2016$response, docvars = anes2016[, c("ideology", "gender")])

# tokenize corpus
toks <- tokens(anes2016_corpus, remove_punct = T, remove_symbols = T, remove_numbers = T, 
    remove_separators = T)

# clean out stopwords and words with 2 or fewer characters
toks_nostop <- tokens_select(toks, pattern = stopwords("en"), selection = "remove", 
    min_nchar = 3)

# only use features that appear at least 5 times in the corpus
feats <- dfm(toks_nostop, tolower = T, verbose = TRUE) %>% dfm_trim(min_termfreq = 5) %>% 
    featnames()

# leave the pads so that non-adjacent words will not become adjacent
toks <- tokens_select(toks_nostop, feats, padding = TRUE)

# create document-feature matrix
anes2016_dfm <- dfm(toks)

# create document-embedding matrix using a la carte
anes2016_dem <- dem(x = anes2016_dfm, pre_trained = glove_wvs, transform = TRUE, 
    transform_matrix = khodakA, verbose = FALSE)

# group by ideology
anes2016_wv_ideology <- dem_group(x = anes2016_dem, groups = anes2016_dem@docvars$ideology)

# check nearest neighbors
anes_nns <- nns(x = anes2016_wv_ideology, pre_trained = glove_wvs, N = 5, candidates = anes2016_wv_ideology@features, 
    as_list = TRUE)

# check nns for Conservatives
anes_nns[["Conservative"]]
```

    ## # A tibble: 5 × 4
    ##   target       feature       rank value
    ##   <chr>        <chr>        <int> <dbl>
    ## 1 Conservative unemployment     1 0.490
    ## 2 Conservative welfare          2 0.489
    ## 3 Conservative immigration      3 0.486
    ## 4 Conservative entitlement      4 0.474
    ## 5 Conservative homelessness     5 0.471

Recall we can run most of the code above by simply calling `get_nns`
directly on the corpus.

``` r
# compare nearest neighbors between groups
anes_nns2 <- get_nns(x = toks, N = 10, groups = docvars(toks, "ideology"), candidates = feats, 
    pre_trained = glove_wvs, transform = TRUE, transform_matrix = khodakA, bootstrap = TRUE, 
    num_bootstraps = 10, as_list = TRUE)

anes_nns2[["Conservative"]]
```

    ## # A tibble: 10 × 5
    ##    target       feature       rank value std.error
    ##    <chr>        <chr>        <int> <dbl>     <dbl>
    ##  1 Conservative unemployment     1 0.489   0.00870
    ##  2 Conservative welfare          2 0.487   0.00464
    ##  3 Conservative immigration      3 0.480   0.00746
    ##  4 Conservative entitlement      4 0.473   0.00540
    ##  5 Conservative homelessness     5 0.472   0.00623
    ##  6 Conservative entitlements     6 0.467   0.00610
    ##  7 Conservative poverty          7 0.450   0.00669
    ##  8 Conservative insecurity       8 0.450   0.00693
    ##  9 Conservative inequality       9 0.437   0.00618
    ## 10 Conservative immigrants      10 0.420   0.00378

To run a regression on the full document rather than a specific target
word use the `.~` operator.

``` r
set.seed(2021L)
model1 <- conText(formula = . ~ ideology + gender, data = toks, pre_trained = glove_wvs, 
    transform = TRUE, transform_matrix = khodakA, bootstrap = TRUE, num_bootstraps = 10, 
    stratify = TRUE, permute = TRUE, num_permutations = 100, window = 6, case_insensitive = TRUE, 
    verbose = FALSE)
```

    ##         Coefficient Normed_Estimate   Std.Error Empirical_Pvalue
    ## 1  ideology_Liberal      0.04343701 0.002878587                0
    ## 2 ideology_Moderate      0.02749989 0.003751723                0
    ## 3       gender_male      0.01842375 0.002045394                0
