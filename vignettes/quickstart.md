Quick Start Guide
================

# Preliminaries

**conText** was designed to work closely with
[quanteda](https://quanteda.io), as such, most functions will expect a
quanteda object such as a *tokens* or *dfm* or *fcm* object. To make the
most of **conText**, we recommend that you familiarize yourself with
some of quanteda’s basic functionalities (see [Quanteda Quick Start
Guide](https://quanteda.io/articles/quickstart.html)).

# Setup

## Installing the package

Since **conText** is not *yet* available on CRAN you will need to
install it directly from GitHub.

``` r
devtools::install_github("prodriguezsosa/conText")
```

## Load package

``` r
library(conText)

# other libraries used in this guide
library(quanteda)
library(dplyr)
library(conText)
library(text2vec)
```

# Data

To use **conText** you will need three objects:

1.  A (quanteda) **corpus** with the documents and corresponding
    document variables (covariates) you want to evaluate.
2.  A set of (GloVe) **pre-trained embeddings**.
3.  A **transformation matrix** specific to the pre-trained embeddings.

In this guide we will use the sample objects included in the package:
`cr_sample_corpus`, `cr_glove_subset` and `cr_transform`. Note, these
are only meant to illustrate function implementations, keep in mind
their (small) size when interepreting results. We’ve made available the
full versions of these objects
[here](https://www.dropbox.com/sh/6dfr3i6no6nzvm0/AADqk6HFTvZJGNyu3FuO62kGa?dl=0).

## Pre-processing

Pre-processing is always context-specific (no pun intended). We
generally recommend not “overly” pre-processing a corpus. Below we
outline how a typical pre-processing pipeline might look. Note,
`cr_sample_corpus` already has some minor pre-processing so some of
these steps may be redundant but are left for completeness. It is
generally a good idea to set `padding = TRUE` in `tokens_select()` to
avoid making non-adjacent words adjacent prior to computing the
embeddings. This is increasingly important as more features are
removed.

``` r
# tokenize corpus removing unnecessary (i.e. semantically uninformative) elements
toks <- tokens(cr_sample_corpus, remove_punct = T, remove_symbols = T, remove_numbers = T, 
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

Suppose we are interested in the semantics surrounding the word
“immigration” in the U.S. Congress during the Obama years (sessions
111th - 114th).

## 1\. Build a (tokenized) corpus of contexts

We begin by identifying all instances of the *target* term
–“immigration”– in our corpus and, for each instance, store it’s
context– the N words (N = 6 in this example) preceding and following the
instance, and tokenize. Given our tokenized corpus, `toks`, we can do
this in one step using `conText::tokens_context()` –a wrapper function
for quanteda::kwic(). Notice, both the input, `x`, and the output are a
quanteda `tokens` objects. Each document in `immig_toks` –tokens of a
context around an instance of *immigration*– inherits the document
variables (`docvars`) of the document from whence it came, along with a
column registering it’s the pattern used. This information can be
retrieved using
`docvars()`.

``` r
# build a tokenized corpus of contexts sorrounding the target term 'immigration'
immig_toks <- tokens_context(x = toks, pattern = "immigr*", window = 6L)
```

    ## 1419 instances of "immigr*" found.

``` r
head(docvars(immig_toks), 3)
```

    ##   pattern party gender
    ## 1 immigr*     D      F
    ## 2 immigr*     D      F
    ## 3 immigr*     D      F

## 2\. Build a document-feature-matrix

Given a tokenized corpus of contexts, we next build it’s corresponding
document-feature-matrix, namely a matrix where each row represents a
given document’s vector of feature counts. We do this using quanteda’s
`dfm()` function.

``` r
# build document-feature matrix
immig_dfm <- dfm(immig_toks)
immig_dfm[1:3, 1:3]
```

    ## Document-feature matrix of: 3 documents, 3 features (66.67% sparse) and 3 docvars.
    ##        features
    ## docs    points working class
    ##   text1      1       1     1
    ##   text2      0       0     0
    ##   text3      0       0     0

## 3\. Build a document-embedding-matrix

Given a ‘dfm’, `immig_dfm`, a set of pre-trained embeddings,
`cr_glove_subset` and a corresponding transformation matrix,
`cr_transform`, we can proceed to embed each document –i.e. context– ‘*a
la carte*’. We embed a document by multiplying each of it’s feature
counts with their corresponding pre-trained feature-embeddings,
column-averaging the resulting vectors, and multiplying by the
transformation matrix. This “transforms” a *sparse* V-dimensional vector
(a vector of feature counts, with V = number of features in the corpus)
into a *dense* D-dimensional vector (a D-dimensional embedding, with D =
dimensions of the pre-trained embeddings). We do this using
`conText::dem()`–‘dem’ standing for *document-embedding-matrix*. Each
row in this matrix represents an ‘a la carte’ (ALC) embedding of a
single instance of “immigration”.

Keep in mind, only those features that appear in the set of pre-trained
embeddings will be used in computing a document’s embedding. Documents
with no features overlapping with the pre-trained provided are dropped.
`dem()` –outputs a ‘dem-class’ object which is similar in many ways to
‘dfm-class’ object. Importantly, a ‘dem-class’ object inherits all the
documenta variables, ‘docvars’, from the ‘dfm’ used to compute it
(except those of documents that could not be embedded). Additionally, a
‘dem-class’ object will store other useful attributes, including the
names of the documents that were embedded and the vector of features
used.

``` r
# build a document-embedding-matrix
immig_dem <- dem(x = immig_dfm, pre_trained = cr_glove_subset, transform = TRUE, 
    transform_matrix = cr_transform, verbose = TRUE)

# each document inherits its corresponding docvars head(immig_dem@docvars)

# you can check which documents were not embedded due to lack of overlapping
# features (in this example all documents are embedded) note: 'quanteda'
# functions like `docvars()` and `docnames()` don't work on `dem` objects, so you
# will have to call the attributes directly.  setdiff(docnames(immig_dfm),
# immig_dem@Dimnames$docs)

# vector of features used to create the embeddings head(immig_dem@features)
```

## 4\. Average over document embeddings

We now have an ALC embedding for each instance of “immigration” in our
sample corpus. To get a single corpus-wide ALC embedding for
“immigration”, we can simply take the column-average of the
single-instance ALC embeddings.

    ## [1]   1 300

However, we are usually interested in exploring semantic differences
across groups. To do so, we can average using a grouping variable
defined by one or a combination of the ‘docvars’. We do this using
`conText::dem_group()` (very similar in flavor to quanteda’s
[`dfm_group()`](https://tutorials.quanteda.io/basic-operations/dfm/dfm_group/)).
In our example below, this results in an ALC embedding of “immigration”
for each party, hence the dimensions \(2\) by \(300\).

``` r
# to get group-specific embeddings, average within party
immig_wv_party <- dem_group(immig_dem, groups = immig_dem@docvars$party)
dim(immig_wv_party)
```

    ## [1]   2 300

## 5\. Comparing group embeddings

Given an ALC embedding for each party, we can proceed to do some
exploratory analysis.

### Nearest neighbors

First, we can evaluate differences in nearest neighbors –features with
the highest cosine-similarity with each group embedding– using
`conText::nns()`. We use the `candidates` argument to limit the set of
features we want `nns` to consider as candidate nearest neighbors. In
this example we limit candidates to those features that appear in our
corpus (otherwise, any word in the set of pre-trained embeddings can be
a
candidate).

``` r
# find nearest neighbors by party setting as_list = FALSE combines each group's
# results into a single tibble (useful for joint plotting)
immig_nns <- nns(immig_wv_party, pre_trained = cr_glove_subset, N = 5, candidates = immig_wv_party@features, 
    as_list = TRUE)

# check out results for Republican party
immig_nns[["R"]]
```

    ## # A tibble: 5 × 4
    ##   target feature      rank value
    ##   <chr>  <chr>       <int> <dbl>
    ## 1 R      immigration     1 0.811
    ## 2 R      illegal         2 0.769
    ## 3 R      immigrants      3 0.723
    ## 4 R      amnesty         4 0.673
    ## 5 R      illegally       5 0.659

### Cosine similarity

We can also look at the relationship between each group’s ALC embedding
of “immigration” and a specific set of features. For example, how does
each party’s understanding of immigration compare on the “reform” vs.
“enforcement” dimension? To answer this, we use `conText::cos_sim()`.
Results (on this limited sample of the data) suggest Democrats are more
prone to speak of “reform” in the context of “immigration” whereas
Republicans are more likely to speak of
“enforcement”.

``` r
# compute the cosine similarity between each party's embedding and a specific set
# of features
cos_sim(immig_wv_party, pre_trained = cr_glove_subset, features = c("reform", "enforcement"), 
    as_list = FALSE)
```

    ##   target     feature     value
    ## 1      D      reform 0.7108357
    ## 2      R      reform 0.4303339
    ## 3      D enforcement 0.5218543
    ## 4      R enforcement 0.5950741

### Nearest neighbors cosine similarity ratio

A third exploratory function included in ‘conText’ is `nns_ratio()`.
Given ALC embeddings for two groups, `nns_ratio()` computes the ratio of
cosine similarities between group embeddings and features –that is, for
any given feature it first computes the similarity between that feature
and each group embedding, and then takes the ratio of these two
similarities. This ratio captures how “discriminant” a feature is of a
given group. Values larger (smaller) than \(1\) mean the feature is more
(less) discriminant of the group in the numerator (denominator). Use the
`numerator` argument to define which group represents the numerator in
this ratio. If `N` is defined, this ratio is computed for union of the
top `N` nearest
neighbors.

``` r
# compute the cosine similarity between each party's embedding and a specific set
# of features
nns_ratio(x = immig_wv_party, N = 10, numerator = "R", candidates = immig_wv_party@features, 
    pre_trained = cr_glove_subset, verbose = FALSE)
```

    ##          feature     value
    ## 1        illegal 1.4803606
    ## 2        enforce 1.4602248
    ## 3        amnesty 1.4085389
    ## 4      illegally 1.4016893
    ## 5           laws 1.1970487
    ## 6    enforcement 1.1403069
    ## 7     immigrants 1.1049763
    ## 8         border 1.0766717
    ## 9            law 1.0516178
    ## 10   immigration 0.9999361
    ## 11        broken 0.7428609
    ## 12        system 0.7383703
    ## 13          come 0.7090780
    ## 14          pass 0.6892514
    ## 15   legislation 0.6455370
    ## 16 comprehensive 0.6116063
    ## 17         bring 0.6107542
    ## 18        reform 0.6053915

## Multiple Keywords

In the above example we had one target word, “immigration”. However, we
can also explore the semantics of multiple targets simultaneously,
including phrases\! We simply provide a vector of patterns in the
`pattern` argument of `tokens_context()`.

``` r
# build a corpus of contexts sorrounding the target term 'immigration'
mkws_toks <- tokens_context(x = toks, pattern = c("immigration", "welfare", "immigration reform", 
    "economy"), window = 6L, verbose = FALSE)

# create document-feature matrix
mkws_dfm <- dfm(mkws_toks)

# create document-embedding matrix using a la carte
mkws_dem <- dem(x = mkws_dfm, pre_trained = cr_glove_subset, transform = TRUE, transform_matrix = cr_transform, 
    verbose = FALSE)

# get embeddings for each pattern
mkws_wvs <- dem_group(mkws_dem, groups = mkws_dem@docvars$pattern)

# find nearest neighbors for each keyword
mkws_nns <- nns(mkws_wvs, pre_trained = cr_glove_subset, N = 5, candidates = mkws_wvs@features, 
    as_list = TRUE)

# to check results for a given pattern
mkws_nns[["immigration reform"]]
```

    ## # A tibble: 5 × 4
    ##   target             feature        rank value
    ##   <chr>              <chr>         <int> <dbl>
    ## 1 immigration reform comprehensive     1 0.836
    ## 2 immigration reform immigration       2 0.732
    ## 3 immigration reform reform            3 0.656
    ## 4 immigration reform pass              4 0.623
    ## 5 immigration reform bipartisan        5 0.606

This can also be useful if we wanted to explore *group differences* in
the semantics of a given *topic* where we define a *topic* by a vector
of *topical* target words. Note, these *topical* embeddings are computed
off of the collection of contexts around the set of pattern words
provided. So, in the example below, the contexts around each of
“immigration”, “immigrant” and “immigration reform” are treated as a
single collection of contexts for the purposes of computing each party’s
*topical* embedding.

``` r
# build a corpus of contexts sorrounding the immigration related words
topical_toks <- tokens_context(x = toks, pattern = c("immigration", "immigrant", 
    "immigration reform"), window = 6L, verbose = FALSE)

# create document-feature matrix
topical_dfm <- dfm(topical_toks)

# create document-embedding matrix using a la carte
topical_dem <- dem(x = topical_dfm, pre_trained = cr_glove_subset, transform = TRUE, 
    transform_matrix = cr_transform, verbose = FALSE)

# get 'topical' embeddings for each party
topical_wvs <- dem_group(topical_dem, groups = topical_dem@docvars$party)

# find nearest neighbors for each keyword
nns(topical_wvs, pre_trained = cr_glove_subset, N = 5, candidates = topical_wvs@features, 
    as_list = FALSE)
```

    ## # A tibble: 10 × 4
    ##    target feature        rank value
    ##    <fct>  <chr>         <int> <dbl>
    ##  1 R      immigration       1 0.832
    ##  2 D      comprehensive     1 0.780
    ##  3 D      immigration       2 0.777
    ##  4 D      reform            3 0.734
    ##  5 R      illegal           2 0.721
    ##  6 D      broken            4 0.676
    ##  7 R      amnesty           3 0.670
    ##  8 R      laws              4 0.650
    ##  9 R      immigrants        5 0.649
    ## 10 D      bipartisan        5 0.594

# Wrapper functions

The above functions give users a lot of flexibility, however, it can be
cumbersome to write each step every time one wants to do some
exploratory analysis on a corpus. In this section we’ll look into a
collection of wrapper functions that allow users to go straight from a
tokenized corpus to results. Each of the three exploratory analysis
functions –`nns()`, `cos_sim()` and `nns_ratio()`– described above has
its corresponding wrapper function.

An key advantage of the wrapper functions is that they make it easy to
obtain standard errors around the sample statistics of interest via
*bootstrapping*. Bootstrapping works by sampling with replacement
documents from our tokenized corpus of contexts (within group if the
argument `groups` is specified) and going through the above steps for
each of our exploratory analysis functions (i.e. computing an ‘a la
carte’ embedding on each bootstrapped sample, then computing the cosine
similarities etc.). Let’s start with `conText::get_nns()`, a wrapper
function for `nns()`.

### Nearest neighbors

We use the same tokenized corpus of “immigration” contexts
–`immig_toks`– as above, and again limit candidate nearest neighbors
to the set of features in our corpus. To group by *party* we set `groups
= docvars(immig_corpus, 'party')` –the `groups` argument can include
more than two groups. If no grouping variable is provided, the full set
of (tokenized) contexts are aggregated into one single embedding. To
estimate standard errors for the cosine similarities between each
party’s embedding and their corresponding nearest neighbors we set
`bootstrap = TRUE` and define the desired number of bootstraps with
`num_bootstrap`. Notice the output now has an additional column
`std.error`. This is simply the standard deviation of the sampling
distribution of cosine similarities obtained via bootstrapping. Note,
values may differ slightly to the step-by-step process outlined above as
they represent averages over bootstrapped samples.

``` r
# we limit candidates to features in our corpus
feats <- featnames(dfm(immig_toks))

# compare nearest neighbors between groups
set.seed(2021L)
immig_party_nns <- get_nns(x = immig_toks, N = 10, groups = docvars(immig_toks, "party"), 
    candidates = feats, pre_trained = cr_glove_subset, transform = TRUE, transform_matrix = cr_transform, 
    bootstrap = TRUE, num_bootstraps = 10, as_list = TRUE)

# nearest neighbors of 'immigration' for Republican party
immig_party_nns[["R"]]
```

    ## # A tibble: 10 × 5
    ##    target feature       rank value std.error
    ##    <chr>  <chr>        <int> <dbl>     <dbl>
    ##  1 R      immigration      1 0.809   0.00851
    ##  2 R      illegal          2 0.763   0.00706
    ##  3 R      immigrants       3 0.727   0.0115 
    ##  4 R      amnesty          4 0.672   0.0119 
    ##  5 R      illegally        5 0.659   0.0118 
    ##  6 R      laws             6 0.603   0.0201 
    ##  7 R      enforcement      7 0.593   0.0168 
    ##  8 R      enforce          8 0.583   0.0177 
    ##  9 R      law              9 0.568   0.0179 
    ## 10 R      undocumented    10 0.565   0.00797

### Cosine similarity

`conText::get_cos_sim()` is a wrapper function for `cos_sim()`, used to
evaluate how similar each group’s (or single if `groups` is not defined)
embedding is to a set of features of interest –as with `get_nns()`, the
`groups` argument can take on more than two groups. Again we set
`bootstrap = TRUE` to obtain standard errors for the cosine
similarities.

``` r
# compute the cosine similarity between each group's embedding and a specific set
# of features
set.seed(2021L)
get_cos_sim(x = immig_toks, groups = docvars(immig_toks, "party"), features = c("reform", 
    "enforce"), pre_trained = cr_glove_subset, transform = TRUE, transform_matrix = cr_transform, 
    bootstrap = TRUE, num_bootstraps = 10, as_list = FALSE)
```

    ## # A tibble: 4 × 4
    ##   target feature value std.error
    ##   <fct>  <fct>   <dbl>     <dbl>
    ## 1 D      reform  0.708   0.0139 
    ## 2 D      enforce 0.400   0.00784
    ## 3 R      reform  0.425   0.0151 
    ## 4 R      enforce 0.583   0.0177

### Nearest neighbors cosine similarity ratio

Finally, `conText::get_nns_ratio()` is a wrapper function for
`nns_ratio()`, used to gauge how discriminant nearest neighbors are of
each group. Unlike `get_nns()` and `get_cos_sim()`, a `groups` argument
must be provided and it must be binary. As with `nns_ratio()`, we use
the `numerator` argument to control which group to use as the numerator
in the underlying ratio. We again limit the candidate nearest neighbors
to the set of local features and set `bootstrap = TRUE` to get standard
errors, in this case of the cosine similarity ratios.

Finally, `get_nns_ratio()` allows us to make inferences using a
permutation test, specifically around the *absolute deviation of the
observed cosine similarity ratio from \(1\)* (this captures how
discriminant a given nearest neighbor is). Specifically, for each
permutation, the grouping variable is randomly shuffled and the absolute
deviation of the cosine similarity ratios from \(1\) is computed. The
empirical p.value is then the proportion of these “permuted” deviations
that are larger than the observed deviation.

The output of `get_nns_ratio()` contains three additional columns
(relative to `nns_ratio()`) if `bootstrap = TRUE` and `permute = TRUE`.
These are the standard errors around the cosine similarity ratios, the
corresponding (empirical) `p.value` and a `group` variable identifying
which group the nearest neighbor belonged to – `shared` means it
appeared in both groups’ top \(N\) nearest neighbors.

``` r
# we limit candidates to features in our corpus
feats <- featnames(dfm(immig_toks))

# compute ratio
set.seed(2021L)
immig_nns_ratio <- get_nns_ratio(x = immig_toks, N = 10, groups = docvars(immig_toks, 
    "party"), numerator = "R", candidates = feats, pre_trained = cr_glove_subset, 
    transform = TRUE, transform_matrix = cr_transform, bootstrap = TRUE, num_bootstraps = 10, 
    permute = TRUE, num_permutations = 10, verbose = FALSE)
```

    ## starting bootstraps 
    ## done with bootstraps 
    ## starting permutations 
    ## done with permutations

``` r
head(immig_nns_ratio)
```

    ## # A tibble: 6 × 5
    ##   feature     value std.error p.value group
    ##   <chr>       <dbl>     <dbl>   <dbl> <chr>
    ## 1 illegal      1.49    0.0339       0 R    
    ## 2 enforce      1.46    0.0450       0 R    
    ## 3 illegally    1.43    0.0443       0 R    
    ## 4 amnesty      1.42    0.0427       0 R    
    ## 5 laws         1.20    0.0427       0 R    
    ## 6 enforcement  1.15    0.0317       0 R

**conText** also includes a plotting function, `plot_nns_ratio`,
specific to `get_nns_ratio()`, providing a nice visualization of its
output. `alpha` defines the desired significance threshold to denote
“significant” results on the plot (indicated by a \(*\) next to the
feature). Also, you can choose between two different visualizations of
the same results using the `horizontal`
argument.

``` r
plot_nns_ratio(x = immig_nns_ratio, alpha = 0.01, horizontal = TRUE)
```

<img src="https://github.com/prodriguezsosa/conText/blob/develop/vignettes/nns_ratio.png" width="100%" />

# Embedding regression

The above framework allows us to explore semantic differences over one
grouping variable at a time. However, we often want to look at covariate
effects while controlling for other covariates. In [Rodriguez, Spirling
and Stewart
(2021)](https://github.com/prodriguezsosa/EmbeddingRegression) we show
how `a la carte` embeddings can be used within a regression-framework
that allows us to do just that. The corresponding package function is
`conText::conText()`.

`conText()` follows a similar syntax as R’s `lm()` and `glm()`
functions. `data` must be a quanteda tokens object with covariates
stored as document variables (`docvars`). We next specify a formula
consisting of the target word of interest, “immigration” and the set of
covariates. We could also have specified `immigration ~ .`, since we are
using all covariates in `data`. Covariates must either be binary
indicator variables or discrete variables that can be transformed into
binary indicator variables e.g. a character or factor variable with
three states will be automatically transformed by `conText()` into two
separate indicator variables with one *baseline* state that is excluded
from the regression. Keep in mind, `conText()` calls on
`tokens_context()`, so you do not need to create a tokenized corpus of
contexts before running it, simply provide it with the tokenized corpus
–`toks` in this example. Finally, the `conText()` can also take
phrases as a dv, you simply need to place it in quotation marks e.g.
`"immigration reform" ~ party + gender`.

``` r
set.seed(2021L)
model1 <- conText(formula = immigration ~ party + gender, data = toks, pre_trained = cr_glove_subset, 
    transform = TRUE, transform_matrix = cr_transform, bootstrap = TRUE, num_bootstraps = 10, 
    stratify = TRUE, permute = TRUE, num_permutations = 100, window = 6, case_insensitive = TRUE, 
    verbose = FALSE)
```

    ##   Coefficient Normed_Estimate  Std.Error Empirical_Pvalue
    ## 1     party_R       0.8301277 0.08035593                0
    ## 2    gender_M       0.4295767 0.05099670                0

``` r
# notice, non-binary covariates are automatically 'dummified'
rownames(model1)
```

    ## [1] "party_R"     "gender_M"    "(Intercept)"

`conText()` outputs a `conText-class` object which is simply a
`dgCMatrix class` matrix corresponding to the beta coefficients (ALC
embeddings) with additional attributes including: a table (automatically
printed) with the normed coefficients (excluding the intercept), their
std. errors and p-values, `@normed_coefficients`, and the set of
features used when creating the embeddings `@features`.

We can combine the coefficients to compute the ALC embeddings for the
various combinations of the covariates (see [Rodriguez, Spirling and
Stewart (2021)](https://github.com/prodriguezsosa/EmbeddingRegression)
for
details).

``` r
# D-dimensional beta coefficients the intercept in this case is the ALC embedding
# for female Democrats beta coefficients can be combined to get each group's ALC
# embedding
DF_wv <- model1["(Intercept)", ]  # (D)emocrat - (F)emale 
DM_wv <- model1["(Intercept)", ] + model1["gender_M", ]  # (D)emocrat - (M)ale 
RF_wv <- model1["(Intercept)", ] + model1["party_R", ]  # (R)epublican - (F)emale 
RM_wv <- model1["(Intercept)", ] + model1["party_R", ] + model1["gender_M", ]  # (R)epublican - (M)ale 

# nearest neighbors
nns(rbind(DF_wv, DM_wv), N = 10, pre_trained = cr_glove_subset, candidates = model1@features)
```

    ## $DM_wv
    ## # A tibble: 10 × 4
    ##    target feature        rank value
    ##    <chr>  <chr>         <int> <dbl>
    ##  1 DM_wv  immigration       1 0.770
    ##  2 DM_wv  reform            2 0.754
    ##  3 DM_wv  comprehensive     3 0.721
    ##  4 DM_wv  broken            4 0.708
    ##  5 DM_wv  system            5 0.589
    ##  6 DM_wv  fix               6 0.574
    ##  7 DM_wv  bipartisan        7 0.565
    ##  8 DM_wv  pass              8 0.555
    ##  9 DM_wv  legislation       9 0.555
    ## 10 DM_wv  bring            10 0.541
    ## 
    ## $DF_wv
    ## # A tibble: 10 × 4
    ##    target feature        rank value
    ##    <chr>  <chr>         <int> <dbl>
    ##  1 DF_wv  comprehensive     1 0.765
    ##  2 DF_wv  immigration       2 0.736
    ##  3 DF_wv  reform            3 0.730
    ##  4 DF_wv  broken            4 0.676
    ##  5 DF_wv  bipartisan        5 0.581
    ##  6 DF_wv  address           6 0.576
    ##  7 DF_wv  legislation       7 0.574
    ##  8 DF_wv  system            8 0.566
    ##  9 DF_wv  fix               9 0.559
    ## 10 DF_wv  pass             10 0.555

To access the normed coefficients for plotting:

``` r
model1@normed_cofficients
```

    ##   Coefficient Normed_Estimate  Std.Error Empirical_Pvalue
    ## 1     party_R       0.8301277 0.08035593                0
    ## 2    gender_M       0.4295767 0.05099670                0

# Local GloVe and transformation matrix

If (a) you have a large enough corpus to train a full GloVe embeddings
model and (b) your corpus is distinctive in some way –e.g. a collection
of articles from scientific journals–, then you may want to consider
estimating your own set of embeddings and transformation matrix
–otherwise you should be good to go using GloVe pre-trained
embeddings. The first step is to estimate GloVe embeddings on the full
corpus which you will then use as your pre-trained embeddings.

## Estimate GloVe embeddings

This example is taken (with minor changes) from [this quanteda
vignette](https://quanteda.io/articles/pkgdown/replication/text2vec.html)
on computing GloVe embeddings using `quanteda` and `text2vec`.

``` r
library(text2vec)

#---------------------------------
# estimate glove model
#---------------------------------

# construct the feature co-occurrence matrix for our toks object (see above)
toks_fcm <- fcm(toks, context = "window", window = 6, count = "frequency", tri = FALSE)

# estimate glove model using text2vec
glove <- GlobalVectors$new(rank = 300, x_max = 10, learning_rate = 0.05)
wv_main <- glove$fit_transform(toks_fcm, n_iter = 10, convergence_tol = 0.001, n_threads = parallel::detectCores())
```

    ## INFO  [19:27:41.585] epoch 1, loss 0.2253 
    ## INFO  [19:27:43.110] epoch 2, loss 0.0767 
    ## INFO  [19:27:44.618] epoch 3, loss 0.0501 
    ## INFO  [19:27:46.133] epoch 4, loss 0.0380 
    ## INFO  [19:27:47.651] epoch 5, loss 0.0309 
    ## INFO  [19:27:49.163] epoch 6, loss 0.0262 
    ## INFO  [19:27:50.712] epoch 7, loss 0.0229 
    ## INFO  [19:27:52.223] epoch 8, loss 0.0204 
    ## INFO  [19:27:53.734] epoch 9, loss 0.0184 
    ## INFO  [19:27:55.244] epoch 10, loss 0.0169

``` r
wv_context <- glove$components
local_glove <- wv_main + t(wv_context)  # word vectors

# qualitative check
find_nns(local_glove["immigration", ], pre_trained = local_glove, N = 5, candidates = feats)
```

    ## [1] "immigration" "law"         "bill"        "one"         "reform"

## Estimating the transformation matrix

Given a corpus and it’s corresponding GloVe embeddings, we can compute a
corresponding transformation matrix using
`conText::compute_transform()`.

``` r
# compute transform weighting = 'log' works well for smaller corpora for large
# corpora use a numeric value e.g. weighting = 500 see:
# https://arxiv.org/pdf/1805.05388.pdf
local_transform <- compute_transform(x = toks_fcm, pre_trained = local_glove, weighting = "log")
```

You should always run some sanity checks to make sure the computed
transformation matrix produces sensical results (keep in mind the corpus
in this example consists of only 200 documents). A more robust approach
(not illustrated below) to verify your transformation matrix is to
randomly select a set of features to exclude from the its estimation but
which have a corresponding pre-trained embedding. Use the transformation
matrix and pre-trained embeddings to compute ALC embeddings for these
excluded features and check their similarity with their corresponding
pre-trained embeddings (it should be reasonably high, \> 0.6).

``` r
#---------------------------------
# check
#---------------------------------

# create document-embedding matrix using our locally trained GloVe embeddings and
# transformation matrix
immig_dem_local <- dem(x = immig_dfm, pre_trained = local_glove, transform = TRUE, 
    transform_matrix = local_transform, verbose = TRUE)

# take the column average to get a single 'corpus-wide' embedding
immig_wv_local <- colMeans(immig_dem_local)

# find nearest neighbors for overall immigraiton embedding
find_nns(immig_wv_local, pre_trained = local_glove, N = 10, candidates = immig_dem_local@features)
```

    ##  [1] "immigration"   "reform"        "comprehensive" "system"       
    ##  [5] "law"           "bill"          "people"        "now"          
    ##  [9] "one"           "laws"

``` r
# we can also compare to corresponding pre-trained embedding
sim2(x = matrix(immig_wv_local, nrow = 1), y = matrix(local_glove["immigration", 
    ], nrow = 1), method = "cosine", norm = "l2")
```

    ##           [,1]
    ## [1,] 0.7709089

# Other

## Feature embedding matrix

We may be interested in simultaneously embedding many features. This can
be useful to (1) compare two corpora along many features simultaneously
and/or (2) estimate context-specific feature embeddings that can
subsequently be fed into a downstream classification task. We’ll focus
on (1) here. Using the same tokenized `cr_sample_corpus` we start by
building a feature-co-occurrence matrix for each group
(party).

``` r
# create feature co-occurrence matrix for each party (set tri = FALSE to work
# with fem)
fcm_D <- fcm(toks[docvars(toks, "party") == "D", ], context = "window", window = 6, 
    count = "frequency", tri = FALSE)
fcm_R <- fcm(toks[docvars(toks, "party") == "R", ], context = "window", window = 6, 
    count = "frequency", tri = FALSE)
```

Given an `fcm` for each group, we can proceed to compute a corresponding
“feature-embedding-matrix” for each group.

``` r
# compute feature-embedding matrix
fem_D <- fem(fcm_D, pre_trained = cr_glove_subset, transform = TRUE, transform_matrix = cr_transform, 
    verbose = FALSE)
fem_R <- fem(fcm_R, pre_trained = cr_glove_subset, transform = TRUE, transform_matrix = cr_transform, 
    verbose = FALSE)

# cr_fem will contain an embedding for each feature
fem_D[1:5, 1:3]
```

    ## 5 x 3 sparse Matrix of class "dgCMatrix"
    ##                                          
    ## let       0.42232447 0.1060923 0.29102675
    ## thank     0.57005432 0.3334870 0.05867746
    ## gentleman 0.42137005 0.2272419 0.15457789
    ## new       0.38544538 0.2579988 0.28578120
    ## york      0.05506132 0.4046657 0.37476059

Finally, we can use the `conText::feature_sim` function to compute
“horizontal” cosine similarities between both `fem`s for the set of
overlapping features. The output of `feature_sim` is ranked from least
similar to most similar features.

``` r
# compute 'horizontal' cosine similarity
feat_comp <- feature_sim(x = fem_R, y = fem_D)

# least similar features
head(feat_comp)
```

    ##         feature      value
    ## 1      speakers -0.1827995
    ## 2      enhanced -0.1570992
    ## 3  alternatives -0.1538617
    ## 4 massachusetts -0.1373783
    ## 5          fort -0.1342687
    ## 6   restaurants -0.1295521

``` r
# most similar features
tail(feat_comp)
```

    ##      feature     value
    ## 3632  broken 0.9347112
    ## 3633  states 0.9348042
    ## 3634  patrol 0.9353494
    ## 3635  united 0.9358804
    ## 3636  border 0.9451811
    ## 3637  reform 0.9561077

## Embedding full documents

In all of examples above we’ve focused on contexts defined as “windows”
around a target word. In [Rodriguez et al.
(2021)](https://github.com/prodriguezsosa/EmbeddingRegression) we show
that the same procedure works well on contexts defined as the full
document, as long as the full document is relatively short –exactly what
threshold to use to define “short” is still an open question. Responses
to open-ended survey questions is a good example (discussed in the
paper). Below we provide an illustrative example using a subset of the
`cr_sample_corpus`. In particular, note the use of the `. ~` operator to
indicate that the full document should be used as the dv.

``` r
# identify documents with fewer than 100 words
short_toks <- toks[sapply(toks, length) <= 100, ]

# run regression on full documents
model2 <- conText(formula = . ~ party, data = short_toks, pre_trained = cr_glove_subset, 
    transform = TRUE, transform_matrix = cr_transform, bootstrap = TRUE, num_bootstraps = 10, 
    stratify = TRUE, permute = TRUE, num_permutations = 100, window = 6, case_insensitive = TRUE, 
    verbose = FALSE)
```

    ##   Coefficient Normed_Estimate  Std.Error Empirical_Pvalue
    ## 1     party_R       0.6864511 0.06096584             0.02
