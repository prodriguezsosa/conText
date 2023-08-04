![logo-conText](https://user-images.githubusercontent.com/6556873/138291456-5dd454d2-b37c-478a-8e37-6b2b4c20710e.jpeg)

# About

**conText** provides a fast, flexible and transparent framework to estimate context-specific word and short document embeddings using the 'a la carte' embeddings approach developed by [Khodak et al. (2018)](https://arxiv.org/abs/1805.05388) and evaluate hypotheses about covariate effects on embeddings using the regression framework developed by [Rodriguez et al. (2021)](https://github.com/prodriguezsosa/EmbeddingRegression).

# How to Install

`install.packages("conText")`

# Datasets

To use **conText** you will need three objects: 

1. A (quanteda) **corpus** with the documents and corresponding document variables you want to evaluate.
2. A set of (GloVe) **pre-trained embeddings**.
3. A **transformation matrix** specific to the pre-trained embeddings.

**conText** includes sample objects for all three but keep in mind these are just meant to illustrate function implementations. In [this Dropbox folder](https://www.dropbox.com/sh/jsyrag7opfo7l7i/AAB1z7tumLuKihGu2-FDmhmKa?dl=0) we have included the raw versions of these objects including the full Stanford GloVe 300-dimensional embeddings (labeled _glove.rds_) and its corresponding transformation matrix estimated by Khodak et al. (2018) (labeled _khodakA.rds_).

# Quick Start Guides

Check out this [Quick Start Guide](https://github.com/prodriguezsosa/conText/blob/master/vignettes/quickstart.md) to get going with `conText`.

# Latest Updates

We'd like to thank Will Hobbs and Breanna Green for bringing to our attention implementation issues with our main estimation routine, `conText`, when samples sizes are small and/or class-ratios are highly skewed towards one group. We are actively collaborating in evaluating alternative re-sampling methods to correct for the finite sample bias associated with `conText`'s coefficient norm estimates. In the meantime, we've pushed an alternative version of the `conText` function, `conText_jackknife` that implements Jackknife debiasing. To illustrate the advantages of using Jackknife, below we showcase the results of simulations using real data where we know the true value of coefficient norm, varying both sample size and class ratios.

We observe that using OLS with bootstrapped standard errors can be problematic (i.e. resulting in a notable bias) for cases with small samples and highly skewed class ratios, especially when the true norm of the coefficient is 0. Jackknife debiasing clearly helps mitigate this bias. Nevertheless we still strongly recommend users: (1) avoid reading too much into results with highly skewed class ratios (2) benchmark their results against a random group of the same size as their group of interest and (3) always follow up any regression-based results with a qualitative comparison of nearest neighbors.

<img width="1461" alt="Screenshot 2023-07-07 at 5 39 26 PM" src="https://github.com/prodriguezsosa/conText/assets/6556873/84fe373a-d9a0-4438-be8b-d9dc84596006">
