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

**conText** includes sample objects for all three but keep in mind these are just meant to illustrate function implementations. In [this Dropbox folder](https://www.dropbox.com/sh/jsyrag7opfo7l7i/AAB1z7tumLuKihGu2-FDmhmKa?dl=0) we have included the raw versions of these objects including the full Stanford GloVe 300-dimensional embeddings (labeled _glove.rds_) and its corresponding transformation matrix estimated by Khodak et al. (2018) (labeled _khodakA.rds_). We provide an equivalent RDS file for the [2024 GloVe embeddings](https://nlp.stanford.edu/projects/glove/) released in July 2025 (labeled _glove_2024.rds).

# Quick Start Guides

Check out this [Quick Start Guide](https://github.com/prodriguezsosa/conText/blob/master/vignettes/quickstart.md) to get going with `conText` (last updated: 07/28/2025).

# Latest Updates

As noted in [Rodriguez et al. (2023)](https://www.cambridge.org/core/journals/american-political-science-review/article/embedding-regression-models-for-contextspecific-description-and-inference/4C90013E5C714C8483ED95CC699022FB) (p. 1272), distance measures typically used to compare representations in high-dimensional space (such as embedding vectors) exhibit statistical bias. In [Green et al. (2025)](https://www.cambridge.org/core/journals/political-analysis/article/measuring-distances-in-high-dimensional-spaces/88126F4A48F121387D249C1856C3665B), we explore the severity of this problem for text-as-data applications and provide and validate a bias correction for the squared Euclidean distance. We implement this estimator and other recommendations from the paper in the latest update to the `conText()` function. Please refer to the [Bias in Distance Measures](https://github.com/prodriguezsosa/conText/blob/master/updates/bias_in_distance_measures.md) vignette for additional information and the [Quick Start Guide](https://github.com/prodriguezsosa/conText/blob/master/vignettes/quickstart.md) for examples of how to use the new version of the function and a description of changes in the output.

# Multilanguage Resources

For those working in languages other than English, we have a set of data and code resources [here](https://alcembeddings.org/)
