# About

An R package for computing feature embeddings `a la carte` and estimating text embedding regression models as described in [Rodriguez, Spirling and Stewart (2021)](https://github.com/prodriguezsosa/EmbeddingRegression).

# How to Install

`devtools::install_github("prodriguezsosa/conText")`

# Datasets

To use **conText** you will need three datasets: 

1. A (quanteda) **corpus** with the documents and corresponding document variables you want to evaluate.
2. A set of (GloVe) **pre-trained embeddings**.
3. A **transformation matrix** specific to the pre-trained embeddings.

In [this Dropbox folder](https://www.dropbox.com/sh/jsyrag7opfo7l7i/AAB1z7tumLuKihGu2-FDmhmKa?dl=0) (see the /data folder) we have included the three datasets we use in the Quick Start Guide along with their documentation (see the /man folder) and source files (see the /data-raw folder).

# Quick Start Guides

Check out this [Quick Start Guide](https://github.com/prodriguezsosa/conText/blob/develop/vignettes/quickstart.md) to get going with `conText`.

