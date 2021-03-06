# About

An R package for estimating text embedding regression models as described in [Rodriguez, Spirling and Stewart (2021)](https://github.com/prodriguezsosa/EmbeddingRegression). 

# How to Install

`devtools::install_github("prodriguezsosa/conText")`

# Datasets

To use conText you will need three datasets: 

1. A **corpus** with the text and corresponding metadata you want to evaluate.
2. A set of **pre-trained embeddings** (a V by D matrix) used to embed context words.
3. A **transformation matrix** (D by D) specific to the pre-trained embeddings.

In [this Dropbox folder](https://www.dropbox.com/sh/jsyrag7opfo7l7i/AAB1z7tumLuKihGu2-FDmhmKa?dl=0) (see the /data folder) we have included the three datasets we use in the Quick Start Guide along with their documentation. Due to memory constraints we could not include them directly in the package. We'll be adding other useful datasets to this folder in the near future.
