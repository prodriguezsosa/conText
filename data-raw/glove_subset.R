# run this file after running cr_sample_corpus.R and anes2016_sample_corpus.R

#---------------------------------
# setup
#---------------------------------
library(dplyr)
# to download the full data set use the following code
url <- 'https://www.dropbox.com/s/2eak4ua08n4yi66/glove.rds?dl=1' # link to raw data
destfile <- 'DESTINATION FILE HERE' # e.g. '~/Dropbox/GitHub/large_data/conText/data/glove.rds'
download.file(url, destfile)

#---------------------------------
# setup
#---------------------------------
library(dplyr)
library(quanteda)
library(quanteda.textstats)

# load cr_corpus_sample
load('~/Dropbox/GitHub/repositories/conText/data/cr_sample_corpus.rda')
load('~/Dropbox/GitHub/repositories/conText/data/anes2016_sample_corpus.rda')
glove <- readRDS('~/Dropbox/GitHub/large_data/conText/data/glove.rds')

# identify top features in sample corpora
feats_cr <- dfm(tokens(cr_sample_corpus), verbose = FALSE) %>%
  textstat_frequency(.) %>%
  filter(frequency > 5 & !(feature %in% stopwords('en'))) %>%
  slice(1:250) %>%
  .$feature

feats_anes2016 <- dfm(tokens(anes2016_sample_corpus), verbose = FALSE) %>%
  textstat_frequency(.) %>%
  filter(frequency > 5 & !(feature %in% stopwords('en'))) %>%
  slice(1:250) %>%
  .$feature

# identify overlap between top features and features in glove
common_features <- intersect(c(feats_cr, feats_anes2016), rownames(glove))

# subset glove
glove_subset <- glove[common_features,]
usethis::use_data(glove_subset, compress = 'xz', overwrite = TRUE)
