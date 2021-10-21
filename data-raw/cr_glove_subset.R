# run this file after running cr_sample_corpus.R

#---------------------------------
# setup
#---------------------------------
library(dplyr)
# to download the full data set use the following code
url <- 'https://www.dropbox.com/s/8xqtqwv1j5rljq6/cr_glove.rds?dl=1' # link to raw data
destfile <- 'DESTINATION FILE HERE' # e.g. '~/Dropbox/GitHub/large_data/conText/data/local_glove.rds'
download.file(url, destfile)

#---------------------------------
# libraries
#---------------------------------
library(dplyr)
library(quanteda)
library(quanteda.textstats)

# load cr_corpus_sample
load('~/Dropbox/GitHub/repositories/conText/data/cr_sample_corpus.rda')
cr_glove <- readRDS('~/Dropbox/GitHub/large_data/conText/data/cr_glove.rds')

# identify top features in sample corpora
feats_cr <- dfm(tokens(cr_sample_corpus), verbose = FALSE) %>%
  textstat_frequency(.) %>%
  filter(frequency > 5 & !(feature %in% stopwords('en'))) %>%
  slice(1:500) %>%
  .$feature

# identify overlap between top features and features in glove
common_features <- intersect(feats_cr, rownames(cr_glove))

# subset glove
cr_glove_subset <- cr_glove[common_features,]
usethis::use_data(cr_glove_subset, compress = 'xz', overwrite = TRUE)
