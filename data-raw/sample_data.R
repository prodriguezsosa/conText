#---------------------------------
# setup
#---------------------------------

library(conText)
library(dplyr)
library(text2vec)

# set path to where you stored the required datasets
# see README for link to raw data
path_to_data <- '~/Dropbox/GitHub/large_data/conText/data/'

# corpus
cr_corpus <- readRDS(paste0(path_to_data, 'cr_corpus.rds'))

# (GloVe) pre-trained embeddings
pre_trained <- readRDS(paste0(path_to_data, 'glove.rds'))

# transformation matrix
khodakA <- readRDS(paste0(path_to_data, 'khodakA.rds'))

#---------------------------------
# sample toy corpus (1000 documents from each party containing the word immigration)
#---------------------------------
set.seed(42)
target_present <- grep('immigration', cr_corpus$speech, fixed = TRUE)
sample_corpus <- cr_corpus[target_present,] %>% group_by(party) %>% sample_n(size = 500) %>% select(speech, party) %>% ungroup()

#---------------------------------
# term counts
#---------------------------------
vocab <- space_tokenizer(sample_corpus$speech) %>%
         itoken(progressbar = FALSE) %>%
         create_vocabulary %>%
         filter(term %in% rownames(pre_trained)) %>%
         prune_vocabulary(vocab_term_max = 1000)

sample_glove <- pre_trained[intersect(vocab$term, rownames(pre_trained)),]

# save datasets
usethis::use_data(khodakA, compress = 'xz', overwrite = TRUE)
usethis::use_data(sample_corpus, compress = 'bzip2', overwrite = TRUE)
usethis::use_data(sample_glove, compress = 'xz', overwrite = TRUE)
