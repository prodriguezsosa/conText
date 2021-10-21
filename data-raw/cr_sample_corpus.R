# to download the full data set use the following code
url <- 'https://www.dropbox.com/s/5xonp4o9pnbjzpn/congressional_record.rds?dl=1' # link to raw data
destfile <- 'DESTINATION FILE HERE' # e.g. '~/Dropbox/GitHub/large_data/conText/data/congressional_record.rds'
download.file(url, destfile)

#---------------------------------
# setup
#---------------------------------
library(dplyr)
library(quanteda)
library(stringr)

# set path to where you stored the required data
path_to_data <- '~/Dropbox/GitHub/large_data/conText/data/'

# load data
cr <- readRDS(paste0(path_to_data, 'congressional_record.rds'))
cr <- cr %>% distinct(speech, .keep_all = TRUE) # remove duplicate speeches
cr_glove_vocab <- readRDS('~/Dropbox/GitHub/large_data/conText/data/cr_glove.rds') %>% rownames(.)

# ----------------
# subset to speeches mentioning immigration related words
# ----------------
target_present <- grepl('immigr*', cr$speech)
cr <- cr[target_present,]

# ----------------
# pre-processing
# ----------------

# tokenize corpus
toks <- tokens(cr$speech, remove_punct=T, remove_symbols=T, remove_numbers=T, remove_separators=T)

# clean out stopwords and words with 2 or fewer characters
toks_nostop <- tokens_select(toks, pattern = stopwords("en"), selection = "remove", min_nchar=3)

# only use features that appear at least 5 times in the corpus and are present in Glove
feats <- dfm(toks_nostop, tolower=T, verbose = FALSE) %>% dfm_trim(min_termfreq = 5) %>% featnames() %>% intersect(., cr_glove_vocab)

# leave the pads so that non-adjacent words will not become adjacent
toks <- tokens_select(toks_nostop, feats, padding = TRUE)

# identify texts with no overlap
overlap <- sapply(toks, length)

# collapse toks
cr_clean_text <- toks %>% lapply(function(i) paste(i, collapse = " ")) %>% unlist()
cr_clean <- cbind(speech = cr_clean_text, cr[,setdiff(colnames(cr), "speech")])
cr_clean <- cr_clean[overlap > 0,]

#---------------------------------
# sample (50 docs from each party-session pair containing the word immigration)
#---------------------------------
set.seed(42)
cr_sample <- cr_clean %>% group_by(party, gender) %>% sample_n(size = 100, replace = FALSE) %>% select(speech, party, gender) %>% ungroup()
cr_sample_corpus <- corpus(cr_sample$speech, docvars = cr_sample[,c('party', 'gender')])

# save datasets
usethis::use_data(cr_sample_corpus, compress = 'bzip2', overwrite = TRUE)

