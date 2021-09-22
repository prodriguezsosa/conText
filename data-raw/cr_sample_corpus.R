# to download the full data set use the following code
url <- 'https://www.dropbox.com/s/5xonp4o9pnbjzpn/congressional_record.rds?dl=1' # link to raw data
destfile <- 'DESTINATION FILE HERE' # e.g. '~/Dropbox/GitHub/large_data/conText/data/congressional_record.rds'
download.file(url, destfile)

#---------------------------------
# setup
#---------------------------------
library(dplyr)
library(quanteda)

# set path to where you stored the required data
path_to_data <- '~/Dropbox/GitHub/large_data/conText/data/'

# load data
cr <- readRDS(paste0(path_to_data, 'congressional_record.rds'))
cr <- cr %>% distinct(speech, .keep_all = TRUE) # remove duplicate speeches

#---------------------------------
# sample (50 docs from each party-session pair containing the word immigration)
#---------------------------------
set.seed(42)
target_present <- grep('immigration', cr$speech)
cr_sample <- cr[target_present,] %>% group_by(party, session_id) %>% sample_n(size = 25) %>% select(speech, party, gender, session_id) %>% ungroup()
cr_sample_corpus <- corpus(cr_sample$speech, docvars = cr_sample[,c('party', 'gender', 'session_id')])

# save datasets
usethis::use_data(cr_sample_corpus, compress = 'bzip2', overwrite = TRUE)

