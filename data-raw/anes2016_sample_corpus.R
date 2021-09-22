# to download the full data set use the following code
url <- 'https://www.dropbox.com/s/jb1onyg0ryp0uu6/anes2016.rds?dl=1' # link to raw data
destfile <- 'DESTINATION FILE HERE' # e.g. '~/Dropbox/GitHub/large_data/conText/data/anes2016.rds'
download.file(url, destfile)

#---------------------------------
# setup
#---------------------------------
library(dplyr)
library(quanteda)

# set path to where you stored the required data
path_to_data <- '~/Dropbox/GitHub/large_data/conText/data/'

# load data
anes2016 <- readRDS(paste0(path_to_data, 'anes2016.rds')) %>% select(respondent_id, response, ideology, yob, generation) %>% filter(generation!='generation z')
anes2016 <- anes2016 %>% na.omit() # drop missing observations
anes2016 <- anes2016 %>% mutate(respondent_id = 1:nrow(anes2016))

#---------------------------------
# sample (300 responses for each ideology-generation pair)
#---------------------------------
set.seed(42)
anes2016_sample <- anes2016 %>% group_by(ideology, generation) %>% sample_n(size = 300) %>% ungroup()
anes2016_sample_corpus <- corpus(anes2016_sample$response, docvars = anes2016_sample[,c('respondent_id', 'ideology', 'yob', 'generation')])

# save datasets
usethis::use_data(anes2016_sample_corpus, compress = 'bzip2', overwrite = TRUE)
