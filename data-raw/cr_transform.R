#---------------------------------
# setup
#---------------------------------
library(dplyr)
# to download the full data set use the following code
url <- 'https://www.dropbox.com/s/d6kbn1vjkfjeyf0/cr_transform.rds?dl=1' # link to raw data
destfile <- 'DESTINATION FILE HERE' # e.g. '~/Dropbox/GitHub/large_data/conText/data/local_transform.rds'
download.file(url, destfile)

# load data
cr_transform <- readRDS('~/Dropbox/GitHub/large_data/conText/data/cr_transform.rds')

# store in /data
usethis::use_data(cr_transform, compress = 'xz', overwrite = TRUE)

