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
# add nominate scores
# ----------------

nominate <- list()
for(i in 111:114){
  nominate[[length(nominate) + 1]] <- read.csv(paste0("~/Dropbox/GitHub/large_data/ALaCarteR/alacarteR/cr/HS", i, "_members.csv")) %>%
    select(session_id = congress, chamber, state = state_abbrev, party = party_code, district = district_code, bioname, nominate_dim1) %>%
    filter(chamber !="President" & party %in% c(100,200)) %>%
    mutate(lastname = toupper(trimws(gsub(",.*$", "", bioname))), firstname = trimws(gsub(".*,", "", bioname)),
           chamber = if_else(chamber == "House", "H", "S"),
           party = if_else(party == 100, "D", "R")) %>%
    select(lastname, session_id, chamber, state, party, bioname, nominate_dim1) %>% tidyr::drop_na()
}

nominate <- bind_rows(nominate)

# merge
cr <- left_join(cr, nominate, by = c('session_id', 'chamber', 'state', 'party', 'lastname')) %>% tidyr::drop_na()

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
cr_sample <- cr_clean %>% group_by(party, gender) %>% sample_n(size = 100, replace = FALSE) %>% mutate(party = factor(party), gender = factor(gender)) %>% select(speech, party, gender, nominate_dim1) %>% ungroup()
cr_sample_corpus <- corpus(cr_sample$speech, docvars = cr_sample[,c('party', 'gender', 'nominate_dim1')])

# save datasets
usethis::use_data(cr_sample_corpus, compress = 'bzip2', overwrite = TRUE)

