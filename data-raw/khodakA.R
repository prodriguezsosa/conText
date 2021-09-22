
# download raw bin file
url <- 'https://www.dropbox.com/s/5xonp4o9pnbjzpn/6B.300d.bin?dl=0' # link to raw data
destfile <- 'DESTINATION FILE HERE' # e.g. '~/Dropbox/GitHub/large_data/conText/data/6B.300d.bin'
download.file(url, destfile)

# upload bin file
finfo = file.info(destfile)  # determine total number of bytes to read full file
toread = file(destfile, "rb")  # establish connection
alldata = readBin(toread, numeric(), size = 4, n = finfo$size, endian = "little")  # floats/numeric = 4 bytes
khodakA <- matrix(alldata, nrow = 300, ncol = 300)  # embedding dimensions = 300
usethis::use_data(khodakA, compress = 'xz', overwrite = TRUE)

