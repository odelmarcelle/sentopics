## code to prepare `LoughranMcDonald` dataset goes here
LM_neg <- unlist(readxl::read_excel("./data-raw/LoughranMcDonald_SentimentWordLists_2018.xlsx", 2, col_names = FALSE))
LM_pos <- unlist(readxl::read_excel("./data-raw/LoughranMcDonald_SentimentWordLists_2018.xlsx", 3, col_names = FALSE))
LM_neg <- data.table::data.table(word = LM_neg, value = -1)
LM_pos <- data.table::data.table(word = LM_pos, value = 1)

LoughranMcDonald <- rbind(LM_neg, LM_pos)
LoughranMcDonald[, word := stringi::stri_trans_tolower(word)]
LoughranMcDonald <- LoughranMcDonald[order(word)]

LoughranMcDonald <- quanteda::dictionary(list(
  negative = LoughranMcDonald[value < 0, word],
  positive = LoughranMcDonald[value > 0, word],
  valence_negator = sentometrics::list_valence_shifters[["en"]][t == 1, x],
  valence_amplifier = sentometrics::list_valence_shifters[["en"]][t == 2, x]
))

usethis::use_data(LoughranMcDonald, overwrite = TRUE)

## LSDfr, now unused
# utils::download.file("http://www.poltext.org/sites/poltext.org/files/frlsd.zip",
#                      dictfile <- tempfile(), mode = "wb")
# utils::unzip(dictfile, exdir = (td <- tempdir()))
# LSDfr <- quanteda::dictionary(file = paste(td, "frlsd.lc3", sep = "/"))
# names(LSDfr) <- c("negative", "positive")
# 
# ## correct line 4120 of "frlsd.lc3"
# LSDfr <- unclass(LSDfr)
# LSDfr[[2]][[1]][1247] <- gsub("\\*.*\\)$", "\\*", LSDfr[[2]][[1]][1247])
# LSDfr <- new("dictionary2", LSDfr)
# 
# LSDfr <- as.dictionary(LSDfr)
# 
# ## It appears that the dictionary obtained from http://www.poltext.org/sites/poltext.org/files/frlsd_0.zip has issues.
# # (blank space and duplicates words). See: lapply(LSDfr, function(x) grep(" $", x))
# # LSDfr <- quanteda::dictionary(file = "./data-raw/frlsd.cat")
# # LSDfr <- data.table::data.table(word = c(LSDfr$NEGATIVE, LSDfr$POSITIVE),
# #                                 value = c(rep(-1, length(LSDfr$NEGATIVE)), rep(1, length(LSDfr$POSITIVE))))
# 
# # usethis::use_data(LSDfr, overwrite = TRUE)
