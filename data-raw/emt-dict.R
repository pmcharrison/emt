x <- read.csv("data-raw/text.csv", stringsAsFactors = FALSE)
df <- data.frame(key = x$dict_id, EN = x$EN, stringsAsFactors = FALSE)
emt_dict <- psychTestR::i18n_dict$new(df)
devtools::use_data(emt_dict, overwrite = TRUE)
