info_page <- function(id) {
  psychTestR::one_button_page(psychTestR::i18n(id),
                              button_text = psychTestR::i18n("AEMT_0003"))
}

instructions <- function() {
  list(info_page("AEMT_0002"))
}