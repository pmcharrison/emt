main_test <- function(label, media_dir, num_items,
                      next_item.criterion,
                      next_item.estimator,
                      next_item.prior_dist,
                      next_item.prior_par,
                      final_ability.estimator,
                      constrain_answers) {
  item_bank <- get_item_bank()
  psychTestRCAT::adapt_test(
    label = label,
    item_bank = item_bank,
    show_item = show_item(media_dir),
    stopping_rule = psychTestRCAT::stopping_rule.num_items(n = num_items),
    opt = emt.options(next_item.criterion = next_item.criterion,
                      next_item.estimator = next_item.estimator,
                      next_item.prior_dist = next_item.prior_dist,
                      next_item.prior_par = next_item.prior_par,
                      final_ability.estimator = final_ability.estimator,
                      constrain_answers = constrain_answers,
                      item_bank = item_bank)
  )
}

show_item <- function(media_dir) {
  function(item, ...) {
    stopifnot(is(item, "item"), nrow(item) == 1L)
    item_page(
      item_number = psychTestRCAT::get_item_number(item),
      num_items_in_test = psychTestRCAT::get_num_items_in_test(item),
      statement = item$statement,
      audio_dir = media_dir,
      audio_1 = item$audio_1,
      audio_2 = item$audio_2,
      audio_3 = item$audio_3,
      audio_4 = item$audio_4
    )
  }
}

item_page <- function(item_number, num_items_in_test, statement,
                      audio_dir,
                      audio_1, audio_2, audio_3, audio_4) {
  for (x in c("item_number", "num_items_in_test"))
    checkmate::qassert(get(x), "X1")
  checkmate::qassert(num_items_in_test, "X1")
  for (x in c("statement", "audio_1", "audio_2", "audio_3", "audio_4"))
    checkmate::qassert(get(x), "S1")

  psychTestR::page(
    shiny::div(
      item_prompt(item_number, test_length, num_items_in_test),
      item_table(audio_dir, audio_1, audio_2, audio_3, audio_4)
    ))
}

item_prompt <- function(item_number, test_length, num_items_in_test) {
  shiny::div(
    shiny::p(
      psychTestR::i18n(
        "AEMT_0036",
        sub = list(num_question = item_number,
                   test_length = if (is.null(num_items_in_test))
                     "?" else
                       num_items_in_test))
    ),
    shiny::p(
      psychTestR::i18n("AEMT_0004")
    )
  )
}

item_table <- function(audio_dir, audio_1, audio_2, audio_3, audio_4) {
  shiny::tags$table(
    mapply(function(audio, button_id, text) {
      shiny::tags$tr(
        shiny::tags$th(style = "padding: 10px;", item_audio(audio_dir, audio)),
        shiny::tags$th(style = "padding: 10px;",
                       shiny::actionButton(inputId = button_id, label = text))
      )
    },
    SIMPLIFY = FALSE,
    audio = c(audio_1, audio_2, audio_3, audio_4),
    button_id = c("audio_1", "audio_2", "audio_3", "audio_4"),
    text = c("AEMT_0006", "AEMT_0007", "AEMT_0008", "AEMT_0008_R_0001_1")
    ))
}

item_audio <- function(audio_dir, audio, type = "mp3") {
  shiny::tags$audio(controls = "controls",
                    source = file.path(audio_dir, audio),
                    type = paste0("audio/", type),
                    "Your browser does not support audio.")
}

# get_admin_ui <- function(item) {
#   item$contour <- ifelse(item$contour_dif == 0, "Preserved", "Violated")
#   item$tonality <- ifelse(item$in_key, "Preserved", "Violated")
#   df <- item[, c("difficulty",
#                  "answer",
#                  "contour",
#                  "tonality",
#                  "num_notes")]
#   names(df) <- plyr::revalue(
#     names(df),
#     c(
#       difficulty = "Difficulty",
#       answer = "Correct answer",
#       contour = "Contour",
#       tonality = "Tonality",
#       num_notes = "Melody length (notes)"
#     ))
#   tab <- htmltools::tags$table(
#     lapply(seq_along(df),
#            function(i) shiny::tags$tr(
#              shiny::tags$td(names(df)[i],
#                             style = "padding:10px;"),
#              shiny::tags$td(format(df[[i]], digits = 3),
#                             style = "padding:10px;"))))
#   shiny::wellPanel(
#     shiny::h4("Item information"),
#     tab
#   )
# }