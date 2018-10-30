#' EMT feedback (no score)
#'
#' Here the participant is given no feedback at the end of the test.
#' @param dict The psychTestR dictionary used for internationalisation.
#' @export
#' @examples
#' \dontrun{
#' demo_emt(feedback = emt.feedback.no_score())}
emt.feedback.no_score <- function(dict = emt::emt_dict) {
  psychTestR::new_timeline(
    psychTestR::one_button_page(
      psychTestR::i18n("AEMT_0035")
    ),
    dict = dict
  )
}
