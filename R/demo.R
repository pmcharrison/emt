#' Demo EMT
#'
#' This function launches a demo for the EMT.
#'
#' @param num_items (Integer scalar) Number of items in the test.
#' @param feedback (Function) Defines the feedback to give the participant
#' at the end of the test. Defaults to a graph-based feedback page.
#' @param admin_password (Scalar character) Password for accessing the admin panel.
#' Defaults to \code{"demo"}.
#' @param researcher_email (Scalar character)
#' If not \code{NULL}, this researcher's email address is displayed
#' at the bottom of the screen so that online participants can ask for help.
#' Defaults to \email{p.m.c.harrison@qmul.ac.uk},
#' the email address of this package's developer.
#' @param dict The psychTestR dictionary used for internationalisation.
#' @param ... Further arguments to be passed to \code{\link{emt}()}.
#' @export
demo_emt <- function(num_items = 10L,
                     feedback = psychTestRCAT::cat.feedback.graph("EMT"),
                     admin_password = "demo",
                     researcher_email = "p.m.c.harrison@qmul.ac.uk",
                     dict = emt::emt_dict,
                     ...) {
  elts <- c(
    psychTestR::new_timeline(psychTestR::one_button_page(
      psychTestR::i18n("demo_intro")
    ), dict = dict),
    emt::emt(num_items = num_items,
             feedback = feedback,
             dict = dict,
             ...),
    psychTestR::new_timeline(
      psychTestR::final_page(psychTestR::i18n("you_may_close_browser")),
      dict = dict
    )
  )

  psychTestR::make_test(
    elts,
    opt = psychTestR::test_options(title = "EMT demo",
                                   admin_password = admin_password,
                                   researcher_email = researcher_email,
                                   demo = TRUE,
                                   languages = "EN"))
}
