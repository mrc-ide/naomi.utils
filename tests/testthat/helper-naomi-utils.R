## File contains test helpers which we want to use both here and in hintr

#' Build JSON from template and a set of params
#'
#' @param naomi_output Calibrated naomi output
#'
#' @return Calibrated naomi output matched to MWI test data on
#'   `naomi.resources` to be used to generate the shipp tool.
#' @keywords internal

make_shipp_testfiles <- function(naomi_output) {
  # Create naomi outputs align with testing data in naomi.resources:
  #   - Change iso3 to "MWI_demo"
  #   - Restrict outputs to admin2
  output <- naomi::read_hintr_output(naomi_output$model_output_path)

  # Areas
  meta_area_demo <- dplyr::mutate(output$output_package$meta_area,
                                  area_id = dplyr::if_else(area_id == "MWI", "MWI_demo", area_id),
                                  parent_area_id = dplyr::if_else(parent_area_id == "MWI", "MWI_demo", parent_area_id))

  meta_area_demo <- dplyr::filter(meta_area_demo, area_level <= 2)

  # Indicators
  ind_demo <- dplyr::mutate(output$output_package$indicators,
                            area_id = dplyr::if_else(area_id == "MWI", "MWI_demo", area_id))

  ind_demo <- dplyr::filter(ind_demo, area_id %in% meta_area_demo$area_id)


  # Options
  options_demo <- output$output_package$fit$model_options
  options_demo$area_scope <- "MWI_demo"
  options_demo$area_level <- 2

  # Save out demo output package
  demo <- output
  demo$output_package$indicators <- ind_demo
  demo$output_package$fit$model_options <- options_demo
  demo$output_package$meta_area <- meta_area_demo

  out_demo <- tempfile(fileext = ".qs2")
  naomi:::hintr_save(demo, out_demo)

  # Add to existing hintr_test data
  shipp_output_demo <- naomi_output
  shipp_output_demo$model_output_path <- out_demo

  shipp_output_demo
}

naomi_evaluate_promise <- function (code, print = FALSE) {
  warnings <- testthat:::Stack$new()
  handle_warning <- function(condition) {
    warnings$push(condition)
    invokeRestart("muffleWarning")
    if (!is.null(findRestart("muffleWarning"))) {
      invokeRestart("muffleWarning")
    }
  }
  messages <- testthat:::Stack$new()
  handle_message <- function(condition) {
    messages$push(condition)
    if (!is.null(findRestart("muffleMessage"))) {
      invokeRestart("muffleMessage")
    }
  }
  progress <- testthat:::Stack$new()
  handle_progress <- function(condition) {
    progress$push(condition)
    if (!is.null(findRestart("muffleProgress"))) {
      invokeRestart("muffleProgress")
    }
  }
  temp <- tempfile()
  result <- withr::with_output_sink(
    temp,
    withCallingHandlers(withVisible(code),
                        warning = handle_warning,
                        message = handle_message,
                        progress = handle_progress))
  if (result$visible && print) {
    withr::with_output_sink(temp, print(result$value))
  }
  output <- brio::read_file(temp)
  list(result = result$value,
       output = output,
       warnings = testthat:::get_messages(warnings$as_list()),
       messages = testthat:::get_messages(messages$as_list()),
       progress = progress$as_list())
}


# Load naomi output

a_hintr_output_calibrated <- list(plot_data_path = normalizePath(testthat::test_path("testdata/plot_data.qs2")),
                                  model_output_path = normalizePath(testthat::test_path("testdata/model_output.qs2")),
                                  version = "2.10.20",
                                  warnings = list())

class(a_hintr_output_calibrated) <- "hintr_output"
