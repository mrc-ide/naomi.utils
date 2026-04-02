#' Read model options from naomi output zip
#'
#' Returns a dataframe of model options used in model fit. This will fail if directory contains zip files that are not naomi outputs.
#'
#' @param file_path path to zipped naomi output file
#' @param out path to directory to save output. Default is current directory.
#'
#' @export

get_model_options <- function(path) {


  print(paste0("Reading files from: ", path))

  # Read in model options
  options <- unz(path, "info/options.yml")
  options <- yaml::read_yaml(options)
  iso3 <- options$area_scope

  surv_prev <- paste(options$survey_prevalence, collapse = ' ')
  options$survey_prevalence <- surv_prev


  if(length(options$survey_art_coverage)) {
    surv_artcov <- paste(options$survey_art_coverage, collapse = ' ')
    options$survey_art_coverage <- surv_artcov
  }


  if(length(options$anc_prevalence_year2)) {
    anc_prev_year2 <- paste(options$anc_prevalence_year2, collapse = ' ')
    options$anc_prevalence_year2 <- anc_prev_year2
  }


  if(length(options$anc_clients_year2)) {
    anc_clients_year2 <- paste(options$anc_clients_year2, collapse = ' ')
    options$anc_clients_year2 <- anc_clients_year2
  }

  if(length(options$anc_art_coverage_year2)) {
    anc_art_coverage_year2 <- paste(options$anc_art_coverage_year2, collapse = ' ')
    options$anc_art_coverage_year2 <- anc_art_coverage_year2
  }

  options <- utils::stack(options) %>%
    dplyr::transmute(option = as.character(ind), value = values)

  options_list <- data.frame(option = c("area_scope",
                                               "area_level",
                                               "calendar_quarter_t2",
                                               "calendar_quarter_t3",
                                               "calendar_quarter_t1",
                                               "survey_prevalence",
                                               "survey_art_coverage",
                                               "survey_recently_infected",
                                               "use_survey_aggregate",
                                               "include_art_t1",
                                               "include_art_t2",
                                               "artattend",
                                               "artattend_t2",
                                               "anc_clients_year2",
                                               "anc_clients_year2_num_months",
                                               "anc_prevalence_year1",
                                               "anc_prevalence_year2",
                                               "anc_art_coverage_year1",
                                               "anc_art_coverage_year2",
                                               "output_aware_plhiv",
                                               "max_iterations",
                                               "no_of_samples",
                                               "rng_seed",
                                               "permissive",
                                               "artattend_log_gamma_offset",
                                               "rho_paed_x_term",
                                               "rho_paed_15to49f_ratio",
                                               "alpha_xst_term",
                                               "adjust_area_growth",
                                               "use_kish_prev",
                                               "deff_prev",
                                               "use_kish_artcov",
                                               "deff_artcov",
                                               "use_kish_recent",
                                               "deff_recent",
                                               "deff_vls",
                                               "use_kish_vls" ) )




  fit_options <- left_join(options_list, options, by = "option")

  # Read in calibration options
  fit_files <- unzip(path, list = TRUE)

  if("fit/calibration_options.yml" %in% fit_files$Name) {
    calib_options <- unz(path, "fit/calibration_options.yml")
    calib_options <- yaml::read_yaml(calib_options)

    calib_options <- utils::stack(calib_options) %>%
      dplyr::transmute(option = as.character(ind), value = values)

  } else if ("fit/calibration_options.csv" %in% fit_files$Name){

    calib_options <- unz(path, "fit/calibration_options.csv")
    calib_options <- readr::read_csv(calib_options, show_col_types = FALSE)

  }

  default_calib_options <- tibble::tribble(~option, ~value,
                                           "spectrum_population_calibration", "national",
                                           "spectrum_plhiv_calibration_level", "none",
                                           "spectrum_plhiv_calibration_strat", "sex_age_group",
                                           "spectrum_artnum_calibration_level", "none",
                                           "spectrum_artnum_calibration_strat", "sex_age_coarse",
                                           "spectrum_aware_calibration_level", "none" ,
                                           "spectrum_aware_calibration_strat", "sex_age_coarse",
                                           "spectrum_infections_calibration_level", "none",
                                           "spectrum_infections_calibration_strat", "sex_age_coarse",
                                            "calibrate_method", "logistic")

  # Add in default calibration options if not present in calibration option file
  diff <- setdiff(default_calib_options$option, calib_options$option)

  if(length(diff)){
    calib_options <- rbind(calib_options,
                           default_calib_options %>%
                           dplyr::filter(option %in% diff))
  }

  # Order calibration options
  full_calib_options <- calib_options %>%
    dplyr::mutate(option = forcats::fct_relevel(option,
                                                 "spectrum_population_calibration",
                                                 "spectrum_plhiv_calibration_level",
                                                 "spectrum_plhiv_calibration_strat",
                                                 "spectrum_artnum_calibration_level",
                                                 "spectrum_artnum_calibration_strat",
                                                 "spectrum_aware_calibration_level",
                                                 "spectrum_aware_calibration_strat",
                                                 "spectrum_infections_calibration_level",
                                                 "spectrum_infections_calibration_strat")) %>%
    arrange(option)


  full_options <- rbind(fit_options,
                        full_calib_options)

  names(full_options)[names(full_options) == "value"] <- iso3

  full_options

  }



#' Generates a summary dataframe model options from folder of naomi output zips
#'
#' @folder_dir File path to folder containing naomi output zip files.
#' @out File path to directory to save output. Default is current directory.
#'
#'@export

summarise_model_options <- function(folder_dir, out = getwd()) {

  # Get list of naomi outputs file paths
  output_zip_paths <- list.files(folder_dir) %>%
    lapply(function(x) file.path(folder_dir, x)) %>%
    stringr::str_subset("zip")

  summary <- output_zip_paths %>%
    lapply(get_model_options) %>%
    purrr::reduce(left_join, by = "option")

  # Save output
  readr::write_csv(summary, paste0(out, Sys.Date(), "_naomi_model_options.csv"))
  }

