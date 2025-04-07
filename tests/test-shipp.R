test_that("SHIPP download can be created", {

  shipp_output_demo <- make_shipp_testfiles(a_hintr_output_calibrated)

  mock_new_simple_progress <- mockery::mock(MockSimpleProgress$new())

  with_mocked_bindings(
    messages <- naomi_evaluate_promise(
      out <- hintr_prepare_shipp_download(shipp_output_demo,
                                          a_hintr_data$pjnz)),
    new_simple_progress = mock_new_simple_progress)

  expect_true(file.exists(out$path))

  expect_type(out$metadata$description, "character")
  expect_length(out$metadata$description, 1)
  expect_equal(out$metadata$areas, "MWI")

  outputs_female <- openxlsx::readWorkbook(out$path, sheet = "All outputs - F")
  expect_true(nrow(outputs_female) > 10)
  outputs_male <- openxlsx::readWorkbook(out$path, sheet = "All outputs - M")
  expect_true(nrow(outputs_male) > 10)
  naomi_outputs <- openxlsx::readWorkbook(out$path, sheet = "NAOMI outputs")
  expect_true(nrow(naomi_outputs) > 4)

  ## Progress messages printed
  expect_length(messages$progress, 1)
  expect_equal(messages$progress[[1]]$message, "Generating SHIPP tool")

  # Test shipp workbook with no kp workbook saved into spectrum
  risk_prop <- shipp_generate_risk_populations(shipp_output_demo$model_output_path,
                                               a_hintr_data$pjnz)

  expect_equal(risk_prop$meta_consensus,
               data.frame(kp = c("FSW", "MSM", "PWID"),
                          consensus_estimate = NA))

  # Test shipp workbook with mock workbook saved into spectrum
  kp_consensus <- readRDS(file.path("testdata/kp_workbook_spectrum.rds"))
  mock_extract_kp_workbook <- mockery::mock(kp_consensus)
  mock_new_simple_progress <- mockery::mock(MockSimpleProgress$new())

  with_mocked_bindings(
    risk_prop_scaled <- shipp_generate_risk_populations(
      shipp_output_demo$model_output_path, a_hintr_data$pjnz),
    new_simple_progress = mock_new_simple_progress,
    extract_kp_workbook = mock_extract_kp_workbook
  )

  # Check that consensus estimates extracted and saved out
  expect_equal(risk_prop_scaled$meta_consensus,
               data.frame(kp = c("FSW", "MSM", "PWID"),
                          consensus_estimate = c(40000, 35500, 5000)))

  # Test that PSE tool adjusted to KP consensus estimates correctly
  model_object <- read_hintr_output(shipp_output_demo$model_output_path)
  outputs <- model_object$output_package
  options <- outputs$fit$model_options
  naomi <- shipp_format_naomi(outputs, options)

  # Naomi population
  naomi_pop <- naomi$naomi_long %>%
    dplyr::filter(indicator == "population") %>%
    dplyr::select(area_id, area_level,sex, age_group, area_level,
                  spectrum_region_code, population = mean)

  naomi_pop$iso3 <- options$area_scope

  # KP PSEs adjusted to consensus estimates when consensus estimates are
  #  < 5% of age matched population  denominator
  fsw_est <- shipp_disaggregate_fsw(outputs, options, naomi_pop, kp_consensus)
  pwid_est <- shipp_disaggregate_pwid(outputs, options, naomi_pop, kp_consensus)
  msm_est <- shipp_disaggregate_msm(outputs, options, naomi_pop, kp_consensus)

  fsw <- sum(fsw_est$fsw)
  pwid <- sum(pwid_est$pwid)
  msm <- sum(msm_est$msm)

  # Note that PWID will be 90% of KP workbook consensus estimate due to exclusion
  # of female PWID
  expect_equal(c(fsw, pwid, msm), c(40000, 4550, 35500))


  # KP PSEs **not** adjusted to consensus estimates when consensus estimates are
  #  > 5% of age matched population denominator
  kp_consensus_bad <- readRDS(file.path("testdata/kp_workbook_spectrum_bad.rds"))
  mock_extract_kp_workbook <- mockery::mock(kp_consensus_bad)
  mock_new_simple_progress <- mockery::mock(MockSimpleProgress$new())

  with_mocked_bindings(
    risk_prop_scaled <- shipp_generate_risk_populations(
      shipp_output_demo$model_output_path, a_hintr_data$pjnz),
    new_simple_progress = mock_new_simple_progress,
    extract_kp_workbook = mock_extract_kp_workbook
  )

  # Check that bad consensus estimates extracted and saved out
  expect_equal(risk_prop_scaled$meta_consensus,
               data.frame(kp = c("FSW", "MSM", "PWID"),
                          consensus_estimate = c(260000, 260000, 260000)))


  # KP PSEs use default proportions from Oli's mode when consensus estimates are
  #  >= 5% of age matched population  denominator

  fsw_est <- shipp_disaggregate_fsw(outputs, options, naomi_pop, kp_consensus_bad)
  pwid_est <- shipp_disaggregate_pwid(outputs, options, naomi_pop, kp_consensus_bad)
  msm_est <- shipp_disaggregate_msm(outputs, options, naomi_pop, kp_consensus_bad)

  fsw <- sum(fsw_est$fsw)
  pwid <- sum(pwid_est$pwid)
  msm <- sum(msm_est$msm)

  expect_equal(c(fsw, pwid, msm), c(62306.311, 7398.486, 24794.842))

})


test_that("Error thrown when SHIPP resources are out of date", {

  kp_error <- paste0("Available KP PSE estimates for: \n",
                     "MWI_1_1; MWI_1_2; MWI_1_3",
                     "\n\n Do not match Naomi estimates for: \n",
                     "MWI_2_1_demo; MWI_2_2_demo; MWI_2_3_demo; MWI_2_4_demo; MWI_2_5_demo",
                     "\n\nTo update estimates, please contact Naomi support.")

  expect_error(hintr_prepare_shipp_download(a_hintr_output_calibrated,
                                            a_hintr_data$pjnz), kp_error)

})
