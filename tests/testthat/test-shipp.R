test_that("SHIPP download can be created", {

  shipp_output_demo <- make_shipp_testfiles(a_hintr_output_calibrated)

  mock_new_simple_progress <- mockery::mock(MockSimpleProgress$new())

  with_mocked_bindings(
    messages <- naomi_evaluate_promise(
      out <- generate_shipp_tool(testthat::test_path("testdata/naomi_output.zip"),
                                          NULL)),
    new_simple_progress = mock_new_simple_progress,
    .package = "naomi")

  expect_true(file.exists(out))

  outputs_female <- openxlsx::readWorkbook(out, sheet = "All outputs - F")
  expect_true(nrow(outputs_female) > 10)
  outputs_male <- openxlsx::readWorkbook(out, sheet = "All outputs - M")
  expect_true(nrow(outputs_male) > 10)
  naomi_outputs <- openxlsx::readWorkbook(out, sheet = "Naomi outputs")
  expect_true(nrow(naomi_outputs) > 4)

  # All tests below relate to KP workbook -> commented out in event KP workbook becomes available
  # For now, consensus estimate default set to Goals

  # Test shipp workbook with no kp workbook saved into spectrum
  # risk_prop <- shipp_generate_risk_populations(testthat::test_path("testdata/naomi_output.zip"),
  #                                              consensus_est = "goals",
  #                                              scale)
  #
  # expect_equal(risk_prop$meta_consensus,
  #              data.frame(kp = c("FSW", "MSM", "PWID"),
  #                         consensus_estimate = NA))
  #
  # # Test shipp workbook with mock workbook saved into spectrum
  # kp_consensus <- readRDS(file.path("testdata/kp_workbook_spectrum.rds"))
  # mock_extract_kp_workbook <- mockery::mock(kp_consensus)
  # mock_new_simple_progress <- mockery::mock(MockSimpleProgress$new())
  #
  # with_mocked_bindings(
  #   risk_prop_scaled <- shipp_generate_risk_populations(
  #     shipp_output_demo$model_output_path, a_hintr_data$pjnz),
  #   new_simple_progress = mock_new_simple_progress,
  #   extract_kp_workbook = mock_extract_kp_workbook
  # )
  #
  # # Check that consensus estimates extracted and saved out
  # expect_equal(risk_prop_scaled$meta_consensus,
  #              data.frame(kp = c("FSW", "MSM", "PWID"),
  #                         consensus_estimate = c(40000, 35500, 5000)))
  #
  # # Test that PSE tool adjusted to KP consensus estimates correctly
  # model_object <- read_hintr_output(shipp_output_demo$model_output_path)
  # outputs <- model_object$output_package
  # options <- outputs$fit$model_options
  # naomi <- shipp_format_naomi(outputs, options)
  #
  # # Naomi population
  # naomi_pop <- naomi$naomi_long %>%
  #   dplyr::filter(indicator == "population") %>%
  #   dplyr::select(area_id, area_level,sex, age_group, area_level,
  #                 spectrum_region_code, population = mean)
  #
  # naomi_pop$iso3 <- options$area_scope
  #
  # # KP PSEs adjusted to consensus estimates when consensus estimates are
  # #  < 5% of age matched population  denominator
  # fsw_est <- shipp_disaggregate_fsw(outputs, options, naomi_pop, kp_consensus)
  # pwid_est <- shipp_disaggregate_pwid(outputs, options, naomi_pop, kp_consensus)
  # msm_est <- shipp_disaggregate_msm(outputs, options, naomi_pop, kp_consensus)
  #
  # fsw <- sum(fsw_est$fsw)
  # pwid <- sum(pwid_est$pwid)
  # msm <- sum(msm_est$msm)
  #
  # # Note that PWID will be 90% of KP workbook consensus estimate due to exclusion
  # # of female PWID
  # expect_equal(c(fsw, pwid, msm), c(40000, 4550, 35500))
  #
  #
  # # KP PSEs **not** adjusted to consensus estimates when consensus estimates are
  # #  > 5% of age matched population denominator
  # kp_consensus_bad <- readRDS(file.path("testdata/kp_workbook_spectrum_bad.rds"))
  # mock_extract_kp_workbook <- mockery::mock(kp_consensus_bad)
  # mock_new_simple_progress <- mockery::mock(MockSimpleProgress$new())
  #
  # with_mocked_bindings(
  #   risk_prop_scaled <- shipp_generate_risk_populations(
  #     shipp_output_demo$model_output_path, a_hintr_data$pjnz),
  #   new_simple_progress = mock_new_simple_progress,
  #   extract_kp_workbook = mock_extract_kp_workbook
  # )
  #
  # # Check that bad consensus estimates extracted and saved out
  # expect_equal(risk_prop_scaled$meta_consensus,
  #              data.frame(kp = c("FSW", "MSM", "PWID"),
  #                         consensus_estimate = c(260000, 260000, 260000)))
  #
  #
  # # KP PSEs use default proportions from Oli's mode when consensus estimates are
  # #  >= 5% of age matched population  denominator
  #
  # fsw_est <- shipp_disaggregate_fsw(outputs, options, naomi_pop, kp_consensus_bad)
  # pwid_est <- shipp_disaggregate_pwid(outputs, options, naomi_pop, kp_consensus_bad)
  # msm_est <- shipp_disaggregate_msm(outputs, options, naomi_pop, kp_consensus_bad)
  #
  # fsw <- sum(fsw_est$fsw)
  # pwid <- sum(pwid_est$pwid)
  # msm <- sum(msm_est$msm)
  #
  # expect_equal(c(fsw, pwid, msm), c(62306.311, 7398.486, 24794.842))

})


test_that("Error thrown when SHIPP resources are out of date", {

  out <- naomi:::read_hintr_output(a_hintr_output_calibrated$model_output_path)
  outputs <- out$output_package
  options <- outputs$fit$model_options
  iso <- options$area_scope
  # Survey year extracted from "Model inputs" tab in SHIPP workbook template.
  # Set to year of most recent survey with SRB data or 2018 in cases where
  # most recent survey is older than 2018
  survey_year <- naomi.resources:::get_srb_year(iso)

  expect_error(assert_shipp_resource_hierarchy(outputs, options),
                                   "KP PSE estimates have area_ids which are missing from Naomi data: MWI_1_1; MWI_1_2; MWI_1_3")

})


test_that("scale_fsw_new_infections = TRUE and FALSE produce different FSW incidence outputs", {

  shipp_output_demo <- make_shipp_testfiles(a_hintr_output_calibrated)

  risk_scaled <- shipp_generate_risk_populations(
    shipp_output_demo,
    a_hintr_data$pjnz,
    consensus_est = "goals",
    scale_fsw_new_infections = TRUE
  )

  risk_unscaled <- shipp_generate_risk_populations(
    shipp_output_demo$model_output_path,
    a_hintr_data$pjnz,
    consensus_est = "goals",
    scale_fsw_new_infections = FALSE
  )

  # FSW new infections should differ between scaled and unscaled
  expect_false(isTRUE(all.equal(
    risk_scaled$female_incidence$infections_sexpaid12m,
    risk_unscaled$female_incidence$infections_sexpaid12m
  )))

  # FSW incidence rates should also differ
  expect_false(isTRUE(all.equal(
    risk_scaled$female_incidence$incidence_sexpaid12m,
    risk_unscaled$female_incidence$incidence_sexpaid12m
  )))

  # Non-FSW infections should also differ, as they are residuals after FSW allocation
  expect_false(isTRUE(all.equal(
    risk_scaled$female_incidence$infections_sexcohab,
    risk_unscaled$female_incidence$infections_sexcohab
  )))

  # Total Naomi infections column is unaffected by scaling choice
  expect_equal(
    risk_scaled$female_incidence$infections,
    risk_unscaled$female_incidence$infections
  )

  # Population and PLHIV are unaffected by the scaling choice
  expect_equal(
    risk_scaled$female_incidence$population,
    risk_unscaled$female_incidence$population
  )

  expect_equal(
    risk_scaled$female_incidence$plhiv,
    risk_unscaled$female_incidence$plhiv
  )

})


test_that("scale_fsw_new_infections = TRUE scales FSW new infections to Goals consensus estimate", {

  shipp_output_demo <- make_shipp_testfiles(a_hintr_output_calibrated)

  risk_scaled <- shipp_generate_risk_populations(
    shipp_output_demo$model_output_path,
    a_hintr_data$pjnz,
    consensus_est = "goals",
    scale_fsw_new_infections = TRUE
  )

  # Get the Goals consensus estimate for FSW new infections (iso3 = "MWI_demo")
  iso <- "MWI_demo"
  goals <- naomi.resources::load_shipp_exdata("goals", "SSA") |>
    dplyr::filter(iso3 == iso)
  fsw_goals_estimate <- goals$`fsw-new_inf`

  # Sum FSW new infections across 5-year age groups (not the pre-aggregated rows)
  five_year_age_groups <- c("Y015_019", "Y020_024", "Y025_029",
                            "Y030_034", "Y035_039", "Y040_044", "Y045_049")

  fsw_infections_total <- risk_scaled$female_incidence |>
    dplyr::filter(age_group %in% five_year_age_groups) |>
    dplyr::summarise(total = sum(infections_sexpaid12m)) |>
    dplyr::pull(total)

  # With scaling enabled, the total FSW new infections should match the Goals estimate
  expect_equal(fsw_infections_total, fsw_goals_estimate, tolerance = 0.01)

})


test_that("scale_fsw_new_infections = FALSE does not scale FSW infections to consensus estimate", {

  shipp_output_demo <- make_shipp_testfiles(a_hintr_output_calibrated)

  risk_unscaled <- shipp_generate_risk_populations(
    testthat::test_path("testdata/naomi_output.zip"), pjnz = NULL,
    consensus_est = "goals", scale_fsw_new_infections = FALSE)

  risk_scaled <- shipp_generate_risk_populations(
    testthat::test_path("testdata/naomi_output.zip"), pjnz = NULL,
    consensus_est = "goals", scale_fsw_new_infections = TRUE)

  # Get Goals FSW consensus estimate for reference
  iso <- "MWI_demo"
  goals <- naomi.resources::load_shipp_exdata("goals", "SSA") |>
    dplyr::filter(iso3 == iso)
  fsw_goals_estimate <- goals$`fsw-new_inf`

  five_year_age_groups <- c("Y015_019", "Y020_024", "Y025_029",
                            "Y030_034", "Y035_039", "Y040_044", "Y045_049")

  fsw_infections_unscaled <- risk_unscaled$female_incidence |>
    dplyr::filter(age_group %in% five_year_age_groups) |>
    dplyr::summarise(total = sum(infections_sexpaid12m)) |>
    dplyr::pull(total)

  fsw_infections_scaled <- risk_scaled$female_incidence |>
    dplyr::filter(age_group %in% five_year_age_groups) |>
    dplyr::summarise(total = sum(infections_sexpaid12m)) |>
    dplyr::pull(total)

  # Unscaled FSW infections should NOT match the Goals consensus estimate
  # Scaled FSW infections should match the Goals consensus estimate
  expect_equal(round(fsw_infections_unscaled), 2094)
  expect_equal(fsw_goals_estimate, 1906)
  expect_equal(fsw_infections_scaled, fsw_goals_estimate)

})
