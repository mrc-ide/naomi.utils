test_that("create_survey_boundaries_dhs() returns simple features data frame", {

  surveys <- create_surveys_dhs("MWI", survey_type = "DHS", survey_characteristics = 23)
  surveys <- surveys[surveys$survey_id == "MWI2004DHS", ]

  region_boundaries <-
    expect_output(
      expect_message(create_survey_boundaries_dhs(surveys),
                     "^Downloading DHS region boundaries: MW2004DHS\n$"),
      NA
    )
  
  expect_is(region_boundaries, "sf")
  expect_equal(nrow(region_boundaries), 3)
})

test_that("create_survey_boundaries_dhs() verbose_download", {
  skip("I can't figure out how to capture the 'trying URL' message from download.file() to test.")
  
  region_boundaries <-
    expect_output(
      expect_message(create_survey_boundaries_dhs(surveys, verbose_download = TRUE),
                     "^Downloading DHS region boundaries: MW2004DHS\n$"),
      "trying  URL '" ## Ideally this, but "trying URL ..." is not captured as output
    )
  
  expect_is(region_boundaries, "sf")
  expect_equal(nrow(region_boundaries), 3)
})

test_that("create_survey_boundaries_dhs() options for multiple levels", {
    
  surveys <- create_surveys_dhs("MWI", survey_type = "DHS", survey_characteristics = 23)
  surveys <- surveys[surveys$survey_id %in% c("MWI2004DHS", "MWI2015DHS"), ]

  boundaries_default <-
    expect_message(
      create_survey_boundaries_dhs(surveys),
      paste0("^\nDHS boundaries contains multiple region levels for survey_id: MWI2015DHS\n",
             ".*",
             " MWI2015DHS.*3 +\n",
             " MWI2015DHS.*28 +TRUE\n")
    )
  expect_equal(nrow(boundaries_default), 31)

  boundaries_select <-
    expect_message(
      create_survey_boundaries_dhs(surveys, levelrnk_select = c("MWI2015DHS" = 1)),
      paste0("^\nDHS boundaries contains multiple region levels for survey_id: MWI2015DHS\n",
             ".*",
             "MWI2015DHS.*3 +TRUE\n")
    )
  expect_equal(nrow(boundaries_select), 6)
  
})
