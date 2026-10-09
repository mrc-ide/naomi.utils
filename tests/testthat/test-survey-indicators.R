## Tiny synthetic survey: national (X) with two level-1 areas, two clusters in
## each, all weights 1, so estimates are plain proportions
make_survey <- function() {
  areas <- data.frame(area_id = c("X", "X_1", "X_2"),
                      area_name = c("Country", "North", "South"),
                      area_level = c(0L, 1L, 1L),
                      parent_area_id = c(NA, "X", "X"), area_sort_order = 1:3)
  survey_meta <- data.frame(survey_id = "X2020DHS", survey_mid_calendar_quarter = "CY2020Q2",
                            female_age_min = 15, female_age_max = 49,
                            male_age_min = 15, male_age_max = 49)
  survey_regions <- data.frame(survey_id = "X2020DHS", survey_region_id = 1:2,
                               survey_region_name = c("North", "South"),
                               survey_region_area_id = c("X_1", "X_2"))
  survey_clusters <- data.frame(survey_id = "X2020DHS", cluster_id = 1:4, res_type = "urban",
                                survey_region_id = c(1, 1, 2, 2),
                                geoloc_area_id = c("X_1", "X_1", "X_2", "X_2"))
  n <- 40
  survey_individuals <- data.frame(survey_id = "X2020DHS", cluster_id = rep(1:4, each = 10),
                                   individual_id = seq_len(n), household = seq_len(n), line = 1,
                                   sex = rep(c("female", "male"), n / 2),
                                   age = rep(c(17, 22, 30, 45), 10), indweight = 1)
  survey_biomarker <- data.frame(survey_id = "X2020DHS", individual_id = seq_len(n), hivweight = 1,
                                 hivstatus = rep(c(1, 0, 0, 0, 0), 8),
                                 artself = NA_real_, arv = NA_real_, vls = NA_real_, recent = NA_real_)
  survey_sexbehav <- data.frame(survey_id = "X2020DHS", individual_id = seq_len(n),
                                nosex12m = rep(c(1, 0, 0, 0), 10), sexcohab = rep(c(0, 1, 1, 0), 10),
                                sexnonreg = rep(c(0, 0, 0, 1), 10), sexpaid12m = 0)
  list(survey_meta = survey_meta, survey_regions = survey_regions, survey_clusters = survey_clusters,
       survey_individuals = survey_individuals, survey_biomarker = survey_biomarker,
       areas = areas, survey_sexbehav = survey_sexbehav)
}

test_that("calc_survey_hiv_indicators() gives plain proportions with unit weights", {
  x <- make_survey()
  out <- calc_survey_hiv_indicators(x$survey_meta, x$survey_regions, x$survey_clusters,
                                    x$survey_individuals, x$survey_biomarker, x$areas,
                                    sex = "female", age_group_include = "Y015_049")
  prev <- out[out$indicator == "prevalence", ]
  f <- merge(x$survey_individuals, x$survey_biomarker)
  f <- f[f$sex == "female", ]
  expect_equal(prev$estimate[prev$area_id == "X"], mean(f$hivstatus))
  expect_equal(prev$n_observations[prev$area_id == "X"], nrow(f))
  expect_equal(prev$estimate[prev$area_id == "X_1"], mean(f$hivstatus[f$cluster_id %in% 1:2]))
})

test_that("calc_survey_sexbehav_indicators() proportions and HIV baseline", {
  x <- make_survey()
  out <- calc_survey_sexbehav_indicators(x$survey_meta, x$survey_regions, x$survey_clusters,
                                         x$survey_individuals, x$survey_biomarker, x$areas,
                                         x$survey_sexbehav, sex = "female",
                                         age_group_include = "Y015_049",
                                         area_top_level = 0, area_bottom_level = 1,
                                         hiv_area_top_level = 0, hiv_area_bottom_level = 0)
  p <- out$risk_group_prop
  f <- merge(x$survey_individuals, x$survey_sexbehav)
  f <- f[f$sex == "female", ]
  expect_equal(p$estimate[p$indicator == "sexcohab" & p$area_id == "X"], mean(f$sexcohab))
  expect_setequal(unique(p$area_id), c("X", "X_1", "X_2"))

  ## HIV by risk group is national only; the all-"all" row equals Naomi prevalence
  h <- out$hiv_by_risk_group
  expect_equal(unique(h$area_id), "X")
  hiv <- calc_survey_hiv_indicators(x$survey_meta, x$survey_regions, x$survey_clusters,
                                    x$survey_individuals, x$survey_biomarker, x$areas,
                                    sex = "female", age_group_include = "Y015_049",
                                    area_top_level = 0, area_bottom_level = 0)
  base <- h[h$indicator == "prevalence" & h$nosex12m == "all" & h$sexcohab == "all" &
              h$sexnonreg == "all" & h$sexpaid12m == "all", ]
  expect_equal(base$estimate, hiv$estimate[hiv$indicator == "prevalence"])
})

test_that("create_individual_hiv_dhs() checks hiv_testing before any download", {
  surveys <- data.frame(SurveyId = c("AA2000DHS", "AA2005DHS"))
  expect_error(create_individual_hiv_dhs(surveys, hiv_testing = "auto"), "must be TRUE/FALSE")
  expect_error(create_individual_hiv_dhs(surveys, hiv_testing = NA), "must be TRUE/FALSE")
  expect_error(create_individual_hiv_dhs(surveys, hiv_testing = c(TRUE, FALSE, TRUE)),
               "must be TRUE/FALSE")
  surveys$hiv_testing <- c(TRUE, NA)
  expect_error(create_individual_hiv_dhs(surveys), "must be TRUE/FALSE")
})
