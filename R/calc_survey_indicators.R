#' Expand list of clusters at each area level
#'
#' This function recursively expands the list of clusters to produce a list
#' of survey clusters within areas at each level.
#'
#' TODO: These should be examples - where is areas_long.rds now?
#' areas_long <- readRDS(here::here("data/areas/areas_long.rds"))
#' survey_clusters <- readRDS(here::here("data/survey/survey_clusters.rds"))
#' survey_regions <- readRDS(here::here("data/survey/survey_regions.rds"))
#'
#' expand_survey_clusters(survey_clusters, areas_long)
#'
#' Get clusters at level 1 areas only
#' expand_survey_clusters(survey_clusters, areas_long, top_level = 1, bottom_level = 1)
#'
#' @keywords internal
expand_survey_clusters <- function(survey_clusters,
                                   survey_regions,
                                   areas,
                                   top_level = min(areas$area_level),
                                   bottom_level = max(areas$area_level)) {

  if (!all(survey_clusters$geoloc_area_id %in% areas$area_id |
           is.na(survey_clusters$geoloc_area_id))) {
    stop("Survey cluster area id not in area hierarchy: ",
         paste0(setdiff(survey_clusters$geoloc_area_id, areas$area_id), collapse = ", "))
  }

  if (!all(survey_regions$survey_region_area_id %in% areas$area_id)) {
    stop("Survey region area id not in area hierarchy: ",
         paste0(setdiff(survey_regions$survey_region_area_id, areas$area_id), collapse = ", "))
  }
  
  clusters <- survey_clusters %>%
    dplyr::select(survey_id, cluster_id, res_type, survey_region_id, geoloc_area_id) %>%
    dplyr::left_join(
             survey_regions %>%
             dplyr::select(survey_id, survey_region_id, survey_region_area_id),
             by = c("survey_id", "survey_region_id")
           ) %>%
    dplyr::mutate(
      ## Get lowest known area for each cluster (geoloc or syvreg if not geocoded)
             area_id = dplyr::if_else(is.na(geoloc_area_id), survey_region_area_id, geoloc_area_id),
             geoloc_area_id = NULL,
             survey_region_area_id = NULL
           ) %>%
    dplyr::left_join(
             areas %>%
             dplyr::select(area_id, area_level, parent_area_id) %>%
             dplyr::arrange(area_id, -area_level) %>%
             dplyr::group_by(area_id) %>%
             dplyr::filter(dplyr::row_number() == 1),
      by = "area_id"
    )

  val <- clusters %>%
    dplyr::filter(dplyr::between(area_level, top_level, bottom_level))

  #' Recursion
  while(any(clusters$area_level >= top_level)) {

    clusters <- clusters %>%
      dplyr::mutate(area_id = parent_area_id,
                    area_level = area_level - 1L,
                    parent_area_id = NULL) %>%
      dplyr::inner_join(areas %>%
                        dplyr::select(area_id, area_level, parent_area_id),
                        by = c("area_id", "area_level"))

    val <- dplyr::bind_rows(
                    val,
                    clusters %>%
                    dplyr::filter(dplyr::between(area_level, top_level, bottom_level))
                  )
  }

  val$parent_area_id <- NULL

  return(val)
}


#' Calculate age/sex/area stratified survey estimates for biomarker outcomes
#'
#' @details
#' All other data will be subsetted based on the `survey_id` values appearing in
#' survey_meta, so if only want to calculate for a subset of surveys it is
#' sufficient to pass subset for survey_meta and full data frames for the others.
#'
#' Much of this function needs to be parsed out into more generic functions and
#' rewritten to be more efficient.
#'   * Age group would be more efficient if traversing a tree structure.
#'   * Need generic function to calculate
#'   * Flexibility about age/sex stratifications to calculate.
#'
#' @param survey_meta Survey metadata.
#' @param survey_regions Survey regions.
#' @param survey_clusters Survey clusters.
#' @param survey_individuals Survey individuals.
#' @param survey_biomarker Survey biomarkers.
#' @param areas Areas.
#' @param sex Sex.
#' @param age_group_include Vector of age agroups to include
#' @param area_top_level Area top level.
#' @param area_bottom_level Area bottom level.
#' @param artcov_definition Definition to use for calculate ART coverage.
#' @param by_res_type Whether to stratify estimates by urban/rural res_type; logical.
#'
#'
#' @details
#'
#' The argument `artcov_definition` controls whether to use both ARV biomarker and
#' self-report (`artcov_definition = "both"`; default), ARV biomarker only
#' (`artcov_definition = "arv"`), or self-report ART use only
#' (`artcov_definition = "artself"`).  If option is `"both"`, then all HIV positive
#' are used as the denomiator and no missing data on either indicator are
#' incorporated. If the option is `"arv"` or `"artself"` then missing values in those
#' variables, respectively, are treated as missing.
#'
#' @export
calc_survey_hiv_indicators <- function(survey_meta,
                                       survey_regions,
                                       survey_clusters,
                                       survey_individuals,
                                       survey_biomarker,
                                       areas,
                                       sex = c("male", "female", "both"),
                                       age_group_include = NULL,
                                       area_top_level = min(areas$area_level),
                                       area_bottom_level = max(areas$area_level),
                                       artcov_definition = c("both", "arv", "artself"),
                                       by_res_type = FALSE) {

  ind <- survey_individuals %>%
    dplyr::inner_join(survey_biomarker,
                      by = c("survey_id", "individual_id")) %>%
    dplyr::filter(survey_id %in% survey_meta$survey_id,
                  !is.na(hivstatus)) %>%
    dplyr::select(survey_id, cluster_id, sex, age, hivweight, hivstatus, artself, arv, vls, recent)

  prep <- expand_survey_individuals(ind, survey_meta, survey_regions, survey_clusters, areas,
                                    sex, age_group_include, area_top_level, area_bottom_level,
                                    by_res_type)

  calc_hiv_outcomes(prep$ind, artcov_definition, survey_meta, areas, prep$age_groups)
}


#' Calculate age/sex/area stratified survey estimates for sexual risk behaviour
#'
#' Survey inputs for the SHIPP sexual behaviour small-area model: the
#' proportion of people in each sexual risk group, and HIV prevalence within
#' each risk group. Uses the same calculation as `calc_survey_hiv_indicators()`.
#'
#' @inheritParams calc_survey_hiv_indicators
#' @param survey_sexbehav Individual risk-group indicators: `survey_id`,
#'   `individual_id` and 0/1 columns, from `create_sexbehav_dhs()`,
#'   `extract_sexbehav_phia()` or `extract_sexbehav_mics()`.
#' @param hiv_area_top_level,hiv_area_bottom_level Area levels for
#'   `hiv_by_risk_group`; default national only, as used by the prevalence
#'   log odds ratio (LOR) step.
#'
#' @return A list of two data frames:
#'   * `risk_group_prop`: proportion with each `survey_sexbehav` indicator,
#'     weighted by `indweight`.
#'   * `hiv_by_risk_group`: HIV indicators stratified by each of `nosex12m`,
#'     `sexcohab`, `sexnonreg` and `sexpaid12m` in turn (the others set to
#'     `"all"`), weighted by `hivweight`. `NULL` if no individual has an HIV
#'     test result (e.g. DHS surveys without HIV testing).
#'
#' @export
calc_survey_sexbehav_indicators <- function(survey_meta,
                                            survey_regions,
                                            survey_clusters,
                                            survey_individuals,
                                            survey_biomarker,
                                            areas,
                                            survey_sexbehav,
                                            sex = c("male", "female", "both"),
                                            age_group_include = NULL,
                                            area_top_level = min(areas$area_level),
                                            area_bottom_level = max(areas$area_level),
                                            artcov_definition = c("both", "arv", "artself"),
                                            by_res_type = FALSE,
                                            hiv_area_top_level = 0,
                                            hiv_area_bottom_level = 0) {

  sexbehav_vars <- setdiff(names(survey_sexbehav), c("survey_id", "individual_id"))
  risk_groups <- intersect(c("nosex12m", "sexcohab", "sexnonreg", "sexpaid12m"), sexbehav_vars)

  ## Expand once: everyone with a survey_biomarker row (callers decide whether
  ## that includes untested respondents); the HIV part reuses the HIV-tested subset
  ind <- survey_individuals %>%
    dplyr::inner_join(survey_biomarker, by = c("survey_id", "individual_id")) %>%
    dplyr::inner_join(survey_sexbehav, by = c("survey_id", "individual_id")) %>%
    dplyr::filter(survey_id %in% survey_meta$survey_id) %>%
    dplyr::select(survey_id, cluster_id, sex, age, indweight,
                  hivweight, hivstatus, artself, arv, vls, recent,
                  dplyr::all_of(sexbehav_vars))
  no_weight <- setdiff(unique(ind$survey_id), ind$survey_id[!is.na(ind$indweight) & ind$indweight > 0])
  if (length(no_weight)) {
    stop("No individual (interview) weights `indweight` for: ", paste(no_weight, collapse = ", "),
         ". Risk-group proportions need them, e.g. PHIA `intwt0`.")
  }
  ## Expand once over both area-level ranges; each output keeps its own levels
  prep <- expand_survey_individuals(ind, survey_meta, survey_regions, survey_clusters, areas,
                                    sex, age_group_include,
                                    min(area_top_level, hiv_area_top_level),
                                    max(area_bottom_level, hiv_area_bottom_level),
                                    by_res_type)

  ## Proportion in each risk group: individual (interview) weights
  ind <- prep$ind %>%
    dplyr::filter(dplyr::between(area_level, area_top_level, area_bottom_level)) %>%
    dplyr::select(-hivweight, -hivstatus, -artself, -arv, -vls, -recent) %>%
    dplyr::rename(weights = indweight) %>%
    tidyr::pivot_longer(cols = dplyr::all_of(sexbehav_vars),
                        names_to = "indicator",
                        values_to = "estimate") %>%
    dplyr::filter(!is.na(estimate))

  risk_group_prop <- calc_all_outcomes(
    ind,
    group_by_vars = c("indicator", "survey_id", "area_id", "res_type", "sex", "age_group"),
    survey_meta = survey_meta, areas = areas, age_groups = prep$age_groups
  )

  ## HIV prevalence within each risk group: HIV-tested only, HIV weights
  ind <- prep$ind %>%
    dplyr::filter(!is.na(hivstatus),
                  dplyr::between(area_level, hiv_area_top_level, hiv_area_bottom_level)) %>%
    dplyr::select(-indweight, -dplyr::all_of(setdiff(sexbehav_vars, risk_groups)))

  hiv_by_risk_group <- NULL
  if (nrow(ind) > 0) {
    ## One risk group at a time, others "all", plus all "all" (what the LOR step uses)
    ind <- dplyr::mutate(ind, dplyr::across(dplyr::all_of(risk_groups), as.character))
    ind <- dplyr::bind_rows(
      dplyr::mutate(ind, dplyr::across(dplyr::all_of(risk_groups), ~ "all")),
      lapply(risk_groups, function(v) {
        dplyr::mutate(ind, dplyr::across(dplyr::all_of(setdiff(risk_groups, v)), ~ "all"))
      })
    )
    hiv_by_risk_group <- calc_hiv_outcomes(ind, artcov_definition, survey_meta, areas,
                                           prep$age_groups, extra_vars = risk_groups)
  }

  list(risk_group_prop = risk_group_prop,
       hiv_by_risk_group = hiv_by_risk_group)
}


#' Expand individuals to every age/sex group and area they contribute to
#'
#' Shared steps 1-4 of the survey indicator calculations.
#'
#' @return List of `ind` (one row per individual x sex x age group x area) and
#'   `age_groups`.
#' @noRd
expand_survey_individuals <- function(ind,
                                      survey_meta,
                                      survey_regions,
                                      survey_clusters,
                                      areas,
                                      sex,
                                      age_group_include,
                                      area_top_level,
                                      area_bottom_level,
                                      by_res_type) {

  ## 1. Identify age groups to calculate for each survey_id
  age_groups <- naomi::get_age_groups()

  if(!is.null(age_group_include))
    age_groups <- dplyr::filter(age_groups, age_group %in% !!age_group_include)

  sex_age_group <- tidyr::crossing(sex, age_groups)

  ## Only keep age groups that are fully contained within survey age range.
  ## For example, if survey sampled age 18-64, don't want to calculate
  ## aggregates for age 15-49.

  sex_age_group <- survey_meta %>%
    dplyr::select(survey_id, female_age_min, female_age_max,
                  male_age_min, male_age_max) %>%
    tidyr::crossing(sex_age_group) %>%
    dplyr::filter(age_group_start >= dplyr::case_when(sex == "male" ~ male_age_min,
                                                      sex == "female" ~ female_age_min,
                                                      sex == "both" ~ pmin(male_age_min,
                                                                           female_age_min)),
                  age_group_start + age_group_span <= dplyr::case_when(sex == "male" ~ male_age_max,
                                                                       sex == "female" ~ female_age_max,
                                                                       sex == "both" ~ pmin(male_age_max,
                                                                                            female_age_max)) + 1) %>%
    dplyr::select(survey_id, sex, age_group, age_group_label, age_group_start, age_group_span)


  ## 2. Expand clusters to identify all clusters within each area

  clust <- dplyr::filter(survey_clusters, survey_id %in% survey_meta$survey_id)
  clust_area <- expand_survey_clusters(clust, survey_regions, areas,
                                       area_top_level, area_bottom_level)

  clust_area <- clust_area %>%
    dplyr::arrange(survey_id, cluster_id, area_id, -area_level) %>%
    dplyr::group_by(survey_id, cluster_id, area_id) %>%
    dplyr::filter(dplyr::row_number() == 1)

  ## 3. Expand individuals dataset to repeat for all individiuals within each
  ##    age/sex group for a given survey

  ind <- ind %>%
    dplyr::bind_rows({.} %>% dplyr::mutate(sex = "both")) %>%
    dplyr::inner_join(sex_age_group, by = c("survey_id", "sex")) %>%
    dplyr::filter(age >= age_group_start,
                  age < age_group_start + age_group_span)

  ## 4. Join expanded age/sex and expanded cluster area map

  ind <- dplyr::inner_join(ind, clust_area, by = c("survey_id", "cluster_id"))

  if(by_res_type)
    ind <- dplyr::bind_rows(ind, dplyr::mutate(ind, res_type = "all"))
  else
    ind <- dplyr::mutate(ind, res_type = "all")

  list(ind = ind, age_groups = age_groups)
}


#' HIV biomarker outcomes (prevalence, ART coverage, VLS, recent infection)
#'
#' @param extra_vars Extra stratifying columns kept in the output (e.g. risk
#'   groups).
#' @noRd
calc_hiv_outcomes <- function(ind, artcov_definition, survey_meta, areas, age_groups,
                              extra_vars = NULL) {

  ## 5. Construct ART coverage indicator as either self-report or ART biomarker
  ##    and gather to long dataset for each biomarker

  if(artcov_definition[1] == "both") {
    ind <- ind %>%
      dplyr::group_by(survey_id) %>%
      dplyr::mutate(has_artcov = any(!is.na(arv) | !is.na(artself)),
                    artcov = dplyr::case_when(!has_artcov ~ NA_integer_,
                                              hivstatus == 0 ~ NA_integer_,
                                              arv == 1 | artself == 1 ~ 1L,
                                              TRUE ~ 0L)) %>%
      dplyr::select(-has_artcov, -artself, -arv) %>%
      dplyr::ungroup()
  } else if(artcov_definition[1] %in% c("arv", "artself")) {
    ind <- dplyr::rename(ind, artcov = artcov_definition[1]) %>%
      dplyr::mutate(artcov = dplyr::if_else(hivstatus == 0,  NA_integer_, as.integer(artcov)))
  } else {
    stop(paste("Invalid artcov_definition value:", artcov_definition[1]))
  }

  ## Rename variables to outcome indicators
  ind <- ind %>%
    dplyr::rename(prevalence = hivstatus,
                  art_coverage = artcov,
                  viral_suppression_plhiv = vls,
                  recent_infected = recent,
                  weights = hivweight)

  ## Pivot to long format
  ind <- ind %>%
    tidyr::pivot_longer(
             cols = c(prevalence, art_coverage, viral_suppression_plhiv, recent_infected),
             names_to = "indicator",
             values_to = "estimate"
           ) %>%
    dplyr::filter(!is.na(estimate))

  calc_all_outcomes(
    ind,
    group_by_vars = c("indicator", "survey_id", "area_id", "res_type", extra_vars, "sex", "age_group"),
    extra_vars = extra_vars,
    survey_meta = survey_meta, areas = areas, age_groups = age_groups
  )
}


#' Survey-weighted estimates for each group
#'
#' 6. Calculate outcomes. `ind` has one row per individual x group with columns
#' `estimate`, `weights`, `cluster_id`, `survey_region_id` and `group_by_vars`.
#'
#' @noRd
calc_all_outcomes <- function(ind, group_by_vars, extra_vars = NULL,
                              survey_meta, areas, age_groups) {

  ## Note: using survey region as strata right now. Most DHS use region + res_type

  dat <- ind %>%
    dplyr::filter(!is.na(weights), weights > 0)
  if (length(extra_vars))
    dat <- dplyr::filter(dat, dplyr::if_all(dplyr::all_of(extra_vars), ~ !is.na(.x)))

  cnt <- dat %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_by_vars))) %>%
    dplyr::summarise(n_clusters = dplyr::n_distinct(cluster_id),
                     n_observations = dplyr::n(),
                     n_eff_kish = sum(weights)^2 / sum(weights^2),
                     .groups = "drop")

  ## One survey design per split; area_level in place of area_id
  split_vars <- replace(group_by_vars, group_by_vars == "area_id", "area_level")
  datspl <- split(dat, do.call(paste, dat[split_vars]))
  by_formula <- stats::reformulate(group_by_vars)

  do_svymean <- function(df) {

    des <- survey::svydesign(~cluster_id,
                             data = df,
                             strata = ~survey_id + survey_region_id,
                             nest = TRUE,
                             weights = ~weights)

    val <- survey::svyby(~estimate,
                         by_formula,
                         des, survey::svymean)
    names(val)[names(val) == "se"] <- "std_error"
    val
  }

  options(survey.lonely.psu="adjust")
  mc.cores <- if(.Platform$OS.type == "windows") 1 else parallel::detectCores()
  est_spl <- parallel::mclapply(datspl, do_svymean, mc.cores = mc.cores)

  val <- cnt %>%
    dplyr::full_join(
             dplyr::bind_rows(est_spl),
             by = group_by_vars
           ) %>%
    dplyr::left_join(
             survey_meta %>% dplyr::select(survey_id, survey_mid_calendar_quarter),
             by = c("survey_id")
    ) %>%
    dplyr::left_join(
             areas %>%
             dplyr::select(area_id, area_name, area_level, area_sort_order),
             by = c("area_id")
           ) %>%
    dplyr::left_join(
             dplyr::select(age_groups, age_group, age_group_sort_order),
             by = "age_group"
           ) %>%
    dplyr::arrange(
             factor(indicator, c("prevalence", "art_coverage", "viral_suppression_plhiv", "recent_infected")),
             survey_id,
             survey_mid_calendar_quarter,
             area_level,
             area_sort_order,
             area_id,
             factor(res_type, c("all", "urban", "rural")),
             factor(sex, c("both", "male", "female")),
             age_group_sort_order
           ) %>%
    dplyr::select(
             indicator,
             survey_id,
             survey_mid_calendar_quarter,
             area_id,
             area_name,
             res_type,
             dplyr::all_of(extra_vars),
             sex,
             age_group,
             n_clusters,
             n_observations,
             n_eff_kish,
             estimate,
             std_error
           ) %>%
    dplyr::distinct()

  ## Calculate 95% CI on logit scale
  val <- val %>%
    dplyr::mutate(
             ci_lower = calc_logit_confint(estimate, std_error, "lower"),
             ci_upper = calc_logit_confint(estimate, std_error, "upper")
           )

  val
}

calc_logit_confint <- function(estimate, std_error, tail, conf.level = 0.95) {

  stopifnot(length(estimate) == length(std_error))
  stopifnot(tail %in% c("lower", "upper"))
  stopifnot(conf.level > 0 & conf.level < 1)

  crit <- qnorm(1 - (1 - conf.level) / 2) * switch(tail, "lower" = -1, "upper" = 1)
  lest <- stats::qlogis(estimate)
  lest_se <- std_error / (estimate * (1 - estimate))
    
  ifelse(estimate < 1 & estimate > 0, stats::plogis(lest + crit * lest_se), NA_real_)
}

#' Find Calendar Quarter Midpoint of Two Dates
#'
#' @param start_date vector coercibel to Date
#' @param end_date vector coercibel to Date
#'
#' @return A vector of calendar quarters
#'
#' @examples
#' start <- c("2005-04-01", "2010-12-01", "2016-01-01")
#' end <-c("2005-08-01", "2011-05-01", "2016-06-01")
#'
#' mid_calendar_quarter <- get_mid_calendar_quarter(start, end)
#'
#' @export
get_mid_calendar_quarter <- function(start_date, end_date) {

  start_date <- lubridate::decimal_date(as.Date(start_date))
  end_date <- lubridate::decimal_date(as.Date(end_date))

  stopifnot(!is.na(start_date))
  stopifnot(!is.na(end_date))
  stopifnot(start_date <= end_date)

  date4 <- (start_date + end_date) / 2
  year <- floor(date4)
  quarter <- floor((date4 %% 1) * 4) + 1

  paste0("CY", year, "Q", quarter)
}
