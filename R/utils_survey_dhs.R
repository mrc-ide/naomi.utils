

assert_iso3 <- function(iso3) {
  stopifnot(grepl("[A-Z]{3}", iso3))
  invisible(TRUE)
}

#' Create surveys dataset from DHS API
#'
#' Construct a surveys dataset from DHS API. Usess `rdhs` to identify the DHS
#' country code from the ISO3, selects relevant surveys, then constructs the
#' `survey_id` and `survey_mid_calendar_quarter`.
#'
#' @param iso3 Three letter ISO3 country code.
#' @param survey_types DHS survey types to access. See `?rdhs::dhs_surveys`.
#' @param survey_characteristics DHS survey characteristic IDs to filter on See `?rdhs::dhs_survey_characteristics`.
#'
#' @return A data frame containing the response from the _dhs_surveys_ API endpoint
#'   and the `survey_id` and `survey_mid_calendar_quarter`.
#'
#' @examples
#' create_surveys_dhs("MWI")
#'
#' @export
#'
create_surveys_dhs <- function(iso3,
                               survey_type = c("DHS", "AIS", "MIS"),
                               survey_characteristics = 23) {

  assert_iso3(iso3)

  countries <- rdhs::dhs_countries()
  dhs_cc <- countries$DHS_CountryCode[countries$ISO3_CountryCode == iso3]

  stopifnot(grepl("[A-Z]{2}", dhs_cc))

  surveys <- rdhs::dhs_surveys(countryIds = dhs_cc,
                               surveyType = survey_type,
                               surveyCharacteristicIds = survey_characteristics)
  surveys$survey_id <- paste0(iso3, surveys$SurveyYear, surveys$SurveyType)
  surveys$survey_mid_calendar_quarter <-
    get_mid_calendar_quarter(
      as.Date(surveys$FieldworkStart)+15,
      as.Date(surveys$FieldworkEnd)+15
    )

  surveys
}


#' Create survey region boundaries dataset from DHS spatial data repository
#'
#' @param surveys data.frame of surveys, returned by `create_surveys_dhs()`.
#'
#' @return A simple features data frame containing DHS region code, region name,
#'    and region boundaries for each survey.
#'
#' @details
#'
#' For some surveys, the DHS spatial data repository and the survey clusters
#' datasets boundaries at multiple levels (e.g. admin 1 and admin 2). In these
#' cases, the admin level with the largest number or regions is selected by
#' default.
#'
#' TODO: Support a workflow for selecting alternative levels.
#'
#' @export
create_survey_boundaries_dhs <- function(surveys) {

  geom_raw <- Map(rdhs::download_boundaries, surveyId = surveys$SurveyId, method = "sf")
  geom_raw <- unlist(geom_raw, recursive = FALSE)

  geom_cols <- c("DHSCC", "SVYID", "REG_ID", "MULTLEVEL", "LEVELRNK", "REGVAR",
                 "REGCODE", "REGNAME", "OTHREGVAR", "OTHREGCO", "OTHREGNA")

  geom <- lapply(geom_raw, "[", geom_cols)
  geom <- do.call(rbind, geom)

  ## Replace values coded as NULL in fields with NA
  geom <- dplyr::mutate_at(geom, dplyr::vars(-geometry),
                           function(x) replace(x, x == "NULL", NA))

  geom <- dplyr::left_join(geom, surveys[c("SurveyId", "SurveyNum")],
                           by = c("SVYID" = "SurveyNum"))

  ## Surveys with multiple boundary levels: choose level with larger number of
  ## areas unless reason not to.
  ##
  ## !! TODO: Develop workflow for a survey that doesn't choose the default
  ##          of level with larger number of areas.


  geom_levels <- dplyr::count(sf::st_drop_geometry(geom),
                              SurveyId, MULTLEVEL, LEVELRNK,
                              name = "n_regions")

  multi_levelId <- unique(geom_levels$SurveyId[duplicated(geom_levels$SurveyId)])

  if (length(multi_levelId)) {

    message("SurveyId = ", multi_levelId, " contains multiple boundary levels.")

    ## !! TODO: Output message indicating which was chosen

    geom_levels <- geom_levels %>%
      dplyr::group_by(SurveyId) %>%
      dplyr::filter(n_regions == max(n_regions))

    geom <- dplyr::semi_join(geom, geom_levels,
                             by = c("SurveyId", "MULTLEVEL", "LEVELRNK"))
  }

  survey_region_boundaries <- dplyr::left_join(geom, surveys[c("SurveyId", "survey_id")],
                                               by = "SurveyId")

  ## Note: REGVAR is retained here for later use in dataset extraction
  survey_region_boundaries <- dplyr::select(survey_region_boundaries,
                                            survey_id,
                                            survey_region_id = REGCODE,
                                            survey_region_name = REGNAME,
                                            REGVAR)

  survey_region_boundaries
}

#' Add REGVAR to surveys dataset
#'
#' The variable name for the survey region variable is sourced from the
#' DHS survey boundaries datasets sourced by `create_survey_boundaries_dhs()`.
#' Utility function to merge survey region variable name to `surveys`
#' dataset from `survey_region_boundaries` dataset.
#'
#' @param surveys surveys dataset, data.frame.
#' @param survey_region_boundaries survey_region_boundaries dataset, sf object.
#'
#' @return The surveys data.frame
#'
#' @details
#'
#' This will throw an error if the REGVAR is not unique to each survey_id within
#' the `survey_region_boundaries` dataset.
#'
#'
#' @export
surveys_add_dhs_regvar <- function(surveys, survey_region_boundaries) {

  if ( !is.null(surveys$REGVAR) ) {
    stop("Surveys dataset already contains REGVAR")
  }

  regvar <- sf::st_drop_geometry(survey_region_boundaries)
  regvar <- dplyr::distinct(regvar, survey_id, REGVAR)

  if (any(duplicated(regvar$surveyid))) {
    dup <- unique(regvar$surveyid[duplicated(regvar$surveyid)])
    stop("Survey regions dataset has multiple REGVAR: ", paste(dup, collapse = ", "))
  }

  val <- dplyr::left_join(surveys, regvar, by = "survey_id")
  val
}

#' Allocate areas to survey regions
#'
#' Allocate areas at the most granular level to survey regions via spatial
#' join based on largest overlapping area.
#'
#' @param areas_wide wide format area hierarchy, created by `naomi::spread_areas()`.
#' @param survey_region_boundaries survey_region_boundaries dataset created by
#'           `create_survey_boundaries_dhs()`.
#'
#' @return A simple features data frame consisting of a mapping of all areas
#'   to a survey_region_id.
#'
#' @details
#'
#' The function `sf:st_join(..., largest = TRUE)` is used to construct a spatial
#' join based on the area of largest overlap.
#'
#' If the mapping is clean, the following should be satisfied:
#'
#'   1. All areas are allocated to a survey region. This might not happen if
#'      an area is non-overlapping with the survey geometry.
#'   2. All survey regions should contain some areas. This might not happen if
#'      all areas overlapping a region are not cleanly nested and have a
#'      larger overlap with other regions.
#'
#' The function `assert_survey_region_areas()` implements these checks.
#'
#' These conditions are not comprehensive and do not guarantee the mapping is
#' accurate, but will catch some basic errors.
#'
#' @export
#'

allocate_areas_survey_regions <- function(areas_wide, survey_region_boundaries) {

  regions <- dplyr::select(survey_region_boundaries, survey_id, survey_region_id)
  regions_spl <- split(regions, regions$survey_id)

  area_id_wide <- dplyr::select(areas_wide, dplyr::starts_with("area_id"))

  ## Wrapper function for sf::st_join to muffle specific message and warning.
  st_join_quiet <- function(x, y, join, ...) {
    suppress_one_warning(
      suppress_one_message(
        sf::st_join(x, y, join, ...),
        "although coordinates are longitude/latitude, st_intersection assumes that they are planar"),
      "attribute variables are assumed to be spatially constant throughout all geometries"
    )
  }

  area_regions <- lapply(regions_spl, st_join_quiet, x = area_id_wide, largest = TRUE)
  area_regions <- do.call(rbind, area_regions)

  survey_region_areas <- sf::st_drop_geometry(survey_region_boundaries) %>%
    dplyr::full_join(area_regions, by = c("survey_id", "survey_region_id"))

  survey_region_areas
}

#' Create survey regions dataset from DHS
#'
#' Construct survey regions dataset by identifying the smallest area_id
#' that contains the whole survey region.
#'
#' @param survey_region_areas Area allocation to survey regions, created by
#'   [`allocate_areas_survey_regions()`]
#'
#' @return Survey regions dataset conforming to schema.
#'
#' @export
create_survey_regions_dhs <- function(survey_region_areas) {

  val <- dplyr::select(survey_region_areas,
                       survey_id, survey_region_id, survey_region_name,
                       dplyr::matches("area_id[0-9]+"))

  val <- tidyr::pivot_longer(val, cols = dplyr::starts_with("area_id"),
                             names_to = "level", values_to = "survey_region_area_id")

  ## Order level in reverse order to select highest level (smallest area) first
  val$level <- forcats::fct_rev(val$level)

  ## Within each survey_region (group_by()), filter the rows where only
  ## a single area_id exists, indicating that the survey region is entirely
  ## contained in that area.
  val <- dplyr::distinct(val)
  val <- dplyr::group_by(val, survey_id, survey_region_id, level)

  val <- dplyr::filter(val, dplyr::n() == 1)

  ## Filter to the first level that contains each region
  val <- dplyr::arrange(val, survey_id, survey_region_id, level)
  val <- dplyr::arrange(val, survey_id, survey_region_id, level)
  val <- dplyr::group_by(val, survey_id, survey_region_id)
  val <- dplyr::filter(val, dplyr::row_number() == 1)

  dplyr::ungroup(val)
  val$level <- NULL

  val
}


#' Create survey clusters dataset
#'
#' Create survey clusters dataset from DHS household recode and
#' geocluster datasets.
#'
#' @param surveys data.frame of surveys, returned by `create_surveys_dhs()`.
#'
#' @return data.frame consisting of survey clusters, survey region id, and
#'   cluster geographic coordinates if available.
#'
#' @examples
#' surveys <- create_surveys_dhs("MWI")
#' survey_regions <- create_survey_boundaries_dhs(surveys)
#' surveys <- surveys_add_dhs_regvar(surveys, survey_regions)
#'
#' survey_clusters <- create_survey_clusters_dhs(surveys)
#'
#' @export
create_survey_clusters_dhs <- function(surveys) {

  hrd <- rdhs::dhs_datasets(surveyIds = surveys$SurveyId,
                            fileType = "HR",
                            fileFormat = "flat")
  hrd$path <- unlist(rdhs::get_datasets(hrd))

  hrd <- dplyr::left_join(hrd,
                          dplyr::select(surveys, SurveyId, survey_id, REGVAR),
                          by = "SurveyId")

  hrclust <- Map(hr_extract_clusters, hrd$path, hrd$survey_id, hrd$REGVAR)
  hrclust <- dplyr::bind_rows(hrclust)

  ## Add geo-coordinates

  ged <- rdhs::dhs_datasets(surveyIds = surveys$SurveyId,
                            fileType = "GE",
                            fileFormat = "flat")
  ged <- dplyr::left_join(ged, surveys[c("SurveyId", "survey_id")],
                          by = "SurveyId")
  ged$path <-  unlist(rdhs::get_datasets(ged))

  ge <- lapply(ged$path, readRDS)
  ge <- lapply(ge, as.data.frame)
  ge <- Map(f = dplyr::mutate,
            ge,
            survey_id = ged$survey_id,
            SurveyYear = ged$SurveyYear,
            SurveyType = ged$SurveyType,
            CountryName = ged$CountryName)
  ge <- Map(replace, ge, lapply(ge, `==`, "NULL"), NA)
  ge <- lapply(ge, type.convert)
  ge <- dplyr::bind_rows(ge)
  ge <- sf::st_as_sf(ge, coords = c("LONGNUM", "LATNUM"), remove = FALSE)

  ge <- dplyr::filter(ge, LONGNUM != 0)
  ge <- dplyr::select(ge,
                      survey_id,
                      cluster_id = DHSCLUST,
                      longitude = LONGNUM,
                      latitude = LATNUM)

  survey_clusters <- dplyr::left_join(hrclust, ge, by = c("survey_id", "cluster_id"))

  survey_clusters
}


hr_extract_clusters <- function(path, survey_id, REGVAR){

  message("Parsing HR dataset: ", survey_id)

  hr <- readRDS(path)
  val <- dplyr::transmute(
                  hr,
                  survey_id,
                  cluster_id = hv001,
                  survey_region_id = haven::zap_labels(.data[[REGVAR]]),
                  res_type = factor(haven::zap_labels(hv025), 1:2, c("urban", "rural"))
                )
  val <- dplyr::distinct(val)

  val
}


#' Assign survey clusters to dataset areas
#'
#' Assign each survey cluster with geocoordinates to an area, ensuring that
#' the assigned area is contained in the specified survey region.
#'
#' @param survey_clusters Interim survey clusters dataset created by
#'                        [`create_survey_clusters_dhs()`].
#' @param survey_region_areas Dataset of the areas contained in each survey
#'   region, created by [`allocate_areas_survey_regions()`].
#'
#' survey_region_areas is a list of candidate location areas
#' for each cluster. Join candidate areas and then select
#' the nearest area based on distance. Usually the coordinate
#' should be contained (distance = 0)
#'
#' @return Survey clusters dataset with an area_id assigned for each cluster
#'    with geographic coordinates and formatted conforming to survey_clusters
#'    schema.
#'
#' @details
#' For each survey cluster with geographic coordinates, the area ID containing
#' the cluster is assigned by:
#'
#' 1. Identify all areas contained in the survey region in which the cluster is
#'    located. This comprises the set of candidate areas where could be located.
#' 2. Calculate the nearest distance from the cluster coordinates to each
#'    candidate area. This distance is 0 if a cluster is contained in an area.
#' 3. Select the area as the area with the nearest distance, in most cases
#'    an area containing the cluster (distance = 0).
#'
#' sf::st_distance() is substantially slower than sf::st_join(). This function
#' could be (maybe much) more efficient by first using st_join() to assign the
#' majority of clusters that are contained in an area, then calculating the
#' distance for the remaining clusters that were not contained inside any of the
#' candidate areas.
#'
#' @export
assign_dhs_cluster_areas <- function(survey_clusters, survey_region_areas) {


  ## NOTE: survey_region_id might change from integer to character in future schema
  survey_clusters$survey_region_id <- as.integer(survey_clusters$survey_region_id)

  ## If a cluster has coordinates, identify which survey region it should be in
  survey_clusters$regcode_match <- ifelse(is.na(survey_clusters$longitude),
                                          NA_integer_, survey_clusters$survey_region_id)

  reg_areas <- dplyr::select(survey_region_areas,
                             survey_id,
                             regcode_match = survey_region_id,
                             area_id,
                             geometry_area = geometry)

  ## Join survey clusters to all areas in that survey region -- candidate areas.
  clust <- dplyr::left_join(survey_clusters, reg_areas,
                            by = c("survey_id", "regcode_match"))

  ## Check that survey_region_id are contained in survey_region_areas
  check_survey_region_id <- survey_clusters[c("survey_id", "survey_region_id", "cluster_id")]
  check_survey_region_id <- dplyr::anti_join(check_survey_region_id, survey_region_areas,
                                             by = c("survey_id", "survey_region_id"))
  if (nrow(check_survey_region_id)) {
    stop("Survey clusters have survey_region_id not contained in survey_regions:\n",
         paste0(capture.output(check_survey_region_id), collapse = "\n"))
  }


  ## Calculate distance between cluster coordinates and each candidate survey region.
  ## Warning: this is a bit slow; could be faster on *nix by replacing Map()
  ##          with parallel::mcMap().

  clust$distance = unlist(Map(sf::st_distance, clust$geometry, clust$geometry_area))


  ## Sanity check: no clusters contained in >1 area (distance = 0)
  ## This should not happen unless survey_region_areas is corrupt with overlapping
  ## areas
  stopifnot( !duplicated(clust[!is.na(clust$longitude) & clust$distance == 0,
                               c("survey_id", "cluster_id")]) )

  ## Identify area_id nearest to the cluster coordinates
  clust <- dplyr::arrange(clust, distance)
  clust <- dplyr::group_by(clust, survey_id, cluster_id)
  clust <- dplyr::filter(clust, dplyr::row_number() == 1)
  clust <- dplyr::ungroup(clust)

  dplyr::transmute(clust,
                   survey_id,
                   cluster_id = cluster_id,
                   res_type,
                   survey_region_id,
                   longitude,
                   latitude,
                   geoloc_area_id = area_id,
                   geoloc_distance = distance)
}


#' Create individual HIV outcomes dataset from DHS
#'
#' Create dataset of indiviaul demographic and HIV outcomes.
#'
#' @param surveys data.frame of surveys, returned by `create_surveys_dhs()`.
#'
#' @return data.frame consisting of survey ID, cluster ID and individual
#'   demographic and HIV outcomes. See details.
#'
#' @details
#'
#' The following fields are extracted:
#'
#'   * survey_id
#'   * cluster_id
#'   * household
#'   * line
#'   * sex
#'   * age
#'   * dob_cmc
#'   * interview_cmc
#'   * indweight
#'   * hivstatus
#'   * arv
#'   * artself
#'   * vls
#'   * cd4
#'   * artall
#'   * hivweight
#'
#' @examples
#'
#' surveys <- create_surveys_dhs("MWI")
#' individuals <- create_individual_hiv_dhs(surveys)
#'
#' @export
create_individual_hiv_dhs <- function(surveys) {

  ird <- rdhs::dhs_datasets(surveyIds = surveys$SurveyId, fileType = "IR", fileFormat = "flat")
  mrd <- rdhs::dhs_datasets(surveyIds = surveys$SurveyId, fileType = "MR", fileFormat = "flat")
  ard <- rdhs::dhs_datasets(surveyIds = surveys$SurveyId, fileType = "AR", fileFormat = "flat")

  ird_paths <- setNames(rdhs::get_datasets(ird), ird$SurveyId)
  mrd_paths <- setNames(rdhs::get_datasets(mrd), mrd$SurveyId)
  ard_paths <- setNames(rdhs::get_datasets(ard), ard$SurveyId)

  individual <- Map(extract_individual_hiv_dhs,
                    SurveyId = surveys$SurveyId,
                    ird_path = ird_paths[surveys$SurveyId],
                    mrd_path = mrd_paths[surveys$SurveyId],
                    ard_path = ard_paths[surveys$SurveyId])

  individual <- dplyr::bind_rows(individual)
  individual <- dplyr::left_join(individual,
                                 dplyr::select(surveys, SurveyId, survey_id),
                                 by = "SurveyId")
  individual <- dplyr::select(individual, survey_id, dplyr::everything(), -SurveyId)

  individual
}


extract_individual_hiv_dhs <- function(SurveyId, ird_path, mrd_path, ard_path){

  message("Parsing IR/MR/AR individual datasets: ", SurveyId)

  ir <- readRDS(ird_path)

  if(SurveyId == "CI2005AIS"){
    ## For Cote d'Ivoire 2005 AIS, individuals are uniquely identified by
    ## four variables {cluster, structure, household, line}.
    ir$v002 <- 100L*ir$sstruct + ir$v002
  }

  if(!exists("aidsex", ir)){
    ir$aidsex <- haven::labelled(2, c("men" = 1, "women" = 2), "Sex")
  }

  if(SurveyId == "MZ2015AIS")
    ir$artself <- if_else(ir$s718 == 1, 1L, NA_integer_)
  else
    ir$artself <- NA

  dat <- ir %>%
    dplyr::transmute(cluster_id = v001,
                     household = v002,
                     line = v003,
                     interview_cmc = v008,
                     sex = factor(haven::zap_labels(aidsex), 1:2, c("male", "female")),
                     age = v012,
                     dob_cmc = v011,
                     indweight = NA,
                     artself)
  
  ## Male recode
  if (!is.null(mrd_path)) {

    mr <- readRDS(mrd_path)
    mr$aidsex <- haven::labelled(1, c("men" = 1, "women" = 2), "Sex")

    if(SurveyId == "MZ2015AIS")
      mr$artself <- if_else(mr$sm519 == 1, 1L, NA_integer_)
    else
      mr$artself <- NA

    dat <- dat %>%
      dplyr::bind_rows(
               mr %>%
               dplyr::transmute(cluster_id = mv001,
                                household = mv002,
                                line = mv003,
                                interview_cmc = mv008,
                                sex = factor(haven::zap_labels(aidsex), 1:2, c("male", "female")),
                                age = mv012,
                                dob_cmc = mv011,
                                indweight = NA,
                                artself)
             )
    
  }
  
  if (!is.null(ard_path)) {
    
    ar <- readRDS(ard_path)
    
    if (SurveyId == "CI2005AIS") {
      ar$hivnumb <- 100L*ar$hivstruct + ar$hivnumb
    }
    
    if (SurveyId == "ZM2013DHS") {
      ar$hiv03 <- ar$shiv51
    }

    if (SurveyId %in% c("MZ2015AIS", "ZM2013DHS")) {
      ar$cd4 <- ar$shiv50
    } else {
      ar$cd4 <- NA
    }

    if (SurveyId %in% c("MZ2015AIS", "LS2014DHS")) {
      ar$vls <- dplyr::if_else(ar$sviral == 9999999,
                               NA_integer_, as.integer(ar$sviral < 1000))
    } else {
      ar$vls <- NA
    }

    if (SurveyId %in% c("MZ2015AIS")) {
      ar$arv = if_else(ar$sbioarv == 9, NA_integer_, as.integer(ar$sbioarv))
    } else {
      ar$arv <- NA
    }

    if (SurveyId == "MZ2015AIS") {
      ar$recent <- dplyr::case_when(ar$slagrecn == 1 ~ 1,
                                    ar$slagrecn == 2 ~ 0)
    } else if (SurveyId == "LSAR72FL") {
      ar$recent <- dplyr::case_when(ar$srecent == 1 ~ 1,
                                    ar$srecent == 2 ~ 0)
    } else {
      ar$recent <- NA
    }

    ar <- dplyr::transmute(ar,
                           cluster_id = hivclust,
                           household = hivnumb,
                           line = hivline,
                           hivweight = hiv05 / 1e6,
                           hivstatus = dplyr::case_when(hiv03 == 0 ~ 0,
                                                        hiv03 %in% 1:3 ~ 1),
                           arv,
                           vls,
                           cd4,
                           recent)
    dat <- dplyr::left_join(dat, ar, by = c("cluster_id", "household", "line"))
  }

  dat$SurveyId <- SurveyId

  dat
}

#' Create DHS survey meta data table
#'
#' @param surveys data.frame of surveys, returned by `create_surveys_dhs()`.
#'
#' @return data.frame of survey metadata specification.
#'
#' @examples
#' surveys <- create_surveys_dhs("MWI")
#' survey_meta <- create_survey_meta_dhs(surveys)
#'
#' @export
create_survey_meta_dhs <- function(surveys) {

  publications <- rdhs::dhs_publications(surveyIds = surveys$SurveyId)

  ## Unsure if filtering on PublicationTitle == "Final Report" is most robust
  ## way to do this.
  final_rep <- dplyr::filter(publications, PublicationTitle == "Final Report")
  final_rep <- dplyr::select(final_rep, SurveyId, report_url = PublicationURL)

  stopifnot( !duplicated(final_rep$SurveyId) )

  surveys <- dplyr::left_join(surveys, final_rep, by = "SurveyId")

  surveys$dataset_url <- paste0("https://dhsprogram.com/methodology/survey/survey-display-",
                                surveys$SurveyNum, ".cfm")

  survey_meta <- dplyr::transmute(surveys,
                                  survey_id,
                                  country = CountryName,
                                  survey_type = SurveyType,
                                  survey_year = SurveyYear,
                                  fieldwork_start = FieldworkStart,
                                  fieldwork_end = FieldworkEnd,
                                  survey_mid_calendar_quarter,
                                  female_age_min =  MinAgeWomen,
                                  female_age_max = MaxAgeWomen,
                                  male_age_min = MinAgeMen,
                                  male_age_max = MaxAgeMen,
                                  report_ref = NA_character_,
                                  report_url = report_url,
                                  dataset_url,
                                  notes = NA_character_)
}


#' Create survey individuals and biomarker dataset from DHS extract
#'
#' @param dat data.frame of merged individual extract, returned by
#'   `create_individual_hiv_dhs()`.
#'
#' @return data.frame matching UNAIDS data schema
#'
#' @export
create_survey_individuals_dhs <- function(dat) {

  dplyr::transmute(
           dat,
           survey_id,
           cluster_id,
           household,
           line,
           interview_cmc,
           sex,
           age,
           dob_cmc,
           indweight
         )
}

#'
#' @rdname create_survey_individuals_dhs
#' @export

create_survey_biomarker_dhs <- function(dat) {
  dplyr::transmute(
           dat,
           survey_id,
           cluster_id,
           household,
           line,
           hivweight,
           hivstatus,
           arv,
           artself,
           vls,
           cd4,
           recent
         )
}

#' Validation of mapping to survey region areas
#'
#' @param survey_region_areas Allocation of areas to survey regions, returned by
#'   [`allocate_areas_survey_regions()`].
#' @param warn Raise a warning instead of an error (default `FALSE`)
#'
#' @return invisibly TRUE or raises an error.
#'
#' @details
#' Conducts checks on `survey_region_areas`:
#'
#' * All areas have been mapped to a survey region in each survey.
#' * All survey regions contain at least one area. Otherwise no clusters could
#'   have come from that survey region.
#'
#' Passing these checks does not confirm the mapping is accurate, but these checks
#' will flag inconsistencies that need cleaning.
#'
#' @export
validate_survey_region_areas <- function(survey_region_areas, warn = FALSE) {

  errfun <- if(warn) warning else stop

  if (any(is.na(survey_region_areas$survey_region_id))) {
    missing_survey_reg <- survey_region_areas %>%
      sf::st_drop_geometry() %>%
      dplyr::filter(is.na(survey_region_id)) %>%
      dplyr::select(survey_id, dplyr::starts_with("area_id"))

    errfun("Areas were not allocated to any survey region:\n",
           paste0(capture.output(missing_survey_reg), collapse = "\n"))
  }

  if (any(is.na(survey_region_areas$area_id))) {
    no_mapped_areas <- survey_region_areas %>%
      sf::st_drop_geometry() %>%
      dplyr::filter(is.na(area_id)) %>%
      dplyr::select(survey_id, survey_region_id, survey_region_name)
    errfun("Survey regions contained no areas:\n",
           paste0(capture.output(no_mapped_areas), collapse = "\n"))
  }

  invisible(TRUE)
}


#' Summary plot of survey cluster coordinates outside boundaries
#'
#' @param survey_clusters Survey clusteres dataset.
#' @param survey_region_boundaries Survey region boundaries dataset.
#'
#' @return A list of grobs, one for each survey.
#'
#' @details
#' The `survey_region_boundaries` dataset is used to define the scope of what
#' is plotted. A subset of regions can be plotted by subsetting that dataset
#' to the desired range.
#'
#' @export
plot_survey_coordinate_check <- function(survey_clusters,
                                         survey_region_boundaries,
                                         survey_region_areas) {

  survey_clusters <- dplyr::semi_join(survey_clusters,
                                      survey_region_boundaries,
                                      by = c("survey_id", "survey_region_id"))

  survey_region_areas <- dplyr::semi_join(survey_region_areas,
                                          survey_region_boundaries,
                                          by = c("survey_id", "survey_region_id"))
  
  clust_spl <- split(survey_clusters, survey_clusters$survey_id)
  region_spl <- split(survey_region_boundaries, survey_region_boundaries$survey_id)
  area_spl <- split(survey_region_areas, survey_region_areas$survey_id)
  
  plot_one <- function(clust, regions, areas) {
    
    subtitle <- sprintf("Total survey clusters: %d\nClusters missing coordinates: %d\nClusters outside region boundaries: %d",
                        nrow(clust),
                        sum(is.na(clust$geoloc_area_id)),
                        sum(clust$geoloc_distance > 0, na.rm = TRUE))

    regions <- dplyr::arrange(regions, survey_id, survey_region_id)
    regions$survey_region_name <- forcats::as_factor(regions$survey_region_name)
    
    clust <- dplyr::left_join(
                      clust,
                      sf::st_drop_geometry(regions),
                      by = c("survey_id", "survey_region_id")
                    )

    outside_clusters <- dplyr::filter(clust, geoloc_distance > 0)
    contained_clusters <- dplyr::filter(clust, geoloc_distance == 0)

    areas <- sf::st_as_sf(areas)
    areas$survey_region_name <- factor(areas$survey_region_name,
                                       levels(regions$survey_region_name))

    ggplot2::ggplot() +
      ggplot2::geom_sf(ggplot2::aes(fill = survey_region_name),
                       data = areas,
                       color = "grey70",
                       size = 0.05,
                       alpha = 0.2) +
        ggplot2::geom_sf(data = regions, fill = NA, size = 0.5) +
        ggplot2::geom_point(ggplot2::aes(longitude, latitude),
                            color = "grey70", size = 0.1,
                            data = contained_clusters) +
        ggplot2::geom_point(ggplot2::aes(longitude, latitude, color = survey_region_name),
                            data = outside_clusters) +
        ggplot2::scale_color_hue(l = 55, drop = FALSE) +
        ggplot2::scale_fill_hue(l = 75, drop = FALSE) +
        ggplot2::theme_minimal() +
        ggplot2::theme(panel.grid = ggplot2::element_blank(),
                       axis.text = ggplot2::element_blank(),
                       axis.title = ggplot2::element_blank(),
                       plot.title = ggplot2::element_text(face = "bold")) +
        ggplot2::ggtitle(areas$survey_id[1], subtitle)
  }

  Map(plot_one, clust_spl, region_spl[names(clust_spl)], area_spl[names(clust_spl)])
}
