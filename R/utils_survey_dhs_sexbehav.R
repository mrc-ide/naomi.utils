#' Create individual sexual behaviour variables from DHS
#'
#' Create dataset of individual sexual behaviour variables.
#'
#' @param surveys data.frame of surveys, returned by `create_surveys_dhs()`.
#'
#' @return data.frame consisting of survey ID, cluster ID and individual
#'   sexual behaviour outcomes. See details.
#'
#' @details
#'
#' Each respondent is placed in one risk group for the past 12 months, the
#' highest that applies: paid sex (`sexpaid12m`), non-regular or multiple
#' partners (`sexnonreg`), one cohabiting partner (`sexcohab`), or no sex
#' (`nosex12m`). The `*spouse` variants count a spouse living elsewhere as
#' cohabiting rather than non-regular. `sexnonregplus` is non-regular or paid.
#' `giftsvar` flags whether the survey asked about exchanging sex for gifts
#' (v791a).
#'
#' Respondents whose group can't be determined are NA in every group, so they
#' drop out of all numerators and denominators. Missing-value rules:
#'
#'   * 1 partner, partner type missing: cohabiting if marital status shows
#'     they live with the partner (v501 = 2, or v501 = 1 and v504 = 1);
#'     otherwise excluded.
#'   * Number of partners missing, partner is a spouse or live-in partner:
#'     cohabiting, or non-regular if a 2nd or 3rd partner is reported.
#'   * Number of partners and partner type missing, partner lives elsewhere:
#'     non-regular if they had sex in the past 12 months (v527); otherwise
#'     excluded. This also excludes respondents never asked the sexual
#'     behaviour module (e.g. the KDHS 2022 short questionnaire).
#'   * Number of partners "don't know" (98): non-regular, as before.
#'
#' @examples
#' \dontrun{
#' surveys <- create_surveys_dhs("MWI")
#' sexbehav <- create_sexbehav_dhs(surveys)
#' }
#'
#' @export
create_sexbehav_dhs <- function(surveys) {

  ird <- rdhs::dhs_datasets(fileType = "IR", fileFormat = "flat")
  mrd <- rdhs::dhs_datasets(fileType = "MR", fileFormat = "flat")

  ird <- dplyr::filter(ird, SurveyId %in% surveys$SurveyId)
  mrd <- dplyr::filter(mrd, SurveyId %in% surveys$SurveyId)

  # doesn't like this line unless the model_datasets data is loaded
  ird_paths <- stats::setNames(rdhs::get_datasets(ird), ird$SurveyId)

  if (nrow(mrd) > 0) {
    mrd_paths <- stats::setNames(rdhs::get_datasets(mrd), mrd$SurveyId)
  } else {
    mrd_paths <- list(NULL)
  }

  df <- Map(extract_sexbehav_dhs,
            SurveyId = surveys$SurveyId,
            ird_path = ird_paths[surveys$SurveyId],
            mrd_path = mrd_paths[surveys$SurveyId])

  df <- dplyr::bind_rows(df)
  df <- dplyr::left_join(df,
                         dplyr::select(surveys, SurveyId, survey_id),
                         by = "SurveyId")
  df <- dplyr::select(df, survey_id, dplyr::everything(), -SurveyId)

  df
}


extract_sexbehav_dhs <- function(SurveyId, ird_path, mrd_path){

  message("Parsing IR/MR Sexual Behaviour datasets: ", SurveyId)

  sb_vars <- c(
    "v504", "v529", "v531",
    "v766b", "v767a", "v767b",
    "v767c", "v791a", "v763a",
    "v763b", "v763c", "v501", "v527"
  )

  ## Individual recode
  # Drop value labels: IR and MR label the same variable differently, which
  # bind_rows() can't combine unless haven happens to be loaded
  ir <- haven::zap_labels(readRDS(ird_path))
  dat <- dplyr::select(ir, individual_id = caseid, tidyselect::any_of(sb_vars))
  dat[setdiff(sb_vars, names(dat))] <- NA

  ## Male recode
  if (!is.null(mrd_path)) {
    spec_fvars <- which(sb_vars %in% c("v791a"))
    sb_mvars <- paste0("m", sb_vars[-spec_fvars])
    sb_mvars <- c(sb_mvars, "mv793")
    mr <- haven::zap_labels(readRDS(mrd_path))
    mdat <- dplyr::select(mr, individual_id = mcaseid, tidyselect::any_of(sb_mvars))
    mdat[setdiff(sb_mvars, names(mdat))] <- NA
    names(mdat) <- sub("^mv", "v", names(mdat))

    dat <- dplyr::bind_rows(dat, mdat)
  }
  if (!"v793" %in% names(dat)) dat$v793 <- NA

  dat$SurveyId <- SurveyId

  dat <- code_sexbehav_dhs(dat)

  # giftsvar = whether the survey asked v791a (any non-missing observation)
  dat$giftsvar <- as.numeric(sum(!is.na(dat$v791a)) > 0)

  dplyr::select(
    dat,
    SurveyId, individual_id, sex12m, nosex12m, sexcohab, sexcohabspouse, sexnonreg,
    sexnonregspouse, sexpaid12m, giftsvar, sexnonregplus, sexnonregspouseplus
  )
}


## Assign each respondent one risk group from the DHS sexual behaviour recode
## variables. See create_sexbehav_dhs() for the rules.
##
## v766b: number of partners in the past 12 months (98 don't know, 99 missing)
## v767a-c: relationship with the 3 most recent partners. The labels in the
##   data don't match the questionnaire; correct coding is 1 spouse,
##   2 boyfriend not living with respondent, 3 other friend, 4 casual
##   acquaintance, 5 relative, 6 commercial sex worker, 7 live-in partner,
##   96 other, 99 missing.
## v504: partner lives with her (1) or elsewhere (2), married/in union only
## v501: marital status (1 married, 2 living with partner)
## v527: time since last sex (1xx days, 2xx weeks, 3xx months, 4xx years,
##   995 within last 4 weeks)
## v791a: sex for gifts/cash (women 15-24); v793: paid for sex (men)
code_sexbehav_dhs <- function(dat) {
  cas_cats <- c(2, 3, 4, 5, 6, 96)
  type_in <- function(codes) {
    (dat$v767a %in% codes) | (dat$v767b %in% codes) | (dat$v767c %in% codes)
  }
  known <- function(x) !is.na(x) & x != 99

  n_known <- dat$v766b %in% 0:95
  n_dk <- dat$v766b %in% 98
  n_missing <- !n_known & !n_dk
  type_known <- known(dat$v767a)
  later_partner <- known(dat$v767b) | known(dat$v767c)
  spouse_or_livein <- dat$v767a %in% c(1, 7)

  ## Sex in the past 12 months from time since last sex
  sex_last_year <- dplyr::case_when(
    dat$v527 %in% c(100:199, 200:299, 300:311, 995) ~ TRUE,
    dat$v527 %in% c(312:399, 400:499, 996) ~ FALSE,
    TRUE ~ NA
  )

  sex12m <- dplyr::case_when(
    dat$v766b %in% 0 ~ FALSE,
    n_known | n_dk ~ TRUE,
    type_known ~ TRUE,
    TRUE ~ sex_last_year
  )

  paid <- dat$v791a %in% 1 | type_in(6) | dat$v793 %in% 1

  ## Non-regular, shared by both definitions: several partners, a casual
  ## partner type, or partner count missing with a 2nd/3rd partner reported
  nonreg_common <- (n_known & dat$v766b > 1) | n_dk | type_in(cas_cats) |
    (n_missing & type_known & later_partner)

  ## Partner count missing and type missing, partner lives elsewhere
  away_unknown <- n_missing & !type_known & dat$v504 %in% 2

  ## One partner type missing: cohabiting if marital status shows they live
  ## with the partner
  one_untyped <- dat$v766b %in% 1 & !type_known
  lives_with <- dat$v501 %in% 2 | (dat$v501 %in% 1 & dat$v504 %in% 1)

  ## Partner count missing, only partner is a spouse or live-in partner
  missing_n_spouse <- n_missing & spouse_or_livein & !later_partner

  group <- dplyr::case_when(
    sex12m %in% FALSE ~ "nosex",
    is.na(sex12m) ~ NA_character_,
    paid ~ "paid",
    nonreg_common | dat$v504 %in% 2 ~ "nonreg",
    dat$v766b %in% 1 & type_in(7) ~ "cohab",
    dat$v766b %in% 1 & type_in(1) & dat$v504 %in% c(1, 9, NA) ~ "cohab",
    one_untyped & lives_with ~ "cohab",
    missing_n_spouse & (dat$v767a %in% 7 | dat$v504 %in% c(1, 9, NA)) ~ "cohab",
    TRUE ~ NA_character_
  )

  ## Spouse definition: a spouse living elsewhere counts as cohabiting
  group_spouse <- dplyr::case_when(
    sex12m %in% FALSE ~ "nosex",
    is.na(sex12m) ~ NA_character_,
    paid ~ "paid",
    nonreg_common ~ "nonreg",
    dat$v766b %in% 1 & spouse_or_livein ~ "cohab",
    dat$v504 %in% 2 & dat$v501 %in% 1 ~ "cohab",
    one_untyped & dat$v501 %in% c(1, 2) ~ "cohab",
    missing_n_spouse ~ "cohab",
    away_unknown ~ "nonreg",
    dat$v766b %in% 1 & dat$v504 %in% 2 ~ "nonreg",
    TRUE ~ NA_character_
  )

  set_groups(dat, sex12m, group, group_spouse)
}


## Turn risk-group assignments into the 0/1 indicator columns. A respondent
## with no group is NA in every group column; sex12m is kept as reported.
set_groups <- function(dat, sex12m, group, group_spouse) {
  is_group <- function(g, value) as.numeric(ifelse(is.na(g), NA, g == value))
  dat$sex12m <- as.numeric(sex12m)
  dat$nosex12m <- is_group(group, "nosex")
  dat$sexcohab <- is_group(group, "cohab")
  dat$sexnonreg <- is_group(group, "nonreg")
  dat$sexpaid12m <- is_group(group, "paid")
  dat$sexnonregplus <- as.numeric(ifelse(is.na(group), NA, group %in% c("nonreg", "paid")))
  dat$sexcohabspouse <- is_group(group_spouse, "cohab")
  dat$sexnonregspouse <- is_group(group_spouse, "nonreg")
  dat$sexnonregspouseplus <- as.numeric(ifelse(is.na(group_spouse), NA,
                                               group_spouse %in% c("nonreg", "paid")))
  dat
}
