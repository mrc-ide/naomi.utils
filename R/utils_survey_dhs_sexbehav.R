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
#' The following fields are extracted:
#'
#'   * survey_id
#'   * individual_id
#'   * eversex
#'   * sex12m
#'   * sexcohab
#'   * sexnonreg
#'   * sexpaid12mo
#'   * giftsvar
#'   * sti12mo
#'
#' Variable `eversex` is the outcome ever reporting sexual activity.
#' Variable `sex12m` is whether an individual reports having been sexually
#' active in the past 12 months. Variable `sexcohab` is whether an individual
#' reports being sexually active in the past 12 months with only one cohabiting
#' partner. Variable `sexnonreg` is whether the individual reports having
#' non-regular sexual partner(s) or multiple partners in the past year. Variable
#' `sexpaid12m` for females is whether respondent reports having had sex in return
#' for cash, gifts, or anything else in the past 12 months (only asked of 15-24
#' y/o women in DHS7) and for males is whether respondent reports having paid
#' for sex in the past 12 months. Variable `giftsvar` is an indicator of whether
#' the survey includes the question among AGYW in DHS7 on cash/gifts/etc in
#' exchange for sex. Variable `sti12m` is whether a respondent
#' reports having any STD, genital sore/ulcer, or genital discharge in the last
#' 12 months.
#'
#' @examples
#'
#' surveys <- create_surveys_dhs("MWI")
#' sexbehav <- create_sexbehav_dhs(surveys)
#'
#' @export
create_sexbehav_dhs <- function(surveys) {

  ird <- rdhs::dhs_datasets(fileType = "IR", fileFormat = "flat")
  mrd <- rdhs::dhs_datasets(fileType = "MR", fileFormat = "flat")

  ird <- dplyr::filter(ird, SurveyId %in% surveys$SurveyId)
  mrd <- dplyr::filter(mrd, SurveyId %in% surveys$SurveyId)

  # doesn't like this line unless the model_datasets data is loaded
  ird_paths <- setNames(rdhs::get_datasets(ird), ird$SurveyId)

  if (nrow(mrd) > 0) {
    mrd_paths <- setNames(rdhs::get_datasets(mrd), mrd$SurveyId)
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
    "v763b", "v763c", "v501"
  )

  ## Individual recode
  ir <- readRDS(ird_path)
  dat <- dplyr::select(ir, individual_id = caseid, tidyselect::any_of(sb_vars))
  dat[setdiff(sb_vars, names(dat))] <- NA

  ## Male recode
  if (!is.null(mrd_path)) {
    spec_fvars <- which(sb_vars %in% c("v791a"))
    sb_mvars <- paste0("m", sb_vars[-spec_fvars])
    sb_mvars <- c(sb_mvars, "mv793")
    mr <- readRDS(mrd_path)
    mdat <- dplyr::select(mr, individual_id = mcaseid, tidyselect::any_of(sb_mvars))
    mdat[setdiff(sb_mvars, names(mdat))] <- NA
    names(mdat) <- sub("^mv", "v", names(mdat))

    dat <- dplyr::bind_rows(dat, mdat)
  }

  ## # Code sexual behaviour outcomes

  # # eversex = whether participant has ever had sex
  # # Recode v531 - age of sexual debut - anyone coded 0 hasn't had sex,
  # # 97 = inconsistent, 98 = Don't Know, 99 = Missing
  # # Assume if you don't know the age of sexual debut that you have had sex
  # # Should inconsistents be TRUE instead of missing?
  # dat$eversex <- dplyr::case_when(dat$v531 %in% c(97,99) ~ NA,
  #                                 dat$v531 == 98 ~ TRUE,
  #                                 TRUE ~ dat$v531 > 0)

  # sex12m = whether reports sexual activity in past 12 mo
  # Recode v766b - number of partners in the past 12 mo
  # use v527/v529 instead?
  dat$sex12m <- dplyr::case_when(
    dat$v766b == 0 ~ FALSE,
    dat$v766b == 99 ~ NA,
    TRUE ~ dat$v766b > 0
  )

  # Does not report sexual activity in the last 12 months
  dat$nosex12m = case_when(
    dat$sex12m == TRUE ~ FALSE,
    dat$sex12m == FALSE ~ TRUE,
    is.na(dat$sex12m) ~ NA
  )

  # sexcohab = whether reports sex with only one cohabiting partner in the past
  # 12 mo.  Recode v766b (# partners in past 12 mo) and v767a-c (relationship
  # w/partners)
  # Currently if your partner type is missing or inconsistent and
  # you had only one partner in the past year you are classified as a "yes"
  # for having sex with only one cohabiting partner

  # v767a-c coding does not match DHS questionnaire and isn't labelled - correct is:
  # 1 = spouse, 2 = boyfriend not living with respondent, 3 = other friend
  # 4 = casual acquaintance, 5 = relative, 6 = commercial sex worker,
  # 7 = live-in partner, 96 = other

  # v504 = whether the partner lives in the household or is now living elsewhere (for currently
  # married or in union women):
  # 1 = living with women
  # 2 = living elsewhere
  # 9 = missing
  # NA = missing

  dat$sexcohab <- dplyr::case_when(
    dat$sex12m == FALSE ~ FALSE,
    dat$v766b == 1 & ((dat$v767a == 7) | (dat$v767b == 7) | (dat$v767c == 7)) ~ TRUE,
    dat$v766b == 1 & ((dat$v767a == 1) | (dat$v767b == 1) | (dat$v767c == 1)) & dat$v504 %in% c(1, 9, NA) ~ TRUE,
    TRUE ~ FALSE
  )

  dat$sexcohabspouse <- dplyr::case_when(
    dat$sex12m == FALSE ~ FALSE,
    dat$v766b == 1 & ((dat$v767a %in% c(1, 7)) | (dat$v767b %in% c(1, 7)) | (dat$v767c %in% c(1, 7))) ~ TRUE,
    dat$v504 == 2 & dat$v501 == 1 ~ TRUE, # Partner lives away and legally married
    TRUE ~ FALSE
  )

  cas_cats <- c(2, 3, 4, 5, 6, 96)

  # sexnonreg = whether the person reports having non-regular sexual partner(s)
  # or multiple partners in the past year. Recode v766b (# partners in past 12 mo)
  # and v767a-c (relationship w/partners)
  dat$sexnonreg <- dplyr::case_when(
    dat$sex12m == FALSE ~ FALSE,
    dat$v504 == 2 ~ TRUE,
    (dat$v767a %in% cas_cats) | (dat$v767b %in% cas_cats) | (dat$v767c %in% cas_cats) ~ TRUE,
    dat$v766b > 1 & dat$v766b != 99 ~ TRUE,
    dat$v766b == 99 ~ NA,
    TRUE ~ FALSE
  )

  dat$sexnonregspouse <- dplyr::case_when(
    dat$sex12m == FALSE ~ FALSE,
    dat$v766b == 1 & ((dat$v767a == 1) | (dat$v767b == 1) | (dat$v767c == 1)) & dat$v504 == 2 ~ FALSE,
    (dat$v767a %in% cas_cats) | (dat$v767b %in% cas_cats) | (dat$v767c %in% cas_cats) ~ TRUE,
    dat$v766b > 1 & dat$v766b != 99 ~ TRUE,
    dat$v766b == 99 ~ NA,
    TRUE ~ FALSE
  )

  # sexpaid12m = whether the person reports having received gifts/cash/anything
  # in exchange for sex (women aged 15-24, recode v791a or v767a-c), or paid for sex in the
  # past 12 months (men, recode v793 and v767a-c)
  # v791a is only collected if 15-24 yo woman has never been in a union - should probably
  # make this var NA if woman is over 25 or if v501 (marital status) is missing
  if(!is.null(mrd_path)) {
    dat$sexpaid12m <- dplyr::case_when(
      dat$sex12m == FALSE ~ FALSE,
      dat$v791a == 1 | (dat$v767a == 6 | dat$v767b == 6 | dat$v767c == 6) | dat$v793 == 1 ~ TRUE,
      (is.na(dat$v791a) & is.na(dat$v767a) & is.na(dat$v767b) & is.na(dat$v767c)) | (is.na(dat$v793) &
      is.na(dat$v767a) & is.na(dat$v767b) & is.na(dat$v767c)) ~ NA,
      TRUE ~ FALSE
    )
  } else {
    dat$sexpaid12m <- dplyr::case_when(
      dat$sex12m == FALSE ~ FALSE,
      dat$v791a == 1 | (dat$v767a == 6 | dat$v767b == 6 | dat$v767c == 6) ~ TRUE,
      (is.na(dat$v791a) & is.na(dat$v767a) & is.na(dat$v767b) & is.na(dat$v767c)) ~ NA,
      TRUE ~ FALSE
    )
  }

  # Either sexnonreg or sexpaid12m
  dat$sexnonregplus <- case_when(
    dat$sexnonreg == TRUE ~ TRUE,
    dat$sexpaid12m == TRUE ~ TRUE,
    TRUE ~ FALSE
  )

  # Either sexnonregspouse or sexpaid12m
  dat$sexnonregspouseplus = case_when(
    dat$sexnonregspouse == TRUE ~ TRUE,
    dat$sexpaid12m == TRUE ~ TRUE,
    TRUE ~ FALSE
  )

  # giftsvar = indicator for whether the survey includes any non-missing observations
  # on question v791a (i.e. whether it was in the questionnaire)
  dat <- dplyr::mutate(dat, giftsvar = ifelse(sum(!is.na(v791a)) > 0, 1, 0))

  # # sti12m = whether the person reports having had an STI, genital sore/ulcer, or
  # # genital discharge in the past 12 months (recode v763a-c)
  # # Only set as NA if were a don't know/missing for all three of the questions
  # dat$sti12m <- dplyr::case_when(dat$v763a == 1 | dat$v763b == 1 |
  #                                   dat$v763c == 1 ~ TRUE,
  #                                 dat$v763a %in% c(8,9) & dat$v763b %in% c(8,9) &
  #                                   dat$v763c %in% c(8,9) ~ NA,
  #                                 is.na(dat$v763a) & is.na(dat$v763b) &
  #                                   is.na(dat$v763c) ~ NA,
  #                                 TRUE ~ FALSE)

  dat$SurveyId <- SurveyId

  # Alterations to make the outcomes closer to being categorical
  # As well as adding in a new variable, sexnonregplus, which is sexnonreg with all
  # the individuals in sexpaid12m added on as well
  dat %>%
    dplyr::select(
      SurveyId, individual_id, sex12m, nosex12m, sexcohab, sexcohabspouse, sexnonreg,
      sexnonregspouse, sexpaid12m, giftsvar, sexnonregplus, sexnonregspouseplus
    ) %>%
    dplyr::mutate(
      # Just want the highest risk category that an individual belongs to
      nosex12m = ifelse(sexcohab | sexnonreg | sexpaid12m, FALSE, nosex12m),
      sexcohab = ifelse(sexnonreg | sexpaid12m, FALSE, sexcohab),
      sexcohabspouse = ifelse(sexnonregspouse | sexpaid12m, FALSE, sexcohabspouse),
      sexnonreg = ifelse(sexpaid12m, FALSE, sexnonreg),
      sexnonregspouse = ifelse(sexpaid12m, FALSE, sexnonregspouse),
      # Turn everything from TRUE / FALSE coding to 1 / 0
      dplyr::across(sex12m:sexnonregspouseplus, ~ as.numeric(.x))
    )
}
