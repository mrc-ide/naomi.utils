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
#' for sex in the past 12 months. Variable `sti12m` is whether a respondent 
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
  
  sb_vars <- c("v529", "v531", "v766b", "v767a", "v767b", "v767c", "v791a", 
               "v763a", "v763b", "v763c", "v501")
  
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
  
  # eversex = whether participant has ever had sex
  # Recode v531 - age of sexual debut - anyone coded 0 hasn't had sex, 
  # 97 = inconsistent, 98 = Don't Know, 99 = Missing
  # Assume if you don't know the age of sexual debut that you have had sex
  # Should inconsistents be TRUE instead of missing?
  dat$eversex <- dplyr::case_when(dat$v531 %in% c(97,99) ~ NA,
                                  dat$v531 == 98 ~ TRUE,
                                  TRUE ~ dat$v531 > 0)
  
  # sex12m = whether reports sexual activity in past 12 mo 
  # Recode v766b - number of partners in the past 12 mo
  # use v527/v529 instead?
  dat$sex12m <- dplyr::case_when(dat$v766b == 0 ~ FALSE,
                                  dat$v766b == 99 ~ NA,
                                  TRUE ~ dat$v766b > 0)
  
  # sexcohab = whether reports sex with only one cohabiting partner in the past
  # 12 mo.  Recode v766b (# partners in past 12 mo) and v767a-c (relationship
  # w/partners, 1/2 is cohabiting)
  # Currently if your partner type is missing or inconsistent and 
  # you had only one partner in the past year you are classified as a "yes"
  # for having sex with only one cohabiting partner
  dat$sexcohab <- dplyr::case_when(dat$sex12m == FALSE ~ FALSE,
                                   dat$v766b == 1 & ((!dat$v767a %in% 3:6) &
                                                       (!dat$v767b %in% 3:6) &
                                                       (!dat$v767c %in% 3:6)) ~ TRUE,
                                   dat$v766b == 99 ~ NA,
                                   TRUE ~ FALSE)
  
  # sexnonreg = whether the person reports having non-regular sexual partner(s)
  # or multiple partners in the past year. Recode v766b (# partners in past 12 mo) 
  # and v767a-c (relationship w/partners, 1/2 is cohabiting)
  dat$sexnonreg <- dplyr::case_when(dat$sex12m == FALSE ~ FALSE,
                                    (dat$v766b > 1 & dat$v766b != 99) | 
                                      (dat$v767a %in% 3:6 | dat$v767b %in% 3:6 |
                                         dat$v767c %in% 3:6) ~ TRUE,
                                    dat$v766b == 99 ~ NA,
                                    TRUE ~ FALSE)
  
  # sexpaid12m = whether the person reports having received gifts/cash/anything
  # in exchange for sex (women aged 15-24, recode v791a or v767a-c), or paid for sex in the
  # past 12 months (men, recode v793 and v767a-c)
  # v791a is only collected if 15-24 yo woman has never been in a union - should probably
  # make this var NA if woman is over 25 or if v501 (marital status) is missing
  dat$sexpaid12m <- dplyr::case_when(dat$v791a == 1 | 
                                        (dat$v767a == 5 | dat$v767b == 5 | 
                                           dat$v767c == 5) | dat$v793 == 1 ~ TRUE,
                                       TRUE ~ FALSE)
  
  # sti12m = whether the person reports having had an STI, genital sore/ulcer, or 
  # genital discharge in the past 12 months (recode v763a-c)
  # Only set as NA if were a don't know/missing for all three of the questions
  dat$sti12m <- dplyr::case_when(dat$v763a == 1 | dat$v763b == 1 | 
                                    dat$v763c == 1 ~ TRUE,
                                  dat$v763a %in% c(8,9) & dat$v763b %in% c(8,9) &
                                    dat$v763c %in% c(8,9) ~ NA,
                                  is.na(dat$v763a) & is.na(dat$v763b) & 
                                    is.na(dat$v763c) ~ NA,
                                  TRUE ~ FALSE)
  
  dat$SurveyId <- SurveyId
  dat$eversex <- as.integer(dat$eversex)
  dat$sex12m <- as.integer(dat$sex12m)
  dat$sexcohab <- as.integer(dat$sexcohab)
  dat$sexnonreg <- as.integer(dat$sexnonreg)
  dat$sexpaid12m <- as.integer(dat$sexpaid12m)
  dat$sti12m <- as.integer(dat$sti12m)
  
  dplyr::select(dat, SurveyId, individual_id, eversex, sex12m, sexcohab, sexnonreg, sexpaid12m, sti12m)
}
