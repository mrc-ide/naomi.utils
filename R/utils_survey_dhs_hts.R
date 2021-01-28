#' Create individual HIV testing history variables from DHS
#'
#' Create dataset of individual HIV testing history variables.
#'
#' @param surveys data.frame of surveys, returned by `create_surveys_dhs()`.
#'
#' @return data.frame consisting of survey ID, cluster ID and individual
#'   HIV testing history outcomes. See details.
#'
#' @details
#'
#' The following fields are extracted:
#'
#'   * survey_id
#'   * individual_id
#'   * evertest
#'   * test12m
#'
#' Variable `evertest` is the outcome ever been tested for HIV and received
#' the result of the most recent HIV test. Variable `test12m` is whether
#' the individual has been tested for HIV and received the result in the
#' past 12 months.
#' 
#' @examples
#'
#' surveys <- create_surveys_dhs("MWI")
#' hts <- create_hivtesting_dhs(surveys)
#'
#' @export
create_hivtesting_dhs <- function(surveys) {

  ird <- rdhs::dhs_datasets(fileType = "IR", fileFormat = "flat")
  mrd <- rdhs::dhs_datasets(fileType = "MR", fileFormat = "flat")

  ird <- dplyr::filter(ird, SurveyId %in% surveys$SurveyId) 
  mrd <- dplyr::filter(mrd, SurveyId %in% surveys$SurveyId)

  ird_paths <- setNames(rdhs::get_datasets(ird), ird$SurveyId)

  if (nrow(mrd) > 0) {
    mrd_paths <- setNames(rdhs::get_datasets(mrd), mrd$SurveyId)
  } else {
    mrd_paths <- list(NULL)
  }

  df <- Map(extract_hivtesting_dhs,
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


extract_hivtesting_dhs <- function(SurveyId, ird_path, mrd_path){

  message("Parsing IR/MR HIV testing datasets: ", SurveyId)

  hts_vars <- c("v751", "v781", "v826", "v826a", "v828", "v840a")

  ## Individual recode
  ir <- readRDS(ird_path)
  dat <- dplyr::select(ir, individual_id = caseid, tidyselect::any_of(hts_vars))
  dat[setdiff(hts_vars, names(dat))] <- NA

  dat$has_test12m <- !all(is.na(dat$v826) & is.na(dat$v826a))

  if (all(is.na(dat$v781))) {
    ## Survey does not have ever been tested
    dat <- dat[integer(0), ]
  }
  
  ## Male recode
  if (!is.null(mrd_path)) {
    hts_mvars <- paste0("m", hts_vars)
    mr <- readRDS(mrd_path)
    mdat <- dplyr::select(mr, individual_id = mcaseid, tidyselect::any_of(hts_mvars))
    names(mdat) <- sub("^mv", "v", names(mdat))
    mdat[setdiff(hts_vars, names(mdat))] <- NA

    mdat$has_test12m <- !all(is.na(mdat$v826) & is.na(mdat$v826a))

    if (all(is.na(mdat$v781))) {
      ## Survey does not have ever been tested
      mdat <- mdat[integer(0), ]
    }
    
    dat <- dplyr::bind_rows(dat, mdat)
  }

  ## # Code HIV testing history outcomes

  ## Correcting Burundi DHS 2010
  if (SurveyId == "BU2010DHS") {
    ## not applicalble among women on v840a (question 922) was incorrectly coded as missing
    ## when in fact it was 'not applicable'
    dat$v781 <- dplyr::case_when(dat$v781 == 9 & is.na(dat$v840a) ~ 0,
                                 dat$v781 == 0 & dat$v840a == 9 ~ 9,
                                 TRUE ~ dat$v781)
  }
  
  dat$evertest <- dplyr::na_if(dat$v781, 9) > 0

  ## Assume if never heard of HIV/AIDS, you have never been tested
  dat$evertest[!is.na(dat$v751) & dat$v751 == 0] <- FALSE
  
  ## Received results or not
  if (!all(is.na(dat$v828))) {
    ## If 'received result' is missing, assume not received (FALSE)
    dat$not_received_result <- dat$v828 %in% c(0, 9) | is.na(dat$v828)

    ## Only recode if evertest is not missing
    dat$evertest[!is.na(dat$evertest) & dat$not_received_result] <- FALSE
  }

  #' Recode v826a to v826
  #' c("less than 12 months"=1, "12-23 months"=2, "2 years or more"=3),
  dat$v826r <- as.numeric(dat$v826a)
  dat$v826r <- dplyr::na_if(dat$v826r, 98)
  dat$v826r <- dplyr::na_if(dat$v826r, 99)
  dat$v826r <- cut(dat$v826r, breaks = c(0, 12, 24, Inf),
                   labels = FALSE, include.lowest = TRUE, right = FALSE)
  
  dat$v826[is.na(dat$v826)] <- dat$v826r[is.na(dat$v826)]
  dat$v826 <- dplyr::na_if(dat$v826, 9)

  dat$test12m <- dat$evertest & dat$v826 == 1
  dat$test12m[!dat$has_test12m] <- NA
  
  dat$SurveyId <- SurveyId
  dat$evertest <- as.integer(dat$evertest)
  dat$test12m <- as.integer(dat$test12m)

  dplyr::select(dat, SurveyId, individual_id, evertest, test12m)
}
