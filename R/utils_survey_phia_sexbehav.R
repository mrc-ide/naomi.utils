#' Extract sexual behaviour categorical variables from PHIA surveys.
#'
#' Same risk groups as [create_sexbehav_dhs()]. Sex in the past 12 months comes
#' from the PHIA derived flags `sexever` and `sex12months`, present in every
#' PHIA. Missing-value rules:
#'
#'   * Never had sex, or no sex in the past 12 months: no sex.
#'   * `sex12months` missing: excluded, unless 0 partners (no sex) or number
#'     of partners "don't know" (non-regular, consistent with DHS).
#'   * Age at first sex refused/don't know and number of partners blank:
#'     excluded.
#'   * Had sex, number of partners blank, "don't know" or out of range (-7):
#'     non-regular, in every PHIA round. Refused: excluded.
#'   * One partner, partner type "don't know", "refused" or blank: excluded.
#'
#' Respondents whose group can't be determined are NA in every group column.
#'
#' @param ind Individuals dataset.
#' @param survey_id The survey name.
#' @return Sexual behaviour categorical variables
#' @export
extract_sexbehav_phia <- function(ind, survey_id) {
  # Drop value labels so comparisons and bind_rows() don't depend on haven
  ind <- haven::zap_labels(ind)
  sb_vars <- c(
    "firstsxage", # Age at first vaginal sex
    "firstsxagedk", # Age at first vaginal sex (don't know)
    "part12monum", # Total sexual partners (past 12 months)
    "part12modkr", # Total sexual partners (past 12 months) (don't know)
    # Does partner i live in this household, where:
    # 1 = Yes
    # 2 = No
    paste0("partlivew", 1:3),
    # Relationship to partner i, where:
    # 1 = Husband or wife
    # 2 = Live-in partner
    # 3 = Partner (not living with)
    # 4 = Ex-spouse or partner
    # 5 = Friend or acquantance
    # 6 = Sex worker
    # 7 = Sex worker client
    # 8 = Stranger
    # 96 = Other
    paste0("partrelation", 1:3),
    paste0("partlastsup", 1:3), # Expectation of gifts, payment, other help with partner i
    "sellsx12mo", # Had sex for money, gifts during past 12 months
    "buysx12mo", # Paid money or given gifts for sex during past 12 months
    "sexever", # Derived: ever had sex (1 yes, 2 no, 99 missing)
    "sex12months" # Derived: sex in past 12 months (1 yes, 2 no, 99 missing)
  )

  # The following other variables may be of future interest, but are not in all the surveys:
  # * analsxever: Age at first anal sex
  # * lifetimesex: Total lifetime sexual partners
  # * lifetimesexdk: Total lifetime sexual partners
  # * paste0("partlastsxtimed", 1:3): How long since last sex with partner i

  # Fix issues with particular surveys having different variable names
  if(survey_id %in% c("ZWE2016PHIA")) { ind <- dplyr::rename(ind, "part12modkr" = "part12monumdk") }
  if(survey_id %in% c("ZMB2016PHIA")) { ind <- dplyr::rename(ind, "part12monum" = "part12mo") }
  if(survey_id %in% c("MWI2016PHIA")) { ind <- dplyr::rename(ind, "part12modkr" = "part12monumdk") }

  dat <- ind %>%
    dplyr::mutate(
      survey_id = survey_id,
      individual_id = personid
    ) %>%
    dplyr::select(survey_id, individual_id, tidyselect::any_of(sb_vars))

  # Fixes issue with e.g. some surveys not having paid sex questions
  dat[setdiff(sb_vars, names(dat))] <- NA

  dat <- code_sexbehav_phia(dat)
  dat$giftsvar <- as.numeric(sum(!is.na(dat$sellsx12mo)) > 0)

  dplyr::select(dat, survey_id, individual_id, sex12m, nosex12m, sexcohab,
                sexcohabspouse, sexnonreg, sexnonregspouse, sexpaid12m, giftsvar,
                sexnonregplus, sexnonregspouseplus)
}

## Assign each respondent one risk group. Negative codes: -9 refused,
## -8 don't know, -7 out of range. PHIA1 records don't know/refused number of
## partners in part12modkr; PHIA2 records them in part12monum.
code_sexbehav_phia <- function(dat) {
  rel <- function(codes) {
    (dat$partrelation1 %in% codes) | (dat$partrelation2 %in% codes) | (dat$partrelation3 %in% codes)
  }

  n_known <- dat$part12monum %in% 0:1000
  n_refused <- dat$part12monum %in% -9 | dat$part12modkr %in% -9
  ## Blank, don't know or out of range: treated as don't know
  n_unknown <- !n_known & !n_refused
  debut_unknown <- dat$firstsxagedk %in% c(-8, -9) & is.na(dat$part12monum)
  type_unknown <- !(dat$partrelation1 %in% c(1:9, 96)) # 9 = boyfriend/girlfriend (PHIA2)

  sex12m <- dplyr::case_when(
    ## 0 partners counts even where the derived flag is missing (TZA2022PHIA)
    dat$sexever %in% 2 | dat$sex12months %in% 2 | dat$part12monum %in% 0 ~ FALSE,
    dat$sex12months %in% 1 ~ TRUE,
    ## Don't know number of partners: assume sexually active, as DHS v766b = 98
    dat$part12monum %in% -8 | dat$part12modkr %in% -8 ~ TRUE,
    TRUE ~ NA
  )

  paid <- dat$sellsx12mo %in% 1 | dat$buysx12mo %in% 1 | rel(c(6, 7))
  ## Exactly one partner, type known
  one <- n_known & dat$part12monum == 1 & !type_unknown
  several <- n_known & dat$part12monum > 1

  excluded <- debut_unknown | n_refused | (n_known & dat$part12monum == 1 & type_unknown)

  group <- dplyr::case_when(
    sex12m %in% FALSE ~ "nosex",
    is.na(sex12m) ~ NA_character_,
    paid ~ "paid",
    excluded ~ NA_character_,
    several | n_unknown ~ "nonreg",
    one & (rel(2) | (dat$partrelation1 %in% 1 & dat$partlivew1 %in% 1) |
             (dat$partrelation2 %in% 1 & dat$partlivew2 %in% 1) |
             (dat$partrelation3 %in% 1 & dat$partlivew3 %in% 1)) ~ "cohab",
    one ~ "nonreg",
    TRUE ~ NA_character_
  )

  ## Spouse definition: a spouse living elsewhere counts as cohabiting
  group_spouse <- dplyr::case_when(
    group %in% c("nosex", "paid") | is.na(group) ~ group,
    several | n_unknown ~ "nonreg",
    one & rel(c(1, 2)) ~ "cohab",
    one ~ "nonreg",
    TRUE ~ NA_character_
  )

  set_groups(dat, sex12m, group, group_spouse)
}

#' Check that each individual is assigned to one and only one risk category.
#'
#' @param survey_sexbehav Output of e.g. `extract_sexbehav_phia`.
#' @return Rows of `survey_sexbehav` not in exactly one category.
#' @export
check_survey_sexbehav <- function(survey_sexbehav) {
  df <- survey_sexbehav %>%
    dplyr::mutate(
      r_tot = nosex12m + sexcohab + sexnonreg + sexpaid12m,
      r_tot_spouse = nosex12m + sexcohabspouse + sexnonregspouse + sexpaid12m
    )

  cat(
    paste0(
      "Without spouse adjustment:\n",
      "The proportion of rows allocatated to one and only one category is ",
      round(sum(df$r_tot == 1) / nrow(df), 3) * 100, "%.\n",
      "The following rows are incorrectly allocated to multiple, or no, categories:\n"
    )
  )

  print(df %>% dplyr::filter(r_tot != 1))

  cat(
    paste0(
      "Under the spouse adjustment:\n",
      "The proportion of rows allocatated to one and only one category is ",
      round(sum(df$r_tot_spouse == 1) / nrow(df), 3) * 100, "%.\n",
      "The following rows are incorrectly allocated to multiple, or no, categories:\n"
    )
  )

  df %>% dplyr::filter(r_tot_spouse != 1)
}
