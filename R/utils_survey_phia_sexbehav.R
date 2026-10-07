#' Extract sexual behaviour categorical variables from PHIA surveys.
#'
#' @param ind Individuals dataset.
#' @param survey_id The survey name.
#' @return Sexual behaviour categorical variables
#' @export
extract_sexbehav_phia <- function(ind, survey_id) {
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
    "buysx12mo" # Paid money or given gifts for sex during past 12 months
  )

  # The following other variables may be of future interest, but are not in all the surveys:
  # * analsxever: Age at first anal sex
  # * lifetimesex: Total lifetime sexual partners
  # * lifetimesexdk: Total lifetime sexual partners
  # * paste0("partlastsxtimed", 1:3): How long since last sex with partner i

  # Fix issues with particular surveys having different variable names
  if(survey_id %in% c("ZWE2016PHIA")) { ind <- rename(ind, "part12modkr" = "part12monumdk") }
  if(survey_id %in% c("ZMB2016PHIA")) { ind <- rename(ind, "part12monum" = "part12mo") }
  if(survey_id %in% c("MWI2016PHIA")) { ind <- rename(ind, "part12modkr" = "part12monumdk") }

  dat <- ind %>%
    mutate(
      survey_id = survey_id,
      individual_id = personid
    ) %>%
    select(survey_id, individual_id, tidyselect::any_of(sb_vars))

  # Fixes issue with e.g. some surveys not having paid sex questions
  dat[setdiff(sb_vars, names(dat))] <- NA

  dat %>%
    mutate(
      # Reports sexual activity in the last 12 months
      sex12m = case_when(
        is.na(firstsxage) & is.na(firstsxagedk) ~ FALSE,
        is.na(firstsxage) & firstsxagedk == 96 ~ FALSE, # 96 is code for no sex
        part12monum > 0 ~ TRUE,
        part12modkr == -8 ~ TRUE, # If don't know number of partners this year assume > 0
        TRUE ~ FALSE
      ),
      # Does not report sexual activity in the last 12 months
      nosex12m = case_when(
        sex12m == TRUE ~ FALSE,
        sex12m == FALSE ~ TRUE,
        is.na(sex12m) ~ NA
      ),
      # Reports sexual activity with exactly one cohabiting partner in the past 12 months
      sexcohab = case_when(
        sex12m == FALSE ~ FALSE,
        (part12monum == 1) & ((partrelation1 == 2) | (partrelation2 == 2) | (partrelation3 == 2)) ~ TRUE,
        (part12monum == 1) & ((partrelation1 == 1) & (partlivew1 == 1)) ~ TRUE,
        (part12monum == 1) & ((partrelation2 == 1) & (partlivew2 == 1)) ~ TRUE,
        (part12monum == 1) & ((partrelation3 == 1) & (partlivew3 == 1)) ~ TRUE,
        TRUE ~ FALSE
      ),
      # Reports sexual activity with exactly one cohabiting partner or exactly one married partner who is living away
      # Either the spouse lives in, in which case they are cohabiting, or they live away, in which case they are also
      # covered here. So there is no need to check with the partlivew variable where the spouse is living.
      sexcohabspouse = case_when(
        sex12m == FALSE ~ FALSE,
        (part12monum == 1) & (partrelation1 %in% c(1, 2)) ~ TRUE,
        (part12monum == 1) & (partrelation2 %in% c(1, 2)) ~ TRUE,
        (part12monum == 1) & (partrelation3 %in% c(1, 2)) ~ TRUE,
        TRUE ~ FALSE
      ),
      # Reports sexual activity with greater than one partner or any non-cohabiting partner
      sexnonreg = case_when(
        nosex12m == TRUE ~ FALSE,
        sexcohab == TRUE ~ FALSE,
        (part12monum == 1) & !(partrelation1 == 2) ~ TRUE, # Any relation but live-in partner
        (part12monum == 1) & !(partrelation2 == 2) ~ TRUE,
        (part12monum == 1) & !(partrelation3 == 2) ~ TRUE,
        part12monum > 1 ~ TRUE, # More than one partner
        TRUE ~ FALSE
      ),
      # Reports sexual activity with greater than one partner or any non-marital non-cohabiting partner
      sexnonregspouse = case_when(
        nosex12m == TRUE ~ FALSE,
        sexcohabspouse == TRUE ~ FALSE,
        (part12monum == 1) & !(partrelation1 %in% c(1, 2)) ~ TRUE, # Any relation but live-in partner or spouse
        (part12monum == 1) & !(partrelation2 %in% c(1, 2)) ~ TRUE,
        (part12monum == 1) & !(partrelation3 %in% c(1, 2)) ~ TRUE,
        part12monum > 1 ~ TRUE, # More than one partner
        TRUE ~ FALSE
      ),
      # Reports having exchanged gifts, cash, or anything else for sex in the past 12 months
      sexpaid12m = case_when(
        nosex12m == TRUE ~ FALSE,
        (sellsx12mo == 1) | (buysx12mo == 1) ~ TRUE,
        (partrelation1 %in% c(6, 7) | partrelation2 %in% c(6, 7) | partrelation3 %in% c(6, 7)) ~ TRUE,
        TRUE ~ FALSE
      ),
      # Indicator for including any non-missing observations for selling sex (i.e. whether it was in the questionnaire)
      giftsvar = case_when(
        sum(!is.na(sellsx12mo)) > 0 ~ TRUE,
        TRUE ~ FALSE
      ),
      # Either sexnonreg or sexpaid12m
      sexnonregplus = case_when(
        sexnonreg == TRUE ~ TRUE,
        sexpaid12m == TRUE ~ TRUE,
        TRUE ~ FALSE
      ),
      # Either sexnonregspouse or sexpaid12m
      sexnonregspouseplus = case_when(
        sexnonregspouse == TRUE ~ TRUE,
        sexpaid12m == TRUE ~ TRUE,
        TRUE ~ FALSE
      ),
      # Just want the highest risk category that an individual belongs to
      nosex12m = ifelse(sexcohab | sexnonreg | sexpaid12m, FALSE, nosex12m),
      sexcohab = ifelse(sexnonreg | sexpaid12m, FALSE, sexcohab),
      sexcohabspouse = ifelse(sexnonregspouse | sexpaid12m, FALSE, sexcohabspouse),
      sexnonreg = ifelse(sexpaid12m, FALSE, sexnonreg),
      sexnonregspouse = ifelse(sexpaid12m, FALSE, sexnonregspouse),
      # Turn everything from TRUE / FALSE coding to 1 / 0
      across(sex12m:sexnonregspouseplus, ~ as.numeric(.x))
    ) %>%
    select(-all_of(sb_vars))
}

#' Check that each individual is assigned to one and only one risk category.
#'
#' @param survey_sexbehav Output of e.g. `extract_sexbehav_phia`.
#' @return Rows of `survey_sexbehav` not in exactly one category.
#' @export
check_survey_sexbehav <- function(survey_sexbehav) {
  df <- survey_sexbehav %>%
    mutate(
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

  print(df %>% filter(r_tot != 1))

  cat(
    paste0(
      "Under the spouse adjustment:\n",
      "The proportion of rows allocatated to one and only one category is ",
      round(sum(df$r_tot_spouse == 1) / nrow(df), 3) * 100, "%.\n",
      "The following rows are incorrectly allocated to multiple, or no, categories:\n"
    )
  )

  df %>% filter(r_tot_spouse != 1)
}
